import os
import glob
from pathlib import Path
import tiktoken
from openai import OpenAI
from rich.console import Console
from rich.prompt import Prompt
from rich.panel import Panel
from rich.markdown import Markdown
from rich.progress import Progress, SpinnerColumn, TextColumn
import subprocess
import json
import traceback
from datetime import datetime
import re

console = Console()

def parse_model_response(response_text):
    """Parse the model's response, extracting summary and file changes."""
    # Split the response into summary and changes
    parts = response_text.split('<file')
    summary = parts[0].strip()
    changes = {}
    
    # If there are no file tags, return just the summary
    if len(parts) == 1:
        return summary, changes
    
    # Process each file change
    for part in parts[1:]:
        # Extract path and content
        path_match = re.search(r'path="([^"]+)"', part)
        if not path_match:
            continue
            
        path = path_match.group(1)
        # Find the content between the file tags
        content_match = re.search(r'>\s*(.*?)\s*</file>', part, re.DOTALL)
        if content_match:
            content = content_match.group(1)
            changes[path] = content
    
    return summary, changes

def save_response(response, task_description):
    try:
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        os.makedirs("ai_data", exist_ok=True)
        os.makedirs("ai_data/responses", exist_ok=True)
        
        # Save the response
        response_file = f"ai_data/responses/response_{timestamp}.txt"
        with open(response_file, 'w', encoding='utf-8') as f:
            f.write(f"Task: {task_description}\n\n")
            f.write("Model Response:\n")
            f.write(response.choices[0].message.content)
        
        # Save the last prompt
        prompt_file = "ai_data/last_prompt.txt"
        with open(prompt_file, 'w', encoding='utf-8') as f:
            f.write(f"Task: {task_description}\n\n")
            f.write("Codebase:\n")
            f.write(content)
        
        console.print(f"[green]Saved model response to {response_file}[/green]")
        return response_file
    except Exception as e:
        console.print(f"[red]Error saving response: {str(e)}[/red]")
        return None

def read_api_keys():
    try:
        with open('api_keys.json', 'r') as f:
            return json.load(f)
    except FileNotFoundError:
        console.print("[red]Error: api_keys.json not found[/red]")
        exit(1)
    except json.JSONDecodeError:
        console.print("[red]Error: api_keys.json is not valid JSON[/red]")
        exit(1)

def read_file_content(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as file:
            return file.read()
    except Exception as e:
        return f"Error reading file {file_path}: {str(e)}"

def count_tokens(text, model="gpt-4"):
    try:
        encoding = tiktoken.encoding_for_model(model)
        return len(encoding.encode(text))
    except Exception as e:
        console.print(f"[red]Error counting tokens: {str(e)}[/red]")
        return 0

def estimate_costs(token_count):
    # Prices per 1M tokens (as of 2024)
    models = {
        "gpt-4.1": {
            "input": 2.00,
            "cached": 0.50,
            "output": 8.00
        },
        "o4-mini": {
            "input": 1.10,
            "cached": 0.275,
            "output": 4.40
        },
        "o3": {
            "input": 10.00,
            "cached": 2.50,
            "output": 40.00
        },
        "gemini-2.5-pro": {
            "input": 1.25 if token_count <= 200_000 else 2.50,
            "output": 10.00 if token_count <= 200_000 else 15.00,
            "cached": 0.00  # Not available yet
        },
        "gemini-1.5-pro": {
            "input": 0.10,
            "output": 0.40,
            "cached": 0.025
        }
    }
    
    return models

def get_task_input():
    console.print("\n[bold]Enter your task description:[/bold]")
    console.print("[dim]Press Ctrl+D (Unix) or Ctrl+Z (Windows) when done[/dim]")
    task = []
    while True:
        try:
            line = input()
            task.append(line)
        except EOFError:
            break
        except KeyboardInterrupt:
            console.print("\n[yellow]Task input cancelled[/yellow]")
            return None
    return "\n".join(task)

def apply_changes(changes):
    try:
        modified_files = []
        with Progress(
            SpinnerColumn(),
            TextColumn("[progress.description]{task.description}"),
            console=console
        ) as progress:
            task = progress.add_task("Applying changes...", total=len(changes))
            
            for file_path, content in changes.items():
                progress.update(task, description=f"Updating {file_path}")
                # Ensure the directory exists, handling both root and subdirectory paths
                dir_path = os.path.dirname(file_path)
                if dir_path:  # Only create directory if path contains subdirectories
                    os.makedirs(dir_path, exist_ok=True)
                with open(file_path, 'w', encoding='utf-8') as f:
                    f.write(content)
                modified_files.append(file_path)
                progress.advance(task)
        
        return modified_files
    except Exception as e:
        console.print(f"[red]Error applying changes: {str(e)}[/red]")
        return []

def read_include_patterns():
    """Read and parse the .aiinclude file for whitelist patterns."""
    try:
        if not os.path.exists('.aiinclude'):
            # Create default .aiinclude if it doesn't exist
            default_patterns = [
                "*.rs",
                "*.txt",
                "*.md"
            ]
            with open('.aiinclude', 'w') as f:
                f.write('\n'.join(default_patterns))
            return default_patterns
        
        with open('.aiinclude', 'r') as f:
            patterns = [line.strip() for line in f if line.strip() and not line.startswith('#')]
            return patterns
    except Exception as e:
        console.print(f"[red]Error reading .aiinclude: {str(e)}[/red]")
        return ["*.rs", "*.txt", "*.md"]  # Fallback to default patterns

def generate_prompt(task_description):
    try:
        # Get include patterns
        include_patterns = read_include_patterns()
        
        # Get all files matching the patterns
        files = []
        for pattern in include_patterns:
            # Handle both root and src directory patterns
            root_files = glob.glob(pattern, recursive=True)
            src_files = glob.glob(f'src/**/{pattern}', recursive=True)
            files.extend(root_files)
            files.extend(src_files)
        
        # Remove duplicates and sort by modification time
        files = list(set(files))
        files.sort(key=lambda x: os.path.getmtime(x))
        
        # Start the structure
        content = '<files>\n'
        
        # Track token counts per file
        file_tokens = []
        
        # Add each file's content
        for file_path in files:
            file_content = read_file_content(file_path)
            file_xml = f'<file path="{file_path}">\n{file_content}\n</file>\n\n'
            token_count = count_tokens(file_xml)
            file_tokens.append((file_path, token_count))
            content += file_xml
        
        content += '</files>'
        
        # Count total tokens
        token_count = count_tokens(content)
        
        return content, token_count, file_tokens
    except Exception as e:
        console.print(f"[red]Error generating prompt: {str(e)}[/red]")
        return None, 0, []

def main():
    try:
        api_keys = read_api_keys()
        client = OpenAI(api_key=api_keys.get('openai'))
        
        current_model = "o4-mini"
        cost_history = []
        token_history = []
        
        def show_include_patterns():
            include_patterns = read_include_patterns()
            console.print("[dim]Current include patterns:[/dim]")
            for pattern in include_patterns:
                console.print(f"[dim]- {pattern}[/dim]")
        
        # Show initial include patterns
        console.print("\n[bold cyan]KATch2 AI Assistant[/bold cyan]")
        show_include_patterns()
        console.print("[dim]Type /help for available commands[/dim]")
        
        while True:
            console.print("\n[bold cyan]KATch2 AI Assistant[/bold cyan]")
            console.print("[dim]Type /help for available commands[/dim]")
            
            if cost_history:
                last_cost = cost_history[-1]
                console.print(f"[dim]Last query: ${last_cost['cost']:.4f} ({last_cost['model']}, {last_cost['input_tokens']} in, {last_cost['output_tokens']} out)[/dim]")
            
            command = Prompt.ask("\n[bold]Command or task[/bold]")
            
            if command.startswith('/'):
                if command == '/model':
                    models = ["o4-mini", "gpt-4.1", "o3", "gemini-2.5-pro", "gemini-1.5-pro"]
                    current_model = Prompt.ask("Select model", choices=models, default=current_model)
                    console.print(f"[green]Switched to model: {current_model}[/green]")
                elif command == '/cost':
                    if not cost_history:
                        console.print("[yellow]No cost history available[/yellow]")
                        continue
                        
                    console.print("\n[bold]Cost History:[/bold]")
                    console.print("Time                 Model          Cost     Input    Output")
                    console.print("-" * 65)
                    
                    for entry in cost_history:
                        time_str = entry['time'].strftime("%Y-%m-%d %H:%M:%S")
                        console.print(f"{time_str}  {entry['model']:<12} ${entry['cost']:>7.4f}  {entry['input_tokens']:>6}  {entry['output_tokens']:>6}")
                    
                    total_cost = sum(entry['cost'] for entry in cost_history)
                    console.print("\n[bold]Total cost:[/bold] ${:.4f}".format(total_cost))
                    
                    # Show cost table for current model
                    console.print("\n[bold]Current Model Costs (per 1M tokens):[/bold]")
                    model_prices = estimate_costs(0)
                    prices = model_prices.get(current_model, model_prices["o4-mini"])
                    console.print(f"Input: ${prices['input']:.2f}")
                    console.print(f"Output: ${prices['output']:.2f}")
                    
                    # Show token counts for last query
                    if token_history:
                        last_tokens = token_history[-1]
                        console.print("\n[bold]Last Query Token Counts:[/bold]")
                        console.print("File                          Tokens")
                        console.print("-" * 40)
                        max_path_len = max(len(path) for path, _ in last_tokens['file_tokens'])
                        for path, tokens in last_tokens['file_tokens']:
                            console.print(f"{path:<{max_path_len}} {tokens/1000:>6.1f}K")
                        console.print(f"\n{'Total tokens:':<{max_path_len}} {last_tokens['total_tokens']/1000:>6.1f}K")
                    
                elif command == '/reload':
                    console.print("[green]Reloading .aiinclude file...[/green]")
                    show_include_patterns()
                elif command == '/help':
                    console.print("\n[bold]Available Commands:[/bold]")
                    console.print("/model  - Switch between available models")
                    console.print("/cost   - Show cost history and current model pricing")
                    console.print("/reload - Reload .aiinclude file")
                    console.print("/exit   - Exit the program")
                elif command == '/exit':
                    break
                continue
            
            task_description = command
            if not task_description:
                task_description = get_task_input()
                if task_description is None:
                    continue
            
            content, token_count, file_tokens = generate_prompt(task_description)
            if content is None:
                continue
            
            # Store token information
            token_history.append({
                'time': datetime.now(),
                'total_tokens': token_count,
                'file_tokens': file_tokens
            })
            
            try:
                system_prompt = """You are a helpful coding assistant. Your task is to modify the provided codebase according to the user's request.

IMPORTANT: Your response must follow this exact format:

1. First, provide a brief summary of the changes you plan to make.
2. Then, for each file you want to modify, use the following XML format:
   <file path="path/to/file">
   [new file content here]
   </file>

CRITICAL REQUIREMENTS:
- The path attribute must be relative to the project root
- The file content MUST be the complete new content of the file
- The XML tags MUST be used exactly as shown, with no additional formatting
- For new files, you MUST include the complete file content
- For existing files, you MUST include the complete modified file content
- The path must match exactly where the file should be created/modified

Example for a new file:
<file path="src/main.rs">
fn main() {
    println!("Hello, world!");
}
</file>

Example for modifying an existing file:
<file path="src/lib.rs">
// ... existing imports ...
pub fn new_function() {
    println!("New functionality!");
}
// ... rest of existing content ...
</file>

Example for modifying a file outside src:
<file path="Cargo.toml">
[package]
name = "my-project"
version = "0.1.0"
// ... rest of file content ...
</file>

Your response will be parsed by a script that expects this exact format. Any deviation will cause errors."""

                response = client.chat.completions.create(
                    model=current_model,
                    messages=[
                        {"role": "system", "content": system_prompt},
                        {"role": "user", "content": f"Codebase:\n{content}\n\nTask: {task_description}"}
                    ]
                )
                
                # Calculate and store the cost of this query
                input_tokens = response.usage.prompt_tokens
                output_tokens = response.usage.completion_tokens
                model_prices = estimate_costs(0)
                prices = model_prices.get(current_model, model_prices["o4-mini"])
                cost = (input_tokens * prices["input"] + output_tokens * prices["output"]) / 1_000_000
                
                # Add to cost history
                cost_history.append({
                    'time': datetime.now(),
                    'model': current_model,
                    'cost': cost,
                    'input_tokens': input_tokens,
                    'output_tokens': output_tokens
                })
                
                # Save the response for debugging
                response_file = save_response(response, task_description)
                
                try:
                    # Parse the response
                    summary, changes = parse_model_response(response.choices[0].message.content)
                    
                    # Show the summary
                    if summary:
                        console.print("\n[bold]Model's Summary:[/bold]")
                        console.print(Panel(summary, title="Summary", border_style="blue"))
                    
                    if changes:
                        modified_files = apply_changes(changes)
                        
                        if modified_files:
                            console.print("\n[green]Modified files:[/green]")
                            for file in modified_files:
                                console.print(f"- {file}")
                            
                            # Show git diff
                            console.print("\n[bold]Changes:[/bold]")
                            diff = subprocess.run(['git', 'diff'], capture_output=True, text=True).stdout
                            if diff.strip():
                                console.print(Markdown(f"```diff\n{diff}\n```"))
                            else:
                                console.print("[dim]No changes detected[/dim]")
                        else:
                            console.print("[yellow]No files were modified[/yellow]")
                    else:
                        console.print("[yellow]No file changes detected in the response[/yellow]")
                        
                except Exception as e:
                    console.print(f"[red]Error processing model response: {str(e)}[/red]")
                    console.print("[yellow]Raw response:[/yellow]")
                    console.print(response.choices[0].message.content)
                    if response_file:
                        console.print(f"[yellow]Full response saved to: {response_file}[/yellow]")
                
            except Exception as e:
                console.print(f"[red]Error calling API: {str(e)}[/red]")
                console.print("[yellow]Full error:[/yellow]")
                console.print(traceback.format_exc())
                
    except KeyboardInterrupt:
        console.print("\n[yellow]Program terminated by user[/yellow]")
    except Exception as e:
        console.print(f"[red]Fatal error: {str(e)}[/red]")
        console.print("[yellow]Full error:[/yellow]")
        console.print(traceback.format_exc())

if __name__ == "__main__":
    main() 