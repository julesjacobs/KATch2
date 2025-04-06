use actix_web::{web, App, HttpServer, HttpResponse, Result};
use actix_files::Files;
use serde::{Deserialize, Serialize};
use std::sync::Mutex;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::fs;
use std::time::{SystemTime, UNIX_EPOCH};
use std::process::Command;

use crate::aut::Aut;
use crate::expr::Expr;
use crate::parser;
use crate::viz;

// Structure to hold application state
struct AppState {
    temp_dir: Mutex<PathBuf>,
}

// Request structure for expression evaluation
#[derive(Deserialize)]
struct ExpressionRequest {
    expression: String,
}

// Response structure for expression evaluation
#[derive(Serialize)]
struct ExpressionResponse {
    success: bool,
    message: String,
    report_url: Option<String>,
    error: Option<String>,
}

// Initialize the web server with necessary state
async fn init_server(port: u16) -> std::io::Result<()> {
    // Create a temporary directory for visualization outputs
    let temp_path = std::env::temp_dir().join("katch2_ui");
    fs::create_dir_all(&temp_path)?;
    
    println!("KATch2 Web UI starting on http://localhost:{}", port);
    println!("Temporary files directory: {}", temp_path.display());
    
    // Start the HTTP server
    HttpServer::new(move || {
        App::new()
            .app_data(web::Data::new(AppState {
                temp_dir: Mutex::new(temp_path.clone()),
            }))
            .route("/api/evaluate", web::post().to(evaluate_expression))
            .service(Files::new("/visualizations", temp_path.clone()))
            .service(Files::new("/", "./static").index_file("index.html"))
    })
    .bind(("127.0.0.1", port))?
    .run()
    .await
}

// Handler for expression evaluation requests
async fn evaluate_expression(
    data: web::Data<AppState>,
    request: web::Json<ExpressionRequest>,
) -> Result<HttpResponse> {
    let expression = &request.expression;
    
    // Create a response structure
    let mut response = ExpressionResponse {
        success: false,
        message: String::new(),
        report_url: None,
        error: None,
    };
    
    // Parse and evaluate the expression
    match parser::parse_expressions(expression) {
        Ok(expressions) => {
            if expressions.is_empty() {
                response.error = Some("No valid expressions found".to_string());
                return Ok(HttpResponse::Ok().json(response));
            }
            
            // Use the first expression
            let expr = &expressions[0];
            
            // Get a unique output directory for this evaluation
            let output_dir = create_output_dir(&data.temp_dir)?;
            
            // Create automaton from expression
            let mut aut = Aut::new(2); // Assuming 2 variables, adjust as needed
            let state = aut.expr_to_state(expr);
            
            // Generate visualization
            match viz::render_aut(state, &mut aut, &output_dir) {
                Ok(_) => {
                    // Determine the relative URL for the visualization report
                    let dir_name = output_dir.file_name().unwrap().to_string_lossy();
                    response.success = true;
                    response.message = format!("Automaton created for: {}", expression);
                    response.report_url = Some(format!("/visualizations/{}/report.html", dir_name));
                }
                Err(err) => {
                    response.error = Some(format!("Error generating visualization: {}", err));
                }
            }
        }
        Err(err) => {
            response.error = Some(format!("Error parsing expression: {}", err));
        }
    }
    
    Ok(HttpResponse::Ok().json(response))
}

// Create a unique output directory for visualization files
fn create_output_dir(temp_dir_mutex: &Mutex<PathBuf>) -> std::io::Result<PathBuf> {
    let temp_dir = temp_dir_mutex.lock().unwrap();
    
    // Create a timestamp-based directory name
    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis();
    
    let output_dir = temp_dir.join(format!("viz_{}", timestamp));
    fs::create_dir_all(&output_dir)?;
    
    Ok(output_dir)
}

// Entry point function to start the web UI
pub async fn start_ui(port: u16) -> std::io::Result<()> {
    // Create the static files
    create_static_files()?;
    
    // Start the server
    init_server(port).await
}

// Create static files for the web UI
pub fn create_static_files() -> std::io::Result<()> {
    let static_dir = Path::new("./static");
    
    // Recreate the static directory
    if static_dir.exists() {
        fs::remove_dir_all(static_dir)?;
    }
    fs::create_dir_all(static_dir)?;
    
    // Create index.html
    let index_html = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>KATch2 Interactive Automaton Editor</title>
    <link rel="stylesheet" href="style.css">
</head>
<body>
    <header>
        <h1>KATch2 Interactive Automaton Editor</h1>
    </header>
    
    <main>
        <section class="editor-section">
            <h2>Expression Editor</h2>
            <div class="editor-container">
                <textarea id="expression-editor" placeholder="Enter your expression here..."></textarea>
                <div class="editor-controls">
                    <button id="evaluate-btn">Evaluate</button>
                </div>
            </div>
            <div id="error-display" class="error-container"></div>
        </section>
        
        <section class="visualization-section">
            <h2>Automaton Visualization</h2>
            <div id="visualization-container">
                <div class="placeholder">Enter an expression and click Evaluate to generate visualization</div>
                <iframe id="visualization-frame" style="display: none;"></iframe>
            </div>
        </section>
    </main>
    
    <footer>
        <p>KATch2 Automaton Visualization Tool</p>
    </footer>

    <script src="app.js"></script>
</body>
</html>"#;
    
    let mut file = fs::File::create(static_dir.join("index.html"))?;
    file.write_all(index_html.as_bytes())?;
    
    // Create style.css
    let style_css = r#"* {
    box-sizing: border-box;
    margin: 0;
    padding: 0;
}

body {
    font-family: 'Linux Libertine', 'Times New Roman', Times, serif;
    line-height: 1.6;
    color: #333;
    background-color: #f8f9fa;
    max-width: 1200px;
    margin: 0 auto;
    padding: 20px;
}

header {
    margin-bottom: 20px;
    border-bottom: 1px solid #ddd;
    padding-bottom: 10px;
}

h1, h2 {
    color: #2c3e50;
    font-weight: normal;
}

h1 {
    font-size: 1.8rem;
}

h2 {
    font-size: 1.4rem;
    margin-bottom: 10px;
}

section {
    background-color: #fff;
    border: 1px solid #ddd;
    border-radius: 4px;
    padding: 15px;
    margin-bottom: 20px;
}

.editor-container {
    display: flex;
    flex-direction: column;
    gap: 10px;
}

#expression-editor {
    width: 100%;
    height: 100px;
    padding: 10px;
    font-family: 'Courier New', monospace;
    font-size: 14px;
    border: 1px solid #ccc;
    border-radius: 4px;
    resize: vertical;
}

.editor-controls {
    display: flex;
    justify-content: flex-end;
}

button {
    background-color: #3498db;
    color: white;
    border: none;
    padding: 8px 15px;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
}

button:hover {
    background-color: #2980b9;
}

.error-container {
    color: #e74c3c;
    font-family: 'Courier New', monospace;
    font-size: 14px;
    padding: 10px;
    margin-top: 10px;
    background-color: #fde9e8;
    border-left: 3px solid #e74c3c;
    display: none;
}

.visualization-section {
    min-height: 400px;
}

#visualization-container {
    width: 100%;
    min-height: 300px;
    border: 1px solid #ddd;
    border-radius: 4px;
    background-color: #f9f9f9;
    overflow: hidden;
}

.placeholder {
    display: flex;
    justify-content: center;
    align-items: center;
    height: 300px;
    color: #7f8c8d;
    text-align: center;
    padding: 20px;
}

#visualization-frame {
    width: 100%;
    height: 600px;
    border: none;
}

footer {
    text-align: center;
    margin-top: 30px;
    padding-top: 10px;
    border-top: 1px solid #ddd;
    color: #7f8c8d;
    font-size: 14px;
}"#;
    
    let mut file = fs::File::create(static_dir.join("style.css"))?;
    file.write_all(style_css.as_bytes())?;
    
    // Create app.js
    let app_js = r#"document.addEventListener('DOMContentLoaded', function() {
    const editor = document.getElementById('expression-editor');
    const evaluateBtn = document.getElementById('evaluate-btn');
    const errorDisplay = document.getElementById('error-display');
    const visualizationContainer = document.getElementById('visualization-container');
    const placeholder = document.querySelector('.placeholder');
    const visualizationFrame = document.getElementById('visualization-frame');
    
    let typingTimer;
    const doneTypingInterval = 1000; // 1 second
    
    // Function to evaluate the expression
    async function evaluateExpression() {
        const expression = editor.value.trim();
        
        // Don't evaluate if empty
        if (!expression) {
            errorDisplay.style.display = 'none';
            visualizationFrame.style.display = 'none';
            placeholder.style.display = 'flex';
            return;
        }
        
        try {
            const response = await fetch('/api/evaluate', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                },
                body: JSON.stringify({ expression }),
            });
            
            const result = await response.json();
            
            if (result.success) {
                // Hide error and placeholder, show visualization
                errorDisplay.style.display = 'none';
                placeholder.style.display = 'none';
                visualizationFrame.style.display = 'block';
                
                // Set the iframe source to the generated report
                visualizationFrame.src = result.report_url;
            } else {
                // Show error message
                errorDisplay.style.display = 'block';
                errorDisplay.textContent = result.error || 'An unknown error occurred';
                
                // Hide visualization if there was an error
                visualizationFrame.style.display = 'none';
                placeholder.style.display = 'flex';
            }
        } catch (error) {
            console.error('Error:', error);
            errorDisplay.style.display = 'block';
            errorDisplay.textContent = 'Network error: Could not connect to the server';
        }
    }
    
    // Event listener for the evaluate button
    evaluateBtn.addEventListener('click', evaluateExpression);
    
    // Evaluate on typing after delay
    editor.addEventListener('keyup', function() {
        clearTimeout(typingTimer);
        if (editor.value) {
            typingTimer = setTimeout(evaluateExpression, doneTypingInterval);
        }
    });
    
    // Cancel the timer on keydown
    editor.addEventListener('keydown', function() {
        clearTimeout(typingTimer);
    });
    
    // Add some example expressions to help users get started
    const exampleExpressions = [
        "test(0, true); test(1, true); assign(0, true)",
        "test(0, true)* + assign(1, false)",
        "(test(0, true) + test(1, false))*"
    ];
    
    // Set a random example in the editor
    const randomExample = exampleExpressions[Math.floor(Math.random() * exampleExpressions.length)];
    editor.value = randomExample;
    
    // Initial evaluation with the example
    setTimeout(evaluateExpression, 500);
});"#;
    
    let mut file = fs::File::create(static_dir.join("app.js"))?;
    file.write_all(app_js.as_bytes())?;
    
    Ok(())
} 