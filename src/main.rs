#![allow(dead_code)]
#![allow(unused)]
#![allow(non_snake_case)]

use clap::{Parser, Subcommand};
use expr::Expr;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

mod aut;
mod expr;
mod fuzz;
mod parser;
mod pre;
mod sp;
mod spp;
mod ui;
mod viz;
/// KATch2: A symbolic automata toolkit for NetKAT expressions
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Run the interactive web UI
    #[command(name = "webui")]
    WebUI {
        /// Port to run the web server on
        #[arg(short, long, default_value = "8080")]
        port: u16,
    },

    /// Parse and process NetKAT expressions from a file or directory
    Parse {
        /// The file or directory path to parse
        path: PathBuf,
    },
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::WebUI { port } => {
            println!("Starting web UI server on port {}", port);
            if let Err(e) = ui::start_ui(*port).await {
                eprintln!("Error running web server: {}", e);
                std::process::exit(1);
            }
        }
        Commands::Parse { path } => {
            // Traditional file processing mode
            if !path.exists() {
                eprintln!("Error: Path \"{}\" does not exist.", path.display());
                std::process::exit(1);
            }

            if path.is_dir() {
                process_directory(&path);
            } else if path.is_file() {
                process_file(&path);
            } else {
                eprintln!(
                    "Error: Path \"{}\" is neither a file nor a directory.",
                    path.display()
                );
                std::process::exit(1);
            }
        }
    }
}

fn process_directory(dir_path: &Path) {
    println!("Processing directory: {}", dir_path.display());
    let mut found_k2_files = false;
    for entry in WalkDir::new(dir_path).into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_file() {
            if let Some(ext) = path.extension() {
                if ext == "k2" {
                    found_k2_files = true;
                    process_file(path);
                }
            }
        }
    }
    if !found_k2_files {
        println!("No .k2 files found in directory.");
    }
}

fn process_file(file_path: &Path) {
    println!("--- Processing file: {} ---", file_path.display());
    match fs::read_to_string(file_path) {
        Ok(content) => {
            match parser::parse_expressions(&content) {
                Ok(expressions) => {
                    if expressions.is_empty() {
                        println!("No expressions found or parsed.");
                    } else {
                        println!("Parsed Expressions:");
                        for (i, expr) in expressions.iter().enumerate() {
                            println!("  {}: {:?}", i + 1, expr);
                            // Potentially print a more user-friendly format later
                            // println!("  {}: {}", i + 1, expr);
                        }
                        for expr in &expressions {
                            process_expression(expr);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("  Error parsing file: {}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("  Error reading file: {}", e);
        }
    }
    println!("-------------------------------");
}

fn process_expression(expr: &Box<Expr>) {
    // Create an automaton from the expression
    let mut aut = aut::Aut::new(expr.num_fields());
    let state = aut.expr_to_state(expr);
    println!("State: {}", state);
    let delta = aut.delta(state);
    println!("Delta: {:?}", delta);
    let epsilon = aut.epsilon(state);
    println!("Epsilon: {:?}", epsilon);
}
