#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(non_snake_case)]

use clap::{Parser, Subcommand};
use crate::expr::Expr;
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
    /// Parse and process NetKAT expressions from a file or directory,
    /// generating a static HTML report in the specified output directory.
    Parse {
        /// The file or directory path to parse.
        path: PathBuf,
        /// The base directory for outputting the static HTML reports.
        #[arg(short, long, value_name = "DIR", default_value = "./out")]
        out_dir: PathBuf,
    },
}

#[tokio::main(flavor = "current_thread")]
async fn main() -> std::io::Result<()> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Parse { path, out_dir } => {
            if !path.exists() {
                eprintln!("Error: Path \"{}\" does not exist.", path.display());
                std::process::exit(1);
            }

            // Ensure the base output directory exists
            if let Err(e) = fs::create_dir_all(&out_dir) {
                eprintln!("Error: Could not create output directory \"{}\": {}", out_dir.display(), e);
                std::process::exit(1);
            }

            if path.is_dir() {
                process_directory(&path, &out_dir);
            } else if path.is_file() {
                process_file(&path, &out_dir);
            } else {
                eprintln!(
                    "Error: Path \"{}\" is neither a file nor a directory.",
                    path.display()
                );
                std::process::exit(1);
            }
        }
    }
    Ok(())
}

fn process_directory(dir_path: &Path, out_dir: &Path) {
    println!("Processing directory: {}", dir_path.display());
    let mut found_k2_files = false;
    for entry in WalkDir::new(dir_path).into_iter().filter_map(|e| e.ok()) {
        let current_path = entry.path();
        if current_path.is_file() {
            if let Some(ext) = current_path.extension() {
                if ext == "k2" {
                    found_k2_files = true;
                    process_file(current_path, out_dir);
                }
            }
        }
    }
    if !found_k2_files {
        println!("No .k2 files found in directory: {}", dir_path.display());
    }
}

fn process_file(file_path: &Path, out_dir: &Path) {
    println!("--- Processing file: {} ---", file_path.display());
    match fs::read_to_string(file_path) {
        Ok(content) => {
            match crate::parser::parse_expressions(&content) {
                Ok(expressions) => {
                    if expressions.is_empty() {
                        println!("No expressions found or parsed in {}.", file_path.display());
                    } else {
                        println!("Parsed {} expressions from {}.", expressions.len(), file_path.display());
                        // Optional: Print details of expressions if needed for debugging
                        // for (i, expr) in expressions.iter().enumerate() {
                        //     println!("  {}: {:?}", i + 1, expr);
                        // }

                        let source_file_name = file_path
                            .file_name()
                            .unwrap_or_else(|| std::ffi::OsStr::new("unknown.k2"))
                            .to_string_lossy();
                        
                        println!("Generating static site for {}...", source_file_name);
                        if let Err(e) = crate::ui::generate_static_site(&expressions, &source_file_name, out_dir) {
                            eprintln!("  Error generating static site for {}: {}", source_file_name, e);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("  Error parsing file {}: {}", file_path.display(), e);
                }
            }
        }
        Err(e) => {
            eprintln!("  Error reading file {}: {}", file_path.display(), e);
        }
    }
    println!("--- Finished processing file: {} ---", file_path.display());
}
