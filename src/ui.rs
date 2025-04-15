use actix_files::Files;
use actix_web::{App, HttpResponse, HttpServer, Result, web};
use serde::{Deserialize, Serialize};
use std::fs;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::process::Command;
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::aut::Aut;
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

// Kill any process listening on the specified port
fn kill_process_on_port(port: u16) -> std::io::Result<()> {
    println!("Checking for existing server on port {}...", port);

    #[cfg(target_family = "unix")]
    {
        // On Unix-like systems (Linux/macOS)
        let lsof_output = Command::new("lsof")
            .args(["-i", &format!(":{}", port), "-t"])
            .output()?;

        if !lsof_output.stdout.is_empty() {
            let pid = String::from_utf8_lossy(&lsof_output.stdout)
                .trim()
                .to_string();
            println!("Found existing server (PID: {}) - shutting it down...", pid);

            Command::new("kill").arg(pid).status()?;

            // Give it a moment to close
            std::thread::sleep(std::time::Duration::from_millis(500));
        } else {
            println!("No existing server found on port {}", port);
        }
    }

    #[cfg(target_family = "windows")]
    {
        // On Windows systems
        let netstat_output = Command::new("cmd")
            .args(["/c", &format!("netstat -ano | findstr :{}", port)])
            .output()?;

        if !netstat_output.stdout.is_empty() {
            // Parse netstat output to extract PID (last column)
            let output_str = String::from_utf8_lossy(&netstat_output.stdout);
            for line in output_str.lines() {
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 5 {
                    if let Ok(pid) = parts[4].parse::<u32>() {
                        println!("Found existing server (PID: {}) - shutting it down...", pid);

                        Command::new("taskkill")
                            .args(["/F", "/PID", &pid.to_string()])
                            .status()?;

                        // Give it a moment to close
                        std::thread::sleep(std::time::Duration::from_millis(500));
                        break;
                    }
                }
            }
        } else {
            println!("No existing server found on port {}", port);
        }
    }

    Ok(())
}

// Open the browser to the specified URL
fn open_browser(url: &str) -> std::io::Result<()> {
    println!("Opening browser to {}", url);

    #[cfg(target_os = "windows")]
    {
        Command::new("cmd").args(["/c", "start", url]).spawn()?;
    }

    #[cfg(target_os = "macos")]
    {
        Command::new("open").arg(url).spawn()?;
    }

    #[cfg(target_os = "linux")]
    {
        // Try common browsers
        let browsers = ["xdg-open", "firefox", "google-chrome", "chromium-browser"];
        let mut success = false;

        for browser in browsers {
            if let Ok(_) = Command::new(browser).arg(url).spawn() {
                success = true;
                break;
            }
        }

        if !success {
            return Err(std::io::Error::new(
                ErrorKind::NotFound,
                "Could not find a browser to open the URL",
            ));
        }
    }

    Ok(())
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

            // Create automaton with the correct number of variables
            let mut aut = Aut::new(expr.num_fields());
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
    // Kill any existing server on the same port
    if let Err(e) = kill_process_on_port(port) {
        println!("Warning: Could not check for existing servers: {}", e);
    }

    // Open the browser in the background
    let browser_url = format!("http://localhost:{}", port);
    std::thread::spawn(move || {
        // Give the server a moment to start
        std::thread::sleep(std::time::Duration::from_secs(1));

        if let Err(e) = open_browser(&browser_url) {
            println!("Warning: Could not open browser: {}", e);
            println!("Please open your browser and navigate to {}", browser_url);
        }
    });

    // Start the server (this blocks until the server exits)
    init_server(port).await
}
