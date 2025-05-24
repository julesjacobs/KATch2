use crate::{aut::Aut, expr::Expr, parser, viz};
use std::fs;
use std::io::{Error as IoError, ErrorKind as IoErrorKind, Write as IoWrite};
use std::path::Path;
use std::fmt::Write as FmtWrite;

/// Generates a static HTML site with visualization reports for a set of NetKAT expressions.
///
/// The site will be created in a subdirectory named after `source_file_name` (e.g., input file's name)
/// inside the `base_output_dir`. Each expression will have its own report in a nested subdirectory.
/// An `index.html` will link to all generated reports.
///
/// # Arguments
/// * `expressions` - A slice of parsed NetKAT expressions.
/// * `source_file_name` - The name of the source (e.g., "example.k2"), used for naming the output subdirectory.
/// * `base_output_dir` - The root directory where the output site (e.g., `./out/`) will be created.
pub fn generate_static_site(
    expressions: &[Box<Expr>],
    source_file_name: &str,
    base_output_dir: &Path,
) -> std::io::Result<()> {
    let site_dir_name = Path::new(source_file_name)
        .file_stem()
        .map_or_else(
            || std::ffi::OsStr::new("unknown_source"),
            |stem| stem,
        )
        .to_string_lossy();

    let site_specific_output_dir = base_output_dir.join(site_dir_name.as_ref());
    fs::create_dir_all(&site_specific_output_dir)?;

    let mut index_html_content = String::new();
    writeln!(
        index_html_content,
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>KATch2 Reports for {}</title>
    <style>
        body {{ font-family: sans-serif; margin: 20px; line-height: 1.6; }}
        h1 {{ border-bottom: 1px solid #eee; padding-bottom: 10px; }}
        ul {{ list-style-type: none; padding-left: 0; }}
        li {{ margin-bottom: 10px; }}
        a {{ text-decoration: none; color: #0066cc; }}
        a:hover {{ text-decoration: underline; }}
        .error {{ color: red; font-weight: bold; }}
    </style>
</head>
<body>
    <h1>NetKAT Expression Reports for: <em>{}</em></h1>
    <ul>"#,
        source_file_name, source_file_name
    ).map_err(|e| IoError::new(IoErrorKind::Other, format!("Failed to write HTML head: {}", e)))?;

    if expressions.is_empty() {
        writeln!(index_html_content, "        <li>No expressions found to generate reports for.</li>")
            .map_err(|e| IoError::new(IoErrorKind::Other, format!("Failed to write empty message: {}", e)))?;
    } else {
        for (i, expr) in expressions.iter().enumerate() {
            let expr_report_subdir_name = format!("expr_{}", i + 1);
            let expr_report_full_path = site_specific_output_dir.join(&expr_report_subdir_name);

            if let Err(e) = fs::create_dir_all(&expr_report_full_path) {
                let error_msg = format!("Failed to create directory for expression {}: {}", i + 1, e);
                eprintln!("{}", error_msg);
                writeln!(index_html_content, "        <li class=\"error\">{}</li>", error_msg)
                    .map_err(|e_fmt| IoError::new(IoErrorKind::Other, format!("Failed to write error li: {}", e_fmt)))?;
                continue;
            }

            let mut aut = Aut::new(expr.num_fields());
            let initial_state_idx = aut.expr_to_state(expr);

            match viz::render_aut(initial_state_idx, &mut aut, &expr_report_full_path) {
                Ok(_) => {
                    let report_link = format!("{}/report.html", expr_report_subdir_name);
                    writeln!(
                        index_html_content,
                        "        <li><a href=\"{}\">Report for Expression {}</a></li>",
                        report_link,
                        i + 1
                    ).map_err(|e| IoError::new(IoErrorKind::Other, format!("Failed to write success li: {}", e)))?;
                }
                Err(e) => {
                    let error_msg = format!("Error generating report for Expression {}: {}", i + 1, e);
                    eprintln!("{}", error_msg);
                    writeln!(index_html_content, "        <li class=\"error\">{}</li>", error_msg)
                        .map_err(|e_fmt| IoError::new(IoErrorKind::Other, format!("Failed to write error li (render fail): {}", e_fmt)))?;
                }
            }
        }
    }

    writeln!(index_html_content, "    </ul>\n</body>\n</html>")
        .map_err(|e| IoError::new(IoErrorKind::Other, format!("Failed to write HTML foot: {}", e)))?;

    let index_file_path = site_specific_output_dir.join("index.html");
    let mut file = fs::File::create(&index_file_path)?;
    file.write_all(index_html_content.as_bytes())?;

    println!(
        "Static site for '{}' generated successfully at: {}",
        source_file_name,
        index_file_path.display()
    );

    Ok(())
}
