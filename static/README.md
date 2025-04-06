# KATch2 Web UI Static Files

This directory contains the static files for the KATch2 web UI:

- `index.html`: Main HTML file for the web interface
- `style.css`: CSS styles for the web interface
- `app.js`: JavaScript code for the web interface functionality

## Development

To modify the web UI, simply edit these files directly. Changes will be reflected the next time you run the web server.

## Structure

- The UI consists of an expression editor and a visualization area
- As you type expressions, they are evaluated after a brief pause
- Results are displayed in the visualization area
- Any errors during parsing or evaluation are shown in an error section

## Running the Web UI

To start the web server:

```bash
cargo run -- webui
```

Then open your browser to http://localhost:8080/ to access the interface. 