use crate::aut::Aut;
use crate::spp::{SPP, SPPstore};
use regex;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::fs::File;
use std::io::Write;
use std::io::{Error, ErrorKind, Result};
use std::path::Path;
use std::process::Command;

/// Helper function to escape HTML special characters in a string
fn html_escape(s: &str) -> String {
    s.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace("\"", "&quot;")
        .replace("'", "&#39;")
}

/// Computes which nodes in the SPP DAG reachable from `root_index` can reach the `1` node.
/// Uses memoization via the `liveness_map` to handle the DAG structure efficiently.
/// Returns true if `root_index` is alive, false otherwise.
fn compute_liveness(index: SPP, store: &SPPstore, liveness_map: &mut HashMap<SPP, bool>) -> bool {
    // Check memoization table first
    if let Some(&is_alive) = liveness_map.get(&index) {
        return is_alive;
    }

    // Base cases
    let is_alive = match index.0 {
        0 => false, // Node 0 is never considered alive (cannot reach 1)
        1 => true,  // Node 1 is always alive
        _ => {
            // Internal node
            // This get might panic if index is invalid, but that indicates an issue elsewhere.
            let node = store.get(index);
            let children = [node.x00, node.x01, node.x10, node.x11];
            let mut node_is_alive = false;
            for child_index in children.iter() {
                // Recursively compute liveness for all children to populate the map fully.
                // The node is alive if *any* child is alive.
                if compute_liveness(*child_index, store, liveness_map) {
                    node_is_alive = true;
                    // Optimization: could potentially break here if we only needed the return value,
                    // but we continue to ensure the map is fully populated for nodes reachable from `index`.
                }
            }
            node_is_alive
        }
    };

    liveness_map.insert(index, is_alive);
    is_alive
}

/// Recursive helper to generate the DOT representation for an SPP node and its descendants,
/// considering only nodes present in the `liveness_map`.
fn generate_dot_recursive(
    index: SPP,
    store: &SPPstore,
    dot_string: &mut String,
    visited: &mut HashSet<SPP>,
    liveness_map: &HashMap<SPP, bool>,
) {
    // Only proceed if the node is alive OR is node 0 (which needs to be drawn if reached)
    let is_alive = liveness_map.get(&index).copied().unwrap_or(false);
    if !is_alive {
        return;
    }

    // Avoid infinite loops and redundant processing
    if !visited.insert(index) {
        return;
    }

    match index.0 {
        0 => {
            // Style for the bottom node (⊥)
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index.0
            ));
        }
        1 => {
            // Style for the top node (⊤)
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index.0
            ));
        }
        _ => {
            // Internal node (guaranteed to be alive at this point)
            let node = store.get(index);

            // Style for internal nodes: small black circle
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index.0
            ));

            let zero_edge_style = "style=dashed, arrowhead=none, color=red"; // Corresponds to 'f' or 0 output/input
            let one_edge_style = "style=solid, arrowhead=none, color=green"; // Corresponds to 't' or 1 output/input
            let intermediate_node_style =
                "shape=point, width=0.00, height=0.00, style=filled, fillcolor=white";

            let mut children_to_recurse = HashSet::new();

            // Define transitions: (child_node, suffix, input_style, output_style)
            // Suffix indicates input/output pair: tt (true/true), tf (true/false), ft (false/true), ff (false/false)
            let transitions = [
                (node.x11, "tt", one_edge_style, one_edge_style), // input=t(1), output=t(1)
                (node.x10, "tf", one_edge_style, zero_edge_style), // input=t(1), output=f(0)
                (node.x01, "ft", zero_edge_style, one_edge_style), // input=f(0), output=t(1)
                (node.x00, "ff", zero_edge_style, zero_edge_style), // input=f(0), output=f(0)
            ];

            for &(child_node, suffix, input_style, output_style) in &transitions {
                // Only draw the edge and intermediate node if the child is alive OR is node 0 (the sink)
                let child_is_alive = liveness_map.get(&child_node).copied().unwrap_or(false);
                if child_is_alive {
                    let inter_id = format!("inter_{}_{}", index.0, suffix);

                    // Define the unique intermediate node
                    dot_string
                        .push_str(&format!("  {} [{}];\n", inter_id, intermediate_node_style));

                    // Edge from parent to intermediate node (style based on input var)
                    dot_string.push_str(&format!(
                        "  node{} -> {} [{}];\n",
                        index.0, inter_id, input_style
                    ));

                    // Edge from intermediate node to child node (style based on output var)
                    dot_string.push_str(&format!(
                        "  {} -> node{} [{}];\n",
                        inter_id, child_node.0, output_style
                    ));

                    // Add child to the set for recursion (if not already visited)
                    // The recursive call itself checks liveness and visited status
                    children_to_recurse.insert(child_node);
                }
            }

            // Recurse for each unique child discovered that needs further expansion
            for child_index in children_to_recurse {
                generate_dot_recursive(child_index, store, dot_string, visited, liveness_map);
            }
        }
    }
}

/// Renders the SPP rooted at `index` into an SVG file using Graphviz `dot`.
///
/// Creates `output_dir` if it doesn't exist.
/// Generates `spp_<index>.dot` (intermediate) and `spp_<index>.svg` inside `output_dir`.
pub fn render_spp(index: SPP, store: &SPPstore, output_dir: &Path) -> Result<()> {
    // Ensure the output directory exists
    fs::create_dir_all(output_dir).map_err(|e| {
        Error::new(
            ErrorKind::Other,
            format!("Failed to create output directory {:?}: {}", output_dir, e),
        )
    })?;

    // Precompute liveness information
    let mut liveness_map = HashMap::new();
    compute_liveness(index, store, &mut liveness_map);

    let mut dot_content = String::from("digraph SPP {\n  rankdir=TB; // Top-to-bottom layout\n");
    let mut visited = HashSet::new();
    // Start the recursive DOT generation, passing the liveness map
    generate_dot_recursive(index, store, &mut dot_content, &mut visited, &liveness_map);
    dot_content.push_str("}\n");

    // Define file paths using the SPP index for uniqueness
    let dot_path = output_dir.join(format!("spp_{}.dot", index.0));
    let svg_path = output_dir.join(format!("spp_{}.svg", index.0));

    // Write the generated DOT content to a file
    fs::write(&dot_path, &dot_content).map_err(|e| {
        Error::new(
            ErrorKind::Other,
            format!("Failed to write DOT file to {:?}: {}", dot_path, e),
        )
    })?;

    // Execute the Graphviz 'dot' command to generate SVG from DOT
    let output = Command::new("dot")
        .arg("-Tsvg")
        .arg(dot_path.as_os_str())
        .arg("-o")
        .arg(svg_path.as_os_str())
        .output() // Use output() to capture stderr for better error reporting
        .map_err(|e| {
            Error::new(
                ErrorKind::NotFound, // Indicates 'dot' might not be installed/in PATH
                format!(
                    "Failed to execute 'dot' command. Is Graphviz installed and in PATH? Error: {}",
                    e
                ),
            )
        })?;

    // Check if the 'dot' command executed successfully
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Optionally remove the intermediate .dot file even on failure
        // if dot_path.exists() { fs::remove_file(&dot_path)?; }
        return Err(Error::new(
            ErrorKind::Other,
            format!(
                "Graphviz 'dot' command failed with status: {}. Stderr: {}",
                output.status,
                stderr.trim()
            ),
        ));
    }

    println!("Successfully generated SPP visualization: {:?}", svg_path);

    // Optionally remove the intermediate .dot file after successful SVG generation
    if dot_path.exists() {
        // Best effort removal, ignore error if it fails
        let _ = fs::remove_file(&dot_path);
    }

    Ok(())
}

/// Renders the Aut automaton into visualizations and an HTML report.
///
/// This function:
/// 1. Explores all reachable states starting from the given root state
/// 2. Renders all SPPs involved in transitions
/// 3. Renders all SPPs from epsilon outputs
/// 4. Creates a graphviz representation of the automaton
/// 5. Generates an HTML report that includes all visualizations
pub fn render_aut(root_state: usize, aut: &mut Aut, output_dir: &Path) -> Result<()> {
    // Ensure the output directory exists
    fs::create_dir_all(output_dir)?;

    // Set up tracking structures
    let mut visited_states = HashSet::new();
    let mut states_to_process = vec![root_state];
    let mut transitions = Vec::new();
    let mut spp_ids = HashSet::new();
    let mut state_expressions = HashMap::new();

    // Explore the automaton
    while let Some(state) = states_to_process.pop() {
        if !visited_states.insert(state) {
            continue; // Skip if already visited
        }

        // Get expression string for this state
        let expr_string = aut.state_to_string(state);
        state_expressions.insert(state, expr_string);

        // Get epsilon for this state
        let epsilon_spp = aut.epsilon(state);
        spp_ids.insert(epsilon_spp);

        // Collect SPPs from the expression
        aut.collect_spps(state, &mut spp_ids);

        // Get transitions (delta) for this state
        let delta = aut.delta(state);
        for (&target_state, &spp) in delta.get_transitions() {
            transitions.push((state, target_state, spp));
            spp_ids.insert(spp);

            // Add target state to processing queue if not already visited
            if !visited_states.contains(&target_state) {
                states_to_process.push(target_state);
            }
        }
    }

    // Render all SPPs
    for spp_id in &spp_ids {
        render_spp(*spp_id, &aut.spp_store(), output_dir)?;
    }

    // Generate dot file for the automaton
    let dot_path = output_dir.join("automaton.dot");
    let svg_path = output_dir.join("automaton.svg");

    let mut dot_content = String::from("digraph Automaton {\n  rankdir=LR;\n");

    // Add nodes with clickable SPP references
    for state in &visited_states {
        let epsilon_spp = aut.epsilon(*state);
        let unknown = String::from("Unknown");
        let expr = state_expressions.get(state).unwrap_or(&unknown);

        // Format the expression to include HTML-like labels
        // This will make SPP references clickable in the SVG
        let spp_pattern = regex::Regex::new(r"SPP\((\d+)\)").unwrap();

        // First, escape the entire expression for HTML
        let html_safe_expr = html_escape(expr);

        // Then replace the SPP references with properly formatted HTML
        let expr_with_links = spp_pattern.replace_all(&html_safe_expr, |caps: &regex::Captures| {
            let spp_id = &caps[1];
            format!("<FONT COLOR=\"#3498db\"><U>SPP({})</U></FONT>", spp_id)
        });

        // Create the node with HTML-like label
        dot_content.push_str(&format!(
            "  node{} [label=<{} ε:{}<BR/>{}>; shape=box; style=rounded];\n",
            state, state, epsilon_spp, expr_with_links
        ));
    }

    // Add edges
    for (src, dst, spp) in &transitions {
        let edge_label = format!("{}", spp);
        dot_content.push_str(&format!(
            "  node{} -> node{} [label=\"{}\"];\n",
            src, dst, edge_label
        ));
    }

    dot_content.push_str("}\n");

    // Write dot file
    fs::write(&dot_path, &dot_content)?;

    // Generate SVG from dot
    let output = Command::new("dot")
        .arg("-Tsvg")
        .arg(&dot_path)
        .arg("-o")
        .arg(&svg_path)
        .output()?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(Error::new(
            ErrorKind::Other,
            format!("Graphviz 'dot' command failed: {}", stderr.trim()),
        ));
    }

    // Generate HTML report
    let html_path = output_dir.join("report.html");
    let mut html_content = String::from(
        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Automaton Visualization</title>
    <style>
        :root {
            --primary-color: #2c3e50;
            --secondary-color: #34495e;
            --accent-color: #3498db;
            --background-color: #fff;
            --card-color: #ffffff;
            --hover-color: #eef7fc;
            --border-color: #e0e0e0;
            --text-color: #333333;
        }
        body {
            font-family: 'Linux Libertine', 'Times New Roman', Times, serif;
            margin: 0;
            padding: 0;
            background-color: var(--background-color);
            color: var(--text-color);
            line-height: 1.5;
            max-width: 1100px;
            margin: 0 auto;
            padding: 10px;
        }
        header {
            margin-bottom: 0.75rem;
        }
        h1, h2, h3 {
            color: var(--primary-color);
            font-weight: normal;
            margin-top: 0.25rem;
            margin-bottom: 0.25rem;
        }
        h1 {
            font-size: 1.6rem;
        }
        h2 {
            font-size: 1.3rem;
            margin-top: 1rem;
        }
        .visualization {
            max-width: 100%;
            height: auto;
            display: block;
            margin: 0 auto;
        }
        .flex-container {
            display: flex;
            flex-wrap: wrap;
            gap: 10px;
            justify-content: flex-start;
            align-items: stretch;
        }
        .spp-row {
            display: flex;
            flex-wrap: wrap;
            width: 100%;
            gap: 10px;
            margin-bottom: 10px;
        }
        .spp-card {
            display: flex;
            flex-direction: column;
            background-color: var(--card-color);
            border: 1px solid var(--border-color);
            padding: 5px;
            flex: 0 1 auto;
            max-width: 200px;
        }
        .spp-title {
            font-size: 1rem;
            font-weight: bold;
            margin-bottom: 3px;
            color: var(--primary-color);
            padding-bottom: 2px;
        }
        .spp-img-container {
            flex-grow: 1;
            display: flex;
            align-items: center;
            justify-content: center;
            overflow: hidden;
        }
        .spp-img {
            max-width: 100%;
            transform: scale(0.5);
            transform-origin: center;
        }
        .tooltip {
            position: absolute;
            visibility: hidden;
            background-color: white;
            border-radius: 2px;
            border: 1px solid #ddd;
            box-shadow: 0 0 5px rgba(0, 0, 0, 0.1);
            padding: 8px;
            z-index: 1000;
            max-width: 300px;
            transition: opacity 0.3s;
            opacity: 0;
            font-family: sans-serif;
            font-size: 0.9rem;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 0.5rem 0;
            font-size: 0.9rem;
        }
        th, td {
            padding: 6px 8px;
            text-align: left;
            border: 1px solid var(--border-color);
        }
        th {
            background-color: var(--secondary-color);
            color: white;
            font-weight: normal;
        }
        tr:nth-child(even) {
            background-color: var(--hover-color);
        }
        a {
            color: var(--accent-color);
            text-decoration: none;
        }
        a:hover {
            text-decoration: underline;
        }
        .modal {
            display: none;
            position: fixed;
            z-index: 1000;
            left: 0;
            top: 0;
            width: 100%;
            height: 100%;
            background-color: rgba(0, 0, 0, 0.5);
            overflow: auto;
        }
        .modal-content {
            background-color: white;
            margin: 5% auto;
            padding: 15px;
            border: 1px solid var(--border-color);
            max-width: 700px;
            width: 80%;
            position: relative;
        }
        .close-button {
            color: #aaa;
            float: right;
            font-size: 22px;
            font-weight: bold;
            cursor: pointer;
        }
        .close-button:hover {
            color: black;
        }
        .svg-container {
            position: relative;
            width: 100%;
            overflow: auto;
            max-height: 70vh;
            border: 1px solid var(--border-color);
            margin-bottom: 10px;
        }
        /* Highlight effect for edges and nodes */
        .node-highlight {
            filter: drop-shadow(0 0 5px var(--accent-color));
        }
        .edge-highlight {
            stroke-width: 2;
            stroke: var(--accent-color);
        }
        .tooltip-content img {
            max-width: 100%;
            height: auto;
        }
        .state-link, .spp-link {
            cursor: pointer;
            color: var(--accent-color);
            text-decoration: underline;
            display: inline-block;
        }
        .spp-reference {
            display: inline-block;
            color: var(--accent-color);
            font-weight: bold;
            cursor: pointer;
        }
        .expr-text {
            max-width: 500px;
            overflow-wrap: break-word;
            word-wrap: break-word;
            font-family: 'Courier New', monospace;
            font-size: 0.85rem;
            padding: 3px;
            background-color: #f5f5f5;
            border: 1px solid #eee;
        }
        .expr-spp {
            color: var(--accent-color);
            cursor: pointer;
            font-weight: bold;
        }
        .expr-spp:hover {
            text-decoration: underline;
        }
        footer {
            font-size: 0.8rem;
            color: #666;
            text-align: center;
            margin-top: 15px;
            padding-top: 5px;
            border-top: 1px solid var(--border-color);
        }
    </style>
</head>
<body>
    <div class="section">
        <h2>Automaton Structure</h2>
        <div class="svg-container" id="automaton-container">
            <object id="automaton-svg" data="automaton.svg" type="image/svg+xml" class="visualization"></object>
            <div id="tooltip" class="tooltip"></div>
        </div>
    </div>
    
    <div class="section">
        <h2>SPP Visualizations</h2>
        <div class="flex-container" id="spp-container">
"#,
    );

    // Add SPP visualizations
    let mut sorted_spps: Vec<_> = spp_ids.iter().collect();
    sorted_spps.sort();

    // Group SPPs in rows of 4 for even height distribution
    const CARDS_PER_ROW: usize = 4;
    let rows = (sorted_spps.len() + CARDS_PER_ROW - 1) / CARDS_PER_ROW; // Ceiling division

    for row_idx in 0..rows {
        html_content.push_str("            <div class=\"spp-row\">\n");
        let start_idx = row_idx * CARDS_PER_ROW;
        let end_idx = std::cmp::min(start_idx + CARDS_PER_ROW, sorted_spps.len());

        for i in start_idx..end_idx {
            let spp_index = sorted_spps[i];
            let spp_id_str = format!("SPP({})", spp_index.0); // For display and ID
            let spp_file_name = format!("spp_{}.svg", spp_index.0); // For file reference

            // Check if the SPP SVG file exists, render if not
            let spp_svg_path = output_dir.join(&spp_file_name);
            if !spp_svg_path.exists() {
                // Call the public spp_store() method to get an immutable reference to the SPPstore
                if let Err(e) = render_spp(*spp_index, aut.spp_store(), output_dir) {
                    eprintln!("Error rendering SPP {} on-demand: {}", spp_id_str, e);
                    // Optionally add an error message to the HTML
                    html_content.push_str(&format!(
                        "                <div class=\"spp-card error\">Error rendering {}</div>\n",
                        spp_id_str
                    ));
                    continue;
                }
            }

            html_content.push_str(&format!(
                r#"                <div class="spp-card" id="spp-{}-card">
                    <div class="spp-title">SPP {}</div>
                    <div class="spp-img-container"><img class="spp-img" src="{}" alt="SPP {}" loading="lazy"></div>
                </div>
"#,
                spp_id_str, // id: SPP(2)
                spp_id_str, // title: SPP SPP(2)
                spp_file_name, // src: spp_2.svg
                spp_id_str  // alt: SPP SPP(2)
            ));
        }
        html_content.push_str("            </div>\n"); // Close spp-row
    }
    html_content.push_str("        </div>\n    </div>\n"); // Close spp-container and section

    // Add state information table
    html_content.push_str(
        r#"        <div class="section">
            <h2>State Information</h2>
            <table id="state-table">
                <thead>
                    <tr>
                        <th>State</th>
                        <th>Expression</th>
                        <th>Epsilon SPP</th>
                    </tr>
                </thead>
                <tbody>
"#,
    );

    // Add state information rows with clickable SPP references
    let mut state_vec: Vec<_> = visited_states.iter().collect();
    state_vec.sort();
    for state in state_vec {
        let epsilon_spp = aut.epsilon(*state);
        let unknown = String::from("Unknown");
        let expr_string = state_expressions.get(state).unwrap_or(&unknown);

        // Replace SPP(n) with clickable spans
        let expr_with_links = make_spp_clickable(expr_string);

        html_content.push_str(&format!(
            "                <tr>\n                    <td>{}</td>\n                    <td><div class=\"expr-text\">{}</div></td>\n                    <td><span class=\"spp-reference\" data-spp=\"{}\">{}</span></td>\n                </tr>\n",
            state, expr_with_links, epsilon_spp, epsilon_spp
        ));
    }

    html_content.push_str(
        r#"                </tbody>
            </table>
        </div>
        
        <div class="section">
            <h2>Transitions</h2>
            <table id="transitions-table">
                <thead>
                    <tr>
                        <th>From</th>
                        <th>To</th>
                        <th>SPP</th>
                    </tr>
                </thead>
                <tbody>
"#,
    );

    // Add transition rows
    let mut sorted_transitions = transitions.clone();
    sorted_transitions.sort_by_key(|(src, dst, _)| (*src, *dst));
    for (src, dst, spp) in sorted_transitions {
        html_content.push_str(&format!(
            "                <tr>\n                    <td>{}</td>\n                    <td>{}</td>\n                    <td><span class=\"spp-reference\" data-spp=\"{}\">{}</span></td>\n                </tr>\n",
            src, dst, spp, spp
        ));
    }

    html_content.push_str(
        r#"                </tbody>
            </table>
        </div>
        
        <footer>
            Automaton Visualization Report - Generated on <span id="generation-date"></span>
        </footer>
        
        <!-- SPP Modal Dialog -->
        <div id="spp-modal" class="modal">
            <div class="modal-content">
                <span class="close-button">&times;</span>
                <h2 id="modal-title">SPP Visualization</h2>
                <div id="modal-content"></div>
            </div>
        </div>

        <script>
            // Set the generation date
            document.getElementById('generation-date').textContent = new Date().toLocaleDateString();
            
            // Initialize when the document is fully loaded
            document.addEventListener('DOMContentLoaded', function() {
                // SVG manipulation variables
                const svgContainer = document.getElementById('automaton-container');
                const svgObject = document.getElementById('automaton-svg');
                const tooltip = document.getElementById('tooltip');
                let svgDoc = null;
                
                // Modal handling
                const modal = document.getElementById('spp-modal');
                const modalTitle = document.getElementById('modal-title');
                const modalContent = document.getElementById('modal-content');
                const closeButton = document.querySelector('.close-button');
                
                // Close the modal when clicking the close button or outside
                closeButton.onclick = function() {
                    modal.style.display = 'none';
                };
                
                window.onclick = function(event) {
                    if (event.target === modal) {
                        modal.style.display = 'none';
                    }
                };
                
                // SPP reference click handlers
                document.querySelectorAll('.spp-reference, .expr-spp').forEach(ref => {
                    ref.addEventListener('click', function() {
                        const sppId = this.getAttribute('data-spp');
                        showSPPModal(sppId);
                    });
                });
                
                // Function to show SPP in a modal
                function showSPPModal(sppId) { // sppId is in format "SPP(X)" or just "X" from node labels
                    let numericSppId = sppId;
                    if (sppId.startsWith('SPP(') && sppId.endsWith(')')) {
                        numericSppId = sppId.substring(4, sppId.length - 1);
                    }
                    modalTitle.textContent = \`SPP \${sppId} Visualization\`; // Display original sppId like SPP(5)
                    modalContent.innerHTML = \`<img src="spp_\${numericSppId}.svg" alt="SPP \${sppId}" style="max-width: 100%;">\`;
                    modal.style.display = 'block';
                }
                
                // Wait for SVG to load
                svgObject.addEventListener('load', function() {
                    // Get access to the SVG document
                    svgDoc = svgObject.contentDocument;
                    
                    // Process all nodes in the SVG
                    const nodes = svgDoc.querySelectorAll('[id^="node"]');
                    nodes.forEach(node => {
                        setupNodeInteraction(node);
                    });
                    
                    // Process all edges (paths) in the SVG
                    const edges = svgDoc.querySelectorAll('path, polygon');
                    edges.forEach(edge => {
                        if (edge.parentElement && edge.parentElement.tagName === 'g' && 
                            edge.parentElement.getAttribute('class') === 'edge') {
                            setupEdgeInteraction(edge);
                        }
                    });
                    
                    // Make SPP references in SVG node labels clickable
                    makeNodeLabelSPPsClickable(svgDoc);
                });
                
                // Make SPP references in SVG node labels clickable
                function makeNodeLabelSPPsClickable(svgDoc) {
                    // Find all text elements that might contain SPP references
                    const textElements = svgDoc.querySelectorAll('text');
                    
                    textElements.forEach(textEl => {
                        // Check if this is a text element that contains SPP reference
                        const tspans = textEl.querySelectorAll('tspan');
                        
                        tspans.forEach(tspan => {
                            // Check if this tspan contains an underlined SPP reference
                            if (tspan.innerHTML && tspan.innerHTML.includes('SPP(')) {
                                // Look for colored text which indicates our SPP references
                                const sppMatch = tspan.innerHTML.match(/SPP\((\d+)\)/);
                                if (sppMatch) {
                                    const sppId = sppMatch[1];
                                    
                                    // Make tspan clickable
                                    tspan.style.cursor = 'pointer';
                                    tspan.style.textDecoration = 'underline';
                                    
                                    // Remove any existing click listeners on the parent node
                                    const parentNode = findParentNode(tspan);
                                    if (parentNode) {
                                        // Store the original click handler
                                        const originalClickHandler = parentNode.onclick;
                                        parentNode.onclick = null;
                                        
                                        // Create a new click handler for the node that checks the target
                                        parentNode.addEventListener('click', function(e) {
                                            // If click was on a tspan containing SPP, don't show epsilon
                                            if (e.target.tagName === 'tspan' && 
                                                e.target.innerHTML && 
                                                e.target.innerHTML.includes('SPP(')) {
                                                return;
                                            }
                                            
                                            // Extract epsilon SPP from the label
                                            const labelContent = parentNode.textContent;
                                            const epsilonMatch = labelContent.match(/ε:(\d+)/);
                                            if (epsilonMatch) {
                                                const epsilonSpp = epsilonMatch[1];
                                                showSPPModal(epsilonSpp);
                                            }
                                        });
                                    }
                                    
                                    // Add click handler to the tspan
                                    tspan.addEventListener('click', function(e) {
                                        showSPPModal(sppId);
                                        e.stopPropagation();
                                    });
                                }
                            }
                        });
                    });
                }
                
                // Helper function to find the parent node element
                function findParentNode(element) {
                    let current = element;
                    while (current) {
                        // Move up the DOM tree
                        current = current.parentElement;
                        
                        // Check if we've found a node element
                        if (current && current.id && current.id.startsWith('node')) {
                            return current;
                        }
                        
                        // If we hit the svg element, stop searching
                        if (current && current.tagName === 'svg') {
                            break;
                        }
                    }
                    return null;
                }
                
                // Setup node interaction (hover, click)
                function setupNodeInteraction(node) {
                    // Extract state ID and epsilon SPP from the node's title or label
                    const nodeId = node.id;
                    const stateId = nodeId.replace('node', '');
                    
                    // Find the node's label element
                    const labelText = node.querySelector('text');
                    
                    if (labelText) {
                        // We'll now handle node clicks differently to accommodate SPP references
                        // The click handler is added in makeNodeLabelSPPsClickable
                        
                        // Setup hover effects
                        node.addEventListener('mouseover', function(e) {
                            this.classList.add('node-highlight');
                            // Extract epsilon SPP from the label
                            const labelContent = labelText.textContent;
                            const epsilonMatch = labelContent.match(/ε:(\d+)/);
                            const epsilonSpp = epsilonMatch ? epsilonMatch[1] : null;
                            
                            showTooltip(e, 
                                `<div class="tooltip-content">
                                    <strong>State ${stateId}</strong>
                                    ${epsilonSpp ? `<br>Epsilon SPP: <span class=\"spp-link\" data-spp=\"${epsilonSpp}\">${epsilonSpp}</span>` : ''}
                                    <br><br>
                                    ${epsilonSpp ? `<img src="spp_${epsilonSpp}.svg" alt="SPP ${epsilonSpp}" width="200">` : ''}
                                </div>`
                            );
                            e.stopPropagation();
                        });
                        
                        node.addEventListener('mousemove', function(e) {
                            updateTooltipPosition(e);
                            e.stopPropagation();
                        });
                        
                        node.addEventListener('mouseout', function(e) {
                            this.classList.remove('node-highlight');
                            hideTooltip();
                            e.stopPropagation();
                        });
                    }
                }
                
                // Setup edge interaction
                function setupEdgeInteraction(edge) {
                    const parentG = edge.parentElement;
                    if (!parentG) return;
                    
                    const titleEl = parentG.querySelector('title');
                    if (!titleEl) return;
                    
                    // Extract edge information from the title element (format is usually "node1->node2")
                    const title = titleEl.textContent;
                    const edgeMatch = title.match(/node(\d+)->node(\d+)/);
                    if (!edgeMatch) return;
                    
                    const fromState = edgeMatch[1];
                    const toState = edgeMatch[2];
                    
                    // Find the label text for this edge
                    const edgeLabel = parentG.querySelector('text');
                    let sppId = null;
                    
                    if (edgeLabel) {
                        sppId = edgeLabel.textContent;
                        
                        // Make the edge label itself clickable
                        edgeLabel.style.cursor = 'pointer';
                        edgeLabel.addEventListener('click', function(e) {
                            if (sppId) {
                                showSPPModal(sppId);
                                e.stopPropagation();
                            }
                        });
                    }
                    
                    // Set up hover effects for the whole edge
                    parentG.addEventListener('mouseover', function(e) {
                        parentG.querySelectorAll('path, polygon').forEach(el => {
                            el.classList.add('edge-highlight');
                        });
                        
                        if (sppId) {
                            showTooltip(e, 
                                `<div class="tooltip-content">
                                    <strong>Transition</strong><br>
                                    From: ${fromState} → To: ${toState}<br>
                                    SPP: <span class="spp-link" data-spp="${sppId}">${sppId}</span>
                                    <br><br>
                                    <img src="spp_${sppId}.svg" alt="SPP ${sppId}" width="200">
                                </div>`
                            );
                        }
                        e.stopPropagation();
                    });
                    
                    parentG.addEventListener('mousemove', function(e) {
                        updateTooltipPosition(e);
                        e.stopPropagation();
                    });
                    
                    parentG.addEventListener('mouseout', function(e) {
                        parentG.querySelectorAll('path, polygon').forEach(el => {
                            el.classList.remove('edge-highlight');
                        });
                        hideTooltip();
                        e.stopPropagation();
                    });
                    
                    // Click to show detailed SPP for the whole edge
                    parentG.addEventListener('click', function(e) {
                        if (sppId) {
                            showSPPModal(sppId);
                            e.stopPropagation();
                        }
                    });
                }
                
                // Show tooltip at the event position
                function showTooltip(event, html) {
                    tooltip.innerHTML = html;
                    updateTooltipPosition(event);
                    tooltip.style.visibility = 'visible';
                    tooltip.style.opacity = '1';
                    
                    // Add click handlers to any SPP links in the tooltip
                    tooltip.querySelectorAll('.spp-link').forEach(link => {
                        link.addEventListener('click', function(e) {
                            const sppId = this.getAttribute('data-spp');
                            showSPPModal(sppId);
                            e.stopPropagation();
                        });
                    });
                }
                
                // Update tooltip position based on mouse coordinates
                function updateTooltipPosition(event) {
                    const rect = svgContainer.getBoundingClientRect();
                    
                    // Adjust position to keep tooltip within visible area
                    let left = event.clientX - rect.left + 10;
                    let top = event.clientY - rect.top + 10;
                    
                    // Ensure the tooltip stays within the container bounds
                    const tooltipRect = tooltip.getBoundingClientRect();
                    if (left + tooltipRect.width > rect.width) {
                        left = event.clientX - rect.left - tooltipRect.width - 10;
                    }
                    if (top + tooltipRect.height > rect.height) {
                        top = event.clientY - rect.top - tooltipRect.height - 10;
                    }
                    
                    tooltip.style.left = `${left}px`;
                    tooltip.style.top = `${top}px`;
                }
                
                // Hide the tooltip
                function hideTooltip() {
                    tooltip.style.opacity = '0';
                    setTimeout(() => {
                        tooltip.style.visibility = 'hidden';
                    }, 300);
                }
            });
        </script>
    </body>
</html>
"#,
    );

    // Write HTML file
    let mut file = File::create(html_path)?;
    file.write_all(html_content.as_bytes())?;

    println!(
        "Successfully generated automaton visualization report at: {:?}",
        output_dir.join("report.html")
    );

    Ok(())
}

/// Helper function to make SPP references in expressions clickable
fn make_spp_clickable(expr: &str) -> String {
    // First HTML escape the expression
    let escaped_expr = html_escape(expr);

    // Use regex to find all instances of SPP(number)
    let spp_pattern = regex::Regex::new(r"SPP\((\d+)\)").unwrap();

    let result = spp_pattern.replace_all(&escaped_expr, |caps: &regex::Captures| {
        let spp_id = &caps[1];
        format!(
            "<span class=\"expr-spp\" data-spp=\"{}\">{}</span>",
            spp_id, &caps[0]
        )
    });

    result.to_string()
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from the parent module (viz.rs)
    use crate::aut::Aut;
    use crate::expr::Expr;
    use crate::spp::SPPstore; // Need the store to create and manage SPPs
    use crate::spp::Var;
    use std::path::Path;

    // Define a constant for the number of variables for test SPPs
    const TEST_NUM_VARS: Var = 3; // Adjust as needed for complexity
    // Define the output directory for test visualizations
    const TEST_OUTPUT_DIR: &str = "out/spptest";
    const TEST_AUT_OUTPUT_DIR: &str = "out/auttest";

    #[test]
    fn test_render_random_spp() {
        // Create a new SPP store
        let mut store = SPPstore::new(TEST_NUM_VARS);

        // Generate a random SPP
        // Note: rand() might require the `rand` crate feature/dependency if not already enabled.
        // Assuming spp.rs includes `use rand;` or similar for `rand::random()`.
        let random_spp_index = store.rand();

        println!("Generated random SPP index: {}", random_spp_index);

        // Define the output path
        let output_dir = Path::new(TEST_OUTPUT_DIR);

        // Attempt to render the SPP
        // Use expect to fail the test immediately if render_spp returns an error
        render_spp(random_spp_index, &store, output_dir).expect("Failed to render the random SPP");

        // The primary check is manual inspection of the SVG file in TEST_OUTPUT_DIR
        println!(
            "Test finished. Please check the generated SVG file for SPP {} in the '{}' directory.",
            random_spp_index, TEST_OUTPUT_DIR
        );
    }

    #[test]
    fn test_render_specific_spps() {
        // Test rendering 0 and 1
        let store = SPPstore::new(1); // Only 1 var needed for 0/1
        let output_dir = Path::new(TEST_OUTPUT_DIR);

        // Render SPP 0 (Zero)
        render_spp(store.zero, &store, output_dir).expect("Failed to render SPP 0 (Zero)");
        println!("Rendered SPP 0 (Zero) to {}", TEST_OUTPUT_DIR);

        // Render SPP 1 (One)
        render_spp(store.one, &store, output_dir).expect("Failed to render SPP 1 (One)");
        println!("Rendered SPP 1 (One) to {}", TEST_OUTPUT_DIR);
    }

    #[test]
    fn test_render_automaton() {
        // Create a simple automaton for testing
        let mut aut = Aut::new(TEST_NUM_VARS);

        // Create a few simple expressions using factory methods
        let field0 = 0;
        let field1 = 1;
        let value1 = true;

        let test_f1_v1 = Expr::test(field0, value1);
        let test_f2_v1 = Expr::test(field1, value1);
        let assign_f1_v1 = Expr::assign(field0, value1);

        // Create a sequence: test(f1=v1); test(f2=v1); assign(f1=v1)
        let seq1 = Expr::sequence(test_f1_v1.clone(), test_f2_v1.clone());
        let seq2 = Expr::sequence(seq1, assign_f1_v1);

        // Create a star operation: (test(f1=v1))*
        let star = Expr::star(test_f1_v1);

        // Create a union of the two: seq + star
        let union = Expr::union(seq2, star);
        let root_state = aut.expr_to_state(&union);

        // Render the automaton
        let output_dir = Path::new(TEST_AUT_OUTPUT_DIR);
        render_aut(root_state, &mut aut, output_dir).expect("Failed to render the test automaton");

        println!(
            "Test finished. Please check the generated automaton report in the '{}' directory.",
            TEST_AUT_OUTPUT_DIR
        );
    }
}
