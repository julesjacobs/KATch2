use crate::spp::{SPP, SPPstore};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{Error, ErrorKind, Result};
use std::path::Path;
use std::process::Command;
use crate::aut::Aut;
use std::fs::File;
use std::io::Write;
use crate::expr::Expr;

/// Computes which nodes in the SPP DAG reachable from `root_index` can reach the `1` node.
/// Uses memoization via the `liveness_map` to handle the DAG structure efficiently.
/// Returns true if `root_index` is alive, false otherwise.
fn compute_liveness(index: SPP, store: &SPPstore, liveness_map: &mut HashMap<SPP, bool>) -> bool {
    // Check memoization table first
    if let Some(&is_alive) = liveness_map.get(&index) {
        return is_alive;
    }

    // Base cases
    let is_alive = match index {
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

    match index {
        0 => {
            // Style for the bottom node (⊥)
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index
            ));
        }
        1 => {
            // Style for the top node (⊤)
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index
            ));
        }
        _ => {
            // Internal node (guaranteed to be alive at this point)
            let node = store.get(index);

            // Style for internal nodes: small black circle
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index
            ));

            let zero_edge_style = "style=dashed, arrowhead=none, color=red"; // Corresponds to 'f' or 0 output/input
            let one_edge_style = "style=solid, arrowhead=none, color=green"; // Corresponds to 't' or 1 output/input
            let intermediate_node_style =
                "shape=point, width=0.00, height=0.00, style=filled, fillcolor=white";

            let mut children_to_recurse = HashSet::new();

            // Define transitions: (child_node, suffix, input_style, output_style)
            // Suffix indicates input/output pair: tt (true/true), tf (true/false), ft (false/true), ff (false/false)
            let transitions = [
                (node.x00, "tt", one_edge_style, one_edge_style), // input=t(1), output=t(1)
                (node.x01, "tf", one_edge_style, zero_edge_style), // input=t(1), output=f(0)
                (node.x10, "ft", zero_edge_style, one_edge_style), // input=f(0), output=t(1)
                (node.x11, "ff", zero_edge_style, zero_edge_style), // input=f(0), output=f(0)
            ];

            for &(child_node, suffix, input_style, output_style) in &transitions {
                // Only draw the edge and intermediate node if the child is alive OR is node 0 (the sink)
                let child_is_alive = liveness_map.get(&child_node).copied().unwrap_or(false);
                if child_is_alive {
                    let inter_id = format!("inter_{}_{}", index, suffix);

                    // Define the unique intermediate node
                    dot_string
                        .push_str(&format!("  {} [{}];\n", inter_id, intermediate_node_style));

                    // Edge from parent to intermediate node (style based on input var)
                    dot_string.push_str(&format!(
                        "  node{} -> {} [{}];\n",
                        index, inter_id, input_style
                    ));

                    // Edge from intermediate node to child node (style based on output var)
                    dot_string.push_str(&format!(
                        "  {} -> node{} [{}];\n",
                        inter_id, child_node, output_style
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
    let dot_path = output_dir.join(format!("spp_{}.dot", index));
    let svg_path = output_dir.join(format!("spp_{}.svg", index));

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
    
    // Explore the automaton
    while let Some(state) = states_to_process.pop() {
        if !visited_states.insert(state) {
            continue; // Skip if already visited
        }
        
        // Get epsilon for this state
        let epsilon_spp = aut.epsilon(state);
        spp_ids.insert(epsilon_spp);
        
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
    
    // Add nodes
    for state in &visited_states {
        let epsilon_spp = aut.epsilon(*state);
        dot_content.push_str(&format!(
            "  node{} [label=\"{} ε:{}\", shape=circle];\n",
            state, state, epsilon_spp
        ));
    }
    
    // Add edges
    for (src, dst, spp) in &transitions {
        dot_content.push_str(&format!(
            "  node{} -> node{} [label=\"{}\"];\n",
            src, dst, spp
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
            format!("Graphviz 'dot' command failed: {}", stderr.trim())
        ));
    }
    
    // Generate HTML report
    let html_path = output_dir.join("report.html");
    let mut html_content = String::from(
        r#"<!DOCTYPE html>
<html>
<head>
    <title>Automaton Visualization Report</title>
    <style>
        body { font-family: Arial, sans-serif; margin: 0; padding: 20px; }
        h1, h2 { color: #333; }
        .section { margin-bottom: 30px; }
        .flex-container { display: flex; flex-wrap: wrap; gap: 20px; }
        .visualization { 
            border: 1px solid #ddd; 
            padding: 10px; 
            border-radius: 5px;
            margin-bottom: 15px;
        }
        table { border-collapse: collapse; width: 100%; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        img { max-width: 100%; }
    </style>
</head>
<body>
    <h1>Automaton Visualization Report</h1>
    
    <div class="section">
        <h2>Automaton Structure</h2>
        <div class="visualization">
            <img src="automaton.svg" alt="Automaton Graph">
        </div>
    </div>
    
    <div class="section">
        <h2>State Information</h2>
        <table>
            <tr>
                <th>State</th>
                <th>Epsilon SPP</th>
            </tr>
"#,
    );
    
    // Add state information rows
    let mut state_vec: Vec<_> = visited_states.iter().collect();
    state_vec.sort();
    for state in state_vec {
        let epsilon_spp = aut.epsilon(*state);
        html_content.push_str(&format!(
            "            <tr>\n                <td>{}</td>\n                <td>{} <a href=\"spp_{}.svg\">[View]</a></td>\n            </tr>\n",
            state, epsilon_spp, epsilon_spp
        ));
    }
    
    html_content.push_str(
        r#"        </table>
    </div>
    
    <div class="section">
        <h2>Transitions</h2>
        <table>
            <tr>
                <th>From</th>
                <th>To</th>
                <th>SPP</th>
            </tr>
"#,
    );
    
    // Add transition rows
    let mut sorted_transitions = transitions.clone();
    sorted_transitions.sort_by_key(|(src, dst, _)| (*src, *dst));
    for (src, dst, spp) in sorted_transitions {
        html_content.push_str(&format!(
            "            <tr>\n                <td>{}</td>\n                <td>{}</td>\n                <td>{} <a href=\"spp_{}.svg\">[View]</a></td>\n            </tr>\n",
            src, dst, spp, spp
        ));
    }
    
    html_content.push_str(
        r#"        </table>
    </div>
    
    <div class="section">
        <h2>SPP Visualizations</h2>
        <div class="flex-container">
"#,
    );
    
    // Add SPP visualizations
    let mut sorted_spps: Vec<_> = spp_ids.iter().collect();
    sorted_spps.sort();
    for spp in sorted_spps {
        html_content.push_str(&format!(
            "            <div class=\"visualization\">\n                <h3>SPP {}</h3>\n                <img src=\"spp_{}.svg\" alt=\"SPP {}\">\n            </div>\n",
            spp, spp, spp
        ));
    }
    
    html_content.push_str(
        r#"        </div>
    </div>
</body>
</html>
"#,
    );
    
    // Write HTML file
    let mut file = File::create(html_path)?;
    file.write_all(html_content.as_bytes())?;
    
    println!("Successfully generated automaton visualization report at: {:?}", output_dir.join("report.html"));
    
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*; // Import items from the parent module (viz.rs)
    use crate::spp::SPPstore; // Need the store to create and manage SPPs
    use crate::spp::Var;
    use crate::aut::Aut;
    use crate::expr::Expr;
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
        let mut store = SPPstore::new(1); // Only 1 var needed for 0/1
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
        
        // Convert expressions to automaton states
        let seq_state = aut.expr_to_state(&seq2);
        let star_state = aut.expr_to_state(&star);
        
        // Create a union of the two: seq + star
        let union = Expr::union(seq2, star);
        let root_state = aut.expr_to_state(&union);
        
        // Render the automaton
        let output_dir = Path::new(TEST_AUT_OUTPUT_DIR);
        render_aut(root_state, &mut aut, output_dir).expect("Failed to render the test automaton");
        
        println!("Test finished. Please check the generated automaton report in the '{}' directory.", TEST_AUT_OUTPUT_DIR);
    }
}
