use crate::spp::{SPP, SPPnode, SPPstore};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{Error, ErrorKind, Result};
use std::path::Path;
use std::process::Command;

/// Computes which nodes in the SPP DAG reachable from `root_index` can reach the `1` node.
/// Uses memoization via the `liveness_map` to handle the DAG structure efficiently.
/// Returns true if `root_index` is alive, false otherwise.
fn compute_liveness(
    index: SPP,
    store: &SPPstore,
    liveness_map: &mut HashMap<SPP, bool>,
) -> bool {
    // Check memoization table first
    if let Some(&is_alive) = liveness_map.get(&index) {
        return is_alive;
    }

    // Base cases
    let is_alive = match index {
        0 => false, // Node 0 is never considered alive (cannot reach 1)
        1 => true,  // Node 1 is always alive
        _ => {      // Internal node
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
    // Only proceed if the node is alive (or is the special case 0, which we always draw if reached)
    if !liveness_map.get(&index).copied().unwrap_or(false) {
        return;
    }

    // Avoid infinite loops and redundant processing
    if !visited.insert(index) {
        return;
    }

    match index {
        0 => {
            // Style for the bottom node (⊥) - always drawn if visited
            dot_string.push_str(&format!(
                "  node{} [label=\"⊥\", shape=box, style=filled, fillcolor=lightcoral];\n",
                index
            ));
        }
        1 => {
            // Style for the top node (⊤) - always drawn if visited (and alive)
            dot_string.push_str(&format!(
                "  node{} [label=\"⊤\", shape=box, style=filled, fillcolor=lightgreen];\n",
                index
            ));
        }
        _ => {
            // Internal node (guaranteed to be alive at this point by the initial check)
            let node = store.get(index);

            // Style for internal nodes: small black circle
            dot_string.push_str(&format!(
                "  node{} [label=\"\", shape=circle, style=filled, fillcolor=black, width=0.1, height=0.1];\n",
                index
            ));

            // Group edges by target child and combine labels
            let mut edges_by_child = HashMap::<SPP, Vec<&str>>::new();
            let potential_edges = [
                (node.x00, "tt"),
                (node.x01, "tf"),
                (node.x10, "ft"),
                (node.x11, "ff"),
            ];

            for (child_index, label) in potential_edges.iter() {
                // Only consider edges pointing to alive children (or 0)
                if liveness_map.get(child_index).copied().unwrap_or(false) {
                    edges_by_child.entry(*child_index).or_default().push(label);
                }
            }

            // Draw merged edges and recurse for unique children
            let mut children_to_recurse = HashSet::new(); // Keep track of children already recursed into
            for (child_index, labels) in edges_by_child.iter() {
                let combined_label = labels.join(",");
                dot_string.push_str(&format!(
                    "  node{} -> node{} [label=\"{}\", labelangle=0];\n",
                    index, child_index, combined_label
                ));
                 // Collect unique children for recursion after drawing all edges from this node
                children_to_recurse.insert(*child_index);
            }

            // Recurse for each unique, alive child discovered
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
                output.status, stderr.trim()
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

#[cfg(test)]
mod tests {
    use super::*; // Import items from the parent module (viz.rs)
    use crate::spp::SPPstore; // Need the store to create and manage SPPs
    use std::path::Path;
    use crate::spp::Var;

    // Define a constant for the number of variables for test SPPs
    const TEST_NUM_VARS: Var = 3; // Adjust as needed for complexity
    // Define the output directory for test visualizations
    const TEST_OUTPUT_DIR: &str = "out/spptest";

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
        render_spp(random_spp_index, &store, output_dir)
            .expect("Failed to render the random SPP");

        // The primary check is manual inspection of the SVG file in TEST_OUTPUT_DIR
        println!(
            "Test finished. Please check the generated SVG file for SPP {} in the '{}' directory.",
            random_spp_index,
            TEST_OUTPUT_DIR
        );
    }

    #[test]
    fn test_render_specific_spps() {
        // Test rendering 0 and 1
        let mut store = SPPstore::new(1); // Only 1 var needed for 0/1
        let output_dir = Path::new(TEST_OUTPUT_DIR);

        // Render SPP 0 (Zero)
        render_spp(store.zero, &store, output_dir)
            .expect("Failed to render SPP 0 (Zero)");
        println!("Rendered SPP 0 (Zero) to {}", TEST_OUTPUT_DIR);

        // Render SPP 1 (One)
        render_spp(store.one, &store, output_dir)
            .expect("Failed to render SPP 1 (One)");
        println!("Rendered SPP 1 (One) to {}", TEST_OUTPUT_DIR);
    }
} 