use wasm_bindgen::prelude::*;
use console_error_panic_hook;
use serde::{Serialize, Deserialize};
use serde_wasm_bindgen;

pub mod aut;
pub mod expr;
pub mod parser;
pub mod pre;
pub mod sp;
pub mod spp;
pub mod viz;

/// The result of analyzing a NetKAT expression.
#[derive(Serialize, Deserialize, Debug)]
pub struct AnalysisResult {
    pub status: String,            // e.g., "Empty", "Non-empty", "Syntax Error"
    pub error: Option<parser::ParseErrorDetails>, // Contains error message and optional span
    pub traces: Option<Vec<(Vec<Vec<bool>>, Option<Vec<bool>>)>>, // 5 random traces when expression is non-empty: (input_trace, final_output)
    // We could also include the string representation of the parsed expression here if useful
    // pub parsed_expr_string: Option<String>,
}

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello from Rust, {}!", name)
}

#[wasm_bindgen]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn analyze_expression(
    expr_str: &str, 
    num_traces_opt: Option<usize>, 
    max_trace_length_opt: Option<usize>
) -> JsValue {
    if expr_str.trim().is_empty() {
        return serde_wasm_bindgen::to_value(&AnalysisResult {
            status: "Empty (no input)".to_string(),
            error: None,
            traces: None,
        })
        .unwrap();
    }

    match parser::parse_expressions(expr_str) {
        Ok(expressions) => {
            if expressions.is_empty() {
                // This case should ideally be handled by the parser returning an error
                // or by the initial trim().is_empty() check.
                return serde_wasm_bindgen::to_value(&AnalysisResult {
                    status: "Empty (parsed as no expressions)".to_string(),
                    error: None,
                    traces: None,
                })
                .unwrap();
            }

            // For now, we analyze the first expression if multiple are parsed (e.g. separated by comments)
            // Later, the UI might only allow/send single expressions or handle multiple.
            let first_expr = &expressions[0];
            let num_fields = first_expr.num_fields();
            let mut aut_handler = aut::Aut::new(num_fields);
            let state_id = aut_handler.expr_to_state(first_expr.as_ref());
            let is_empty = aut_handler.is_empty(state_id);

            let (status, traces) = if is_empty {
                ("Empty".to_string(), None)
            } else {
                let mut traces_vec = Vec::new();
                let num_traces_to_generate = num_traces_opt.unwrap_or(5); // Default to 5 traces
                let max_len = max_trace_length_opt.unwrap_or(5); // Default to max length 5
                
                for _ in 0..num_traces_to_generate {
                    if let Some(trace) = aut_handler.random_trace(state_id, max_len) {
                        traces_vec.push(trace);
                    }
                }
                if traces_vec.is_empty() {
                    ("Empty".to_string(), None) // Could still be empty if random_trace fails or policy is very restrictive
                } else {
                    ("Non-empty".to_string(), Some(traces_vec))
                }
            };

            serde_wasm_bindgen::to_value(&AnalysisResult {
                status,
                error: None,
                traces,
            })
            .unwrap()
        }
        Err(err_details) => {
            serde_wasm_bindgen::to_value(&AnalysisResult {
                status: "Syntax Error".to_string(),
                error: Some(err_details),
                traces: None,
            })
            .unwrap()
        }
    }
}
