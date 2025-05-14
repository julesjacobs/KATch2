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

#[derive(Serialize, Deserialize, Debug)]
pub struct AnalysisResult {
    pub status: String, 
    pub error_message: Option<String>,
    // pub parsed_expression: Option<String>, // Can be added later
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
pub fn analyze_expression(expr_str: &str) -> JsValue {
    if expr_str.trim().is_empty() {
        return serde_wasm_bindgen::to_value(&AnalysisResult {
            status: "Waiting for input...".to_string(),
            error_message: None,
        }).unwrap_or(JsValue::NULL);
    }

    match parser::parse_expressions(expr_str) {
        Ok(expressions) => {
            if expressions.is_empty() {
                return serde_wasm_bindgen::to_value(&AnalysisResult {
                    status: "No expression found".to_string(),
                    error_message: None,
                }).unwrap_or(JsValue::NULL);
            }
            
            let first_expr = &expressions[0];

            // Determine num_fields (assuming Expr has a method for this)
            // This might need adjustment based on actual Expr structure
            let num_fields = first_expr.num_fields(); 
            let mut aut = aut::Aut::new(num_fields);
            let state_id = aut.expr_to_state(first_expr);
            
            let is_empty_result = if aut.is_empty(state_id) {
                "Empty".to_string()
            } else {
                "Non-empty".to_string()
            };

            serde_wasm_bindgen::to_value(&AnalysisResult {
                status: is_empty_result,
                error_message: None,
            }).unwrap_or(JsValue::NULL)
        }
        Err(parse_err) => {
            serde_wasm_bindgen::to_value(&AnalysisResult {
                status: "Syntax Error".to_string(),
                error_message: Some(parse_err),
            }).unwrap_or(JsValue::NULL)
        }
    }
}
