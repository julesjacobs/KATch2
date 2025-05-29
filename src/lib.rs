use wasm_bindgen::prelude::*;
use console_error_panic_hook;
use serde::{Serialize, Deserialize};
use serde_wasm_bindgen;

pub mod aut;
pub mod desugar;
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

/// The result of analyzing the difference between two NetKAT expressions.
#[derive(Serialize, Deserialize, Debug)]
pub struct DifferenceResult {
    pub expr1_errors: Option<parser::ParseErrorDetails>, // Errors from parsing the first expression (e.g., user's code)
    pub expr2_errors: Option<parser::ParseErrorDetails>, // Errors from parsing the second expression (e.g., target solution)
    pub example_traces: Option<Vec<(Vec<Vec<bool>>, Option<Vec<bool>>)>>, // Traces from (expr1 - expr2)
    // If both errors are None and example_traces is None or Empty, then (expr1 - expr2) is empty.
}

#[wasm_bindgen]
pub fn greet(name: &str) -> String {
    format!("Hello from Rust, {}!", name)
}

#[wasm_bindgen]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

/// Helper function that performs the core analysis logic on expression strings.
/// For single expression analysis, pass "0" for expr2_str.
/// For difference analysis, pass both expression strings.
fn analyze_expressions_internal(
    expr1_str: &str,
    expr2_str: &str,
    num_traces: usize,
    max_trace_length: usize
) -> Result<(bool, Option<Vec<(Vec<Vec<bool>>, Option<Vec<bool>>)>>), (Option<parser::ParseErrorDetails>, Option<parser::ParseErrorDetails>)> {
    let mut expr1_parse_result = None;
    let mut expr2_parse_result = None;

    let parsed_expr1 = match parser::parse_expressions(expr1_str) {
        Ok(mut exprs) => {
            if exprs.is_empty() {
                expr1_parse_result = Some(parser::ParseErrorDetails {
                    message: "Expression 1 is empty or only comments".to_string(),
                    span: None,
                });
                None
            } else {
                Some(exprs.remove(0))
            }
        }
        Err(e) => {
            expr1_parse_result = Some(e);
            None
        }
    };

    let parsed_expr2 = match parser::parse_expressions(expr2_str) {
        Ok(mut exprs) => {
            if exprs.is_empty() {
                expr2_parse_result = Some(parser::ParseErrorDetails {
                    message: "Expression 2 is empty or only comments".to_string(),
                    span: None,
                });
                None
            } else {
                Some(exprs.remove(0))
            }
        }
        Err(e) => {
            expr2_parse_result = Some(e);
            None
        }
    };

    // If either expression failed to parse, return early with error info
    if expr1_parse_result.is_some() || expr2_parse_result.is_some() || parsed_expr1.is_none() || parsed_expr2.is_none() {
        return Err((expr1_parse_result, expr2_parse_result));
    }

    let parsed1 = parsed_expr1.unwrap();
    let parsed2 = parsed_expr2.unwrap();
    
    // Desugar the expressions
    let expr1 = match desugar::desugar(&parsed1) {
        Ok(e) => e,
        Err(err) => {
            expr1_parse_result = Some(parser::ParseErrorDetails {
                message: format!("Desugaring error: {}", err),
                span: None,
            });
            return Err((expr1_parse_result, expr2_parse_result));
        }
    };
    
    let expr2 = match desugar::desugar(&parsed2) {
        Ok(e) => e,
        Err(err) => {
            expr2_parse_result = Some(parser::ParseErrorDetails {
                message: format!("Desugaring error: {}", err),
                span: None,
            });
            return Err((expr1_parse_result, expr2_parse_result));
        }
    };

    // Determine the unified field count
    let expr1_fields = expr1.num_fields();
    let expr2_fields = expr2.num_fields();
    let unified_field_count = std::cmp::max(expr1_fields, expr2_fields);

    // Create the target expression (difference: expr1 - expr2)
    let target_expr = expr::Expr::Difference(
        Box::new(expr1.as_ref().clone()),
        Box::new(expr2.as_ref().clone())
    );

    // Create automaton handler with the unified field count
    let mut aut_handler = aut::Aut::new(unified_field_count);

    // Convert the target expression to an automaton state
    let state_id = aut_handler.expr_to_state(&target_expr);
    let is_empty = aut_handler.is_empty(state_id);

    let traces = if is_empty {
        None
    } else {
        let mut traces_set = std::collections::HashSet::new();
        let max_attempts = num_traces * 10; // Try up to 10x the requested number
        
        for _ in 0..max_attempts {
            if let Some(trace) = aut_handler.random_trace(state_id, max_trace_length) {
                traces_set.insert(trace);
                // Stop early if we have enough unique traces
                if traces_set.len() >= num_traces {
                    break;
                }
            }
        }
        
        if traces_set.is_empty() {
            None
        } else {
            // Convert to vector and sort lexicographically
            let mut traces_vec: Vec<_> = traces_set.into_iter().collect();
            traces_vec.sort();
            Some(traces_vec)
        }
    };

    Ok((is_empty, traces))
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

    match analyze_expressions_internal(expr_str, "0", num_traces_opt.unwrap_or(5), max_trace_length_opt.unwrap_or(5)) {
        Ok((is_empty, traces)) => {
            let status = if is_empty {
                "Analysis result: Drops all packets".to_string()
            } else {
                "Analysis result: Allows traffic".to_string()
            };

            serde_wasm_bindgen::to_value(&AnalysisResult {
                status,
                error: None,
                traces,
            })
            .unwrap()
        }
        Err((expr1_error, _expr2_error)) => {
            // For single expression analysis, we only care about expr1 errors
            serde_wasm_bindgen::to_value(&AnalysisResult {
                status: "Syntax Error".to_string(),
                error: expr1_error,
                traces: None,
            })
            .unwrap()
        }
    }
}

#[wasm_bindgen]
pub fn analyze_difference(
    expr1_str: &str, 
    expr2_str: &str, 
    num_traces_opt: Option<usize>, 
    max_trace_length_opt: Option<usize>
) -> JsValue {
    match analyze_expressions_internal(expr1_str, expr2_str, num_traces_opt.unwrap_or(3), max_trace_length_opt.unwrap_or(5)) {
        Ok((_is_empty, traces)) => {
            serde_wasm_bindgen::to_value(&DifferenceResult {
                expr1_errors: None,
                expr2_errors: None,
                example_traces: traces,
            }).unwrap()
        }
        Err((expr1_errors, expr2_errors)) => {
            serde_wasm_bindgen::to_value(&DifferenceResult {
                expr1_errors,
                expr2_errors,
                example_traces: None,
            }).unwrap()
        }
    }
}

#[cfg(test)]
mod perf_tests {
    use super::*;
    use std::time::Instant;

    #[test]
    fn test_performance_10_bits() {
        let num_vars = 10;
        let mut store = spp::SPPstore::new(num_vars);
        
        println!("Testing performance with {} bits", num_vars);
        
        // Test 1: Basic construction
        let start = Instant::now();
        let zero = store.zero;
        let one = store.one;
        let top = store.top;
        println!("Basic construction: {:?}", start.elapsed());
        
        // Test 2: is_zero calls
        let start = Instant::now();
        for _ in 0..1000 {
            store.is_zero(zero);
            store.is_zero(one);
            store.is_zero(top);
        }
        println!("1000 is_zero calls: {:?}", start.elapsed());
        
        // Test 3: Equality checks
        let start = Instant::now();
        for _ in 0..1000 {
            let _ = zero == spp::SPP::new(0);
            let _ = one == spp::SPP::new(1);
        }
        println!("1000 equality checks: {:?}", start.elapsed());
        
        // Test 4: Random packet generation
        let start = Instant::now();
        for _ in 0..100 {
            store.random_packet_pair(one);
        }
        println!("100 random packet generations: {:?}", start.elapsed());
        
        // Test 5: Creating a simple SPP using union
        let start = Instant::now();
        let simple_spp = store.union(zero, one);
        println!("Creating simple SPP: {:?}", start.elapsed());
        
        // Test 6: is_zero on constructed SPP
        let start = Instant::now();
        for _ in 0..100 {
            store.is_zero(simple_spp);
        }
        println!("100 is_zero calls on constructed SPP: {:?}", start.elapsed());
    }

    #[test]
    fn test_zero_equality_vs_is_zero() {
        let num_vars = 10;
        let mut store = spp::SPPstore::new(num_vars);
        
        // Test that store.zero is the canonical zero
        let zero_canonical = store.zero;
        
        // Create another zero by construction
        let mut constructed_zero = spp::SPP::new(0);
        for _ in 0..num_vars {
            constructed_zero = store.mk(constructed_zero, constructed_zero, constructed_zero, constructed_zero);
        }
        
        println!("Canonical zero: {:?}", zero_canonical);
        println!("Constructed zero: {:?}", constructed_zero);
        println!("Are they equal? {}", zero_canonical == constructed_zero);
        
        // Test performance difference
        let start = Instant::now();
        for _ in 0..1000 {
            let _ = constructed_zero == zero_canonical;
        }
        let equality_time = start.elapsed();
        
        let start = Instant::now();
        for _ in 0..1000 {
            store.is_zero(constructed_zero);
        }
        let is_zero_time = start.elapsed();
        
        println!("1000 equality checks: {:?}", equality_time);
        println!("1000 is_zero calls: {:?}", is_zero_time);
        println!("is_zero is {}x slower", is_zero_time.as_nanos() / equality_time.as_nanos().max(1));
    }

    #[test]
    fn test_performance_x10_equals_0() {
        let expr_str = "x10==0";
        println!("Testing performance for expression: {}", expr_str);
        
        // Test parsing
        let start = Instant::now();
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let parse_time = start.elapsed();
        println!("Parsing time: {:?}", parse_time);
        
        let first_expr = &expressions[0];
        let num_fields = first_expr.num_fields();
        println!("Number of fields detected: {}", num_fields);
        
        // Test automaton creation
        let start = Instant::now();
        let mut aut_handler = aut::Aut::new(num_fields);
        let aut_creation_time = start.elapsed();
        println!("Automaton creation time: {:?}", aut_creation_time);
        
        // Test expr to state conversion
        let start = Instant::now();
        let state_id = aut_handler.expr_to_state(first_expr.as_ref());
        let expr_to_state_time = start.elapsed();
        println!("Expression to state conversion time: {:?}", expr_to_state_time);
        
        // Test emptiness check
        let start = Instant::now();
        let is_empty = aut_handler.is_empty(state_id);
        let emptiness_check_time = start.elapsed();
        println!("Emptiness check time: {:?}", emptiness_check_time);
        println!("Is empty: {}", is_empty);
        
        if !is_empty {
            // Test single trace generation
            let start = Instant::now();
            let trace = aut_handler.random_trace(state_id, 5);
            let single_trace_time = start.elapsed();
            println!("Single trace generation time: {:?}", single_trace_time);
            println!("Trace result: {:?}", trace.is_some());
            
            // Test multiple trace generation
            let start = Instant::now();
            let mut traces_vec = Vec::new();
            for _ in 0..5 {
                if let Some(trace) = aut_handler.random_trace(state_id, 5) {
                    traces_vec.push(trace);
                }
            }
            let multiple_traces_time = start.elapsed();
            println!("5 traces generation time: {:?}", multiple_traces_time);
            println!("Generated {} traces", traces_vec.len());
        }
        
        // Test the full analyze_expression function
        let start = Instant::now();
        let result = analyze_expressions_internal(expr_str, "0", 5, 5);
        let full_analysis_time = start.elapsed();
        match result {
            Ok((_is_empty, _traces)) => println!("Full analyze_expression time: {:?}", full_analysis_time),
            Err(_) => println!("Full analyze_expression failed: {:?}", full_analysis_time),
        }
    }

    #[test]
    fn test_x10_equals_0_bdd_size() {
        let expr_str = "x10==0";
        println!("Analyzing BDD structure for expression: {}", expr_str);
        
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let first_expr = &expressions[0];
        let num_fields = first_expr.num_fields();
        println!("Number of fields: {}", num_fields);
        
        let mut aut_handler = aut::Aut::new(num_fields);
        let state_id = aut_handler.expr_to_state(first_expr.as_ref());
        
        // Get the epsilon SPP (represents the expression as a packet relation)
        let epsilon_spp = aut_handler.epsilon(state_id);
        println!("Epsilon SPP: {:?}", epsilon_spp);
        
        // Check the structure of the SPP store
        let spp_store = aut_handler.spp_store();
        println!("SPP store has {} nodes", spp_store.num_nodes());
        
        // Get the eliminate_dup result
        let start = Instant::now();
        let dup_spp = aut_handler.eliminate_dup(state_id);
        let eliminate_dup_time = start.elapsed();
        println!("eliminate_dup took: {:?}, result: {:?}", eliminate_dup_time, dup_spp);
        
        // Test a simpler expression for comparison
        let simple_expr_str = "x0==0";
        let simple_expressions = parser::parse_expressions(simple_expr_str).unwrap();
        let simple_first_expr = &simple_expressions[0];
        let simple_num_fields = simple_first_expr.num_fields();
        
        let mut simple_aut = aut::Aut::new(simple_num_fields);
        let simple_state = simple_aut.expr_to_state(simple_first_expr.as_ref());
        
        let start = Instant::now();
        let simple_dup_spp = simple_aut.eliminate_dup(simple_state);
        let simple_eliminate_dup_time = start.elapsed();
        println!("Simple x0==0 eliminate_dup took: {:?}, result: {:?}", simple_eliminate_dup_time, simple_dup_spp);
        
        println!("Performance difference: {}x slower for x10 vs x0", 
                eliminate_dup_time.as_nanos() / simple_eliminate_dup_time.as_nanos().max(1));
    }

    #[test]
    fn test_eliminate_dup_benchmark() {
        println!("Benchmarking eliminate_dup vs is_empty for x10==0");
        
        let expr_str = "x10==0";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let first_expr = &expressions[0];
        let num_fields = first_expr.num_fields();
        
        // Test 1: Fresh eliminate_dup call
        let start = Instant::now();
        let mut aut1 = aut::Aut::new(num_fields);
        let state1 = aut1.expr_to_state(first_expr.as_ref());
        let aut1_creation_time = start.elapsed();
        
        let start = Instant::now();
        let dup_spp1 = aut1.eliminate_dup(state1);
        let eliminate_dup_time1 = start.elapsed();
        
        println!("Fresh eliminate_dup: aut creation {:?}, eliminate_dup {:?}, result: {:?}", 
                aut1_creation_time, eliminate_dup_time1, dup_spp1);
        
        // Test 2: Second eliminate_dup call (should be cached)
        let start = Instant::now();
        let dup_spp2 = aut1.eliminate_dup(state1);
        let eliminate_dup_time2 = start.elapsed();
        println!("Cached eliminate_dup: {:?}, result: {:?}", eliminate_dup_time2, dup_spp2);
        
        // Test 3: Fresh is_empty call
        let start = Instant::now();
        let mut aut3 = aut::Aut::new(num_fields);
        let state3 = aut3.expr_to_state(first_expr.as_ref());
        let aut3_creation_time = start.elapsed();
        
        let start = Instant::now();
        let is_empty_result = aut3.is_empty(state3);
        let is_empty_time = start.elapsed();
        
        println!("Fresh is_empty: aut creation {:?}, is_empty {:?}, result: {}", 
                aut3_creation_time, is_empty_time, is_empty_result);
        
        // Test 4: Fresh random_trace call
        let start = Instant::now();
        let mut aut4 = aut::Aut::new(num_fields);
        let state4 = aut4.expr_to_state(first_expr.as_ref());
        let aut4_creation_time = start.elapsed();
        
        let start = Instant::now();
        let trace_result = aut4.random_trace(state4, 3);
        let random_trace_time = start.elapsed();
        
        println!("Fresh random_trace: aut creation {:?}, random_trace {:?}, trace found: {}", 
                aut4_creation_time, random_trace_time, trace_result.is_some());
        
        // Test 5: Multiple eliminate_dup calls to see if it's consistently slow
        let mut total_eliminate_dup_time = std::time::Duration::new(0, 0);
        for i in 0..5 {
            let start = Instant::now();
            let mut aut_fresh = aut::Aut::new(num_fields);
            let state_fresh = aut_fresh.expr_to_state(first_expr.as_ref());
            let dup_result = aut_fresh.eliminate_dup(state_fresh);
            let single_time = start.elapsed();
            total_eliminate_dup_time += single_time;
            println!("eliminate_dup #{}: {:?}, result: {:?}", i+1, single_time, dup_result);
        }
        
        println!("Average eliminate_dup time over 5 calls: {:?}", total_eliminate_dup_time / 5);
        println!("Comparison: eliminate_dup {:?} vs is_empty {:?} vs random_trace {:?}", 
                eliminate_dup_time1, is_empty_time, random_trace_time);
    }

    #[test]
    fn test_web_ui_alias_expressions() {
        // Test the specific expression that's causing errors in the web UI
        let test_cases = vec![
            ("let ip = &x[0..23] in ip==2", "Basic bit range alias"),
            ("let ip = &x[0..32] in ip==192.168.1.1", "IP address alias"),
            ("let port = &x[32..48] in port==80", "Port alias"),
            ("let byte = &x[0..8] in byte := 255", "Byte assignment"),
            ("let nibble = &x[0..4] in nibble == 15", "Nibble test"),
            ("let a = &x[0..8] in let b = &x[8..16] in a==1 & b==2", "Nested aliases"),
        ];

        for (expr_str, description) in test_cases {
            println!("\nTesting {}: {}", description, expr_str);
            
            // Test parsing
            match parser::parse_expressions(expr_str) {
                Ok(parsed_exprs) => {
                    println!("  ✓ Parsing succeeded");
                    if let Some(expr) = parsed_exprs.first() {
                        println!("  Parsed: {:?}", expr);
                        
                        // Test desugaring
                        match desugar::desugar(expr) {
                            Ok(desugared) => {
                                println!("  ✓ Desugaring succeeded");
                                println!("  Desugared: {:?}", desugared);
                                
                                // Test that it can be processed by automaton
                                let num_fields = desugared.num_fields();
                                let mut aut = aut::Aut::new(num_fields);
                                
                                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                    aut.expr_to_state(&desugared)
                                })) {
                                    Ok(state_id) => {
                                        println!("  ✓ Automaton conversion succeeded: state {}", state_id);
                                        
                                        // Test emptiness check
                                        let is_empty = aut.is_empty(state_id);
                                        println!("  Is empty: {}", is_empty);
                                    }
                                    Err(e) => {
                                        println!("  ✗ Automaton conversion panicked: {:?}", e);
                                    }
                                }
                            }
                            Err(e) => {
                                println!("  ✗ Desugaring failed: {}", e);
                            }
                        }
                    }
                }
                Err(e) => {
                    println!("  ✗ Parsing failed: {}", e);
                }
            }
        }
    }

    #[test]
    fn test_analyze_expression_web_ui() {
        // Test the actual web UI function with problematic expressions
        let test_cases = vec![
            "let ip = &x[0..23] in ip==2",
            "let ip = &x[0..32] in ip==192.168.1.1",
            "x[0..8] == 255",  // Direct bit range test
            "x[0..8] := 10",   // Direct bit range assignment
            "let byte = &x[0..8] in byte == 10",
            "let nibble = &x[0..4] in nibble := 15",
        ];

        for expr_str in test_cases {
            println!("\nTesting analyze_expression with: {}", expr_str);
            
            match analyze_expressions_internal(expr_str, "0", 5, 5) {
                Ok((is_empty, traces)) => {
                    println!("  ✓ Analysis succeeded");
                    println!("  Is empty: {}", is_empty);
                    if let Some(traces) = traces {
                        println!("  Traces found: {}", traces.len());
                    }
                }
                Err((expr1_err, _)) => {
                    if let Some(err) = expr1_err {
                        println!("  ✗ Analysis failed: {}", err.message);
                    } else {
                        println!("  ✗ Analysis failed with unknown error");
                    }
                }
            }
        }
    }

    #[test]
    fn test_is_empty_profiling() {
        println!("Profiling individual operations in is_empty algorithm");
        
        let expr_str = "x10==0";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let first_expr = &expressions[0];
        let num_fields = first_expr.num_fields();
        
        let mut aut = aut::Aut::new(num_fields);
        let state = aut.expr_to_state(first_expr.as_ref());
        
        // Test individual SPP operations that is_empty uses
        println!("Testing SP operations on large sets...");
        
        // Create the "all packets" SP (what is_empty starts with)
        let start = Instant::now();
        let sp_one = aut.spp_store().sp.one;
        let sp_zero = aut.spp_store().sp.zero;
        println!("SP creation: {:?}", start.elapsed());
        
        // Test SP difference operation (used heavily in is_empty)
        let start = Instant::now();
        let sp_diff = aut.spp_store_mut().sp.difference(sp_one, sp_zero);
        let sp_diff_time = start.elapsed();
        println!("SP difference: {:?}, result: {:?}", sp_diff_time, sp_diff);
        
        // Test SP union operation  
        let start = Instant::now();
        let sp_union = aut.spp_store_mut().sp.union(sp_one, sp_zero);
        let sp_union_time = start.elapsed();
        println!("SP union: {:?}, result: {:?}", sp_union_time, sp_union);
        
        // Test push operation (the expensive one in is_empty)
        let epsilon_spp = aut.epsilon(state);
        let start = Instant::now();
        let pushed = aut.spp_store_mut().push(sp_one, epsilon_spp);
        let push_time = start.elapsed();
        println!("Push operation: {:?}, input SP: {:?}, SPP: {:?}, result: {:?}", 
                push_time, sp_one, epsilon_spp, pushed);
        
        // Test delta computation
        let start = Instant::now();
        let delta_st = aut.delta(state);
        let delta_time = start.elapsed();
        println!("Delta computation: {:?}, transitions: {}", 
                delta_time, delta_st.get_transitions().len());
        
        // Test what happens when we do multiple push operations
        let start = Instant::now();
        let mut current_sp = sp_one;
        for (_target_state, spp) in delta_st.get_transitions() {
            current_sp = aut.spp_store_mut().push(current_sp, *spp);
            println!("  Push with SPP {:?} -> SP {:?}", spp, current_sp);
        }
        let multiple_push_time = start.elapsed();
        println!("Multiple push operations: {:?}", multiple_push_time);
        
        // Test the core of is_empty - the worklist algorithm
        println!("Testing simplified is_empty worklist...");
        let start = Instant::now();
        let mut todo = vec![(state, sp_one)];
        let mut sp_map = std::collections::HashMap::new();
        let mut iterations = 0;
        
        while !todo.is_empty() && iterations < 10 { // Limit to 10 iterations
            let (current_state, sp) = todo.pop().unwrap();
            let original_sp = sp_map.entry(current_state).or_insert(sp_zero);
            let to_add = aut.spp_store_mut().sp.difference(sp, *original_sp);
            
            if to_add != sp_zero {
                *original_sp = aut.spp_store_mut().sp.union(*original_sp, to_add);
                
                // This is the expensive part - computing transitions
                let transitions = aut.delta(current_state);
                for (next_state, spp) in transitions.get_transitions() {
                    let pushed_sp = aut.spp_store_mut().push(to_add, *spp);
                    todo.push((*next_state, pushed_sp));
                }
            }
            iterations += 1;
        }
        let worklist_time = start.elapsed();
        println!("Worklist algorithm ({} iterations): {:?}", iterations, worklist_time);
    }
}
