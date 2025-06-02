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
    
    #[test]
    fn test_pattern_matching_cidr() {
        // Test CIDR notation pattern matching
        let expr_str = "let ip = &x[0..32] in ip ~ 192.168.1.0/24";
        
        // Parse and desugar
        let expressions = parser::parse_expressions(expr_str).unwrap();
        assert_eq!(expressions.len(), 1);
        println!("Parsed expression: {:?}", expressions[0]);
        
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        println!("Desugared expression: {:?}", desugared);
        
        // Check that it desugars to the expected structure
        let num_fields = desugared.num_fields();
        println!("Number of fields: {}", num_fields);
        
        // For now, use a fixed number of fields if num_fields is 0
        let actual_fields = if num_fields == 0 { 32 } else { num_fields };
        
        // Create automaton and check it's not empty
        let mut aut = aut::Aut::new(actual_fields);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "CIDR pattern should allow some traffic");
    }
    
    #[test]
    fn test_pattern_matching_ip_range() {
        // Test IP range pattern matching
        let expr_str = "let src = &x[0..32] in src ~ 10.0.0.1-10.0.0.10";
        
        let expressions = parser::parse_expressions(expr_str).unwrap();
        assert_eq!(expressions.len(), 1);
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        
        let num_fields = desugared.num_fields();
        let actual_fields = if num_fields == 0 { 32 } else { num_fields };
        
        let mut aut = aut::Aut::new(actual_fields);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "IP range pattern should allow some traffic");
    }
    
    #[test]
    fn test_pattern_matching_wildcard() {
        // Test wildcard mask pattern matching
        let expr_str = "let dst = &x[32..64] in dst ~ 192.168.1.0 mask 0.0.0.255";
        
        let expressions = parser::parse_expressions(expr_str).unwrap();
        assert_eq!(expressions.len(), 1);
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        
        let num_fields = desugared.num_fields();
        let actual_fields = if num_fields == 0 { 64 } else { num_fields };
        
        let mut aut = aut::Aut::new(actual_fields);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Wildcard pattern should allow some traffic");
    }
    
    #[test]
    fn test_pattern_matching_exact() {
        // Test exact IP pattern matching
        let expr_str = "x[0..32] ~ 192.168.1.100";
        
        let expressions = parser::parse_expressions(expr_str).unwrap();
        assert_eq!(expressions.len(), 1);
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        
        let num_fields = desugared.num_fields();
        let actual_fields = if num_fields == 0 { 32 } else { num_fields };
        
        let mut aut = aut::Aut::new(actual_fields);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Exact IP pattern should allow some traffic");
    }
    
    #[test]
    fn test_pattern_matching_combined() {
        // Test combining pattern matching with other operations
        let expr_str = "let src = &x[0..32] in let dst = &x[32..64] in (src ~ 10.0.0.0/8) & (dst ~ 192.168.0.0/16)";
        
        let expressions = parser::parse_expressions(expr_str).unwrap();
        assert_eq!(expressions.len(), 1);
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        
        let num_fields = desugared.num_fields();
        let actual_fields = if num_fields == 0 { 64 } else { num_fields };
        
        let mut aut = aut::Aut::new(actual_fields);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Combined pattern should allow some traffic");
    }
    
    #[test]
    fn test_pattern_matching_negation() {
        // Test complement of pattern matching (using set difference)
        let expr_str = "let ip = &x[0..32] in T - (ip ~ 192.168.1.0/24)";
        
        let expressions = parser::parse_expressions(expr_str).unwrap();
        assert_eq!(expressions.len(), 1);
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        
        let num_fields = desugared.num_fields();
        let actual_fields = if num_fields == 0 { 32 } else { num_fields };
        
        let mut aut = aut::Aut::new(actual_fields);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Complement of pattern should allow some traffic");
    }
    
    #[test]
    fn test_large_ip_range_performance() {
        use std::time::Instant;
        
        // Test various large IP ranges to ensure efficient handling
        let test_cases = vec![
            ("x[0..32] ~ 10.0.0.0-10.0.255.255", "Class A /16 range (65,536 addresses)"),
            ("x[0..32] ~ 172.16.0.0-172.31.255.255", "Private network range (1,048,576 addresses)"),
            ("x[0..32] ~ 0.0.0.0-255.255.255.255", "Full IPv4 range (4,294,967,296 addresses)"),
            ("let ip = &x[0..32] in ip ~ 1.0.0.0-200.255.255.255", "Large range with alias"),
        ];
        
        for (expr_str, description) in test_cases {
            println!("\nTesting {}: {}", description, expr_str);
            
            // Parse the expression
            let start = Instant::now();
            let expressions = parser::parse_expressions(expr_str)
                .expect(&format!("Failed to parse: {}", expr_str));
            let parse_time = start.elapsed();
            
            // Desugar the expression
            let start = Instant::now();
            let desugared = desugar::desugar(&expressions[0])
                .expect(&format!("Failed to desugar: {}", expr_str));
            let desugar_time = start.elapsed();
            
            // Count unions to verify efficiency
            fn count_unions(expr: &expr::Expr) -> usize {
                match expr {
                    expr::Expr::Union(e1, e2) => 1 + count_unions(e1) + count_unions(e2),
                    expr::Expr::Intersect(e1, e2) => count_unions(e1) + count_unions(e2),
                    expr::Expr::Sequence(e1, e2) => count_unions(e1) + count_unions(e2),
                    expr::Expr::Star(e) => count_unions(e),
                    expr::Expr::TestNegation(e) => count_unions(e),
                    _ => 0,
                }
            }
            
            let union_count = count_unions(&desugared);
            println!("  Parse time: {:?}", parse_time);
            println!("  Desugar time: {:?}", desugar_time);
            println!("  Union count: {}", union_count);
            
            // Verify the times are reasonable (not exponential)
            assert!(parse_time.as_millis() < 100, "Parsing took too long: {:?}", parse_time);
            assert!(desugar_time.as_millis() < 100, "Desugaring took too long: {:?}", desugar_time);
            
            // Verify union count is bounded (not exponential in address count)
            assert!(union_count <= 128, "Too many unions ({}), likely not using efficient bounds", union_count);
            
            // Try to build automaton (this would fail or hang with exponential expressions)
            let num_fields = desugared.num_fields();
            let actual_fields = if num_fields == 0 { 32 } else { num_fields };
            
            let start = Instant::now();
            let mut aut = aut::Aut::new(actual_fields);
            let state = aut.expr_to_state(&desugared);
            let aut_time = start.elapsed();
            
            println!("  Automaton construction time: {:?}", aut_time);
            assert!(aut_time.as_millis() < 1000, "Automaton construction took too long: {:?}", aut_time);
            
            // Verify the automaton is not trivially empty
            assert!(!aut.is_empty(state), "Range should allow some traffic");
        }
    }
    
    #[test]
    fn test_tutorial_syntactic_sugar_examples() {
        // Test syntactic sugar examples from the tutorial
        
        // Basic bit range access
        let expr_str = "x[0..8] := 192";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        assert!(matches!(desugared.as_ref(), expr::Expr::Sequence(_, _)), "Should desugar to sequence");
        
        // Bit range test
        let expr_str = "x[8..16] ~ 168";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        assert!(matches!(desugared.as_ref(), expr::Expr::Intersect(_, _)), "Should desugar to intersection");
        
        // IP address assignment
        let expr_str = "x[0..32] := 192.168.1.1";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "IP assignment should not be empty");
        
        // Port test
        let expr_str = "x[32..48] ~ 80";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(48);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Port test should not be empty");
        
        // Bit range alias basic
        let expr_str = "let ip_src = &x[0..32] in ip_src := 10.0.0.1";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Bit range alias should not be empty");
        
        // Multiple aliases
        let expr_str = r#"let src = &x[0..32] in
let dst = &x[32..64] in
src ~ 192.168.1.100 & dst ~ 8.8.8.8"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(64);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Multiple aliases should not be empty");
        
        // Readable firewall rule
        let expr_str = r#"let src_ip = &x[0..32] in
let dst_port = &x[32..48] in
src_ip == 192.168.1.0 + src_ip == 192.168.1.1;
dst_port := 443"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(48);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Firewall rule should not be empty");
        
        // Direct bit range access (sub-ranges of aliases not supported)
        let expr_str = r#"let src_ip = &x[96..128] in
src_ip ~ 10.0.0.1"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(128);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Direct bit range alias should not be empty");
        
        // Combined example
        let expr_str = r#"let setup_defaults = (x[0..32] := 192.168.1.1 ; x[32..64] := 8.8.8.8) in
x[32..40] == 80; setup_defaults"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        // This is NOT empty - the test checks bits 32-40 for value 80, then assigns the full 32-64 range
        let mut aut = aut::Aut::new(64);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Combined example should not be empty");
        
        // NAT example
        let expr_str = r#"let src_ip = &x[0..32] in
let dst_ip = &x[32..64] in
let is_private = src_ip == 192.168.1.1 + src_ip == 192.168.1.2 in
let is_external = dst_ip == 8.8.8.8 + dst_ip == 1.1.1.1 in
is_private & is_external; src_ip := 203.0.113.1"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(64);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "NAT rule should not be empty");
    }
    
    #[test]
    fn test_tutorial_pattern_examples() {
        // Test all pattern matching examples from the tutorial
        
        // Exact IP matching
        let expr_str = "let src = &x[0..32] in src ~ 192.168.1.100";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Exact IP match should not be empty");
        
        // CIDR notation
        let expr_str = "let src = &x[0..32] in src ~ 192.168.1.0/24";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "CIDR match should not be empty");
        
        // Multiple subnets
        let expr_str = "let dst = &x[32..64] in dst ~ 10.0.0.0/8 + dst ~ 172.16.0.0/12 + dst ~ 192.168.0.0/16";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(64);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Multiple subnet match should not be empty");
        
        // IP range
        let expr_str = "let src = &x[0..32] in src ~ 10.0.0.1-10.0.0.10";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "IP range match should not be empty");
        
        // Large IP ranges
        let expr_str = "let ip = &x[0..32] in ip ~ 10.0.0.0-10.255.255.255 + ip ~ 172.16.0.0-172.31.255.255 + ip ~ 192.168.0.0-192.168.255.255";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Large IP range match should not be empty");
        
        // Wildcard mask
        let expr_str = "let dst = &x[32..64] in dst ~ 192.168.1.1 mask 0.0.255.0";
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(64);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Wildcard mask match should not be empty");
        
        // Pattern with different literal formats
        let expr_str = r#"let port = &x[0..16] in
let proto = &x[16..24] in
port ~ 1024-65535 &
(proto ~ 0x06 +
proto ~ 0x11 +
proto ~ 0x01)"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(24);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Different literal format patterns should not be empty");
        
        // Complex firewall rule
        let expr_str = r#"let src = &x[0..32] in
let dst = &x[32..64] in
let port = &x[64..80] in
(src ~ 192.168.1.0/24) &
(dst ~ 10.0.1.0/24) &
(port ~ 80 + port ~ 443) +
(src ~ 10.0.1.0/24) &
(dst ~ 192.168.1.0/24) &
(port ~ 1024-65535)"#;
        let expressions = parser::parse_expressions(expr_str).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(80);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Complex firewall rule should not be empty");
    }
    
    #[test]
    fn test_pattern_range_equivalence() {
        // Test that efficient range patterns are equivalent to their expansions
        // by checking that (efficient XOR expansion) is empty
        
        // Helper to expand a range manually
        fn expand_range(start: u32, end: u32, start_val: u128, end_val: u128) -> expr::Exp {
            let mut terms = Vec::new();
            let width = (end - start) as usize;
            
            for val in start_val..=end_val {
                let mut bits = Vec::new();
                // Generate bits in MSB-first order
                for i in (0..width).rev() {
                    bits.push((val >> i) & 1 == 1);
                }
                terms.push(expr::Expr::bit_range_test(start, end, bits));
            }
            
            // Build disjunction
            if terms.is_empty() {
                expr::Expr::zero()
            } else {
                let mut result = terms.pop().unwrap();
                while let Some(term) = terms.pop() {
                    result = expr::Expr::union(term, result);
                }
                result
            }
        }
        
        // Test case 1: Small IP range
        {
            let expr_str = "x[0..32] ~ 192.168.1.10-192.168.1.15";
            let expressions = parser::parse_expressions(expr_str).unwrap();
            let efficient = desugar::desugar(&expressions[0]).unwrap();
            
            // Manually expand the same range
            let start_ip = (192u128 << 24) | (168u128 << 16) | (1u128 << 8) | 10u128;
            let end_ip = (192u128 << 24) | (168u128 << 16) | (1u128 << 8) | 15u128;
            let expansion = expand_range(0, 32, start_ip, end_ip);
            
            // Check equivalence: (efficient XOR expansion) should be empty
            let xor = expr::Expr::xor(efficient.clone(), expansion);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "Efficient range should be equivalent to expansion");
        }
        
        // Test case 2: Port range
        {
            let expr_str = "let port = &x[0..16] in port ~ 8080-8090";
            let expressions = parser::parse_expressions(expr_str).unwrap();
            let efficient = desugar::desugar(&expressions[0]).unwrap();
            
            // Manually expand
            let expansion = expand_range(0, 16, 8080, 8090);
            
            // Check equivalence
            let xor = expr::Expr::xor(efficient.clone(), expansion);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(16);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "Efficient port range should be equivalent to expansion");
        }
        
        // Test case 3: Power of 2 aligned range
        {
            let expr_str = "x[0..8] ~ 128-255";  // Upper half of byte
            let expressions = parser::parse_expressions(expr_str).unwrap();
            let efficient = desugar::desugar(&expressions[0]).unwrap();
            
            // This should be equivalent to just testing the MSB
            let msb_test = expr::Expr::test(0, true);
            
            // Check equivalence
            let xor = expr::Expr::xor(efficient.clone(), msb_test);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(8);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "Range 128-255 should be equivalent to MSB=1");
        }
        
        // Test case 4: CIDR equivalence
        {
            // 192.168.1.0/24 should be equivalent to 192.168.1.0-192.168.1.255
            let cidr_expr = parser::parse_expressions("x[0..32] ~ 192.168.1.0/24").unwrap();
            let range_expr = parser::parse_expressions("x[0..32] ~ 192.168.1.0-192.168.1.255").unwrap();
            
            let cidr = desugar::desugar(&cidr_expr[0]).unwrap();
            let range = desugar::desugar(&range_expr[0]).unwrap();
            
            // Check equivalence
            let xor = expr::Expr::xor(cidr, range);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "CIDR /24 should be equivalent to corresponding range");
        }
        
        // Test case 5: Complex range that would be inefficient to enumerate
        {
            // This range has 100 addresses - good for testing efficiency
            let expr_str = "x[0..32] ~ 10.0.0.156-10.0.1.0";  // 156..255 + 0
            let expressions = parser::parse_expressions(expr_str).unwrap();
            let efficient = desugar::desugar(&expressions[0]).unwrap();
            
            // For this test, just verify it doesn't create too many unions
            fn count_unions(expr: &expr::Expr) -> usize {
                match expr {
                    expr::Expr::Union(e1, e2) => 1 + count_unions(e1) + count_unions(e2),
                    expr::Expr::Intersect(e1, e2) => count_unions(e1) + count_unions(e2),
                    _ => 0,
                }
            }
            
            let union_count = count_unions(&efficient);
            assert!(union_count < 100, "Should use efficient bounds, not enumerate all 101 addresses");
            
            // Also verify it's not empty and accepts the right values
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&efficient);
            assert!(!aut.is_empty(state), "Range should not be empty");
        }
    }
    
    #[test]
    fn test_cidr_pattern_equivalence() {
        // Test that CIDR patterns are equivalent to their expanded ranges
        // by checking that (CIDR XOR expansion) is empty
        
        // Helper to manually expand a CIDR to all matching IPs
        fn expand_cidr(start: u32, end: u32, base_ip: u32, prefix_len: usize) -> expr::Exp {
            let width = (end - start) as usize;
            let mut terms = Vec::new();
            
            // Calculate the number of host bits
            let host_bits = 32 - prefix_len;
            let num_addresses = 1u32 << host_bits;
            
            // Generate all IPs in the CIDR range
            for i in 0..num_addresses {
                let ip = base_ip | i;
                let mut bits = Vec::new();
                // Generate bits in MSB-first order
                for bit_idx in (0..width).rev() {
                    bits.push((ip >> bit_idx) & 1 == 1);
                }
                terms.push(expr::Expr::bit_range_test(start, end, bits));
            }
            
            // Build disjunction
            if terms.is_empty() {
                expr::Expr::zero()
            } else {
                let mut result = terms.pop().unwrap();
                while let Some(term) = terms.pop() {
                    result = expr::Expr::union(term, result);
                }
                result
            }
        }
        
        // Test case 1: /24 network (256 addresses)
        {
            let cidr_expr = parser::parse_expressions("x[0..32] ~ 192.168.1.0/24").unwrap();
            let cidr = desugar::desugar(&cidr_expr[0]).unwrap();
            
            // Manually expand the CIDR
            let base_ip = (192u32 << 24) | (168u32 << 16) | (1u32 << 8) | 0u32;
            let expansion = expand_cidr(0, 32, base_ip, 24);
            
            // Check equivalence
            let xor = expr::Expr::xor(cidr.clone(), expansion);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "/24 CIDR should be equivalent to 256 address expansion");
        }
        
        // Test case 2: /28 network (16 addresses)
        {
            let cidr_expr = parser::parse_expressions("x[0..32] ~ 10.0.0.16/28").unwrap();
            let cidr = desugar::desugar(&cidr_expr[0]).unwrap();
            
            // The /28 network 10.0.0.16/28 includes IPs 10.0.0.16 - 10.0.0.31
            let base_ip = (10u32 << 24) | (0u32 << 16) | (0u32 << 8) | 16u32;
            let expansion = expand_cidr(0, 32, base_ip, 28);
            
            // Check equivalence
            let xor = expr::Expr::xor(cidr.clone(), expansion);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "/28 CIDR should be equivalent to 16 address expansion");
        }
        
        // Test case 3: /32 network (single host)
        {
            let cidr_expr = parser::parse_expressions("x[0..32] ~ 192.168.1.100/32").unwrap();
            let single_ip = parser::parse_expressions("x[0..32] ~ 192.168.1.100").unwrap();
            
            let cidr = desugar::desugar(&cidr_expr[0]).unwrap();
            let exact = desugar::desugar(&single_ip[0]).unwrap();
            
            // /32 should be equivalent to exact IP match
            let xor = expr::Expr::xor(cidr, exact);
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "/32 CIDR should be equivalent to exact IP match");
        }
        
        // Test case 4: /16 network (larger test)
        {
            let cidr_expr = parser::parse_expressions("x[0..32] ~ 172.16.0.0/16").unwrap();
            let cidr = desugar::desugar(&cidr_expr[0]).unwrap();
            
            // For /16, we can't enumerate all 65536 addresses, but we can test samples
            // Test that some addresses inside the range match
            let inside_tests = vec![
                "x[0..32] ~ 172.16.0.0",
                "x[0..32] ~ 172.16.0.1",
                "x[0..32] ~ 172.16.255.254",
                "x[0..32] ~ 172.16.255.255",
                "x[0..32] ~ 172.16.128.128",
            ];
            
            for test_expr in inside_tests {
                let ip_expr = parser::parse_expressions(test_expr).unwrap();
                let ip = desugar::desugar(&ip_expr[0]).unwrap();
                
                // IP AND CIDR should equal IP (meaning IP is in CIDR range)
                let intersection = expr::Expr::intersect(cidr.clone(), ip.clone());
                let inter_desugared = desugar::desugar(&intersection).unwrap();
                
                let xor = expr::Expr::xor(inter_desugared, ip);
                let xor_desugared = desugar::desugar(&xor).unwrap();
                
                let mut aut = aut::Aut::new(32);
                let state = aut.expr_to_state(&xor_desugared);
                assert!(aut.is_empty(state), "{} should be in 172.16.0.0/16", test_expr);
            }
            
            // Test that addresses outside the range don't match
            let outside_tests = vec![
                "x[0..32] ~ 172.15.255.255",
                "x[0..32] ~ 172.17.0.0",
                "x[0..32] ~ 173.16.0.0",
            ];
            
            for test_expr in outside_tests {
                let ip_expr = parser::parse_expressions(test_expr).unwrap();
                let ip = desugar::desugar(&ip_expr[0]).unwrap();
                
                // IP AND CIDR should be empty (meaning IP is not in CIDR range)
                let intersection = expr::Expr::intersect(cidr.clone(), ip);
                let inter_desugared = desugar::desugar(&intersection).unwrap();
                
                let mut aut = aut::Aut::new(32);
                let state = aut.expr_to_state(&inter_desugared);
                assert!(aut.is_empty(state), "{} should NOT be in 172.16.0.0/16", test_expr);
            }
        }
        
        // Test case 5: Special cases
        {
            // /0 should match everything
            let cidr_all = parser::parse_expressions("x[0..32] ~ 0.0.0.0/0").unwrap();
            let all = desugar::desugar(&cidr_all[0]).unwrap();
            
            // Should be equivalent to T (true/one)
            let xor = expr::Expr::xor(all, expr::Expr::one());
            let xor_desugared = desugar::desugar(&xor).unwrap();
            
            let mut aut = aut::Aut::new(32);
            let state = aut.expr_to_state(&xor_desugared);
            assert!(aut.is_empty(state), "/0 CIDR should match everything");
        }
        
        // Test case 6: CIDR with port field
        {
            let expr_str = "let ip = &x[0..32] in ip ~ 10.10.0.0/16";
            let cidr_expr = parser::parse_expressions(expr_str).unwrap();
            let cidr = desugar::desugar(&cidr_expr[0]).unwrap();
            
            // Test boundary addresses
            let start_ip = parser::parse_expressions("let ip = &x[0..32] in ip ~ 10.10.0.0").unwrap();
            let end_ip = parser::parse_expressions("let ip = &x[0..32] in ip ~ 10.10.255.255").unwrap();
            
            let start = desugar::desugar(&start_ip[0]).unwrap();
            let end = desugar::desugar(&end_ip[0]).unwrap();
            
            // Both should be in the CIDR range
            for (ip, name) in [(start, "start"), (end, "end")] {
                let intersection = expr::Expr::intersect(cidr.clone(), ip.clone());
                let inter_desugared = desugar::desugar(&intersection).unwrap();
                
                let xor = expr::Expr::xor(inter_desugared, ip);
                let xor_desugared = desugar::desugar(&xor).unwrap();
                
                let mut aut = aut::Aut::new(32);
                let state = aut.expr_to_state(&xor_desugared);
                assert!(aut.is_empty(state), "{} address should be in CIDR range", name);
            }
        }
    }
    
    #[test]
    fn test_pattern_matching_literals() {
        // Test pattern matching with different literal formats
        
        // Hexadecimal literal
        let hex_expr = "x[0..32] ~ 0xC0A80101";  // 192.168.1.1 in hex
        let expressions = parser::parse_expressions(hex_expr).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Hex pattern should allow traffic");
        
        // Binary literal
        let bin_expr = "x[0..8] ~ 0b11000000";  // 192 in binary
        let expressions = parser::parse_expressions(bin_expr).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(8);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Binary pattern should allow traffic");
        
        // Decimal literal
        let dec_expr = "x[0..32] ~ 3232235777";  // 192.168.1.1 as decimal
        let expressions = parser::parse_expressions(dec_expr).unwrap();
        let desugared = desugar::desugar(&expressions[0]).unwrap();
        let mut aut = aut::Aut::new(32);
        let state = aut.expr_to_state(&desugared);
        assert!(!aut.is_empty(state), "Decimal pattern should allow traffic");
        
        // Test that different literal formats work for exact matches
        println!("All literal format tests passed!");
    }
}
