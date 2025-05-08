document.addEventListener('DOMContentLoaded', () => {
    const codeInput = document.getElementById('codeInput');
    const outputArea = document.getElementById('outputArea');
    const errorArea = document.getElementById('errorArea');
    const cycleExampleButton = document.getElementById('cycleExampleButton');
    const cycleErrorExampleButton = document.getElementById('cycleErrorExampleButton');

    const examples = [
        "fn x => x",                                      // Identity
        "42",                                             // Number
        "(fn x => x) 123",                                // Simple application
        "nil",                                            // Nil literal
        "[]",                                             // Empty list literal
        "[1,2,3]",                                        // List literal
        "1 :: [2,3]",                                     // Cons operator
        "let id = fn x => x in [id 0, id 1, id 2]",         // List with function application
        "match [1,2] with | nil => 0 | h::t => h end",      // Basic match
        "match [] with | nil => 100 | h::t => h end",       // Match on empty list
        "let l = 1 :: 2 :: nil in match l with | nil => 0 | h::t => h end", // Match on let-bound list
        // Map function
        "let map = rec map f => fn l =>\n  match l with\n  | nil => nil\n  | h::t => (f h) :: (map f t)\n  end\nin\nlet make_zero = fn x => 0\nin\nmap make_zero [10,20,30]", // Should be [0,0,0]
        // Append function
        "let append = fn l1 => fn l2 =>\n  match l1 with\n  | nil => l2\n  | h::t => h :: (append t l2)\n  end\nin\nappend [1,2] [3,4]", // Should be [1,2,3,4]
        // Nested match example
        "let process_item = fn item => item (* for type checking *) in\nmatch (1 :: 2 :: nil) with\n| nil => nil\n| h1::t1 =>\n  match t1 with\n  | nil => [process_item h1]\n  | h2::t2 => (process_item h1) :: (process_item h2) :: t2\n  end\nend", // Should be 1 :: 2 :: nil (List(Int))
        "fn k => fn y => k",                              // K combinator
        "fn s => fn k => fn x => s k (k x)",              // S combinator (SKI)
        "let id = fn x => x in id 123",                   // Let binding
        "let id = fn x => x in id id",                    // Let with polymorphic id
        "let const_ = fn k => fn y => k in const_ 10 (fn z => z)", // Let with K
        "fn f => fn x => f (f x)",                        // Twice combinator
        "let twice = fn f => fn x => f (f x) in twice (fn y => y) 10", // Apply twice
        "fn g => let id = fn x => x in g (id 10)",
        // Rec examples
        "rec is_non_empty l => match l with | nil => 0 | h::t => 1 end", // Returns Int (0 for empty, 1 for non-empty)
        "let an_int_list = [1,2,3] in (rec is_non_empty l => match l with | nil => 0 | h::t => 1 end) an_int_list",
        // Map function using rec
        "let make_zero = fn x => 0 in\nrec map f l =>\n  match l with\n  | nil => nil\n  | h::t => (f h) :: (map f t) \n  end\nin\nmap make_zero [10,20,30]"
    ];
    let currentExampleIndex = -1; // Start at -1 so first click loads example 0

    const errorExamples = [
        "fn x => x x",                                     // Classic occurs check error
        "123 (fn x => x)",                                 // Applying a number as a function
        "let apply_if_fn = fn f => fn x => f x in let not_fn = 10 in apply_if_fn not_fn 0", // Tries to use Int as a function
        "let K = fn a => fn b => a in let I = fn z => z in (K (I 10)) (I K)",   // Mismatch in applying partially applied K
        "let f = fn x => 10 in let g = fn y => y y in f g", // Error localized to `y y`
        "let force_error = fn x => x 0 in let user = fn f => force_error f in user 10", // Passes Int where func expected
        // List and Match related errors:
        "[10, fn x => x, 20]",                            // Heterogeneous list elements
        "100 :: 200",                                      // Tail of cons is not a list
        "match 123 with | nil => 0 | h::t => h end",       // Matching a non-list value
        "match [1] with | nil => 0 | h::t => (h :: nil) end", // Case bodies have incompatible types (Int vs List(Int))
        "let list_fn = fn l => match l with | nil => 0 | h::t => h end in list_fn (fn x => x)" // Applying list function to non-list
    ];
    let currentErrorExampleIndex = -1;

    function loadExample(index) {
        codeInput.value = examples[index];
        // Dispatch an input event to trigger the parsing and type inference
        const event = new Event('input', { bubbles: true, cancelable: true });
        codeInput.dispatchEvent(event);
    }

    cycleExampleButton.addEventListener('click', () => {
        currentExampleIndex = (currentExampleIndex + 1) % examples.length;
        loadExample(currentExampleIndex);
    });

    cycleErrorExampleButton.addEventListener('click', () => {
        currentErrorExampleIndex = (currentErrorExampleIndex + 1) % errorExamples.length;
        // Reuse loadExample, but pass from the errorExamples array
        codeInput.value = errorExamples[currentErrorExampleIndex];
        const event = new Event('input', { bubbles: true, cancelable: true });
        codeInput.dispatchEvent(event);
    });

    codeInput.addEventListener('input', () => {
        const code = codeInput.value;
        // Clear previous state
        outputArea.innerHTML = ''; 
        errorArea.textContent = ''; 

        if (code.trim() === '') {
            outputArea.textContent = 'Type some code or click "Next Example"!';
            return;
        }

        try {
            const ast = parse(code);
            if (!ast) { // Should be caught by parser throwing an error if code is not empty.
                outputArea.textContent = 'Type some code or click "Next Example"!';
                return;
            }

            const initialEnv = createInitialEnv();
            nextTypeVarId = 0; 
            
            // Perform inference. Errors will be attached to AST nodes.
            infer(ast, initialEnv); 

            const prettyCode = prettyPrintAST(ast);
            outputArea.innerHTML = prettyCode;
            
            const errorNodes = [];
            collectErrorNodes(ast, errorNodes);

            if (errorNodes.length > 0) {
                // For now, display the first rich error found. 
                // Could be enhanced to show all, or a summary.
                const primaryErrorNode = errorNodes.find(n => typeof n.error === 'object' && n.error.messageTemplate);
                
                if (primaryErrorNode) {
                    errorArea.innerHTML = formatRichErrorMessage(primaryErrorNode.error, primaryErrorNode);
                } else if (errorNodes.length > 0 && typeof errorNodes[0].error === 'string'){
                    // Fallback for simple string errors if no rich error object found first
                    errorArea.textContent = errorNodes[0].error; 
                } else {
                    errorArea.textContent = "Type errors found. Hover over highlighted expressions for details."; // Generic fallback
                }
            } else {
                errorArea.textContent = ''; // Clear if no errors
            }

        } catch (e) { 
            // This catch block now primarily handles parse errors or unexpected critical errors.
            // Type errors should ideally be attached to nodes and displayed via prettyPrintAST.
            errorArea.textContent = e.message;
            outputArea.innerHTML = ''; // Clear output on critical error
            console.error("Critical Error:", e);
        }
    });

    // Helper to recursively collect all nodes that have an .error property
    function collectErrorNodes(node, errorNodesList) {
        if (!node) return;
        if (node.error) {
            errorNodesList.push(node);
        }

        // Traverse children - specific to AST structure
        if (node.body) collectErrorNodes(node.body, errorNodesList); // Abs
        if (node.func) collectErrorNodes(node.func, errorNodesList); // App
        if (node.arg) collectErrorNodes(node.arg, errorNodesList);   // App
        if (node.expr1) collectErrorNodes(node.expr1, errorNodesList); // Let
        if (node.expr2) collectErrorNodes(node.expr2, errorNodesList); // Let
        if (node.expr) collectErrorNodes(node.expr, errorNodesList);   // Match
        if (node.elements) { // List
            node.elements.forEach(el => collectErrorNodes(el, errorNodesList));
        }
        if (node.head) collectErrorNodes(node.head, errorNodesList); // Cons
        if (node.tail) collectErrorNodes(node.tail, errorNodesList); // Cons
        if (node.cases) { // Match cases
            node.cases.forEach(c => {
                // collectErrorNodes(c.pattern, errorNodesList); // Patterns don't typically hold errors themselves after parsing
                collectErrorNodes(c.body, errorNodesList);
            });
        }
    }

    function formatRichErrorMessage(errorObj, causeNode) {
        if (!errorObj || typeof errorObj !== 'object' || !errorObj.messageTemplate) {
            // Fallback for simple string errors if they somehow reach here, or if errorObj is malformed
            return typeof errorObj === 'string' ? escapeHTML(errorObj) : "An unexpected error occurred, and error details are malformed.";
        }

        let message = errorObj.messageTemplate;

        if (errorObj.lhs && errorObj.lhs.type && errorObj.lhs.originNode) {
            const lhsTypeStr = prettyPrintType(errorObj.lhs.type);
            const lhsOriginDesc = errorObj.lhs.originNode.span 
                ? `expression at (${errorObj.lhs.originNode.span.start}-${errorObj.lhs.originNode.span.end})` 
                : (errorObj.lhs.originNode.type ? `'${errorObj.lhs.originNode.type}' expression` : "LHS expression");
            message = message.replace(/{LHS_TYPE}/g, `<span class="error-type-source">${escapeHTML(lhsTypeStr)}</span>`);
            message = message.replace(/{LHS_EXPR_DESC}/g, `<span class="error-expr-desc">${escapeHTML(lhsOriginDesc)}</span>`);
        } else {
             message = message.replace(/{LHS_TYPE}/g, `<span class="error-type-source">unknown type</span>`);
             message = message.replace(/{LHS_EXPR_DESC}/g, `<span class="error-expr-desc">unknown expression</span>`);
        }

        if (errorObj.rhs && errorObj.rhs.type && errorObj.rhs.originNode) {
            const rhsTypeStr = prettyPrintType(errorObj.rhs.type);
            const rhsOriginDesc = errorObj.rhs.originNode.span
                ? `expression at (${errorObj.rhs.originNode.span.start}-${errorObj.rhs.originNode.span.end})`
                : (errorObj.rhs.originNode.type ? `'${errorObj.rhs.originNode.type}' expression` : "RHS expression");
            message = message.replace(/{RHS_TYPE}/g, `<span class="error-type-target">${escapeHTML(rhsTypeStr)}</span>`);
            message = message.replace(/{RHS_EXPR_DESC}/g, `<span class="error-expr-desc">${escapeHTML(rhsOriginDesc)}</span>`);
        } else {
            message = message.replace(/{RHS_TYPE}/g, `<span class="error-type-target">unknown type</span>`);
            message = message.replace(/{RHS_EXPR_DESC}/g, `<span class="error-expr-desc">unknown expression</span>`);
        }
        
        // Specific for App errors
        if (errorObj.arg && errorObj.arg.type && errorObj.arg.originNode) {
            const argTypeStr = prettyPrintType(errorObj.arg.type);
            const argOriginDesc = errorObj.arg.originNode.span
                ? `expression at (${errorObj.arg.originNode.span.start}-${errorObj.arg.originNode.span.end})`
                : (errorObj.arg.originNode.type ? `'${errorObj.arg.originNode.type}' argument expression` : "argument expression");
            message = message.replace(/{ARG_TYPE}/g, `<span class="error-type-target">${escapeHTML(argTypeStr)}</span>`);
            message = message.replace(/{ARG_EXPR_DESC}/g, `<span class="error-expr-desc">${escapeHTML(argOriginDesc)}</span>`);
        }
        if (errorObj.expected_arg_type) { // For App template
            const expectedArgTypeStr = prettyPrintType(errorObj.expected_arg_type);
            message = message.replace(/{EXPECTED_ARG_TYPE}/g, `<span class="error-type-generic">${escapeHTML(expectedArgTypeStr)}</span>`);
        }
        if (errorObj.ret_type) {
            const retTypeStr = prettyPrintType(errorObj.ret_type);
            message = message.replace(/{RET_TYPE}/g, `<span class="error-type-generic">${escapeHTML(retTypeStr)}</span>`);
        }

        // Specific for Var undefined errors
        if (errorObj.var_name) {
            message = message.replace(/{VAR_NAME}/g, `<span class="error-var-name">${escapeHTML(errorObj.var_name)}</span>`);
        }
        // Specific for Match no cases error
        if (errorObj.pattern_example) {
            message = message.replace(/{PATTERN_EXAMPLE}/g, `<span class="error-code-example">${escapeHTML(errorObj.pattern_example)}</span>`);
        }
        // Specific for Cons tail error with RHS_ORIGIN_DESC
        if (errorObj.rhs_origin_desc) {
            message = message.replace(/{RHS_ORIGIN_DESC}/g, `<span class="error-expr-desc">${escapeHTML(errorObj.rhs_origin_desc)}</span>`);
        }

        if (errorObj.detail) {
            message += `<br><span class="error-detail">Detail: ${escapeHTML(errorObj.detail)}</span>`;
        } else {
            message += `<br><span class="error-detail">No further details.</span>`;
        }

        const causeNodeDesc = causeNode && causeNode.type ? `(Error originated in '${causeNode.type}' construct` : "(Error details:"
        const causeSpan = causeNode && causeNode.span ? ` at ${causeNode.span.start}-${causeNode.span.end})` : ")";
        return `<div class="rich-error-message">${message}<br><span class="error-cause-location">${escapeHTML(causeNodeDesc + causeSpan)}</span></div>`;
    }

    // --- AST Node Types ---
    // Num: { type: 'Num', value: number, span }
    // Var: { type: 'Var', name: string, span }
    // Abs: { type: 'Abs', param: string, body: ASTNode, span, paramInferredType }
    // App: { type: 'App', func: ASTNode, arg: ASTNode, span }
    // Let: { type: 'Let', binder: string, expr1: ASTNode, expr2: ASTNode, span, binderInferredScheme }
    // Nil: { type: 'Nil', span }
    // List: { type: 'List', elements: [ASTNode], span }
    // Cons: { type: 'Cons', head: ASTNode, tail: ASTNode, span }
    // Match: { type: 'Match', expr: ASTNode, cases: [{ pattern: PatternNode, body: ASTNode }], span }
    // PatternNode can be:
    //   NilPattern:  { type: 'NilPattern', span }
    //   ConsPattern: { type: 'ConsPattern', headBinder: string, tailBinder: string, span }
    // RecAbs: { type: 'RecAbs', funcName: string, param: string, body: ASTNode, span, funcNameInferredType, paramInferredType }

    // --- Tokenizer ---
    function tokenize(code) {
        // Added rec
        const regex = /\s*(fn|let|in|match|with|end|nil|rec|::|=>|=|\[|\]|,|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+|\(|\)|\S)\s*/g;
        let match;
        const tokens = [];
        while ((match = regex.exec(code)) !== null) {
            if (match[1]) { // match[1] is the non-whitespace part
                const tokenValue = match[1];
                // Calculate start/end of the actual token, not the whole match with whitespace
                const tokenStart = match.index + match[0].indexOf(tokenValue);
                const tokenEnd = tokenStart + tokenValue.length;
                tokens.push({ value: tokenValue, start: tokenStart, end: tokenEnd });
            }
        }
        return tokens;
    }

    // --- Parser ---
    let tokens = [];
    let currentTokenIndex = 0;

    function currentToken() {
        return tokens[currentTokenIndex]; // Returns token object or undefined
    }

    function peekToken() { // Sometimes useful to peek without consuming, not strictly needed with currentToken()
        if (currentTokenIndex < tokens.length) {
            return tokens[currentTokenIndex];
        }
        return undefined; // Or a special EOF token object
    }

    function nextToken() {
        if (currentTokenIndex < tokens.length) {
            return tokens[currentTokenIndex++]; // Returns token object
        }
        return undefined; // Or throw error / return EOF token
    }

    function expectToken(expected) {
        const tokenObj = nextToken();
        if (!tokenObj || tokenObj.value !== expected) {
            const found = tokenObj ? `'${tokenObj.value}' (at ${tokenObj.start})` : 'end of input';
            const errPos = tokenObj ? tokenObj.start : (tokens.length > 0 ? tokens[tokens.length-1].end : 0);
            throw new Error(`Parse Error: Expected '${expected}' but got ${found}.`);
        }
        return tokenObj;
    }

    function parse(code) {
        tokens = tokenize(code);
        currentTokenIndex = 0;
        if (tokens.length === 0) return null;

        const result = parseExpression();
        if (currentTokenIndex < tokens.length) {
            const unexpectedToken = currentToken();
            throw new Error(`Parse Error: Unexpected token '${unexpectedToken.value}' at position ${unexpectedToken.start} after parsing expression.`);
        }
        return result;
    }

    // New top-level parseExpression that starts the precedence chain
    function parseExpression() {
        return parseConsExpression(); // Start with cons, which will call application, then atom
    }

    // Parses :: (cons) expressions (right-associative)
    function parseConsExpression() {
        let left = parseApplicationChain(); // Higher precedence: applications and atoms
        if (left && currentToken() && currentToken().value === '::') {
            const opToken = nextToken(); // Consume '::'
            const right = parseConsExpression(); // Recursive call for right-associativity
            if (!right) {
                throw new Error(`Parse Error: Expected expression after '::' at ${opToken.start}`);
            }
            return { 
                type: 'Cons', 
                head: left, 
                tail: right, 
                span: { start: left.span.start, end: right.span.end }
            };
        }
        return left;
    }

    // Renamed from parseExpression - parses applications (left-associative chains)
    function parseApplicationChain() {
        let expr = parseAtom();
        if (!expr) return null; 

        let exprStartSpan = expr.span.start;

        // Stop if we hit a token that closes the current expression scope or starts a new one (like 'in' or 'with')
        while (currentTokenIndex < tokens.length && 
               currentToken().value !== ')' && 
               currentToken().value !== '=>' && 
               currentToken().value !== 'in' && 
               currentToken().value !== 'with' && // Added 'with' here
               currentToken().value !== '|' &&   // Added '|' as it terminates an expression before a case body
               currentToken().value !== 'end') { // Added 'end' as it terminates the match expression
            
            const tokObj = currentToken();
            const tok = tokObj.value;

            const isIdentifier = /^[a-zA-Z_][a-zA-Z0-9_]*$/.test(tok);
            // Check if the current token can be the start of an argument to apply
            if (tok === '(' ||          
                /^[0-9]+$/.test(tok) || 
                tok === 'fn' ||         
                tok === 'let' ||        
                tok === 'match' ||      // A nested match can be an argument
                tok === 'nil' ||        // nil can be an argument
                tok === '[' ||          // A list literal can be an argument
                // An identifier that is NOT a terminating keyword for the current parsing context.
                (isIdentifier && 
                 tok !== 'fn' && tok !== 'let' && tok !== 'in' && 
                 tok !== 'match' && tok !== 'with' && tok !== 'end' && tok !== 'nil' && // Added with, end, nil
                 tok !== '=>' && tok !== '=' && tok !== '::' && tok !== '|' && tok !== ']' && tok !== ')')
            ) {
                 const argExpr = parseAtom(); // parseAtom will correctly handle keywords if they start a new structure
                 expr = { 
                    type: 'App', 
                    func: expr, 
                    arg: argExpr, 
                    span: { start: exprStartSpan, end: argExpr.span.end }
                 };
            } else {
                break; 
            }
        }
        return expr;
    }

    // Parses atoms: numbers, variables, (expressions), or abstractions/let-expressions
    function parseAtom() {
        const tokenObj = currentToken(); 
        if (!tokenObj) {
            // This should ideally be caught by the top-level parse function or parseExpression before calling parseAtom if tokens run out.
            throw new Error("Parse Error: Unexpected end of input when expecting an atom.");
        }
        const token = tokenObj.value;
        const tokenStart = tokenObj.start;
        let atomNode;

        if (token === 'fn') {
            nextToken(); // Consume 'fn'
            const paramToken = nextToken();
            if (!paramToken || !/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(paramToken.value)) {
                throw new Error(`Parse Error: Expected parameter name after 'fn', got '${paramToken ? paramToken.value : 'nothing'}' at ${paramToken ? paramToken.start : tokenObj.end}`);
            }
            expectToken('=>');
            const body = parseExpression();
            atomNode = { type: 'Abs', param: paramToken.value, body: body, span: { start: tokenStart, end: body.span.end } };
        } else if (token === 'rec') {
            nextToken(); // Consume 'rec'
            const funcNameToken = nextToken();
            if (!funcNameToken || !/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(funcNameToken.value)) {
                throw new Error(`Parse Error: Expected function name after 'rec', got '${funcNameToken ? funcNameToken.value : 'nothing'}' at ${funcNameToken ? funcNameToken.start : tokenObj.end}`);
            }
            const paramToken = nextToken();
            if (!paramToken || !/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(paramToken.value)) {
                throw new Error(`Parse Error: Expected parameter name after 'rec ${funcNameToken.value}', got '${paramToken ? paramToken.value : 'nothing'}' at ${paramToken ? paramToken.start : funcNameToken.end}`);
            }
            expectToken('=>');
            const body = parseExpression();
            atomNode = { 
                type: 'RecAbs', 
                funcName: funcNameToken.value, 
                param: paramToken.value, 
                body: body, 
                span: { start: tokenStart, end: body.span.end } 
            };
        } else if (token === 'let') {
            nextToken(); // Consume 'let'
            const binderToken = nextToken();
            if (!binderToken || !/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(binderToken.value)) {
                throw new Error(`Parse Error: Expected variable name after 'let', got '${binderToken ? binderToken.value : 'nothing'}' at ${binderToken ? binderToken.start : tokenObj.end}`);
            }
            expectToken('=');
            const expr1 = parseExpression();
            expectToken('in');
            const expr2 = parseExpression();
            atomNode = { type: 'Let', binder: binderToken.value, expr1: expr1, expr2: expr2, span: { start: tokenStart, end: expr2.span.end } };
        } else if (token === 'match') {
            atomNode = parseMatchExpression(tokenObj); // Pass starting token for span
        } else if (token === 'nil') {
            nextToken(); // Consume 'nil'
            atomNode = { type: 'Nil', span: { start: tokenStart, end: tokenObj.end } };
        } else if (token === '[') {
            nextToken(); // Consume '['
            const elements = [];
            let firstElement = true;
            let listEndToken = tokenObj;

            while (currentToken() && currentToken().value !== ']') {
                if (!firstElement) {
                    expectToken(',');
                }
                const elementNode = parseExpression(); // Parse element expression
                elements.push(elementNode);
                listEndToken = elementNode.span.end > listEndToken.end ? {end: elementNode.span.end} : listEndToken;
                firstElement = false;
            }
            const closingBracket = expectToken(']');
            listEndToken = closingBracket; // The end of the list is the closing bracket

            atomNode = { type: 'List', elements: elements, span: { start: tokenStart, end: listEndToken.end } };
        } else if (/^[0-9]+$/.test(token)) {
            nextToken(); // Consume number
            atomNode = { type: 'Num', value: parseInt(token, 10), span: { start: tokenStart, end: tokenObj.end } };
        } else if (/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(token)) {
            nextToken(); // Consume variable
            atomNode = { type: 'Var', name: token, span: { start: tokenStart, end: tokenObj.end } };
        } else if (token === '(') {
            nextToken(); // Consume '('
            const exprInsideParens = parseExpression();
            const closingParenToken = expectToken(')');
            // The AST node is the expression itself, but its span should cover the parentheses
            exprInsideParens.span = { start: tokenStart, end: closingParenToken.end };
            atomNode = exprInsideParens;
        } else {
            throw new Error(`Parse Error: Unexpected token '${token}' at position ${tokenStart} when parsing an atom.`);
        }
        return atomNode;
    }

    function parseMatchExpression(matchToken) {
        nextToken(); // Consume 'match'
        const exprToMatch = parseExpression();
        expectToken('with');
        
        const cases = [];
        let lastCaseEnd = matchToken.end; // For overall span calculation

        while (currentToken() && currentToken().value !== 'end') {
            expectToken('|');
            let patternNode;
            const patternStartToken = currentToken();

            if (!patternStartToken) {
                throw new Error(`Parse Error: Expected pattern after '|' at ${lastCaseEnd}`);
            }

            if (patternStartToken.value === 'nil') {
                nextToken(); // Consume 'nil'
                patternNode = { type: 'NilPattern', span: { start: patternStartToken.start, end: patternStartToken.end } };
            } else if (/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(patternStartToken.value)) {
                // Potential ConsPattern: h :: t
                const headBinderToken = nextToken(); // Consume head binder
                if (currentToken() && currentToken().value === '::') {
                    nextToken(); // Consume '::'
                    const tailBinderToken = nextToken(); 
                    if (!tailBinderToken || !/^[a-zA-Z_][a-zA-Z0-9_]*$/.test(tailBinderToken.value)) {
                        const errPos = tailBinderToken ? tailBinderToken.start : (currentToken() ? currentToken().start : headBinderToken.end);
                        throw new Error(`Parse Error: Expected tail variable binder after '::' in pattern, got ${tailBinderToken ? `'${tailBinderToken.value}'` : 'nothing'} at ${errPos}`);
                    }
                    patternNode = { 
                        type: 'ConsPattern', 
                        headBinder: headBinderToken.value, 
                        tailBinder: tailBinderToken.value, 
                        span: { start: patternStartToken.start, end: tailBinderToken.end }
                    };
                } else {
                    // If not 'h :: t', it's an invalid pattern for now, or could be extended for var patterns if desired.
                    throw new Error(`Parse Error: Invalid pattern. Expected 'nil' or 'h :: t'. Got variable '${headBinderToken.value}' without '::' at ${headBinderToken.start}.`);
                }
            } else {
                throw new Error(`Parse Error: Invalid pattern start '${patternStartToken.value}' at ${patternStartToken.start}. Expected 'nil' or variable for 'h :: t'.`);
            }

            expectToken('=>');
            const caseBody = parseExpression();
            cases.push({ pattern: patternNode, body: caseBody });
            lastCaseEnd = caseBody.span.end;
        }

        if (cases.length === 0) {
            throw new Error(`Parse Error: Match expression must have at least one case. Missing cases after 'with' near ${exprToMatch.span.end}.`);
        }

        const endToken = expectToken('end');
        lastCaseEnd = endToken.end;

        return { 
            type: 'Match', 
            expr: exprToMatch, 
            cases: cases, 
            span: { start: matchToken.start, end: lastCaseEnd }
        };
    }

    // --- Type System & Inference ---
    let nextTypeVarId = 0;

    // Represents a type variable (e.g., a, b, t0, t1)
    class TVar {
        constructor(id, originNode = null) {
            this.id = id;
            this.instance = null; 
            this.origin = originNode;
        }
        toString() { 
            if (this.instance) return this.instance.toString();
            return `t${this.id}`;
        }
    }

    // Represents a type constructor (e.g., Int, ->)
    class TCon {
        constructor(name, args = [], originNode = null) {
            this.name = name; 
            this.args = args; 
            this.origin = originNode;
        }
        toString() { 
            const needsParens = (type) => {
                const prunedType = prune(type);
                return prunedType instanceof TCon && prunedType.name === '->';
            };

            if (this.name === '->' && this.args.length === 2) {
                const p1 = prune(this.args[0]);
                const p2 = prune(this.args[1]);
                const p1Str = needsParens(p1) ? `(${p1.toString()})` : p1.toString();
                // For right-associativity of ->, p2 (the return type) is NOT parenthesized here
                // even if it's an arrow type. Its own toString() will handle its structure.
                return `${p1Str} -> ${p2.toString()}`;
            }
            if (this.args.length > 0) {
                const argsStr = this.args.map(arg => {
                    const argPruned = prune(arg); 
                    return needsParens(argPruned) ? `(${argPruned.toString()})` : argPruned.toString();
                }).join(' '); // Space separated arguments
                // e.g. "List (A -> B)" or "Pair (A -> B) (C -> D)"
                // If constructor name implies brackets/parens like Tuple(A,B) then this might need adjustment.
                // For now, space separation seems a good general default.
                return `${this.name}${argsStr ? ` ${argsStr}` : ''}`; 
            }
            return this.name;
        }
    }

    // Global T_INT needs to be created without a specific origin initially,
    // or it implies all Ints come from one place. We'll create specific instances in _infer.
    // const T_INT = new TCon('Int'); // Will be replaced by contextual creation

    const T_LIST = (elementType, originNode = null) => new TCon('List', [elementType], originNode);

    function newTVar(originNode = null) {
        return new TVar(nextTypeVarId++, originNode);
    }

    // Type Environment: maps variable names to Types or TypeSchemes
    function createInitialEnv() {
        // return {}; // Start with an empty environment
        // Or, add some built-in types if we had them (e.g., for built-in functions)
        // For now, we don't have built-ins that are not part of the language itself (like numbers).
        return new Map();
    }

    // Dereferences a type variable until it's a TCon or an unbound TVar.
    function prune(t) {
        if (t instanceof TVar && t.instance) {
            t.instance = prune(t.instance); // Path compression
            return t.instance;
        }
        return t;
    }

    function unify(t1, t2) {
        const type1 = prune(t1);
        const type2 = prune(t2);

        if (type1 instanceof TVar) {
            if (type1 !== type2) { // Avoid unifying a var with itself
                if (occursCheck(type1, type2)) {
                    throw new Error(`Type Error: Occurs check failed for t${type1.id} in ${type2.toString()}`);
                }
                type1.instance = type2;
            }
        } else if (type2 instanceof TVar) {
            if (type1 !== type2) {
                 if (occursCheck(type2, type1)) {
                    throw new Error(`Type Error: Occurs check failed for t${type2.id} in ${type1.toString()}`);
                }
                type2.instance = type1;
            }
        } else if (type1 instanceof TCon && type2 instanceof TCon) {
            if (type1.name !== type2.name || type1.args.length !== type2.args.length) {
                throw new Error(`Type Error: Cannot unify ${type1.toString()} with ${type2.toString()} (constructor mismatch)`);
            }
            for (let i = 0; i < type1.args.length; i++) {
                unify(type1.args[i], type2.args[i]);
            }
        } else {
            // This case should ideally not be reached if types are well-formed TVar or TCon
            throw new Error(`Type Error: Cannot unify ${type1.toString()} with ${type2.toString()} (unknown types)`);
        }
    }

    function occursCheck(tvar, type) {
        const t = prune(type);
        if (t === tvar) {
            return true;
        }
        if (t instanceof TCon) {
            return t.args.some(arg => occursCheck(tvar, arg));
        }
        return false;
    }

    // --- Type Copying with New Origin ---
    function deepCopyTypeWithNewOrigin(type, newOriginNode, visitedTvrs = new Map()) {
        const t = prune(type); // Work with the representative type

        if (t instanceof TVar) {
            if (visitedTvrs.has(t)) {
                return visitedTvrs.get(t); // Return already copied TVar to maintain sharing
            }
            // Create a new TVar, copying the ID for debuggability, but with the new origin.
            // Crucially, it does *not* copy the instance; it's a fresh var.
            const newTvar = new TVar(t.id, newOriginNode); 
            visitedTvrs.set(t, newTvar);
            // If original tvar had an instance, its copy should also point to a copy of that instance.
            // This makes it truly deep. But for var use, we want a *fresh* version of its current type at use site.
            // So, if t.instance exists, we should copy that with the newOriginNode as well.
            if (t.instance) {
                newTvar.instance = deepCopyTypeWithNewOrigin(t.instance, newOriginNode, visitedTvrs);
            }
            return newTvar;
        } else if (t instanceof TCon) {
            const newArgs = t.args.map(arg => deepCopyTypeWithNewOrigin(arg, newOriginNode, visitedTvrs));
            return new TCon(t.name, newArgs, newOriginNode);
        }
        return t; // Should not happen if types are well-formed
    }

    // --- Type Scheme related functions ---
    // Represents a type scheme (forall a, b. type)
    class TypeScheme {
        constructor(type, quantifiedVars, originNode = null) { // Added originNode
            this.type = type; 
            this.quantifiedVars = quantifiedVars; 
            this.origin = originNode; // Origin of the let-binding expression usually
        }

        toString() { 
            if (this.type instanceof TVar) {
                return this.type.toString();
            } else if (this.type instanceof TCon) {
                return this.type.toString();
            } else if (this.type instanceof TypeScheme) {
                return `forall ${this.quantifiedVars.map(v => v.toString()).join(', ')}. ${this.type.toString()}`;
            }
            return 'TYPE_UNKNOWN';
        }
    }

    function generalize(type, env, defSiteOriginNode = null) { // Added defSiteOriginNode
        const typeFreeVars = getFreeTypeVars(type);
        const envFreeVars = getFreeTypeVarsInEnv(env);
        const quantifiedVars = new Set();
        for (const tvar of typeFreeVars) {
            if (!envFreeVars.has(tvar)) {
                quantifiedVars.add(tvar);
            }
        }
        return new TypeScheme(type, quantifiedVars, defSiteOriginNode);
    }

    function instantiate(typeScheme, useSiteOriginNode) { // Added useSiteOriginNode
        const substitutionMap = new Map(); // Maps original TVar ID from quantifiedVars to new fresh TVar instance
        
        typeScheme.quantifiedVars.forEach(oldVar => {
            // Fresh TVars created during instantiation get the useSiteOriginNode
            substitutionMap.set(oldVar.id, newTVar(useSiteOriginNode)); 
        });

        // Recursive helper to copy the scheme's body, applying substitutions and new origins.
        function copyAndSubstitute(type) {
            const t = prune(type);
            if (t instanceof TVar) {
                if (substitutionMap.has(t.id)) {
                    return substitutionMap.get(t.id); // Return the fresh TVar with useSiteOrigin
                }
                // This TVar was free in the scheme's body (not quantified).
                // It needs to be copied with the new useSiteOriginNode.
                // Use deepCopy to handle its potential .instance, ensuring the whole structure is fresh for this use site.
                return deepCopyTypeWithNewOrigin(t, useSiteOriginNode, new Map()); // Pass fresh visitedTvrs map
            } else if (t instanceof TCon) {
                // Recursively copy arguments, then create new TCon with useSiteOriginNode
                const newArgs = t.args.map(arg => copyAndSubstitute(arg));
                return new TCon(t.name, newArgs, useSiteOriginNode);
            }
            return t; // Should not occur for well-formed types
        }
        return copyAndSubstitute(typeScheme.type);
    }

    // For now, env maps var names to Types. No Scheme/generalization/instantiation yet.
    function infer(astNode, env) {
        // Resetting inferred types on the AST can be done here if AST is reused across edits.
        // However, parse() creates a new AST each time, so it's not strictly needed now.
        return _infer(astNode, env);
    }

    function _infer(astNode, env) {
        let inferredType;
        // Clear any previous error for this node (if AST is ever reused/re-inferred on)
        // delete astNode.error; // Not strictly needed as we create fresh ASTs

        try { // Outer try for operations within this specific node's inference
            switch (astNode.type) {
                case 'Num':
                    inferredType = new TCon('Int', [], astNode); // Set origin to Num node
                    break;
                case 'Nil':
                    inferredType = T_LIST(newTVar(astNode), astNode); // TVar and TList get Nil node as origin
                    break;
                case 'List': {
                    if (astNode.elements.length === 0) {
                        inferredType = T_LIST(newTVar(astNode), astNode);
                    } else {
                        const elementTypes = astNode.elements.map(el => _infer(el, env));
                        const firstElementTypePruned = prune(elementTypes[0]);
                        
                        for (let i = 1; i < elementTypes.length; i++) {
                            const currentElementTypePruned = prune(elementTypes[i]);
                            try {
                                unify(firstElementTypePruned, currentElementTypePruned);
                            } catch (e) {
                                const originFirst = firstElementTypePruned.origin || astNode.elements[0]; 
                                const originCurrent = currentElementTypePruned.origin || astNode.elements[i]; 

                                if (originFirst) originFirst.errorHighlight = 'conflict_source';
                                if (originCurrent) originCurrent.errorHighlight = 'conflict_target';
                                
                                astNode.error = { 
                                    messageTemplate: "List Element Type Mismatch: Element (type {RHS_TYPE} from {RHS_EXPR_DESC}) conflicts with the list's established element type {LHS_TYPE} (from element at {LHS_EXPR_DESC}). All list elements must have the same type.",
                                    lhs: { type: firstElementTypePruned, originNode: originFirst },
                                    rhs: { type: currentElementTypePruned, originNode: originCurrent },
                                    detail: e.message.replace("Type Error: ", "")
                                };
                                break; 
                            }
                        }
                        if (!astNode.error) { 
                            for(const elNode of astNode.elements) {
                                if (elNode.error && typeof elNode.error === 'string') { 
                                    astNode.error = `Error within a list element: ${elNode.error}`; 
                                    break;
                                }
                            }
                        }
                        inferredType = T_LIST(firstElementTypePruned, astNode); 
                    }
                    break;
                }
                case 'Cons': { 
                    const headType = _infer(astNode.head, env);
                    const tailType = _infer(astNode.tail, env);
                    const elementType = newTVar(astNode.head); 
                    let headUnificationFailed = false; 
                    try {
                        unify(headType, elementType);
                    } catch (e) {
                        headUnificationFailed = true;
                        const prunedHeadType = prune(headType);
                        const originHead = prunedHeadType.origin || astNode.head;
                        if (originHead) originHead.errorHighlight = 'conflict_source'; 
                        if (elementType.origin) elementType.origin.errorHighlight = 'conflict_target';
                        
                        astNode.error = { 
                            messageTemplate: "Type Issue with Head of '::': The head expression (type {LHS_TYPE} from {LHS_EXPR_DESC}) has an internal type conflict, often an occurs check failure, when used as a list element (expected type {RHS_TYPE} derived from usage).",
                            lhs: { type: prunedHeadType, originNode: originHead },
                            rhs: { type: elementType, originNode: (elementType.origin || astNode.head) }, 
                            detail: e.message.replace("Type Error: ", "")
                        };
                    }
                    if (!astNode.error) { 
                        try {
                            const expectedTailListType = T_LIST(elementType, astNode.tail); 
                            unify(tailType, expectedTailListType);
                        } catch (e) {
                            const prunedTailType = prune(tailType);
                            const originTail = prunedTailType.origin || astNode.tail;
                            const originHeadForElementType = elementType.origin || astNode.head; 

                            if (originTail) originTail.errorHighlight = 'conflict_source';
                            if (originHeadForElementType) originHeadForElementType.errorHighlight = 'conflict_target';

                            astNode.error = { 
                                messageTemplate: "Type Mismatch for Tail of '::': The tail (type {LHS_TYPE} from {LHS_EXPR_DESC}) is not a list of elements matching the head's type ({RHS_TYPE} from expression at {RHS_ORIGIN_DESC}).",
                                lhs: { type: prunedTailType, originNode: originTail },
                                rhs: { type: elementType, originNode: originHeadForElementType }, 
                                rhs_origin_desc: `(${originHeadForElementType.span?.start}-${originHeadForElementType.span?.end})`, // For {RHS_ORIGIN_DESC}
                                detail: e.message.replace("Type Error: ", "")
                            };
                        }
                    }
                    if (!astNode.error) {
                        if (astNode.head.error && typeof astNode.head.error === 'string') {
                            astNode.error = `Error in head of '::': ${astNode.head.error}`;
                        } else if (astNode.tail.error && typeof astNode.tail.error === 'string') {
                            astNode.error = `Error in tail of '::': ${astNode.tail.error}`;
                        }
                    }
                    inferredType = T_LIST(prune(elementType), astNode); 
                    break;
                }
                case 'Var': {
                    const typeInEnv = env.get(astNode.name);
                    if (!typeInEnv) {
                        astNode.error = {
                            messageTemplate: "Undefined Variable: The name '{VAR_NAME}' is not defined in the current scope.",
                            var_name: astNode.name,
                            lhs: { type: newTVar(astNode), originNode: astNode }, // Placeholder for LHS, points to the var itself
                            rhs: { type: newTVar(astNode), originNode: astNode }, // Placeholder for RHS
                            detail: `Variable '${astNode.name}' could not be found.`
                        };
                        astNode.errorHighlight = 'conflict_source'; 
                        inferredType = newTVar(astNode); 
                    } else if (typeInEnv instanceof TypeScheme) {
                        inferredType = instantiate(typeInEnv, astNode); // Pass astNode for origin setting
                    } else {
                        // Use the type from env directly. Its .origin points to its definition site.
                        // The current Var node (astNode) is the use site.
                        // If this type is involved in a unification error, its .origin will be used.
                        inferredType = typeInEnv; 
                    }
                    break;
                }
                case 'Abs': { 
                    const paramType = newTVar(astNode); // Parameter's type var originates from the Abs node (or param ident node if we had one)
                    astNode.paramInferredType = paramType;
                    const newEnv = new Map(env).set(astNode.param, paramType);
                    const bodyType = _infer(astNode.body, newEnv);
                    inferredType = new TCon('->', [paramType, bodyType], astNode); // Arrow type originates from Abs node
                    break;
                }
                case 'Let': { 
                    const t_expr1 = _infer(astNode.expr1, env);
                    let scheme_expr1;
                    if (astNode.expr1.error) {
                        // Create a dummy scheme with a fresh var if expr1 failed
                        scheme_expr1 = new TypeScheme(newTVar(astNode.expr1), new Set(), astNode.expr1); 
                    } else {
                        // Pass origin of the let expression (astNode.expr1 is closer to the def)
                        scheme_expr1 = generalize(t_expr1, env, astNode.expr1); 
                    }
                    astNode.binderInferredScheme = scheme_expr1;
                    const newEnv = new Map(env).set(astNode.binder, scheme_expr1);
                    inferredType = _infer(astNode.expr2, newEnv);
                    break;
                }
                case 'App': { 
                    const funcType = _infer(astNode.func, env);
                    const argType = _infer(astNode.arg, env);
                    const returnType = newTVar(astNode); 
                    try {
                        const expectedFuncArrowType = new TCon('->', [argType, returnType], astNode); 
                        unify(funcType, expectedFuncArrowType);
                        inferredType = prune(returnType);
                    } catch (e) { 
                        const prunedFuncType = prune(funcType);
                        const prunedArgType = prune(argType); 
                        const funcOriginNode = prunedFuncType.origin || astNode.func; 
                        const argOriginNode = prunedArgType.origin || astNode.arg; 

                        let msgTemplate;

                        if (prunedFuncType instanceof TCon && prunedFuncType.name === '->') {
                            const expectedParamType = prune(prunedFuncType.args[0]);
                            const originOfExpectedParamType = expectedParamType.origin || funcOriginNode; 

                            if (originOfExpectedParamType && originOfExpectedParamType !== astNode) originOfExpectedParamType.errorHighlight = 'conflict_source';
                            if (argOriginNode && argOriginNode !== astNode) argOriginNode.errorHighlight = 'conflict_target';

                            msgTemplate = "Type Mismatch: Cannot apply function (type {LHS_TYPE} from {LHS_EXPR_DESC}) to argument (type {ARG_TYPE} from {ARG_EXPR_DESC}). Function expected an argument of type {EXPECTED_ARG_TYPE} but received {ARG_TYPE}.";
                        
                        } else if (!(prunedFuncType instanceof TVar)) { 
                            if (funcOriginNode && funcOriginNode !== astNode) funcOriginNode.errorHighlight = 'conflict_source';
                            if (argOriginNode && argOriginNode !== astNode) argOriginNode.errorHighlight = 'conflict_target';
                            msgTemplate = "Type Error: Expression (type {LHS_TYPE} from {LHS_EXPR_DESC}) is not a function and cannot be applied to argument (type {ARG_TYPE} from {ARG_EXPR_DESC}).";
                        
                        } else { 
                            if (funcOriginNode && funcOriginNode !== astNode) funcOriginNode.errorHighlight = 'conflict_source';
                            if (argOriginNode && argOriginNode !== astNode) argOriginNode.errorHighlight = 'conflict_target';
                            msgTemplate = "Type Mismatch: Expression (type {LHS_TYPE} from {LHS_EXPR_DESC}) was expected to be a function that can accept argument (type {ARG_TYPE} from {ARG_EXPR_DESC}), but type inference led to a conflict.";
                        }
                        
                        astNode.error = {
                            messageTemplate: msgTemplate,
                            lhs: { type: prunedFuncType, originNode: funcOriginNode }, 
                            rhs: { type: new TCon('->', [prunedArgType, returnType], astNode), originNode: astNode }, 
                            arg: { type: prunedArgType, originNode: argOriginNode }, 
                            expected_arg_type: (prunedFuncType instanceof TCon && prunedFuncType.name === '->') ? prune(prunedFuncType.args[0]) : null, 
                            ret_type: returnType, 
                            detail: e.message.replace("Type Error: ", "")
                        };
                        inferredType = returnType; 
                    }
                    break;
                }
                case 'Match': {
                    const exprToMatchType = _infer(astNode.expr, env);
                    const listElementType = newTVar(astNode.expr); // Element type related to matched expression

                    try {
                        unify(exprToMatchType, T_LIST(listElementType, astNode.expr));
                    } catch (e) {
                        astNode.expr.error = `Expression in match must be a list. Got '${prettyPrintType(prune(exprToMatchType))}'. Details: ${e.message}`;
                        astNode.error = "Expression in match is not a list type.";
                        inferredType = newTVar(astNode); 
                        break; 
                    }

                    if (astNode.cases.length === 0) {
                        astNode.error = "Match expression must have at least one case.";
                        inferredType = newTVar(astNode); 
                        break;
                    }
                    let firstCaseBodyType = null;
                    let firstCaseBodyOriginNode = null; 

                    for (const caseItem of astNode.cases) {
                        let caseEnv = new Map(env); // Create new env for each case
                        const pattern = caseItem.pattern;
                        let caseBodyType;
                        if (pattern.type === 'NilPattern') {
                            caseBodyType = _infer(caseItem.body, caseEnv);
                        } else if (pattern.type === 'ConsPattern') {
                            hasConsCase = true;
                            const headBinderType = prune(listElementType); 
                            // headBinderType.origin is from listElementType (i.e., astNode.expr for match)
                            caseEnv.set(pattern.headBinder, headBinderType); 

                            const tailBinderType = T_LIST(prune(listElementType), pattern); // tail's list type originates from pattern
                            caseEnv.set(pattern.tailBinder, tailBinderType);
                            caseBodyType = _infer(caseItem.body, caseEnv);
                        }
                        if (!firstCaseBodyType) {
                            firstCaseBodyType = caseBodyType;
                        } else {
                             try {
                                unify(firstCaseBodyType, caseBodyType);
                            } catch (e) {
                                const originFirstCaseBody = firstCaseBodyType.origin || firstCaseBodyOriginNode;
                                const originCurrentCaseBody = caseBodyType.origin || caseItem.body;

                                if (originFirstCaseBody) originFirstCaseBody.errorHighlight = 'conflict_source';
                                if (originCurrentCaseBody) originCurrentCaseBody.errorHighlight = 'conflict_target';
                                
                                astNode.error = { 
                                    messageTemplate: "Type Mismatch in match cases: Case body (type {RHS_TYPE} from {RHS_EXPR_DESC}) is incompatible with type {LHS_TYPE} from a preceding case body (at {LHS_EXPR_DESC}). All case bodies must result in the same type.",
                                    lhs: { type: firstCaseBodyType, originNode: originFirstCaseBody },
                                    rhs: { type: caseBodyType, originNode: originCurrentCaseBody },
                                    detail: e.message.replace("Type Error: ", "")
                                };
                                break; 
                            }
                        }
                    }
                    
                    if (!astNode.error) { 
                        if (astNode.expr.error && typeof astNode.expr.error === 'string') {
                             astNode.error = `Error in matched expression: ${astNode.expr.error}`;
                        } else {
                            for (const caseItem of astNode.cases) {
                                if (caseItem.body.error && typeof caseItem.body.error === 'string') {
                                    astNode.error = `Error in match case body: ${caseItem.body.error}`;
                                    break;
                                }
                            }
                        }
                    }
                    inferredType = prune(firstCaseBodyType) ? prune(firstCaseBodyType) : newTVar(astNode);
                    break;
                }
                case 'RecAbs': { // rec funcName param => body
                    const paramType = newTVar(astNode); // Origin for param type var is the RecAbs node itself
                    const returnType = newTVar(astNode); // Origin for return type var is also RecAbs node
                    
                    const funcType = new TCon('->', [paramType, returnType], astNode); // func type originates from RecAbs
                    astNode.funcNameInferredType = funcType; // Store for pretty printer
                    astNode.paramInferredType = paramType;  // Store for pretty printer

                    const newEnv = new Map(env);
                    newEnv.set(astNode.funcName, funcType); // Add f: alpha -> beta to env
                    newEnv.set(astNode.param, paramType);   // Add x: alpha to env

                    const bodyType = _infer(astNode.body, newEnv);

                    try {
                        unify(bodyType, returnType); // Unify body's actual type with declared return type beta
                    } catch (e) {
                        const originBody = bodyType.origin || astNode.body;
                        const originRetTypeExpectation = returnType.origin || astNode; // returnType var originates from RecAbs

                        if (originBody) originBody.errorHighlight = 'conflict_source';
                        if (originRetTypeExpectation) originRetTypeExpectation.errorHighlight = 'conflict_target';

                        astNode.error = {
                            messageTemplate: "Recursive function body type mismatch: The body (type {LHS_TYPE} from {LHS_EXPR_DESC}) does not match the function's implied return type ({RHS_TYPE} based on usage or initial assumption from {RHS_EXPR_DESC}).",
                            lhs: { type: bodyType, originNode: originBody },
                            rhs: { type: returnType, originNode: originRetTypeExpectation },
                            detail: e.message.replace("Type Error: ", "")
                        };
                        // Even if unification fails, the funcType (alpha -> beta) is the type of the rec expression
                    }
                    // The type of the rec expression is funcType (alpha -> beta)
                    inferredType = funcType; 
                    break;
                }
                default:
                    // Should not happen with a valid AST
                    astNode.error = `Unknown AST node type: ${astNode.type}`;
                    inferredType = newTVar(); // Placeholder
                    break;
            }
        } catch (criticalError) {
            // This catch is for unexpected errors during an _infer call itself, not sub-calls.
            console.error("Critical error during _infer for node:", astNode, criticalError);
            astNode.error = `Internal inference error: ${criticalError.message}`;
            inferredType = newTVar(); // Placeholder
        }

        astNode.inferredType = inferredType;
        return inferredType;
    }

    // --- Pretty Printer (for Types) ---
    function prettyPrintType(type) {
        // If type is a TypeScheme, print its full form.
        if (type instanceof TypeScheme) {
            return type.toString();
        }
        const t = prune(type);
        return t.toString(); // Rely on TVar and TCon toString methods for non-schemes
    }

    // --- HTML Escaping Helper ---
    function escapeHTML(str) {
        if (typeof str !== 'string') return '';
        return str.replace(/[&<>\"]/g, function (match) { 
            switch (match) {
                case '&': return '&amp;';
                case '<': return '&lt;';
                case '>': return '&gt;';
                case '\"': return '&quot;';
                default: return match;
            }
        });
    }

    // --- Indentation Helper ---
    const INDENT_SIZE = 2;
    function indent(level) {
        return ' '.repeat(level * INDENT_SIZE);
    }

    // --- Pretty Printer (for AST with Types) ---
    function prettyPrintAST(node, indentLevel = 0) { 
        if (!node) return '';

        const typeStr = (typeObj) => typeObj ? prettyPrintType(typeObj) : 'TYPE_UNKNOWN';
        const nodeErrorMsg = node.error ? escapeHTML(node.error) : null;
        let errClass = nodeErrorMsg ? ' hl-error-expr' : '';
        const errDataAttr = nodeErrorMsg ? ` data-errormsg="${nodeErrorMsg}"` : '';

        if (node.errorHighlight) {
            errClass += (errClass ? ' ' : '') + `hl-${node.errorHighlight}`; 
        }

        const isComplex = (n) => n && (n.type === 'Abs' || n.type === 'Let' || n.type === 'Match' || n.type === 'Cons');

        switch (node.type) {
            case 'Num': {
                const typeDataAttr = !nodeErrorMsg ? ` data-type="${typeStr(node.inferredType)}"` : '';
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return `<span class="hl-literal${finalErrClass}"${typeDataAttr}${errDataAttr}>${node.value}</span>`;
            }
            case 'NilPattern': 
                return `<span class="hl-keyword">nil</span>`;
            case 'ConsPattern':
                return `<span class="hl-identifier">${node.headBinder}</span> <span class="hl-operator">::</span> <span class="hl-identifier">${node.tailBinder}</span>`;
            case 'Nil': {
                const typeDataAttr = !nodeErrorMsg ? ` data-type="${typeStr(node.inferredType)}"` : '';
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return `<span class="hl-keyword${finalErrClass}"${typeDataAttr}${errDataAttr}>nil</span>`;
            }
            case 'List': {
                const elementsStr = node.elements.map(el => prettyPrintAST(el, indentLevel)).join(", ");
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return `<span class="hl-punctuation${finalErrClass}"${errDataAttr}>[</span>${elementsStr}<span class="hl-punctuation${finalErrClass}"${errDataAttr}>]</span>`;
            }
            case 'Cons': {
                let headStr = prettyPrintAST(node.head, indentLevel);
                if (node.head.type === 'Cons' || node.head.type === 'Abs' || node.head.type === 'Let') {
                    headStr = `<span class="hl-punctuation">(</span>${headStr}<span class="hl-punctuation">)</span>`;
                }
                
                const tailBody = node.tail;
                let tailStr = prettyPrintAST(tailBody, isComplex(tailBody) ? indentLevel + 1 : indentLevel);
                if (isComplex(tailBody)) {
                    tailStr = `\n${indent(indentLevel + 1)}${tailStr}`;
                }

                if (tailBody.type === 'Abs' || tailBody.type === 'Let' || tailBody.type === 'App' ) { 
                     if (!isComplex(tailBody)) { 
                        tailStr = `<span class="hl-punctuation">(</span>${tailStr.trim()}<span class="hl-punctuation">)</span>`;
                     } 
                }
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return `${headStr} <span class="hl-operator${finalErrClass}"${errDataAttr}>::</span> ${tailStr.trim()}`;
            }
            case 'Var': {
                const typeDataAttr = !nodeErrorMsg ? ` data-type="${typeStr(node.inferredType)}"` : '';
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return `<span class="hl-identifier${finalErrClass}"${typeDataAttr}${errDataAttr}>${node.name}</span>`;
            }
            case 'Abs': {
                const paramTypeStr = typeStr(node.paramInferredType);
                const bodyNode = node.body;
                let bodyStr = prettyPrintAST(bodyNode, indentLevel + 1);
                
                if (isComplex(bodyNode)) {
                    bodyStr = `\n${indent(indentLevel + 1)}${bodyStr}`;
                } else {
                    bodyStr = ` ${bodyStr}`;
                }
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return `<span class="hl-keyword${finalErrClass}"${errDataAttr}>fn</span> <span class="hl-identifier" data-type="${paramTypeStr}">${node.param}</span> <span class="hl-operator">=></span>${bodyStr.trimEnd()}`;
            }
            case 'App': {
                let funcStr = prettyPrintAST(node.func, indentLevel);
                if (node.func.type === 'Abs' || node.func.type === 'Let') {
                    funcStr = `<span class="hl-punctuation">(</span>${funcStr}<span class="hl-punctuation">)</span>`;
                }

                const argNode = node.arg;
                let argStr = prettyPrintAST(argNode, isComplex(argNode) ? indentLevel + 1 : indentLevel);

                if (isComplex(argNode)) {
                    argStr = `\n${indent(indentLevel + 1)}${argStr}`;
                } else {
                    argStr = ` ${argStr}`;
                }

                if (argNode.type === 'Abs' || argNode.type === 'App' || argNode.type === 'Let' || argNode.type === 'Cons') { 
                     if (!isComplex(argNode)) {
                        argStr = `<span class="hl-punctuation">(</span>${argStr.trim()}<span class="hl-punctuation">)</span>`;
                     } 
                }
                const appOutput = `${funcStr}${argStr.trimEnd()}`;
                if (nodeErrorMsg) {
                    return `<span class="${errClass.trim()}"${errDataAttr}>${appOutput}</span>`;
                }
                return appOutput;
            }
            case 'Let': {
                const binderSchemeStr = node.binderInferredScheme ? prettyPrintType(node.binderInferredScheme) : 'SCHEME_UNKNOWN';
                
                const expr1Node = node.expr1;
                let expr1Str = prettyPrintAST(expr1Node, indentLevel + 1);
                expr1Str = `\n${indent(indentLevel + 1)}${expr1Str}`;

                const expr2Node = node.expr2;
                let expr2Str = prettyPrintAST(expr2Node, indentLevel + 1);
                expr2Str = `\n${indent(indentLevel + 1)}${expr2Str}`;
                
                const baseOutput = 
                    `<span class="hl-keyword">let</span> <span class="hl-identifier" data-type="${binderSchemeStr}">${node.binder}</span> ` +
                    `<span class="hl-operator">=</span>${expr1Str.trimEnd()}\n${indent(indentLevel)}` +
                    `<span class="hl-keyword">in</span>${expr2Str.trimEnd()}`;

                if (nodeErrorMsg) {
                    return `<span class="${errClass.trim()}"${errDataAttr}>${baseOutput}</span>`;
                }
                return baseOutput;
            }
            case 'Match': {
                const exprStr = prettyPrintAST(node.expr, indentLevel);
                const casesHtml = node.cases.map(c => {
                    const patternStr = prettyPrintAST(c.pattern, indentLevel + 1); 
                    const bodyNode = c.body;
                    let bodyStr = prettyPrintAST(bodyNode, indentLevel + 2);
                    bodyStr = `\n${indent(indentLevel + 2)}${bodyStr}`;
                    
                    return `\n${indent(indentLevel + 1)}<span class="hl-operator">|</span> ${patternStr} <span class="hl-operator">=></span>${bodyStr.trimEnd()}`;
                }).join(''); 

                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return (
                    `<span class="hl-keyword${finalErrClass}"${errDataAttr}>match</span> ${exprStr} ` +
                    `<span class="hl-keyword">with</span>${casesHtml}\n${indent(indentLevel)}` +
                    `<span class="hl-keyword">end</span>`
                );
            }
            case 'RecAbs': {
                const funcNameTypeStr = typeStr(node.funcNameInferredType);
                const paramTypeStr = typeStr(node.paramInferredType);
                const bodyNode = node.body;
                let bodyStr = prettyPrintAST(bodyNode, indentLevel + 1);
                
                if (isComplex(bodyNode)) {
                    bodyStr = `\n${indent(indentLevel + 1)}${bodyStr}`;
                } else {
                    bodyStr = ` ${bodyStr}`;
                }

                // errClass and errDataAttr apply to the 'rec' keyword if the RecAbs node itself has an error.
                const finalErrClass = errClass ? ` ${errClass.trim()}` : '';
                return (
                    `<span class="hl-keyword${finalErrClass}"${errDataAttr}>rec</span> ` +
                    `<span class="hl-identifier" data-type="${funcNameTypeStr}">${node.funcName}</span> ` +
                    `<span class="hl-identifier" data-type="${paramTypeStr}">${node.param}</span> ` +
                    `<span class="hl-operator">=></span>${bodyStr.trimEnd()}`
                );
            }
            default:
                console.error("Unknown node type in prettyPrintAST:", node);
                return `<span class="error">ErrorPrettyPrinting(${node.type})</span>`;
        }
    }

    // --- Helper: Get Free Type Variables ---
    function getFreeTypeVars(type, freeVarsSet = new Set()) {
        const t = prune(type);
        if (t instanceof TVar) {
            freeVarsSet.add(t);
        } else if (t instanceof TCon) {
            t.args.forEach(arg => getFreeTypeVars(arg, freeVarsSet));
        }
        // If t is a TypeScheme, this function should ideally be called on its .type after instantiation,
        // or handle it by not adding its quantified vars if called directly on a scheme (not typical here).
        return freeVarsSet;
    }

    function getFreeTypeVarsInEnv(env) {
        const freeVars = new Set();
        for (const value of env.values()) {
            if (value instanceof TypeScheme) {
                // For a type scheme 'forall a. T', its free variables in the env context
                // are the free variables of T, *excluding* 'a'.
                // However, generalize() needs free vars of the *current* env types.
                // If 'value' is a scheme, its contribution to env's free vars is FV(value.type) - value.quantifiedVars
                // This is complex. A simpler way for generalize: FV(type) - FV(env types).
                // The types stored in env for generalization are post-instantiation or non-polymorphic.
                // So, just get free vars of the concrete types.
                getFreeTypeVars(value.type ? value.type : value, freeVars); // If it's a scheme, use its type, else use value directly.
                                                                        // This part needs care: env contains schemes and simple types.
                                                                        // For `generalize`, we care about TVars in env that are *not yet bound* by a Scheme's forall.
                                                                        // So, if it is a TVar or TCon directly in env (like a lambda param), its free vars are added.
                                                                        // If it is a TypeScheme, its quantified variables are NOT free in the outer context.
                // Correct approach for generalize: FV(type to generalize) - FV(current lexical env)
                // FV(current lexical env) means all free vars of types of *active* lexical variables.
                // For TVars directly in env (like lambda params):
                 if (value instanceof TVar || value instanceof TCon) {
                    getFreeTypeVars(value, freeVars);
                }
                // For TypeSchemes in env, their quantified variables are NOT free in this env.
                // Only the free variables within their .type that are *not* in their .quantifiedVars set.
                else if (value instanceof TypeScheme) {
                    const schemeFreeVars = getFreeTypeVars(value.type);
                    for (const fv of schemeFreeVars) {
                        if (!value.quantifiedVars.has(fv)) {
                            freeVars.add(fv);
                        }
                    }
                }


            } else if (value instanceof TVar || value instanceof TCon) { // Non-scheme types
                 getFreeTypeVars(value, freeVars);
            }
        }
        return freeVars;
    }

    // Initial message
    if (codeInput.value.trim() === '') {
        outputArea.textContent = 'Type some code or click "Next Example"!';
    } else {
        // If there is code on load (e.g. browser saved value), process it.
        const event = new Event('input', { bubbles: true, cancelable: true });
        codeInput.dispatchEvent(event);
    }
}); 