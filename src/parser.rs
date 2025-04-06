use crate::expr::{Exp, Expr};
use crate::pre::{Field, Value};
use std::iter::Peekable;
use std::str::Chars;

// --- Lexer ---

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Zero,       // 0
    One,        // 1
    Top,        // T
    Assign,     // :=
    Eq,         // ==
    Plus,       // +
    And,        // &
    Xor,        // ^
    Minus,      // -
    Not,        // !
    Semicolon,  // ;
    Star,       // *
    Dup,        // dup
    LtlX,       // X
    LtlU,       // U
    LParen,     // (
    RParen,     // )
    Field(u32), // x followed by digits
    DirectiveK(u32), // #k=N
    Eof,        // End of input
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    num_fields: u32, // Number of fields, max field index is k-1
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, num_fields: u32) -> Self {
        Lexer {
            iter: input.chars().peekable(),
            num_fields,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.iter.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.iter.peek()
    }

    fn skip_whitespace(&mut self) {
        loop { // Use loop for potentially multiple comments/whitespace blocks
            self.skip_single_whitespace_block();
            // Check if the next character starts a comment
            if let Some(&'/') = self.peek_char() {
                // Need to peek ahead two characters
                let mut chars = self.iter.clone(); // Clone to peek without consuming
                if chars.next() == Some('/') && chars.next() == Some('/') {
                    // Consume the '//'
                    self.next_char();
                    self.next_char();
                    // Consume the rest of the line
                    while let Some(&c) = self.peek_char() {
                        if c == '\n' {
                            self.next_char(); // Consume the newline
                            break; // End of comment line
                        } else {
                            self.next_char(); // Consume comment character
                        }
                    }
                    // After consuming the comment and its newline, continue the loop
                    // to handle potential whitespace/comments on the next line
                    continue;
                } else {
                    // It's just a single '/' or something else, break the loop
                    break;
                }
            } else {
                // Not a comment start, break the loop
                break;
            }
        }
    }

    // Helper to skip a block of whitespace characters
    fn skip_single_whitespace_block(&mut self) {
        while let Some(&c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace(); // Skips whitespace AND comments now
        // Special handling for #k= directive at the beginning of a token scan
        if let Some(&'#') = self.peek_char() {
            // Clone iterator to look ahead without consuming
            let mut lookahead = self.iter.clone();
            // Check for '#k='
            if lookahead.next() == Some('#') && lookahead.next() == Some('k') && lookahead.next() == Some('=') {
                // Consume '#k='
                self.next_char();
                self.next_char();
                self.next_char();

                let mut num_str = String::new();
                while let Some(&c) = self.peek_char() {
                    if c.is_digit(10) {
                        num_str.push(self.next_char().unwrap());
                    } else {
                        break;
                    }
                }

                if num_str.is_empty() {
                    return Err("Expected digits after '#k=' directive".to_string());
                }

                return match num_str.parse::<u32>() {
                    Ok(k_value) => {
                        if k_value == 0 {
                             return Err("Value for #k directive must be positive".to_string());
                        }
                        // Lexer simply returns the token after parsing number.
                        // It does NOT consume or validate the rest of the line.
                        Ok(Token::DirectiveK(k_value))
                    }
                    Err(_) => Err("Invalid number for #k directive".to_string()),
                };
            }
            // If it starts with '#' but isn't '#k=', fall through to the normal error handling
        }

        // Original token matching logic
        match self.next_char() {
            None => Ok(Token::Eof),
            Some(c) => match c {
                '0' => Ok(Token::Zero),
                '1' => Ok(Token::One),
                '+' => Ok(Token::Plus),
                '&' => Ok(Token::And),
                '^' => Ok(Token::Xor),
                '-' => Ok(Token::Minus),
                '!' => Ok(Token::Not),
                ';' => Ok(Token::Semicolon),
                '*' => Ok(Token::Star),
                '(' => Ok(Token::LParen),
                ')' => Ok(Token::RParen),
                'T' => Ok(Token::Top),
                'U' => Ok(Token::LtlU),
                'X' => Ok(Token::LtlX),
                ':' => {
                    if self.peek_char() == Some(&'=') {
                        self.next_char();
                        Ok(Token::Assign)
                    } else {
                        Err("Expected ':' after ':' for assignment".to_string())
                    }
                }
                '=' => {
                    if self.peek_char() == Some(&'=') {
                        self.next_char();
                        Ok(Token::Eq)
                    } else {
                        Err("Expected '=' after '=' for equality test".to_string())
                    }
                }
                'x' => {
                    let mut num_str = String::new();
                    while let Some(&c) = self.peek_char() {
                        if c.is_digit(10) {
                            num_str.push(self.next_char().unwrap());
                        } else {
                            break;
                        }
                    }
                    if num_str.is_empty() {
                        Err("Expected digits after 'x' for field".to_string())
                    } else {
                        match num_str.parse::<u32>() {
                            Ok(index) => {
                                if index < self.num_fields {
                                    Ok(Token::Field(index))
                                } else {
                                    Err(format!(
                                        "Field index {} exceeds maximum k={}",
                                        index, self.num_fields
                                    ))
                                }
                            }
                            Err(_) => Err("Invalid field index number".to_string()),
                        }
                    }
                }
                'd' => {
                    if self.peek_char() == Some(&'u') {
                        self.next_char();
                        if self.peek_char() == Some(&'p') {
                            self.next_char();
                            Ok(Token::Dup)
                        } else {
                            Err("Expected `dup`".to_string())
                        }
                    } else {
                        Err("Expected `dup`".to_string())
                    }
                }
                _ => Err(format!("Unexpected character: {}", c)),
            },
        }
    }

    /// Consumes characters until the next newline or EOF.
    /// Used after parsing // comments.
    fn consume_rest_of_line(&mut self) {
         while let Some(&c) = self.peek_char() {
            if c == '\n' {
                self.next_char(); // Consume the newline
                break;
            } else {
                self.next_char(); // Consume other character
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token::Eof) => None,
            Ok(tok) => Some(Ok(tok)),
            Err(e) => Some(Err(e)),
        }
    }
}

// --- Parser ---

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    num_fields: u32,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Peekable<Lexer<'a>>, num_fields: u32) -> Self {
        Parser {
            lexer,
            num_fields,
        }
    }

    /// Parses a single complete expression.
    pub fn parse_single_expression(&mut self) -> Result<Exp, String> {
        if let Some(Ok(Token::DirectiveK(_))) = self.lexer.peek() {
            return Err("#k directive is only allowed at the beginning of the file".to_string());
        }
        let expr = self.parse_until()?; // Start with lowest precedence
        Ok(expr)
    }

    // Helper to get the next token
    fn next_token(&mut self) -> Result<Token, String> {
        self.lexer.next().unwrap_or(Ok(Token::Eof))
    }

    // Helper to peek at the next token
    fn peek_token(&mut self) -> Result<&Token, String> {
        match self.lexer.peek() {
            Some(Ok(token)) => Ok(token),
            Some(Err(e)) => Err(e.clone()),
            None => Ok(&Token::Eof),
        }
    }

    // Recursive descent parsing functions based on operator precedence:
    // Precedence (Lowest to Highest):
    // 1. U (LTL Until) - Right associative? Usually yes.
    // 2. ; (Sequence) - Left associative
    // 3. +, ^, - (Union, Xor, Difference) - Left associative
    // 4. & (Intersect) - Left associative
    // 5. !, X (Complement, LTL Next) - Prefix unary
    // 6. * (Star) - Postfix unary
    // 7. :=, == (Assign, Test) - Non-associative? Usually require primary exprs
    // 8. Primary (Literals, Parentheses, dup, field)

    // parse_until handles 'U'
    fn parse_until(&mut self) -> Result<Exp, String> {
        let left = self.parse_sequence()?;
        // LTL Until is right-associative
        if let Ok(Token::LtlU) = self.peek_token() {
            self.next_token()?; // Consume 'U'
            let right = self.parse_until()?; // Recurse for right associativity
            Ok(Expr::ltl_until(left, right))
        } else {
            Ok(left)
        }
    }

    // parse_sequence handles ';'
    fn parse_sequence(&mut self) -> Result<Exp, String> {
        let mut left = self.parse_additive()?;
        while let Ok(Token::Semicolon) = self.peek_token() {
            self.next_token()?; // Consume ';'
            let right = self.parse_additive()?;
            left = Expr::sequence(left, right);
        }
        Ok(left)
    }

    // parse_additive handles '+', '^', '-'
    fn parse_additive(&mut self) -> Result<Exp, String> {
        let mut left = self.parse_intersect()?;
        loop {
            match self.peek_token() {
                Ok(Token::Plus) => {
                    self.next_token()?;
                    let right = self.parse_intersect()?;
                    left = Expr::union(left, right);
                }
                Ok(Token::Xor) => {
                    self.next_token()?;
                    let right = self.parse_intersect()?;
                    left = Expr::xor(left, right);
                }
                Ok(Token::Minus) => {
                    self.next_token()?;
                    let right = self.parse_intersect()?;
                    left = Expr::difference(left, right);
                }
                _ => break,
            }
        }
        Ok(left)
    }

    // parse_intersect handles '&'
    fn parse_intersect(&mut self) -> Result<Exp, String> {
        let mut left = self.parse_unary()?;
        while let Ok(Token::And) = self.peek_token() {
            self.next_token()?; // Consume '&'
            let right = self.parse_unary()?;
            left = Expr::intersect(left, right);
        }
        Ok(left)
    }

    // parse_unary handles prefix '!', 'X' and postfix '*'
    fn parse_unary(&mut self) -> Result<Exp, String> {
        match self.peek_token()? {
            Token::Not => {
                self.next_token()?; // Consume '!'
                let expr = self.parse_unary()?; // Apply to the result of next level
                Ok(Expr::complement(expr))
            }
            Token::LtlX => {
                self.next_token()?; // Consume 'X'
                let expr = self.parse_unary()?; // Apply to the result of next level
                Ok(Expr::ltl_next(expr))
            }
            _ => {
                // If not a prefix operator, parse the primary expression
                let mut expr = self.parse_primary()?;
                // Check for postfix '*'
                // Allow multiple postfix stars potentially? Loop for e***
                while let Ok(Token::Star) = self.peek_token() {
                    self.next_token()?; // Consume '*'
                    expr = Expr::star(expr);
                }
                Ok(expr)
            }
        }
    }

    // parse_primary handles literals, parentheses, field ops, dup
    fn parse_primary(&mut self) -> Result<Exp, String> {
        let token = self.next_token()?;
        match token {
            Token::Zero => Ok(Expr::zero()),
            Token::One => Ok(Expr::one()),
            Token::Top => Ok(Expr::top()),
            Token::Dup => Ok(Expr::dup()),
            Token::Field(idx) => {
                // Look ahead for '==' or ':='
                match self.peek_token()? {
                    Token::Eq => {
                        self.next_token()?; // Consume '=='
                        match self.next_token()? {
                            Token::Zero => Ok(Expr::test(idx, false)),
                            Token::One => Ok(Expr::test(idx, true)),
                            other => Err(format!("Expected 0 or 1 after '==', found {:?}", other)),
                        }
                    }
                    Token::Assign => {
                        self.next_token()?; // Consume ':='
                        match self.next_token()? {
                            Token::Zero => Ok(Expr::assign(idx, false)),
                            Token::One => Ok(Expr::assign(idx, true)),
                            other => Err(format!("Expected 0 or 1 after ':=', found {:?}", other)),
                        }
                    }
                    _ => Err(format!("Expected '==' or ':=' after field x{}", idx)),
                }
            }
            Token::LParen => {
                let expr = self.parse_until()?; // Parse expression within parentheses, start from lowest precedence (U)
                match self.next_token()? {
                    Token::RParen => Ok(expr),
                    other => Err(format!(
                        "Expected closing parenthesis ')', found {:?}",
                        other
                    )),
                }
            }
            Token::Eof => Err("Unexpected end of input in primary expression".to_string()),
            other => Err(format!(
                "Unexpected token in primary expression: {:?}",
                other
            )),
        }
    }
}

// --- Main Parsing Functions ---

/// Parses a single NetKAT expression string into an Exp AST.
/// `num_fields` specifies the maximum allowed fields (e.g., if num_fields=2, fields x0, x1 are allowed).
pub fn parse_expr(input: &str, num_fields: u32) -> Result<Exp, String> {
    let mut lexer = Lexer::new(input, num_fields).peekable();
    let mut parser = Parser::new(lexer, num_fields);
    let expr = parser.parse_single_expression()?;
    match parser.peek_token() {
        Ok(Token::Eof) => Ok(expr),
        Ok(tok) => Err(format!("Unexpected token after single expression: {:?}. Use parse_expressions for multiple expressions.", tok)),
        Err(e) => Err(e),
    }
}

/// Parses a string containing multiple NetKAT expressions (separated by whitespace/newlines/comments)
/// into a Vec<Exp>.
/// `num_fields_arg` specifies the default number of allowed fields (from command line or default).
/// This can be overridden by a `#k=N` directive at the beginning of the input string.
pub fn parse_expressions(input: &str, num_fields_arg: u32) -> Result<Vec<Exp>, String> {
    let mut effective_num_fields = num_fields_arg;

    let mut k_from_directive = None;
    {
        let mut initial_lexer = Lexer::new(input, num_fields_arg).peekable();
        match initial_lexer.peek() {
            Some(Ok(Token::DirectiveK(k_value))) => {
                k_from_directive = Some(*k_value);
            }
            Some(Err(e)) => return Err(e.clone()),
            _ => {}
        }
    }

    if let Some(k) = k_from_directive {
        effective_num_fields = k;
    }

    let mut final_lexer = Lexer::new(input, effective_num_fields).peekable();

    if k_from_directive.is_some() {
        match final_lexer.next() {
            Some(Ok(Token::DirectiveK(_))) => {
                match final_lexer.peek() {
                    Some(Ok(Token::Eof)) | None => { /* Ok - EOF or empty input after directive */ },
                    Some(Ok(tok)) => {
                        return Err(format!("Expected newline after #k=N directive, found token {:?} on same line", tok));
                    }
                    Some(Err(e)) => return Err(e.clone()), // Error during the peek after directive
                }
            }
            Some(Err(e)) => return Err(e), // Error consuming the directive itself (should not happen)
            _ => return Err("Internal error: Directive peek/consume mismatch".to_string()), // Should not happen
        }
    }

    let mut parser = Parser::new(final_lexer, effective_num_fields);
    let mut expressions = Vec::new();

    loop {
        match parser.peek_token() {
            Ok(Token::Eof) => break,
            Ok(Token::DirectiveK(_)) => {
                return Err("#k directive is only allowed at the beginning of the file".to_string());
            }
            Ok(_) => {
                let expr = parser.parse_single_expression()?;
                expressions.push(expr);
            }
            Err(e) => return Err(e),
        }
    }

    Ok(expressions)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_with_k(s: &str, k: u32) -> Result<Vec<Exp>, String> {
        parse_expressions(s, k)
    }

    #[test]
    fn test_k_directive() {
        assert_eq!(
            parse_with_k("#k=2\nx0==1", 3),
            Ok(vec![Expr::test(0, true)])
        );
        assert!(parse_with_k("#k=2\nx1==0", 1).is_ok());
        assert!(parse_with_k("#k=2\nx2==0", 3).is_err());
        assert_eq!(
            parse_with_k("#k=1\nx0 == 0", 5),
             Ok(vec![Expr::test(0, false)])
        );
        assert!(parse_with_k("#k=1\nx1 == 0", 5).is_err());
        assert_eq!(
            parse_with_k("x2==1", 3),
            Ok(vec![Expr::test(2, true)])
        );
        assert!(parse_with_k("x3==1", 3).is_err());
        assert_eq!(
             parse_with_k("  // comment \n #k=1 \n x0==0", 2),
             Ok(vec![Expr::test(0, false)])
        );
        assert_eq!(
             parse_with_k("#k=1 // Set k=1\nx0==0", 2),
             Ok(vec![Expr::test(0, false)])
        );
         assert_eq!(
             parse_with_k("#k=1  \nx0==0", 2),
             Ok(vec![Expr::test(0, false)])
         );
         // Test EOF after directive
         assert_eq!(
             parse_with_k("#k=5", 2), // k=5 from directive, no expressions follow
             Ok(vec![])
         );
         assert_eq!(
             parse_with_k("#k=5 ", 2), // k=5 from directive, trailing space before EOF
             Ok(vec![])
         );
    }

    #[test]
    fn test_k_directive_errors() {
        assert!(parse_with_k("#k=abc\nx0==0", 3).is_err());
        assert!(parse_with_k("#k=\nx0==0", 3).is_err());
        assert!(parse_with_k("#k=0\nx0==0", 3).is_err());
        assert!(parse_with_k("x0==0\n#k=2", 3).is_err());
        assert!(parse_with_k("#k 2\nx0==0", 3).is_err());
        // Errors due to stricter newline/comment requirement
        assert!(parse_with_k("#k=1x0==0", 2).is_err());
        assert!(parse_with_k("#k=2 x0==0", 3).is_err());
        assert!(parse_with_k("#k=2/", 3).is_err()); // Single slash error
        assert!(parse_with_k("#k=2 /", 3).is_err()); // Space then single slash
        // Multiple directives
        assert!(parse_with_k("#k=2\n#k=3\nx0==0", 4).is_err());
    }

    fn parse(s: &str) -> Result<Exp, String> {
        let result = parse_expressions(s, 3)?;
        if result.len() == 1 {
            Ok(result.into_iter().next().unwrap())
        } else if result.is_empty() {
             Err("No expression found for single parse".to_string())
        }
        else {
            Err(format!("Expected single expression, found {}", result.len()))
        }
    }

    fn parse_multi(s: &str) -> Result<Vec<Exp>, String> {
        parse_expressions(s, 3)
    }

    #[test]
    fn test_simple_literals() {
        assert_eq!(parse("0"), Ok(Expr::zero()));
        assert_eq!(parse("1"), Ok(Expr::one()));
        assert_eq!(parse("T"), Ok(Expr::top()));
        assert_eq!(parse("dup"), Ok(Expr::dup()));
    }

    #[test]
    fn test_comments() {
        assert_eq!(parse("// comment\n 0 // another comment"), Ok(Expr::zero()));
        assert_eq!(parse("1 // just 1"), Ok(Expr::one()));
        assert_eq!(parse("// line 1\n // line 2\n T"), Ok(Expr::top()));
        assert_eq!(parse("x0 == 1 // test x0\n // next line"), Ok(Expr::test(0, true)));
    }

    #[test]
    fn test_multiple_expressions() {
        assert_eq!(
            parse_multi("0\n1"),
            Ok(vec![Expr::zero(), Expr::one()])
        );
        assert_eq!(
            parse_multi("x0==1 // first\n x1:=0 // second"),
            Ok(vec![Expr::test(0, true), Expr::assign(1, false)])
        );
        assert_eq!(
            parse_multi(" T ; 1 \n\n // blank line \n (x0==0)* // star"),
            Ok(vec![
                Expr::sequence(Expr::top(), Expr::one()),
                Expr::star(Expr::test(0, false))
            ])
        );
        assert_eq!(parse_multi(""), Ok(vec![]));
        assert_eq!(parse_multi(" // only comment "), Ok(vec![]));
    }

    #[test]
    fn test_field_ops() {
        assert_eq!(parse("x1 := 0"), Ok(Expr::assign(1, false)));
        assert_eq!(parse("x0 == 1"), Ok(Expr::test(0, true)));
        assert_eq!(parse("x2 := 1"), Ok(Expr::assign(2, true)));
        assert!(parse("x3 == 0").is_err(), "Field index > k");
        assert!(parse("x1 = 0").is_err(), "Single equals");
        assert!(parse("x1 := 2").is_err(), "Invalid value");
        assert!(parse("x1 == T").is_err(), "Invalid value T");
    }

    #[test]
    fn test_simple_binary_ops() {
        assert_eq!(
            parse("x0==0 + x1==1"),
            Ok(Expr::union(Expr::test(0, false), Expr::test(1, true)))
        );
        assert_eq!(
            parse("x0==0 ; T"),
            Ok(Expr::sequence(Expr::test(0, false), Expr::top()))
        );
        assert_eq!(
            parse("x0==0 & x1==1"),
            Ok(Expr::intersect(Expr::test(0, false), Expr::test(1, true)))
        );
        assert_eq!(
            parse("x0==0 ^ x1==1"),
            Ok(Expr::xor(Expr::test(0, false), Expr::test(1, true)))
        );
        assert_eq!(
            parse("x0==0 - x1==1"),
            Ok(Expr::difference(Expr::test(0, false), Expr::test(1, true)))
        );
    }

    #[test]
    fn test_unary_ops() {
        assert_eq!(parse("!x0==0"), Ok(Expr::complement(Expr::test(0, false))));
        assert_eq!(parse("X T"), Ok(Expr::ltl_next(Expr::top())));
        assert_eq!(parse("(x0==0)*"), Ok(Expr::star(Expr::test(0, false))));
        assert_eq!(
            parse("!(x0==0)*"),
            Ok(Expr::complement(Expr::star(Expr::test(0, false))))
        );
        assert_eq!(
            parse("(!x0==0)*"),
            Ok(Expr::star(Expr::complement(Expr::test(0, false))))
        );
        assert_eq!(
            parse("(x0==0)**"),
            Ok(Expr::star(Expr::star(Expr::test(0, false))))
        );
    }

    #[test]
    fn test_precedence_and_paren() {
        assert_eq!(
            parse("x0==0 ; x1==1 + x2==0"),
            Ok(Expr::sequence(
                Expr::test(0, false),
                Expr::union(Expr::test(1, true), Expr::test(2, false))
            ))
        );
        assert_eq!(
            parse("(x0==0 ; x1==1) + x2==0"),
            Ok(Expr::union(
                Expr::sequence(Expr::test(0, false), Expr::test(1, true)),
                Expr::test(2, false)
            ))
        );

        assert_eq!(
            parse("x0==0 & x1==1 + x2==0"),
            Ok(Expr::union(
                Expr::intersect(Expr::test(0, false), Expr::test(1, true)),
                Expr::test(2, false)
            ))
        );
        assert_eq!(
            parse("x0==0 & (x1==1 + x2==0)"),
            Ok(Expr::intersect(
                Expr::test(0, false),
                Expr::union(Expr::test(1, true), Expr::test(2, false))
            ))
        );
    }

    #[test]
    fn test_ltl_ops() {
        assert_eq!(parse("X x0==0"), Ok(Expr::ltl_next(Expr::test(0, false))));
        assert_eq!(
            parse("x0==0 U x1==1"),
            Ok(Expr::ltl_until(Expr::test(0, false), Expr::test(1, true)))
        );
        assert_eq!(
            parse("x0==0 U x1==1 U x2==0"),
            Ok(Expr::ltl_until(
                Expr::test(0, false),
                Expr::ltl_until(Expr::test(1, true), Expr::test(2, false))
            ))
        );
        assert_eq!(
            parse("x0==0 ; x1==1 U x2==0"),
            Ok(Expr::ltl_until(
                Expr::sequence(Expr::test(0, false), Expr::test(1, true)),
                Expr::test(2, false)
            ))
        );
        assert_eq!(
            parse("(x0==0 ; x1==1) U x2==0"),
            Ok(Expr::ltl_until(
                Expr::sequence(Expr::test(0, false), Expr::test(1, true)),
                Expr::test(2, false)
            ))
        );
    }

    #[test]
    fn test_complex() {
        assert_eq!(
            parse("!(x0==0 + x1:=1)* ; dup U T"),
            Ok(Expr::ltl_until(
                Expr::sequence(
                    Expr::complement(Expr::star(Expr::union(
                        Expr::test(0, false),
                        Expr::assign(1, true)
                    ))),
                    Expr::dup()
                ),
                Expr::top()
            ))
        );
    }

    #[test]
    fn test_errors() {
        assert!(parse("x0 ==").is_err());
        assert!(parse("x1 :=").is_err());
        assert!(parse("(x0==0").is_err());
        assert!(parse("x0==0)").is_err());
        assert!(parse("x0==0 +").is_err());
        assert!(parse("+ x0==0").is_err());
        assert!(parse("x0=1").is_err());
        assert!(parse("field == 1").is_err());
        assert!(parse("0 1").is_err(), "Requires operator between 0 and 1");
        assert!(parse("x0==1 x1==0").is_err(), "Requires operator");
        assert!(parse("!").is_err(), "Requires expression after !");
        assert!(parse(";").is_err(), "Requires expressions around ;");
    }
}

