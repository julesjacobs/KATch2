use crate::expr::{Exp, Expr, Field, Value};
use std::iter::Peekable;
use std::str::Chars;

// --- Lexer ---

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Zero,         // 0
    One,          // 1
    Top,          // T
    Assign,       // :=
    Eq,           // ==
    Plus,         // +
    And,          // &
    Xor,          // ^
    Minus,        // -
    Not,          // !
    Semicolon,    // ;
    Star,         // *
    Dup,          // dup
    LtlX,         // X
    LtlU,         // U
    LParen,       // (
    RParen,       // )
    Field(usize), // x followed by digits
    Eof,          // End of input
}

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    num_fields: usize, // Number of fields, max field index is k-1
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str, num_fields: usize) -> Self {
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
        while let Some(&c) = self.peek_char() {
            if c.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn next_token(&mut self) -> Result<Token, String> {
        self.skip_whitespace();
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
                        match num_str.parse::<usize>() {
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
    num_fields: usize,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, num_fields: usize) -> Self {
        Parser {
            lexer: Lexer::new(input, num_fields).peekable(),
            num_fields,
        }
    }

    pub fn parse(&mut self) -> Result<Exp, String> {
        // Entry point for parsing - typically starts with the lowest precedence operator
        self.parse_until() // Start with LTL Until as lowest precedence
    }

    // Helper to get the next token
    fn next_token(&mut self) -> Result<Token, String> {
        self.lexer.next().unwrap_or(Ok(Token::Eof)) // Handle end of iterator
    }

    // Helper to peek at the next token
    fn peek_token(&mut self) -> Result<&Token, String> {
        match self.lexer.peek() {
            Some(Ok(token)) => Ok(token),
            Some(Err(e)) => Err(e.clone()),
            None => Ok(&Token::Eof), // Treat end as EOF
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
                            Token::Zero => Ok(Expr::test(Field(idx), Value(0))),
                            Token::One => Ok(Expr::test(Field(idx), Value(1))),
                            other => Err(format!("Expected 0 or 1 after '==', found {:?}", other)),
                        }
                    }
                    Token::Assign => {
                        self.next_token()?; // Consume ':='
                        match self.next_token()? {
                            Token::Zero => Ok(Expr::assign(Field(idx), Value(0))),
                            Token::One => Ok(Expr::assign(Field(idx), Value(1))),
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

// --- Main Parsing Function ---

/// Parses a NetKAT expression string into an Exp AST.
/// `num_fields` specifies the maximum allowed fields (e.g., if num_fields=2, fields x0, x1 are allowed).
pub fn parse_expr(input: &str, num_fields: usize) -> Result<Exp, String> {
    let mut parser = Parser::new(input, num_fields);
    let expr = parser.parse()?;
    // Check if there are any remaining tokens - should be EOF
    match parser.peek_token() {
        Ok(Token::Eof) => Ok(expr),
        Ok(tok) => Err(format!("Unexpected token after expression: {:?}", tok)),
        Err(e) => Err(e), // Error from lexer peek
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(s: &str) -> Result<Exp, String> {
        parse_expr(s, 3) // Assuming k=3 for tests
    }

    #[test]
    fn test_simple_literals() {
        assert_eq!(parse("0"), Ok(Expr::zero()));
        assert_eq!(parse("1"), Ok(Expr::one()));
        assert_eq!(parse("T"), Ok(Expr::top()));
        assert_eq!(parse("dup"), Ok(Expr::dup()));
    }

    #[test]
    fn test_field_ops() {
        assert_eq!(parse("x1 := 0"), Ok(Expr::assign(Field(1), Value(0))));
        assert_eq!(parse("x0 == 1"), Ok(Expr::test(Field(0), Value(1))));
        assert_eq!(parse("x2 := 1"), Ok(Expr::assign(Field(2), Value(1))));
        assert!(parse("x3 == 0").is_err(), "Field index > k");
        assert!(parse("x1 = 0").is_err(), "Single equals");
        assert!(parse("x1 := 2").is_err(), "Invalid value");
        assert!(parse("x1 == T").is_err(), "Invalid value T");
    }

    #[test]
    fn test_simple_binary_ops() {
        assert_eq!(
            parse("x0==0 + x1==1"),
            Ok(Expr::union(
                Expr::test(Field(0), Value(0)),
                Expr::test(Field(1), Value(1))
            ))
        );
        assert_eq!(
            parse("x0==0 ; T"),
            Ok(Expr::sequence(Expr::test(Field(0), Value(0)), Expr::top()))
        );
        assert_eq!(
            parse("x0==0 & x1==1"),
            Ok(Expr::intersect(
                Expr::test(Field(0), Value(0)),
                Expr::test(Field(1), Value(1))
            ))
        );
        assert_eq!(
            parse("x0==0 ^ x1==1"),
            Ok(Expr::xor(
                Expr::test(Field(0), Value(0)),
                Expr::test(Field(1), Value(1))
            ))
        );
        assert_eq!(
            parse("x0==0 - x1==1"),
            Ok(Expr::difference(
                Expr::test(Field(0), Value(0)),
                Expr::test(Field(1), Value(1))
            ))
        );
    }

    #[test]
    fn test_unary_ops() {
        assert_eq!(
            parse("!x0==0"),
            Ok(Expr::complement(Expr::test(Field(0), Value(0))))
        );
        assert_eq!(parse("X T"), Ok(Expr::ltl_next(Expr::top())));
        assert_eq!(
            parse("(x0==0)*"),
            Ok(Expr::star(Expr::test(Field(0), Value(0))))
        );
        assert_eq!(
            parse("!(x0==0)*"), // ! has higher precedence than postfix *
            Ok(Expr::complement(Expr::star(Expr::test(Field(0), Value(0)))))
        );
        assert_eq!(
            parse("(!x0==0)*"),
            Ok(Expr::star(Expr::complement(Expr::test(Field(0), Value(0)))))
        );
        assert_eq!(
            parse("(x0==0)**"), // Multiple stars
            Ok(Expr::star(Expr::star(Expr::test(Field(0), Value(0)))))
        );
    }

    #[test]
    fn test_precedence_and_paren() {
        // Test ; vs +
        assert_eq!(
            parse("x0==0 ; x1==1 + x2==0"),
            // Precedence: +,^,- > & > ; > U
            // ; is lower than +
            Ok(Expr::sequence(
                Expr::test(Field(0), Value(0)),
                Expr::union(
                    Expr::test(Field(1), Value(1)),
                    Expr::test(Field(2), Value(0))
                )
            ))
        );
        assert_eq!(
            parse("(x0==0 ; x1==1) + x2==0"),
            Ok(Expr::union(
                Expr::sequence(
                    Expr::test(Field(0), Value(0)),
                    Expr::test(Field(1), Value(1))
                ),
                Expr::test(Field(2), Value(0))
            ))
        );

        // Test & vs +
        assert_eq!(
            parse("x0==0 & x1==1 + x2==0"),
            // + higher than &
            Ok(Expr::union(
                Expr::intersect(
                    Expr::test(Field(0), Value(0)),
                    Expr::test(Field(1), Value(1))
                ),
                Expr::test(Field(2), Value(0))
            ))
        );
        assert_eq!(
            parse("x0==0 & (x1==1 + x2==0)"),
            Ok(Expr::intersect(
                Expr::test(Field(0), Value(0)),
                Expr::union(
                    Expr::test(Field(1), Value(1)),
                    Expr::test(Field(2), Value(0))
                )
            ))
        );
    }

    #[test]
    fn test_ltl_ops() {
        assert_eq!(
            parse("X x0==0"),
            Ok(Expr::ltl_next(Expr::test(Field(0), Value(0))))
        );
        assert_eq!(
            parse("x0==0 U x1==1"),
            Ok(Expr::ltl_until(
                Expr::test(Field(0), Value(0)),
                Expr::test(Field(1), Value(1))
            ))
        );
        assert_eq!(
            parse("x0==0 U x1==1 U x2==0"), // Right associative: x0 U (x1 U x2)
            Ok(Expr::ltl_until(
                Expr::test(Field(0), Value(0)),
                Expr::ltl_until(
                    Expr::test(Field(1), Value(1)),
                    Expr::test(Field(2), Value(0))
                )
            ))
        );
        assert_eq!(
            parse("x0==0 ; x1==1 U x2==0"), // ; lower than U
            Ok(Expr::ltl_until(
                Expr::sequence(
                    Expr::test(Field(0), Value(0)),
                    Expr::test(Field(1), Value(1))
                ),
                Expr::test(Field(2), Value(0))
            ))
        );
        assert_eq!(
            parse("(x0==0 ; x1==1) U x2==0"),
            Ok(Expr::ltl_until(
                Expr::sequence(
                    Expr::test(Field(0), Value(0)),
                    Expr::test(Field(1), Value(1))
                ),
                Expr::test(Field(2), Value(0))
            ))
        );
    }

    #[test]
    fn test_complex() {
        assert_eq!(
            parse("!(x0==0 + x1:=1)* ; dup U T"),
            // Prec: !,* > +,^,- > & > ; > U
            // ! applies to (x0==0 + x1:=1)* -> Complement(Star(Union(...)))
            Ok(Expr::ltl_until(
                Expr::sequence(
                    Expr::complement(Expr::star(Expr::union(
                        Expr::test(Field(0), Value(0)),
                        Expr::assign(Field(1), Value(1))
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
        assert!(parse("+ x0==0").is_err()); // Unary plus not supported
        assert!(parse("x0=1").is_err());
        assert!(parse("field == 1").is_err());
        assert!(parse("0 1").is_err(), "Requires operator between 0 and 1");
        assert!(parse("x0==1 x1==0").is_err(), "Requires operator");
        assert!(parse("!").is_err(), "Requires expression after !");
        assert!(parse(";").is_err(), "Requires expressions around ;");
    }
}
