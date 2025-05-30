use crate::expr::{Exp, Expr, Pattern};
use std::iter::Peekable;
use std::str::Chars;
use serde::{Serialize, Deserialize};
use std::fmt;


/// Represents the span of an error in the source code.
/// Line and column numbers are 1-indexed for Monaco editor.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ErrorSpan {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

/// Contains details about a parsing error.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ParseErrorDetails {
    pub message: String,
    pub span: Option<ErrorSpan>, // Span is optional for errors not tied to a specific location
}

impl fmt::Display for ParseErrorDetails {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
        // Optionally, add span information if self.span.is_some()
        // For example:
        // if let Some(span) = &self.span {
        //     write!(f, "{} (at line {}, column {})", self.message, span.start_line, span.start_column)?;
        // } else {
        //     write!(f, "{}", self.message)?;
        // }
        // Ok(())
    }
}

// --- Position and Span ---

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Position {
    /// 0-indexed byte offset in the input string
    pub offset: usize,
    /// 1-indexed line number
    pub line: usize,
    /// 1-indexed column number (UTF-8 characters)
    pub column: usize,
}

impl Position {
    fn new(offset: usize, line: usize, column: usize) -> Self {
        Position { offset, line, column }
    }

    fn advance(&mut self, char_value: char) {
        self.offset += char_value.len_utf8();
        if char_value == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    fn new(start: Position, end: Position) -> Self {
        Span { start, end }
    }
}

// --- Custom Error for the parser ---
#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
}

impl ParseError {
    fn new(message: String, span: Span) -> Self {
        ParseError { message, span }
    }
}

// --- Token Definition ---

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Zero,       // 0
    One,        // 1
    Top,        // T
    Assign,     // :=
    Eq,         // ==
    Plus,       // +
    And,        // &
    Xor,        // ^
    Minus,      // -
    Not,        // ~
    TestNot,    // !
    Semicolon,  // ;
    Star,       // *
    Dup,        // dup
    LtlX,       // X
    LtlU,       // U
    LtlF,       // F
    LtlG,       // G
    LtlR,       // R
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    DotDot,     // ..
    Field(u32), // x followed by digits
    Number(String), // Decimal numeric literal
    BinaryLiteral(String), // Binary literal (0b1010)
    HexLiteral(String), // Hexadecimal literal (0xFF)
    IpLiteral(String), // IP address literal (192.168.1.1)
    Ident(String), // Variable identifier
    If,         // if keyword
    Then,       // then keyword
    Else,       // else keyword
    Let,        // let keyword
    In,         // in keyword
    End,        // end keyword (for multiple expressions parsing)
    Eof,        // End of input
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, span: Span) -> Self {
        Token { kind, span }
    }
}

// --- Lexer ---

pub struct Lexer<'a> {
    iter: Peekable<Chars<'a>>,
    current_pos: Position,
    // input_str: &'a str, // Keep a reference to the input for slicing if needed for error context
    iterator_has_yielded_eof: bool, // New field to track EOF iteration state
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            iter: input.chars().peekable(),
            current_pos: Position::new(0, 1, 1),
            // input_str: input,
            iterator_has_yielded_eof: false, // Initialize the new field
        }
    }

    fn next_char_with_pos(&mut self) -> Option<(char, Position)> {
        let start_pos = self.current_pos;
        match self.iter.next() {
            Some(c) => {
                self.current_pos.advance(c);
                Some((c, start_pos))
            }
            None => None,
        }
    }
    
    // Only peeks at char, doesn't advance position or return it
    fn peek_char(&mut self) -> Option<&char> {
        self.iter.peek()
    }
    
    fn is_valid_ip_format(&self, s: &str) -> bool {
        // Handle CIDR notation (e.g., 192.168.1.0/24)
        let base_ip = if let Some(slash_pos) = s.find('/') {
            let (ip_part, suffix) = s.split_at(slash_pos);
            // Check if suffix is a valid CIDR prefix
            let prefix_part = &suffix[1..];
            if prefix_part.is_empty() || !prefix_part.chars().all(|c| c.is_digit(10)) {
                return false;
            }
            if let Ok(prefix) = prefix_part.parse::<u32>() {
                if prefix > 32 {
                    return false;
                }
            } else {
                return false;
            }
            ip_part
        } else if s.contains('-') {
            // Handle IP range (e.g., 192.168.1.10-192.168.1.20)
            let parts: Vec<&str> = s.split('-').collect();
            if parts.len() != 2 {
                return false;
            }
            // Validate both IPs in the range
            for ip in parts {
                if !self.is_valid_single_ip(ip) {
                    return false;
                }
            }
            return true;
        } else {
            s
        };
        
        self.is_valid_single_ip(base_ip)
    }
    
    fn is_valid_single_ip(&self, s: &str) -> bool {
        let parts: Vec<&str> = s.split('.').collect();
        if parts.len() != 4 {
            return false;
        }
        
        for part in parts {
            if part.is_empty() {
                return false;
            }
            // Check if all characters are digits
            if !part.chars().all(|c| c.is_digit(10)) {
                return false;
            }
            // Check if the number is in valid range (0-255)
            if let Ok(num) = part.parse::<u32>() {
                if num > 255 {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    fn skip_whitespace(&mut self) -> Result<(), ParseError> {
        loop {
            let start_skip_pos = self.current_pos;
            match self.peek_char() {
                Some(&c) => {
                    if c == '/' {
                        // Potential comment
                        let mut temp_iter = self.iter.clone();
                        let mut temp_pos = self.current_pos;
                        
                        temp_iter.next(); // Consume first '/'
                        temp_pos.advance('/');

                        if temp_iter.peek() == Some(&'/') {
                            // It's a line comment
                            self.next_char_with_pos(); // Consume first '/'
                            self.next_char_with_pos(); // Consume second '/'
                            while let Some(&next_c) = self.peek_char() {
                                if next_c == '\n' {
                                    // self.next_char_with_pos(); // Consume newline to advance position correctly
                                    break; // Stop before consuming newline, let outer loop handle it or EOF
                                }
                                self.next_char_with_pos(); // Consume comment char
                            }
                            continue; // Restart loop to check next char (could be newline or more whitespace)
                        } else {
                            // Single '/' is an error in NetKAT, but we treat it as unexpected here
                            // Or, let the main tokenizer handle it if '/' becomes an operator
                             return Err(ParseError::new(
                                "Unexpected character: /".to_string(),
                                Span::new(start_skip_pos, self.current_pos), // Span of just the '/'
                            ));
                        }
                    }
                    if !c.is_whitespace() {
                        break; // Not whitespace, not a comment
                    }
                    self.next_char_with_pos(); // Consume whitespace char
                }
                None => break, // EOF
            }
        }
        Ok(())
    }


    // This is the main tokenizing function that will need careful span management
    pub fn next_token(&mut self) -> Result<Token, ParseError> {
        self.skip_whitespace()?;
        
        let start_pos = self.current_pos;

        match self.next_char_with_pos() {
            None => Ok(Token::new(TokenKind::Eof, Span::new(start_pos, self.current_pos))),
            Some((c, _char_start_pos)) => { // _char_start_pos is same as start_pos due to skip_whitespace
                let kind = match c {
                    '0' => {
                        // Check for special prefixes: 0b (binary) or 0x (hex)
                        match self.peek_char() {
                            Some(&'b') => {
                                // Binary literal
                                self.next_char_with_pos(); // Consume 'b'
                                let mut binary_str = String::new();
                                while let Some(&next_c) = self.peek_char() {
                                    if next_c == '0' || next_c == '1' {
                                        binary_str.push(self.next_char_with_pos().unwrap().0);
                                    } else {
                                        break;
                                    }
                                }
                                if binary_str.is_empty() {
                                    return Err(ParseError::new(
                                        "Expected binary digits after '0b'".to_string(),
                                        Span::new(start_pos, self.current_pos),
                                    ));
                                }
                                TokenKind::BinaryLiteral(binary_str)
                            }
                            Some(&'x') | Some(&'X') => {
                                // Hexadecimal literal
                                self.next_char_with_pos(); // Consume 'x' or 'X'
                                let mut hex_str = String::new();
                                while let Some(&next_c) = self.peek_char() {
                                    if next_c.is_ascii_hexdigit() {
                                        hex_str.push(self.next_char_with_pos().unwrap().0);
                                    } else {
                                        break;
                                    }
                                }
                                if hex_str.is_empty() {
                                    return Err(ParseError::new(
                                        "Expected hexadecimal digits after '0x'".to_string(),
                                        Span::new(start_pos, self.current_pos),
                                    ));
                                }
                                TokenKind::HexLiteral(hex_str)
                            }
                            Some(ch) if ch.is_digit(10) => {
                                // This is part of a multi-digit number, parse it as a decimal number
                                let mut num_str = String::new();
                                num_str.push(c);
                                while let Some(&next_c) = self.peek_char() {
                                    if next_c.is_digit(10) {
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                    } else if next_c == '.' {
                                        // Look ahead to see if this is ".." (range operator) or part of IP
                                        let mut temp_iter = self.iter.clone();
                                        temp_iter.next(); // Skip the first '.'
                                        if temp_iter.peek() == Some(&'.') {
                                            // This is "..", don't consume it
                                            break;
                                        } else {
                                            // This might be part of an IP address
                                            num_str.push(self.next_char_with_pos().unwrap().0);
                                        }
                                    } else if next_c == '/' && num_str.contains('.') {
                                        // CIDR notation
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                        // Continue reading the prefix length
                                        while let Some(&c) = self.peek_char() {
                                            if c.is_digit(10) {
                                                num_str.push(self.next_char_with_pos().unwrap().0);
                                            } else {
                                                break;
                                            }
                                        }
                                        break;
                                    } else if next_c == '-' && num_str.contains('.') {
                                        // IP range
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                        // Continue reading the second IP
                                        while let Some(&c) = self.peek_char() {
                                            if c.is_digit(10) || c == '.' {
                                                num_str.push(self.next_char_with_pos().unwrap().0);
                                            } else {
                                                break;
                                            }
                                        }
                                        break;
                                    } else {
                                        break;
                                    }
                                }
                                // Check if this looks like an IP address (contains dots)
                                if num_str.contains('.') && self.is_valid_ip_format(&num_str) {
                                    TokenKind::IpLiteral(num_str)
                                } else {
                                    TokenKind::Number(num_str.replace(".", ""))
                                }
                            }
                            Some('.') => {
                                // Could be an IP address starting with 0
                                let mut num_str = String::new();
                                num_str.push(c);
                                while let Some(&next_c) = self.peek_char() {
                                    if next_c.is_digit(10) {
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                    } else if next_c == '.' {
                                        // Look ahead to see if this is ".." (range operator) or part of IP
                                        let mut temp_iter = self.iter.clone();
                                        temp_iter.next(); // Skip the first '.'
                                        if temp_iter.peek() == Some(&'.') {
                                            // This is "..", don't consume it
                                            break;
                                        } else {
                                            // This might be part of an IP address
                                            num_str.push(self.next_char_with_pos().unwrap().0);
                                        }
                                    } else if next_c == '/' && num_str.contains('.') {
                                        // CIDR notation
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                        // Continue reading the prefix length
                                        while let Some(&c) = self.peek_char() {
                                            if c.is_digit(10) {
                                                num_str.push(self.next_char_with_pos().unwrap().0);
                                            } else {
                                                break;
                                            }
                                        }
                                        break;
                                    } else if next_c == '-' && num_str.contains('.') {
                                        // IP range
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                        // Continue reading the second IP
                                        while let Some(&c) = self.peek_char() {
                                            if c.is_digit(10) || c == '.' {
                                                num_str.push(self.next_char_with_pos().unwrap().0);
                                            } else {
                                                break;
                                            }
                                        }
                                        break;
                                    } else {
                                        break;
                                    }
                                }
                                // Check if this looks like an IP address (contains dots)
                                if num_str.contains('.') && self.is_valid_ip_format(&num_str) {
                                    TokenKind::IpLiteral(num_str)
                                } else {
                                    TokenKind::Number(num_str.replace(".", ""))
                                }
                            }
                            _ => {
                                // Single digit 0
                                TokenKind::Zero
                            }
                        }
                    }
                    '1' => {
                        // Check if this is part of a larger number
                        if self.peek_char().map_or(false, |ch| ch.is_digit(10) || *ch == '.') {
                            // This is part of a multi-digit number, parse it as a number
                            let mut num_str = String::new();
                            num_str.push(c);
                            while let Some(&next_c) = self.peek_char() {
                                if next_c.is_digit(10) {
                                    num_str.push(self.next_char_with_pos().unwrap().0);
                                } else if next_c == '.' {
                                    // Look ahead to see if this is ".." (range operator) or part of IP
                                    let mut temp_iter = self.iter.clone();
                                    temp_iter.next(); // Skip the first '.'
                                    if temp_iter.peek() == Some(&'.') {
                                        // This is "..", don't consume it
                                        break;
                                    } else {
                                        // This might be part of an IP address
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                    }
                                } else if next_c == '/' && num_str.contains('.') {
                                    // CIDR notation
                                    num_str.push(self.next_char_with_pos().unwrap().0);
                                    // Continue reading the prefix length
                                    while let Some(&c) = self.peek_char() {
                                        if c.is_digit(10) {
                                            num_str.push(self.next_char_with_pos().unwrap().0);
                                        } else {
                                            break;
                                        }
                                    }
                                    break;
                                } else if next_c == '-' && num_str.contains('.') {
                                    // IP range
                                    num_str.push(self.next_char_with_pos().unwrap().0);
                                    // Continue reading the second IP
                                    while let Some(&c) = self.peek_char() {
                                        if c.is_digit(10) || c == '.' {
                                            num_str.push(self.next_char_with_pos().unwrap().0);
                                        } else {
                                            break;
                                        }
                                    }
                                    break;
                                } else {
                                    break;
                                }
                            }
                            // Check if this looks like an IP address (contains dots)
                            if num_str.contains('.') && self.is_valid_ip_format(&num_str) {
                                TokenKind::IpLiteral(num_str)
                            } else {
                                TokenKind::Number(num_str.replace(".", ""))
                            }
                        } else {
                            // Single digit 1
                            TokenKind::One
                        }
                    }
                    '+' => TokenKind::Plus,
                    '&' => TokenKind::And,
                    '^' => TokenKind::Xor,
                    '-' => TokenKind::Minus,
                    '~' => TokenKind::Not,
                    '!' => TokenKind::TestNot,
                    ';' => TokenKind::Semicolon,
                    '*' => TokenKind::Star,
                    '(' => TokenKind::LParen,
                    ')' => TokenKind::RParen,
                    '[' => TokenKind::LBracket,
                    ']' => TokenKind::RBracket,
                    '.' => {
                        if self.peek_char() == Some(&'.') {
                            self.next_char_with_pos(); // Consume second '.'
                            TokenKind::DotDot
                        } else {
                            return Err(ParseError::new(
                                "Expected '..' for range operator".to_string(),
                                Span::new(start_pos, self.current_pos),
                            ));
                        }
                    }
                    ':' => {
                        if self.peek_char() == Some(&'=') {
                            self.next_char_with_pos(); // Consume '='
                            TokenKind::Assign
                        } else {
                            return Err(ParseError::new(
                                "Expected ':=' for assignment".to_string(),
                                Span::new(start_pos, self.current_pos),
                            ));
                        }
                    }
                    '=' => {
                        if self.peek_char() == Some(&'=') {
                            self.next_char_with_pos(); // Consume '='
                            TokenKind::Eq
                        } else {
                            // Single '=' is used in let bindings
                            TokenKind::Eq
                        }
                    }
                    // Let all identifiers including 'x', 'x0', 'x1', etc. go through the general identifier handling
                    _ => {
                        // Check if it's a digit that could start a number
                        if c.is_digit(10) {
                            let mut num_str = String::new();
                            num_str.push(c);
                            while let Some(&next_c) = self.peek_char() {
                                if next_c.is_digit(10) {
                                    num_str.push(self.next_char_with_pos().unwrap().0);
                                } else if next_c == '.' {
                                    // Look ahead to see if this is ".." (range operator) or part of IP
                                    let mut temp_iter = self.iter.clone();
                                    temp_iter.next(); // Skip the first '.'
                                    if temp_iter.peek() == Some(&'.') {
                                        // This is "..", don't consume it
                                        break;
                                    } else {
                                        // This might be part of an IP address
                                        num_str.push(self.next_char_with_pos().unwrap().0);
                                    }
                                } else if next_c == '/' && num_str.contains('.') {
                                    // CIDR notation
                                    num_str.push(self.next_char_with_pos().unwrap().0);
                                    // Continue reading the prefix length
                                    while let Some(&c) = self.peek_char() {
                                        if c.is_digit(10) {
                                            num_str.push(self.next_char_with_pos().unwrap().0);
                                        } else {
                                            break;
                                        }
                                    }
                                    break;
                                } else if next_c == '-' && num_str.contains('.') {
                                    // IP range
                                    num_str.push(self.next_char_with_pos().unwrap().0);
                                    // Continue reading the second IP
                                    while let Some(&c) = self.peek_char() {
                                        if c.is_digit(10) || c == '.' {
                                            num_str.push(self.next_char_with_pos().unwrap().0);
                                        } else {
                                            break;
                                        }
                                    }
                                    break;
                                } else {
                                    break;
                                }
                            }
                            // Check if this looks like an IP address (contains dots)
                            if num_str.contains('.') && self.is_valid_ip_format(&num_str) {
                                TokenKind::IpLiteral(num_str)
                            } else {
                                TokenKind::Number(num_str.replace(".", ""))
                            }
                        }
                        // Check if it's a letter that could start an identifier
                        else if c.is_alphabetic() {
                            let mut ident = String::new();
                            ident.push(c);
                            while let Some(&next_c) = self.peek_char() {
                                if next_c.is_alphanumeric() || next_c == '_' {
                                    ident.push(self.next_char_with_pos().unwrap().0);
                                } else {
                                    break;
                                }
                            }
                            
                            // Check for reserved words
                            match ident.as_str() {
                                "in" => TokenKind::In,
                                "if" => TokenKind::If,
                                "then" => TokenKind::Then,
                                "else" => TokenKind::Else,
                                "let" => TokenKind::Let,
                                "dup" => TokenKind::Dup,
                                "end" => TokenKind::End,
                                // Single letter tokens
                                "T" => TokenKind::Top,
                                "X" => TokenKind::LtlX,
                                "U" => TokenKind::LtlU,
                                "F" => TokenKind::LtlF,
                                "G" => TokenKind::LtlG,
                                "R" => TokenKind::LtlR,
                                // Handle field identifiers like x0, x1, x2, etc.
                                s if s.starts_with('x') && s.len() > 1 && s[1..].chars().all(|c| c.is_digit(10)) => {
                                    match s[1..].parse::<u32>() {
                                        Ok(index) => TokenKind::Field(index),
                                        Err(_) => TokenKind::Ident(ident)
                                    }
                                }
                                _ => TokenKind::Ident(ident)
                            }
                        } else {
                            return Err(ParseError::new(
                                format!("Unexpected character: {}", c),
                                Span::new(start_pos, self.current_pos), // Span of the single unexpected char
                            ));
                        }
                    }
                };
                Ok(Token::new(kind, Span::new(start_pos, self.current_pos)))
            }
        }
    }
}

// --- Iterator for Lexer ---
// This now yields Result<Token, ParseError> where Token includes its span.
impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.iterator_has_yielded_eof {
            return None; // Stop iteration if EOF has already been yielded
        }

        let token_result = self.next_token(); // Get the next token from the lexer's core logic
        
        match &token_result { // Peek into the result
            Ok(token) if token.kind == TokenKind::Eof => {
                self.iterator_has_yielded_eof = true; // Mark that EOF is about to be yielded
                Some(token_result) // Yield the EOF token
            }
            _ => {
                // For any other token or an error, just yield it.
                // If next_token() itself returns an error, that will be passed through.
                Some(token_result)
            }
        }
    }
}


// --- Parser ---
// This will need to be updated to use SpannedToken and ParseError

/*
OPERATOR PRECEDENCE AND ASSOCIATIVITY TABLE
===========================================

The parser implements the following precedence hierarchy (highest to lowest precedence):

1. PRIMARY EXPRESSIONS (highest precedence)
   - Literals: 0, 1, T, dup, end
   - Field operations: x1:=0, x2==1 (atomic constructs)
   - Parenthesized expressions: (expr)

2. POSTFIX OPERATORS
   - Star: *  (left-associative)
   - Example: a** = (a*)*

3. PREFIX OPERATORS  
   - Complement: ~
   - Test Negation: !
   - LTL Next: X
   - LTL Future: F  
   - LTL Globally: G
   - All prefix operators are right-associative
   - Example: ~~a = ~(~(a)), ~X a = ~(X(a)), !!a = !(!(a))

4. INTERSECTION (left-associative)
   - And: &
   - Example: a & b & c = ((a & b) & c)

5. SEQUENCE (left-associative)
   - Semicolon: ;
   - Example: a ; b ; c = ((a ; b) ; c)

6. ADDITIVE (left-associative)
   - Union: +
   - XOR: ^  
   - Difference: -
   - Example: a + b - c = ((a + b) - c)

7. TEMPORAL OPERATORS (lowest precedence, right-associative)
   - Until: U
   - Release: R
   - Example: a U b U c = (a U (b U c))

PRECEDENCE INTERACTION EXAMPLES:
- !a* + b ; c & d U e  =>  (!(a*) + (b ; (c & d))) U e
- (a + b) ; c  =>  forces additive before sequence
- a ; b + c  =>  (a ; b) + c  (sequence binds tighter than additive)
- !a*  =>  !(a*)  (postfix binds tighter than prefix)
- a + b*  =>  a + (b*)  (postfix binds tighter than infix)

Field operations like x1:=0 and x2==1 are parsed as atomic expressions 
at the primary level and have the highest precedence.
*/

pub struct Parser<'a> {
    // The lexer now yields Result<Token, ParseError>, where Token is SpannedToken
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self { // Takes Lexer directly
        Parser { 
            lexer: lexer.peekable() 
        }
    }

    /// Parses a single complete expression.
    pub fn parse_single_expression(&mut self) -> Result<Exp, ParseError> { // Returns ParseError
        let expr = self.parse_until()?; // Start with lowest precedence
        // Here, we might check for trailing tokens if a single expression is expected to consume all input.
        // For now, it parses one expression.
        Ok(expr)
    }

    // Helper to get the next token
    // Consumes the token from the lexer.
    fn next_token_internal(&mut self) -> Result<Token, ParseError> { // Returns full Token
        // The iterator's next() already wraps Lexer's next_token result.
        // If lexer.next() is None (because Eof was hit and made None), we synthesize an Eof token.
        // This ensures parser always has a token to look at, even if it's Eof.
        // However, the iterator implementation now makes Eof stop iteration (returns None).
        // So, if self.lexer.next() is None, it truly means the lexer is exhausted.
        // The lexer itself produces an Eof token before stopping.
        // This means the parser should get an Eof token as a valid item.

        // Let's reconsider: the lexer's iterator *will* stop if next_token returns an Eof Token.
        // To handle this, next_token_internal should perhaps check peek_token first.
        // If peek_token shows Eof, next_token_internal can return that Eof without advancing,
        // or advance and return it.
        // The current lexer iterator:
        // next() -> Some(Ok(Token(Eof, span))) -> then next call to next_token() from iterator.next() would be None.
        // So, the parser's next_token_internal will get Ok(Token(Eof, span))
        // And a subsequent call will get None from self.lexer.next().

        match self.lexer.next() {
            Some(Ok(token)) => Ok(token),
            Some(Err(e)) => Err(e),
            None => {
                // This case should ideally not be reached if the lexer guarantees an Eof token
                // before the iterator ends. If it can be reached, we need a dummy Eof span.
                // For now, assume Lexer's Eof is the definitive end.
                // If we get here, it's likely after an Eof has already been consumed.
                // Consider what span an Eof token generated here should have.
                // Perhaps the parser should primarily use peek_token and only consume when sure.
                // Let's rely on the lexer providing an Eof token.
                // This path implies the iterator is exhausted.
                // The parser's logic for parse_until etc. should handle TokenKind::Eof from peek_token.
                // So if next_token_internal is called when peek is Eof, it consumes Eof and returns it.
                // If called again, it would hit this None.
                // This feels like an unexpected state if the parser always checks peek_token.

                // Safest for now: if lexer iterator is exhausted, it means an EOF token was the last thing
                // it would have produced (or an error).
                // The `Parser::peek_token` should handle the `None` case from `self.lexer.peek()`
                // and map it to a reference to a synthetic EOF token.
                // `next_token_internal` should ideally not be called if `peek_token` is already EOF,
                // unless the parser logic explicitly wants to consume the EOF.

                // For now, let's assume this means the stream is truly finished.
                // The lexer's iterator gives None *after* it has given the EOF token.
                // So if the Parser calls next_token_internal *again* after receiving EOF, it gets None.
                Err(ParseError::new("Unexpected end of token stream after EOF".to_string(), Default::default())) // Default span is not ideal
            }
        }
    }
    
    // Renaming to avoid conflict and signify it's the one the parser methods should use
    fn consume_token(&mut self) -> Result<Token, ParseError> {
        self.next_token_internal()
    }


    // Helper to peek at the next token without consuming it.
    // Returns a Result containing a reference to the Token or a cloned ParseError.
    // This now needs to handle the possibility of the lexer returning an error on peek.
    fn peek_token_internal(&mut self) -> Result<&Token, ParseError> {
        match self.lexer.peek() {
            Some(Ok(token_ref)) => Ok(token_ref),
            Some(Err(parse_error)) => Err(parse_error.clone()), // Clone error to return owned
            None => {
                // Iterator is exhausted. This implies an EOF token was already produced and consumed.
                // The parser logic should usually stop before this if it's peeking for next ops.
                // We need a way to represent EOF here without allocating a new Token each time.
                // This is tricky. For now, error. Later, perhaps a static EOF token reference.
                // Or, the lexer never truly "ends" but keeps yielding EOFs.
                // Current lexer: next_token yields Eof, then iterator's next yields None.
                // So, peek() will be None after Eof is consumed.
                Err(ParseError::new("Peeked beyond EOF".to_string(), Default::default()))
            }
        }
    }
    
    // Renaming for clarity
    fn peek_kind(&mut self) -> Result<&TokenKind, ParseError> {
        self.peek_token_internal().map(|token| &token.kind)
    }
    
    fn peek_token(&mut self) -> Result<&Token, ParseError> { // Expose this one for parser methods
        self.peek_token_internal()
    }


    // Recursive descent parsing functions based on operator precedence:
    // New precedence hierarchy (highest to lowest):
    // 1. Postfix: * 
    // 2. Prefix: !, X, F, G
    // 3. Intersection: & (left-associative)
    // 4. Sequence: ; (left-associative)
    // 5. Additive: +, ^, - (left-associative)
    // 6. Until/Release: U, R (right-associative)

    fn parse_until(&mut self) -> Result<Exp, ParseError> {
        let left = self.parse_additive()?;
        // Peek at the kind directly, to keep the token for its span if needed for error
        // This loop structure is for right-associativity. A single check is enough.
        match self.peek_token() { 
            Ok(peeked_token) => {
                match peeked_token.kind {
                    TokenKind::LtlU => {
                        let op_token = self.consume_token()?; // Consume 'U'
                        // Check for EOF before parsing RHS
                        match self.peek_kind() {
                            Ok(&TokenKind::Eof) => {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span,
                                ));
                            }
                            Err(ref pe) if pe.message.contains("Peeked beyond EOF") || pe.message.contains("Unexpected end of token stream") => {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span,
                                ));
                            }
                            Ok(_) => { // Other token, parse it
                                let right = self.parse_until()?; 
                                Ok(Expr::ltl_until(left, right))
                            }
                            Err(pe) => Err(pe.clone()), // Propagate other peek errors
                        }
                    }
                    TokenKind::LtlR => {
                        let op_token = self.consume_token()?; // Consume 'R'
                        // Check for EOF before parsing RHS
                        match self.peek_kind() {
                            Ok(&TokenKind::Eof) => {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span,
                                ));
                            }
                            Err(ref pe) if pe.message.contains("Peeked beyond EOF") || pe.message.contains("Unexpected end of token stream") => {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span,
                                ));
                            }
                            Ok(_) => { // Other token, parse it
                                let right = self.parse_until()?; 
                                let not_left = Expr::complement(left);
                                let not_right = Expr::complement(right);
                                let until = Expr::ltl_until(not_left, not_right);
                                Ok(Expr::complement(until))
                            }
                            Err(pe) => Err(pe.clone()), // Propagate other peek errors
                        }
                    }
                    _ => Ok(left), // No U or R, return current left
                }
            }
            Err(pe) => Err(pe.clone()), // Error peeking for U/R operator itself
        }
    }

    fn parse_additive(&mut self) -> Result<Exp, ParseError> {
        let mut left = self.parse_sequence()?;
        loop {
            // Peek at the kind directly, to keep the token for its span if needed for error
            let peeked_token_result = self.peek_token();
            
            match peeked_token_result {
                Ok(peeked_token) => {
                    match peeked_token.kind {
                        TokenKind::Plus => {
                            let op_token = self.consume_token()?; // Consume '+'
                            // Check for EOF before parsing RHS
                            if self.peek_kind()? == &TokenKind::Eof {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span, // Span of the operator
                                ));
                            }
                            let right = self.parse_sequence()?;
                            left = Expr::union(left, right);
                        }
                        TokenKind::Xor => {
                            let op_token = self.consume_token()?; // Consume '^'
                            // Check for EOF before parsing RHS
                            if self.peek_kind()? == &TokenKind::Eof {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span,
                                ));
                            }
                            let right = self.parse_sequence()?;
                            left = Expr::xor(left, right);
                        }
                        TokenKind::Minus => {
                            let op_token = self.consume_token()?; // Consume '-'
                            // Check for EOF before parsing RHS
                            if self.peek_kind()? == &TokenKind::Eof {
                                return Err(ParseError::new(
                                    format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                                    op_token.span,
                                ));
                            }
                            let right = self.parse_sequence()?;
                            left = Expr::difference(left, right);
                        }
                        _ => break, // Not Plus, Xor, or Minus
                    }
                }
                Err(pe) => return Err(pe.clone()), // Error during peeking
            }
        }
        Ok(left)
    }

    fn parse_sequence(&mut self) -> Result<Exp, ParseError> {
        let mut left = self.parse_intersect()?;
        while self.peek_kind()? == &TokenKind::Semicolon {
            let _op_token = self.consume_token()?; // Consume ';'
            let right = self.parse_intersect()?;
            left = Expr::sequence(left, right);
        }
        Ok(left)
    }

    fn parse_intersect(&mut self) -> Result<Exp, ParseError> {
        let mut left = self.parse_prefix()?;
        while self.peek_kind()? == &TokenKind::And {
            self.consume_token()?; // Consume '&'
            let right = self.parse_prefix()?;
            left = Expr::intersect(left, right);
        }
        Ok(left)
    }

    fn parse_prefix(&mut self) -> Result<Exp, ParseError> {
        match self.peek_kind()? {
            TokenKind::Not => {
                let op_token = self.consume_token()?; // Consume '~'
                // Check for EOF before parsing operand
                if self.peek_kind()? == &TokenKind::Eof {
                    return Err(ParseError::new(
                        format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                        op_token.span,
                    ));
                }
                let operand = self.parse_prefix()?; // Right-associative
                Ok(Expr::complement(operand))
            }
            TokenKind::TestNot => {
                let op_token = self.consume_token()?; // Consume '!'
                // Check for EOF before parsing operand
                if self.peek_kind()? == &TokenKind::Eof {
                    return Err(ParseError::new(
                        format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                        op_token.span,
                    ));
                }
                let operand = self.parse_prefix()?; // Right-associative
                // We'll validate that operand is in test fragment during desugaring
                Ok(Expr::test_negation(operand))
            }
            TokenKind::LtlX => { // LTL Next
                let op_token = self.consume_token()?; // Consume 'X'
                // Check for EOF before parsing operand
                if self.peek_kind()? == &TokenKind::Eof {
                    return Err(ParseError::new(
                        format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                        op_token.span,
                    ));
                }
                let operand = self.parse_prefix()?;
                Ok(Expr::ltl_next(operand))
            }
            TokenKind::LtlF => { // LTL Future: F e ≡ T U e
                let op_token = self.consume_token()?;
                // Check for EOF before parsing operand
                if self.peek_kind()? == &TokenKind::Eof {
                    return Err(ParseError::new(
                        format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                        op_token.span,
                    ));
                }
                let operand = self.parse_prefix()?;
                Ok(Expr::ltl_until(Expr::top(), operand)) // Uses helper from expr.rs
            }
            TokenKind::LtlG => { // LTL Globally: G e ≡ ¬F¬e ≡ ¬(T U ¬e)
                let op_token = self.consume_token()?;
                // Check for EOF before parsing operand
                if self.peek_kind()? == &TokenKind::Eof {
                    return Err(ParseError::new(
                        format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                        op_token.span,
                    ));
                }
                let operand = self.parse_prefix()?;
                Ok(Expr::ltl_globally(operand)) // Uses helper from expr.rs
            }
            _ => {
                // No prefix operator, parse postfix
                self.parse_postfix()
            }
        }
    }

    fn parse_postfix(&mut self) -> Result<Exp, ParseError> {
        let mut base_expr = self.parse_atom_or_field_expression()?;
        
        // Handle postfix star (left-associative, though only one postfix operator exists)
        while self.peek_kind()? == &TokenKind::Star {
            let _star_token = self.consume_token()?; // Consume '*'
            base_expr = Expr::star(base_expr);
        }
        Ok(base_expr)
    }

    // This function replaces the old parse_primary_and_assign_eq and parts of parse_primary.
    // It parses atomic literals, dup, parenthesized expressions, and field assignments/tests.
    fn parse_atom_or_field_expression(&mut self) -> Result<Exp, ParseError> {
        let current_token = self.peek_token()?.clone(); // Clone to avoid lifetime issues if we consume

        match current_token.kind {
            TokenKind::Field(index) => {
                self.consume_token()?; // Consume field token
                
                let next_token_after_field = self.peek_token()?.clone();
                match next_token_after_field.kind {
                    TokenKind::LBracket => {
                        // Parse bit range: x[start..end]
                        self.consume_token()?; // Consume '['
                        
                        // Parse start index
                        let start_token = self.consume_token()?;
                        let start = match start_token.kind {
                            TokenKind::Number(num_str) => {
                                num_str.parse::<u32>().map_err(|_| ParseError::new(
                                    format!("Invalid start index in bit range: {}", num_str),
                                    start_token.span,
                                ))?
                            }
                            _ => return Err(ParseError::new(
                                format!("Expected number for bit range start, found {}", token_kind_to_user_string(&start_token.kind)),
                                start_token.span,
                            )),
                        };
                        
                        // Expect '..'
                        match self.consume_token()? {
                            Token { kind: TokenKind::DotDot, .. } => {},
                            tok => return Err(ParseError::new(
                                format!("Expected '..' in bit range, found {}", token_kind_to_user_string(&tok.kind)),
                                tok.span,
                            )),
                        }
                        
                        // Parse end index
                        let end_token = self.consume_token()?;
                        let end = match end_token.kind {
                            TokenKind::Number(num_str) => {
                                num_str.parse::<u32>().map_err(|_| ParseError::new(
                                    format!("Invalid end index in bit range: {}", num_str),
                                    end_token.span,
                                ))?
                            }
                            _ => return Err(ParseError::new(
                                format!("Expected number for bit range end, found {}", token_kind_to_user_string(&end_token.kind)),
                                end_token.span,
                            )),
                        };
                        
                        // Expect ']'
                        match self.consume_token()? {
                            Token { kind: TokenKind::RBracket, .. } => {},
                            tok => return Err(ParseError::new(
                                format!("Expected ']' to close bit range, found {}", token_kind_to_user_string(&tok.kind)),
                                tok.span,
                            )),
                        }
                        
                        // Check what follows: := or ==
                        let op_token = self.peek_token()?.clone();
                        match op_token.kind {
                            TokenKind::Assign => {
                                self.consume_token()?; // Consume ':='
                                
                                // Parse the number value
                                let value_token = self.consume_token()?;
                                let value_str = match value_token.kind {
                                    TokenKind::Number(num_str) => num_str,
                                    _ => return Err(ParseError::new(
                                        format!("Expected number after bit range assignment, found {}", token_kind_to_user_string(&value_token.kind)),
                                        value_token.span,
                                    )),
                                };
                                
                                // Convert number to bit vector
                                let bits = number_to_bits(&value_str, (end - start) as usize)?;
                                Ok(Expr::bit_range_assign(start, end, bits))
                            }
                            TokenKind::Eq => {
                                self.consume_token()?; // Consume '=='
                                
                                // Parse the number value
                                let value_token = self.consume_token()?;
                                let value_str = match value_token.kind {
                                    TokenKind::Number(num_str) => num_str,
                                    _ => return Err(ParseError::new(
                                        format!("Expected number after bit range test, found {}", token_kind_to_user_string(&value_token.kind)),
                                        value_token.span,
                                    )),
                                };
                                
                                // Convert number to bit vector
                                let bits = number_to_bits(&value_str, (end - start) as usize)?;
                                Ok(Expr::bit_range_test(start, end, bits))
                            }
                            TokenKind::Not => {
                                // Pattern match operator x[start..end] ~ pattern
                                self.consume_token()?; // Consume '~'
                                
                                // Parse pattern with field width
                                let field_width = (end - start) as usize;
                                let pattern = self.parse_pattern(Some(field_width))?;
                                Ok(Expr::bit_range_match(start, end, pattern))
                            }
                            _ => return Err(ParseError::new(
                                format!("Expected ':=', '==' or '~' after bit range, found {}", token_kind_to_user_string(&op_token.kind)),
                                op_token.span,
                            )),
                        }
                    }
                    TokenKind::Assign => {
                        self.consume_token()?; // Consume ':='
                        
                        // Peek for the value token (0 or 1)
                        let value_token_peek = self.peek_token().map_err(|e| e.clone())?;
                        if value_token_peek.kind == TokenKind::Eof {
                            return Err(ParseError::new(
                                format!("Expected '0' or '1' after 'x{} :=' but found end of input", index),
                                value_token_peek.span, // Span of the EOF token
                            ));
                        }
                        // If not EOF, proceed to parse it (original logic)
                        let rhs_expr = self.parse_primary()?; // Use parse_primary for 0 or 1
                        let value_bool = match *rhs_expr {
                            Expr::Zero => false,
                            Expr::One => true,
                            _ => return Err(ParseError::new(
                                "Right-hand side of assignment 'xN :=' must be '0' or '1'".to_string(),
                                // Ideally, span of the rhs_expr. For now, operator span (or current_token's for it)
                                // Let's use the span of the token that formed rhs_expr.
                                // parse_primary consumes the token, so rhs_expr.span() would be ideal if Exp had spans.
                                // For now, using the operator's span is a placeholder.
                                // The original code used next_token_after_field.span, which is the operator.
                                next_token_after_field.span, 
                            )),
                        };
                        Ok(Expr::assign(index, value_bool))
                    }
                    TokenKind::Eq => {
                        self.consume_token()?; // Consume '=='
                        
                        // Peek for the value token (0 or 1)
                        let value_token_peek = self.peek_token().map_err(|e| e.clone())?;
                        if value_token_peek.kind == TokenKind::Eof {
                            return Err(ParseError::new(
                                format!("Expected '0' or '1' after 'x{} ==' but found end of input", index),
                                value_token_peek.span, // Span of the EOF token
                            ));
                        }
                        // If not EOF, proceed to parse it (original logic)
                        let rhs_expr = self.parse_primary()?; // Use parse_primary for 0 or 1
                        let value_bool = match *rhs_expr {
                            Expr::Zero => false,
                            Expr::One => true,
                            _ => return Err(ParseError::new(
                                "Right-hand side of test 'xN ==' must be '0' or '1'".to_string(),
                                // Using the operator's span as a placeholder, similar to Assign.
                                next_token_after_field.span,
                            )),
                        };
                        Ok(Expr::test(index, value_bool))
                    }
                    TokenKind::Not => {
                        // Pattern match: xN ~ pattern
                        self.consume_token()?; // Consume '~'
                        
                        // Parse pattern
                        let pattern = self.parse_pattern(None)?;
                        
                        // Convert to test based on pattern
                        match pattern {
                            Pattern::Exact(bits) if bits.len() == 1 => {
                                // Single bit pattern, convert to test
                                Ok(Expr::test(index, bits[0]))
                            }
                            _ => Err(ParseError::new(
                                format!("Field 'x{}' with '~' operator only supports patterns '0' or '1'", index),
                                next_token_after_field.span,
                            ))
                        }
                    }
                    _ => Err(ParseError::new(
                        format!("Expected ':=', '==' or '~' after field 'x{}', found {}", index, token_kind_to_user_string(&next_token_after_field.kind)),
                        next_token_after_field.span,
                    )),
                }
            }
            // If expression
            TokenKind::If => {
                self.consume_token()?; // Consume 'if'
                let cond = self.parse_until()?; // Parse condition
                
                // Expect 'then'
                match self.consume_token()? {
                    Token { kind: TokenKind::Then, .. } => {},
                    tok => return Err(ParseError::new(
                        format!("Expected 'then' after condition, but found {}", token_kind_to_user_string(&tok.kind)),
                        tok.span,
                    )),
                }
                
                let then_expr = self.parse_until()?; // Parse then branch
                
                // Expect 'else'
                match self.consume_token()? {
                    Token { kind: TokenKind::Else, .. } => {},
                    tok => return Err(ParseError::new(
                        format!("Expected 'else' after then branch, but found {}", token_kind_to_user_string(&tok.kind)),
                        tok.span,
                    )),
                }
                
                let else_expr = self.parse_until()?; // Parse else branch
                
                Ok(Expr::if_then_else(cond, then_expr, else_expr))
            }
            // Let expression
            TokenKind::Let => {
                self.consume_token()?; // Consume 'let'
                
                // Expect identifier
                let var_name = match self.consume_token()? {
                    Token { kind: TokenKind::Ident(name), .. } => name,
                    tok => return Err(ParseError::new(
                        format!("Expected variable name after 'let', but found {}", token_kind_to_user_string(&tok.kind)),
                        tok.span,
                    )),
                };
                
                // Expect '='
                match self.consume_token()? {
                    Token { kind: TokenKind::Eq, .. } => {},
                    tok => return Err(ParseError::new(
                        format!("Expected '=' after variable name, but found {}", token_kind_to_user_string(&tok.kind)),
                        tok.span,
                    )),
                }
                
                // Check if this is a bit range alias by looking for &x[...]
                if self.peek_kind()? == &TokenKind::And {
                    // This is a bit range alias: let alias = &x[start..end] in expr
                    self.consume_token()?; // Consume '&'
                    
                    // Expect identifier (x or alias name)
                    let base_token = self.consume_token()?;
                    let base_name = match &base_token.kind {
                        TokenKind::Ident(name) => name.clone(),
                        _ => return Err(ParseError::new(
                            format!("Expected identifier after '&' in bit range alias, but found {}", token_kind_to_user_string(&base_token.kind)),
                            base_token.span,
                        )),
                    };
                    
                    // Expect '['
                    match self.consume_token()? {
                        Token { kind: TokenKind::LBracket, .. } => {},
                        tok => return Err(ParseError::new(
                            format!("Expected '[' after '&x' in bit range alias, but found {}", token_kind_to_user_string(&tok.kind)),
                            tok.span,
                        )),
                    }
                    
                    // Parse start index
                    let start_token = self.consume_token()?;
                    let start = match start_token.kind {
                        TokenKind::Number(num_str) => {
                            num_str.parse::<u32>().map_err(|_| ParseError::new(
                                format!("Invalid start index in bit range alias: {}", num_str),
                                start_token.span,
                            ))?
                        }
                        TokenKind::Zero => 0,
                        TokenKind::One => 1,
                        _ => return Err(ParseError::new(
                            format!("Expected number for start index, found {}", token_kind_to_user_string(&start_token.kind)),
                            start_token.span,
                        )),
                    };
                    
                    // Expect '..'
                    match self.consume_token()? {
                        Token { kind: TokenKind::DotDot, .. } => {},
                        tok => return Err(ParseError::new(
                            "Expected '..' in bit range alias".to_string(),
                            tok.span,
                        )),
                    }
                    
                    // Parse end index
                    let end_token = self.consume_token()?;
                    let end = match end_token.kind {
                        TokenKind::Number(num_str) => {
                            num_str.parse::<u32>().map_err(|_| ParseError::new(
                                format!("Invalid end index in bit range alias: {}", num_str),
                                end_token.span,
                            ))?
                        }
                        TokenKind::Zero => 0,
                        TokenKind::One => 1,
                        _ => return Err(ParseError::new(
                            format!("Expected number for end index, found {}", token_kind_to_user_string(&end_token.kind)),
                            end_token.span,
                        )),
                    };
                    
                    // Expect ']'
                    match self.consume_token()? {
                        Token { kind: TokenKind::RBracket, .. } => {},
                        tok => return Err(ParseError::new(
                            "Expected ']' after bit range alias".to_string(),
                            tok.span,
                        )),
                    }
                    
                    // Expect 'in'
                    match self.consume_token()? {
                        Token { kind: TokenKind::In, .. } => {},
                        tok => return Err(ParseError::new(
                            format!("Expected 'in' after bit range alias, but found {}", token_kind_to_user_string(&tok.kind)),
                            tok.span,
                        )),
                    }
                    
                    let body = self.parse_until()?; // Parse body
                    
                    // For now, only support 'x' as the base
                    if base_name != "x" {
                        return Err(ParseError::new(
                            format!("Sub-ranges of aliases not yet supported. Use &x[start..end] instead of &{}[start..end]", base_name),
                            base_token.span,
                        ));
                    }
                    
                    Ok(Expr::let_bit_range(var_name, start, end, body))
                } else {
                    // Regular let binding
                    let def = self.parse_until()?; // Parse definition
                    
                    // Expect 'in'
                    match self.consume_token()? {
                        Token { kind: TokenKind::In, .. } => {},
                        tok => return Err(ParseError::new(
                            format!("Expected 'in' after definition, but found {}", token_kind_to_user_string(&tok.kind)),
                            tok.span,
                        )),
                    }
                    
                    let body = self.parse_until()?; // Parse body
                    
                    Ok(Expr::let_in(var_name, def, body))
                }
            }
            // Variable reference or bit range
            TokenKind::Ident(name) => {
                // Look ahead to see if this is a bit range
                if name == "x" {
                    // Try to peek at the next token after 'x'
                    // We need to temporarily consume 'x' to peek at what follows
                    let _x_token = self.consume_token()?; // Consume 'x'
                    
                    if matches!(self.peek_kind(), Ok(&TokenKind::LBracket)) {
                        // This is a bit range expression x[start..end]
                        // 'x' is already consumed above
                        self.consume_token()?; // Consume '['
                    
                    // Parse start index
                    let start_token = self.consume_token()?;
                    let start = match start_token.kind {
                        TokenKind::Number(num_str) => {
                            num_str.parse::<u32>().map_err(|_| ParseError::new(
                                format!("Invalid start index in bit range: {}", num_str),
                                start_token.span,
                            ))?
                        }
                        TokenKind::Zero => 0,
                        TokenKind::One => 1,
                        _ => return Err(ParseError::new(
                            format!("Expected number for bit range start, found {}", token_kind_to_user_string(&start_token.kind)),
                            start_token.span,
                        )),
                    };
                    
                    // Expect '..'
                    match self.consume_token()? {
                        Token { kind: TokenKind::DotDot, .. } => {},
                        tok => return Err(ParseError::new(
                            format!("Expected '..' in bit range, found {}", token_kind_to_user_string(&tok.kind)),
                            tok.span,
                        )),
                    }
                    
                    // Parse end index
                    let end_token = self.consume_token()?;
                    let end = match end_token.kind {
                        TokenKind::Number(num_str) => {
                            num_str.parse::<u32>().map_err(|_| ParseError::new(
                                format!("Invalid end index in bit range: {}", num_str),
                                end_token.span,
                            ))?
                        }
                        TokenKind::Zero => 0,
                        TokenKind::One => 1,
                        _ => return Err(ParseError::new(
                            format!("Expected number for bit range end, found {}", token_kind_to_user_string(&end_token.kind)),
                            end_token.span,
                        )),
                    };
                    
                    // Expect ']'
                    match self.consume_token()? {
                        Token { kind: TokenKind::RBracket, .. } => {},
                        tok => return Err(ParseError::new(
                            format!("Expected ']' to close bit range, found {}", token_kind_to_user_string(&tok.kind)),
                            tok.span,
                        )),
                    }
                    
                    // Check what follows: := or ==
                    let op_token = self.peek_token()?.clone();
                    match op_token.kind {
                        TokenKind::Assign => {
                            self.consume_token()?; // Consume ':='
                            
                            // Parse the literal value
                            let value_token = self.consume_token()?;
                            let (value_str, literal_type) = match value_token.kind {
                                TokenKind::Number(num_str) => (num_str, LiteralType::Decimal),
                                TokenKind::BinaryLiteral(bin_str) => (bin_str, LiteralType::Binary),
                                TokenKind::HexLiteral(hex_str) => (hex_str, LiteralType::Hexadecimal),
                                TokenKind::IpLiteral(ip_str) => (ip_str, LiteralType::IpAddress),
                                TokenKind::Zero => ("0".to_string(), LiteralType::Decimal),
                                TokenKind::One => ("1".to_string(), LiteralType::Decimal),
                                _ => return Err(ParseError::new(
                                    format!("Expected number after bit range assignment, found {}", token_kind_to_user_string(&value_token.kind)),
                                    value_token.span,
                                )),
                            };
                            
                            // Convert literal to bit vector
                            let bits = literal_to_bits(&value_str, literal_type, (end - start) as usize)?;
                            Ok(Expr::bit_range_assign(start, end, bits))
                        }
                        TokenKind::Eq => {
                            self.consume_token()?; // Consume '=='
                            
                            // Parse the literal value
                            let value_token = self.consume_token()?;
                            let (value_str, literal_type) = match value_token.kind {
                                TokenKind::Number(num_str) => (num_str, LiteralType::Decimal),
                                TokenKind::BinaryLiteral(bin_str) => (bin_str, LiteralType::Binary),
                                TokenKind::HexLiteral(hex_str) => (hex_str, LiteralType::Hexadecimal),
                                TokenKind::IpLiteral(ip_str) => (ip_str, LiteralType::IpAddress),
                                TokenKind::Zero => ("0".to_string(), LiteralType::Decimal),
                                TokenKind::One => ("1".to_string(), LiteralType::Decimal),
                                _ => return Err(ParseError::new(
                                    format!("Expected number after bit range test, found {}", token_kind_to_user_string(&value_token.kind)),
                                    value_token.span,
                                )),
                            };
                            
                            // Convert literal to bit vector
                            let bits = literal_to_bits(&value_str, literal_type, (end - start) as usize)?;
                            Ok(Expr::bit_range_test(start, end, bits))
                        }
                        TokenKind::Not => {
                            // Pattern match operator x[start..end] ~ pattern
                            self.consume_token()?; // Consume '~'
                            
                            // Parse pattern with field width
                            let field_width = (end - start) as usize;
                            let pattern = self.parse_pattern(Some(field_width))?;
                            Ok(Expr::bit_range_match(start, end, pattern))
                        }
                        _ => return Err(ParseError::new(
                            format!("Expected ':=', '==' or '~' after bit range, found {}", token_kind_to_user_string(&op_token.kind)),
                            op_token.span,
                        )),
                    }
                    } else {
                        // Not a bit range, 'x' is already consumed, just return it as a variable
                        Ok(Expr::var(name))
                    }
                } else {
                    // Check if this identifier is followed by := or ==
                    self.consume_token()?; // Consume the identifier
                    
                    // Look ahead for assignment or test
                    match self.peek_kind()? {
                        TokenKind::Assign => {
                            // Variable assignment: var := value
                            self.consume_token()?; // Consume ':='
                            
                            // Parse the value (should be a number or literal that evaluates to bits)
                            let value_token = self.consume_token()?;
                            let (value_str, literal_type) = match value_token.kind {
                                TokenKind::Number(num_str) => (num_str, LiteralType::Decimal),
                                TokenKind::BinaryLiteral(bin_str) => (bin_str, LiteralType::Binary),
                                TokenKind::HexLiteral(hex_str) => (hex_str, LiteralType::Hexadecimal),
                                TokenKind::IpLiteral(ip_str) => (ip_str, LiteralType::IpAddress),
                                TokenKind::Zero => ("0".to_string(), LiteralType::Decimal),
                                TokenKind::One => ("1".to_string(), LiteralType::Decimal),
                                _ => return Err(ParseError::new(
                                    format!("Expected value after variable assignment, found {}", token_kind_to_user_string(&value_token.kind)),
                                    value_token.span,
                                )),
                            };
                            
                            // For variable assignment, infer bit width from literal format
                            let inferred_bits = infer_literal_bit_width(&value_str, literal_type)?;
                            let bits = literal_to_bits(&value_str, literal_type, inferred_bits)?;
                            Ok(Expr::var_assign(name, bits))
                        }
                        TokenKind::Eq => {
                            // Variable test: var == value
                            self.consume_token()?; // Consume '=='
                            
                            // Parse the value
                            let value_token = self.consume_token()?;
                            let (value_str, literal_type) = match value_token.kind {
                                TokenKind::Number(num_str) => (num_str, LiteralType::Decimal),
                                TokenKind::BinaryLiteral(bin_str) => (bin_str, LiteralType::Binary),
                                TokenKind::HexLiteral(hex_str) => (hex_str, LiteralType::Hexadecimal),
                                TokenKind::IpLiteral(ip_str) => (ip_str, LiteralType::IpAddress),
                                TokenKind::Zero => ("0".to_string(), LiteralType::Decimal),
                                TokenKind::One => ("1".to_string(), LiteralType::Decimal),
                                _ => return Err(ParseError::new(
                                    format!("Expected value after variable test, found {}", token_kind_to_user_string(&value_token.kind)),
                                    value_token.span,
                                )),
                            };
                            
                            // For variable test, infer bit width from literal format
                            let inferred_bits = infer_literal_bit_width(&value_str, literal_type)?;
                            let bits = literal_to_bits(&value_str, literal_type, inferred_bits)?;
                            Ok(Expr::var_test(name, bits))
                        }
                        TokenKind::Not => {
                            // Pattern match: var ~ pattern
                            self.consume_token()?; // Consume '~'
                            
                            // Parse pattern (no field width for variables)
                            let pattern = self.parse_pattern(None)?;
                            Ok(Expr::var_match(name, pattern))
                        }
                        _ => {
                            // Just a variable reference
                            Ok(Expr::var(name))
                        }
                    }
                }
            }
            // These are simple primaries, handled by the simplified parse_primary
            TokenKind::Zero | TokenKind::One | TokenKind::Top | TokenKind::Dup | TokenKind::LParen | TokenKind::End => {
                self.parse_primary()
            }
            // Any other token here is unexpected when trying to parse an atom or field expression
            _ => Err(ParseError::new(
                format!("Unexpected {} when expecting an expression", token_kind_to_user_string(&current_token.kind)),
                current_token.span,
            )),
        }
    }

    // Simplified parse_primary: handles only 0, 1, T, dup, and parenthesized expressions.
    // Field-related constructs are now handled by parse_atom_or_field_expression.
    fn parse_primary(&mut self) -> Result<Exp, ParseError> {
        let token = self.consume_token()?; // Consume the token for the primary
        match token.kind {
            TokenKind::Zero => Ok(Expr::zero()),
            TokenKind::One => Ok(Expr::one()),
            TokenKind::Top => Ok(Expr::top()),
            TokenKind::Dup => Ok(Expr::dup()),
            TokenKind::LParen => {
                let expr = self.parse_until()?;
                match self.consume_token()? { // Expect RParen
                    Token { kind: TokenKind::RParen, .. } => Ok(expr),
                    tok => Err(ParseError::new(
                        format!("Expected character ')' to close parenthesis, but found {}", token_kind_to_user_string(&tok.kind)),
                        tok.span,
                    )),
                }
            }
            // Field is handled by parse_atom_or_field_expression.
            // Other tokens are invalid starts for a primary expression.
            TokenKind::Field(_) | TokenKind::Ident(_) |
            TokenKind::LtlX | TokenKind::LtlF | TokenKind::LtlG | TokenKind::LtlU | TokenKind::LtlR |
            TokenKind::Not | TokenKind::TestNot | TokenKind::Star | TokenKind::Semicolon | TokenKind::Plus |
            TokenKind::And | TokenKind::Xor | TokenKind::Minus | TokenKind::Assign | TokenKind::Eq |
            TokenKind::RParen | TokenKind::LBracket | TokenKind::RBracket | TokenKind::DotDot | 
            TokenKind::Number(_) | TokenKind::BinaryLiteral(_) | TokenKind::HexLiteral(_) | TokenKind::IpLiteral(_) |
            TokenKind::If | TokenKind::Then | TokenKind::Else | TokenKind::Let | TokenKind::In | TokenKind::Eof => { // Removed End from here due to unreachable pattern, it's handled below.
                 Err(ParseError::new(
                    format!("Unexpected {} when expecting a primary expression (like '0', '1', 'T', 'dup', or '(')", token_kind_to_user_string(&token.kind)),
                    token.span,
                ))
            }
            TokenKind::End => Ok(Expr::end()), // Moved here to be last, resolves unreachable_patterns for End.
        }
    }
    
    /// Parse a pattern for pattern matching expressions
    fn parse_pattern(&mut self, field_width: Option<usize>) -> Result<Pattern, ParseError> {
        let token = self.consume_token()?;
        
        match token.kind {
            TokenKind::IpLiteral(ip_str) => {
                // Check if this is CIDR notation (contains /)
                if let Some(slash_pos) = ip_str.find('/') {
                    let (addr_str, prefix_str) = ip_str.split_at(slash_pos);
                    let prefix_str = &prefix_str[1..]; // Skip the '/'
                    
                    // Parse IP address
                    let addr_bits = ip_to_bits(addr_str)?;
                    
                    // Parse prefix length
                    let prefix_len = prefix_str.parse::<usize>().map_err(|_| ParseError::new(
                        format!("Invalid CIDR prefix length: {}", prefix_str),
                        token.span,
                    ))?;
                    
                    if prefix_len > 32 {
                        return Err(ParseError::new(
                            format!("CIDR prefix length {} exceeds maximum of 32", prefix_len),
                            token.span,
                        ));
                    }
                    
                    Ok(Pattern::Cidr { address: addr_bits, prefix_len })
                } else if ip_str.contains('-') {
                    // IP range: start-end
                    let parts: Vec<&str> = ip_str.split('-').collect();
                    if parts.len() != 2 {
                        return Err(ParseError::new(
                            format!("Invalid range format: {}", ip_str),
                            token.span,
                        ));
                    }
                    
                    let start_bits = ip_to_bits(parts[0])?;
                    let end_bits = ip_to_bits(parts[1])?;
                    
                    Ok(Pattern::IpRange { start: start_bits, end: end_bits })
                } else {
                    // Check if next token is "mask" for wildcard syntax
                    if matches!(self.peek_kind(), Ok(&TokenKind::Ident(ref s)) if s == "mask") {
                        self.consume_token()?; // Consume "mask"
                        
                        // Parse mask value
                        let mask_token = self.consume_token()?;
                        let mask_bits = match mask_token.kind {
                            TokenKind::IpLiteral(mask_str) => ip_to_bits(&mask_str)?,
                            TokenKind::Number(num_str) => {
                                literal_to_bits(&num_str, LiteralType::Decimal, 32)?
                            }
                            TokenKind::HexLiteral(hex_str) => {
                                literal_to_bits(&hex_str, LiteralType::Hexadecimal, 32)?
                            }
                            _ => return Err(ParseError::new(
                                format!("Expected mask value, found {}", token_kind_to_user_string(&mask_token.kind)),
                                mask_token.span,
                            ))
                        };
                        
                        let addr_bits = ip_to_bits(&ip_str)?;
                        Ok(Pattern::Wildcard { address: addr_bits, mask: mask_bits })
                    } else {
                        // Just an exact IP match
                        let bits = ip_to_bits(&ip_str)?;
                        Ok(Pattern::Exact(bits))
                    }
                }
            }
            TokenKind::Number(num_str) => {
                // Check for CIDR notation with regular number
                if matches!(self.peek_kind(), Ok(&TokenKind::Minus)) {
                    // This might be a range
                    self.consume_token()?; // Consume '-'
                    
                    let end_token = self.consume_token()?;
                    let end_str = match end_token.kind {
                        TokenKind::Number(s) => s,
                        TokenKind::IpLiteral(s) => s,
                        _ => return Err(ParseError::new(
                            format!("Expected number after '-' in range, found {}", token_kind_to_user_string(&end_token.kind)),
                            end_token.span,
                        ))
                    };
                    
                    // Use field width if provided, otherwise default to minimal width
                    let start_val = num_str.parse::<u128>().map_err(|_| ParseError::new(
                        format!("Invalid number: {}", num_str),
                        token.span,
                    ))?;
                    let end_val = if end_str.contains('.') {
                        // This is an IP address end point, parse it as IP
                        let end_bits = ip_to_bits(&end_str)?;
                        bits_to_u128(&end_bits).map_err(|_| ParseError::new(
                            format!("IP address too large for range"),
                            end_token.span,
                        ))?
                    } else {
                        end_str.parse::<u128>().map_err(|_| ParseError::new(
                            format!("Invalid number: {}", end_str),
                            end_token.span,
                        ))?
                    };
                    
                    // Determine bit width needed for the range
                    let max_val = std::cmp::max(start_val, end_val);
                    let min_width = if max_val == 0 {
                        1
                    } else {
                        (128 - max_val.leading_zeros()) as usize
                    };
                    
                    // Use field width if provided and sufficient, otherwise use minimal width
                    let width = match field_width {
                        Some(fw) if fw >= min_width => fw,
                        _ => min_width,
                    };
                    
                    let start_bits = u128_to_bits(start_val, width);
                    let end_bits = if end_str.contains('.') {
                        ip_to_bits(&end_str)?
                    } else {
                        u128_to_bits(end_val, width)
                    };
                    
                    Ok(Pattern::IpRange { start: start_bits, end: end_bits })
                } else {
                    // Just an exact match
                    let val = num_str.parse::<u128>().map_err(|_| ParseError::new(
                        format!("Invalid number: {}", num_str),
                        token.span,
                    ))?;
                    
                    // Determine bit width
                    let min_width = if val == 0 {
                        1
                    } else {
                        (128 - val.leading_zeros()) as usize
                    };
                    
                    let width = match field_width {
                        Some(fw) if fw >= min_width => fw,
                        _ => min_width,
                    };
                    
                    let bits = u128_to_bits(val, width);
                    Ok(Pattern::Exact(bits))
                }
            }
            TokenKind::BinaryLiteral(bin_str) => {
                let inferred_width = infer_literal_bit_width(&bin_str, LiteralType::Binary)?;
                let bits = literal_to_bits(&bin_str, LiteralType::Binary, inferred_width)?;
                Ok(Pattern::Exact(bits))
            }
            TokenKind::HexLiteral(hex_str) => {
                let inferred_width = infer_literal_bit_width(&hex_str, LiteralType::Hexadecimal)?;
                let bits = literal_to_bits(&hex_str, LiteralType::Hexadecimal, inferred_width)?;
                Ok(Pattern::Exact(bits))
            }
            TokenKind::Zero => {
                // Handle pattern ~ 0
                Ok(Pattern::Exact(vec![false]))
            }
            TokenKind::One => {
                // Handle pattern ~ 1
                Ok(Pattern::Exact(vec![true]))
            }
            _ => Err(ParseError::new(
                format!("Expected pattern (IP address, number, or literal), found {}", token_kind_to_user_string(&token.kind)),
                token.span,
            ))
        }
    }
}

/// Convert u128 to bit vector of specified width
fn u128_to_bits(val: u128, width: usize) -> Vec<bool> {
    let mut bits = Vec::with_capacity(width);
    for i in (0..width).rev() {
        bits.push((val >> i) & 1 == 1);
    }
    bits
}

/// Convert bit vector to u128
fn bits_to_u128(bits: &[bool]) -> Result<u128, ParseError> {
    if bits.len() > 128 {
        return Err(ParseError::new(
            format!("Bit vector too large (max 128 bits)"),
            Default::default(),
        ));
    }
    
    let mut val = 0u128;
    for (i, &bit) in bits.iter().enumerate() {
        if bit {
            val |= 1u128 << (bits.len() - 1 - i);
        }
    }
    Ok(val)
}

/// Helper function to convert IP address string to 32-bit vector
fn ip_to_bits(ip_str: &str) -> Result<Vec<bool>, ParseError> {
    let parts: Vec<&str> = ip_str.split('.').collect();
    if parts.len() != 4 {
        return Err(ParseError::new(
            format!("Invalid IP address format: {}", ip_str),
            Default::default(),
        ));
    }
    
    let mut bits = Vec::with_capacity(32);
    for part in parts {
        let octet = part.parse::<u8>().map_err(|_| ParseError::new(
            format!("Invalid IP octet: {}", part),
            Default::default(),
        ))?;
        
        // Add 8 bits for this octet (MSB first)
        for i in (0..8).rev() {
            bits.push((octet >> i) & 1 == 1);
        }
    }
    
    Ok(bits)
}


#[derive(Debug, Clone, Copy)]
enum LiteralType {
    Decimal,
    Binary,
    Hexadecimal,
    IpAddress,
}

// Helper function to convert various literal formats to a bit vector
// Infer bit width based on literal format
fn infer_literal_bit_width(literal_str: &str, literal_type: LiteralType) -> Result<usize, ParseError> {
    match literal_type {
        LiteralType::Binary => {
            // Binary literal: bit width = number of digits
            Ok(literal_str.len())
        }
        LiteralType::Hexadecimal => {
            // Hex literal: bit width = 4 * number of digits
            Ok(literal_str.len() * 4)
        }
        LiteralType::IpAddress => {
            // IP address: always 32 bits
            Ok(32)
        }
        LiteralType::Decimal => {
            // Decimal: use minimal bit width
            let num = literal_str.parse::<u128>().map_err(|_| ParseError::new(
                format!("Invalid decimal number: {}", literal_str),
                Default::default(),
            ))?;
            
            // Find minimal bit width needed
            if num == 0 {
                Ok(1)
            } else {
                // Calculate bits needed: floor(log2(num)) + 1
                Ok((128 - num.leading_zeros()) as usize)
            }
        }
    }
}

fn literal_to_bits(literal_str: &str, literal_type: LiteralType, expected_bits: usize) -> Result<Vec<bool>, ParseError> {
    let num = match literal_type {
        LiteralType::Decimal => {
            literal_str.parse::<u128>().map_err(|_| ParseError::new(
                format!("Invalid decimal number: {}", literal_str),
                Default::default(),
            ))?
        }
        LiteralType::Binary => {
            u128::from_str_radix(literal_str, 2).map_err(|_| ParseError::new(
                format!("Invalid binary literal: 0b{}", literal_str),
                Default::default(),
            ))?
        }
        LiteralType::Hexadecimal => {
            u128::from_str_radix(literal_str, 16).map_err(|_| ParseError::new(
                format!("Invalid hexadecimal literal: 0x{}", literal_str),
                Default::default(),
            ))?
        }
        LiteralType::IpAddress => {
            // Convert IP address to 32-bit integer
            let parts: Vec<&str> = literal_str.split('.').collect();
            if parts.len() != 4 {
                return Err(ParseError::new(
                    format!("Invalid IP address format: {}", literal_str),
                    Default::default(),
                ));
            }
            
            let mut ip_num = 0u128;
            for (i, part) in parts.iter().enumerate() {
                let octet = part.parse::<u8>().map_err(|_| ParseError::new(
                    format!("Invalid IP octet: {}", part),
                    Default::default(),
                ))?;
                ip_num |= (octet as u128) << (8 * (3 - i));
            }
            ip_num
        }
    };
    
    let mut bits = Vec::with_capacity(expected_bits);
    for i in 0..expected_bits {
        bits.push((num >> i) & 1 == 1);
    }
    
    // Check if the number fits in the expected bits
    if num >= (1u128 << expected_bits) {
        return Err(ParseError::new(
            format!("Number {} requires more than {} bits", num, expected_bits),
            Default::default(),
        ));
    }
    
    Ok(bits)
}

// Helper function to convert a number string to a bit vector (for backward compatibility)
fn number_to_bits(num_str: &str, expected_bits: usize) -> Result<Vec<bool>, ParseError> {
    literal_to_bits(num_str, LiteralType::Decimal, expected_bits)
}

// Main public parsing function
// It needs to convert the internal ParseError (with its 0-indexed Span)
// into the lib::ParseErrorDetails (with its 1-indexed ErrorSpan) for src/lib.rs.
pub fn parse_expressions(input: &str) -> Result<Vec<Exp>, ParseErrorDetails> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let mut expressions = Vec::new();

    loop {
        match parser.peek_kind() {
            Ok(&TokenKind::Eof) => break, 
            Err(ref pe) if pe.message.contains("Peeked beyond EOF") || pe.message.contains("Unexpected end of token stream") => break, 
            Err(pe) => return Err(convert_parse_error(pe.clone(), input)), // Cloned because pe is a reference
            _ => {} 
        }

        match parser.parse_single_expression() {
            Ok(expr) => expressions.push(expr),
            Err(pe) => return Err(convert_parse_error(pe, input)),
        }

        match parser.peek_kind() {
            Ok(&TokenKind::Eof) => break, 
            Err(ref pe) if pe.message.contains("Peeked beyond EOF") || pe.message.contains("Unexpected end of token stream") => break, 
            Ok(ref kind_from_first_peek) => {
                // Immediately clone the kind to own it and release the borrow from peek_kind.
                let owned_kind = (*kind_from_first_peek).clone();

                // Now, separately get the span from a new peek_token call.
                let span_for_error = match parser.peek_token() { 
                    Ok(token) => token.span,
                    Err(_) => Default::default(), // Fallback if peeking full token fails
                };

                let err = ParseError::new(
                    format!("Expected operator, but found {}", token_kind_to_user_string(&owned_kind)),
                    span_for_error,
                );
                return Err(convert_parse_error(err, input));
            }
            Err(pe) => return Err(convert_parse_error(pe.clone(), input)), 
        }
    }

    if expressions.is_empty() && !input.trim().is_empty() {
        // This path might indicate an issue if input was expected to produce expressions.
        // However, the parser loop correctly handles empty valid inputs (like only comments).
        // If an error occurred before any expressions were parsed, it would have been returned already.
        // If the input is non-empty but yields no expressions (e.g., "end" or "end end"),
        // it could be valid depending on grammar interpretation (e.g. zero expressions allowed).
        // For now, we allow it to return an empty Vec. The caller (lib.rs) can decide how to treat it.
    }

    Ok(expressions)
}

// Helper to convert internal ParseError to the lib's ParseErrorDetails
fn convert_parse_error(pe: ParseError, _input: &str) -> ParseErrorDetails {
    // In a more advanced scenario, `input` could be used with `pe.span.offset`
    // to find line/column if `pe.span` only had offsets. But our `pe.span` has line/col.
    ParseErrorDetails {
        message: pe.message,
        span: Some(ErrorSpan { // Using ErrorSpan directly, assuming it's in scope via `use crate::ErrorSpan`
            start_line: pe.span.start.line,
            start_column: pe.span.start.column,
            end_line: pe.span.end.line,
            // End column for Monaco is typically exclusive for single char, inclusive for multi-char.
            // Our `end.column` is the column *after* the last char of the token.
            // For a single char token at col 5, start.col=5, end.col=6.
            // Monaco marker: startColumn, endColumn. If endColumn is startColumn+1, it's one char.
            // Let's make end_column inclusive for Monaco.
            // If token is 1 char, start=5, end=6. Monaco: start=5, end=6. Correct.
            // If token "dup" starts col 5: d(5) u(6) p(7). start.col=5, end.col=8.
            // Monaco: start=5, end=8. Correct (highlights d,u,p).
            end_column: pe.span.end.column,
        }),
    }
}

// Helper to convert TokenKind to a user-friendly string representation
fn token_kind_to_user_string(kind: &TokenKind) -> String {
    match kind {
        TokenKind::Zero => "'0'".to_string(),
        TokenKind::One => "'1'".to_string(),
        TokenKind::Top => "keyword 'T'".to_string(),
        TokenKind::Assign => "operator ':='".to_string(),
        TokenKind::Eq => "operator '=='".to_string(),
        TokenKind::Plus => "operator '+'".to_string(),
        TokenKind::And => "operator '&'".to_string(),
        TokenKind::Xor => "operator '^'".to_string(),
        TokenKind::Minus => "operator '-'".to_string(),
        TokenKind::Not => "operator '~'".to_string(),
        TokenKind::TestNot => "operator '!'".to_string(),
        TokenKind::Semicolon => "operator ';'".to_string(),
        TokenKind::Star => "operator '*'".to_string(),
        TokenKind::Dup => "keyword 'dup'".to_string(),
        TokenKind::LtlX => "LTL operator 'X'".to_string(),
        TokenKind::LtlU => "LTL operator 'U'".to_string(),
        TokenKind::LtlF => "LTL operator 'F'".to_string(),
        TokenKind::LtlG => "LTL operator 'G'".to_string(),
        TokenKind::LtlR => "LTL operator 'R'".to_string(),
        TokenKind::LParen => "character '('".to_string(),
        TokenKind::RParen => "character ')'".to_string(),
        TokenKind::LBracket => "character '['".to_string(),
        TokenKind::RBracket => "character ']'".to_string(),
        TokenKind::DotDot => "operator '..'".to_string(),
        TokenKind::Field(u) => format!("field 'x{}'", u),
        TokenKind::Number(n) => format!("number '{}'", n),
        TokenKind::BinaryLiteral(b) => format!("binary literal '0b{}'", b),
        TokenKind::HexLiteral(h) => format!("hexadecimal literal '0x{}'", h),
        TokenKind::IpLiteral(ip) => format!("IP literal '{}'", ip),
        TokenKind::Ident(s) => format!("identifier '{}'", s),
        TokenKind::If => "keyword 'if'".to_string(),
        TokenKind::Then => "keyword 'then'".to_string(),
        TokenKind::Else => "keyword 'else'".to_string(),
        TokenKind::Let => "keyword 'let'".to_string(),
        TokenKind::In => "keyword 'in'".to_string(),
        TokenKind::End => "keyword 'end'".to_string(),
        TokenKind::Eof => "end of input".to_string(),
    }
}

// --- Tests ---
#[cfg(test)]
mod tests {
    use super::{parse_expressions, Exp}; // Removed Lexer, Parser, TokenKind
    use crate::expr::Expr; 

    // Helper to parse all expressions from a string and get a Vec<Exp>
    // Maps error to String for test assertion convenience.
    fn parse_all(s: &str) -> Result<Vec<Exp>, String> {
        match parse_expressions(s) {
            Ok(exprs) => Ok(exprs),
            Err(details) => Err(details.message), // Using the message from ParseErrorDetails
        }
    }

    // Helper to parse a single expression expected from a string.
    // Fails if zero or more than one expression is parsed.
    fn parse_single_unwrap(s: &str) -> Exp {
        match parse_expressions(s) {
            Ok(mut exprs) => {
                if exprs.len() == 1 {
                    exprs.remove(0)
                } else {
                    panic!("Expected single expression, found {} for input: {}", exprs.len(), s);
                }
            }
            Err(details) => panic!("Parse error for input '{}': {}", s, details.message),
        }
    }

    #[test]
    fn test_simple_literals() {
        assert_eq!(parse_single_unwrap("0"), Expr::zero());
        assert_eq!(parse_single_unwrap("1"), Expr::one());
        assert_eq!(parse_single_unwrap("T"), Expr::top());
        assert_eq!(parse_single_unwrap("dup"), Expr::dup());
    }

    #[test]
    fn test_field_operations() {
        assert_eq!(parse_single_unwrap("x1 := 0"), Expr::assign(1, false));
        assert_eq!(parse_single_unwrap("x2 == 1"), Expr::test(2, true));
    }

    #[test]
    fn test_sequence() {
        assert_eq!(parse_single_unwrap("x0==1 ; x1==0"), 
                   Expr::sequence(Expr::test(0, true), Expr::test(1, false)));
    }

    #[test]
    fn test_union() {
        assert_eq!(parse_single_unwrap("0 + 1"), Expr::union(Expr::zero(), Expr::one()));
    }

    #[test]
    fn test_parentheses() {
        assert_eq!(parse_single_unwrap("(0+1)"), Expr::union(Expr::zero(), Expr::one()));
        assert_eq!(parse_single_unwrap("~(0)"), Expr::complement(Expr::zero()));
    }

    #[test]
    fn test_star() {
        assert_eq!(parse_single_unwrap("0*"), Expr::star(Expr::zero()));
        assert_eq!(parse_single_unwrap("(x1==0)*"), Expr::star(Expr::test(1, false)));
    }

    #[test]
    fn test_ltl_operators() {
        assert_eq!(parse_single_unwrap("X 0"), Expr::ltl_next(Expr::zero()));
        // F e = T U e
        assert_eq!(parse_single_unwrap("F 0"), Expr::ltl_until(Expr::top(), Expr::zero())); 
        // G e = !F!e = !(T U !e)
        assert_eq!(parse_single_unwrap("G 0"), Expr::ltl_globally(Expr::zero()));
        assert_eq!(parse_single_unwrap("0 U 1"), Expr::ltl_until(Expr::zero(), Expr::one()));
        // e1 R e2 = !( !e1 U !e2 )
        assert_eq!(parse_single_unwrap("0 R 1"), 
            Expr::complement(Expr::ltl_until(Expr::complement(Expr::zero()), Expr::complement(Expr::one())))
        );
    }

    #[test]
    fn test_empty_input() {
        assert!(parse_all("").unwrap().is_empty());
        assert!(parse_all("   ").unwrap().is_empty()); // Whitespace only
    }

    #[test]
    fn test_comments_only() {
        assert!(parse_all("// this is a comment").unwrap().is_empty());
        assert!(parse_all("// comment 1\n// comment 2").unwrap().is_empty());
    }

    #[test]
    fn test_expression_then_eof() {
        assert_eq!(parse_single_unwrap("x7 == 0"), Expr::test(7, false));
    }

    #[test]
    fn test_invalid_token() {
        assert!(parse_all("$").is_err());
        assert!(parse_all("0 $").is_err());
    }

    #[test]
    fn test_incomplete_expressions() {
        assert!(parse_all("x1 :=").is_err(), "Incomplete assignment");
        assert!(parse_all("x1 ==").is_err(), "Incomplete test");
        assert!(parse_all("0 +").is_err(), "Incomplete addition");
        assert!(parse_all("(").is_err(), "Unclosed parenthesis");
        assert!(parse_all("0 U").is_err(), "Incomplete Until");
    }

    #[test]
    fn test_unexpected_token_after_expr() {
        // parse_expressions expects 'end' or EOF after an expression if there are multiple.
        // If single expression, it should be fine.
        assert!(parse_all("0 1").is_err(), "Expected 'end' or EOF after expr, not another expr"); 
    }

    #[test]
    fn test_complex_expression() {
        let expr_str = "(x1:=0 ; (T* & ~(x2==1))) + F (x3==0 U x4==1)";
        assert!(parse_all(expr_str).is_ok());
        // More detailed check if needed by comparing the resulting Exp structure.
    }

    #[test]
    fn test_syntax_errors() {
        let test_cases = vec![
            "x := 0",
            "x1 :=",
            "x1 : 0",
            "x1 = 0",
            "x1 == ",
            "0 +",
            "(0 + 1",
            "0 + 1)",
            "~ ",
            "F ",
            "X ",
            "0 U ",
            "0 R ",
            "foo",
            "x1 := T",
            "x1 == T",
            "0 1",
            "*0",
            "0;*1",
            ":",
            "==",
            "x",
            "e",
            "en",
            "du",
            "0 // comment", // This one should pass
            "0 // comment \n 1"
        ];

        println!("--- Checking Syntax Error Messages ---");
        for (i, input) in test_cases.iter().enumerate() {
            match parse_all(input) {
                Ok(exprs) => {
                    if input.trim() == "0 // comment" { // This one is valid
                        if exprs.len() == 1 && exprs[0] == Expr::zero() {
                            println!("{}. Input: {:<15} -> PASSED (Correctly parsed)", i + 1, input);
                        } else {
                             println!("{}. Input: {:<15} -> UNEXPECTED SUCCESS (but was expected to pass): {:?}", i + 1, input, exprs);
                        }
                    } else {
                        println!("{}. Input: {:<15} -> UNEXPECTED SUCCESS: {:?}", i + 1, input, exprs);
                    }
                }
                Err(e) => {
                    println!("{}. Input: {:<15} -> Error: \"{}\"", i + 1, input, e);
                }
            }
        }
        println!("--- Finished Checking Syntax Error Messages ---");
        assert!(true); // Dummy assertion
    }

    // ===== OPERATOR PRECEDENCE TESTS =====
    
    #[test]
    fn test_precedence_postfix_vs_prefix() {
        // Postfix should bind tighter than prefix
        // !a* should be !(a*), not (!a)*
        assert_eq!(parse_single_unwrap("~0*"), 
                   parse_single_unwrap("~(0*)"));
        assert_eq!(parse_single_unwrap("X 1*"), 
                   parse_single_unwrap("X (1*)"));
        assert_eq!(parse_single_unwrap("F T*"), 
                   parse_single_unwrap("F (T*)"));
        assert_eq!(parse_single_unwrap("G dup*"), 
                   parse_single_unwrap("G (dup*)"));
    }

    #[test]
    fn test_precedence_prefix_vs_infix() {
        // Prefix should bind tighter than infix
        // !a + b should be (!a) + b, not !(a + b)
        assert_eq!(parse_single_unwrap("~0 + 1"), 
                   parse_single_unwrap("(~0) + 1"));
        assert_eq!(parse_single_unwrap("X 0 & 1"), 
                   parse_single_unwrap("(X 0) & 1"));
        assert_eq!(parse_single_unwrap("F 0 ; 1"), 
                   parse_single_unwrap("(F 0) ; 1"));
        assert_eq!(parse_single_unwrap("G 0 U 1"), 
                   parse_single_unwrap("(G 0) U 1"));
    }

    #[test]
    fn test_precedence_infix_vs_postfix() {
        // Postfix should bind tighter than infix
        // a + b* should be a + (b*), not (a + b)*
        assert_eq!(parse_single_unwrap("0 + 1*"), 
                   parse_single_unwrap("0 + (1*)"));
        assert_eq!(parse_single_unwrap("0 & T*"), 
                   parse_single_unwrap("0 & (T*)"));
        assert_eq!(parse_single_unwrap("0 ; dup*"), 
                   parse_single_unwrap("0 ; (dup*)"));
        assert_eq!(parse_single_unwrap("0 U 1*"), 
                   parse_single_unwrap("0 U (1*)"));
    }

    #[test]
    fn test_precedence_intersection_vs_sequence() {
        // Intersection should bind tighter than sequence
        // a & b ; c should be (a & b) ; c
        assert_eq!(parse_single_unwrap("0 & 1 ; T"), 
                   parse_single_unwrap("(0 & 1) ; T"));
        // a ; b & c should be a ; (b & c)
        assert_eq!(parse_single_unwrap("0 ; 1 & T"), 
                   parse_single_unwrap("0 ; (1 & T)"));
    }

    #[test]
    fn test_precedence_sequence_vs_additive() {
        // Sequence should bind tighter than additive (this was the original issue)
        // a ; b + c should be (a ; b) + c
        assert_eq!(parse_single_unwrap("0 ; 1 + T"), 
                   parse_single_unwrap("(0 ; 1) + T"));
        // a + b ; c should be a + (b ; c)
        assert_eq!(parse_single_unwrap("0 + 1 ; T"), 
                   parse_single_unwrap("0 + (1 ; T)"));
        
        // Same for other additive operators
        assert_eq!(parse_single_unwrap("0 ; 1 ^ T"), 
                   parse_single_unwrap("(0 ; 1) ^ T"));
        assert_eq!(parse_single_unwrap("0 ; 1 - T"), 
                   parse_single_unwrap("(0 ; 1) - T"));
    }

    #[test]
    fn test_precedence_additive_vs_until() {
        // Additive should bind tighter than until/release
        // a + b U c should be (a + b) U c
        assert_eq!(parse_single_unwrap("0 + 1 U T"), 
                   parse_single_unwrap("(0 + 1) U T"));
        // a U b + c should be a U (b + c)
        assert_eq!(parse_single_unwrap("0 U 1 + T"), 
                   parse_single_unwrap("0 U (1 + T)"));
        
        // Same for release
        assert_eq!(parse_single_unwrap("0 + 1 R T"), 
                   parse_single_unwrap("(0 + 1) R T"));
    }

    #[test]
    fn test_associativity_left_associative() {
        // Left-associative operators: &, ;, +, ^, -
        
        // Intersection
        assert_eq!(parse_single_unwrap("0 & 1 & T"), 
                   parse_single_unwrap("(0 & 1) & T"));
        
        // Sequence
        assert_eq!(parse_single_unwrap("0 ; 1 ; T"), 
                   parse_single_unwrap("(0 ; 1) ; T"));
        
        // Union
        assert_eq!(parse_single_unwrap("0 + 1 + T"), 
                   parse_single_unwrap("(0 + 1) + T"));
        
        // XOR
        assert_eq!(parse_single_unwrap("0 ^ 1 ^ T"), 
                   parse_single_unwrap("(0 ^ 1) ^ T"));
        
        // Difference
        assert_eq!(parse_single_unwrap("0 - 1 - T"), 
                   parse_single_unwrap("(0 - 1) - T"));
    }

    #[test]
    fn test_associativity_right_associative() {
        // Right-associative operators: U, R, prefix operators
        
        // Until
        assert_eq!(parse_single_unwrap("0 U 1 U T"), 
                   parse_single_unwrap("0 U (1 U T)"));
        
        // Release
        assert_eq!(parse_single_unwrap("0 R 1 R T"), 
                   parse_single_unwrap("0 R (1 R T)"));
        
        // Prefix operators
        assert_eq!(parse_single_unwrap("~~0"), 
                   parse_single_unwrap("~(~0)"));
        assert_eq!(parse_single_unwrap("~X 0"), 
                   parse_single_unwrap("~(X 0)"));
        assert_eq!(parse_single_unwrap("X F 0"), 
                   parse_single_unwrap("X (F 0)"));
    }

    #[test]
    fn test_associativity_postfix() {
        // Multiple stars should be left-associative: a** = (a*)*
        assert_eq!(parse_single_unwrap("0**"), 
                   parse_single_unwrap("(0*)*"));
        assert_eq!(parse_single_unwrap("T***"), 
                   parse_single_unwrap("((T*)*)*"));
    }

    #[test]
    fn test_mixed_precedence_complex() {
        // Complex mixed precedence tests
        
        // !a* + b ; c & d U e should be ((!((a)*)) + (b ; (c & d))) U e
        assert_eq!(parse_single_unwrap("~0* + 1 ; T & dup U x1==0"), 
                   parse_single_unwrap("(~(0*) + (1 ; (T & dup))) U x1==0"));
        
        // a & !b* ; c + d should be ((a & (!(b*))) ; c) + d 
        // Precedence: postfix (*), prefix (!), intersection (&), sequence (;), additive (+)
        assert_eq!(parse_single_unwrap("0 & ~1* ; T + dup"), 
                   parse_single_unwrap("((0 & ~(1*)) ; T) + dup"));
    }

    #[test]
    fn test_parentheses_override_precedence() {
        // Parentheses should override precedence - test meaningful differences
        
        // Show that parentheses change the default precedence behavior
        assert_ne!(parse_single_unwrap("(0 + 1) ; T"), 
                   parse_single_unwrap("0 + 1 ; T")); // Default: 0 + (1 ; T)
        
        assert_ne!(parse_single_unwrap("0 + (1 ; T)"), 
                   parse_single_unwrap("0 ; 1 + T")); // Default: (0 ; 1) + T
        
        assert_ne!(parse_single_unwrap("(~0)*"), 
                   parse_single_unwrap("~0*")); // Default: ~(0*)
        
        assert_ne!(parse_single_unwrap("(0 U 1) + T"), 
                   parse_single_unwrap("0 U 1 + T")); // Default: 0 U (1 + T)
    }

    #[test]
    fn test_field_operations_precedence() {
        // Field operations should be atomic (highest precedence)
        assert_eq!(parse_single_unwrap("x1 := 0 + x2 == 1"), 
                   parse_single_unwrap("(x1 := 0) + (x2 == 1)"));
        assert_eq!(parse_single_unwrap("~x1 := 0"), 
                   parse_single_unwrap("~(x1 := 0)"));
        assert_eq!(parse_single_unwrap("x1 == 1*"), 
                   parse_single_unwrap("(x1 == 1)*"));
    }

    #[test]
    fn test_precedence_edge_cases() {
        // Edge cases that might be tricky
        
        // Prefix followed by postfix should work
        assert_eq!(parse_single_unwrap("~T*"), 
                   parse_single_unwrap("~(T*)"));
        
        // Multiple prefix operators
        assert_eq!(parse_single_unwrap("~~X F 0"), 
                   parse_single_unwrap("~(~(X (F 0)))"));
        
        // Mixing all operator types
        assert_eq!(parse_single_unwrap("~(x1:=0)* + T ; dup & 1 U 0"), 
                   parse_single_unwrap("(~(x1:=0)* + (T ; (dup & 1))) U 0"));
    }

    #[test]
    fn test_original_user_issue() {
        // Test the specific issue reported by the user:
        // (a ; b + c) should be parsed as (a ; b) + c, not a ; (b + c)
        
        // Using concrete expressions to make the test clearer
        assert_eq!(parse_single_unwrap("x1==0 ; x2==1 + T"), 
                   parse_single_unwrap("(x1==0 ; x2==1) + T"));
        
        // Verify the opposite case with parentheses
        assert_eq!(parse_single_unwrap("x1==0 ; (x2==1 + T)"), 
                   parse_single_unwrap("x1==0 ; (x2==1 + T)")); // Already correct
        
        // They should be different expressions
        assert_ne!(parse_single_unwrap("x1==0 ; x2==1 + T"), 
                   parse_single_unwrap("x1==0 ; (x2==1 + T)"));
        
        // More examples showing the fix
        assert_eq!(parse_single_unwrap("0 ; 1 + T"), 
                   parse_single_unwrap("(0 ; 1) + T"));
        assert_eq!(parse_single_unwrap("dup ; T ^ 1"), 
                   parse_single_unwrap("(dup ; T) ^ 1"));
        assert_eq!(parse_single_unwrap("T ; 0 - 1"), 
                   parse_single_unwrap("(T ; 0) - 1"));
    }
    
    #[test]
    fn test_bit_range_parsing() {
        // Test parsing bit range assignments
        assert_eq!(parse_single_unwrap("x[0..8] := 255"), 
                   Expr::bit_range_assign(0, 8, vec![true, true, true, true, true, true, true, true]));
        
        // Test parsing bit range tests
        assert_eq!(parse_single_unwrap("x[2..4] == 3"), 
                   Expr::bit_range_test(2, 4, vec![true, true]));
        
        // Test with zero
        assert_eq!(parse_single_unwrap("x[0..4] == 0"), 
                   Expr::bit_range_test(0, 4, vec![false, false, false, false]));
        
        // Test single bit range
        assert_eq!(parse_single_unwrap("x[5..6] := 1"), 
                   Expr::bit_range_assign(5, 6, vec![true]));
    }
    
    #[test]
    fn test_bit_range_in_expression() {
        // Test bit ranges in larger expressions
        let expr = parse_single_unwrap("x[0..4] == 5 + x[4..8] := 10");
        let expected = Expr::union(
            Expr::bit_range_test(0, 4, vec![true, false, true, false]),
            Expr::bit_range_assign(4, 8, vec![false, true, false, true])
        );
        assert_eq!(expr, expected);
        
        // Test with sequence
        let expr2 = parse_single_unwrap("x[0..2] := 3 ; x[2..4] == 0");
        let expected2 = Expr::sequence(
            Expr::bit_range_assign(0, 2, vec![true, true]),
            Expr::bit_range_test(2, 4, vec![false, false])
        );
        assert_eq!(expr2, expected2);
    }
    
    #[test]
    fn test_literal_formats() {
        // Test binary literals
        let expr1 = parse_single_unwrap("x[0..4] := 0b1010");
        assert_eq!(expr1, Expr::bit_range_assign(0, 4, vec![false, true, false, true]));
        
        // Test hexadecimal literals
        let expr2 = parse_single_unwrap("x[0..8] := 0xFF");
        assert_eq!(expr2, Expr::bit_range_assign(0, 8, vec![true, true, true, true, true, true, true, true]));
        
        // Test IP address literals (192.168.1.1 = 0xC0A80101)
        let expr3 = parse_single_unwrap("x[0..32] := 192.168.1.1");
        // IP addresses are converted in big-endian format: 192.168.1.1 = 0xC0A80101
        // But our bit vector is little-endian, so bit 0 is the LSB
        // 0xC0A80101 = 3232235777 in decimal  
        let mut ip_bits = vec![false; 32];
        let ip_num = 0xC0A80101u32;
        for i in 0..32 {
            ip_bits[i] = (ip_num >> i) & 1 == 1;
        }
        assert_eq!(expr3, Expr::bit_range_assign(0, 32, ip_bits));
        
        // Test mixed formats in compound expression
        let expr4 = parse_single_unwrap("x[0..4] == 0b1100 + x[4..12] := 0xF0");
        let expected4 = Expr::union(
            Expr::bit_range_test(0, 4, vec![false, false, true, true]),
            Expr::bit_range_assign(4, 12, vec![false, false, false, false, true, true, true, true])
        );
        assert_eq!(expr4, expected4);
    }
    
    #[test]
    fn test_literal_edge_cases() {
        // Test single bits
        assert_eq!(parse_single_unwrap("x[0..1] := 0b1"), 
                   Expr::bit_range_assign(0, 1, vec![true]));
        
        assert_eq!(parse_single_unwrap("x[5..6] == 0x0"), 
                   Expr::bit_range_test(5, 6, vec![false]));
                   
        // Test larger hex values
        assert_eq!(parse_single_unwrap("x[0..16] := 0xABCD"),
                   Expr::bit_range_assign(0, 16, vec![
                       true, false, true, true, false, false, true, true, // 0xCD = 205
                       true, true, false, true, false, true, false, true   // 0xAB = 171
                   ]));
    }
    
    #[test]
    fn test_let_parsing() {
        // Basic let binding
        let expr = parse_single_unwrap("let x = 1 in x + 0");
        assert_eq!(expr, Expr::let_in(
            "x".to_string(),
            Expr::one(),
            Expr::union(Expr::var("x".to_string()), Expr::zero())
        ));
        
        // Let with assignment
        let expr = parse_single_unwrap("let config = x0 := 1 in config ; x1 := 0");
        assert_eq!(expr, Expr::let_in(
            "config".to_string(),
            Expr::assign(0, true),
            Expr::sequence(
                Expr::var("config".to_string()),
                Expr::assign(1, false)
            )
        ));
        
        // Let with test
        let expr = parse_single_unwrap("let test = x0 == 1 in test & x1 == 0");
        assert_eq!(expr, Expr::let_in(
            "test".to_string(),
            Expr::test(0, true),
            Expr::intersect(
                Expr::var("test".to_string()),
                Expr::test(1, false)
            )
        ));
    }
    
    #[test]
    fn test_nested_let_parsing() {
        // Nested let bindings
        let expr = parse_single_unwrap("let x = 0 in let y = 1 in x + y");
        assert_eq!(expr, Expr::let_in(
            "x".to_string(),
            Expr::zero(),
            Expr::let_in(
                "y".to_string(),
                Expr::one(),
                Expr::union(
                    Expr::var("x".to_string()),
                    Expr::var("y".to_string())
                )
            )
        ));
        
        // Let with complex expression
        let expr = parse_single_unwrap("let p = (x0 := 1 ; x1 := 0) in p + p*");
        assert_eq!(expr, Expr::let_in(
            "p".to_string(),
            Expr::sequence(Expr::assign(0, true), Expr::assign(1, false)),
            Expr::union(
                Expr::var("p".to_string()),
                Expr::star(Expr::var("p".to_string()))
            )
        ));
    }
    
    #[test]
    fn test_let_with_various_names() {
        // Let with various valid variable names
        let expr = parse_single_unwrap("let myVar = 1 in myVar");
        assert_eq!(expr, Expr::let_in(
            "myVar".to_string(),
            Expr::one(),
            Expr::var("myVar".to_string())
        ));
        
        let expr = parse_single_unwrap("let test_var = 0 in test_var");
        assert_eq!(expr, Expr::let_in(
            "test_var".to_string(),
            Expr::zero(),
            Expr::var("test_var".to_string())
        ));
        
        let expr = parse_single_unwrap("let v123 = x0 := 1 in v123");
        assert_eq!(expr, Expr::let_in(
            "v123".to_string(),
            Expr::assign(0, true),
            Expr::var("v123".to_string())
        ));
    }
    
    #[test]
    fn test_let_precedence() {
        // Let has low precedence, binds to the right
        let expr = parse_single_unwrap("0 + let x = 1 in x");
        assert_eq!(expr, Expr::union(
            Expr::zero(),
            Expr::let_in(
                "x".to_string(),
                Expr::one(),
                Expr::var("x".to_string())
            )
        ));
        
        // Parentheses override precedence
        let expr = parse_single_unwrap("(let x = 1 in x) + 0");
        assert_eq!(expr, Expr::union(
            Expr::let_in(
                "x".to_string(),
                Expr::one(),
                Expr::var("x".to_string())
            ),
            Expr::zero()
        ));
    }
    
    #[test]
    fn test_let_errors() {
        // Missing 'in' keyword
        assert!(parse_all("let x = 1 x").is_err());
        
        // Missing body
        assert!(parse_all("let x = 1 in").is_err());
        
        // Missing definition
        assert!(parse_all("let x = in 1").is_err());
        
        // Missing variable name
        assert!(parse_all("let = 1 in 0").is_err());
        
        // Invalid variable name
        assert!(parse_all("let 123 = 1 in 0").is_err());
    }
    
    #[test]
    fn test_bit_range_alias_parsing() {
        // Basic bit range alias
        let expr = parse_single_unwrap("let ip = &x[0..32] in ip := 192.168.1.1");
        // IP address 192.168.1.1 in little-endian bit representation
        // Using the same conversion as in test_literal_formats
        let mut ip_bits = vec![false; 32];
        let ip_num = 0xC0A80101u32; // 192.168.1.1 in hex
        for i in 0..32 {
            ip_bits[i] = (ip_num >> i) & 1 == 1;
        }
        assert_eq!(expr, Expr::let_bit_range(
            "ip".to_string(),
            0,
            32,
            Expr::var_assign("ip".to_string(), ip_bits)
        ));
        
        // Bit range alias with test
        let expr = parse_single_unwrap("let port = &x[32..48] in port == 80");
        let port_bits = vec![false, false, false, false, true, false, true]; // 80 in 7 bits (minimal)
        assert_eq!(expr, Expr::let_bit_range(
            "port".to_string(),
            32,
            48,
            Expr::var_test("port".to_string(), port_bits)
        ));
        
        // Multiple bit range aliases
        let expr = parse_single_unwrap("let src = &x[0..32] in let dst = &x[32..64] in src == 10.0.0.1 & dst == 10.0.0.2");
        let mut ip1_bits = vec![false; 32];
        let ip1_num = 0x0A000001u32; // 10.0.0.1 in hex
        for i in 0..32 {
            ip1_bits[i] = (ip1_num >> i) & 1 == 1;
        }
        let mut ip2_bits = vec![false; 32];
        let ip2_num = 0x0A000002u32; // 10.0.0.2 in hex
        for i in 0..32 {
            ip2_bits[i] = (ip2_num >> i) & 1 == 1;
        }
        assert_eq!(expr, Expr::let_bit_range(
            "src".to_string(),
            0,
            32,
            Expr::let_bit_range(
                "dst".to_string(),
                32,
                64,
                Expr::intersect(
                    Expr::var_test("src".to_string(), ip1_bits),
                    Expr::var_test("dst".to_string(), ip2_bits)
                )
            )
        ));
    }
    
    #[test]
    fn test_alias_with_regular_let() {
        // Mix of regular let and bit range alias
        let expr = parse_single_unwrap("let config = x0 := 1 in let ip = &x[0..32] in config ; ip := 192.168.1.1");
        let mut ip_bits = vec![false; 32];
        let ip_num = 0xC0A80101u32; // 192.168.1.1 in hex
        for i in 0..32 {
            ip_bits[i] = (ip_num >> i) & 1 == 1;
        }
        assert_eq!(expr, Expr::let_in(
            "config".to_string(),
            Expr::assign(0, true),
            Expr::let_bit_range(
                "ip".to_string(),
                0,
                32,
                Expr::sequence(
                    Expr::var("config".to_string()),
                    Expr::var_assign("ip".to_string(), ip_bits)
                )
            )
        ));
    }
    
    #[test]
    fn test_alias_errors() {
        // Missing ampersand
        assert!(parse_all("let ip = x[0..32] in ip := 1").is_err());
        
        // Missing brackets
        assert!(parse_all("let ip = &x in ip := 1").is_err());
        
        // Missing range
        assert!(parse_all("let ip = &x[] in ip := 1").is_err());
        
        // Invalid range syntax
        assert!(parse_all("let ip = &x[0-32] in ip := 1").is_err());
        
        // Non-x field reference
        assert!(parse_all("let ip = &y[0..32] in ip := 1").is_err());
        
        // Missing 'in' keyword
        assert!(parse_all("let ip = &x[0..32] ip := 1").is_err());
    }
    
    #[test]
    fn test_alias_precedence() {
        // Alias in larger expression
        let expr = parse_single_unwrap("0 + let ip = &x[0..32] in ip == 10.0.0.1");
        let mut ip_bits = vec![false; 32];
        let ip_num = 0x0A000001u32; // 10.0.0.1 in hex
        for i in 0..32 {
            ip_bits[i] = (ip_num >> i) & 1 == 1;
        }
        assert_eq!(expr, Expr::union(
            Expr::zero(),
            Expr::let_bit_range(
                "ip".to_string(),
                0,
                32,
                Expr::var_test("ip".to_string(), ip_bits)
            )
        ));
    }
}

// Temporary function to satisfy lib.rs if it's calling something specific not yet refactored.
// pub fn old_tokenize_placeholder(input: &str) -> Result<Vec<super::Token>, String> {
//     Err("Tokenizer placeholder".to_string())
// }
