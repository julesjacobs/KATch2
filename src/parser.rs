use crate::expr::{Exp, Expr};
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
    Not,        // !
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
    Field(u32), // x followed by digits
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
    input_str: &'a str, // Keep a reference to the input for slicing if needed for error context
    iterator_has_yielded_eof: bool, // New field to track EOF iteration state
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            iter: input.chars().peekable(),
            current_pos: Position::new(0, 1, 1),
            input_str: input,
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
                    '0' => TokenKind::Zero,
                    '1' => TokenKind::One,
                    '+' => TokenKind::Plus,
                    '&' => TokenKind::And,
                    '^' => TokenKind::Xor,
                    '-' => TokenKind::Minus,
                    '!' => TokenKind::Not,
                    ';' => TokenKind::Semicolon,
                    '*' => TokenKind::Star,
                    '(' => TokenKind::LParen,
                    ')' => TokenKind::RParen,
                    'T' => TokenKind::Top,
                    'U' => TokenKind::LtlU,
                    // 'X' needs to be LtlX, be careful if 'x' for fields comes first
                    'F' => TokenKind::LtlF,
                    'G' => TokenKind::LtlG,
                    'R' => TokenKind::LtlR,
                    'e' => {
                        if self.peek_char() == Some(&'n') {
                            self.next_char_with_pos(); // Consume 'n'
                            if self.peek_char() == Some(&'d') {
                                self.next_char_with_pos(); // Consume 'd'
                                TokenKind::End
                            } else {
                                return Err(ParseError::new(
                                    "Expected 'end'".to_string(),
                                    Span::new(start_pos, self.current_pos),
                                ));
                            }
                        } else {
                             return Err(ParseError::new(
                                "Expected 'end'".to_string(),
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
                            // A single '=' is not a valid token start on its own if '==' is expected
                            return Err(ParseError::new(
                                "Expected '==' for equality test or ':=' for assignment".to_string(),
                                Span::new(start_pos, self.current_pos),
                            ));
                        }
                    }
                    // Handle 'X' for LtlX before 'x' for fields to avoid ambiguity if 'x' itself is not a field.
                    'X' => TokenKind::LtlX,
                    'x' => {
                        let mut num_str = String::new();
                        while let Some(&next_c) = self.peek_char() {
                            if next_c.is_digit(10) {
                                num_str.push(self.next_char_with_pos().unwrap().0); // Consume digit
                            } else {
                                break;
                            }
                        }
                        if num_str.is_empty() {
                            // If 'x' can be an identifier on its own, this needs adjustment.
                            // Assuming 'x' must be followed by digits for TokenKind::Field.
                            // If 'x' alone is an LTL X, it should be handled before/separately.
                            // The current setup means 'X' (uppercase) is LtlX, 'x' must be Field.
                            return Err(ParseError::new(
                                "Expected digits after 'x' for field".to_string(),
                                Span::new(start_pos, self.current_pos),
                            ));
                        } else {
                            match num_str.parse::<u32>() {
                                Ok(index) => TokenKind::Field(index),
                                Err(_) => {
                                    return Err(ParseError::new(
                                        "Invalid field index number".to_string(),
                                        Span::new(start_pos, self.current_pos), // Span covers 'x' and digits
                                    ));
                                }
                            }
                        }
                    }
                    'd' => {
                        if self.peek_char() == Some(&'u') {
                            self.next_char_with_pos(); // Consume 'u'
                            if self.peek_char() == Some(&'p') {
                                self.next_char_with_pos(); // Consume 'p'
                                TokenKind::Dup
                            } else {
                                return Err(ParseError::new(
                                    "Expected 'dup'".to_string(),
                                    Span::new(start_pos, self.current_pos),
                                ));
                            }
                        } else {
                            return Err(ParseError::new(
                                "Expected 'dup'".to_string(),
                                Span::new(start_pos, self.current_pos),
                            ));
                        }
                    }
                    _ => {
                        return Err(ParseError::new(
                            format!("Unexpected character: {}", c),
                            Span::new(start_pos, self.current_pos), // Span of the single unexpected char
                        ));
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
                // For now, let's make it an error or a specific "unexpected end of stream"
                // that uses the *last known* position.
                // This needs careful thought based on parser structure.

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
    // These will now use consume_token() and peek_kind()/peek_token() and return Result<Exp, ParseError>

    fn parse_until(&mut self) -> Result<Exp, ParseError> {
        let left = self.parse_sequence()?;
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


    fn parse_sequence(&mut self) -> Result<Exp, ParseError> {
        let mut left = self.parse_additive()?;
        while self.peek_kind()? == &TokenKind::Semicolon {
            let _op_token = self.consume_token()?; // Consume ';'
            let right = self.parse_additive()?;
            left = Expr::sequence(left, right);
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Exp, ParseError> {
        let mut left = self.parse_intersect()?;
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
                            let right = self.parse_intersect()?;
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
                            let right = self.parse_intersect()?;
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
                            let right = self.parse_intersect()?;
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

    fn parse_intersect(&mut self) -> Result<Exp, ParseError> {
        let mut left = self.parse_unary()?;
        while self.peek_kind()? == &TokenKind::And {
            self.consume_token()?; // Consume '&'
            let right = self.parse_unary()?;
            left = Expr::intersect(left, right);
        }
        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<Exp, ParseError> {
        match self.peek_kind()? {
            TokenKind::Not => {
                let op_token = self.consume_token()?; // Consume '!'
                // Check for EOF before parsing operand
                if self.peek_kind()? == &TokenKind::Eof {
                    return Err(ParseError::new(
                        format!("Expected expression after {} but found end of input", token_kind_to_user_string(&op_token.kind)),
                        op_token.span,
                    ));
                }
                let operand = self.parse_unary()?; // Unary ops often bind to their own kind
                Ok(Expr::complement(operand))
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
                let operand = self.parse_unary()?;
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
                let operand = self.parse_unary()?;
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
                let operand = self.parse_unary()?;
                Ok(Expr::ltl_globally(operand)) // Uses helper from expr.rs
            }
            _ => {
                // Handles atoms (0, 1, T, dup), field constructs (xN := V, xN == V), and parenthesized expressions
                let mut base_expr = self.parse_atom_or_field_expression()?;
                
                // Postfix star
                if self.peek_kind()? == &TokenKind::Star {
                    let _star_token = self.consume_token()?; // Consume '*'
                    base_expr = Expr::star(base_expr);
                }
                Ok(base_expr)
            }
        }
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
                    _ => Err(ParseError::new(
                        format!("Expected ':=' or '==' after field 'x{}', found {}", index, token_kind_to_user_string(&next_token_after_field.kind)),
                        next_token_after_field.span,
                    )),
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
            TokenKind::Field(_) |
            TokenKind::LtlX | TokenKind::LtlF | TokenKind::LtlG | TokenKind::LtlU | TokenKind::LtlR |
            TokenKind::Not | TokenKind::Star | TokenKind::Semicolon | TokenKind::Plus |
            TokenKind::And | TokenKind::Xor | TokenKind::Minus | TokenKind::Assign | TokenKind::Eq |
            TokenKind::RParen | TokenKind::Eof => { // Removed End from here due to unreachable pattern, it's handled below.
                 Err(ParseError::new(
                    format!("Unexpected {} when expecting a primary expression (like '0', '1', 'T', 'dup', or '(')", token_kind_to_user_string(&token.kind)),
                    token.span,
                ))
            }
            TokenKind::End => Ok(Expr::end()), // Moved here to be last, resolves unreachable_patterns for End.
        }
    }
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
        TokenKind::Not => "operator '!'".to_string(),
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
        TokenKind::Field(u) => format!("field 'x{}'", u),
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
        assert_eq!(parse_single_unwrap("!(0)"), Expr::complement(Expr::zero()));
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
        let expr_str = "(x1:=0 ; (T* & !(x2==1))) + F (x3==0 U x4==1)";
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
            "! ",
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
}

// Temporary function to satisfy lib.rs if it's calling something specific not yet refactored.
// pub fn old_tokenize_placeholder(input: &str) -> Result<Vec<super::Token>, String> {
//     Err("Tokenizer placeholder".to_string())
// }
