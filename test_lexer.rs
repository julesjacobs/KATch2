use katch2::parser::{Lexer, TokenKind};

fn main() {
    let input = "x[0..8] := 255";
    let lexer = Lexer::new(input);
    
    for token_result in lexer {
        match token_result {
            Ok(token) => println\!("{:?}", token.kind),
            Err(e) => println\!("Error: {:?}", e),
        }
    }
}
EOF < /dev/null