use medic_ast::ast::StatementNode;
use medic_codegen_llvm::generate_ir_string;
use medic_lexer::streaming_lexer::StreamingLexer;
use medic_lexer::token::Token;
use medic_parser::parser::{parse_program, TokenSlice};

fn main() {
    // Minimal reproduction of the failing test
    let src = r#"
fn calc(a: int, b: int) -> int {
  let x = a * b;
  return x
}
"#;
    
    let lx = StreamingLexer::new(src);
    let tokens: Vec<Token> = lx.collect();
    let input = TokenSlice::new(&tokens);
    let (_rest, program) = parse_program(input).expect("parse ok");
    
    match generate_ir_string(&program) {
        Ok(ir) => {
            println!("SUCCESS: IR generated");
            println!("{}", ir);
        },
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }
}
