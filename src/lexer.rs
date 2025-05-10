// Lexer for Medi language using the industry-standard 'logos' crate
// Recognizes healthcare keywords, identifiers, literals, operators, and delimiters

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // Keywords (add more healthcare-specific keywords as needed)
    #[token("patient")] Patient,
    #[token("observation")] Observation,
    #[token("medication")] Medication,
    #[token("fhir_query")] FhirQuery,
    #[token("kaplan_meier")] KaplanMeier,
    #[token("regulate")] Regulate,
    #[token("report")] Report,
    #[token("let")] Let,
    #[token("fn")] Fn,
    #[token("true")] True,
    #[token("false")] False,
    
    // Identifiers (variables, function names, medical terms)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")] Identifier,
    
    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())] IntLiteral(i64),
    #[regex(r#""[^"]*""#)] StringLiteral,
    
    // Operators
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("/")] Slash,
    #[token("%") ] Percent,
    #[token("=")] Assign,
    #[token("==")] EqEq,
    #[token("!=")] Neq,
    #[token("<")] Lt,
    #[token(">")] Gt,
    #[token("<=")] Le,
    #[token(">=")] Ge,
    #[token("&&")] AndAnd,
    #[token("||")] OrOr,
    
    // Delimiters
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("{")] LBrace,
    #[token("}")] RBrace,
    #[token(",")] Comma,
    #[token(";")] Semicolon,
    #[token(".")] Dot,
    
    // Whitespace (skipped)
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Error,
}
