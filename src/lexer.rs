// Lexer for Medi language using the industry-standard 'logos' crate
// Recognizes healthcare keywords, identifiers, literals, operators, and delimiters

use logos::Logos;

#[derive(Logos, Debug, PartialEq, Clone)]
pub enum Token {
    // --- Healthcare-specific keywords ---
    #[token("patient")] Patient,
    #[token("observation")] Observation,
    #[token("medication")] Medication,
    #[token("fhir_query")] FhirQuery,
    #[token("kaplan_meier")] KaplanMeier,
    #[token("regulate")] Regulate,
    #[token("report")] Report,

    // --- General keywords ---
    #[token("let")] Let,
    #[token("fn")] Fn,
    #[token("if")] If,
    #[token("else")] Else,
    #[token("elif")] Elif,
    #[token("for")] For,
    #[token("while")] While,
    #[token("return")] Return,
    #[token("match")] Match,
    #[token("case")] Case,
    #[token("yield")] Yield,
    #[token("await")] Await,
    #[token("async")] Async,
    #[token("in")] In,
    #[token("not")] Not,
    #[token("and")] And,
    #[token("or")] Or,
    #[token("is")] Is,
    #[token("as")] As,
    #[token("from")] From,
    #[token("import")] Import,
    #[token("with")] With,
    #[token("try")] Try,
    #[token("except")] Except,
    #[token("raise")] Raise,
    #[token("finally")] Finally,
    #[token("pass")] Pass,
    #[token("del")] Del,
    #[token("global")] Global,
    #[token("const")] Const,
    #[token("static")] Static,
    #[token("struct")] Struct,
    #[token("enum")] Enum,
    #[token("impl")] Impl,
    #[token("trait")] Trait,
    #[token("type")] Type,
    #[token("mod")] Mod,
    #[token("pub")] Pub,
    #[token("export")] Export,

    // --- Identifiers ---
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")] Identifier,

    // --- Literals ---
    #[regex(r"0b[01_]+", |lex| i64::from_str_radix(&lex.slice()[2..].replace('_', ""), 2).ok())] BinLiteral(i64),
    #[regex(r"0o[0-7_]+", |lex| i64::from_str_radix(&lex.slice()[2..].replace('_', ""), 8).ok())] OctLiteral(i64),
    #[regex(r"0x[0-9a-fA-F_]+", |lex| i64::from_str_radix(&lex.slice()[2..].replace('_', ""), 16).ok())] HexLiteral(i64),
    #[regex(r"[0-9][0-9_]*", |lex| lex.slice().replace('_', "").parse().ok())] IntLiteral(i64),
    #[regex(r"[0-9][0-9_]*\.[0-9_]+([eE][+-]?[0-9_]+)?", |lex| lex.slice().replace('_', "").parse().ok())] FloatLiteral(f64),
    #[regex(r#"'([^'\\]|\\.)*'"#, |lex| lex.slice().to_string())] SingleQuotedString(String),
    #[regex(r#"\"([^"\\]|\\.)*\""#, |lex| lex.slice().to_string())] DoubleQuotedString(String),
    #[regex(r#"r#?\"([^"\\]|\\.)*\""#, |lex| lex.slice().to_string())] RawString(String),
    #[regex(r#"b\"([^"\\]|\\.)*\""#, |lex| lex.slice().to_string())] ByteString(String),
    #[token("true")] True,
    #[token("false")] False,
    #[token("null")] Null,
    #[token("None")] NoneVal,

    // --- Operators ---
    #[token("+")] Plus,
    #[token("-")] Minus,
    #[token("*")] Star,
    #[token("**")] DoubleStar,
    #[token("/")] Slash,
    #[token("//")] DoubleSlash,
    #[token("%") ] Percent,
    #[token("=")] Assign,
    #[token("+=")] PlusAssign,
    #[token("-=")] MinusAssign,
    #[token("*=")] StarAssign,
    #[token("/=")] SlashAssign,
    #[token("%=")] PercentAssign,
    #[token("**=")] DoubleStarAssign,
    #[token("//=")] DoubleSlashAssign,
    #[token("==")] EqEq,
    #[token("!=")] Neq,
    #[token("<")] Lt,
    #[token(">")] Gt,
    #[token("<=")] Le,
    #[token(">=")] Ge,
    #[token("&&")] AndAnd,
    #[token("||")] OrOr,
    #[token("!")] Bang,
    #[token("&")] BitAnd,
    #[token("|")] BitOr,
    #[token("^")] BitXor,
    #[token("~")] BitNot,
    #[token("<<")] Shl,
    #[token(">>")] Shr,
    #[token("->")] Arrow,
    #[token(":")] Colon,
    #[token(";")] Semicolon,
    #[token(".")] Dot,
    #[token(",")] Comma,

    // --- Delimiters ---
    #[token("(")] LParen,
    #[token(")")] RParen,
    #[token("[")] LBracket,
    #[token("]")] RBracket,
    #[token("{")] LBrace,
    #[token("}")] RBrace,
    #[token("@") ] At,
    #[token("$")] Dollar,
    #[token("\\")] Backslash,
    #[token("_")] Underscore,

    // --- Whitespace and comments (skipped) ---
    #[regex(r"[ \t\n\r]+", logos::skip)]
    #[regex(r"//[^\n]*", logos::skip)]
    #[regex(r"#.*", logos::skip)]
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]
    #[regex(r"'''(.|\n)*?'''", logos::skip)]
    #[regex(r#"""(.|\n)*?"""#, logos::skip)]

    // --- Error ---
    Error,
    
    // Identifiers (variables, function names, medical terms)
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")] Identifier,
    
    // Literals
    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())] IntLiteral(i64),
    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())] FloatLiteral(f64),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().to_string())] StringLiteral(String),
    #[token("true")] True,
    #[token("false")] False,
    
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

    // Comments (skipped)
    #[regex(r"//[^
]*", logos::skip)]
    #[regex(r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", logos::skip)]

    // Error token for unrecognized input
    Error,
}
