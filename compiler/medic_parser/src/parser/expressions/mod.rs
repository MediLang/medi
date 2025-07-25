//! # Expression Parser
//!
//! This module handles the parsing of expressions in the Medi language, including:
//! - Primary expressions (literals, identifiers, parenthesized expressions)
//! - Binary expressions with operator precedence and associativity
//! - Match expressions
//! - Block expressions
//!
//! ## Operator Precedence
//!
//! Operators are listed in order of increasing precedence:
//!
//! | Operator(s) | Associativity | Description           |
//! |-------------|---------------|-----------------------|
//! | `||`        | Left          | Logical OR            |
//! | `&&`        | Left          | Logical AND           |
//! | `==`, `!=`  | Left          | Equality comparison   |
//! | `<`, `>`, `<=`, `>=` | Left  | Relational comparison |
//! | `+`, `-`    | Left          | Addition/Subtraction  |
//! | `*`, `/`, `%` | Left        | Multiplication/Division/Modulo |
//! | `**`        | Right         | Exponentiation        |
//! | `of`, `per` | Left          | Medical operators     |
//! | `!`, `-`    | Right         | Unary operators       |
//!
//! ## Expression Grammar
//!
//! ```ebnf
//! expression     → assignment ;
//! assignment     → IDENTIFIER "=" assignment
//!                | logic_or ;
//! logic_or       → logic_and ( "||" logic_and )* ;
//! logic_and      → equality ( "&&" equality )* ;
//! equality       → comparison ( ( "!=" | "==" ) comparison )* ;
//! comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
//! term           → factor ( ( "-" | "+" ) factor )* ;
//! factor         → unary ( ( "/" | "*" | "%" ) unary )* ;
//! unary          → ( "!" | "-" ) unary | primary ;
//! primary        → NUMBER | STRING | "true" | "false" | "nil"
//!                | "(" expression ")"
//!                | IDENTIFIER
//!                | "match" expression "{" match_arm* "}" ;
//! match_arm      → pattern "=>" expression ","? ;
//! pattern        → IDENTIFIER | "_" | literal ;
//! ```

use nom::error::ErrorKind;
use nom::Err;
use nom::IResult;

use crate::parser::{
    get_binary_operator, get_operator_precedence, is_comparison_operator, parse_block,
    take_token_if, BinaryExpressionNode, BinaryOperator, BlockNode, ExpressionNode, IdentifierNode,
    LiteralNode, MatchArmNode, MatchNode, PatternNode, StatementNode, TokenSlice, TokenType,
};

use super::{identifiers::parse_identifier, literals::parse_literal};

use medic_ast::ast::ExpressionNode as AstExpressionNode;
use medic_ast::ast::MatchNode as AstMatchNode;
use medic_ast::ast::StatementNode as AstStatementNode;

/// Parses a single match arm in the format: <pattern> => <expression>
fn parse_match_arm(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, MatchArmNode> {
    log::debug!("=== parse_match_arm ===");
    log::debug!("Input length: {}", input.0.len());

    // Parse the pattern
    let (input, pattern) = parse_pattern(input)?;

    // Skip whitespace before the fat arrow
    let input = input.skip_whitespace();

    // Parse the fat arrow
    let (input, _) = take_token_if(|tt| *tt == TokenType::FatArrow, ErrorKind::Tag)(input)?;

    // Skip whitespace after the fat arrow
    let input = input.skip_whitespace();

    // Parse the expression
    let (input, expr) = parse_expression(input)?;

    // Create and return the match arm
    Ok((
        input,
        MatchArmNode {
            pattern,
            body: Box::new(expr),
        },
    ))
}

/// Parses a pattern in a match arm
fn parse_pattern(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, PatternNode> {
    log::debug!("=== parse_pattern ===");
    log::debug!("Input length: {}", input.0.len());

    if input.0.is_empty() {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    // Check for wildcard pattern (_)
    if let TokenType::Underscore = input.0[0].token_type {
        return Ok((input.advance(), PatternNode::Wildcard));
    }

    // Check for literal pattern (numbers, strings, booleans)
    if let TokenType::Integer(n) = input.0[0].token_type {
        return Ok((input.advance(), PatternNode::Literal(LiteralNode::Int(n))));
    }

    if let TokenType::Float(n) = input.0[0].token_type {
        return Ok((input.advance(), PatternNode::Literal(LiteralNode::Float(n))));
    }

    if let TokenType::String(s) = &input.0[0].token_type {
        return Ok((
            input.advance(),
            PatternNode::Literal(LiteralNode::String(s.to_string())),
        ));
    }

    if let TokenType::Boolean(b) = input.0[0].token_type {
        return Ok((input.advance(), PatternNode::Literal(LiteralNode::Bool(b))));
    }

    // Check for identifier pattern
    if let TokenType::Identifier(ident) = &input.0[0].token_type {
        // Create a simple identifier node without span for now
        // The AST will handle the span during a later phase
        let ident_node = IdentifierNode::new(ident.to_string());
        return Ok((input.advance(), PatternNode::Identifier(ident_node)));
    }

    // If no pattern matched, return an error
    Err(nom::Err::Error(nom::error::Error::new(
        input,
        ErrorKind::Tag,
    )))
}

/// Parses a match expression in expression context (e.g., `match x { 1 => 1, _ => 0 }`).
/// Also handles the concise syntax: `x { 1 => 1, _ => 0 }`
///
/// This function parses both the full `match` syntax and the concise syntax.
pub fn parse_match_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_match_expression ===");
    log::debug!("Input length: {}", input.0.len());

    // Log the input tokens for debugging
    if !input.0.is_empty() {
        log::debug!(
            "Next token: {:?} at {}:{}",
            input.0[0].token_type,
            input.0[0].location.line,
            input.0[0].location.column
        );

        // Log more tokens for context
        let num_tokens = input.0.len().min(5);
        log::debug!("Next {} tokens:", num_tokens);
        for i in 0..num_tokens {
            log::debug!(
                "  {}: {:?} at {}:{}",
                i,
                input.0[i].token_type,
                input.0[i].location.line,
                input.0[i].location.column
            );
        }
    }

    // Check if this is the full match syntax or the concise syntax
    let (input, expr) = if input
        .0
        .get(0)
        .map_or(false, |t| t.token_type == TokenType::Match)
    {
        // Full match syntax: match <expr> { ... }
        log::debug!("Parsing full match syntax");
        let (input, _) = take_token_if(|tt| *tt == TokenType::Match, ErrorKind::Tag)(input)?;
        let input = input.skip_whitespace();
        let (input, expr) = parse_expression(input)?;
        (input, expr)
    } else if input
        .0
        .get(1)
        .map_or(false, |t| t.token_type == TokenType::LeftBrace)
    {
        // Concise syntax: <expr> { ... }
        log::debug!("Parsing concise match syntax");
        // Parse the expression before the left brace
        // We need to parse up to but not including the left brace
        let expr_end = input
            .0
            .iter()
            .position(|t| t.token_type == TokenType::LeftBrace)
            .unwrap_or(input.0.len());
        let (expr_input, rest) = input.0.split_at(expr_end);
        let (_, expr) = parse_expression(TokenSlice(expr_input))?;
        // Return the rest of the input (starting with the left brace)
        (TokenSlice(rest), expr)
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    // Skip whitespace before the opening brace
    let input = input.skip_whitespace();

    // Parse the opening brace
    let (mut input, _) = take_token_if(|tt| *tt == TokenType::LeftBrace, ErrorKind::Tag)(input)?;

    // Parse match arms
    let mut arms = Vec::new();
    let mut input = input.skip_whitespace();

    // Keep parsing arms until we hit the closing brace
    while !input.0.is_empty() {
        // Check for closing brace
        if let Some((first, _)) = input.0.split_first() {
            if first.token_type == TokenType::RightBrace {
                input = input.advance();
                break;
            }
        }

        // Parse a single arm
        let (new_input, arm) = parse_match_arm(input)?;
        arms.push(arm);
        input = new_input.skip_whitespace();

        // Check for comma separator
        if let Some((first, rest)) = input.0.split_first() {
            if first.token_type == TokenType::Comma {
                input = TokenSlice(rest);
                input = input.skip_whitespace();
            } else if first.token_type != TokenType::RightBrace {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )));
            }
        }
    }

    // Create a match statement node
    let match_stmt = StatementNode::Match(Box::new(MatchNode {
        expr: Box::new(expr),
        arms,
    }));

    // Convert the statement to an expression
    let match_expr = ExpressionNode::from_statement(match_stmt);

    Ok((input, match_expr))
}

// Re-export nested expressions API
pub mod nested;
pub mod struct_literal;

pub use nested::parse_nested_binary_expression;
pub use struct_literal::parse_struct_literal;

// Note: Block expressions are parsed using the `parse_block` function from the parent module.
// This is the primary and recommended way to parse block expressions. The function handles
// all aspects of block parsing including statements, semicolons, and scoping.
//
// Example usage:
// ```rust
// let (remaining_input, block) = parse_block(input_tokens)?;
// let expr = ExpressionNode::from_statement(StatementNode::Block(block));
// ```

/// Parses an expression, which can be a binary expression, primary expression, or block expression.
///
/// This function handles the parsing of all expression types in the language, including:
/// - Primary expressions (literals, identifiers, parenthesized expressions)
/// - Binary expressions with operator precedence and associativity
/// - Match expressions
/// - Special handling for medical operators 'of' and 'per'
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_expression ===");
    log::debug!("Input length: {}", input.0.len());

    // Log the first few tokens for better context
    if !input.0.is_empty() {
        let num_tokens = input.0.len().min(5);
        log::debug!("Next {} tokens in parse_expression:", num_tokens);
        for (i, token) in input.0.iter().take(num_tokens).enumerate() {
            log::debug!(
                "  {}: {:?} at {}:{}",
                i,
                token.token_type,
                token.location.line,
                token.location.column
            );
        }

        // Don't handle match expressions here - let parse_match_statement handle them
        // This prevents parse_expression from consuming match expressions at the statement level

        // Log if we see an identifier at the start
        if let TokenType::Identifier(_) = input.0[0].token_type {
            log::debug!("Found identifier at start: {:?}", input.0[0].token_type);
        }
    } else {
        log::debug!("No input tokens in parse_expression");
    }

    // If this is a match expression at the statement level, don't consume it here
    if !input.0.is_empty() && input.0[0].token_type == TokenType::Match {
        log::debug!(
            "Found match expression at statement level, deferring to parse_match_statement"
        );
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    // Use the precedence climbing parser for the entire expression
    log::debug!("Attempting to parse as binary expression");
    log::debug!("Input tokens before parse_nested_binary_expression:");
    for (i, token) in input.0.iter().take(5).enumerate() {
        log::debug!(
            "  {}: {:?} at {}:{}",
            i,
            token.token_type,
            token.location.line,
            token.location.column
        );
    }

    let result = parse_nested_binary_expression(input, 0, false);

    log::debug!("parse_nested_binary_expression result: {:?}", result);
    if let Err(e) = &result {
        log::error!("Failed to parse binary expression: {:?}", e);
    }

    match &result {
        Ok((remaining, expr)) => {
            log::debug!("parse_expression success, parsed: {:?}", expr);
            log::debug!("Remaining tokens: {}", remaining.0.len());

            if !remaining.0.is_empty() {
                log::debug!(
                    "Next token after expression: {:?} at {}:{}",
                    remaining.0[0].token_type,
                    remaining.0[0].location.line,
                    remaining.0[0].location.column
                );

                // Check for match expression after binary expression
                if remaining.0[0].token_type == TokenType::Match {
                    log::debug!("Found match expression after binary expression");
                    // Parse the remaining as a match expression and combine with the binary expression
                    let (remaining, match_expr) = parse_match_expression(*remaining)?;
                    // Convert the previous result to a binary expression with the match expression as the right operand
                    if let Ok((_, left_expr)) = &result {
                        return Ok((
                            remaining,
                            ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                                left: left_expr.clone(),
                                operator: BinaryOperator::Mul, // Default to multiplication for now
                                right: match_expr,
                            })),
                        ));
                    }
                }
            }

            // Return the original result if no match expression follows
            result
        }
        Err(e) => {
            log::error!("parse_expression failed: {:?}", e);

            // If binary expression parsing failed, try parsing as a match expression
            if input
                .0
                .first()
                .map_or(false, |t| t.token_type == TokenType::Match)
            {
                log::debug!("Trying to parse as match expression after binary expression failed");
                return parse_match_expression(input);
            }

            // If we get here, return the original error
            result
        }
    }
}

/// Parses a binary expression with special handling for 'of' and 'per' operators.
///
/// This is a helper function that wraps the standard binary expression parser
/// to handle the special precedence rules for 'of' and 'per'.
pub fn parse_binary_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // First, parse the left-hand side
    let (mut input, mut left) = parse_primary(input)?;

    // Check for 'of' or 'per' operators with special handling
    if !input.0.is_empty() {
        match input.0[0].token_type {
            TokenType::Of => {
                // Consume the 'of' token
                input = input.advance();

                // Parse the right-hand side with higher precedence for 'of'
                // This ensures '2 of 3 * doses' is parsed as '2 of (3 * doses)'
                let (new_input, right) = parse_nested_binary_expression(input, 1, false)?;
                input = new_input;

                // Create the 'of' expression with proper span
                let start_span = match &left {
                    ExpressionNode::Identifier(i) => i.span,
                    ExpressionNode::Literal(l) => l.span,
                    ExpressionNode::Binary(b) => b.span,
                    _ => Span::default(),
                };

                let end_span = match &right {
                    ExpressionNode::Identifier(i) => i.span,
                    ExpressionNode::Literal(l) => l.span,
                    ExpressionNode::Binary(b) => b.span,
                    _ => Span::default(),
                };

                let span = Span {
                    start: start_span.start,
                    end: end_span.end,
                    line: start_span.line,
                    column: start_span.column,
                };

                let bin_expr = BinaryExpressionNode {
                    left,
                    operator: BinaryOperator::Of,
                    right,
                };

                left = ExpressionNode::Binary(Spanned::new(Box::new(bin_expr), span));

                // Continue parsing any remaining binary expressions
                let (new_input, expr) = parse_nested_binary_expression(input, 0, false)?;
                input = new_input;

                // If we parsed more operators, combine with the 'of' expression
                if let ExpressionNode::Binary(bin) = expr {
                    let start_span = match &left {
                        ExpressionNode::Binary(b) => b.span,
                        _ => span,
                    };

                    let end_span = match &bin.right {
                        ExpressionNode::Identifier(i) => i.span,
                        ExpressionNode::Literal(l) => l.span,
                        ExpressionNode::Binary(b) => b.span,
                        _ => span,
                    };

                    let combined_span = Span {
                        start: start_span.start,
                        end: end_span.end,
                        line: start_span.line,
                        column: start_span.column,
                    };

                    let combined_bin = BinaryExpressionNode {
                        left,
                        operator: bin.operator,
                        right: bin.right,
                    };

                    left =
                        ExpressionNode::Binary(Spanned::new(Box::new(combined_bin), combined_span));
                }
            }
            TokenType::Per => {
                // Consume the 'per' token
                input = input.advance();

                // Parse the right-hand side with higher precedence for 'per'
                // This ensures '5 * mg per day' is parsed as '(5 * mg) per day'
                let (new_input, right) = parse_nested_binary_expression(input, 1, false)?;
                input = new_input;

                // Create the 'per' expression with proper span
                let start_span = match &left {
                    ExpressionNode::Identifier(i) => i.span,
                    ExpressionNode::Literal(l) => l.span,
                    ExpressionNode::Binary(b) => b.span,
                    _ => Span::default(),
                };

                let end_span = match &right {
                    ExpressionNode::Identifier(i) => i.span,
                    ExpressionNode::Literal(l) => l.span,
                    ExpressionNode::Binary(b) => b.span,
                    _ => Span::default(),
                };

                let span = Span {
                    start: start_span.start,
                    end: end_span.end,
                    line: start_span.line,
                    column: start_span.column,
                };

                let bin_expr = BinaryExpressionNode {
                    left,
                    operator: BinaryOperator::Per,
                    right,
                };

                left = ExpressionNode::Binary(Spanned::new(Box::new(bin_expr), span));

                // Continue parsing any remaining binary expressions
                let (new_input, expr) = parse_nested_binary_expression(input, 0, false)?;
                input = new_input;

                // If we parsed more operators, combine with the 'per' expression
                if let ExpressionNode::Binary(bin) = expr {
                    let start_span = match &left {
                        ExpressionNode::Binary(b) => b.span,
                        _ => span,
                    };

                    let end_span = match &bin.right {
                        ExpressionNode::Identifier(i) => i.span,
                        ExpressionNode::Literal(l) => l.span,
                        ExpressionNode::Binary(b) => b.span,
                        _ => span,
                    };

                    let combined_span = Span {
                        start: start_span.start,
                        end: end_span.end,
                        line: start_span.line,
                        column: start_span.column,
                    };

                    let combined_bin = BinaryExpressionNode {
                        left,
                        operator: bin.operator,
                        right: bin.right,
                    };

                    left =
                        ExpressionNode::Binary(Spanned::new(Box::new(combined_bin), combined_span));
                }
            }
            _ => {
                // For other operators, use the standard precedence climbing
                let (new_input, expr) = parse_nested_binary_expression(input, 0, false)?;
                input = new_input;

                // If we parsed a binary expression, combine it with the left-hand side
                if let ExpressionNode::Binary(bin) = expr {
                    let start_span = match &left {
                        ExpressionNode::Identifier(i) => i.span,
                        ExpressionNode::Literal(l) => l.span,
                        ExpressionNode::Binary(b) => b.span,
                        _ => Span::default(),
                    };

                    let end_span = match &bin.right {
                        ExpressionNode::Identifier(i) => i.span,
                        ExpressionNode::Literal(l) => l.span,
                        ExpressionNode::Binary(b) => b.span,
                        _ => start_span,
                    };

                    let combined_span = Span {
                        start: start_span.start,
                        end: end_span.end,
                        line: start_span.line,
                        column: start_span.column,
                    };

                    let combined_bin = BinaryExpressionNode {
                        left,
                        operator: bin.operator,
                        right: bin.right,
                    };

                    left =
                        ExpressionNode::Binary(Spanned::new(Box::new(combined_bin), combined_span));
                } else {
                    left = expr;
                }
            }
        }
    }

    Ok((input, left))
}

/// Parses a primary expression, which is an expression that can appear as an operand in other expressions.
/// This includes literals, identifiers, parenthesized expressions, and block expressions.
pub fn parse_primary(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    if log::log_enabled!(log::Level::Debug) {
        log::debug!("=== parse_primary ===");
        log::debug!("Input length: {}", input.0.len());
    }

    if input.0.is_empty() {
        if log::log_enabled!(log::Level::Error) {
            log::error!("parse_primary: Empty input");
        }
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    if log::log_enabled!(log::Level::Debug) {
        log::debug!("Next token: {:?}", input.0[0].token_type);
    }

    let token_type = &input.0[0].token_type;
    log::debug!("parse_primary: Processing token type: {:?}", token_type);

    match token_type {
        // Handle match expressions first - check for 'match' keyword
        TokenType::Match => {
            log::debug!("Found 'match' keyword, parsing match expression");
            return parse_match_expression(input);
        }
        // Handle identifiers that might be part of a match expression or a regular identifier
        TokenType::Identifier(_) => {
            // First, check if this is followed by a left brace, indicating a match expression
            if input.0.len() > 1 && input.0[1].token_type == TokenType::LeftBrace {
                log::debug!("Found identifier followed by '{{', treating as match expression");
                // This is a match expression, so we need to handle it as a statement
                return parse_match_expression(input);
            }

            // Otherwise, parse as a regular identifier
            let (new_input, expr) = parse_identifier(input)?;

            // Return the parsed identifier
            match expr {
                ExpressionNode::Identifier(ident) => {
                    return Ok((new_input, ExpressionNode::Identifier(ident)));
                }
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        ErrorKind::Tag,
                    )));
                }
            }
        }
        // Handle literals
        TokenType::Integer(_) | TokenType::Float(_) => {
            if log::log_enabled!(log::Level::Debug) {
                log::debug!(
                    "Parsing number literal: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
                log::debug!("Remaining tokens: {}", input.0.len());
            }
            let (mut input, lit) = parse_literal(input)?;

            // Check for implicit multiplication with an identifier (e.g., "5 mg")
            if !input.0.is_empty() {
                if let TokenType::Identifier(_) = input.0[0].token_type {
                    let (new_input, right) = parse_identifier(input)?;
                    input = new_input;
                    // Create a Spanned BinaryExpressionNode
                    let bin_expr = BinaryExpressionNode {
                        left: ExpressionNode::Literal(Spanned::new(lit, lit.span)),
                        operator: BinaryOperator::Mul,
                        right,
                    };

                    // Create a span that covers the entire binary expression
                    let span = Span {
                        start: lit.span.start,
                        end: match &right {
                            ExpressionNode::Identifier(i) => i.span.end,
                            ExpressionNode::Literal(l) => l.span.end,
                            _ => lit.span.end, // Fallback
                        },
                        line: lit.span.line,
                        column: lit.span.column,
                    };

                    return Ok((
                        input,
                        ExpressionNode::Binary(Spanned::new(Box::new(bin_expr), span)),
                    ));
                }
            }

            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Successfully parsed number literal: {:?}", lit);
            }
            // Wrap the literal in a Spanned
            let span = lit.span;
            Ok((input, ExpressionNode::Literal(Spanned::new(lit, span))))
        }
        TokenType::String(_) | TokenType::Boolean(_) => {
            if log::log_enabled!(log::Level::Debug) {
                log::debug!(
                    "Parsing literal: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
                log::debug!("Remaining tokens: {}", input.0.len());
            }
            let (input, lit) = parse_literal(input)?;
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Successfully parsed literal: {:?}", lit);
            }
            // Wrap the literal in a Spanned
            let span = lit.span;
            Ok((input, ExpressionNode::Literal(Spanned::new(lit, span))))
        }
        // Match expressions are handled in the first pattern match above
        // Handle identifiers, member expressions, and implicit multiplication
        TokenType::Identifier(_) => {
            log::debug!(
                "Found identifier '{}' at {}:{}, checking for struct literal",
                match &input.0[0].token_type {
                    TokenType::Identifier(s) => s.to_string(),
                    _ => "".to_string(),
                },
                input.0[0].location.line,
                input.0[0].location.column
            );
            log::debug!("Remaining tokens: {}", input.0.len());

            // Don't treat `x {}` as a struct literal in certain contexts
            let is_special_context = input
                .0
                .iter()
                .any(|t| matches!(t.token_type, TokenType::If | TokenType::Match));

            if !is_special_context {
                // Only check for struct literals if we're not in a special context
                if input.0.len() > 1 {
                    match &input.0[1].token_type {
                        TokenType::LeftBrace => {
                            log::debug!(
                                "Found LeftBrace at {}:{}, parsing as struct literal",
                                input.0[1].location.line,
                                input.0[1].location.column
                            );
                            return parse_struct_literal(input);
                        }
                        _ => log::debug!(
                            "Next token is {:?}, not a struct literal",
                            input.0[1].token_type
                        ),
                    }
                } else {
                    log::debug!("No more tokens after identifier");
                }

                log::debug!("Not a struct literal, parsing as regular identifier");
                // Look ahead to see if this is a struct literal
                if input.0.len() > 1 && matches!(input.0[1].token_type, TokenType::LeftBrace) {
                    return parse_struct_literal(input);
                }
            } else {
                log::debug!("In if statement context, not treating as struct literal");
            }

            // Otherwise parse as identifier or member expression
            let (mut input, left) = parse_identifier(input)?;

            // Check for implicit multiplication with a following number (e.g., "doses 3")
            if !input.0.is_empty() {
                if let TokenType::Integer(_) | TokenType::Float(_) = input.0[0].token_type {
                    let (new_input, right) = parse_primary(input)?;
                    input = new_input;
                    return Ok((
                        input,
                        ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                            left,
                            operator: BinaryOperator::Mul,
                            right,
                        })),
                    ));
                }
            }

            Ok((input, left))
        }
        TokenType::Dot => {
            // First parse the identifier or member expression
            let (mut input, left) = parse_identifier(input)?;

            // Check for implicit multiplication with a following number (e.g., "doses 3")
            if !input.0.is_empty() {
                if let TokenType::Integer(_) | TokenType::Float(_) = input.0[0].token_type {
                    let (new_input, right) = parse_primary(input)?;
                    input = new_input;
                    return Ok((
                        input,
                        ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                            left,
                            operator: BinaryOperator::Mul,
                            right,
                        })),
                    ));
                }
            }

            Ok((input, left))
        }
        // Handle parenthesized expressions
        TokenType::LeftParen => {
            log::debug!("Found LeftParen, parsing parenthesized expression");
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Parsing parenthesized expression");
            }
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag)(input)
                    .map_err(|e| {
                        if log::log_enabled!(log::Level::Error) {
                            log::error!("Failed to parse left parenthesis: {:?}", e);
                        }
                        e
                    })?;

            let (input, expr) = parse_expression(input).map_err(|e| {
                if log::log_enabled!(log::Level::Error) {
                    log::error!("Failed to parse expression inside parentheses: {:?}", e);
                }
                e
            })?;

            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::RightParen), ErrorKind::Tag)(input)
                    .map_err(|e| {
                        log::error!("Failed to parse right parenthesis: {:?}", e);
                        e
                    })?;

            log::debug!("Successfully parsed parenthesized expression");
            Ok((input, expr))
        }
        // Handle block expressions
        TokenType::LeftBrace => {
            log::debug!("Found LeftBrace, parsing block expression");
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Parsing block expression");
            }
            // Parse the block as a statement first
            let (input, block) = parse_block(input).map_err(|e| {
                if log::log_enabled!(log::Level::Error) {
                    log::error!("Failed to parse block: {:?}", e);
                }
                e
            })?;

            if log::log_enabled!(log::Level::Debug) {
                log::debug!(
                    "Successfully parsed block with {} statements",
                    block.statements.len()
                );
            }

            // Create a block statement node
            let stmt = StatementNode::Block(block);

            // Convert the statement to an expression
            // This will create an expression containing the statement
            let expr = ExpressionNode::from_statement(stmt);
            if log::log_enabled!(log::Level::Debug) {
                log::debug!("Converted block to expression: {:?}", expr);
            }

            Ok((input, expr))
        }
        _ => {
            if log::log_enabled!(log::Level::Error) {
                log::error!(
                    "Unexpected token in parse_primary: {:?}",
                    input.0[0].token_type
                );
            }
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

// is_comparison_operator is already imported from the parent module

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{parse_block, TokenSlice};
    use medic_ast::ast::*;
    use medic_lexer::token::Token;

    // Helper function to tokenize a string for testing
    fn tokenize(input: &str) -> Vec<Token> {
        use medic_lexer::Lexer;
        let lexer = Lexer::new(input);
        lexer.collect()
    }

    #[test]
    fn test_parse_primary_literal() {
        let tokens = tokenize("42");
        let (_, expr) = parse_primary(TokenSlice::new(&tokens)).unwrap();
        match expr {
            ExpressionNode::Literal(LiteralNode::Int(42)) => {}
            _ => panic!("Expected integer literal 42"),
        }
    }

    #[test]
    fn test_parse_primary_identifier() {
        let tokens = tokenize("x");
        let (_, expr) = parse_primary(TokenSlice::new(&tokens)).unwrap();
        match expr {
            ExpressionNode::Identifier(ident) => assert_eq!(ident.name, "x"),
            _ => panic!("Expected identifier"),
        }
    }

    #[test]
    fn test_parse_primary_parens() {
        let tokens = tokenize("(x + 1)");
        let (_, expr) = parse_primary(TokenSlice::new(&tokens)).unwrap();
        match expr {
            ExpressionNode::Binary(_) => {}
            _ => panic!("Expected binary expression"),
        }
    }

    #[test]
    fn test_parse_block_expression() {
        let tokens = tokenize("{ let x = 5; }");
        let (_, block) = parse_block(TokenSlice::new(&tokens)).unwrap();
        assert_eq!(block.statements.len(), 1);
    }

    #[test]
    fn test_parse_primary_block_expression_fails() {
        // Block expressions are valid primary expressions, so this should succeed
        let tokens = tokenize("{ 42 }");
        let result = parse_primary(TokenSlice::new(&tokens));
        assert!(
            result.is_ok(),
            "Block expressions should be valid primary expressions"
        );

        // Verify the parsed block has one statement
        if let Ok((_, ExpressionNode::Statement(stmt))) = result {
            if let StatementNode::Block(block) = *stmt {
                assert_eq!(block.statements.len(), 1, "Block should have one statement");
            } else {
                panic!("Expected block statement, got {:?}", stmt);
            }
        } else {
            panic!("Expected Statement variant, got {:?}", result);
        }
    }

    #[test]
    fn test_parse_block_expression_missing_brace() {
        let input = "{ let x = 5 "; // Missing closing brace
        let tokens = tokenize(input);
        let token_slice = TokenSlice::new(&tokens);
        let result = parse_block(token_slice);
        assert!(result.is_err(), "Expected error for missing closing brace");
    }

    #[test]
    fn test_parse_block_expression_with_stray_semicolons() {
        // Test with semicolons in various positions
        let test_cases = [
            ("{ ;let x = 1; let y = 2; }", 2),            // Leading semicolon
            ("{ let x = 1;; let y = 2; }", 2),            // Double semicolon
            ("{ let x = 1; ;let y = 2; }", 2),            // Semicolon with space
            ("{ ;;;let x = 1;;; let y = 2;;; }", 2),      // Multiple semicolons
            ("{ ; ; let x = 1; ; ; let y = 2; ; ; }", 2), // Mixed semicolons with spaces
            ("{ ; }", 0),                                 // Just semicolons
            ("{ ; ; }", 0),                               // Multiple semicolons
            ("{ let x = 1; }", 1),                        // Single statement
            ("{ let x = 1; let y = 2; }", 2),             // Multiple statements
        ];

        for (input, expected_count) in test_cases {
            let tokens = tokenize(input);
            let result = parse_block(TokenSlice::new(&tokens));
            assert!(result.is_ok(), "Failed to parse: {}", input);

            let (_, block) = result.unwrap();
            assert_eq!(
                block.statements.len(),
                expected_count,
                "Incorrect number of statements in: {}",
                input
            );
        }
    }
}
