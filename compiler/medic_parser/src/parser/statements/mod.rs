use nom::branch::alt;
use nom::combinator::{map, recognize, value};
use nom::error::ErrorKind;
use nom::multi::separated_list1;
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;

use crate::parser::{
    parse_block, parse_expression, parse_primary, take_token_if, TokenSlice, TokenType,
};

use medic_ast::ast::{
    AssignmentNode, BinaryExpressionNode, BinaryOperator, BlockNode, CallExpressionNode,
    ExpressionNode, IdentifierNode, IfNode, LetStatementNode, LiteralNode, MatchArmNode, MatchNode,
    MemberExpressionNode, PatternNode, ProgramNode, ReturnNode, StatementNode, WhileNode,
};

use super::{expressions::parse_expression as parse_expr, identifiers::parse_identifier};
use std::convert::TryFrom;

/// Parses a `let` statement from the input token stream.
///
/// Expects the sequence: `let <identifier> = <expression>;`. Returns a `StatementNode::Let` containing the parsed variable name and value.
///
/// # Semicolon Handling
/// The parser will tolerate missing semicolons after let statements. If a semicolon is missing,
/// it will log a warning but continue parsing. This is to provide a better developer experience
/// while still encouraging proper syntax.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, statements::parse_let_statement};
///
/// // With semicolon (preferred)
/// let tokens = vec![
///     Token::new(TokenType::Let, "let", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", Location { line: 1, column: 5, offset: 4 }),
///     Token::new(TokenType::Equal, "=", Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Integer(42), "42", Location { line: 1, column: 9, offset: 8 }),
///     Token::new(TokenType::Semicolon, ";", Location { line: 1, column: 11, offset: 10 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_let_statement(input);
/// assert!(result.is_ok());
///
/// // Without semicolon (tolerated with warning)
/// let tokens = vec![
///     Token::new(TokenType::Let, "let", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier(InternedString::from("y")), "y", Location { line: 1, column: 5, offset: 4 }),
///     Token::new(TokenType::Equal, "=", Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Integer(42), "42", Location { line: 1, column: 9, offset: 8 })
///     // No semicolon here
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_let_statement(input);
/// assert!(result.is_ok()); // Still succeeds despite missing semicolon
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
///
/// # Returns
/// A tuple containing the remaining input and the parsed statement if successful
pub fn parse_let_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    log::debug!("=== parse_let_statement ===");
    log::debug!("Starting with input length: {}", input.0.len());

    // Log the next few tokens for better context
    if input.0.is_empty() {
        log::error!("Unexpected empty input in parse_let_statement");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Eof,
        )));
    }

    log::debug!("Next tokens:");
    for i in 0..std::cmp::min(5, input.0.len()) {
        log::debug!(
            "  Token {}: {:?} at {}:{}",
            i,
            input.0[i].token_type,
            input.0[i].location.line,
            input.0[i].location.column
        );
    }

    // Consume 'let' keyword
    log::debug!("Looking for 'let' keyword");
    let (input, _) = match take_token_if(|t| matches!(t, TokenType::Let), ErrorKind::Tag)(input) {
        Ok((input, _)) => {
            log::debug!("Consumed 'let' keyword");
            if !input.0.is_empty() {
                log::debug!(
                    "After 'let', next token: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
                log::debug!("Remaining tokens: {}", input.0.len());
            } else {
                log::error!("Unexpected end of input after 'let'");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Eof,
                )));
            }
            (input, ())
        }
        Err(e) => {
            log::error!("Expected 'let' keyword: {:?}", e);
            return Err(e);
        }
    };

    // Parse the identifier
    log::debug!("Parsing identifier");
    let (input, ident_expr) = match parse_identifier(input) {
        Ok((input, ident_expr)) => {
            log::debug!("Successfully parsed identifier");
            if !input.0.is_empty() {
                log::debug!(
                    "After identifier, next token: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
            } else {
                log::error!("Unexpected end of input after identifier");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Eof,
                )));
            }
            (input, ident_expr)
        }
        Err(e) => {
            log::error!("Failed to parse identifier: {:?}", e);
            return Err(e);
        }
    };

    // Extract the identifier from the expression
    log::debug!("Extracting identifier from expression");
    let ident = if let ExpressionNode::Identifier(ident) = ident_expr {
        log::debug!("Extracted identifier: {}", ident.node.name);
        // Create a new IdentifierNode with the same name and span
        let span = ident.span;
        Spanned::new(
            IdentifierNode {
                name: ident.node.name.clone(),
            },
            span,
        )
    } else {
        log::error!(
            "Expected identifier after 'let', got {:?} at {}:{}",
            ident_expr,
            input.0.first().map(|t| t.location.line).unwrap_or(0),
            input.0.first().map(|t| t.location.column).unwrap_or(0)
        );
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    // Consume '='
    let (input, _) = match take_token_if(|t| matches!(t, TokenType::Equal), ErrorKind::Tag)(input) {
        Ok((input, _)) => {
            log::debug!("Consumed '='");
            if !input.0.is_empty() {
                log::debug!(
                    "After '=', next token: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
            } else {
                log::error!("Unexpected end of input after '='");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Eof,
                )));
            }
            (input, ())
        }
        Err(e) => {
            log::error!("Failed to parse '=': {:?}", e);
            return Err(e);
        }
    };

    // Parse the expression (which won't consume the semicolon)
    log::debug!("Starting to parse expression");
    let (input, expr) = match parse_expression(input) {
        Ok((input, expr)) => {
            log::debug!("Successfully parsed expression: {:?}", expr);
            if !input.0.is_empty() {
                log::debug!(
                    "After expression, next token: {:?} at {}:{}",
                    input.0[0].token_type,
                    input.0[0].location.line,
                    input.0[0].location.column
                );
            } else {
                log::warn!("No more tokens after expression (expected semicolon)");
            }
            (input, expr)
        }
        Err(e) => {
            log::error!("Failed to parse expression: {:?}", e);
            return Err(e);
        }
    };

    // Consume the semicolon if present
    log::debug!("Checking for semicolon");
    let input = if let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
        log::debug!(
            "Found semicolon at {}:{}",
            input.0[0].location.line,
            input.0[0].location.column
        );
        let input = input.advance();
        log::debug!("After semicolon, remaining tokens: {}", input.0.len());
        if !input.0.is_empty() {
            log::debug!(
                "Next token after semicolon: {:?} at {}:{}",
                input.0[0].token_type,
                input.0[0].location.line,
                input.0[0].location.column
            );
        } else {
            log::debug!("No more tokens after semicolon");
        }
        input
    } else {
        log::warn!(
            "No semicolon found after let statement. Next token: {:?} at {}:{}",
            input.0.first().map(|t| &t.token_type),
            input.0.first().map(|t| t.location.line).unwrap_or(0),
            input.0.first().map(|t| t.location.column).unwrap_or(0)
        );
        // Don't fail if there's no semicolon, just continue
        input
    };

    // Create the let statement with proper span
    let span = Span {
        start: ident.span.start,
        end: match &expr {
            ExpressionNode::Identifier(i) => i.span.end,
            ExpressionNode::Literal(l) => l.span.end,
            ExpressionNode::Binary(b) => b.span.end,
            _ => ident.span.end, // Fallback
        },
        line: ident.span.line,
        column: ident.span.column,
    };

    let stmt = LetStatementNode {
        name: ident.node,
        value: expr,
    };

    let stmt = Spanned::new(stmt, span);

    Ok((input, StatementNode::Let(Box::new(stmt))))
}

// ReturnNode is already imported above

/// Parses a `return` statement, optionally with a return value expression.
///
/// Accepts either a bare `return;` or `return <expression>;`, returning a `StatementNode::Return` with or without a value.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, statements::parse_return_statement};
///
/// // Example: return 42;
/// let tokens = vec![
///     Token::new(TokenType::Return, "return", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Integer(42), "42", Location { line: 1, column: 8, offset: 7 }),
///     Token::new(TokenType::Semicolon, ";", Location { line: 1, column: 10, offset: 9 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_return_statement(input);
/// assert!(result.is_ok());
///
/// // Example: return;
/// let tokens = vec![
///     Token::new(TokenType::Return, "return", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Semicolon, ";", Location { line: 1, column: 8, offset: 7 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_return_statement(input);
/// assert!(result.is_ok());
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
///
/// # Returns
/// A tuple containing the remaining input and the parsed return statement if successful
pub fn parse_return_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Consume 'return' keyword and get its span
    let (mut input, return_token) =
        take_token_if(|t| matches!(t, TokenType::Return), ErrorKind::Tag)(input)?;
    let start_span = return_token.location.into();

    // Check if there's an expression to parse
    if let Ok((new_input, expr)) = parse_expression(input) {
        input = new_input;
        let (new_input, semicolon_token) =
            take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;

        // Create a span from the return keyword to the semicolon
        let end_span = semicolon_token.location.into();
        let span = Span {
            start: start_span.start,
            end: end_span.end,
            line: start_span.line,
            column: start_span.column,
        };

        let return_node = ReturnNode {
            value: Some(Box::new(expr)),
        };

        Ok((
            new_input,
            StatementNode::Return(Spanned::new(Box::new(return_node), span)),
        ))
    } else {
        // No expression, just a bare return
        let (new_input, semicolon_token) =
            take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;

        // Create a span from the return keyword to the semicolon
        let end_span = semicolon_token.location.into();
        let span = Span {
            start: start_span.start,
            end: end_span.end,
            line: start_span.line,
            column: start_span.column,
        };

        let return_node = ReturnNode { value: None };

        Ok((
            new_input,
            StatementNode::Return(Spanned::new(Box::new(return_node), span)),
        ))
    }
}

/// Parses an `if` statement, including optional `else` and `else if` clauses.
///
/// This function consumes the `if` keyword, parses the condition expression, and the associated block for the `then` branch. If an `else` keyword is present, it parses either an `else if` statement recursively or an `else` block. Returns an `IfNode` wrapped in a `StatementNode::If`.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, statements::parse_if_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::If, "if", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", Location { line: 1, column: 4, offset: 3 }),
///     Token::new(TokenType::LeftBrace, "{", Location { line: 1, column: 6, offset: 5 }),
///     Token::new(TokenType::RightBrace, "}", Location { line: 1, column: 7, offset: 6 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_if_statement(input);
/// assert!(result.is_ok());
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
///
/// # Returns
/// A tuple containing the remaining input and the parsed if statement if successful
pub fn parse_if_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Consume 'if' keyword and get its span
    let (mut input, if_token) =
        take_token_if(|t| matches!(t, TokenType::If), ErrorKind::Tag)(input)?;
    let start_span = if_token.location.into();

    // Parse the condition as a full expression
    let (new_input, condition) = parse_expression(input)?;
    input = new_input;

    // Parse the then block
    let (new_input, then_block) = parse_block(input)?;
    input = new_input;

    // Track the end span for the if statement
    let mut end_span = then_block.span;
    let mut else_branch = None;

    // Check for else clause
    if let Ok((new_input, else_token)) =
        take_token_if(|t| matches!(t, TokenType::Else), ErrorKind::Tag)(input)
    {
        input = new_input;

        // Parse else if or else block
        if let Ok((new_input, _)) =
            take_token_if(|t| matches!(t, TokenType::If), ErrorKind::Tag)(input)
        {
            // It's an else if
            let (new_input, else_if) = parse_if_statement(new_input)?;
            end_span = else_if.span();
            else_branch = Some(BlockNode {
                statements: vec![else_if],
                span: end_span,
            });
            input = new_input;
        } else {
            // It's an else block
            let (new_input, else_block) = parse_block(input)?;
            end_span = else_block.span;
            else_branch = Some(else_block);
            input = new_input;
        }
    }

    // Create the full span from 'if' to the end of the else block (or then block if no else)
    let span = Span {
        start: start_span.start,
        end: end_span.end,
        line: start_span.line,
        column: start_span.column,
    };

    let if_node = IfNode {
        condition,
        then_branch: then_block,
        else_branch: else_branch,
    };

    Ok((
        input,
        StatementNode::If(Spanned::new(Box::new(if_node), span)),
    ))
}

// WhileNode is already imported above

/// Parses a `while` statement, including its condition and body block.
///
/// Returns a `StatementNode::While` containing the parsed condition expression and body.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, statements::parse_while_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::While, "while", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Less, "<", Location { line: 1, column: 9, offset: 8 }),
///     Token::new(TokenType::Integer(10), "10", Location { line: 1, column: 11, offset: 10 }),
///     Token::new(TokenType::LeftBrace, "{", Location { line: 1, column: 13, offset: 12 }),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", Location { line: 1, column: 15, offset: 14 }),
///     Token::new(TokenType::Equal, "=", Location { line: 1, column: 17, offset: 16 }),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", Location { line: 1, column: 19, offset: 18 }),
///     Token::new(TokenType::Plus, "+", Location { line: 1, column: 21, offset: 20 }),
///     Token::new(TokenType::Integer(1), "1", Location { line: 1, column: 23, offset: 22 }),
///     Token::new(TokenType::Semicolon, ";", Location { line: 1, column: 24, offset: 23 }),
///     Token::new(TokenType::RightBrace, "}", Location { line: 1, column: 26, offset: 25 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_while_statement(input);
/// assert!(result.is_ok());
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
///
/// # Returns
/// A tuple containing the remaining input and the parsed while statement if successful
pub fn parse_while_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    log::trace!("Starting to parse while statement");

    // Consume 'while' keyword and get its span
    let (input, while_token) =
        take_token_if(|t| matches!(t, TokenType::While), ErrorKind::Tag)(input)?;
    let start_span = while_token.location.into();

    log::trace!("After consuming 'while' keyword");

    // Parse the condition (with or without parentheses)
    log::debug!("Parsing while condition");
    let (input, condition) = if let Ok((input, _)) =
        take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag)(input)
    {
        // Condition is wrapped in parentheses
        log::debug!("Found opening parenthesis for condition");
        let (input, condition) = parse_expression(input)?;
        let (input, _) =
            take_token_if(|t| matches!(t, TokenType::RightParen), ErrorKind::Tag)(input)?;
        log::debug!("Found closing parenthesis for condition");
        (input, condition)
    } else {
        // Condition without parentheses - parse as a general expression
        log::debug!("No parentheses found, parsing condition as expression");
        parse_expression(input)?
    };

    log::debug!("Successfully parsed while condition");
    log::debug!("Condition parsed successfully: {:?}", condition);

    // Parse the body block
    log::trace!("Parsing body block...");
    let (input, body) = parse_block(input).map_err(|e| {
        log::error!("Error parsing block: {:?}", e);
        e
    })?;

    log::trace!("Body block parsed successfully");

    // Create the full span from 'while' to the end of the body block
    let span = Span {
        start: start_span.start,
        end: body.span.end,
        line: start_span.line,
        column: start_span.column,
    };

    let while_node = WhileNode { condition, body };

    Ok((
        input,
        StatementNode::While(Spanned::new(Box::new(while_node), span)),
    ))
}

/// Parses an assignment statement (e.g., `x = 42;` or `x.y = z + 1;`).
///
/// Expects an l-value (identifier or member expression) followed by an equals sign and an expression.
/// Returns a `StatementNode::Assignment` containing the target and value expressions.
///
/// # Errors
/// Returns an error if the target is not a valid l-value (not an identifier or member expression).
pub fn parse_assignment_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Parse the target (l-value). We only want an identifier or member access,
    // not the whole `= …` expression.
    // `parse_identifier` already understands dotted member chains, so it is
    // sufficient here and guarantees we stop *before* the `=`.
    let (input, target) = parse_identifier(input)?;
    let start_span = match &target {
        ExpressionNode::Identifier(ident) => ident.span,
        ExpressionNode::Member(member) => member.span,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    };

    // Get the equals sign token for span calculation
    let (input, equals_token) =
        take_token_if(|t| matches!(t, TokenType::Equal), ErrorKind::Tag)(input)?;
    let eq_span = equals_token.location.into();

    // Parse the expression after the equals sign
    let input = input.skip_whitespace();
    let (input, value) = parse_expression(input)?;

    // Calculate the full span from target start to value end
    let end_span = match &value {
        ExpressionNode::Identifier(ident) => ident.span,
        ExpressionNode::Literal(lit) => lit.span,
        ExpressionNode::Binary(bin) => bin.span,
        ExpressionNode::Member(mem) => mem.span,
        _ => start_span, // Fallback
    };

    let span = Span {
        start: start_span.start,
        end: end_span.end,
        line: start_span.line,
        column: start_span.column,
    };

    // Create the assignment node with proper spans
    let assignment = AssignmentNode {
        target: match target {
            ExpressionNode::Identifier(ident) => ExpressionNode::Identifier(ident),
            ExpressionNode::Member(member) => ExpressionNode::Member(member),
            _ => {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )))
            }
        },
        value,
    };

    // Wrap in Spanned and return
    Ok((
        input,
        StatementNode::Assignment(Spanned::new(Box::new(assignment), span)),
    ))
}
/// where <arms> is a comma-separated list of patterns and expressions.
///
/// # Examples
/// ```
/// // Full syntax
/// match x {
///     1 => "one",
///     2 => "two",
///     _ => "other"
/// }
///
/// // Concise syntax
/// x {
///     1 => "one",
///     2 => "two",
///     _ => "other"
/// }
/// ```
pub(crate) fn parse_match_statement(
    input: TokenSlice<'_>,
) -> IResult<TokenSlice<'_>, StatementNode> {
    log::debug!("=== parse_match_statement ===");
    log::debug!("Input length: {}", input.0.len());

    // Check if this is a match statement or concise match syntax
    let (mut input, expr) = if let TokenType::Match = input.0[0].token_type {
        // Full match syntax: match <expr> { ... }
        log::debug!("Parsing full match syntax");
        let input = input.advance(); // Consume 'match' keyword
        let input = input.skip_whitespace();

        // Parse the expression before the left brace
        let expr_end = input
            .0
            .iter()
            .position(|t| t.token_type == TokenType::LeftBrace)
            .unwrap_or_else(|| {
                log::error!("Could not find opening brace in match expression");
                input.0.len()
            });

        if expr_end == 0 {
            log::error!("Expected expression before opening brace in match statement");
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }

        let (expr_input, rest) = input.0.split_at(expr_end);
        let (_, expr) = super::expressions::parse_expression(TokenSlice(expr_input))?;
        (TokenSlice(rest), expr)
    } else if input.0.len() > 1 && input.0[1].token_type == TokenType::LeftBrace {
        // Concise syntax: <ident> { ... }
        log::debug!("Parsing concise match syntax");

        // For concise syntax, the expression is just the identifier
        if let TokenType::Identifier(id) = &input.0[0].token_type {
            log::debug!("Found identifier: {}", id);
            let expr = ExpressionNode::Identifier(IdentifierNode {
                name: id.to_string(),
            });
            // Return the input with the identifier and left brace consumed
            (TokenSlice(&input.0[2..]), expr)
        } else {
            log::error!(
                "Expected identifier in concise match syntax, got: {:?}",
                input.0[0].token_type
            );
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    } else {
        log::error!(
            "Expected 'match' keyword or concise match syntax, got: {:?}",
            input.0[0].token_type
        );
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    log::debug!("Parsed match expression: {:?}", expr);
    let input = input.skip_whitespace();

    // Parse opening brace
    let (input, _) = take_token_if(|tt| *tt == TokenType::LeftBrace, ErrorKind::Tag)(input)
        .map_err(|e| {
            log::error!("Failed to find opening brace: {:?}", e);
            log::error!("Next token: {:?}", input.0.first().map(|t| &t.token_type));
            e
        })?;

    log::debug!("Found opening brace, remaining tokens: {}", input.0.len());
    log::debug!("Starting to parse match arms...");

    let mut input = input;
    let mut arms = Vec::new();

    // Keep parsing arms until we hit the closing brace
    while !input.0.is_empty() {
        // Check for closing brace
        if let Some((first, _)) = input.0.split_first() {
            if first.token_type == TokenType::RightBrace {
                input = input.advance();
                break;
            }
        }

        // Parse pattern
        log::debug!("Parsing match arm pattern at token: {:?}", input.0[0]);
        let (mut next_input, pattern) = match input.0.split_first() {
            Some((token, rest)) => {
                match &token.token_type {
                    TokenType::Underscore => {
                        log::debug!("Found wildcard pattern");
                        (TokenSlice(rest), PatternNode::Wildcard)
                    }
                    TokenType::Identifier(id) => {
                        // Check if this is a variant pattern (e.g., Some(x))
                        if rest
                            .first()
                            .map_or(false, |t| t.token_type == TokenType::LeftParen)
                        {
                            // This is a variant pattern like Some(x)
                            log::debug!("Found variant pattern: {}", id);
                            let variant_name = id.to_string();
                            let inner_rest = &rest[1..]; // Skip the identifier and left paren

                            // Parse the inner pattern
                            if let Some((inner_token, inner_rest)) = inner_rest.split_first() {
                                match &inner_token.token_type {
                                    TokenType::Identifier(inner_id) => {
                                        // Handle simple identifier inside the variant
                                        let inner_pattern =
                                            PatternNode::Identifier(IdentifierNode {
                                                name: inner_id.to_string(),
                                            });

                                        // Skip to the closing parenthesis
                                        if let Some((close_paren, rest_after_paren)) =
                                            inner_rest.split_first()
                                        {
                                            if close_paren.token_type == TokenType::RightParen {
                                                (
                                                    TokenSlice(rest_after_paren),
                                                    PatternNode::Variant {
                                                        name: variant_name,
                                                        inner: Box::new(inner_pattern),
                                                    },
                                                )
                                            } else {
                                                log::error!("Expected closing parenthesis after variant pattern, got: {:?}", close_paren.token_type);
                                                return Err(nom::Err::Error(
                                                    nom::error::Error::new(input, ErrorKind::Tag),
                                                ));
                                            }
                                        } else {
                                            log::error!(
                                                "Unexpected end of input after variant pattern"
                                            );
                                            return Err(nom::Err::Error(nom::error::Error::new(
                                                input,
                                                ErrorKind::Eof,
                                            )));
                                        }
                                    }
                                    _ => {
                                        log::error!(
                                            "Expected identifier inside variant pattern, got: {:?}",
                                            inner_token.token_type
                                        );
                                        return Err(nom::Err::Error(nom::error::Error::new(
                                            input,
                                            ErrorKind::Tag,
                                        )));
                                    }
                                }
                            } else {
                                log::error!("Unexpected end of input after variant pattern");
                                return Err(nom::Err::Error(nom::error::Error::new(
                                    input,
                                    ErrorKind::Eof,
                                )));
                            }
                        } else {
                            // This is a simple identifier pattern
                            log::debug!("Found identifier pattern: {}", id);
                            (
                                TokenSlice(rest),
                                PatternNode::Identifier(IdentifierNode {
                                    name: id.to_string(),
                                }),
                            )
                        }
                    }
                    TokenType::Integer(n) => {
                        log::debug!("Found integer literal pattern: {}", n);
                        (TokenSlice(rest), PatternNode::Literal(LiteralNode::Int(*n)))
                    }
                    _ => {
                        log::error!("Unexpected token in pattern: {:?}", token.token_type);
                        return Err(nom::Err::Error(nom::error::Error::new(
                            input,
                            ErrorKind::Tag,
                        )));
                    }
                }
            }
            None => {
                log::error!("Unexpected end of input while parsing pattern");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Eof,
                )));
            }
        };

        // Parse the fat arrow
        log::debug!("Looking for fat arrow after pattern");
        let next_input =
            match take_token_if(|tt| *tt == TokenType::FatArrow, ErrorKind::Tag)(next_input) {
                Ok((input, _)) => {
                    log::debug!("Found fat arrow");
                    input
                }
                Err(e) => {
                    log::error!("Failed to find fat arrow: {:?}", e);
                    log::error!(
                        "Next token: {:?}",
                        next_input.0.first().map(|t| &t.token_type)
                    );
                    return Err(e);
                }
            };

        // Parse the expression
        log::debug!("Parsing expression for match arm");
        let (mut after_expr, body) = match parse_expression(next_input) {
            Ok(result) => result,
            Err(e) => {
                log::error!("Failed to parse expression in match arm: {:?}", e);
                log::error!(
                    "Next token: {:?}",
                    next_input.0.first().map(|t| &t.token_type)
                );
                return Err(e);
            }
        };

        // Add the arm to the list
        log::debug!("Adding match arm with pattern: {:?}", pattern);
        arms.push(MatchArmNode {
            pattern,
            body: Box::new(body),
        });

        // Check for comma or closing brace
        if let Some((first, rest)) = after_expr.0.split_first() {
            match first.token_type {
                TokenType::Comma => {
                    // Skip the comma and continue with the next arm
                    after_expr = TokenSlice(rest);
                    log::debug!("Skipping comma between match arms");
                }
                TokenType::RightBrace => {
                    // Found the closing brace, we're done
                    after_expr = TokenSlice(rest);
                    input = after_expr;
                    break;
                }
                _ => {
                    log::error!(
                        "Expected comma or closing brace after match arm, got: {:?}",
                        first.token_type
                    );
                    return Err(nom::Err::Error(nom::error::Error::new(
                        after_expr,
                        ErrorKind::Tag,
                    )));
                }
            }
        } else {
            // End of input, but we still need a closing brace
            log::error!("Unexpected end of input, expected comma or closing brace");
            return Err(nom::Err::Error(nom::error::Error::new(
                after_expr,
                ErrorKind::Eof,
            )));
        }

        // Update input for the next iteration
        input = after_expr;
    }

    // We've already consumed the closing brace in the loop, so just return the input
    let input = TokenSlice(input.0);

    log::debug!(
        "Successfully parsed match statement with {} arms",
        arms.len()
    );
    log::debug!("Remaining input length: {}", input.0.len());
    if !input.0.is_empty() {
        log::debug!("Next token after match: {:?}", input.0[0].token_type);
    }

    Ok((
        input,
        StatementNode::Match(Box::new(MatchNode {
            expr: Box::new(expr),
            arms,
        })),
    ))
}

/// Parses a single statement from the input token stream.
///
/// Determines the statement type based on the first token and delegates to the appropriate parser.
pub fn parse_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    log::debug!("=== parse_statement ===");
    log::debug!("Input length: {}", input.0.len());

    if !input.0.is_empty() {
        log::debug!(
            "Next token: {:?} at {}:{}",
            input.0[0].token_type,
            input.0[0].location.line,
            input.0[0].location.column
        );

        // Log more tokens for better context
        let num_tokens = input.0.len().min(5);
        log::debug!("Next {} tokens:", num_tokens);
        for i in 0..num_tokens {
            log::debug!("  {}: {:?}", i, input.0[i].token_type);
        }
    } else {
        log::error!("Unexpected end of input in parse_statement");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    }

    // Try to parse different statement types based on the first token
    let result = match input.peek() {
        Some(token) => {
            log::debug!(
                "parse_statement: Processing token: {:?} at {}:{} ",
                token.token_type,
                token.location.line,
                token.location.column
            );

            // First, check for match statement (either full or concise syntax)
            match &token.token_type {
                TokenType::Match => {
                    log::debug!("Found 'match' statement");
                    let result = parse_match_statement(input);
                    log::debug!("parse_match_statement (full) result: {:?}", result);
                    return result;
                }
                TokenType::Identifier(_)
                    if input.0.len() > 1
                        && matches!(input.0[1].token_type, TokenType::LeftBrace) =>
                {
                    log::debug!("Found concise match syntax, parsing as match statement");
                    let result = parse_match_statement(input);
                    log::debug!("parse_match_statement (concise) result: {:?}", result);
                    return result;
                }
                _ => {}
            }

            match &token.token_type {
                TokenType::Let => {
                    log::debug!("Found 'let' statement");
                    parse_let_statement(input)
                }
                TokenType::Return => {
                    log::debug!("Found 'return' statement");
                    parse_return_statement(input)
                }
                TokenType::If => {
                    log::debug!("Found 'if' statement");
                    parse_if_statement(input)
                }
                TokenType::Match => {
                    log::debug!("Found 'match' statement");
                    let result = parse_match_statement(input);
                    log::debug!("parse_match_statement (full) result: {:?}", result);
                    result
                }
                TokenType::While => {
                    log::debug!("Found 'while' statement");
                    parse_while_statement(input)
                }
                _ => {
                    log::debug!("No statement keyword found, checking for concise match syntax");

                    // Check for concise match syntax: <ident> { ... }
                    if input.0.len() > 1 {
                        if let TokenType::Identifier(_) = input.0[0].token_type {
                            if let TokenType::LeftBrace = input.0[1].token_type {
                                log::debug!(
                                    "Found concise match syntax, parsing as match statement"
                                );
                                return parse_match_statement(input);
                            }
                        }
                    }

                    log::debug!("No concise match syntax found, trying expression statement");
                    // Log the next few tokens for better debugging
                    log::debug!("Next tokens in parse_statement (expression case):");
                    for (i, t) in input.0.iter().take(5).enumerate() {
                        log::debug!(
                            "  {}: {:?} at {}:{}",
                            i,
                            t.token_type,
                            t.location.line,
                            t.location.column
                        );
                    }

                    // Try to parse as an expression statement
                    match parse_expression(input) {
                        Ok((input, expr)) => {
                            log::debug!("Successfully parsed expression statement");
                            // Consume the semicolon if present
                            let input = if let Some(TokenType::Semicolon) =
                                input.peek().map(|t| &t.token_type)
                            {
                                log::debug!("Consuming semicolon after expression");
                                input.advance()
                            } else {
                                log::debug!("No semicolon after expression");
                                input
                            };
                            Ok((input, StatementNode::Expr(expr)))
                        }
                        Err(e) => {
                            log::error!("Failed to parse expression: {:?}", e);
                            if let nom::Err::Error(ref err) = e {
                                log::error!("Error at position: {}", err.input.0.len());
                                if !err.input.0.is_empty() {
                                    log::error!(
                                        "Error token: {:?} at {}:{}",
                                        err.input.0[0].token_type,
                                        err.input.0[0].location.line,
                                        err.input.0[0].location.column
                                    );
                                }
                            }
                            Err(e)
                        }
                    }
                }
            }
        }
        None => {
            log::error!("Unexpected end of input in parse_statement");
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Eof,
            )))
        }
    };

    // Log the result of parsing
    match result {
        Ok((remaining, stmt)) => {
            log::debug!("Successfully parsed statement: {:?}", stmt);
            log::debug!("Remaining tokens after statement: {}", remaining.0.len());

            if !remaining.0.is_empty() {
                log::debug!(
                    "Next token after statement: {:?} at {}:{}",
                    remaining.0[0].token_type,
                    remaining.0[0].location.line,
                    remaining.0[0].location.column
                );
            }

            Ok((remaining, stmt))
        }
        Err(e) => {
            log::error!("Failed to parse statement: {:?}", e);
            if let nom::Err::Error(ref e) = e {
                log::error!("Error at position: {}", e.input.0.len());
                if !e.input.0.is_empty() {
                    log::error!("Error token: {:?}", e.input.0[0]);
                }
            }
            Err(e)
        }
    }
}
