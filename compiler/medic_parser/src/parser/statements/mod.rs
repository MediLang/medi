use nom::error::ErrorKind;
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
        log::debug!("Extracted identifier: {}", ident.name);
        // Create a new IdentifierNode with the same name
        IdentifierNode {
            name: ident.name.clone(),
        }
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

    let let_stmt = LetStatementNode {
        name: ident,
        value: expr,
    };

    Ok((input, StatementNode::Let(Box::new(let_stmt))))
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
    // Consume 'return' keyword
    let (mut input, _) = take_token_if(|t| matches!(t, TokenType::Return), ErrorKind::Tag)(input)?;

    // Check if there's an expression to parse
    if let Ok((new_input, expr)) = parse_expression(input) {
        input = new_input;
        let (new_input, _) =
            take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
        let return_node = ReturnNode {
            value: Some(Box::new(expr)),
        };
        Ok((new_input, StatementNode::Return(Box::new(return_node))))
    } else {
        // No expression, just a bare return
        let (new_input, _) =
            take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
        let return_node = ReturnNode { value: None };
        Ok((new_input, StatementNode::Return(Box::new(return_node))))
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
    // Consume 'if' keyword
    let (mut input, _) = take_token_if(|t| matches!(t, TokenType::If), ErrorKind::Tag)(input)?;

    // Parse the condition - first try to parse as a simple identifier
    let (new_input, condition) =
        match take_token_if(|t| matches!(t, TokenType::Identifier(_)), ErrorKind::Alpha)(input) {
            Ok((new_input, token)) => {
                // Successfully parsed an identifier
                if let TokenType::Identifier(name) = &token.token_type {
                    (
                        new_input,
                        ExpressionNode::Identifier(IdentifierNode {
                            name: name.to_string(),
                        }),
                    )
                } else {
                    unreachable!() // We already checked this is an identifier
                }
            }
            Err(_) => {
                // Fall back to parsing as a full expression
                parse_expression(input)?
            }
        };
    input = new_input;

    // Parse the then block
    let (new_input, then_block) = parse_block(input)?;
    input = new_input;

    // Check for else clause
    if let Ok((new_input, _)) =
        take_token_if(|t| matches!(t, TokenType::Else), ErrorKind::Tag)(input)
    {
        // Parse else if or else block
        if let Ok((new_input, _)) =
            take_token_if(|t| matches!(t, TokenType::If), ErrorKind::Tag)(new_input)
        {
            // It's an else if
            let (new_input, else_if) = parse_if_statement(new_input)?;
            let if_node = IfNode {
                condition,
                then_branch: then_block,
                else_branch: Some(BlockNode {
                    statements: vec![else_if],
                }),
            };
            Ok((new_input, StatementNode::If(Box::new(if_node))))
        } else {
            // It's an else block
            let (new_input, else_block) = parse_block(new_input)?;
            let if_node = IfNode {
                condition,
                then_branch: then_block,
                else_branch: Some(else_block),
            };
            Ok((new_input, StatementNode::If(Box::new(if_node))))
        }
    } else {
        // No else clause
        let if_node = IfNode {
            condition,
            then_branch: then_block,
            else_branch: None,
        };
        Ok((input, StatementNode::If(Box::new(if_node))))
    }
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

    // Consume 'while' keyword
    let (input, _) = take_token_if(|t| matches!(t, TokenType::While), ErrorKind::Tag)(input)?;

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

    let while_node = WhileNode { condition, body };
    Ok((input, StatementNode::While(Box::new(while_node))))
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
    //
    // `parse_identifier` already understands dotted member chains, so it is
    // sufficient here and guarantees we stop *before* the `=`.
    let (input, target) = parse_identifier(input)?;

    // Check if the next token is an equals sign
    let (input, _) = take_token_if(|t| matches!(t, TokenType::Equal), ErrorKind::Tag)(input)?;

    // Parse the value expression
    let (input, value) = parse_expression(input)?;

    // Consume the semicolon
    let (input, _) = take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;

    Ok((
        input,
        StatementNode::Assignment(Box::new(AssignmentNode { target, value })),
    ))
}

/// Parses a single statement from the input token stream.
///
/// Determines the statement type based on the first token and delegates to the appropriate parser for `let`, `return`, `if`, or `while` statements. If no recognized statement keyword is found, attempts to parse an expression statement terminated by a semicolon. Returns the parsed statement node and the remaining input.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{TokenSlice, statements::parse_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::Let, "let", Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier(InternedString::from("x")), "x", Location { line: 1, column: 5, offset: 4 }),
///     Token::new(TokenType::Equal, "=", Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Integer(5), "5", Location { line: 1, column: 9, offset: 8 }),
///     Token::new(TokenType::Semicolon, ";", Location { line: 1, column: 10, offset: 9 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_statement(input);
/// assert!(result.is_ok());
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
/// # Returns
/// A tuple containing the remaining input and the parsed statement if successful
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

        if input.0.len() > 1 {
            log::debug!(
                "Next 2 tokens: {:?} and {:?}",
                input.0[0].token_type,
                input.0[1].token_type
            );
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
        Some(token) => match &token.token_type {
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
            TokenType::While => {
                log::debug!("Found 'while' statement");
                parse_while_statement(input)
            }
            _ => {
                log::debug!("No statement keyword found, trying expression statement");
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
                        Err(e)
                    }
                }
            }
        },
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
