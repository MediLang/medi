use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    parse_block, parse_expression, take_token_if, BlockNode, ExpressionNode, StatementNode,
    TokenSlice, TokenType,
};

use super::{expressions::parse_expression as parse_expr, identifiers::parse_identifier};

use medic_ast::ast::{AssignmentNode, LetStatementNode};

/// Parses a `let` statement from the input token stream.
///
/// Expects the sequence: `let <identifier> = <expression>;`. Returns a `StatementNode::Let` containing the parsed variable name and value.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{TokenSlice, statements::parse_let_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::Let, "let".to_string(), Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Location { line: 1, column: 5, offset: 4 }),
///     Token::new(TokenType::Equal, "=".to_string(), Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Integer(42), "42".to_string(), Location { line: 1, column: 9, offset: 8 }),
///     Token::new(TokenType::Semicolon, ";".to_string(), Location { line: 1, column: 11, offset: 10 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_let_statement(input);
/// assert!(result.is_ok());
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
///
/// # Returns
/// A tuple containing the remaining input and the parsed statement if successful
pub fn parse_let_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    println!("parse_let_statement: Starting with input: {:?}", input);

    // Consume 'let' keyword
    let (input, _) = take_token_if(|t| matches!(t, TokenType::Let), ErrorKind::Tag)(input)?;
    println!("parse_let_statement: After consuming 'let': {:?}", input);

    // Parse the identifier
    let (input, ident_expr) = parse_identifier(input)?;
    println!("parse_let_statement: After parsing identifier: {:?}", input);

    // Extract the identifier from the expression
    let ident = if let ExpressionNode::Identifier(ident) = ident_expr {
        ident
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    // Consume '='
    let (input, _) = take_token_if(|t| matches!(t, TokenType::Equal), ErrorKind::Tag)(input)?;
    println!("parse_let_statement: After consuming '=': {:?}", input);

    // Parse the expression (which won't consume the semicolon)
    println!("parse_let_statement: Before parse_expression: {:?}", input);
    let (mut input, expr) = parse_expression(input)?;
    println!("parse_let_statement: After parse_expression: {:?}", input);

    // Consume the semicolon if present
    if !input.0.is_empty() && matches!(input.0[0].token_type, TokenType::Semicolon) {
        println!("Consuming semicolon after expression");
        input = input.advance();
    } else {
        println!("No semicolon found after expression");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }

    let let_stmt = LetStatementNode {
        name: ident,
        value: expr,
    };

    Ok((input, StatementNode::Let(Box::new(let_stmt))))
}

use medic_ast::ast::ReturnNode;

/// Parses a `return` statement, optionally with a return value expression.
///
/// Accepts either a bare `return;` or `return <expression>;`, returning a `StatementNode::Return` with or without a value.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{TokenSlice, statements::parse_return_statement};
///
/// // Example: return 42;
/// let tokens = vec![
///     Token::new(TokenType::Return, "return".to_string(), Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Integer(42), "42".to_string(), Location { line: 1, column: 8, offset: 7 }),
///     Token::new(TokenType::Semicolon, ";".to_string(), Location { line: 1, column: 10, offset: 9 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_return_statement(input);
/// assert!(result.is_ok());
///
/// // Example: return;
/// let tokens = vec![
///     Token::new(TokenType::Return, "return".to_string(), Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Semicolon, ";".to_string(), Location { line: 1, column: 8, offset: 7 }),
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

use medic_ast::ast::IfNode;

/// Parses an `if` statement, including optional `else` and `else if` clauses.
///
/// This function consumes the `if` keyword, parses the condition expression, and the associated block for the `then` branch. If an `else` keyword is present, it parses either an `else if` statement recursively or an `else` block. Returns an `IfNode` wrapped in a `StatementNode::If`.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{TokenSlice, statements::parse_if_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::If, "if".to_string(), Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Location { line: 1, column: 4, offset: 3 }),
///     Token::new(TokenType::LeftBrace, "{".to_string(), Location { line: 1, column: 6, offset: 5 }),
///     Token::new(TokenType::RightBrace, "}".to_string(), Location { line: 1, column: 7, offset: 6 }),
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

    // Parse the condition
    let (new_input, condition) = parse_expression(input)?;
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

use medic_ast::ast::WhileNode;

/// Parses a `while` statement, including its condition and body block.
///
/// Returns a `StatementNode::While` containing the parsed condition expression and body.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{TokenSlice, statements::parse_while_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::While, "while".to_string(), Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Less, "<".to_string(), Location { line: 1, column: 9, offset: 8 }),
///     Token::new(TokenType::Integer(10), "10".to_string(), Location { line: 1, column: 11, offset: 10 }),
///     Token::new(TokenType::LeftBrace, "{".to_string(), Location { line: 1, column: 13, offset: 12 }),
///     Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Location { line: 1, column: 15, offset: 14 }),
///     Token::new(TokenType::Equal, "=".to_string(), Location { line: 1, column: 17, offset: 16 }),
///     Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Location { line: 1, column: 19, offset: 18 }),
///     Token::new(TokenType::Plus, "+".to_string(), Location { line: 1, column: 21, offset: 20 }),
///     Token::new(TokenType::Integer(1), "1".to_string(), Location { line: 1, column: 23, offset: 22 }),
///     Token::new(TokenType::Semicolon, ";".to_string(), Location { line: 1, column: 24, offset: 23 }),
///     Token::new(TokenType::RightBrace, "}".to_string(), Location { line: 1, column: 26, offset: 25 }),
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
    if cfg!(debug_assertions) {
        log::trace!("Starting to parse while statement");
    }

    // Consume 'while' keyword and get remaining input
    let (input, _) = take_token_if(
        |t| {
            if cfg!(debug_assertions) {
                log::trace!("Checking token: {:?}", t);
            }
            matches!(t, TokenType::While)
        },
        ErrorKind::Tag,
    )(input)?;

    if cfg!(debug_assertions) {
        log::trace!("After consuming 'while' keyword");
    }

    // Parse the condition
    if cfg!(debug_assertions) {
        log::trace!("Parsing condition...");
    }
    let (input, condition) = parse_expression(input).map_err(|e| {
        if cfg!(debug_assertions) {
            log::error!("Error parsing condition: {:?}", e);
        }
        e
    })?;

    if cfg!(debug_assertions) {
        log::trace!("Condition parsed successfully: {:?}", condition);
    }

    // Parse the body block
    if cfg!(debug_assertions) {
        log::trace!("Parsing body block...");
    }
    let (input, body) = parse_block(input).map_err(|e| {
        if cfg!(debug_assertions) {
            log::error!("Error parsing block: {:?}", e);
        }
        e
    })?;

    if cfg!(debug_assertions) {
        log::trace!("Body block parsed successfully");
    }

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
    // First try to parse the target (l-value)
    let (input, target) = parse_expression(input)?;

    // Verify that the target is a valid l-value (Identifier or Member expression)
    match &target {
        ExpressionNode::Identifier(_) | ExpressionNode::Member(_) => { /* Valid l-value */ }
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Verify,
            )));
        }
    }

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
/// use medic_parser::parser::{TokenSlice, statements::parse_statement};
///
/// let tokens = vec![
///     Token::new(TokenType::Let, "let".to_string(), Location { line: 1, column: 1, offset: 0 }),
///     Token::new(TokenType::Identifier("x".to_string()), "x".to_string(), Location { line: 1, column: 5, offset: 4 }),
///     Token::new(TokenType::Equal, "=".to_string(), Location { line: 1, column: 7, offset: 6 }),
///     Token::new(TokenType::Integer(5), "5".to_string(), Location { line: 1, column: 9, offset: 8 }),
///     Token::new(TokenType::Semicolon, ";".to_string(), Location { line: 1, column: 10, offset: 9 }),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_statement(input);
/// assert!(result.is_ok());
/// ```
///
/// # Arguments
/// * `input` - A slice of tokens to parse
///
/// # Returns
/// A tuple containing the remaining input and the parsed statement if successful
pub fn parse_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Check the first token to determine the statement type
    if let Some(token) = input.peek() {
        match token.token_type {
            TokenType::Let => {
                // parse_let_statement already consumes the semicolon
                parse_let_statement(input)
            }
            TokenType::Return => {
                let (input, stmt) = parse_return_statement(input)?;
                // Consume the semicolon if present
                if let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
                    let (new_input, _) = take_token_if(
                        |t| matches!(t, TokenType::Semicolon),
                        ErrorKind::Tag,
                    )(input)?;
                    return Ok((new_input, stmt));
                }
                Ok((input, stmt))
            }
            TokenType::If => parse_if_statement(input),
            TokenType::While => parse_while_statement(input),
            _ => {
                // Try to parse an assignment statement
                if let Ok((mut input, stmt)) = parse_assignment_statement(input) {
                    // Consume the semicolon if present
                    if let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
                        let (new_input, _) = take_token_if(
                            |t| matches!(t, TokenType::Semicolon),
                            ErrorKind::Tag,
                        )(input)?;
                        input = new_input;
                    }
                    return Ok((input, stmt));
                }

                // Fall back to parsing an expression statement
                let (mut input, expr) = parse_expression(input)?;
                // Consume the semicolon if present
                if let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
                    let (new_input, _) = take_token_if(
                        |t| matches!(t, TokenType::Semicolon),
                        ErrorKind::Tag,
                    )(input)?;
                    input = new_input;
                }
                Ok((input, StatementNode::Expr(expr)))
            }
        }
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )))
    }
}
