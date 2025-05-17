use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    parse_block, parse_expression, take_token_if, BlockNode, ExpressionNode, StatementNode,
    TokenSlice, TokenType,
};

use super::{expressions::parse_expression as parse_expr, identifiers::parse_identifier};

use medic_ast::ast::LetStatementNode;

/// Parse a let statement
pub fn parse_let_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Consume 'let' keyword
    let (mut input, _) = take_token_if(|t| matches!(t, TokenType::Let), ErrorKind::Tag)(input)?;

    // Parse the identifier
    let (new_input, ident_expr) = parse_identifier(input)?;
    input = new_input;

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
    let (new_input, _) = take_token_if(|t| matches!(t, TokenType::Equal), ErrorKind::Tag)(input)?;
    input = new_input;

    // Parse the expression
    let (new_input, expr) = parse_expression(input)?;
    input = new_input;

    // Consume ';'
    let (new_input, _) =
        take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;

    let let_stmt = LetStatementNode {
        name: ident,
        value: expr,
    };

    Ok((new_input, StatementNode::Let(Box::new(let_stmt))))
}

use medic_ast::ast::ReturnNode;

/// Parse a return statement
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

/// Parse an if statement
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

/// Parse a while statement
pub fn parse_while_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Consume 'while' keyword
    let (mut input, _) = take_token_if(|t| matches!(t, TokenType::While), ErrorKind::Tag)(input)?;

    // Parse the condition
    let (new_input, condition) = parse_expression(input)?;
    input = new_input;

    // Parse the body block
    let (new_input, body) = parse_block(input)?;

    let while_node = WhileNode { condition, body };

    Ok((new_input, StatementNode::While(Box::new(while_node))))
}

/// Parse a statement
pub fn parse_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Check the first token to determine the statement type
    if let Some(token) = input.peek() {
        match token.token_type {
            TokenType::Let => parse_let_statement(input),
            TokenType::Return => parse_return_statement(input),
            TokenType::If => parse_if_statement(input),
            TokenType::While => parse_while_statement(input),
            // TODO: Add more statement types
            _ => {
                // Try to parse an expression statement
                let (input, expr) = parse_expression(input)?;
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
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
