//! Parser implementation for Medi language using nom and TokenSlice
//! Handles healthcare-specific constructs, expressions, and statements

use medic_ast::ast::*;
use medic_lexer::token::{Token, TokenType};

use nom::{
    branch::alt,
    combinator::{map, opt},
    error::{Error as NomError, ErrorKind},
    multi::many0,
    sequence::{delimited, terminated, tuple},
    Err, IResult, InputLength,
};

/// A slice of tokens for parsing
#[derive(Clone, Debug)]
pub struct TokenSlice<'a>(pub &'a [Token]);

impl<'a> TokenSlice<'a> {
    /// Create a new TokenSlice from a slice of tokens
    pub fn new(tokens: &'a [Token]) -> Self {
        TokenSlice(tokens)
    }

    /// Get the first token in the slice
    pub fn first(&self) -> Option<&Token> {
        self.0.first()
    }

    /// Check if the slice is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Peek at the first token without consuming it
    pub fn peek(&self) -> Option<&Token> {
        self.first()
    }
}

/// Implement InputLength for TokenSlice
impl InputLength for TokenSlice<'_> {
    fn input_len(&self) -> usize {
        self.0.len()
    }
}

/// Helper function to take a token if it matches a predicate
pub fn take_token_if<'a, F>(
    predicate: F,
    err_kind: ErrorKind,
) -> impl Fn(TokenSlice<'a>) -> IResult<TokenSlice<'a>, Token>
where
    F: Fn(&TokenType) -> bool,
{
    move |input: TokenSlice<'a>| {
        if let Some(token) = input.first() {
            if predicate(&token.token_type) {
                let remaining = &input.0[1..];
                return Ok((TokenSlice(remaining), token.clone()));
            }
        }
        Err(Err::Error(NomError::new(input.clone(), err_kind)))
    }
}

/// Parse a block of statements
pub fn parse_block(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {
    let (input, _) = take_token_if(|t| matches!(t, TokenType::LeftBrace), ErrorKind::Tag)(input)?;

    // Parse statements with optional semicolons
    let (input, statements) = many0(
        // Try parsing a statement with or without a semicolon
        map(
            tuple((
                parse_statement,
                opt(take_token_if(
                    |t| matches!(t, TokenType::Semicolon),
                    ErrorKind::Tag,
                )),
            )),
            |(stmt, _)| stmt,
        ),
    )(input)?;

    // If the last token before the right brace is not a semicolon, try parsing a final expression as a statement
    let (input, last_expr) = opt(parse_expression)(input)?;

    let statements = match last_expr {
        Some(expr) => {
            let mut statements = statements;
            statements.push(StatementNode::Expr(expr));
            statements
        }
        None => statements,
    };

    let (input, _) = take_token_if(|t| matches!(t, TokenType::RightBrace), ErrorKind::Tag)(input)?;

    Ok((input, BlockNode { statements }))
}

/// Parse a primary expression (literals, identifiers, parenthesized expressions)
pub fn parse_primary(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    alt((
        map(parse_literal, ExpressionNode::Literal),
        map(parse_identifier, ExpressionNode::Identifier),
        // Parenthesized expressions
        delimited(
            take_token_if(|t| matches!(t, TokenType::LeftParen), ErrorKind::Tag),
            parse_expression,
            take_token_if(|t| matches!(t, TokenType::RightParen), ErrorKind::Tag),
        ),
    ))(input)
}

/// Parse a member access expression (e.g., obj.prop)
pub fn parse_member_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    let (mut input, mut expr) = parse_primary(input)?;

    // Keep parsing dot operators to build a chain of member accesses
    loop {
        if let Some(token) = input.peek() {
            if matches!(token.token_type, TokenType::Dot) {
                // Consume the dot
                let (new_input, _) =
                    take_token_if(|t| matches!(t, TokenType::Dot), ErrorKind::Tag)(input)?;
                input = new_input;

                // Parse the property name
                let (new_input, property) = parse_identifier(input)?;
                input = new_input;

                // Create a member expression
                expr = ExpressionNode::Member(Box::new(MemberExpressionNode {
                    object: expr,
                    property: property.name,
                }));

                continue;
            }
        }
        break;
    }

    Ok((input, expr))
}

/// Parse a binary expression with the given minimum precedence
pub fn parse_binary_expression(
    input: TokenSlice<'_>,
    min_precedence: u8,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    // Parse the left-hand side
    let (mut input, mut left) = parse_member_expression(input)?;

    // Keep parsing binary operators as long as they have at least the minimum precedence
    loop {
        // Check if there's a binary operator next
        if let Some(token) = input.peek() {
            if let Some(op) = get_binary_operator(&token.token_type) {
                let precedence = get_operator_precedence(&op);
                if precedence >= min_precedence {
                    // Consume the operator
                    let (new_input, _) =
                        take_token_if(|t| get_binary_operator(t).is_some(), ErrorKind::Tag)(input)?;
                    input = new_input;

                    // Parse the right-hand side with a higher precedence to ensure proper associativity
                    let (new_input, right) = parse_binary_expression(input, precedence + 1)?;
                    input = new_input;

                    // Create a binary expression
                    left = ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left,
                        operator: op,
                        right,
                    }));

                    continue;
                }
            }
        }
        break;
    }

    Ok((input, left))
}

/// Parse an expression
pub fn parse_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    parse_binary_expression(input, 0)
}

/// Get a binary operator from a token type
pub fn get_binary_operator(token_type: &TokenType) -> Option<BinaryOperator> {
    match token_type {
        TokenType::Plus => Some(BinaryOperator::Add),
        TokenType::Minus => Some(BinaryOperator::Sub),
        TokenType::Star => Some(BinaryOperator::Mul),
        TokenType::Slash => Some(BinaryOperator::Div),
        TokenType::Percent => Some(BinaryOperator::Mod),
        TokenType::EqualEqual => Some(BinaryOperator::Eq),
        TokenType::NotEqual => Some(BinaryOperator::Ne),
        TokenType::Less => Some(BinaryOperator::Lt),
        TokenType::LessEqual => Some(BinaryOperator::Le),
        TokenType::Greater => Some(BinaryOperator::Gt),
        TokenType::GreaterEqual => Some(BinaryOperator::Ge),
        TokenType::And => Some(BinaryOperator::And),
        TokenType::Or => Some(BinaryOperator::Or),
        _ => None,
    }
}

/// Get the precedence of a binary operator
pub fn get_operator_precedence(op: &BinaryOperator) -> u8 {
    match op {
        BinaryOperator::Or => 1,
        BinaryOperator::And => 2,
        BinaryOperator::Eq | BinaryOperator::Ne => 3,
        BinaryOperator::Lt | BinaryOperator::Le | BinaryOperator::Gt | BinaryOperator::Ge => 4,
        BinaryOperator::Add | BinaryOperator::Sub => 5,
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 6,
        BinaryOperator::Range => 7,
    }
}

/// Extension trait for TokenType to add unwrapping methods
trait TokenTypeExt {
    fn unwrap_identifier(&self) -> String;
    fn is_identifier(&self) -> bool;
    fn unwrap_literal(&self) -> LiteralNode;
    fn is_literal(&self) -> bool;
}

impl TokenTypeExt for TokenType {
    fn unwrap_identifier(&self) -> String {
        match self {
            TokenType::Identifier(s) => s.clone(),
            TokenType::Patient => "patient".to_string(),
            TokenType::Observation => "observation".to_string(),
            TokenType::Medication => "medication".to_string(),
            _ => panic!("Expected an identifier token, got {:?}", self),
        }
    }

    fn is_identifier(&self) -> bool {
        matches!(
            self,
            TokenType::Identifier(_)
                | TokenType::Patient
                | TokenType::Observation
                | TokenType::Medication
        )
    }

    fn unwrap_literal(&self) -> LiteralNode {
        match self {
            TokenType::Integer(val) => LiteralNode::Int(*val),
            TokenType::Float(val) => LiteralNode::Float(*val),
            TokenType::Boolean(val) => LiteralNode::Bool(*val),
            TokenType::String(val) => LiteralNode::String(val.clone()),
            _ => panic!("Expected a literal token, got {:?}", self),
        }
    }

    fn is_literal(&self) -> bool {
        matches!(
            self,
            TokenType::Integer(_)
                | TokenType::Float(_)
                | TokenType::Boolean(_)
                | TokenType::String(_)
        )
    }
}

impl TokenTypeExt for Token {
    fn unwrap_identifier(&self) -> String {
        self.token_type.unwrap_identifier()
    }

    fn is_identifier(&self) -> bool {
        self.token_type.is_identifier()
    }

    fn unwrap_literal(&self) -> LiteralNode {
        self.token_type.unwrap_literal()
    }

    fn is_literal(&self) -> bool {
        self.token_type.is_literal()
    }
}

/// Parse an identifier
/// Only accepts valid identifiers (variables, healthcare keywords)
pub fn parse_identifier(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, IdentifierNode> {
    let (input, token) = take_token_if(|t| t.is_identifier(), ErrorKind::Tag)(input)?;
    let identifier = IdentifierNode {
        name: token.token_type.unwrap_identifier(),
    };
    Ok((input, identifier))
}

/// Parse a literal
pub fn parse_literal(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, LiteralNode> {
    let (input, token) = take_token_if(|t| t.is_literal(), ErrorKind::Tag)(input)?;
    Ok((input, token.token_type.unwrap_literal()))
}

pub fn parse_program(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ProgramNode> {
    // A program is zero or more statements.
    let (i, statements) = many0(parse_statement)(input)?;
    // Ensure all input is consumed by the program parser, or it's an error if there are trailing tokens.
    // For now, we allow trailing tokens, as `many0` will stop on first error / non-match.
    // To enforce full consumption, one might add: `if !i.is_empty() { return Err(...) }`
    Ok((i, ProgramNode { statements }))
}

// ---- Placeholder functions from before - to be implemented or refined ----

/// Parse an if statement from a token stream
///
/// This function parses if statements with the following syntax:
/// - `if <condition> { <statements> }`
/// - `if <condition> { <statements> } else { <statements> }`
/// - `if <condition> { <statements> } else if <condition> { <statements> } ... [else { <statements> }]`
///
/// The else-if chains are represented as nested if statements in the else branch.
pub fn parse_if_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Match 'if' keyword
    let (input, _) = take_token_if(|t| matches!(t, TokenType::If), ErrorKind::Tag)(input)?;

    // Parse the condition expression
    let (input, condition) = parse_expression(input)?;

    // Parse the then branch (block)
    let (input, then_branch) = parse_block(input)?;

    // Check for an 'else' branch
    let (input, else_branch) = if let Some(token) = input.peek() {
        if matches!(token.token_type, TokenType::Else) {
            // Consume the 'else' token
            let (input, _) =
                take_token_if(|t| matches!(t, TokenType::Else), ErrorKind::Tag)(input)?;

            // Check if this is an 'else if' or just an 'else'
            if let Some(token) = input.peek() {
                if matches!(token.token_type, TokenType::If) {
                    // This is an 'else if', parse it as a nested if statement
                    let (input, if_stmt) = parse_if_statement(input)?;

                    // Create a block containing just the if statement
                    let else_block = BlockNode {
                        statements: vec![if_stmt],
                    };

                    (input, Some(else_block))
                } else {
                    // This is a regular 'else', parse the block
                    let (input, else_block) = parse_block(input)?;
                    (input, Some(else_block))
                }
            } else {
                // Unexpected end of input after 'else'
                return Err(nom::Err::Error(NomError::new(input, ErrorKind::Tag)));
            }
        } else {
            // No 'else' branch
            (input, None)
        }
    } else {
        // No more tokens, so no 'else' branch
        (input, None)
    };

    // Create the IfNode and wrap it in a StatementNode
    let if_node = IfNode {
        condition,
        then_branch,
        else_branch,
    };

    Ok((input, StatementNode::If(Box::new(if_node))))
}

/// Parse a while statement from a token stream
///
/// This function parses while statements with the following syntax:
/// - `while <condition> { <statements> }`
///
/// The body of the while loop must be a block of statements.
pub fn parse_while_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Match 'while' keyword
    let (input, _) = take_token_if(|t| matches!(t, TokenType::While), ErrorKind::Tag)(input)?;

    // Parse the condition expression
    let (input, condition) = parse_expression(input)?;

    // Parse the body (block)
    let (input, body) = parse_block(input)?;

    // Create the WhileNode and wrap it in a StatementNode
    let while_node = WhileNode { condition, body };

    Ok((input, StatementNode::While(Box::new(while_node))))
}

/// Parse an assignment statement from a token stream
///
/// # Supported L-values
/// Currently, only the following expression types are supported as l-values:
/// - Identifiers (e.g., `x = 10;`)
/// - Member expressions (e.g., `obj.prop = 20;`)
///
/// # Error Handling
/// If an unsupported expression type is used as an l-value, the parser will
/// emit a diagnostic message and return an error.
pub fn parse_assignment_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // First, parse the target of the assignment (l-value)
    let (i, target) = parse_expression(input.clone())?;

    // Expect an assignment operator
    let (i, _) = take_token_if(|t| matches!(t, TokenType::Equal), ErrorKind::Tag)(i)?;

    // Parse the value to be assigned (r-value)
    let (i, value) = parse_expression(i)?;

    // Validate the l-value
    match &target {
        ExpressionNode::Identifier(_) | ExpressionNode::Member(_) => {
            // Create an assignment statement node
            let assignment_node = AssignmentNode { target, value };

            Ok((i, StatementNode::Assignment(Box::new(assignment_node))))
        }
        _ => {
            // Invalid l-value
            eprintln!("Invalid l-value in assignment. Only identifiers and member expressions are supported as l-values, but found: {:?}", target);

            // Return an error
            Err(Err::Error(NomError::new(input.clone(), ErrorKind::Tag)))
        }
    }
}

/// Parse a statement from a token stream
pub fn parse_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    alt((
        parse_let_statement,
        parse_assignment_statement,
        parse_if_statement,
        parse_while_statement,
        parse_return_statement,
        map(
            terminated(
                parse_expression,
                take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag),
            ),
            StatementNode::Expr,
        ),
        map(parse_block, StatementNode::Block),
    ))(input)
}

/// Parses a let statement.
/// e.g., `let x = 10;`
pub fn parse_let_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Let), ErrorKind::Tag)(input)?;
    let (i, identifier_node) = parse_identifier(i)?;
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Equal), ErrorKind::Tag)(i)?;
    let (i, value_node) = parse_expression(i)?;
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Semicolon), ErrorKind::Tag)(i)?;
    Ok((
        i,
        StatementNode::Let(Box::new(LetStatementNode {
            name: identifier_node,
            value: value_node,
        })),
    ))
}

/// Parses a return statement.
/// e.g., `return x;` or `return;`
pub fn parse_return_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Return), ErrorKind::Tag)(input)?;
    // Use nom's `opt` combinator to parse an optional expression.
    let (i, value_opt) = opt(parse_expression)(i)?;
    let (i, _) = take_token_if(|tt| matches!(tt, TokenType::Semicolon), ErrorKind::Tag)(i)?;
    Ok((
        i,
        StatementNode::Return(Box::new(ReturnNode {
            value: value_opt.map(Box::new),
        })),
    ))
}

/// Parse a for statement from a token stream
pub fn parse_for_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Placeholder implementation - to be refactored with TokenSlice
    Err(nom::Err::Error(NomError::new(
        input,
        ErrorKind::Permutation,
    )))
}

/// Parse a match statement from a token stream
pub fn parse_match_statement(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StatementNode> {
    // Placeholder implementation - to be refactored with TokenSlice
    Err(nom::Err::Error(NomError::new(
        input,
        ErrorKind::Permutation,
    )))
}

/// Parse a pattern from a token stream
pub fn parse_pattern(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, PatternNode> {
    // Placeholder implementation - to be refactored with TokenSlice for actual pattern parsing
    Err(nom::Err::Error(NomError::new(
        input,
        ErrorKind::Permutation,
    )))
}

#[cfg(test)]
mod tests;
