use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{parse_expression, take_token_if, ExpressionNode, Span, TokenSlice, TokenType};
use medic_ast::ast::ArrayLiteralNode;
use medic_ast::ast::NodeList;
use medic_ast::Spanned;

/// Parses an array literal in the format `[expr1, expr2, ...]`
pub fn parse_array_literal(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_array_literal ===");
    log::debug!("Input length: {}", input.0.len());

    if input.0.is_empty() {
        log::error!("Unexpected end of input in parse_array_literal");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Eof,
        )));
    }

    // Parse the opening bracket
    log::debug!("Looking for opening bracket '[' ...");
    let (input, left_bracket) =
        take_token_if(|t| matches!(t, TokenType::LeftBracket), ErrorKind::Char)(input)?;

    // Save the start position of the array
    let start_span = Span {
        start: left_bracket.location.offset,
        end: left_bracket.location.offset + left_bracket.lexeme.len(),
        line: left_bracket.location.line as u32,
        column: left_bracket.location.column as u32,
    };

    // Parse zero or more comma-separated expressions until a right bracket
    log::debug!("Parsing array elements...");
    let mut elements: NodeList<ExpressionNode> = NodeList::new();

    // Early check for empty array: next token is RightBracket
    if let Some(tok) = input.0.first() {
        if matches!(tok.token_type, TokenType::RightBracket) {
            // consume the right bracket and finish
            let (input2, right_bracket) =
                take_token_if(|t| matches!(t, TokenType::RightBracket), ErrorKind::Char)(input)?;

            let span = Span {
                start: start_span.start,
                end: right_bracket.location.offset + right_bracket.lexeme.len(),
                line: start_span.line,
                column: start_span.column,
            };

            return Ok((
                input2,
                ExpressionNode::Array(Spanned::new(Box::new(ArrayLiteralNode { elements }), span)),
            ));
        }
    }

    // Parse first element
    let (mut input2, first_elem) = parse_expression(input)?;
    elements.push(first_elem);

    // Parse subsequent elements prefixed by commas
    loop {
        if input2.0.is_empty() {
            log::error!("Unexpected end of input while parsing array elements");
            return Err(nom::Err::Error(nom::error::Error::new(
                input2,
                ErrorKind::Eof,
            )));
        }
        match &input2.0[0].token_type {
            TokenType::Comma => {
                // consume comma
                let (rest, _) =
                    take_token_if(|t| matches!(t, TokenType::Comma), ErrorKind::Char)(input2)?;
                // parse next element
                let (rest, elem) = parse_expression(rest)?;
                elements.push(elem);
                input2 = rest;
            }
            TokenType::RightBracket => {
                // end of array
                break;
            }
            other => {
                log::error!("Expected ',' or ']' in array literal, found: {other:?}");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input2,
                    ErrorKind::Tag,
                )));
            }
        }
    }

    // Parse the closing bracket
    let (input3, right_bracket) =
        take_token_if(|t| matches!(t, TokenType::RightBracket), ErrorKind::Char)(input2)?;

    let span = Span {
        start: start_span.start,
        end: right_bracket.location.offset + right_bracket.lexeme.len(),
        line: start_span.line,
        column: start_span.column,
    };

    Ok((
        input3,
        ExpressionNode::Array(Spanned::new(Box::new(ArrayLiteralNode { elements }), span)),
    ))
}
