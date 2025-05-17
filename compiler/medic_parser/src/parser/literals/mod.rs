use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{take_token_if, LiteralNode, TokenSlice, TokenType};

/// Parse a literal value
pub fn parse_literal(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, LiteralNode> {
    if let Some(token) = input.peek() {
        match &token.token_type {
            TokenType::Integer(i) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Integer(_)), ErrorKind::Tag)(input)?;
                Ok((input, LiteralNode::Int(*i)))
            }
            TokenType::Float(f) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Float(_)), ErrorKind::Tag)(input)?;
                Ok((input, LiteralNode::Float(*f)))
            }
            TokenType::String(s) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::String(_)), ErrorKind::Tag)(input)?;
                Ok((input, LiteralNode::String(s.clone())))
            }
            TokenType::Bool(b) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Bool(_)), ErrorKind::Tag)(input)?;
                Ok((input, LiteralNode::Bool(*b)))
            }
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            ))),
        }
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )))
    }
}
