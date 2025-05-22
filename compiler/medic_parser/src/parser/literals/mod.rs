use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{take_token_if, LiteralNode, TokenSlice, TokenType};

/// Parses a literal token (integer, float, string, or boolean) from the input and returns it as a `LiteralNode`.
///
/// Returns a parsing error if the next token is not a supported literal type or if the input is empty.
///
/// # Examples
///
/// ```
/// use medic_parser::parser::literals::parse_literal;
/// use medic_parser::lexer::{TokenSlice, Token, TokenType};
/// use medic_parser::ast::LiteralNode;
///
/// let tokens = vec![Token { token_type: TokenType::Integer(42), ..Default::default() }];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_literal(input);
/// assert!(matches!(result, Ok((_, LiteralNode::Int(42)))));
/// ```
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
