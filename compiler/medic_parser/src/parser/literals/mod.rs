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
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{parse_literal, TokenSlice};
/// use medic_ast::ast::LiteralNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Integer(42), "42".to_string(), loc.clone())
/// ];
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
