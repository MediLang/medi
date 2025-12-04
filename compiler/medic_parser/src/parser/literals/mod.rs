use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{take_token_if, LiteralNode, TokenSlice, TokenType};
use medic_ast::ast::Spanned;
use medic_lexer::token::Location;

/// Parses a literal token (integer, float, string, or boolean) from the input and returns it as a `LiteralNode`.
///
/// Returns a parsing error if the next token is not a supported literal type or if the input is empty.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{parse_literal, TokenSlice};
/// use medic_ast::ast::LiteralNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Integer(42), "42", loc.clone())
/// ];
/// let input = TokenSlice::new(&tokens);
/// let result = parse_literal(input);
/// let (_, spanned) = result.unwrap();
/// assert!(matches!(spanned.node, LiteralNode::Int(42)));
/// ```
pub fn parse_literal(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, Spanned<LiteralNode>> {
    if let Some(token) = input.peek() {
        let span = token.location.into();
        let result = match &token.token_type {
            TokenType::Integer(i) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Integer(_)), ErrorKind::Tag)(input)?;
                (input, LiteralNode::Int(*i))
            }
            TokenType::Float(f) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Float(_)), ErrorKind::Tag)(input)?;
                (input, LiteralNode::Float(*f))
            }
            TokenType::String(s) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::String(_)), ErrorKind::Tag)(input)?;
                (input, LiteralNode::String(s.to_string()))
            }
            TokenType::Boolean(b) => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Boolean(_)), ErrorKind::Tag)(input)?;
                (input, LiteralNode::Bool(*b))
            }
            _ => {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )))
            }
        };

        Ok((result.0, Spanned::new(result.1, span)))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use medic_lexer::token::{Location, Token};

    fn loc() -> Location {
        Location {
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    #[test]
    fn parse_float_literal() {
        let tokens = vec![Token::new(TokenType::Float(1.5), "1.5", loc())];
        let input = TokenSlice::new(&tokens);
        let (_, spanned) = parse_literal(input).unwrap();
        assert!(matches!(spanned.node, LiteralNode::Float(f) if (f - 1.5).abs() < 0.001));
    }

    #[test]
    fn parse_string_literal() {
        let tokens = vec![Token::new(
            TokenType::String("hello".into()),
            "\"hello\"",
            loc(),
        )];
        let input = TokenSlice::new(&tokens);
        let (_, spanned) = parse_literal(input).unwrap();
        assert!(matches!(spanned.node, LiteralNode::String(ref s) if s == "hello"));
    }

    #[test]
    fn parse_bool_literal() {
        let tokens = vec![Token::new(TokenType::Boolean(true), "true", loc())];
        let input = TokenSlice::new(&tokens);
        let (_, spanned) = parse_literal(input).unwrap();
        assert!(matches!(spanned.node, LiteralNode::Bool(true)));
    }

    #[test]
    fn parse_unsupported_token_errors() {
        let tokens = vec![Token::new(TokenType::Let, "let", loc())];
        let input = TokenSlice::new(&tokens);
        assert!(parse_literal(input).is_err());
    }

    #[test]
    fn parse_empty_input_errors() {
        let tokens: Vec<Token> = vec![];
        let input = TokenSlice::new(&tokens);
        assert!(parse_literal(input).is_err());
    }
}
