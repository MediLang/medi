use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    take_token_if, ExpressionNode, IdentifierNode, MemberExpressionNode, TokenSlice, TokenType,
};

use super::expressions::parse_expression;

/// Parses an identifier or certain keywords as an identifier, supporting member access expressions.
///
/// Accepts an identifier token or specific keywords (`Patient`, `Observation`, `Medication`, `If`, `Else`) as valid identifiers. If the identifier is immediately followed by a dot (`.`), parses the subsequent member access chain as a member expression.
///
/// # Returns
///
/// On success, returns the remaining input and an `ExpressionNode` representing the parsed identifier or member expression. Returns a parsing error if the input does not start with a valid identifier or supported keyword.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{parse_identifier, TokenSlice};
/// use medic_ast::ast::ExpressionNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Identifier("foo".to_string()), "foo".to_string(), loc.clone()),
///     Token::new(TokenType::Dot, ".".to_string(), loc.clone()),
///     Token::new(TokenType::Identifier("bar".to_string()), "bar".to_string(), loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let (rest, expr) = parse_identifier(slice).unwrap();
/// assert!(matches!(expr, ExpressionNode::Member { .. }));
/// ```
/// ```
pub fn parse_identifier(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    if let Some(token) = input.peek() {
        match &token.token_type {
            TokenType::Identifier(name) => {
                let (input, _) = take_token_if(
                    |t| matches!(t, TokenType::Identifier(_)),
                    ErrorKind::Tag,
                )(input)?;

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(
                            input,
                            ExpressionNode::Identifier(IdentifierNode { name: name.clone() }),
                        );
                    }
                }

                Ok((
                    input,
                    ExpressionNode::Identifier(IdentifierNode { name: name.clone() }),
                ))
            }
            // Handle healthcare keywords as identifiers
            TokenType::Patient => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Patient), ErrorKind::Tag)(input)?;

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(
                            input,
                            ExpressionNode::Identifier(IdentifierNode {
                                name: "patient".to_string(),
                            }),
                        );
                    }
                }

                Ok((
                    input,
                    ExpressionNode::Identifier(IdentifierNode {
                        name: "patient".to_string(),
                    }),
                ))
            }
            TokenType::Observation => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Observation), ErrorKind::Tag)(input)?;

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(
                            input,
                            ExpressionNode::Identifier(IdentifierNode {
                                name: "observation".to_string(),
                            }),
                        );
                    }
                }

                Ok((
                    input,
                    ExpressionNode::Identifier(IdentifierNode {
                        name: "observation".to_string(),
                    }),
                ))
            }
            TokenType::Medication => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Medication), ErrorKind::Tag)(input)?;

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(
                            input,
                            ExpressionNode::Identifier(IdentifierNode {
                                name: "medication".to_string(),
                            }),
                        );
                    }
                }

                Ok((
                    input,
                    ExpressionNode::Identifier(IdentifierNode {
                        name: "medication".to_string(),
                    }),
                ))
            }
            // Handle other keywords that can be used as identifiers
            TokenType::If | TokenType::Else => {
                let name = match token.token_type {
                    TokenType::If => "if",
                    TokenType::Else => "else",
                    _ => unreachable!(),
                };
                let (input, _) = take_token_if(|t| t == &token.token_type, ErrorKind::Tag)(input)?;

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(
                            input,
                            ExpressionNode::Identifier(IdentifierNode {
                                name: name.to_string(),
                            }),
                        );
                    }
                }

                Ok((
                    input,
                    ExpressionNode::Identifier(IdentifierNode {
                        name: name.to_string(),
                    }),
                ))
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

/// Parses a chained member access expression (e.g., `object.property.subproperty`).
///
/// Continues parsing member accesses as long as dot tokens are present, constructing nested member expression nodes. Returns an error if a member property is not a valid identifier.
///
/// # Examples
///
/// ```
/// use medic_lexer::token::{Token, TokenType, Location};
/// use medic_parser::parser::{parse_identifier, parse_member_expression, TokenSlice};
/// use medic_ast::ast::ExpressionNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Identifier("foo".to_string()), "foo".to_string(), loc.clone()),
///     Token::new(TokenType::Dot, ".".to_string(), loc.clone()),
///     Token::new(TokenType::Identifier("bar".to_string()), "bar".to_string(), loc.clone()),
///     Token::new(TokenType::Dot, ".".to_string(), loc.clone()),
///     Token::new(TokenType::Identifier("baz".to_string()), "baz".to_string(), loc.clone()),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let (input, object) = parse_identifier(input).unwrap();
/// let (remaining, expr) = parse_member_expression(input, object).unwrap();
/// // expr now represents foo.bar.baz as nested member expressions
/// assert!(matches!(expr, ExpressionNode::Member { .. }));
/// ```
pub fn parse_member_expression(
    input: TokenSlice<'_>,
    object: ExpressionNode,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    let mut input = input;
    let mut expr = object;

    // Keep parsing member accesses as long as we find dots
    while let Some(token) = input.peek() {
        if !matches!(token.token_type, TokenType::Dot) {
            break;
        }

        // Consume the dot
        input = input.advance();

        // Parse the next identifier or keyword after the dot
        let (new_input, ident_name) = if let Ok((input, token)) =
            take_token_if(|t| matches!(t, TokenType::Identifier(_)), ErrorKind::Tag)(input)
        {
            // Handle regular identifiers
            if let TokenType::Identifier(name) = &token.token_type {
                (input, name.clone())
            } else {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )));
            }
        } else if let Ok((input, token)) = take_token_if(
            |t| {
                matches!(
                    t,
                    TokenType::Patient
                        | TokenType::Observation
                        | TokenType::Medication
                        | TokenType::If
                        | TokenType::Else
                )
            },
            ErrorKind::Tag,
        )(input)
        {
            // Handle keywords that can be used as identifiers
            let name = match token.token_type {
                TokenType::Patient => "patient",
                TokenType::Observation => "observation",
                TokenType::Medication => "medication",
                TokenType::If => "if",
                TokenType::Else => "else",
                _ => unreachable!(),
            };
            (input, name.to_string())
        } else {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        };

        // Create a new member expression with the current identifier
        expr = ExpressionNode::Member(Box::new(MemberExpressionNode {
            object: expr,
            property: IdentifierNode { name: ident_name },
        }));

        input = new_input;
    }

    Ok((input, expr))
}
