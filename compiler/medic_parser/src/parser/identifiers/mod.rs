use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    take_token_if, ExpressionNode, IdentifierNode, MemberExpressionNode, TokenSlice, TokenType,
};

use super::expressions::parse_expression;

/// Parse an identifier
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

/// Parse a member access expression (e.g., `object.property`)
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

        // Parse the member name as an identifier
        let (new_input, member) = parse_identifier(input)?;
        input = new_input;

        // Create a new member expression node with the member as the property
        if let ExpressionNode::Identifier(ident) = member {
            expr = ExpressionNode::Member(Box::new(MemberExpressionNode {
                object: expr,
                property: ident,
            }));
        } else {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    }

    Ok((input, expr))
}
