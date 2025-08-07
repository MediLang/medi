use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{
    take_token_if, ExpressionNode, IdentifierNode, MemberExpressionNode, TokenSlice, TokenType,
};
use medic_ast::ast::Spanned;
use medic_ast::visit::Span;

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
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{parse_identifier, TokenSlice};
/// use medic_ast::ast::ExpressionNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Identifier(InternedString::from("foo")), "foo", loc.clone()),
///     Token::new(TokenType::Dot, ".", loc.clone()),
///     Token::new(TokenType::Identifier(InternedString::from("bar")), "bar", loc.clone()),
/// ];
/// let slice = TokenSlice::new(&tokens);
/// let (rest, expr) = parse_identifier(slice).unwrap();
/// assert!(matches!(expr, ExpressionNode::Member { .. }));
/// ```
/// ```
pub fn parse_identifier(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_identifier ===");
    log::debug!("Input length: {}", input.0.len());

    if let Some(token) = input.peek() {
        log::debug!(
            "Current token: {:?} at {}:{}",
            token.token_type,
            token.location.line,
            token.location.column
        );
        match &token.token_type {
            TokenType::Identifier(name) => {
                log::debug!("Parsing identifier: {name}");
                let result =
                    take_token_if(|t| matches!(t, TokenType::Identifier(_)), ErrorKind::Tag)(input);

                let (input, _) = match result {
                    Ok(r) => r,
                    Err(e) => {
                        log::error!("Failed to parse identifier: {e:?}");
                        return Err(e);
                    }
                };
                log::debug!("Successfully parsed identifier: {name}");

                // Create a new identifier node with span information
                let id_node = IdentifierNode {
                    name: name.to_string(),
                };

                // Create a Spanned wrapper for the identifier
                let span: Span = token.location.into();
                let spanned_id = Spanned::new(id_node, span);

                // Return the identifier as an expression
                let expr = ExpressionNode::Identifier(spanned_id);

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(input, expr);
                    }
                }

                Ok((input, expr))
            }
            // Handle healthcare keywords as identifiers
            TokenType::Patient => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Patient), ErrorKind::Tag)(input)?;

                // Create a Spanned identifier node for 'patient' keyword
                let ident_node = IdentifierNode::new("patient".to_string());
                let span: Span = token.location.into();
                let expr = ExpressionNode::Identifier(Spanned::new(ident_node, span));

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(input, expr);
                    }
                }

                Ok((input, expr))
            }
            // Handle other keywords that can be used as identifiers
            TokenType::Observation => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Observation), ErrorKind::Tag)(input)?;

                // Create a Spanned identifier node for 'observation' keyword
                let ident_node = IdentifierNode::new("observation".to_string());
                let span: Span = token.location.into();
                let expr = ExpressionNode::Identifier(Spanned::new(ident_node, span));

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(input, expr);
                    }
                }

                Ok((input, expr))
            }
            TokenType::Medication => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Medication), ErrorKind::Tag)(input)?;

                // Create a Spanned identifier node for 'medication' keyword
                let ident_node = IdentifierNode::new("medication".to_string());
                let span: Span = token.location.into();
                let expr = ExpressionNode::Identifier(Spanned::new(ident_node, span));

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(input, expr);
                    }
                }

                Ok((input, expr))
            }
            TokenType::If => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::If), ErrorKind::Tag)(input)?;

                // Create a Spanned identifier node for 'if' keyword
                let ident_node = IdentifierNode::new("if".to_string());
                let span: Span = token.location.into();
                let expr = ExpressionNode::Identifier(Spanned::new(ident_node, span));

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(input, expr);
                    }
                }

                Ok((input, expr))
            }
            TokenType::Else => {
                let (input, _) =
                    take_token_if(|t| matches!(t, TokenType::Else), ErrorKind::Tag)(input)?;

                // Create a Spanned identifier node for 'else' keyword
                let ident_node = IdentifierNode::new("else".to_string());
                let span: Span = token.location.into();
                let expr = ExpressionNode::Identifier(Spanned::new(ident_node, span));

                // Check for member access
                if let Some(next_token) = input.peek() {
                    if matches!(next_token.token_type, TokenType::Dot) {
                        return parse_member_expression(input, expr);
                    }
                }

                Ok((input, expr))
            }
            // Add other keyword cases as needed
            _ => {
                log::error!("Expected identifier, found: {:?}", token.token_type);
                Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )))
            }
        }
    } else {
        log::error!("Unexpected end of input in parse_identifier");
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Eof,
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
/// use medic_lexer::string_interner::InternedString;
/// use medic_parser::parser::{parse_identifier, parse_member_expression, TokenSlice};
/// use medic_ast::ast::ExpressionNode;
///
/// let loc = Location { line: 1, column: 1, offset: 0 };
/// let tokens = vec![
///     Token::new(TokenType::Identifier(InternedString::from("foo")), "foo", loc.clone()),
///     Token::new(TokenType::Dot, ".", loc.clone()),
///     Token::new(TokenType::Identifier(InternedString::from("bar")), "bar", loc.clone()),
///     Token::new(TokenType::Dot, ".", loc.clone()),
///     Token::new(TokenType::Identifier(InternedString::from("baz")), "baz", loc.clone()),
/// ];
/// let input = TokenSlice::new(&tokens);
/// let (input, object) = parse_identifier(input).unwrap();
/// let (remaining, expr) = parse_member_expression(input, object).unwrap();
/// // expr now represents foo.bar.baz as nested member expressions
/// assert!(matches!(expr, ExpressionNode::Member { .. }));
/// ```
pub fn parse_member_expression(
    mut input: TokenSlice<'_>,
    mut object: ExpressionNode,
) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_member_expression ===");
    log::debug!("Initial object: {object:?}");

    // Continue processing as long as we have a dot followed by an identifier
    while let Some(token) = input.peek() {
        if !matches!(token.token_type, TokenType::Dot) {
            break;
        }

        // Consume the dot
        let (new_input, _) = take_token_if(|t| matches!(t, TokenType::Dot), ErrorKind::Tag)(input)?;
        input = new_input;

        // Parse the property name (must be an identifier)
        let (new_input, property) = match input.peek() {
            Some(t) if matches!(t.token_type, TokenType::Identifier(_)) => {
                // Consume the identifier token
                let (input, token) = take_token_if(
                    |t| matches!(t, TokenType::Identifier(_)),
                    ErrorKind::Tag,
                )(input)?;

                // Extract the identifier name
                let name = match &token.token_type {
                    TokenType::Identifier(name) => name.to_string(),
                    _ => unreachable!("Expected identifier token"),
                };

                // Create a Spanned identifier node for the property
                let ident_node = IdentifierNode::new(name);
                let spanned_ident = Spanned::new(ident_node, token.location.into());

                (input, spanned_ident)
            }
            _ => {
                log::error!("Expected identifier after dot in member expression");
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )));
            }
        };
        input = new_input;

        // Get the span from the start of the object to the end of the property
        let object_span = object.span();
        let property_span = property.span;

        // Create a new member expression node with the current object and property
        let member_expr = MemberExpressionNode {
            object: object.clone(),
            property: property.node, // Move the IdentifierNode from Spanned
        };

        // Create a span that covers the entire member expression
        let span = Span {
            start: object_span.start,
            end: property_span.end,
            line: object_span.line,
            column: object_span.column,
        };

        // Create the Spanned member expression and wrap in ExpressionNode::Member
        let member_expr = ExpressionNode::Member(Spanned::new(Box::new(member_expr), span));

        // Update the object for the next iteration
        object = member_expr;
        log::debug!("Updated object: {object:?}");
    }

    Ok((input, object))
}
