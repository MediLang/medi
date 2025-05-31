use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::multispace0,
    combinator::map,
    multi::separated_list0,
    sequence::{delimited, pair, separated_pair},
    IResult,
};

use crate::parser::{
    parse_expression, take_token_if, ExpressionNode, IdentifierNode, TokenSlice, TokenType,
};
use medic_ast::ast::{StructField, StructLiteralNode};

/// Parses a struct field in the format `name: expression`
pub fn parse_struct_field(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, StructField> {
    log::debug!("=== parse_struct_field ===");
    log::debug!("Input length: {}", input.0.len());
    
    if !input.0.is_empty() {
        log::debug!("Current token: {:?} at {}:{}", 
            input.0[0].token_type, 
            input.0[0].location.line, 
            input.0[0].location.column
        );
    }
    
    // Parse the field name (identifier)
    log::debug!("Parsing field name");
    let (input, name_token) = take_token_if(
        |t| matches!(t, TokenType::Identifier(_)),
        nom::error::ErrorKind::Alpha,
    )(input)?;
    
    let name = match &name_token.token_type {
        TokenType::Identifier(name) => name.to_string(),
        _ => unreachable!(), // We already checked this in the take_token_if
    };
    
    // Parse the colon
    log::debug!("Parsing colon after field name");
    let (input, _) = match take_token_if(
        |t| matches!(t, TokenType::Colon),
        nom::error::ErrorKind::Char,
    )(input) {
        Ok(result) => {
            log::debug!("Successfully parsed colon");
            result
        },
        Err(e) => {
            log::error!("Expected ':' after field name: {:?}", e);
            return Err(e);
        }
    };
    
    // Parse the field value (expression)
    log::debug!("Parsing field value expression");
    let (input, value) = match parse_expression(input) {
        Ok((input, expr)) => {
            log::debug!("Successfully parsed field value expression");
            (input, expr)
        },
        Err(e) => {
            log::error!("Failed to parse field value expression: {:?}", e);
            return Err(e);
        }
    };
    
    Ok((
        input,
        StructField {
            name,
            value,
        },
    ))
}

/// Parses a struct literal in the format `TypeName { field1: value1, field2: value2 }`
pub fn parse_struct_literal(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, ExpressionNode> {
    log::debug!("=== parse_struct_literal ===");
    log::debug!("Input length: {}", input.0.len());
    
    if !input.0.is_empty() {
        log::debug!("Next token: {:?} at {}:{}", 
            input.0[0].token_type, 
            input.0[0].location.line, 
            input.0[0].location.column
        );
    } else {
        log::error!("Unexpected end of input in parse_struct_literal");
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Eof)));
    }
    
    // Check if we're in an if statement context
    let is_in_if_context = input.0.iter().any(|t| matches!(t.token_type, TokenType::If));
    if is_in_if_context {
        log::debug!("In if statement context, not parsing as struct literal");
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    }
    
    // Parse the type name
    log::debug!("Parsing type name...");
    let (input, type_name_token) = match take_token_if(
        |t| matches!(t, TokenType::Identifier(_)),
        nom::error::ErrorKind::Alpha,
    )(input) {
        Ok(result) => {
            log::debug!("Successfully parsed type name");
            result
        },
        Err(e) => {
            log::error!("Failed to parse type name: {:?}", e);
            return Err(e);
        }
    };
    
    let type_name = match &type_name_token.token_type {
        TokenType::Identifier(name) => {
            log::debug!("Type name: {}", name);
            name.to_string()
        },
        _ => unreachable!(), // We already checked this in the take_token_if
    };
    
    // Parse the opening brace
    log::debug!("Looking for opening brace...");
    let (input, _) = match take_token_if(
        |t| matches!(t, TokenType::LeftBrace),
        nom::error::ErrorKind::Char,
    )(input) {
        Ok(result) => {
            log::debug!("Found opening brace");
            result
        },
        Err(e) => {
            log::error!("Expected '{{' after type name: {:?}", e);
            return Err(e);
        }
    };
    
    // Parse the fields (comma-separated)
    log::debug!("Parsing struct fields...");
    let (input, fields) = match separated_list0(
        |input| {
            let (input, _) = take_token_if(
                |t| matches!(t, TokenType::Comma),
                nom::error::ErrorKind::Char,
            )(input)?;
            log::debug!("Found comma, continuing to next field");
            Ok((input, ()))
        },
        parse_struct_field,
    )(input) {
        Ok((input, fields)) => {
            log::debug!("Successfully parsed {} fields", fields.len());
            (input, fields)
        },
        Err(e) => {
            log::error!("Failed to parse struct fields: {:?}", e);
            return Err(e);
        }
    };
    
    // Parse the closing brace
    log::debug!("Looking for closing brace...");
    let (input, _) = match take_token_if(
        |t| matches!(t, TokenType::RightBrace),
        nom::error::ErrorKind::Char,
    )(input) {
        Ok((input, _)) => {
            log::debug!("Found closing brace");
            (input, ())
        },
        Err(e) => {
            log::error!("Expected '}}' after struct fields: {:?}", e);
            return Err(e);
        }
    };
    
    Ok((
        input,
        ExpressionNode::Struct(Box::new(StructLiteralNode {
            type_name,
            fields,
 })),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::TokenSlice;
    use medic_lexer::{token::Token, Location};
    use medic_ast::ast::LiteralNode;

    #[test]
    fn test_parse_struct_literal() {
        let tokens = vec![
            Token::new(TokenType::Identifier("Point".into()), "Point", Location { line: 1, column: 1, offset: 0 }),
            Token::new(TokenType::LeftBrace, "{", Location { line: 1, column: 7, offset: 6 }),
            Token::new(TokenType::Identifier("x".into()), "x", Location { line: 1, column: 8, offset: 7 }),
            Token::new(TokenType::Colon, ":", Location { line: 1, column: 9, offset: 8 }),
            Token::new(TokenType::Integer(10), "10", Location { line: 1, column: 11, offset: 10 }),
            Token::new(TokenType::Comma, ",", Location { line: 1, column: 13, offset: 12 }),
            Token::new(TokenType::Identifier("y".into()), "y", Location { line: 1, column: 15, offset: 14 }),
            Token::new(TokenType::Colon, ":", Location { line: 1, column: 16, offset: 15 }),
            Token::new(TokenType::Integer(20), "20", Location { line: 1, column: 18, offset: 17 }),
            Token::new(TokenType::RightBrace, "}", Location { line: 1, column: 20, offset: 19 }),
        ];
        
        let input = TokenSlice::new(&tokens);
        let result = parse_struct_literal(input);
        assert!(result.is_ok());
        
        let (remaining, expr) = result.unwrap();
        assert!(remaining.is_empty());
        
        if let ExpressionNode::Struct(s) = expr {
            assert_eq!(s.type_name, "Point");
            assert_eq!(s.fields.len(), 2);
            assert_eq!(s.fields[0].name, "x");
            assert_eq!(s.fields[1].name, "y");
        } else {
            panic!("Expected Struct variant");
        }
    }
}
