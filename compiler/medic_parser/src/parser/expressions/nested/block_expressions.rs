use nom::error::ErrorKind;
use nom::IResult;

use crate::parser::{take_token_if, BlockNode, StatementNode, TokenSlice, TokenType};

/// Parses a block expression: `{ <statements> }`
pub fn parse_block_expression(input: TokenSlice<'_>) -> IResult<TokenSlice<'_>, BlockNode> {
    // Parse the opening brace
    let (mut input, _) =
        take_token_if(|t| matches!(t, TokenType::LeftBrace), ErrorKind::Tag)(input)?;

    let mut statements = Vec::new();

    // Parse statements until we hit a right brace or end of input
    loop {
        // Check for end of input
        if input.is_empty() {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Eof,
            )));
        }

        // Check for right brace
        if matches!(
            input.peek().map(|t| &t.token_type),
            Some(TokenType::RightBrace)
        ) {
            break;
        }
        // Skip any leading semicolons
        while let Some(TokenType::Semicolon) = input.peek().map(|t| &t.token_type) {
            let (new_input, _) =
                take_token_if(|t| matches!(t, TokenType::Semicolon), ErrorKind::Tag)(input)?;
            input = new_input;
        }

        // If we hit the right brace after skipping semicolons, we're done
        if matches!(
            input.peek().map(|t| &t.token_type),
            Some(TokenType::RightBrace)
        ) {
            break;
        }

        // Parse the statement (which should handle its own semicolon)
        let (new_input, stmt) = super::super::super::parse_statement(input)?;
        input = new_input;
        statements.push(stmt);

        // Don't consume semicolons here - let the statement parser handle them
        // This allows for statements that don't require semicolons (like blocks)
    }

    // Consume the right brace
    let (input, _) = take_token_if(|t| matches!(t, TokenType::RightBrace), ErrorKind::Tag)(input)?;

    Ok((input, BlockNode { statements }))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::test_utils::tokenize;
    use medic_ast::ast::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse_block_expression_unterminated() {
        let input = "{ let x = 5 "; // Missing closing brace
        let tokens = tokenize(input);
        let token_slice = TokenSlice::new(&tokens);
        let result = parse_block_expression(token_slice);

        // Should return an error for unterminated block
        assert!(
            result.is_err(),
            "Expected error for unterminated block expression"
        );
    }

    #[test]
    fn test_parse_block_expression() {
        let input = "{ let x = 5; }";
        let tokens = tokenize(input);
        eprintln!("Tokens: {:?}", tokens);

        let slice = TokenSlice::new(&tokens);
        eprintln!("Token slice: {:?}", slice);

        let result = parse_block_expression(slice);

        eprintln!("Parse result: {:?}", result);

        assert!(result.is_ok(), "Expected Ok, got {:?}", result);

        let (remaining, block) = result.unwrap();
        assert!(
            remaining.is_empty(),
            "Expected no remaining tokens, got {:?}",
            remaining
        );
        assert_eq!(block.statements.len(), 1, "Expected 1 statement in block");

        // Verify the statement is a let statement
        if let StatementNode::Let(let_stmt) = &block.statements[0] {
            assert_eq!(let_stmt.name.name, "x", "Expected variable name 'x'");

            // Access the expression value correctly
            match &let_stmt.value {
                ExpressionNode::Literal(lit) => {
                    if let LiteralNode::Int(5) = lit {
                        // Success!
                        eprintln!("Successfully parsed block with let x = 5;");
                    } else {
                        panic!("Expected integer literal 5, got {:?}", lit);
                    }
                }
                _ => panic!("Expected literal expression, got {:?}", let_stmt.value),
            }
        } else {
            panic!(
                "Expected let statement in block, got {:?}",
                block.statements[0]
            );
        }
    }
}
