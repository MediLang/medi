// Parser implementation for Medi language using nom (industry standard)
// Expands to handle literals, identifiers, parenthesized expressions, and scaffolds for more

use crate::ast::*;
use nom::{IResult, branch::alt, bytes::complete::tag, character::complete::{alphanumeric1, digit1, multispace0, char}, combinator::{map, map_res}, sequence::{delimited, preceded}, multi::many0};

pub fn parse_int_literal(input: &str) -> IResult<&str, ExpressionNode> {
    map_res(digit1, |s: &str| s.parse::<i64>().map(|i| ExpressionNode::Literal(LiteralNode::Int(i))))(input)
}

pub fn parse_identifier(input: &str) -> IResult<&str, ExpressionNode> {
    map(alphanumeric1, |s: &str| ExpressionNode::Identifier(s.to_string()))(input)
}

pub fn parse_paren_expr(input: &str) -> IResult<&str, ExpressionNode> {
    delimited(
        preceded(multispace0, char('(')),
        parse_expression,
        preceded(multispace0, char(')')),
    )(input)
}

pub fn parse_expression(input: &str) -> IResult<&str, ExpressionNode> {
    // For now, just parse literals, identifiers, or parenthesized expressions
    preceded(
        multispace0,
        alt((parse_int_literal, parse_identifier, parse_paren_expr)),
    )(input)
}

// TODO: Add parsing for binary expressions, function calls, member expressions, statements, etc.
