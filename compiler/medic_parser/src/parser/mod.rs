// Parser implementation for Medic language using nom (industry standard)
// Expands to handle literals, identifiers, parenthesized expressions, and scaffolds for more

use medic_ast::ast; // For ast::TypeName references
use medic_ast::ast::*;
use nom::character::complete::{alpha1, digit1};
use nom::combinator::opt;
use nom::error::Error as NomError;
use nom::IResult;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char, multispace0},
    combinator::{map, map_res},
    multi::{many0, separated_list0},
    sequence::{delimited, preceded, tuple},
};

pub fn parse_int_literal(input: &str) -> IResult<&str, ExpressionNode> {
    preceded(
        multispace0,
        map_res(digit1, |s: &str| {
            s.parse::<i64>()
                .map(|i| ExpressionNode::Literal(LiteralNode::Int(i)))
        }),
    )(input)
}

use nom::combinator::recognize;
use nom::{bytes::complete::take_while, sequence::pair};

pub fn parse_identifier(input: &str) -> IResult<&str, ExpressionNode> {
    preceded(
        multispace0,
        map(
            recognize(pair(
                alt((recognize(char('_')), alpha1)),
                take_while(|c: char| c.is_alphanumeric() || c == '_'),
            )),
            |s: &str| ExpressionNode::Identifier(s.to_string()),
        ),
    )(input)
}

pub fn parse_string_literal(input: &str) -> IResult<&str, ExpressionNode> {
    use nom::bytes::complete::is_not;
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::delimited;
    map(
        delimited(preceded(multispace0, char('"')), is_not("\""), char('"')),
        |s: &str| ExpressionNode::Literal(LiteralNode::String(s.to_string())),
    )(input)
}

fn parse_paren_expr(input: &str) -> IResult<&str, ExpressionNode> {
    delimited(
        preceded(multispace0, char('(')),
        parse_expression,
        preceded(multispace0, char(')')),
    )(input)
}

// ---- Binary Expression Parsing with Precedence ----
use medic_ast::ast::BinaryOperator;

fn get_precedence(op: &BinaryOperator) -> u8 {
    match op {
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 5,
        BinaryOperator::Add | BinaryOperator::Sub => 4,
        BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Le | BinaryOperator::Ge => 3,
        BinaryOperator::Eq | BinaryOperator::Neq => 2,
        BinaryOperator::And => 1,
        BinaryOperator::Or => 1,
        BinaryOperator::Range => 1,
        BinaryOperator::Assign => 0, // Assignment is lowest precedence
    }
}

fn parse_operator(input: &str) -> IResult<&str, BinaryOperator> {
    preceded(
        multispace0,
        alt((
            // All tag combinators use &str input, which is correct for nom 7 and &str parsers
            map(tag("+"), |_| BinaryOperator::Add),
            map(tag("-"), |_| BinaryOperator::Sub),
            map(tag("*"), |_| BinaryOperator::Mul),
            map(tag("/"), |_| BinaryOperator::Div),
            map(tag("%"), |_| BinaryOperator::Mod),
            map(tag("=="), |_| BinaryOperator::Eq),
            map(tag("!="), |_| BinaryOperator::Neq),
            map(tag("<="), |_| BinaryOperator::Le),
            map(tag(">="), |_| BinaryOperator::Ge),
            map(tag("<"), |_| BinaryOperator::Lt),
            map(tag(">"), |_| BinaryOperator::Gt),
            map(tag("&&"), |_| BinaryOperator::And),
            map(tag("||"), |_| BinaryOperator::Or),
            map(tag("="), |_| BinaryOperator::Assign),
            map(tag(".."), |_| BinaryOperator::Range),
        )),
    )(input)
}

// Precedence climbing parser for binary expressions
fn parse_binary_expr(input: &str, min_prec: u8) -> IResult<&str, ExpressionNode> {
    let (mut input, mut lhs) = parse_primary_expr(input)?;
    loop {
        let op_res = parse_operator(input);
        if let Ok((next_input, op)) = op_res {
            let prec = get_precedence(&op);
            if prec < min_prec {
                break;
            }
            // Consume operator
            let (next_input, rhs) = parse_binary_expr(next_input, prec + 1)?;
            lhs = ExpressionNode::Binary(Box::new(ast::BinaryExpressionNode {
                left: lhs.clone(),
                operator: op,
                right: rhs,
            }));
            input = next_input;
        } else {
            break;
        }
    }
    Ok((input, lhs))
}

fn parse_call_args(input: &str) -> IResult<&str, Vec<ExpressionNode>> {
    delimited(
        preceded(multispace0, char('(')),
        separated_list0(preceded(multispace0, char(',')), parse_expression),
        preceded(multispace0, char(')')),
    )(input)
}

pub fn parse_member_expr(input: &str) -> IResult<&str, ExpressionNode> {
    use nom::branch::alt;
    use nom::bytes::complete::take_while_m_n;
    use nom::character::complete::char;
    use nom::combinator::map;
    use nom::sequence::{preceded, tuple};

    // ICD10:ICD10:[A-Z][0-9][0-9AB](\.[0-9A-Z]{1,4})?
    let parse_icd_code = map(
        tuple((
            tag::<&str, &str, NomError<&str>>("ICD10:"),
            take_while_m_n::<_, _, NomError<&str>>(1, 1, |c: char| c.is_ascii_uppercase()),
            take_while_m_n::<_, _, NomError<&str>>(2, 2, |c: char| {
                c.is_ascii_digit() || c == 'A' || c == 'B'
            }),
            opt(preceded(
                char::<&str, NomError<&str>>('.'),
                take_while_m_n::<_, _, NomError<&str>>(1, 4, |c: char| {
                    c.is_ascii_digit() || c.is_ascii_uppercase()
                }),
            )),
        )),
        |(prefix, letter, digits, opt_ext): (&str, &str, &str, Option<&str>)| {
            let mut s = String::from(prefix);
            s.push_str(letter);
            s.push_str(digits);
            if let Some(ext) = opt_ext {
                s.push('.');
                s.push_str(ext);
            }
            ExpressionNode::IcdCode(s)
        },
    );
    // CPT:CPT:[0-9]{5}
    let parse_cpt_code = map(
        tuple((
            tag::<&str, &str, NomError<&str>>("CPT:"),
            take_while_m_n::<_, _, NomError<&str>>(5, 5, |c: char| c.is_ascii_digit()),
        )),
        |(prefix, digits): (&str, &str)| {
            let mut s = String::from(prefix);
            s.push_str(digits);
            ExpressionNode::CptCode(s)
        },
    );
    // SNOMED:SNOMED:[0-9]{6,9}
    let snomed_code = map(
        pair(
            tag::<&str, &str, NomError<&str>>("SNOMED:"),
            take_while_m_n::<_, _, NomError<&str>>(6, 9, |c: char| c.is_ascii_digit()),
        ),
        |(prefix, digits): (&str, &str)| {
            let mut s = String::from(prefix);
            s.push_str(digits);
            ExpressionNode::SnomedCode(s)
        },
    );

    if let Ok((rest, expr)) = alt((parse_icd_code, parse_cpt_code, snomed_code))(input) {
        return Ok((rest, expr));
    }

    let (input, expr) = parse_primary_expr(input)?;
    let mut current_input = input;
    let mut current_expr = expr;

    loop {
        // Try to parse member access
        let member_access = preceded::<_, _, _, nom::error::Error<_>, _, _>(
            multispace0,
            pair(char('.'), parse_identifier),
        )(current_input);

        match member_access {
            Ok((next_input, (_, ExpressionNode::Identifier(property)))) => {
                current_input = next_input;
                current_expr = ExpressionNode::Member(Box::new(MemberExpressionNode {
                    object: current_expr,
                    property,
                }));
            }
            Ok((_, _)) => break, // Any other expression type is not valid for member access
            Err(_) => break,
        }

        // Try to parse call arguments
        if let Ok((next_input, args)) = parse_call_args(current_input) {
            current_input = next_input;
            current_expr = ExpressionNode::Call(Box::new(CallExpressionNode {
                callee: current_expr,
                arguments: args,
            }));
        }
    }

    Ok((current_input, current_expr))
}

fn parse_primary_expr(input: &str) -> IResult<&str, ExpressionNode> {
    let (mut input, mut expr) = preceded(
        multispace0,
        alt((
            parse_bool_literal,
            parse_float_literal,
            parse_int_literal,
            parse_string_literal,
            parse_identifier,
            parse_paren_expr,
        )),
    )(input)?;

    // Try to parse member access
    loop {
        let member_access = preceded::<_, _, _, nom::error::Error<_>, _, _>(
            multispace0,
            pair(char('.'), parse_identifier),
        )(input);

        match member_access {
            Ok((next_input, (_, ExpressionNode::Identifier(property)))) => {
                input = next_input;
                expr = ExpressionNode::Member(Box::new(MemberExpressionNode {
                    object: expr,
                    property,
                }));
            }
            _ => break,
        }
    }

    Ok((input, expr))
}

// Top-level expression parser: parses full binary expressions

pub fn parse_expression(input: &str) -> IResult<&str, ExpressionNode> {
    parse_binary_expr(input, 0)
}

// ---- Statement Parsing ----
use medic_ast::ast::{
    AssignmentNode, BlockNode, ForNode, IfNode, LetStatementNode, MatchNode, ReturnNode,
    StatementNode,
};

fn parse_let_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, nom::bytes::complete::tag("let"))(input)?;
    let (input, name_expr) = preceded(multispace0, parse_identifier)(input)?;
    let name = if let ExpressionNode::Identifier(n) = name_expr {
        n
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    };
    let (input, _) = preceded(multispace0, char('='))(input)?;
    let (input, value) = parse_expression(input)?;
    let (input, _) = preceded(multispace0, char(';'))(input)?;
    Ok((
        input,
        StatementNode::Let(Box::new(LetStatementNode { name, value })),
    ))
}

fn parse_assignment(input: &str) -> IResult<&str, StatementNode> {
    let (input, target) = parse_identifier(input)?;
    let (input, _) = preceded(multispace0, char('='))(input)?;
    let (input, value) = parse_expression(input)?;
    let (input, _) = preceded(multispace0, char(';'))(input)?;
    Ok((
        input,
        StatementNode::Assignment(Box::new(AssignmentNode { target, value })),
    ))
}

fn parse_expr_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, expr) = parse_expression(input)?;
    let (input, _) = preceded(multispace0, char(';'))(input)?;
    Ok((input, StatementNode::Expr(expr)))
}

fn parse_block(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, char('{'))(input)?;
    let (input, mut stmts) = many0(parse_statement)(input)?;
    let (input, last_expr) = opt(preceded(multispace0, parse_expression))(input)?;
    let (input, _) = preceded(multispace0, char('}'))(input)?;

    if let Some(expr) = last_expr {
        stmts.push(StatementNode::Expr(expr));
    }
    Ok((input, StatementNode::Block(BlockNode { statements: stmts })))
}

fn parse_if_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, tag("if"))(input)?;
    let (input, cond) = preceded(multispace0, parse_expression)(input)?;
    let (input, then_branch) = parse_block(input)?;
    let (input, else_branch) = {
        let input_trim = preceded(
            multispace0::<&str, NomError<&str>>,
            tag::<&str, &str, NomError<&str>>("else"),
        )(input);
        if let Ok((input, _)) = input_trim {
            let (input, maybe_if) = opt(preceded(
                multispace0::<&str, NomError<&str>>,
                tag::<&str, &str, NomError<&str>>("if"),
            ))(input)?;

            if maybe_if.is_some() {
                // This is an else-if
                let (input, cond) = preceded(multispace0, parse_expression)(input)?;
                let (input, body) = parse_block(input)?;
                let (input, else_branch) = {
                    let input_trim = preceded(
                        multispace0::<&str, NomError<&str>>,
                        tag::<&str, &str, NomError<&str>>("else"),
                    )(input);
                    if let Ok((input, _)) = input_trim {
                        let (input, else_block) = parse_block(input)?;
                        match else_block {
                            StatementNode::Block(b) => Ok((input, Some(b))),
                            _ => Err(nom::Err::Error(nom::error::Error::new(
                                input,
                                nom::error::ErrorKind::Tag,
                            ))),
                        }
                    } else {
                        Ok((input, None))
                    }?
                };
                match body {
                    StatementNode::Block(then_block) => {
                        let if_stmt = StatementNode::If(Box::new(IfNode {
                            condition: cond,
                            then_branch: then_block,
                            else_branch,
                        }));
                        Ok((
                            input,
                            Some(BlockNode {
                                statements: vec![if_stmt],
                            }),
                        ))
                    }
                    _ => Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    ))),
                }
            } else {
                // This is a regular else block
                let (input, else_block) = parse_block(input)?;
                match else_block {
                    StatementNode::Block(b) => Ok((input, Some(b))),
                    _ => Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    ))),
                }
            }
        } else {
            Ok((input, None))
        }?
    };

    match then_branch {
        StatementNode::Block(then_block) => {
            if let Some(BlockNode {
                statements: else_statements,
            }) = else_branch
            {
                if else_statements.len() == 1 && matches!(else_statements[0], StatementNode::If(_))
                {
                    // If the else branch contains a single if statement, use its else branch
                    if let StatementNode::If(if_node) = &else_statements[0] {
                        Ok((
                            input,
                            StatementNode::If(Box::new(IfNode {
                                condition: cond,
                                then_branch: then_block,
                                else_branch: if_node.else_branch.clone(),
                            })),
                        ))
                    } else {
                        unreachable!()
                    }
                } else {
                    Ok((
                        input,
                        StatementNode::If(Box::new(IfNode {
                            condition: cond,
                            then_branch: then_block,
                            else_branch: Some(BlockNode {
                                statements: else_statements,
                            }),
                        })),
                    ))
                }
            } else {
                Ok((
                    input,
                    StatementNode::If(Box::new(IfNode {
                        condition: cond,
                        then_branch: then_block,
                        else_branch: None,
                    })),
                ))
            }
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_statement(input: &str) -> IResult<&str, StatementNode> {
    preceded(
        multispace0,
        alt((
            parse_let_statement,
            parse_if_statement,
            parse_while_statement,
            parse_for_statement,
            parse_match_statement,
            parse_return_statement,
            parse_block,
            parse_assignment,
            parse_expr_statement,
        )),
    )(input)
}

pub fn parse_program(input: &str) -> IResult<&str, Vec<StatementNode>> {
    many0(parse_statement)(input)
}

fn parse_bool_literal(input: &str) -> IResult<&str, ExpressionNode> {
    preceded(
        multispace0,
        alt((
            map(tag("true"), |_| {
                ExpressionNode::Literal(LiteralNode::Bool(true))
            }),
            map(tag("false"), |_| {
                ExpressionNode::Literal(LiteralNode::Bool(false))
            }),
        )),
    )(input)
}

fn parse_float_literal(input: &str) -> IResult<&str, ExpressionNode> {
    preceded(
        multispace0,
        map_res(recognize(tuple((digit1, char('.'), digit1))), |s: &str| {
            s.parse::<f64>()
                .map(|f| ExpressionNode::Literal(LiteralNode::Float(f)))
        }),
    )(input)
}

fn parse_while_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, tag("while"))(input)?;
    let (input, cond) = preceded(multispace0, parse_expression)(input)?;
    let (input, body) = parse_block(input)?;
    match body {
        StatementNode::Block(b) => Ok((
            input,
            StatementNode::While(Box::new(WhileNode {
                condition: cond,
                body: b,
            })),
        )),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_for_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, tag("for"))(input)?;
    let (input, var_expr) = preceded(multispace0, parse_identifier)(input)?;
    let var = match var_expr {
        ExpressionNode::Identifier(n) => n,
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                nom::error::ErrorKind::Tag,
            )))
        }
    };
    let (input, _) = preceded(multispace0, tag("in"))(input)?;
    let (input, iter) = preceded(multispace0, parse_expression)(input)?;
    let (input, body) = preceded(multispace0, parse_block)(input)?;
    match body {
        StatementNode::Block(b) => Ok((
            input,
            StatementNode::For(Box::new(ForNode { var, iter, body: b })),
        )),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_match_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, tag("match"))(input)?;
    let (input, expr) = preceded(multispace0, parse_expression)(input)?;
    let (mut input, _) = preceded(multispace0, char('{'))(input)?;
    let mut arms = Vec::new();
    loop {
        let (next_input, _) = multispace0::<&str, NomError<&str>>(input)?;
        if let Ok((after_brace, _)) = preceded(
            multispace0::<&str, NomError<&str>>,
            char::<&str, NomError<&str>>('}'),
        )(next_input)
        {
            input = after_brace;
            break;
        }
        let (ni, pat) = preceded(multispace0, parse_pattern)(next_input)?;
        let (ni, _) = preceded(multispace0, tag("=>"))(ni)?;
        let (ni, expr) = preceded(multispace0, parse_expression)(ni)?;
        let (ni, _) = opt(preceded(multispace0, char(',')))(ni)?;
        arms.push((
            pat,
            BlockNode {
                statements: vec![StatementNode::Expr(expr)],
            },
        ));
        input = ni;
    }
    Ok((
        input,
        StatementNode::Match(Box::new(MatchNode { expr, arms })),
    ))
}

fn parse_pattern(input: &str) -> IResult<&str, ExpressionNode> {
    preceded(
        multispace0,
        alt((
            parse_bool_literal,
            parse_float_literal,
            parse_int_literal,
            parse_string_literal,
            parse_identifier,
        )),
    )(input)
}

fn parse_return_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, tag("return"))(input)?;
    let (input, value) = if let Ok((input, expr)) = parse_expression(input) {
        let (input, _) = preceded(multispace0, char(';'))(input)?;
        (input, Some(expr))
    } else {
        let (input, _) = preceded(multispace0, char(';'))(input)?;
        (input, None)
    };
    Ok((input, StatementNode::Return(Box::new(ReturnNode { value }))))
}

// ---- End Statement Parsing ----

#[cfg(test)]
mod tests;
