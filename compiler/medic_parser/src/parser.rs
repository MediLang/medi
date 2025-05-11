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
        BinaryOperator::Or => 1,
        BinaryOperator::And => 2,
        BinaryOperator::Eq | BinaryOperator::Neq => 3,
        BinaryOperator::Lt | BinaryOperator::Gt | BinaryOperator::Le | BinaryOperator::Ge => 4,
        BinaryOperator::Add | BinaryOperator::Sub => 5,
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 6,
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

fn is_healthcare_query(name: &str) -> bool {
    matches!(name, "fhir_query" | "kaplan_meier" | "regulate" | "report")
}

fn parse_member_expr(input: &str) -> IResult<&str, ExpressionNode> {
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
    let parse_snomed_code = map(
        tuple((
            tag::<&str, &str, NomError<&str>>("SNOMED:"),
            take_while_m_n::<_, _, NomError<&str>>(6, 9, |c: char| c.is_ascii_digit()),
        )),
        |(prefix, digits): (&str, &str)| {
            let mut s = String::from(prefix);
            s.push_str(digits);
            ExpressionNode::SnomedCode(s)
        },
    );
    if let Ok((rest, expr)) = alt((parse_icd_code, parse_cpt_code, parse_snomed_code))(input) {
        return Ok((rest, expr));
    }
    let (mut input, mut expr) = alt((
        parse_bool_literal,
        parse_float_literal,
        parse_string_literal,
        parse_identifier,
        parse_paren_expr,
    ))(input)?;
    println!(
        "[DEBUG] After primary parse: next input = {:?}",
        &input[..input.len().min(20)]
    );
    loop {
        // Always attempt to parse function/healthcare call (allowing optional whitespace before '(')
        println!(
            "[DEBUG] Attempting parse_call_args on input: {:?}",
            &input[..input.len().min(20)]
        );
        let call_res = parse_call_args(input);
        println!("[DEBUG] Result of parse_call_args: {:?}", call_res);
        if let Ok((next_input, args)) = call_res {
            if let ExpressionNode::Identifier(name) = &expr {
                if is_healthcare_query(name) {
                    expr = ExpressionNode::HealthcareQuery(Box::new(ast::HealthcareQueryNode {
                        query_type: name.clone(),
                        arguments: args,
                    }));
                } else {
                    expr = ExpressionNode::Call(Box::new(ast::CallExpressionNode {
                        callee: expr,
                        arguments: args,
                    }));
                }
            }
            input = next_input;
            continue;
        }
        // Member access
        let res = preceded(preceded(multispace0, char('.')), parse_identifier)(input);
        if let Ok((next_input, ExpressionNode::Identifier(prop))) = res {
            expr = ExpressionNode::Member(Box::new(ast::MemberExpressionNode {
                object: expr,
                property: prop,
            }));
            input = next_input;
        } else {
            break;
        }
    }
    Ok((input, expr))
}

fn parse_primary_expr(input: &str) -> IResult<&str, ExpressionNode> {
    parse_member_expr(input)
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
    let (input, target) = parse_member_expr(input)?;
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
    let (input, stmts) = delimited(
        preceded(multispace0, char('{')),
        many0(parse_statement),
        preceded(multispace0, char('}')),
    )(input)?;
    Ok((input, StatementNode::Block(BlockNode { statements: stmts })))
}

fn parse_if_statement(input: &str) -> IResult<&str, StatementNode> {
    let (input, _) = preceded(multispace0, tag("if"))(input)?;
    let (input, cond) = delimited(
        preceded(multispace0, char('(')),
        parse_expression,
        preceded(multispace0, char(')')),
    )(input)?;
    let (input, then_branch) = parse_block(input)?;
    let (input, else_branch) = {
        let input_trim = preceded(
            multispace0::<&str, NomError<&str>>,
            tag::<&str, &str, NomError<&str>>("else"),
        )(input);
        if let Ok((input, _)) = input_trim {
            let (input, else_block) = parse_block(input)?;
            match else_block {
                StatementNode::Block(b) => (input, Some(b)),
                _ => {
                    return Err(nom::Err::Failure(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )));
                }
            }
        } else {
            (input, None)
        }
    };
    Ok((
        input,
        StatementNode::If(Box::new(IfNode {
            condition: cond,
            then_branch: match then_branch {
                StatementNode::Block(b) => b,
                _ => {
                    return Err(nom::Err::Error(nom::error::Error::new(
                        input,
                        nom::error::ErrorKind::Tag,
                    )));
                }
            },
            else_branch,
        })),
    ))
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
    let (input, cond) = delimited(
        preceded(multispace0, char('(')),
        parse_expression,
        preceded(multispace0, char(')')),
    )(input)?;
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
    let var = if let ExpressionNode::Identifier(n) = var_expr {
        n
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        )));
    };
    let (input, _) = preceded(multispace0, tag("in"))(input)?;
    let (input, iter) = parse_expression(input)?;
    let (input, body) = parse_block(input)?;
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
    let (input, expr) = parse_expression(input)?;
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
        let (ni, pat) = match parse_pattern(next_input) {
            Ok(v) => v,
            Err(_) => {
                return Err(nom::Err::Failure(nom::error::Error::new(
                    next_input,
                    nom::error::ErrorKind::Tag,
                )));
            }
        };
        let (ni, _) = preceded(multispace0, tag("=>"))(ni)?;
        let (ni, block) = parse_block(ni)?;
        match block {
            StatementNode::Block(b) => arms.push((pat, b)),
            _ => {
                return Err(nom::Err::Failure(nom::error::Error::new(
                    ni,
                    nom::error::ErrorKind::Tag,
                )));
            }
        }
        input = ni;
        let (ni2, _) = multispace0::<&str, NomError<&str>>(input)?;
        input = ni2;
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
mod tests {
    use super::*;
    use medic_ast::ast::{
        AssignmentNode, BlockNode, ForNode, IfNode, LetStatementNode, MatchNode, ReturnNode,
        StatementNode, WhileNode,
    };

    #[test]
    fn test_let_statement() {
        let input = "let x = 42;";
        let (_rest, stmt) = parse_let_statement(input).unwrap();
        assert_eq!(
            stmt,
            StatementNode::Let(Box::new(LetStatementNode {
                name: "x".to_string(),
                value: ExpressionNode::Literal(LiteralNode::Int(42)),
            }))
        );
    }

    #[test]
    fn test_assignment_statement() {
        let input = "x = 5;";
        let (_rest, stmt) = parse_assignment(input).unwrap();
        assert_eq!(
            stmt,
            StatementNode::Assignment(Box::new(AssignmentNode {
                target: ExpressionNode::Identifier("x".to_string()),
                value: ExpressionNode::Literal(LiteralNode::Int(5)),
            }))
        );
    }

    #[test]
    fn test_block_statement() {
        let input = "{ let x = 1; x = 2; }";
        let (_rest, stmt) = parse_block(input).unwrap();
        assert_eq!(
            stmt,
            StatementNode::Block(BlockNode {
                statements: vec![
                    StatementNode::Let(Box::new(LetStatementNode {
                        name: "x".to_string(),
                        value: ExpressionNode::Literal(LiteralNode::Int(1)),
                    })),
                    StatementNode::Assignment(Box::new(AssignmentNode {
                        target: ExpressionNode::Identifier("x".to_string()),
                        value: ExpressionNode::Literal(LiteralNode::Int(2)),
                    })),
                ]
            })
        );
    }

    #[test]
    fn test_if_else_statement() {
        let input = "if (x > 0) { let y = 1; } else { let y = 2; }";
        let (_rest, stmt) = parse_if_statement(input).unwrap();
        assert_eq!(
            stmt,
            StatementNode::If(Box::new(IfNode {
                condition: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                    left: ExpressionNode::Identifier("x".to_string()),
                    operator: BinaryOperator::Gt,
                    right: ExpressionNode::Literal(LiteralNode::Int(0)),
                })),
                then_branch: BlockNode {
                    statements: vec![StatementNode::Let(Box::new(LetStatementNode {
                        name: "y".to_string(),
                        value: ExpressionNode::Literal(LiteralNode::Int(1)),
                    }))]
                },
                else_branch: Some(BlockNode {
                    statements: vec![StatementNode::Let(Box::new(LetStatementNode {
                        name: "y".to_string(),
                        value: ExpressionNode::Literal(LiteralNode::Int(2)),
                    }))]
                })
            }))
        );
    }

    #[test]
    fn test_while_statement() {
        let input = "while (x < 10) { x = x + 1; }";
        let (_rest, stmt) = parse_while_statement(input).unwrap();
        match stmt {
            StatementNode::While(wn) => {
                assert_eq!(
                    wn.condition,
                    ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                        left: ExpressionNode::Identifier("x".to_string()),
                        operator: BinaryOperator::Lt,
                        right: ExpressionNode::Literal(LiteralNode::Int(10)),
                    }))
                );
                assert_eq!(wn.body.statements.len(), 1);
            }
            _ => panic!("Expected while statement"),
        }
    }

    #[test]
    fn test_for_statement() {
        let input = "for i in patients { let x = i; }";
        let (_rest, stmt) = parse_for_statement(input).unwrap();
        match stmt {
            StatementNode::For(fn_) => {
                assert_eq!(fn_.var, "i");
                assert_eq!(fn_.body.statements.len(), 1);
            }
            _ => panic!("Expected for statement"),
        }
    }

    #[test]
    fn test_match_statement() {
        let input = "match x { 1 => { let y = 10; } 2 => { let y = 20; } }";
        let (_rest, stmt) = parse_match_statement(input).unwrap();
        match stmt {
            StatementNode::Match(mn) => {
                assert_eq!(mn.arms.len(), 2);
            }
            _ => panic!("Expected match statement"),
        }
    }

    #[test]
    fn test_return_statement() {
        let input = "return 42;";
        let (_rest, stmt) = parse_return_statement(input).unwrap();
        match stmt {
            StatementNode::Return(rn) => {
                assert_eq!(
                    rn.value,
                    Some(ExpressionNode::Literal(LiteralNode::Int(42)))
                );
            }
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_return_unit_statement() {
        let input = "return;";
        let (_rest, stmt) = parse_return_statement(input).unwrap();
        match stmt {
            StatementNode::Return(rn) => {
                assert_eq!(rn.value, None);
            }
            _ => panic!("Expected return statement"),
        }
    }

    #[test]
    fn test_bool_literal() {
        let input = "true";
        let (_rest, expr) = parse_bool_literal(input).unwrap();
        assert_eq!(expr, ExpressionNode::Literal(LiteralNode::Bool(true)));
        let input = "false";
        let (_rest, expr) = parse_bool_literal(input).unwrap();
        assert_eq!(expr, ExpressionNode::Literal(LiteralNode::Bool(false)));
    }

    #[test]
    fn test_float_literal() {
        let input = "3.14";
        let (_rest, expr) = parse_float_literal(input).unwrap();
        assert_eq!(expr, ExpressionNode::Literal(LiteralNode::Float(3.14)));
    }

    #[test]
    fn test_icd_code() {
        let input = "ICD10:E11.9";
        let (_rest, expr) = parse_primary_expr(input).unwrap();
        assert_eq!(expr, ExpressionNode::IcdCode("ICD10:E11.9".to_string()));
    }

    #[test]
    fn test_cpt_code() {
        let input = "CPT:99213";
        let (_rest, expr) = parse_primary_expr(input).unwrap();
        assert_eq!(expr, ExpressionNode::CptCode("CPT:99213".to_string()));
    }

    #[test]
    fn test_snomed_code() {
        let input = "SNOMED:44054006";
        let (_rest, expr) = parse_primary_expr(input).unwrap();
        assert_eq!(
            expr,
            ExpressionNode::SnomedCode("SNOMED:44054006".to_string())
        );
    }
}
