use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::{take_while, take_while1};
use nom::character::{is_alphabetic, is_digit};
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use std::str;

use crate::interpreter::*;

fn integer(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = take_while1(is_digit)(input);
    match ret {
        Ok((input, digit)) => {
            let value = i32::from_str_radix(str::from_utf8(digit).unwrap(), 10).unwrap();
            Ok((
                input,
                Expression::Literal(Box::new(Literal::Integer(value))),
            ))
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_integer() {
    let (input, exp) = integer(b"12345 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(Box::new(Literal::Integer(12345))), exp);

    integer(b"").unwrap_err();
    integer(b"abc").unwrap_err();
}

fn identifier(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = take_while1(is_alphabetic)(input);
    match ret {
        Ok((input, s)) => {
            let value = str::from_utf8(s).unwrap();
            Ok((input, Expression::Identifier(value.to_string())))
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_identifier() {
    let (input, exp) = identifier(b"abc ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Identifier("abc".to_string()), exp);

    identifier(b"").unwrap_err();
    identifier(b"12345").unwrap_err();
}

fn program(input: &[u8]) -> IResult<&[u8], Vec<TopLevel>> {
    many0((top_level_definition))(input)
}

fn top_level_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    alt((function_definition, global_variable_definition))(input)
}

fn function_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    match tuple((
        tag("define"),
        identifier,
        tag("("),
        identifier,
        many0(tuple((tag(","), identifier))),
        tag(")"),
        block_expression,
    ))(input)
    {
        Ok((input, (_, id, _, arg0, args_pairs, _, block))) => {
            if let Expression::Identifier(s) = id {
                let mut args = vec![];
                if let Expression::Identifier(name) = arg0 {
                    args.push(name);
                } else {
                    panic!();
                }
                for arg in args_pairs {
                    if let Expression::Identifier(name) = arg.1 {
                        args.push(name);
                    } else {
                        panic!();
                    }
                }
                Ok((
                    input,
                    TopLevel::FunctionDefinition {
                        name: s,
                        args: args,
                        body: block,
                    },
                ))
            } else {
                panic!();
            }
        }
        Err(e) => Err(e),
    }
}

fn global_variable_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    match tuple((tag("global"), identifier, tag("="), expression))(input) {
        Ok((input, (_, id, _, exp))) => {
            if let Expression::Identifier(s) = id {
                Ok((
                    input,
                    TopLevel::GlobalVariableDefinition {
                        name: s.to_string(),
                        expression: Box::new(exp),
                    },
                ))
            } else {
                panic!();
            }
        }
        Err(e) => Err(e),
    }
}

fn lines(input: &[u8]) -> IResult<&[u8], Vec<Expression>> {
    many0(line)(input)
}

fn line(input: &[u8]) -> IResult<&[u8], Expression> {
    alt((
        while_expression,
        if_expression,
        assignment,
        expression_line,
        block_expression,
    ))(input)
}

fn if_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((tag("if"), tag("("), expression, tag(")"), line))(input) {
        Ok((input, (_, _, exp, _, then))) => match tuple((tag("else"), line))(input) {
            Ok((input, (_, el))) => Ok((
                input,
                Expression::If {
                    condition: Box::new(exp),
                    then_clause: Box::new(then),
                    else_clause: Some(Box::new(el)),
                },
            )),
            Err(_) => Ok((
                input,
                Expression::If {
                    condition: Box::new(exp),
                    then_clause: Box::new(then),
                    else_clause: None,
                },
            )),
        },
        Err(e) => Err(e),
    }
}

fn while_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((tag("while"), tag("("), expression, tag(")"), line))(input) {
        Ok((input, (_, _, exp, _, line))) => {
            return Ok((
                input,
                Expression::While {
                    condition: Box::new(exp),
                    body: Box::new(line),
                },
            ))
        }
        Err(e) => Err(e),
    }
}

fn block_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((tag("{"), many0(line), tag("}")))(input) {
        Ok((input, (_, lines, _))) => Ok((input, Expression::Block { expressions: lines })),
        Err(e) => Err(e),
    }
}

fn assignment(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((identifier, tag("="), expression, tag(";")))(input) {
        Ok((input, (id, _, exp, _))) => {
            if let Expression::Identifier(s) = id {
                return Ok((
                    input,
                    Expression::Assignment {
                        name: s.to_string(),
                        expression: Box::new(exp),
                    },
                ));
            } else {
                panic!();
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_assignment() {
    let (input, exp) = assignment(b"abc=12345; ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::Assignment {
            name: "abc".to_string(),
            expression: Box::new(Expression::Literal(Box::new(Literal::Integer(12345))))
        },
        exp
    );
}

fn expression_line(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((expression, tag(";")))(input) {
        Ok((input, (exp, _))) => Ok((input, exp)),
        Err(e) => Err(e),
    }
}

#[test]
fn test_expression_line() {
    let (input, exp) = expression_line(b"12345; ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(Box::new(Literal::Integer(12345))), exp);
}

fn expression(input: &[u8]) -> IResult<&[u8], Expression> {
    comparative(input)
}

fn comparative(input: &[u8]) -> IResult<&[u8], Expression> {
    match additive(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((
                alt((tag("<"), tag(">"), tag("<="), tag(">="), tag("=="))),
                additive,
            )))(input);

            let mut cur = exp;
            match ret {
                Ok((input, v)) => {
                    for elem in v {
                        let (op, ex) = elem;
                        match op {
                            b"<" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::LessThan,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b">" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::GreaterThan,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b"<=" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::LessOrEqual,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b">=" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::GreaterOrEqual,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b"==" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::EqualEqual,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b"!=" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::NotEqual,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            _ => {
                                panic!();
                            }
                        }
                    }

                    Ok((input, cur))
                }
                Err(e) => Err(e),
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_comparative() {
    let (input, exp) = comparative(b"1<2 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Expression::Literal(Box::new(Literal::Integer(1)))),
            rhs: Box::new(Expression::Literal(Box::new(Literal::Integer(2)))),
        },
        exp
    );
}

fn additive(input: &[u8]) -> IResult<&[u8], Expression> {
    match multitive(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((alt((tag("+"), tag("-"))), multitive)))(input);

            let mut cur = exp;
            match ret {
                Ok((input, v)) => {
                    for elem in v {
                        let (op, ex) = elem;
                        match op {
                            b"+" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b"-" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::Subtract,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            _ => {
                                panic!();
                            }
                        }
                    }

                    Ok((input, cur))
                }
                Err(e) => Err(e),
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_additive() {
    let (input, exp) = additive(b"1+2 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(Expression::Literal(Box::new(Literal::Integer(1)))),
            rhs: Box::new(Expression::Literal(Box::new(Literal::Integer(2)))),
        },
        exp
    );
}

fn multitive(input: &[u8]) -> IResult<&[u8], Expression> {
    match primary(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((alt((tag("*"), tag("/"))), primary)))(input);

            let mut cur = exp;
            match ret {
                Ok((input, v)) => {
                    for elem in v {
                        let (op, ex) = elem;
                        match op {
                            b"*" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::Multiply,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            b"/" => {
                                cur = Expression::BinaryExpression {
                                    operator: Operator::Divide,
                                    lhs: Box::new(cur),
                                    rhs: Box::new(ex),
                                };
                            }
                            _ => {
                                panic!();
                            }
                        }
                    }

                    Ok((input, cur))
                }
                Err(e) => Err(e),
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_multitive() {
    let (input, exp) = multitive(b"1*2 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::BinaryExpression {
            operator: Operator::Multiply,
            lhs: Box::new(Expression::Literal(Box::new(Literal::Integer(1)))),
            rhs: Box::new(Expression::Literal(Box::new(Literal::Integer(2)))),
        },
        exp
    );
}

fn primary(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = tuple((tag("("), expression, tag(")")))(input);
    if let Ok((input, (_, exp, _))) = ret {
        return Ok((input, exp));
    }

    let ret = integer(input);
    if let Ok((input, exp)) = ret {
        return Ok((input, exp));
    }

    let ret = function_call(input);
    if let Ok((input, exp)) = ret {
        return Ok((input, exp));
    }

    identifier(input)
}

#[test]
fn test_primary() {
    let (input, exp) = primary(b"(42) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(Box::new(Literal::Integer(42))), exp);

    let (input, exp) = primary(b"42 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(Box::new(Literal::Integer(42))), exp);

    let (input, exp) = primary(b"func(123) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::FunctionCall {
            name: "func".to_string(),
            args: vec![Expression::Literal(Box::new(Literal::Integer(123)))],
        },
        exp
    );

    let (input, exp) = primary(b"abc ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Identifier("abc".to_string()), exp);
}

fn function_call(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((identifier, tag("(")))(input) {
        Ok((input, (name, _))) => {
            if let Expression::Identifier(name2) = name {
                match expression(input) {
                    Ok((input, exp1)) => {
                        let ret = tuple((many0(tuple((tag(","), expression))), tag(")")))(input);
                        match ret {
                            Ok((input, (v, _))) => {
                                let mut args = vec![exp1];
                                for pair in v {
                                    args.push(pair.1);
                                }
                                Ok((
                                    input,
                                    Expression::FunctionCall {
                                        name: name2,
                                        args: args,
                                    },
                                ))
                            }
                            Err(e) => Err(e),
                        }
                    }
                    Err(e) => Err(e),
                }
            } else {
                panic!()
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_function_call() {
    let (input, exp) = function_call(b"func(123) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::FunctionCall {
            name: "func".to_string(),
            args: vec![Expression::Literal(Box::new(Literal::Integer(123)))],
        },
        exp
    );
}
