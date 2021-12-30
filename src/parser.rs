use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::{take_while, take_while1};
use nom::character::complete::{digit1, multispace0, multispace1};
use nom::character::{is_alphabetic, is_digit};
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use std::str;

use crate::interpreter::*;

fn space0(input: &[u8]) -> IResult<&[u8], &[u8]> {
    multispace0(input)
}

fn space1(input: &[u8]) -> IResult<&[u8], &[u8]> {
    multispace1(input)
}

fn integer(input: &[u8]) -> IResult<&[u8], Expression> {
    match digit1(input) {
        Ok((input, digit)) => {
            let value = i32::from_str_radix(str::from_utf8(digit).unwrap(), 10).unwrap();
            Ok((input, Expression::Literal(value)))
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_integer() {
    let (input, exp) = integer(b"12345 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(12345), exp);

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

pub fn parse(input: String) -> Option<Vec<TopLevel>> {
    let target = input.as_bytes();
    match tuple((program, tag("__EOF__")))(target) {
        Ok((_, t)) => Some(t.0),
        Err(_) => None,
    }
}

fn program(input: &[u8]) -> IResult<&[u8], Vec<TopLevel>> {
    match many0(tuple((top_level_definition, space0)))(input) {
        Ok((input, top_level_vec)) => {
            let mut ret = Vec::new();
            for top in top_level_vec {
                ret.push(top.0);
            }
            Ok((input, ret))
        }
        Err(e) => Err(e),
    }
}

fn top_level_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    alt((function_definition, global_variable_definition))(input)
}

fn function_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    match tuple((
        tag("define"),
        space1,
        identifier,
        space0,
        tag("("),
        space0,
        opt(tuple((
            identifier,
            many0(tuple((space0, tag(","), space0, identifier))),
            space0,
        ))),
        tag(")"),
        space0,
        block_expression,
    ))(input)
    {
        Ok((input, (_, _, id, _, _, _, arg_opt, _, _, block))) => {
            if let Expression::Identifier(s) = id {
                let args = match arg_opt {
                    Some(t) => {
                        let mut args = vec![];
                        if let Expression::Identifier(arg0) = t.0 {
                            args.push(arg0);
                        } else {
                            panic!();
                        }

                        for arg in t.1 {
                            if let Expression::Identifier(argn) = arg.3 {
                                args.push(argn);
                            } else {
                                panic!();
                            }
                        }
                        args
                    }
                    None => Vec::new(),
                };

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

#[test]
fn test_function_definition() {
    let (input, exp) = function_definition(b"define func ( ) {1;} ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        TopLevel::FunctionDefinition {
            name: "func".to_string(),
            args: vec![],
            body: Expression::Block {
                expressions: vec![Expression::Literal(1),],
            },
        },
        exp
    );

    let (input, exp) = function_definition(b"define func ( abc ) {1;} ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        TopLevel::FunctionDefinition {
            name: "func".to_string(),
            args: vec!["abc".to_string()],
            body: Expression::Block {
                expressions: vec![Expression::Literal(1),],
            },
        },
        exp
    );
}

fn global_variable_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    match tuple((
        tag("global"),
        space1,
        identifier,
        space0,
        tag("="),
        space0,
        expression,
    ))(input)
    {
        Ok((input, (_, _, id, _, _, _, exp))) => {
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

#[test]
fn test_global_variable_definition() {
    let (input, exp) = global_variable_definition(b"global abc=123;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        TopLevel::GlobalVariableDefinition {
            name: "abc".to_string(),
            expression: Box::new(Expression::Literal(123)),
        },
        exp
    );
}

fn lines(input: &[u8]) -> IResult<&[u8], Vec<Expression>> {
    many0(line)(input)
}

fn line(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((
        alt((
            while_expression,
            for_in_expression,
            if_expression,
            assignment,
            expression_line,
            block_expression,
        )),
        space0,
    ))(input)
    {
        Ok((input, (exp, _))) => Ok((input, exp)),
        Err(e) => Err(e),
    }
}

#[test]
fn test_line() {
    let (input, exp) = line(b"abc=12345; ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Expression::Assignment {
            name: "abc".to_string(),
            expression: Box::new(Expression::Literal(12345))
        },
        exp
    );

    let (input, exp) = line(b"12345; ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(Expression::Literal(12345), exp);
}

fn if_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((
        tag("if"),
        space0,
        tag("("),
        space0,
        expression,
        space0,
        tag(")"),
        space0,
        line,
    ))(input)
    {
        Ok((input, (_, _, _, _, exp, _, _, _, then))) => {
            match tuple((space0, tag("else"), space0, line))(input) {
                Ok((input, (_, _, _, el))) => Ok((
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
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_if_expression() {
    let (input, exp) = if_expression(b"if(1)2;").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Expression::If {
            condition: Box::new(Expression::Literal(1)),
            then_clause: Box::new(Expression::Literal(2)),
            else_clause: None,
        },
        exp
    );

    let (input, exp) = if_expression(b"if(1)2;else3;").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Expression::If {
            condition: Box::new(Expression::Literal(1)),
            then_clause: Box::new(Expression::Literal(2)),
            else_clause: Some(Box::new(Expression::Literal(3))),
        },
        exp
    );
}

fn while_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((
        tag("while"),
        space0,
        tag("("),
        space0,
        expression,
        space0,
        tag(")"),
        space0,
        line,
    ))(input)
    {
        Ok((input, (_, _, _, _, exp, _, _, _, line))) => {
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

#[test]
fn test_while_expression() {
    let (input, exp) = while_expression(b"while(1!=2){3;} ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Expression::While {
            condition: Box::new(Expression::BinaryExpression {
                operator: Operator::NotEqual,
                lhs: Box::new(Expression::Literal(1)),
                rhs: Box::new(Expression::Literal(2)),
            }),
            body: Box::new(Expression::Block {
                expressions: vec![Expression::Literal(3)],
            },),
        },
        exp
    );
}

fn for_in_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((
        tag("for"),
        space0,
        tag("("),
        space0,
        identifier,
        space1,
        tag("in"),
        space1,
        expression,
        space1,
        tag("to"),
        space1,
        expression,
        space0,
        tag(")"),
        space0,
        line,
    ))(input)
    {
        Ok((input, (_, _, _, _, id, _, _, _, exp0, _, _, _, exp1, _, _, _, line))) => {
            if let Expression::Identifier(name) = id {
                Ok((
                    input,
                    Expression::Block {
                        expressions: vec![
                            Expression::Assignment {
                                name: String::from(&name),
                                expression: Box::new(exp0),
                            },
                            Expression::While {
                                condition: Box::new(Expression::BinaryExpression {
                                    operator: Operator::LessThan,
                                    lhs: Box::new(Expression::Identifier(String::from(&name))),
                                    rhs: Box::new(exp1),
                                }),
                                body: Box::new(Expression::Block {
                                    expressions: vec![
                                        line,
                                        Expression::Assignment {
                                            name: String::from(&name),
                                            expression: Box::new(Expression::BinaryExpression {
                                                operator: Operator::Add,
                                                lhs: Box::new(Expression::Identifier(
                                                    String::from(&name),
                                                )),
                                                rhs: Box::new(Expression::Literal(1)),
                                            }),
                                        },
                                    ],
                                }),
                            },
                        ],
                    },
                ))
            } else {
                panic!();
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_for_in_expression() {
    let (input, exp) = for_in_expression(b"for( i in 1 to 10)3; ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Expression::Block {
            expressions: vec![
                Expression::Assignment {
                    name: "i".to_string(),
                    expression: Box::new(Expression::Literal(1)),
                },
                Expression::While {
                    condition: Box::new(Expression::BinaryExpression {
                        operator: Operator::LessThan,
                        lhs: Box::new(Expression::Identifier("i".to_string())),
                        rhs: Box::new(Expression::Literal(10)),
                    }),
                    body: Box::new(Expression::Block {
                        expressions: vec![
                            Expression::Literal(3),
                            Expression::Assignment {
                                name: "i".to_string(),
                                expression: Box::new(Expression::BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: Box::new(Expression::Identifier("i".to_string())),
                                    rhs: Box::new(Expression::Literal(1)),
                                }),
                            },
                        ],
                    })
                }
            ],
        },
        exp
    );
}

fn block_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((tag("{"), space0, many0(line), space0, tag("}")))(input) {
        Ok((input, (_, _, lines, _, _))) => Ok((input, Expression::Block { expressions: lines })),
        Err(e) => Err(e),
    }
}

#[test]
fn test_block_expression() {
    let (input, exp) = block_expression(b"{1;} ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::Block {
            expressions: vec![Expression::Literal(1),],
        },
        exp
    );
}

fn assignment(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((
        identifier,
        space0,
        tag("="),
        space0,
        expression,
        space0,
        tag(";"),
    ))(input)
    {
        Ok((input, (id, _, _, _, exp, _, _))) => {
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
            expression: Box::new(Expression::Literal(12345))
        },
        exp
    );
}

fn expression_line(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((expression, space0, tag(";")))(input) {
        Ok((input, (exp, _, _))) => Ok((input, exp)),
        Err(e) => Err(e),
    }
}

#[test]
fn test_expression_line() {
    let (input, exp) = expression_line(b"12345; ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(12345), exp);
}

fn expression(input: &[u8]) -> IResult<&[u8], Expression> {
    comparative(input)
}

fn comparative(input: &[u8]) -> IResult<&[u8], Expression> {
    match additive(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((
                space0,
                alt((
                    tag("<"),
                    tag(">"),
                    tag("<="),
                    tag(">="),
                    tag("=="),
                    tag("!="),
                )),
                space0,
                additive,
            )))(input);

            let mut cur = exp;
            match ret {
                Ok((input, v)) => {
                    for elem in v {
                        let (_, op, _, ex) = elem;
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
    let (input, exp) = comparative(b"1<2;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        Expression::BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Expression::Literal(1)),
            rhs: Box::new(Expression::Literal(2)),
        },
        exp
    );
}

fn additive(input: &[u8]) -> IResult<&[u8], Expression> {
    match multitive(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((
                space0,
                alt((tag("+"), tag("-"))),
                space0,
                multitive,
            )))(input);

            let mut cur = exp;
            match ret {
                Ok((input, v)) => {
                    for elem in v {
                        let (_, op, _, ex) = elem;
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
    let (input, exp) = additive(b"1+2;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        Expression::BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(Expression::Literal(1)),
            rhs: Box::new(Expression::Literal(2)),
        },
        exp
    );
}

fn multitive(input: &[u8]) -> IResult<&[u8], Expression> {
    match primary(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((space0, alt((tag("*"), tag("/"))), space0, primary)))(input);

            let mut cur = exp;
            match ret {
                Ok((input, v)) => {
                    for elem in v {
                        let (_, op, _, ex) = elem;
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
    let (input, exp) = multitive(b"1 * 2;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        Expression::BinaryExpression {
            operator: Operator::Multiply,
            lhs: Box::new(Expression::Literal(1)),
            rhs: Box::new(Expression::Literal(2)),
        },
        exp
    );
}

fn primary(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = tuple((tag("("), space0, expression, space0, tag(")")))(input);
    if let Ok((input, (_, _, exp, _, _))) = ret {
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

    let ret = labelled_call(input);
    if let Ok((input, exp)) = ret {
        return Ok((input, exp));
    }

    identifier(input)
}

#[test]
fn test_primary() {
    let (input, exp) = primary(b"( 42 ) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(42), exp);

    let (input, exp) = primary(b"42 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Literal(42), exp);

    let (input, exp) = primary(b"func(123) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::FunctionCall {
            name: "func".to_string(),
            args: vec![Expression::Literal(123)],
        },
        exp
    );

    let (input, exp) = primary(b"func[abc=123] ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::LabelledCall {
            name: "func".to_string(),
            args: vec![LabelledParameter {
                name: "abc".to_string(),
                expression: Box::new(Expression::Literal(123)),
            },],
        },
        exp
    );

    let (input, exp) = primary(b"abc ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Expression::Identifier("abc".to_string()), exp);
}

fn function_call(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((identifier, space0, tag("("), space0))(input) {
        Ok((input, (name, _, _, _))) => {
            if let Expression::Identifier(name2) = name {
                match expression(input) {
                    Ok((input, exp1)) => {
                        let ret = tuple((
                            many0(tuple((tag(","), space0, expression, space0))),
                            tag(")"),
                        ))(input);
                        match ret {
                            Ok((input, (v, _))) => {
                                let mut args = vec![exp1];
                                for pair in v {
                                    args.push(pair.2);
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
            args: vec![Expression::Literal(123)],
        },
        exp
    );
}

fn labelled_patameter(input: &[u8]) -> IResult<&[u8], LabelledParameter> {
    match tuple((identifier, space0, tag("="), space0, expression))(input) {
        Ok((input, (id, _, _, _, exp))) => {
            if let Expression::Identifier(name) = id {
                Ok((
                    input,
                    LabelledParameter {
                        name: name,
                        expression: Box::new(exp),
                    },
                ))
            } else {
                panic!()
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_labelled_patameter() {
    let (input, param) = labelled_patameter(b"a=123 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        LabelledParameter {
            name: "a".to_string(),
            expression: Box::new(Expression::Literal(123)),
        },
        param
    );
}

fn labelled_call(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((
        identifier,
        space0,
        tag("["),
        space0,
        opt(tuple((
            labelled_patameter,
            many0(tuple((space0, tag(","), space0, labelled_patameter))),
            space0,
        ))),
        tag("]"),
    ))(input)
    {
        Ok((input, (id, _, _, _, arg_opt, _))) => {
            if let Expression::Identifier(s) = id {
                let args = match arg_opt {
                    Some(t) => {
                        let mut args = vec![t.0];
                        for arg in t.1 {
                            args.push(arg.3);
                        }
                        args
                    }
                    None => Vec::new(),
                };

                Ok((
                    input,
                    Expression::LabelledCall {
                        name: s,
                        args: args,
                    },
                ))
            } else {
                panic!();
            }
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_labelled_call() {
    let (input, exp) = labelled_call(b"func[abc=123,def=456] ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Expression::LabelledCall {
            name: "func".to_string(),
            args: vec![
                LabelledParameter {
                    name: "abc".to_string(),
                    expression: Box::new(Expression::Literal(123)),
                },
                LabelledParameter {
                    name: "def".to_string(),
                    expression: Box::new(Expression::Literal(456)),
                },
            ],
        },
        exp
    );
}
