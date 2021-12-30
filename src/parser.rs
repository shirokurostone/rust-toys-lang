use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::take_while1;
use nom::character::complete::{digit1, multispace0, multispace1};
use nom::character::is_alphabetic;
use nom::combinator::opt;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use std::str;

use crate::interpreter::Expression::*;
use crate::interpreter::*;

fn space0(input: &[u8]) -> IResult<&[u8], &[u8]> {
    multispace0(input)
}

fn space1(input: &[u8]) -> IResult<&[u8], &[u8]> {
    multispace1(input)
}

fn integer(input: &[u8]) -> IResult<&[u8], Expression> {
    digit1(input).map(|(input, digit)| {
        let value = str::from_utf8(digit).unwrap().parse::<i32>().unwrap();
        (input, Literal(value))
    })
}

#[test]
fn test_integer() {
    let (input, exp) = integer(b"12345 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Literal(12345), exp);

    integer(b"").unwrap_err();
    integer(b"abc").unwrap_err();
}

fn identifier(input: &[u8]) -> IResult<&[u8], Expression> {
    take_while1(is_alphabetic)(input).map(|(input, s)| {
        let value = str::from_utf8(s).unwrap();
        (input, Identifier(value.to_string()))
    })
}

#[test]
fn test_identifier() {
    let (input, exp) = identifier(b"abc ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Identifier("abc".to_string()), exp);

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
    many0(tuple((top_level_definition, space0)))(input).map(|(input, top_level_vec)| {
        let mut ret = Vec::new();
        for top in top_level_vec {
            ret.push(top.0);
        }
        (input, ret)
    })
}

fn top_level_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    alt((function_definition, global_variable_definition))(input)
}

fn function_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    tuple((
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
    .map(|(input, (_, _, id, _, _, _, arg_opt, _, _, block))| {
        if let Identifier(s) = id {
            let args = match arg_opt {
                Some(t) => {
                    let mut args = vec![];
                    if let Identifier(arg0) = t.0 {
                        args.push(arg0);
                    } else {
                        panic!();
                    }

                    for arg in t.1 {
                        if let Identifier(argn) = arg.3 {
                            args.push(argn);
                        } else {
                            panic!();
                        }
                    }
                    args
                }
                None => Vec::new(),
            };

            (
                input,
                TopLevel::FunctionDefinition {
                    name: s,
                    args,
                    body: block,
                },
            )
        } else {
            panic!();
        }
    })
}

#[test]
fn test_function_definition() {
    let (input, exp) = function_definition(b"define func ( ) {1;} ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        TopLevel::FunctionDefinition {
            name: "func".to_string(),
            args: vec![],
            body: Block {
                expressions: vec![Literal(1),],
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
            body: Block {
                expressions: vec![Literal(1),],
            },
        },
        exp
    );
}

fn global_variable_definition(input: &[u8]) -> IResult<&[u8], TopLevel> {
    tuple((
        tag("global"),
        space1,
        identifier,
        space0,
        tag("="),
        space0,
        expression,
    ))(input)
    .map(|(input, (_, _, id, _, _, _, exp))| {
        if let Identifier(s) = id {
            (
                input,
                TopLevel::GlobalVariableDefinition {
                    name: s,
                    expression: Box::new(exp),
                },
            )
        } else {
            panic!();
        }
    })
}

#[test]
fn test_global_variable_definition() {
    let (input, exp) = global_variable_definition(b"global abc=123;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        TopLevel::GlobalVariableDefinition {
            name: "abc".to_string(),
            expression: Box::new(Literal(123)),
        },
        exp
    );
}

fn line(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
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
    .map(|(input, (exp, _))| (input, exp))
}

#[test]
fn test_line() {
    let (input, exp) = line(b"abc=12345; ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Assignment {
            name: "abc".to_string(),
            expression: Box::new(Literal(12345))
        },
        exp
    );

    let (input, exp) = line(b"12345; ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(Literal(12345), exp);
}

fn if_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
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
    .map(|(input, (_, _, _, _, exp, _, _, _, then))| {
        match tuple((space0, tag("else"), space0, line))(input) {
            Ok((input, (_, _, _, el))) => (
                input,
                If {
                    condition: Box::new(exp),
                    then_clause: Box::new(then),
                    else_clause: Some(Box::new(el)),
                },
            ),
            Err(_) => (
                input,
                If {
                    condition: Box::new(exp),
                    then_clause: Box::new(then),
                    else_clause: None,
                },
            ),
        }
    })
}

#[test]
fn test_if_expression() {
    let (input, exp) = if_expression(b"if(1)2;").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        If {
            condition: Box::new(Literal(1)),
            then_clause: Box::new(Literal(2)),
            else_clause: None,
        },
        exp
    );

    let (input, exp) = if_expression(b"if(1)2;else3;").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        If {
            condition: Box::new(Literal(1)),
            then_clause: Box::new(Literal(2)),
            else_clause: Some(Box::new(Literal(3))),
        },
        exp
    );
}

fn while_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
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
    .map(|(input, (_, _, _, _, exp, _, _, _, line))| {
        (
            input,
            While {
                condition: Box::new(exp),
                body: Box::new(line),
            },
        )
    })
}

#[test]
fn test_while_expression() {
    let (input, exp) = while_expression(b"while(1!=2){3;} ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        While {
            condition: Box::new(BinaryExpression {
                operator: Operator::NotEqual,
                lhs: Box::new(Literal(1)),
                rhs: Box::new(Literal(2)),
            }),
            body: Box::new(Block {
                expressions: vec![Literal(3)],
            },),
        },
        exp
    );
}

fn for_in_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
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
    .map(
        |(input, (_, _, _, _, id, _, _, _, exp0, _, _, _, exp1, _, _, _, line))| {
            if let Identifier(name) = id {
                (
                    input,
                    Block {
                        expressions: vec![
                            Assignment {
                                name: String::from(&name),
                                expression: Box::new(exp0),
                            },
                            While {
                                condition: Box::new(BinaryExpression {
                                    operator: Operator::LessThan,
                                    lhs: Box::new(Identifier(String::from(&name))),
                                    rhs: Box::new(exp1),
                                }),
                                body: Box::new(Block {
                                    expressions: vec![
                                        line,
                                        Assignment {
                                            name: String::from(&name),
                                            expression: Box::new(BinaryExpression {
                                                operator: Operator::Add,
                                                lhs: Box::new(Identifier(String::from(&name))),
                                                rhs: Box::new(Literal(1)),
                                            }),
                                        },
                                    ],
                                }),
                            },
                        ],
                    },
                )
            } else {
                panic!();
            }
        },
    )
}

#[test]
fn test_for_in_expression() {
    let (input, exp) = for_in_expression(b"for( i in 1 to 10)3; ").unwrap();
    assert_eq!(b"", input);
    assert_eq!(
        Block {
            expressions: vec![
                Assignment {
                    name: "i".to_string(),
                    expression: Box::new(Literal(1)),
                },
                While {
                    condition: Box::new(BinaryExpression {
                        operator: Operator::LessThan,
                        lhs: Box::new(Identifier("i".to_string())),
                        rhs: Box::new(Literal(10)),
                    }),
                    body: Box::new(Block {
                        expressions: vec![
                            Literal(3),
                            Assignment {
                                name: "i".to_string(),
                                expression: Box::new(BinaryExpression {
                                    operator: Operator::Add,
                                    lhs: Box::new(Identifier("i".to_string())),
                                    rhs: Box::new(Literal(1)),
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
    tuple((tag("{"), space0, many0(line), space0, tag("}")))(input)
        .map(|(input, (_, _, lines, _, _))| (input, Block { expressions: lines }))
}

#[test]
fn test_block_expression() {
    let (input, exp) = block_expression(b"{1;} ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Block {
            expressions: vec![Literal(1),],
        },
        exp
    );
}

fn assignment(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
        identifier,
        space0,
        tag("="),
        space0,
        expression,
        space0,
        tag(";"),
    ))(input)
    .map(|(input, (id, _, _, _, exp, _, _))| {
        if let Identifier(s) = id {
            (
                input,
                Assignment {
                    name: s,
                    expression: Box::new(exp),
                },
            )
        } else {
            panic!();
        }
    })
}

#[test]
fn test_assignment() {
    let (input, exp) = assignment(b"abc=12345; ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        Assignment {
            name: "abc".to_string(),
            expression: Box::new(Literal(12345))
        },
        exp
    );
}

fn expression_line(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((expression, space0, tag(";")))(input).map(|(input, (exp, _, _))| (input, exp))
}

#[test]
fn test_expression_line() {
    let (input, exp) = expression_line(b"12345; ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Literal(12345), exp);
}

fn expression(input: &[u8]) -> IResult<&[u8], Expression> {
    comparative(input)
}

fn comparative(input: &[u8]) -> IResult<&[u8], Expression> {
    match additive(input) {
        Ok((input, exp)) => {
            let mut cur = exp;
            many0(tuple((
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
            )))(input)
            .map(|(input, v)| {
                for elem in v {
                    let (_, op, _, ex) = elem;
                    cur = BinaryExpression {
                        operator: match op {
                            b"<" => Operator::LessThan,
                            b">" => Operator::GreaterThan,
                            b"<=" => Operator::LessOrEqual,
                            b">=" => Operator::GreaterOrEqual,
                            b"==" => Operator::EqualEqual,
                            b"!=" => Operator::NotEqual,
                            _ => panic!(),
                        },
                        lhs: Box::new(cur),
                        rhs: Box::new(ex),
                    };
                }
                (input, cur)
            })
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_comparative() {
    let (input, exp) = comparative(b"1<2;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Literal(1)),
            rhs: Box::new(Literal(2)),
        },
        exp
    );
}

fn additive(input: &[u8]) -> IResult<&[u8], Expression> {
    match multitive(input) {
        Ok((input, exp)) => {
            let mut cur = exp;
            many0(tuple((
                space0,
                alt((tag("+"), tag("-"))),
                space0,
                multitive,
            )))(input)
            .map(|(input, v)| {
                for elem in v {
                    let (_, op, _, ex) = elem;
                    cur = BinaryExpression {
                        operator: match op {
                            b"+" => Operator::Add,
                            b"-" => Operator::Subtract,
                            _ => panic!(),
                        },
                        lhs: Box::new(cur),
                        rhs: Box::new(ex),
                    };
                }

                (input, cur)
            })
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_additive() {
    let (input, exp) = additive(b"1+2;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(Literal(1)),
            rhs: Box::new(Literal(2)),
        },
        exp
    );
}

fn multitive(input: &[u8]) -> IResult<&[u8], Expression> {
    match primary(input) {
        Ok((input, exp)) => {
            let mut cur = exp;
            many0(tuple((space0, alt((tag("*"), tag("/"))), space0, primary)))(input).map(
                |(input, v)| {
                    for elem in v {
                        let (_, op, _, ex) = elem;
                        cur = BinaryExpression {
                            operator: match op {
                                b"*" => Operator::Multiply,
                                b"/" => Operator::Divide,
                                _ => panic!(),
                            },
                            lhs: Box::new(cur),
                            rhs: Box::new(ex),
                        };
                    }

                    (input, cur)
                },
            )
        }
        Err(e) => Err(e),
    }
}

#[test]
fn test_multitive() {
    let (input, exp) = multitive(b"1 * 2;").unwrap();
    assert_eq!(b";", input);
    assert_eq!(
        BinaryExpression {
            operator: Operator::Multiply,
            lhs: Box::new(Literal(1)),
            rhs: Box::new(Literal(2)),
        },
        exp
    );
}

fn primary(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = tuple((tag("("), space0, expression, space0, tag(")")))(input);
    if let Ok((input, (_, _, exp, _, _))) = ret {
        return Ok((input, exp));
    }

    alt((integer, function_call, labelled_call, identifier))(input)
}

#[test]
fn test_primary() {
    let (input, exp) = primary(b"( 42 ) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Literal(42), exp);

    let (input, exp) = primary(b"42 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Literal(42), exp);

    let (input, exp) = primary(b"func(123) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        FunctionCall {
            name: "func".to_string(),
            args: vec![Literal(123)],
        },
        exp
    );

    let (input, exp) = primary(b"func[abc=123] ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        LabelledCall {
            name: "func".to_string(),
            args: vec![LabelledParameter {
                name: "abc".to_string(),
                expression: Box::new(Literal(123)),
            },],
        },
        exp
    );

    let (input, exp) = primary(b"abc ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(Identifier("abc".to_string()), exp);
}

fn function_call(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
        identifier,
        space0,
        tag("("),
        space0,
        opt(tuple((
            expression,
            many0(tuple((space0, tag(","), space0, expression))),
            space0,
        ))),
        tag(")"),
    ))(input)
    .map(|(input, (id, _, _, _, arg_opt, _))| {
        if let Identifier(s) = id {
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

            (input, FunctionCall { name: s, args })
        } else {
            panic!();
        }
    })
}

#[test]
fn test_function_call() {
    let (input, exp) = function_call(b"func(123) ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        FunctionCall {
            name: "func".to_string(),
            args: vec![Literal(123)],
        },
        exp
    );
}

fn labelled_patameter(input: &[u8]) -> IResult<&[u8], LabelledParameter> {
    tuple((identifier, space0, tag("="), space0, expression))(input).map(
        |(input, (id, _, _, _, exp))| {
            if let Identifier(name) = id {
                (
                    input,
                    LabelledParameter {
                        name,
                        expression: Box::new(exp),
                    },
                )
            } else {
                panic!()
            }
        },
    )
}

#[test]
fn test_labelled_patameter() {
    let (input, param) = labelled_patameter(b"a=123 ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        LabelledParameter {
            name: "a".to_string(),
            expression: Box::new(Literal(123)),
        },
        param
    );
}

fn labelled_call(input: &[u8]) -> IResult<&[u8], Expression> {
    tuple((
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
    .map(|(input, (id, _, _, _, arg_opt, _))| {
        if let Identifier(s) = id {
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

            (input, LabelledCall { name: s, args })
        } else {
            panic!();
        }
    })
}

#[test]
fn test_labelled_call() {
    let (input, exp) = labelled_call(b"func[abc=123,def=456] ").unwrap();
    assert_eq!(b" ", input);
    assert_eq!(
        LabelledCall {
            name: "func".to_string(),
            args: vec![
                LabelledParameter {
                    name: "abc".to_string(),
                    expression: Box::new(Literal(123)),
                },
                LabelledParameter {
                    name: "def".to_string(),
                    expression: Box::new(Literal(456)),
                },
            ],
        },
        exp
    );
}
