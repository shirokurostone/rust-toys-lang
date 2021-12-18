use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::take_while;
use nom::character::is_alphabetic;
use nom::character::is_digit;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use std::str;

use crate::interpreter::*;

fn integer(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = take_while(is_digit)(input);
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

fn identifier(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = take_while(is_alphabetic)(input);
    match ret {
        Ok((input, s)) => {
            let value = str::from_utf8(s).unwrap();
            Ok((input, Expression::Identifier(value.to_string())))
        }
        Err(e) => Err(e),
    }
}

fn expression(input: &[u8]) -> IResult<&[u8], Expression> {
    integer(input)
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

fn primary(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = tuple((tag("("), expression, tag(")")))(input);
    if let Ok((input, (_, exp, _))) = ret {
        return Ok((input, exp));
    }

    let ret = integer(input);
    if let Ok((input, exp)) = ret {
        return Ok((input, exp));
    }

    identifier(input)
}

fn function_call(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((identifier, tag("(")))(input) {
        Ok((input, (name, _))) => {
            if let Expression::Identifier(name2) = name {
                match expression(input) {
                    Ok((input, exp1)) => {
                        let ret = many0(tuple((tag(","), expression)))(input);
                        match ret {
                            Ok((input, v)) => {
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
