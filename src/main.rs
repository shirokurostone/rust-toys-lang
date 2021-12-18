use std::collections::HashMap;

#[derive(Debug)]
enum TopLevel {
    FunctionDefinition {
        name: String,
        args: Vec<String>,
        body: Expression,
    },
    GlobalVariableDefinition {
        name: String,
        expression: Box<Expression>,
    },
}

#[derive(Debug)]
enum Expression {
    BinaryExpression {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Literal(Box<Literal>),
    Identifier(String),
    Assignment {
        name: String,
        expression: Box<Expression>,
    },
    Block {
        expressions: Vec<Expression>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        then_clause: Box<Expression>,
        else_clause: Option<Box<Expression>>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
}

#[derive(Debug)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    EqualEqual,
    NotEqual,
}

impl Operator {
    fn value(&self) -> &str {
        match self {
            Operator::Add => "+",
            Operator::Subtract => "-",
            Operator::Multiply => "*",
            Operator::Divide => "/",
            Operator::LessThan => "<",
            Operator::LessOrEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterOrEqual => ">=",
            Operator::EqualEqual => "==",
            Operator::NotEqual => "!=",
        }
    }
}

#[derive(Debug)]
enum Literal {
    Integer(i32),
}

fn integer(value: i32) -> Expression {
    Expression::Literal(Box::new(Literal::Integer(value)))
}

struct Interpreter<'a> {
    environments: Vec<HashMap<String, i32>>,
    function_environment: HashMap<String, &'a TopLevel>,
}

impl<'a> Interpreter<'a> {
    fn new() -> Interpreter<'a> {
        let mut i = Interpreter {
            environments: Vec::new(),
            function_environment: HashMap::<String, &'a TopLevel>::new(),
        };
        i.environments.push(HashMap::<String, i32>::new());
        i
    }

    fn call_main(&mut self, top_levels: Vec<&'a TopLevel>) -> i32 {
        for top in top_levels {
            match top {
                TopLevel::FunctionDefinition {
                    name,
                    args: _,
                    body: _,
                } => {
                    self.function_environment.insert(name.to_string(), top);
                }
                TopLevel::GlobalVariableDefinition { name, expression } => {
                    let v = self.interpret(expression);
                    self.set_variable(name.to_string(), v);
                }
            }
        }

        match self.function_environment.get(&"main".to_string()) {
            Some(def) => {
                if let TopLevel::FunctionDefinition {
                    name: _,
                    args: _,
                    body,
                } = def
                {
                    self.interpret(body)
                } else {
                    0
                }
            }
            None => 0,
        }
    }

    fn get_variable(&self, name: String) -> Option<i32> {
        for env in self.environments.iter().rev() {
            if let Some(v) = env.get(&name) {
                return Some(*v);
            }
        }
        None
    }

    fn set_variable(&mut self, name: String, value: i32) -> Option<i32> {
        for env in self.environments.iter_mut().rev() {
            if env.contains_key(&name) {
                return env.insert(name.to_string(), value);
            }
        }
        self.environments
            .last_mut()?
            .insert(name.to_string(), value);
        None
    }

    fn interpret(&mut self, expression: &Expression) -> i32 {
        match expression {
            Expression::BinaryExpression {
                operator: op,
                lhs,
                rhs,
            } => {
                let l = self.interpret(lhs);
                let r = self.interpret(rhs);
                match op {
                    Operator::Add => l + r,
                    Operator::Subtract => l - r,
                    Operator::Multiply => l * r,
                    Operator::Divide => l / r,
                    Operator::LessThan => {
                        if l < r {
                            1i32
                        } else {
                            0i32
                        }
                    }
                    Operator::LessOrEqual => {
                        if l <= r {
                            1i32
                        } else {
                            0i32
                        }
                    }
                    Operator::GreaterThan => {
                        if l > r {
                            1i32
                        } else {
                            0i32
                        }
                    }
                    Operator::GreaterOrEqual => {
                        if l >= r {
                            1i32
                        } else {
                            0i32
                        }
                    }
                    Operator::EqualEqual => {
                        if l == r {
                            1i32
                        } else {
                            0i32
                        }
                    }
                    Operator::NotEqual => {
                        if l != r {
                            1i32
                        } else {
                            0i32
                        }
                    }
                }
            }
            Expression::Literal(literal) => match &**literal {
                Literal::Integer(value) => *value,
            },
            Expression::Identifier(s) => match self.get_variable(s.to_string()) {
                Some(v) => v,
                None => 0,
            },
            Expression::Assignment {
                name,
                expression: exp,
            } => {
                let value = self.interpret(&**exp);
                self.set_variable(name.to_string(), value);
                value
            }
            Expression::Block { expressions } => {
                let mut ret = 0;
                for exp in expressions {
                    ret = self.interpret(exp);
                }
                ret
            }
            Expression::While { condition, body } => {
                loop {
                    let cond = self.interpret(condition);
                    if cond == 0 {
                        break;
                    }
                    self.interpret(body);
                }
                1
            }
            Expression::If {
                condition,
                then_clause,
                else_clause,
            } => {
                let cond = self.interpret(condition);
                if cond != 0 {
                    self.interpret(then_clause)
                } else {
                    match else_clause {
                        Some(e) => self.interpret(e),
                        None => 1i32,
                    }
                }
            }
            Expression::FunctionCall {
                name,
                args: actual_params,
            } => match self.function_environment.get(name) {
                Some(def) => {
                    let mut ret = 0;
                    if let TopLevel::FunctionDefinition {
                        name: _,
                        args: formal_params,
                        body,
                    } = def
                    {
                        let mut values = Vec::new();
                        for param in actual_params {
                            values.push(self.interpret(param));
                        }

                        let mut env = HashMap::<String, i32>::new();
                        for i in 0..(formal_params.len()) {
                            match formal_params.get(i) {
                                Some(v) => {
                                    env.insert(v.to_string(), values[i]);
                                }
                                None => (),
                            }
                        }

                        self.environments.push(env);
                        ret = self.interpret(body);
                        self.environments.pop();
                    }
                    ret
                }
                None => 0,
            },
        }
    }
}

#[test]
fn interpret_literal() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Expression::Literal(Box::new(Literal::Integer(42))));

    assert_eq!(42, actual);
}

#[test]

fn interpret_binary_expression() {
    let mut interpreter = Interpreter::new();

    let actual_add = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::Add,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(1 + 2, actual_add);

    let actual_subtract = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::Subtract,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });

    assert_eq!(1 - 2, actual_subtract);

    let actual_multiply = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::Multiply,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(1 * 2, actual_multiply);

    let actual_divide = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::Divide,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(1 / 2, actual_divide);

    let actual_less_than = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::LessThan,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(1, actual_less_than);

    let actual_less_or_equal = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::LessOrEqual,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(1, actual_less_or_equal);

    let actual_greater_than = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::GreaterThan,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(0, actual_greater_than);

    let actual_greater_or_equal = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::GreaterOrEqual,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(0, actual_greater_or_equal);

    let actual_equal_equal = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::EqualEqual,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(0, actual_equal_equal);

    let actual_not_equal = interpreter.interpret(&Expression::BinaryExpression {
        operator: Operator::NotEqual,
        lhs: Box::new(integer(1)),
        rhs: Box::new(integer(2)),
    });
    assert_eq!(1, actual_not_equal);
}

#[test]
fn interpret_identifier() {
    let mut interpreter = Interpreter::new();
    interpreter.set_variable("hoge".to_string(), 42);

    let actual = interpreter.interpret(&Expression::Identifier("hoge".to_string()));

    assert_eq!(42, actual);
}

#[test]
fn interpret_assignment() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Expression::Assignment {
        name: "hoge".to_string(),
        expression: Box::new(integer(42)),
    });

    assert_eq!(42, actual);
    assert_eq!(Some(42i32), interpreter.get_variable("hoge".to_string()));
}

#[test]
fn interpret_block() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Expression::Block {
        expressions: vec![
            Expression::Assignment {
                name: "a".to_string(),
                expression: Box::new(integer(1)),
            },
            Expression::Assignment {
                name: "b".to_string(),
                expression: Box::new(integer(2)),
            },
            Expression::Assignment {
                name: "c".to_string(),
                expression: Box::new(integer(3)),
            },
            integer(4),
        ],
    });

    assert_eq!(4, actual);
    assert_eq!(Some(1i32), interpreter.get_variable("a".to_string()));
    assert_eq!(Some(2i32), interpreter.get_variable("b".to_string()));
    assert_eq!(Some(3i32), interpreter.get_variable("c".to_string()));
}

#[test]
fn interpret_while() {
    let mut interpreter = Interpreter::new();
    interpreter.set_variable("a".to_string(), 0i32);

    let actual = interpreter.interpret(&Expression::While {
        condition: Box::new(Expression::BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Expression::Identifier("a".to_string())),
            rhs: Box::new(integer(10)),
        }),
        body: Box::new(Expression::Assignment {
            name: "a".to_string(),
            expression: Box::new(Expression::BinaryExpression {
                operator: Operator::Add,
                lhs: Box::new(Expression::Identifier("a".to_string())),
                rhs: Box::new(integer(1)),
            }),
        }),
    });

    assert_eq!(1, actual);
    assert_eq!(Some(10i32), interpreter.get_variable("a".to_string()));
}

#[test]
fn interpret_if_then() {
    let mut interpreter = Interpreter::new();
    interpreter.set_variable("a".to_string(), 0i32);

    let actual = interpreter.interpret(&Expression::If {
        condition: Box::new(Expression::BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Expression::Identifier("a".to_string())),
            rhs: Box::new(integer(10)),
        }),
        then_clause: Box::new(Expression::Assignment {
            name: "b".to_string(),
            expression: Box::new(integer(1)),
        }),
        else_clause: Some(Box::new(Expression::Assignment {
            name: "b".to_string(),
            expression: Box::new(integer(2)),
        })),
    });

    assert_eq!(1, actual);
    assert_eq!(Some(1i32), interpreter.get_variable("b".to_string()));
}

#[test]
fn interpret_if_else() {
    let mut interpreter = Interpreter::new();
    interpreter.set_variable("a".to_string(), 0i32);

    let actual = interpreter.interpret(&Expression::If {
        condition: Box::new(Expression::BinaryExpression {
            operator: Operator::GreaterThan,
            lhs: Box::new(Expression::Identifier("a".to_string())),
            rhs: Box::new(integer(10)),
        }),
        then_clause: Box::new(Expression::Assignment {
            name: "b".to_string(),
            expression: Box::new(integer(1)),
        }),
        else_clause: Some(Box::new(Expression::Assignment {
            name: "b".to_string(),
            expression: Box::new(integer(2)),
        })),
    });

    assert_eq!(2, actual);
    assert_eq!(Some(2i32), interpreter.get_variable("b".to_string()));
}

#[test]
fn interpret_function_call() {
    let mut interpreter = Interpreter::new();

    let main_func = TopLevel::FunctionDefinition {
        name: "main".to_string(),
        args: vec![],
        body: Expression::FunctionCall {
            name: "hoge".to_string(),
            args: vec![integer(1), integer(2)],
        },
    };
    let hoge_func = TopLevel::FunctionDefinition {
        name: "hoge".to_string(),
        args: vec!["a".to_string(), "b".to_string()],
        body: Expression::BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(Expression::Identifier("a".to_string())),
            rhs: Box::new(Expression::Identifier("b".to_string())),
        },
    };

    let top_level = vec![&main_func, &hoge_func];

    let actual = interpreter.call_main(top_level);
    assert_eq!(3, actual);
}

#[test]
fn test_global_variable_definition() {
    let mut interpreter = Interpreter::new();

    let var_def = TopLevel::GlobalVariableDefinition {
        name: "hoge".to_string(),
        expression: Box::new(integer(42)),
    };
    let main_func = TopLevel::FunctionDefinition {
        name: "main".to_string(),
        args: vec![],
        body: Expression::Identifier("hoge".to_string()),
    };

    let top_level = vec![&var_def, &main_func];

    let actual = interpreter.call_main(top_level);
    assert_eq!(42, actual);
}

#[test]
fn test_factorial() {
    let mut interpreter = Interpreter::new();
    let main_func = TopLevel::FunctionDefinition {
        name: "main".to_string(),
        args: vec![],
        body: Expression::Block {
            expressions: vec![Expression::FunctionCall {
                name: "fact".to_string(),
                args: vec![integer(5)],
            }],
        },
    };
    let fact_func = TopLevel::FunctionDefinition {
        name: "fact".to_string(),
        args: vec!["n".to_string()],
        body: Expression::Block {
            expressions: vec![Expression::If {
                condition: Box::new(Expression::BinaryExpression {
                    operator: Operator::LessThan,
                    lhs: Box::new(Expression::Identifier("n".to_string())),
                    rhs: Box::new(integer(2)),
                }),
                then_clause: Box::new(integer(1)),
                else_clause: Some(Box::new(Expression::BinaryExpression {
                    operator: Operator::Multiply,
                    lhs: Box::new(Expression::Identifier("n".to_string())),
                    rhs: Box::new(Expression::FunctionCall {
                        name: "fact".to_string(),
                        args: vec![Expression::BinaryExpression {
                            operator: Operator::Subtract,
                            lhs: Box::new(Expression::Identifier("n".to_string())),
                            rhs: Box::new(integer(1)),
                        }],
                    }),
                })),
            }],
        },
    };
    let top_level = vec![&main_func, &fact_func];
    let actual = interpreter.call_main(top_level);
    assert_eq!(120, actual);
}

fn main() {
    println!("Hello, world!");
}

use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::streaming::take_while;
use nom::character::is_alphabetic;
use nom::character::is_digit;
use nom::multi::many0;
use nom::sequence::tuple;
use nom::IResult;
use std::str;

fn parse_integer(input: &[u8]) -> IResult<&[u8], Expression> {
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

fn parse_identifier(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = take_while(is_alphabetic)(input);
    match ret {
        Ok((input, s)) => {
            let value = str::from_utf8(s).unwrap();
            Ok((input, Expression::Identifier(value.to_string())))
        }
        Err(e) => Err(e),
    }
}

fn parse_expression(input: &[u8]) -> IResult<&[u8], Expression> {
    parse_integer(input)
}

fn parse_comparative(input: &[u8]) -> IResult<&[u8], Expression> {
    match parse_additive(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((
                alt((tag("<"), tag(">"), tag("<="), tag(">="), tag("=="))),
                parse_additive,
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

fn parse_additive(input: &[u8]) -> IResult<&[u8], Expression> {
    match parse_multitive(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((alt((tag("+"), tag("-"))), parse_multitive)))(input);

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

fn parse_multitive(input: &[u8]) -> IResult<&[u8], Expression> {
    match parse_primary(input) {
        Ok((input, exp)) => {
            let ret = many0(tuple((alt((tag("*"), tag("/"))), parse_primary)))(input);

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

fn parse_primary(input: &[u8]) -> IResult<&[u8], Expression> {
    let ret = tuple((tag("("), parse_expression, tag(")")))(input);
    if let Ok((input, (_, exp, _))) = ret {
        return Ok((input, exp));
    }

    let ret = parse_integer(input);
    if let Ok((input, exp)) = ret {
        return Ok((input, exp));
    }

    parse_identifier(input)
}

fn parse_function_call(input: &[u8]) -> IResult<&[u8], Expression> {
    match tuple((parse_identifier, tag("(")))(input) {
        Ok((input, (name, _))) => {
            if let Expression::Identifier(name2) = name {
                match parse_expression(input) {
                    Ok((input, exp1)) => {
                        let ret = many0(tuple((tag(","), parse_expression)))(input);
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
