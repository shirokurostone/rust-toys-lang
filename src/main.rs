use std::collections::HashMap;

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
}

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
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
            LessThan => "<",
            LessOrEqual => "<=",
            GreaterThan => ">",
            GreaterOrEqual => ">=",
            EqualEqual => "==",
            NotEqual => "!=",
        }
    }
}

enum Literal {
    Integer(i32),
}

fn integer(value: i32) -> Expression {
    Expression::Literal(Box::new(Literal::Integer(value)))
}

struct Interpreter {
    environment: HashMap<String, i32>,
}

impl Interpreter {
    fn new() -> Interpreter {
        Interpreter {
            environment: HashMap::<String, i32>::new(),
        }
    }

    fn interpret(&mut self, expression: &Expression) -> i32 {
        match expression {
            Expression::BinaryExpression {
                operator: op,
                lhs: lhs,
                rhs: rhs,
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
            Expression::Identifier(s) => self.environment[s],
            Expression::Assignment {
                name: name,
                expression: exp,
            } => {
                let value = self.interpret(&**exp);
                self.environment.insert(name.to_string(), value);
                value
            }
            Expression::Block {
                expressions: expressions,
            } => {
                for exp in expressions {
                    self.interpret(exp);
                }
                1
            }
            Expression::While {
                condition: condition,
                body: body,
            } => {
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
                condition: condition,
                then_clause: then_clause,
                else_clause: else_clause,
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
    interpreter.environment.insert("hoge".to_string(), 42);

    let actual = interpreter.interpret(&Expression::Identifier("hoge".to_string()));

    assert_eq!(42, actual);
}

#[test]
fn interpret_assignment() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Expression::Assignment {
        name: "hoge".to_string(),
        expression: Box::new(integer((42))),
    });

    assert_eq!(42, actual);
    assert_eq!(
        Some(&42i32),
        interpreter.environment.get(&"hoge".to_string())
    );
}

#[test]
fn interpret_block() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Expression::Block {
        expressions: vec![
            Expression::Assignment {
                name: "a".to_string(),
                expression: Box::new(integer((1))),
            },
            Expression::Assignment {
                name: "b".to_string(),
                expression: Box::new(integer((2))),
            },
            Expression::Assignment {
                name: "c".to_string(),
                expression: Box::new(integer((3))),
            },
        ],
    });

    assert_eq!(1, actual);
    assert_eq!(Some(&1i32), interpreter.environment.get(&"a".to_string()));
    assert_eq!(Some(&2i32), interpreter.environment.get(&"b".to_string()));
    assert_eq!(Some(&3i32), interpreter.environment.get(&"c".to_string()));
}

#[test]
fn interpret_while() {
    let mut interpreter = Interpreter::new();
    interpreter.environment.insert("a".to_string(), 0i32);

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
    assert_eq!(Some(&10i32), interpreter.environment.get(&"a".to_string()));
}

#[test]
fn interpret_if_then() {
    let mut interpreter = Interpreter::new();
    interpreter.environment.insert("a".to_string(), 0i32);

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
    assert_eq!(Some(&1i32), interpreter.environment.get(&"b".to_string()));
}

#[test]
fn interpret_if_else() {
    let mut interpreter = Interpreter::new();
    interpreter.environment.insert("a".to_string(), 0i32);

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
    assert_eq!(Some(&2i32), interpreter.environment.get(&"b".to_string()));
}

fn main() {
    println!("Hello, world!");
}
