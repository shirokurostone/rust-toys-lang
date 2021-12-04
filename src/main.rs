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
}

enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl Operator {
    fn value(&self) -> &str {
        match self {
            Add => "+",
            Subtract => "-",
            Multiply => "*",
            Divide => "/",
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

fn main() {
    println!("Hello, world!");
}
