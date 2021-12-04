enum Expression {
    BinaryExpression(Box<BinaryExpression>),
    Literal(Box<Literal>),
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

enum BinaryExpression {
    Add { lhs: Expression, rhs: Expression },
    Subtract { lhs: Expression, rhs: Expression },
    Multiply { lhs: Expression, rhs: Expression },
    Divide { lhs: Expression, rhs: Expression },
}

enum Literal {
    Integer(i32),
}

struct Interpreter();

impl Interpreter {
    fn interpret(&self, expression: &Expression) -> i32 {
        match expression {
            Expression::BinaryExpression(exp) => match &**exp {
                BinaryExpression::Add { lhs, rhs } => {
                    let l = self.interpret(lhs);
                    let r = self.interpret(rhs);
                    l + r
                }
                BinaryExpression::Subtract { lhs, rhs } => {
                    let l = self.interpret(lhs);
                    let r = self.interpret(rhs);
                    l - r
                }
                BinaryExpression::Multiply { lhs, rhs } => {
                    let l = self.interpret(lhs);
                    let r = self.interpret(rhs);
                    l * r
                }
                BinaryExpression::Divide { lhs, rhs } => {
                    let l = self.interpret(lhs);
                    let r = self.interpret(rhs);
                    l / r
                }
            },
            Expression::Literal(literal) => match &**literal {
                Literal::Integer(value) => *value,
            },
        }
    }
}

#[test]
fn interpret_literal() {
    let interpreter = Interpreter();

    let actual = interpreter.interpret(&Expression::Literal(Box::new(Literal::Integer(42))));

    assert_eq!(42, actual);
}

#[test]

fn interpret_binary_expression() {
    let interpreter = Interpreter();

    let actual_add = interpreter.interpret(&Expression::BinaryExpression(Box::new(
        BinaryExpression::Add {
            lhs: Expression::Literal(Box::new(Literal::Integer(1))),
            rhs: Expression::Literal(Box::new(Literal::Integer(2))),
        },
    )));
    assert_eq!(1 + 2, actual_add);

    let actual_subtract = interpreter.interpret(&Expression::BinaryExpression(Box::new(
        BinaryExpression::Subtract {
            lhs: Expression::Literal(Box::new(Literal::Integer(1))),
            rhs: Expression::Literal(Box::new(Literal::Integer(2))),
        },
    )));
    assert_eq!(1 - 2, actual_subtract);

    let actual_multiply = interpreter.interpret(&Expression::BinaryExpression(Box::new(
        BinaryExpression::Multiply {
            lhs: Expression::Literal(Box::new(Literal::Integer(1))),
            rhs: Expression::Literal(Box::new(Literal::Integer(2))),
        },
    )));
    assert_eq!(1 * 2, actual_multiply);

    let actual_divide = interpreter.interpret(&Expression::BinaryExpression(Box::new(
        BinaryExpression::Divide {
            lhs: Expression::Literal(Box::new(Literal::Integer(1))),
            rhs: Expression::Literal(Box::new(Literal::Integer(2))),
        },
    )));
    assert_eq!(1 / 2, actual_divide);
}

fn main() {
    println!("Hello, world!");
}
