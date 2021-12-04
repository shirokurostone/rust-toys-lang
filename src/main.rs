enum Expression {
    BinaryExpression {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
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

enum Literal {
    Integer(i32),
}

fn integer(value: i32) -> Expression {
    Expression::Literal(Box::new(Literal::Integer(value)))
}

struct Interpreter();

impl Interpreter {
    fn interpret(&self, expression: &Expression) -> i32 {
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

fn main() {
    println!("Hello, world!");
}
