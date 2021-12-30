#[derive(Debug, PartialEq)]
pub enum TopLevel {
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

#[derive(Debug, PartialEq)]
pub enum Expression {
    BinaryExpression {
        operator: Operator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Literal(i32),
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
    LabelledCall {
        name: String,
        args: Vec<LabelledParameter>,
    },
}

#[derive(Debug, PartialEq)]
pub struct LabelledParameter {
    pub name: String,
    pub expression: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
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
