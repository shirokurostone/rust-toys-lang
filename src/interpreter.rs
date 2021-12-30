use std::collections::HashMap;

use crate::interpreter::Expression::*;

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

pub struct Interpreter<'a> {
    environments: Vec<HashMap<String, i32>>,
    function_environment: HashMap<String, &'a TopLevel>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Interpreter<'a> {
        let mut i = Interpreter {
            environments: Vec::new(),
            function_environment: HashMap::<String, &'a TopLevel>::new(),
        };
        i.environments.push(HashMap::<String, i32>::new());
        i
    }

    pub fn call_main(&mut self, top_levels: Vec<&'a TopLevel>) -> i32 {
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
                    panic!();
                }
            }
            None => panic!(),
        }
    }

    pub fn get_variable(&self, name: String) -> Option<i32> {
        for env in self.environments.iter().rev() {
            if let Some(v) = env.get(&name) {
                return Some(*v);
            }
        }
        None
    }

    pub fn set_variable(&mut self, name: String, value: i32) -> Option<i32> {
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

    pub fn interpret(&mut self, expression: &Expression) -> i32 {
        match expression {
            BinaryExpression {
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
            Literal(value) => *value,
            Identifier(s) => match self.get_variable(s.to_string()) {
                Some(v) => v,
                None => panic!(),
            },
            Assignment {
                name,
                expression: exp,
            } => {
                let value = self.interpret(&**exp);
                self.set_variable(name.to_string(), value);
                value
            }
            Block { expressions } => {
                let mut ret = 0;
                for exp in expressions {
                    ret = self.interpret(exp);
                }
                ret
            }
            While { condition, body } => {
                loop {
                    let cond = self.interpret(condition);
                    if cond == 0 {
                        break;
                    }
                    self.interpret(body);
                }
                1
            }
            If {
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
            FunctionCall {
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
                        if actual_params.len() != formal_params.len() {
                            panic!();
                        }

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
                                None => panic!(),
                            }
                        }

                        self.environments.push(env);
                        ret = self.interpret(body);
                        self.environments.pop();
                    }
                    ret
                }
                None => panic!(),
            },
            LabelledCall {
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
                        if actual_params.len() != formal_params.len() {
                            panic!();
                        }

                        let mut labelled_parameter_map =
                            HashMap::<&String, &LabelledParameter>::new();
                        for param in actual_params {
                            labelled_parameter_map.insert(&param.name, param);
                        }

                        let mut env = HashMap::<String, i32>::new();
                        for i in 0..(formal_params.len()) {
                            match formal_params.get(i) {
                                Some(v) => {
                                    let labelled_parameter =
                                        *labelled_parameter_map.get(v).unwrap();
                                    let exp = &(*labelled_parameter.expression);
                                    env.insert(v.to_string(), self.interpret(exp));
                                }
                                None => panic!(),
                            }
                        }

                        self.environments.push(env);
                        ret = self.interpret(body);
                        self.environments.pop();
                    }
                    ret
                }
                None => panic!(),
            },
        }
    }
}

#[test]
fn interpret_literal() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Literal(42));

    assert_eq!(42, actual);
}

#[test]

fn interpret_binary_expression() {
    let mut interpreter = Interpreter::new();

    let actual_add = interpreter.interpret(&BinaryExpression {
        operator: Operator::Add,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(1 + 2, actual_add);

    let actual_subtract = interpreter.interpret(&BinaryExpression {
        operator: Operator::Subtract,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });

    assert_eq!(1 - 2, actual_subtract);

    let actual_multiply = interpreter.interpret(&BinaryExpression {
        operator: Operator::Multiply,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(1 * 2, actual_multiply);

    let actual_divide = interpreter.interpret(&BinaryExpression {
        operator: Operator::Divide,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(1 / 2, actual_divide);

    let actual_less_than = interpreter.interpret(&BinaryExpression {
        operator: Operator::LessThan,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(1, actual_less_than);

    let actual_less_or_equal = interpreter.interpret(&BinaryExpression {
        operator: Operator::LessOrEqual,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(1, actual_less_or_equal);

    let actual_greater_than = interpreter.interpret(&BinaryExpression {
        operator: Operator::GreaterThan,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(0, actual_greater_than);

    let actual_greater_or_equal = interpreter.interpret(&BinaryExpression {
        operator: Operator::GreaterOrEqual,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(0, actual_greater_or_equal);

    let actual_equal_equal = interpreter.interpret(&BinaryExpression {
        operator: Operator::EqualEqual,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(0, actual_equal_equal);

    let actual_not_equal = interpreter.interpret(&BinaryExpression {
        operator: Operator::NotEqual,
        lhs: Box::new(Literal(1)),
        rhs: Box::new(Literal(2)),
    });
    assert_eq!(1, actual_not_equal);
}

#[test]
fn interpret_identifier() {
    let mut interpreter = Interpreter::new();
    interpreter.set_variable("hoge".to_string(), 42);

    let actual = interpreter.interpret(&Identifier("hoge".to_string()));

    assert_eq!(42, actual);
}

#[test]
fn interpret_assignment() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Assignment {
        name: "hoge".to_string(),
        expression: Box::new(Literal(42)),
    });

    assert_eq!(42, actual);
    assert_eq!(Some(42i32), interpreter.get_variable("hoge".to_string()));
}

#[test]
fn interpret_block() {
    let mut interpreter = Interpreter::new();

    let actual = interpreter.interpret(&Block {
        expressions: vec![
            Assignment {
                name: "a".to_string(),
                expression: Box::new(Literal(1)),
            },
            Assignment {
                name: "b".to_string(),
                expression: Box::new(Literal(2)),
            },
            Assignment {
                name: "c".to_string(),
                expression: Box::new(Literal(3)),
            },
            Literal(4),
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

    let actual = interpreter.interpret(&While {
        condition: Box::new(BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Identifier("a".to_string())),
            rhs: Box::new(Literal(10)),
        }),
        body: Box::new(Assignment {
            name: "a".to_string(),
            expression: Box::new(BinaryExpression {
                operator: Operator::Add,
                lhs: Box::new(Identifier("a".to_string())),
                rhs: Box::new(Literal(1)),
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

    let actual = interpreter.interpret(&If {
        condition: Box::new(BinaryExpression {
            operator: Operator::LessThan,
            lhs: Box::new(Identifier("a".to_string())),
            rhs: Box::new(Literal(10)),
        }),
        then_clause: Box::new(Assignment {
            name: "b".to_string(),
            expression: Box::new(Literal(1)),
        }),
        else_clause: Some(Box::new(Assignment {
            name: "b".to_string(),
            expression: Box::new(Literal(2)),
        })),
    });

    assert_eq!(1, actual);
    assert_eq!(Some(1i32), interpreter.get_variable("b".to_string()));
}

#[test]
fn interpret_if_else() {
    let mut interpreter = Interpreter::new();
    interpreter.set_variable("a".to_string(), 0i32);

    let actual = interpreter.interpret(&If {
        condition: Box::new(BinaryExpression {
            operator: Operator::GreaterThan,
            lhs: Box::new(Identifier("a".to_string())),
            rhs: Box::new(Literal(10)),
        }),
        then_clause: Box::new(Assignment {
            name: "b".to_string(),
            expression: Box::new(Literal(1)),
        }),
        else_clause: Some(Box::new(Assignment {
            name: "b".to_string(),
            expression: Box::new(Literal(2)),
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
        body: FunctionCall {
            name: "hoge".to_string(),
            args: vec![Literal(1), Literal(2)],
        },
    };
    let hoge_func = TopLevel::FunctionDefinition {
        name: "hoge".to_string(),
        args: vec!["a".to_string(), "b".to_string()],
        body: BinaryExpression {
            operator: Operator::Add,
            lhs: Box::new(Identifier("a".to_string())),
            rhs: Box::new(Identifier("b".to_string())),
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
        expression: Box::new(Literal(42)),
    };
    let main_func = TopLevel::FunctionDefinition {
        name: "main".to_string(),
        args: vec![],
        body: Identifier("hoge".to_string()),
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
        body: Block {
            expressions: vec![FunctionCall {
                name: "fact".to_string(),
                args: vec![Literal(5)],
            }],
        },
    };
    let fact_func = TopLevel::FunctionDefinition {
        name: "fact".to_string(),
        args: vec!["n".to_string()],
        body: Block {
            expressions: vec![If {
                condition: Box::new(BinaryExpression {
                    operator: Operator::LessThan,
                    lhs: Box::new(Identifier("n".to_string())),
                    rhs: Box::new(Literal(2)),
                }),
                then_clause: Box::new(Literal(1)),
                else_clause: Some(Box::new(BinaryExpression {
                    operator: Operator::Multiply,
                    lhs: Box::new(Identifier("n".to_string())),
                    rhs: Box::new(FunctionCall {
                        name: "fact".to_string(),
                        args: vec![BinaryExpression {
                            operator: Operator::Subtract,
                            lhs: Box::new(Identifier("n".to_string())),
                            rhs: Box::new(Literal(1)),
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
