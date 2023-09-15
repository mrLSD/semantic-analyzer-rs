use crate::ast;
use crate::types::expression::Expression;
use crate::types::{Binding, FunctionCall, LetBinding};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    Great,
    Less,
    Eq,
    GreatEq,
    LessEq,
    NotEq,
}

impl From<ast::Condition> for Condition {
    fn from(value: ast::Condition) -> Self {
        match value {
            ast::Condition::Great => Self::Great,
            ast::Condition::Less => Self::Less,
            ast::Condition::Eq => Self::Eq,
            ast::Condition::GreatEq => Self::GreatEq,
            ast::Condition::LessEq => Self::LessEq,
            ast::Condition::NotEq => Self::NotEq,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicCondition {
    And,
    Or,
}

impl From<ast::LogicCondition> for LogicCondition {
    fn from(value: ast::LogicCondition) -> Self {
        match value {
            ast::LogicCondition::And => Self::And,
            ast::LogicCondition::Or => Self::Or,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionCondition {
    pub left: Expression,
    pub condition: Condition,
    pub right: Expression,
}

impl From<ast::ExpressionCondition<'_>> for ExpressionCondition {
    fn from(value: ast::ExpressionCondition<'_>) -> Self {
        Self {
            left: value.left.into(),
            condition: value.condition.into(),
            right: value.right.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionLogicCondition {
    pub left: ExpressionCondition,
    pub right: Option<(LogicCondition, Box<ExpressionLogicCondition>)>,
}

impl From<ast::ExpressionLogicCondition<'_>> for ExpressionLogicCondition {
    fn from(value: ast::ExpressionLogicCondition<'_>) -> Self {
        Self {
            left: value.left.into(),
            right: value
                .right
                .map(|(v, expr)| (v.into(), Box::new(expr.as_ref().clone().into()))),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfCondition {
    Single(Expression),
    Logic(ExpressionLogicCondition),
}

impl From<ast::IfCondition<'_>> for IfCondition {
    fn from(value: ast::IfCondition<'_>) -> Self {
        match value {
            ast::IfCondition::Single(v) => Self::Single(v.into()),
            ast::IfCondition::Logic(v) => Self::Logic(v.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: IfCondition,
    pub body: IfBodyStatements,
    pub else_statement: Option<IfBodyStatements>,
    pub else_if_statement: Option<Box<IfStatement>>,
}

impl From<ast::IfStatement<'_>> for IfStatement {
    fn from(value: ast::IfStatement<'_>) -> Self {
        Self {
            condition: value.condition.into(),
            body: value.body.into(),
            else_statement: value.else_statement.map(Into::into),
            else_if_statement: value
                .else_if_statement
                .map(|v| Box::new(v.as_ref().clone().into())),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatements {
    If(Vec<IfBodyStatement>),
    Loop(Vec<IfLoopBodyStatement>),
}

impl From<ast::IfBodyStatements<'_>> for IfBodyStatements {
    fn from(value: ast::IfBodyStatements<'_>) -> Self {
        match value {
            ast::IfBodyStatements::If(v) => Self::If(v.iter().map(|v| v.clone().into()).collect()),
            ast::IfBodyStatements::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopBodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Return(Expression),
    Break,
    Continue,
}

impl From<ast::LoopBodyStatement<'_>> for LoopBodyStatement {
    fn from(value: ast::LoopBodyStatement<'_>) -> Self {
        match value {
            ast::LoopBodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::LoopBodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::LoopBodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::LoopBodyStatement::If(v) => Self::If(v.into()),
            ast::LoopBodyStatement::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::LoopBodyStatement::Return(v) => Self::Return(v.into()),
            ast::LoopBodyStatement::Break => Self::Break,
            ast::LoopBodyStatement::Continue => Self::Continue,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Return(Expression),
}

impl From<ast::IfBodyStatement<'_>> for IfBodyStatement {
    fn from(value: ast::IfBodyStatement<'_>) -> Self {
        match value {
            ast::IfBodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::IfBodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::IfBodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::IfBodyStatement::If(v) => Self::If(v.into()),
            ast::IfBodyStatement::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::IfBodyStatement::Return(v) => Self::Return(v.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfLoopBodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Return(Expression),
    Break,
    Continue,
}

impl From<ast::IfLoopBodyStatement<'_>> for IfLoopBodyStatement {
    fn from(value: ast::IfLoopBodyStatement<'_>) -> Self {
        match value {
            ast::IfLoopBodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::IfLoopBodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::IfLoopBodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::IfLoopBodyStatement::If(v) => Self::If(v.into()),
            ast::IfLoopBodyStatement::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::IfLoopBodyStatement::Return(v) => Self::Return(v.into()),
            ast::IfLoopBodyStatement::Break => Self::Break,
            ast::IfLoopBodyStatement::Continue => Self::Continue,
        }
    }
}
