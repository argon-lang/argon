use crate::{Expr, ExprContext};
use argon_parser::ast::Identifier;
use argon_util::UniqueIdentifier;
use core::hash::{Hash, Hasher};
use derivative::Derivative;

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub struct BlockLabel<EC: ExprContext + ?Sized> {
    pub id: UniqueIdentifier,
    pub name: Option<Identifier>,
    pub kind: BlockLabelKind,
    pub block_result_type: Expr<EC>,
}

impl<EC: ExprContext + ?Sized> PartialEq for BlockLabel<EC> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<EC: ExprContext + ?Sized> Eq for BlockLabel<EC> {}

impl<EC: ExprContext + ?Sized> Hash for BlockLabel<EC> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BlockLabelKind {
    Block,
    Condition,
    Loop,
    WhileOuter,
    WhileInner,
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub enum BlockLabelDeclaration<EC: ExprContext + ?Sized> {
    Block(BlockLabel<EC>),
    Loop(LoopLabels<EC>),
}

#[derive(Derivative)]
#[derivative(Debug(bound = ""))]
#[derivative(Clone(bound = ""))]
pub enum LoopLabels<EC: ExprContext + ?Sized> {
    // Loop has a single label
    // break exits, redo and next go back to the beginning
    Loop(BlockLabel<EC>),

    // While has two labels
    // break exits
    // retry goes back to the beginning of the inner block
    // next goes back to the beginning of the outer block
    While {
        outer: BlockLabel<EC>,
        inner: BlockLabel<EC>,
    },
}

impl<EC: ExprContext + ?Sized> LoopLabels<EC> {
    pub fn name(&self) -> Option<&Identifier> {
        match self {
            LoopLabels::Loop(label) => label.name.as_ref(),
            LoopLabels::While { outer, .. } => outer.name.as_ref(),
        }
    }

    pub fn break_label(self) -> BlockLabel<EC> {
        match self {
            LoopLabels::Loop(label) => label,
            LoopLabels::While { outer, .. } => outer,
        }
    }

    pub fn next_label(self) -> (BlockLabel<EC>, bool) {
        match self {
            LoopLabels::Loop(label) => (label, false),
            LoopLabels::While { inner, .. } => (inner, true),
        }
    }

    pub fn redo_label(self) -> BlockLabel<EC> {
        match self {
            LoopLabels::Loop(label) => label,
            LoopLabels::While { inner, .. } => inner,
        }
    }
}
