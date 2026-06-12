use argon_compiler::DefaultExprContext;
use argon_expr::{BlockLabel, Expr};
use crate::vm::{AnyRegister, EmitResult, EmitStop, ExprEmitter};
use argon_format::vm as vf;

use alloc::boxed::Box;
use core::mem;
use crate::vm::pattern::emit_pattern;

pub(super) struct ConditionEmitter<'a, 'b> {
    pub expr_emitter: &'b mut ExprEmitter<'a>,
    pub when_true_label: &'b vf::BlockId,
    pub when_false_label: &'b vf::BlockId,
}

impl<'a, 'b> ConditionEmitter<'a, 'b> {
    pub(super) fn emit_condition(&mut self, mut expr: &Expr<DefaultExprContext>) -> EmitResult<()> {
        loop {
            match expr {
                Expr::BoolLiteral(true) => {},
                Expr::BoolLiteral(false) => {
                    self.expr_emitter.emit(vf::Instruction::BlockBreak {
                        block_id: Box::new(self.when_false_label.clone()),
                    });
                    return Err(EmitStop::Branch);
                },

                Expr::And(a, b) => {
                    let skip_a_true_label = self.expr_emitter.claim_block_id();
                    let (block, _) = self.expr_emitter.with_nested_block(|emitter| {
                        let mut cond_emitter = ConditionEmitter {
                            expr_emitter: emitter,
                            when_true_label: &skip_a_true_label,
                            when_false_label: self.when_false_label,
                        };

                        cond_emitter.emit_condition(a)
                    })?;
                    self.expr_emitter.emit_block(skip_a_true_label, block);
                    self.emit_condition(b)?;
                },

                Expr::Or(a, b) => {
                    let skip_a_false_label = self.expr_emitter.claim_block_id();
                    let (block, _) = self.expr_emitter.with_nested_block(|emitter| -> Result<(), _> {
                        let mut cond_emitter = ConditionEmitter {
                            expr_emitter: emitter,
                            when_true_label: self.when_true_label,
                            when_false_label: &skip_a_false_label,
                        };

                        cond_emitter.emit_condition(a)?;
                        emitter.emit(vf::Instruction::BlockBreak {
                            block_id: Box::new(self.when_true_label.clone()),
                        });
                        Err(EmitStop::Branch)
                    })?;

                    self.expr_emitter.emit_block(skip_a_false_label, block);

                    self.emit_condition(b)?;
                },

                Expr::Not(a) => {
                    let mut cond_emitter = ConditionEmitter {
                        expr_emitter: self.expr_emitter,
                        when_true_label: &self.when_false_label,
                        when_false_label: &self.when_true_label,
                    };

                    cond_emitter.emit_condition(a)?;
                    self.expr_emitter.emit(vf::Instruction::BlockBreak {
                        block_id: Box::new(self.when_false_label.clone()),
                    });
                    Err(EmitStop::Branch)?;
                },

                Expr::Is { value, pattern } => {
                    let r = self.expr_emitter.expr(value, AnyRegister)?;
                    emit_pattern(
                        self.expr_emitter,
                        self.when_false_label,
                        r,
                        pattern,
                    )?;
                }

                Expr::Condition { value, .. } => {
                    expr = &**value;
                    continue;
                },

                _ => {
                    let r = self.expr_emitter.expr(expr, AnyRegister)?;
                    self.expr_emitter.emit(vf::Instruction::BlockBreakUnless {
                        block_id: Box::new(self.when_false_label.clone()),
                        condition: Box::new(r),
                    });
                },
            }
            break;
        }

        Ok(())
    }
}

