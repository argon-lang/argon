use argon_compiler::{Context, DefaultExprContext, EmptyHole};
use argon_ir::{Instruction, IrContextShifter, Register, Value};
use argon_util::CompileError;

use super::{Hole, Model, TypeCheckExprContext};

#[derive(Clone, Copy)]
pub(super) struct DefaultToTypeCheckContextShifter;

impl IrContextShifter for DefaultToTypeCheckContextShifter {
    type EC1 = DefaultExprContext;
    type EC2 = TypeCheckExprContext;

    fn shift_hole(&mut self, hole: EmptyHole) -> Value<Self::EC2> {
        match hole {}
    }

    fn shift_instruction_hole(
        &mut self,
        _destination: Register,
        hole: EmptyHole,
    ) -> Instruction<Self::EC2> {
        match hole {}
    }
}

pub(super) struct TypeCheckToDefaultContextShifter {
    pub context: Context,
    pub model: Model,
}

impl IrContextShifter for TypeCheckToDefaultContextShifter {
    type EC1 = TypeCheckExprContext;
    type EC2 = DefaultExprContext;

    fn shift_hole(&mut self, hole: Hole) -> Value<Self::EC2> {
        let _ = self.model.get_hole_value(&hole);
        self.context
            .reporter()
            .report_error(CompileError::could_not_infer(hole.0.location.clone()));
        Value::Error
    }

    fn shift_instruction_hole(
        &mut self,
        _destination: Register,
        hole: Hole,
    ) -> Instruction<Self::EC2> {
        self.context
            .reporter()
            .report_error(CompileError::could_not_infer(hole.0.location.clone()));
        panic!("instruction holes cannot be represented in the default expression context")
    }
}
