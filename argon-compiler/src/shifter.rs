use crate::{
    DefaultExprContext, EmptyHole, Enum, EnumVariant, Function, Instance, Method, Record,
    RecordField, Trait,
};
use alloc::sync::Arc;
use argon_expr::{Expr, ExprContext, ExprContextShifter};
use core::marker::PhantomData;

pub struct DefaultToExprTypeContextShifter<EC: DefaultExprAssociatedTypes + ?Sized>(
    PhantomData<EC>,
);

impl<EC: DefaultExprAssociatedTypes + ?Sized> Clone for DefaultToExprTypeContextShifter<EC> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<EC: DefaultExprAssociatedTypes + ?Sized> Copy for DefaultToExprTypeContextShifter<EC> {}

impl<EC: DefaultExprAssociatedTypes + ?Sized> Default for DefaultToExprTypeContextShifter<EC> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

pub trait DefaultExprAssociatedTypes:
    ExprContext<
        Function = Arc<dyn Function>,
        Method = Arc<dyn Method>,
        Record = Arc<dyn Record>,
        RecordField = Arc<dyn RecordField>,
        Enum = Arc<dyn Enum>,
        EnumVariant = Arc<dyn EnumVariant>,
        Trait = Arc<dyn Trait>,
        Instance = Arc<dyn Instance>,
    >
{
}

impl<EC> DefaultExprAssociatedTypes for EC where
    EC: ExprContext<
            Function = Arc<dyn Function>,
            Method = Arc<dyn Method>,
            Record = Arc<dyn Record>,
            RecordField = Arc<dyn RecordField>,
            Enum = Arc<dyn Enum>,
            EnumVariant = Arc<dyn EnumVariant>,
            Trait = Arc<dyn Trait>,
            Instance = Arc<dyn Instance>,
        > + ?Sized
{
}

impl<EC: DefaultExprAssociatedTypes + ?Sized> ExprContextShifter
    for DefaultToExprTypeContextShifter<EC>
{
    type EC1 = DefaultExprContext;
    type EC2 = EC;

    fn shift_hole(&mut self, hole: EmptyHole) -> Expr<Self::EC2> {
        match hole {}
    }
}
