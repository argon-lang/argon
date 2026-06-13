use crate::{Expr, ExprContext, Normalizer, NormalizerScanner};
use alloc::vec::Vec;
use argon_util::Fuel;

pub trait Unify {
    type EC: ExprContext;
    type Norm<'a>: Normalizer<EC = Self::EC>
    where
        Self: 'a;
    fn normalize_fuel(&self) -> Fuel;
    fn normalizer<'a>(&'a mut self) -> Self::Norm<'a>;

    fn unify_hole(&mut self, a: <Self::EC as ExprContext>::Hole, b: Expr<Self::EC>) -> bool;

    fn unify(&mut self, mut a: Expr<Self::EC>, mut b: Expr<Self::EC>) -> bool {
        {
            let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
            norm.normalize(&mut a);
        }
        {
            let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
            norm.normalize(&mut b);
        }

        match (a, b) {
            (Expr::Error, _) | (_, Expr::Error) => true,
            (Expr::Hole(a), b) => self.unify_hole(a, b),

            (a, Expr::Hole(b)) => self.unify_hole(b, a),

            (Expr::And(a1, b1), Expr::And(a2, b2)) | (Expr::Or(a1, b1), Expr::Or(a2, b2)) => {
                self.unify(*a1, *a2) && self.unify(*b1, *b2)
            }
            (Expr::BoolLiteral(a), Expr::BoolLiteral(b)) => a == b,
            (
                Expr::Builtin {
                    builtin: a,
                    arguments: a_args,
                },
                Expr::Builtin {
                    builtin: b,
                    arguments: b_args,
                },
            ) => a == b && self.unify_all(a_args, b_args),
            (Expr::EnumType(a), Expr::EnumType(b)) => {
                a.enum_ == b.enum_ && self.unify_all(a.arguments, b.arguments)
            }
            (
                Expr::FunctionType {
                    a: a_arg,
                    r: a_result,
                },
                Expr::FunctionType {
                    a: b_arg,
                    r: b_result,
                },
            ) => self.unify(*a_arg, *b_arg) && self.unify(*a_result, *b_result),
            (Expr::IntLiteral(a), Expr::IntLiteral(b)) => a == b,
            (Expr::RecordType(a), Expr::RecordType(b)) => {
                &a.record == &b.record && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::StringLiteral(a), Expr::StringLiteral(b)) => a == b,
            (Expr::TraitType(a), Expr::TraitType(b)) => {
                a.trait_ == b.trait_ && self.unify_all(a.arguments, b.arguments)
            }
            (Expr::Tuple { items: a }, Expr::Tuple { items: b }) => self.unify_all(a, b),
            (Expr::Type(a), Expr::Type(b)) => self.unify(*a, *b),
            (Expr::BigType(a), Expr::BigType(b)) => a == b,
            (Expr::Variable(a), Expr::Variable(b)) => a == b,
            _ => false,
        }
    }

    fn unify_all(&mut self, a: Vec<Expr<Self::EC>>, b: Vec<Expr<Self::EC>>) -> bool {
        a.len() == b.len() && a.into_iter().zip(b).all(|(a, b)| self.unify(a, b))
    }
}
