use derivative::Derivative;
use esexpr::core_types::num_bigint::BigInt;
use crate::{Builtin, Expr, ExprContext, NormalizerScanner, Unify};



#[derive(Derivative)]
#[derivative(Debug)]
pub enum ExpectedType<'a, EC: ExprContext + ?Sized> {
    AnyMetaType,
    Exact(&'a Expr<EC>),
}

impl<EC: ExprContext + ?Sized> Clone for ExpectedType<'_, EC> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<EC: ExprContext + ?Sized> Copy for ExpectedType<'_, EC> {}

pub enum TypeCompareTransformation<EC: ExprContext + ?Sized> {
    Box(Expr<EC>),
    Unbox(Expr<EC>),
}


pub trait TypeComparer: Unify {
    fn is_type(&mut self, mut expr: Expr<Self::EC>) -> bool {
        {
            let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
            norm.normalize(&mut expr);
        }

        match expr {
            Expr::Type(_) => true,
            Expr::BigType(_) => true,
            Expr::Tuple { items } => items.into_iter().all(|item| self.is_type(item)),
            _ => false,
        }
    }

    fn type_matches_expected(
        &mut self,
        transformations: &mut Vec<TypeCompareTransformation<Self::EC>>,
        actual_type: &Expr<Self::EC>,
        expected_type: ExpectedType<'_, Self::EC>,
    ) -> bool {
        match expected_type {
            ExpectedType::AnyMetaType => self.is_type(actual_type.clone()),

            ExpectedType::Exact(expected_type) => {
                let mut expected_type = expected_type.clone();
                let mut actual_type = actual_type.clone();

                loop {
                    {
                        let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
                        norm.normalize(&mut expected_type);
                    }

                    {
                        let mut norm = NormalizerScanner::new(self.normalize_fuel(), self.normalizer());
                        norm.normalize(&mut actual_type);
                    }

                    match actual_type {
                        Expr::Type(ref n) => match &expected_type {
                            Expr::BigType(_) => return true,
                            Expr::Type(n2) => match (&**n, &**n2) {
                                (Expr::IntLiteral(n), Expr::IntLiteral(n2)) => {
                                    return n >= &BigInt::ZERO && n2 >= &BigInt::ZERO && n <= n2;
                                }
                                _ => {}
                            },
                            _ => {}
                        },

                        Expr::Builtin(Builtin::NeverType) => {
                            return true;
                        }

                        Expr::BoxedType(actual_unboxed) => {
                            if let Expr::BoxedType(expected_unboxed) = expected_type {
                                expected_type = *expected_unboxed;
                            } else {
                                transformations.push(TypeCompareTransformation::Unbox(
                                    (*actual_unboxed).clone(),
                                ));
                            }

                            actual_type = *actual_unboxed;
                            continue;
                        }

                        _ => {}
                    }

                    match expected_type {
                        Expr::BoxedType(expected_unboxed) => {
                            transformations.push(TypeCompareTransformation::Box(
                                (*expected_unboxed).clone(),
                            ));
                            expected_type = *expected_unboxed;
                            continue;
                        }

                        _ => {}
                    }

                    break;
                }

                self.unify(expected_type, actual_type)
            }
        }
    }
}
