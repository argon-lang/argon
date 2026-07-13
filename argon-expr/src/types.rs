use crate::{Builtin, Expr, ExprContext, NormalizerScanner, Unify};
use derivative::Derivative;
use esexpr::core_types::num_bigint::BigInt;
use std::mem;

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
    Share,
    Borrow,
    BorrowMut,
}

impl<EC: ExprContext + ?Sized> TypeCompareTransformation<EC> {
    pub fn transform(self, e: &mut Expr<EC>, t: &mut Expr<EC>) {
        match self {
            TypeCompareTransformation::Box(box_type) => {
                let e2 = mem::replace(e, Expr::Error);
                let t2 = mem::replace(t, Expr::Error);

                *e = Expr::Box {
                    t: Box::new(box_type),
                    value: Box::new(e2),
                };
                *t = Expr::BoxedType(Box::new(t2));
            }
            TypeCompareTransformation::Unbox(box_type) => {
                let e2 = mem::replace(e, Expr::Error);

                *e = Expr::Unbox {
                    t: Box::new(box_type.clone()),
                    value: Box::new(e2),
                };
                *t = box_type;
            }
            TypeCompareTransformation::Share => {
                let e2 = mem::replace(e, Expr::Error);
                let t2 = mem::replace(t, Expr::Error);

                *e = Expr::Share {
                    value: Box::new(e2),
                };
                *t = Expr::Shared {
                    inner: Box::new(t2),
                };
            }
            TypeCompareTransformation::Borrow => {
                let e2 = mem::replace(e, Expr::Error);
                let t2 = mem::replace(t, Expr::Error);

                *e = Expr::Borrow {
                    value: Box::new(e2),
                };
                *t = Expr::Use {
                    is_mutable: false,
                    inner: Box::new(t2),
                };
            }
            TypeCompareTransformation::BorrowMut => {
                let e2 = mem::replace(e, Expr::Error);
                let t2 = mem::replace(t, Expr::Error);

                *e = Expr::BorrowMut {
                    value: Box::new(e2),
                };
                *t = Expr::Use {
                    is_mutable: true,
                    inner: Box::new(t2),
                };
            }
        }
    }
}

pub trait TypeComparer: Unify {
    fn is_type(&self, mut expr: Expr<Self::EC>) -> bool {
        {
            let mut model = self.model().clone();
            let mut norm =
                NormalizerScanner::new(self.normalize_fuel(), Self::normalizer(&mut model));
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
                        let mut norm = NormalizerScanner::new(
                            self.normalize_fuel(),
                            Self::normalizer(self.model_mut()),
                        );
                        norm.normalize(&mut expected_type);
                    }

                    {
                        let mut norm = NormalizerScanner::new(
                            self.normalize_fuel(),
                            Self::normalizer(self.model_mut()),
                        );
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
                            transformations
                                .push(TypeCompareTransformation::Box((*expected_unboxed).clone()));
                            expected_type = *expected_unboxed;
                            continue;
                        }

                        Expr::Use {
                            is_mutable: false,
                            inner: inner_expected,
                        } => {
                            if let Expr::Use {
                                is_mutable: false,
                                inner: inner_actual,
                            } = actual_type
                            {
                                actual_type = *inner_actual;
                            } else {
                                transformations.push(TypeCompareTransformation::Borrow);
                            }

                            expected_type = *inner_expected;
                        }

                        Expr::Use {
                            is_mutable: true,
                            inner: inner_expected,
                        } => {
                            if let Expr::Use {
                                is_mutable: true,
                                inner: inner_actual,
                            } = actual_type
                            {
                                actual_type = *inner_actual;
                            } else {
                                transformations.push(TypeCompareTransformation::BorrowMut);
                            }

                            expected_type = *inner_expected;
                        }

                        Expr::Shared {
                            inner: inner_expected,
                        } => {
                            if let Expr::Shared {
                                inner: inner_actual,
                            } = actual_type
                            {
                                actual_type = *inner_actual;
                            } else {
                                transformations.push(TypeCompareTransformation::Share);
                            }

                            expected_type = *inner_expected;
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
