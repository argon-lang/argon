use super::{
    ExprNormalizer, TypeCheckExprContext, TypeChecker, build_subst_holes_for_args,
    default_to_type_check_shifter,
};
use alloc::{string::ToString, sync::Arc, vec, vec::Vec};
#[cfg(test)]
use argon_compiler::Context;
use argon_compiler::{Enum, EnumVariant, z3expr::Z3Expr};
use argon_expr::{
    Builtin, Expr, ExprContextShifter, ExprScannerMut, ExpressionOwner, LocatedExpr,
    LocatedPattern, NormalizerScanner, Pattern, SubstScanner,
};
use argon_z3::ast::{Ast, Bool, Dynamic, Int, Seq, String as Z3String};
use argon_z3::{Context as Z3Context, FuncDecl, SatResult};
use num_bigint::BigInt;
use parse18_runtime::Location;

pub(super) struct ExhaustiveChecker<'a, 'access, 'scope, 'model> {
    pub(super) tc: &'a mut TypeChecker<'access, 'scope, 'model>,
    pub(super) location: &'a Location,
}

impl<'a, 'access, 'scope, 'model> ExhaustiveChecker<'a, 'access, 'scope, 'model> {
    pub(super) fn check<'b>(
        &mut self,
        value_type: &LocatedExpr<TypeCheckExprContext>,
        patterns: impl IntoIterator<Item = &'b LocatedPattern<TypeCheckExprContext>>,
    ) -> bool {
        let z3 = Z3Context::new();
        let mut z3expr = Z3Expr::<TypeCheckExprContext>::new(&z3, self.tc.context.clone());
        let value = Dynamic::fresh_const("argon_match_value", &z3expr.argon_value_sort().value);

        let value_type_constraint = self.value_type_constraint(&mut z3expr, &value, value_type);
        z3expr.solver().assert(value_type_constraint);

        for pattern in patterns {
            let matched = self.pattern_constraint(&mut z3expr, &value, pattern);
            z3expr.solver().assert(matched.not());
        }

        let sat_result = z3expr.solver().check();

        sat_result == SatResult::Unsat
    }

    fn value_type_constraint<'z3>(
        &mut self,
        z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
        value: &Dynamic<'z3>,
        value_type: &LocatedExpr<TypeCheckExprContext>,
    ) -> Bool<'z3> {
        let mut value_type = value_type.clone();
        {
            let mut norm = NormalizerScanner::new(
                self.tc.context.normalize_fuel(),
                ExprNormalizer {
                    model: &mut self.tc.model,
                },
            );
            norm.normalize(&mut value_type);
        }

        match value_type.value {
            Expr::Builtin(Builtin::BoolType) => tester(
                z3expr,
                &z3expr.argon_value_sort().value_testers.bool_literal,
                value,
            ),

            Expr::Builtin(Builtin::IntType { .. }) => tester(
                z3expr,
                &z3expr.argon_value_sort().value_testers.int_literal,
                value,
            ),

            Expr::Builtin(Builtin::StringType) => tester(
                z3expr,
                &z3expr.argon_value_sort().value_testers.string_literal,
                value,
            ),

            Expr::Tuple { items } => {
                let tuple = tester(
                    z3expr,
                    &z3expr.argon_value_sort().value_testers.tuple,
                    value,
                );
                let values = tuple_items(z3expr, value);
                let length = values
                    .length()
                    .equals(&Int::from_u64(z3expr.z3_context(), items.len() as u64));
                let item_constraints = items
                    .iter()
                    .enumerate()
                    .map(|(idx, item_type)| {
                        let item = values.nth(idx as u64);
                        self.value_type_constraint(z3expr, &item, item_type)
                    })
                    .collect::<Vec<_>>();
                bool_and(
                    z3expr.z3_context(),
                    &[vec![tuple, length], item_constraints].concat(),
                )
            }

            Expr::EnumType(enum_type) => {
                let enum_literal = tester(
                    z3expr,
                    &z3expr.argon_value_sort().value_testers.enum_variant_literal,
                    value,
                );
                let value_enum = enum_literal_enum(z3expr, value);
                let value_variant = enum_literal_variant(z3expr, value);
                let expected_enum = enum_term(z3expr, enum_type.enum_.clone());
                let enum_matches = value_enum.equals(&expected_enum);
                let declared_variant_matches = enum_type
                    .enum_
                    .clone()
                    .variants()
                    .iter()
                    .map(|variant| {
                        let sig = variant.clone().signature().as_ref().clone();
                        let mut sig = sig.shift(&mut default_to_type_check_shifter());
                        let mut subst = SubstScanner::new();
                        build_subst_holes_for_args(
                            self.location,
                            &mut subst,
                            &ExpressionOwner::EnumVariant(variant.clone()),
                            &sig,
                        );
                        sig.scan_mut(&mut subst);

                        let mut conj = Vec::new();

                        let enum_term = z3expr.enum_term(&enum_type.enum_);
                        let enum_variant_term = z3expr.enum_variant_term(&variant);

                        conj.push(value_variant.equals(&enum_variant_term));
                        conj.push(z3expr.is_variant_of_enum(&enum_term, &enum_variant_term));
                        conj.extend(sig.parameters.into_iter().enumerate().map(|(i, param)| {
                            let param_term = z3expr.enum_variant_arg(value, i);

                            self.value_type_constraint(z3expr, &param_term, &param.param_type)
                        }));
                        conj.extend(variant.clone().fields().iter().map(|field| {
                            let field_type = field.clone().field_type().as_ref().clone();
                            let mut field_type = default_to_type_check_shifter().shift(field_type);
                            subst.scan(&mut field_type);

                            let field_term = z3expr.enum_field(value, field);

                            self.value_type_constraint(z3expr, &field_term, &field_type)
                        }));

                        bool_and(z3expr.z3_context(), &conj)
                    })
                    .collect::<Vec<_>>();
                bool_and(
                    z3expr.z3_context(),
                    &[
                        enum_literal,
                        enum_matches,
                        bool_or(z3expr.z3_context(), &declared_variant_matches),
                    ],
                )
            }

            _ => Bool::from_bool(z3expr.z3_context(), true),
        }
    }

    fn pattern_constraint<'z3>(
        &mut self,
        z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
        value: &Dynamic<'z3>,
        pattern: &LocatedPattern<TypeCheckExprContext>,
    ) -> Bool<'z3> {
        match &pattern.value {
            Pattern::Error => Bool::from_bool(z3expr.z3_context(), false),

            Pattern::Discard { .. } => Bool::from_bool(z3expr.z3_context(), true),

            Pattern::Binding(_, inner) => self.pattern_constraint(z3expr, value, inner),

            Pattern::Tuple(items) => {
                let tuple = tester(
                    z3expr,
                    &z3expr.argon_value_sort().value_testers.tuple,
                    value,
                );
                let values = tuple_items(z3expr, value);
                let length = values
                    .length()
                    .equals(&Int::from_u64(z3expr.z3_context(), items.len() as u64));
                let item_constraints = items
                    .iter()
                    .enumerate()
                    .map(|(idx, item_pattern)| {
                        let item = values.nth(idx as u64);
                        self.pattern_constraint(z3expr, &item, item_pattern)
                    })
                    .collect::<Vec<_>>();
                bool_and(
                    z3expr.z3_context(),
                    &[vec![tuple, length], item_constraints].concat(),
                )
            }

            Pattern::EnumVariant {
                enum_type,
                variant,
                args,
                fields,
            } => {
                let enum_literal = tester(
                    z3expr,
                    &z3expr.argon_value_sort().value_testers.enum_variant_literal,
                    value,
                );
                let value_enum = enum_literal_enum(z3expr, value);
                let value_variant = enum_literal_variant(z3expr, value);
                let expected_enum = enum_term(z3expr, enum_type.enum_.clone());
                let expected_variant = enum_variant_term(z3expr, variant.clone());
                let enum_matches = value_enum.equals(&expected_enum);
                let variant_matches = value_variant.equals(&expected_variant);

                let arguments = enum_literal_arguments(z3expr, value);

                let argument_constraints = args
                    .iter()
                    .enumerate()
                    .map(|(idx, arg_pattern)| {
                        let arg = arguments.nth(idx as u64);
                        self.pattern_constraint(z3expr, &arg, arg_pattern)
                    })
                    .collect::<Vec<_>>();

                let field_constraints = fields
                    .iter()
                    .map(|_| Bool::from_bool(z3expr.z3_context(), true))
                    .collect::<Vec<_>>();

                let mut constraints = vec![enum_literal, enum_matches, variant_matches];
                constraints.extend(argument_constraints);
                constraints.extend(field_constraints);

                bool_and(z3expr.z3_context(), &constraints)
            }

            Pattern::String(pattern_string) => {
                match Z3String::from_str(z3expr.z3_context(), pattern_string) {
                    Ok(pattern_value) => {
                        let string_literal = tester(
                            z3expr,
                            &z3expr.argon_value_sort().value_testers.string_literal,
                            value,
                        );
                        let actual_value = string_literal_value(z3expr, value);
                        bool_and(
                            z3expr.z3_context(),
                            &[string_literal, actual_value.equals(&pattern_value)],
                        )
                    }
                    Err(_) => Bool::from_bool(z3expr.z3_context(), false),
                }
            }

            Pattern::Int(pattern_int) => {
                let int_literal = tester(
                    z3expr,
                    &z3expr.argon_value_sort().value_testers.int_literal,
                    value,
                );
                let actual_value = int_literal_value(z3expr, value);
                bool_and(
                    z3expr.z3_context(),
                    &[
                        int_literal,
                        actual_value.equals(&z3_int_from_big_int(z3expr.z3_context(), pattern_int)),
                    ],
                )
            }

            Pattern::Bool(pattern_bool) => {
                let bool_literal = tester(
                    z3expr,
                    &z3expr.argon_value_sort().value_testers.bool_literal,
                    value,
                );
                let actual_value = bool_literal_value(z3expr, value);
                bool_and(
                    z3expr.z3_context(),
                    &[
                        bool_literal,
                        actual_value.equals(&Bool::from_bool(z3expr.z3_context(), *pattern_bool)),
                    ],
                )
            }
        }
    }
}

fn tester<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    tester: &FuncDecl<'z3>,
    value: &Dynamic<'z3>,
) -> Bool<'z3> {
    dynamic_to_bool(tester.apply(&[value]), z3expr)
}

fn tuple_items<'z3>(z3expr: &Z3Expr<'z3, TypeCheckExprContext>, value: &Dynamic<'z3>) -> Seq<'z3> {
    dynamic_to_seq(
        z3expr
            .argon_value_sort()
            .value_accessors
            .tuple_items
            .apply(&[value]),
    )
}

fn enum_literal_enum<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    value: &Dynamic<'z3>,
) -> Dynamic<'z3> {
    z3expr
        .argon_value_sort()
        .value_accessors
        .enum_variant_literal_enum
        .apply(&[value])
}

fn enum_literal_variant<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    value: &Dynamic<'z3>,
) -> Dynamic<'z3> {
    z3expr
        .argon_value_sort()
        .value_accessors
        .enum_variant_literal_variant
        .apply(&[value])
}

fn enum_literal_arguments<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    value: &Dynamic<'z3>,
) -> Seq<'z3> {
    dynamic_to_seq(
        z3expr
            .argon_value_sort()
            .value_accessors
            .enum_variant_literal_arguments
            .apply(&[value]),
    )
}

fn string_literal_value<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    value: &Dynamic<'z3>,
) -> Z3String<'z3> {
    z3expr
        .argon_value_sort()
        .value_accessors
        .string_literal_value
        .apply(&[value])
        .as_string()
        .expect("string literal accessor must return a Z3 string")
}

fn int_literal_value<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    value: &Dynamic<'z3>,
) -> Int<'z3> {
    z3expr
        .argon_value_sort()
        .value_accessors
        .int_literal_value
        .apply(&[value])
        .as_int()
        .expect("int literal accessor must return a Z3 int")
}

fn bool_literal_value<'z3>(
    z3expr: &Z3Expr<'z3, TypeCheckExprContext>,
    value: &Dynamic<'z3>,
) -> Bool<'z3> {
    z3expr
        .argon_value_sort()
        .value_accessors
        .bool_literal_value
        .apply(&[value])
        .as_bool()
        .expect("bool literal accessor must return a Z3 bool")
}

fn enum_term<'z3>(
    z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
    enum_: Arc<dyn Enum>,
) -> Dynamic<'z3> {
    z3expr.enum_term(&enum_)
}

fn enum_variant_term<'z3>(
    z3expr: &mut Z3Expr<'z3, TypeCheckExprContext>,
    variant: Arc<dyn EnumVariant>,
) -> Dynamic<'z3> {
    z3expr.enum_variant_term(&variant)
}

fn bool_and<'z3>(context: &'z3 Z3Context, values: &[Bool<'z3>]) -> Bool<'z3> {
    if values.is_empty() {
        Bool::from_bool(context, true)
    } else {
        Bool::and(&values.iter().collect::<Vec<_>>())
    }
}

fn bool_or<'z3>(context: &'z3 Z3Context, values: &[Bool<'z3>]) -> Bool<'z3> {
    if values.is_empty() {
        Bool::from_bool(context, false)
    } else {
        Bool::or(&values.iter().collect::<Vec<_>>())
    }
}

fn dynamic_to_seq(value: Dynamic<'_>) -> Seq<'_> {
    value
        .as_seq()
        .expect("Argon value sequence accessor must return a Z3 sequence")
}

fn dynamic_to_bool<'z3>(value: Dynamic<'z3>, _: &Z3Expr<'z3, TypeCheckExprContext>) -> Bool<'z3> {
    value
        .as_bool()
        .expect("Argon value predicate must return a Z3 bool")
}

fn z3_int_from_big_int<'z3>(context: &'z3 Z3Context, value: &BigInt) -> Int<'z3> {
    match Int::from_str(context, &value.to_string()) {
        Ok(value) => value,
        Err(error) => panic!("failed to create Z3 integer from Argon integer literal: {error:?}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::type_checker::Model;
    use argon_compiler::access::AccessToken;
    use argon_compiler::scope::ShiftedScope;
    use argon_compiler::test_utils::TestScope;
    use argon_compiler::{ModulePath, TubeName};
    use argon_expr::{EnumType, ErasureMode, ExprLocationExt};
    use mitsein::vec1;

    fn test_location() -> Location {
        Location {
            file: std::path::PathBuf::from("test"),
            start: parse18_runtime::FilePosition { line: 0, column: 0 },
            end: parse18_runtime::FilePosition { line: 0, column: 0 },
        }
    }

    fn located_pattern(
        pattern: Pattern<TypeCheckExprContext>,
    ) -> LocatedPattern<TypeCheckExprContext> {
        LocatedPattern {
            value: pattern,
            location: test_location(),
        }
    }

    #[test]
    fn bool_patterns_are_exhaustive_when_both_literals_are_present() {
        let context: Context = argon_compiler::test_utils::TestContext::default().into();
        let access = AccessToken::new(
            TubeName(vec1!["test".to_owned()]),
            ModulePath(vec!["test".to_owned()]),
        );
        let mut model = Model::new();
        let scope = TestScope;
        let shifted_scope = ShiftedScope::new(&scope, default_to_type_check_shifter());
        let mut local_scope = argon_compiler::scope::LocalVariableScope::new(shifted_scope);
        let mut tc = TypeChecker {
            context: context.clone(),
            access: &access,
            scope: &mut local_scope,
            model: &mut model,
            erasure_check_mode: ErasureMode::Concrete,
            ensures_clauses: Vec::new(),
            return_position: false,
            enable_ownership: false,
        };
        let location = test_location();
        let mut checker = ExhaustiveChecker {
            tc: &mut tc,
            location: &location,
        };

        assert!(checker.check(
            &Expr::bool_type().with_location(location.clone()),
            &[
                located_pattern(Pattern::Bool(true)),
                located_pattern(Pattern::Bool(false))
            ],
        ));
    }

    #[test]
    fn bool_patterns_are_not_exhaustive_when_one_literal_is_missing() {
        let context: Context = argon_compiler::test_utils::TestContext::default().into();
        let access = AccessToken::new(
            TubeName(vec1!["test".to_owned()]),
            ModulePath(vec!["test".to_owned()]),
        );
        let mut model = Model::new();
        let scope = TestScope;
        let shifted_scope = ShiftedScope::new(&scope, default_to_type_check_shifter());
        let mut local_scope = argon_compiler::scope::LocalVariableScope::new(shifted_scope);
        let mut tc = TypeChecker {
            context: context.clone(),
            access: &access,
            scope: &mut local_scope,
            model: &mut model,
            erasure_check_mode: ErasureMode::Concrete,
            ensures_clauses: Vec::new(),
            return_position: false,
            enable_ownership: false,
        };
        let location = test_location();
        let mut checker = ExhaustiveChecker {
            tc: &mut tc,
            location: &location,
        };

        assert!(!checker.check(
            &Expr::bool_type().with_location(location.clone()),
            &[located_pattern(Pattern::Bool(true))],
        ));
    }

    #[test]
    fn enum_patterns_are_exhaustive_when_every_variant_is_present() {
        let variant_a =
            Arc::new(argon_compiler::test_utils::TestEnumVariant::new("A")) as Arc<dyn EnumVariant>;
        let variant_b =
            Arc::new(argon_compiler::test_utils::TestEnumVariant::new("B")) as Arc<dyn EnumVariant>;
        let enum_ = Arc::new(argon_compiler::test_utils::TestEnum::new(vec![
            variant_a.clone(),
            variant_b.clone(),
        ]));
        let enum_type = EnumType {
            enum_: enum_.clone() as Arc<dyn Enum>,
            arguments: vec![],
        };

        let context: Context = argon_compiler::test_utils::TestContext::default().into();
        let access = AccessToken::new(
            TubeName(vec1!["test".to_owned()]),
            ModulePath(vec!["test".to_owned()]),
        );
        let mut model = Model::new();
        let scope = TestScope;
        let shifted_scope = ShiftedScope::new(&scope, default_to_type_check_shifter());
        let mut local_scope = argon_compiler::scope::LocalVariableScope::new(shifted_scope);
        let mut tc = TypeChecker {
            context: context.clone(),
            access: &access,
            scope: &mut local_scope,
            model: &mut model,
            erasure_check_mode: ErasureMode::Concrete,
            ensures_clauses: Vec::new(),
            return_position: false,
            enable_ownership: false,
        };
        let location = test_location();
        let mut checker = ExhaustiveChecker {
            tc: &mut tc,
            location: &location,
        };

        assert!(checker.check(
            &Expr::EnumType(enum_type.clone()).with_location(location.clone()),
            &[
                located_pattern(Pattern::EnumVariant {
                    enum_type: enum_type.clone(),
                    variant: variant_a,
                    args: vec![],
                    fields: vec![],
                }),
                located_pattern(Pattern::EnumVariant {
                    enum_type,
                    variant: variant_b,
                    args: vec![],
                    fields: vec![],
                }),
            ],
        ));
    }
}
