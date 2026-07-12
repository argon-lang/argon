use crate::{Context, Enum, EnumVariant, Record, RecordField, RecordFieldOwner};
use alloc::format;
use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;
use argon_expr::{Builtin, Expr, ExprContext, IntegerType, RecordFieldLiteral, Variable};
use core::str::FromStr;
use hashbrown::{HashMap, HashSet};
use z3::ast::{self, Ast, BV, Bool, Dynamic, Int, Seq, String as Z3String};
use z3::{
    DatatypeAccessor, DatatypeBuilder, DatatypeSort, DatatypeVariant, FuncDecl, Params, Pattern,
    Solver, Sort,
};

const EXPECTED_TYPE: &str = "ExpectedType";

pub struct Z3Expr<
    EC: ExprContext<Enum = Arc<dyn Enum>, EnumVariant = Arc<dyn EnumVariant>> + ?Sized,
> {
    solver: Solver,
    argon_value_sort: ArgonValueSort,
    expected_type_sort: ExpectedTypeSort,
    is_variant_of_enum: FuncDecl,
    inhabited: FuncDecl,
    opaque_values: HashMap<Expr<EC>, Dynamic>,
    asserted_enums: HashSet<Arc<dyn Enum>>,
    enum_terms: HashMap<Arc<dyn Enum>, Dynamic>,
    enum_variant_terms: HashMap<Arc<dyn EnumVariant>, Dynamic>,
    instance_terms: HashMap<EC::Instance, Dynamic>,
    record_terms: HashMap<EC::Record, Dynamic>,
    variable_terms: HashMap<Variable<EC>, Dynamic>,
}

struct ConstructorAxiomArg<'a> {
    name: &'static str,
    sort: Sort,
    accessor: Option<&'a FuncDecl>,
}

impl<
    EC: ExprContext<
            Enum = Arc<dyn Enum>,
            EnumVariant = Arc<dyn EnumVariant>,
            Record = Arc<dyn Record>,
            RecordField = Arc<dyn RecordField>,
        > + ?Sized,
> Z3Expr<EC>
{
    #[must_use]
    pub fn new(context: Context) -> Self {
        let argon_value_sort = ArgonValueSort::new();
        let expected_type_sort = ExpectedTypeSort::new(&argon_value_sort.value);
        let is_variant_of_enum = FuncDecl::new(
            "is_variant_of_enum",
            &[
                &argon_value_sort.enum_sort,
                &argon_value_sort.enum_variant_sort,
            ],
            &Sort::bool(),
        );
        let inhabited = FuncDecl::new(
            "argon_value_inhabited",
            &[&argon_value_sort.value],
            &Sort::bool(),
        );

        let result = Self {
            solver: Solver::new(),
            argon_value_sort,
            expected_type_sort,
            is_variant_of_enum,
            inhabited,
            opaque_values: HashMap::new(),
            asserted_enums: HashSet::new(),
            enum_terms: HashMap::new(),
            enum_variant_terms: HashMap::new(),
            instance_terms: HashMap::new(),
            record_terms: HashMap::new(),
            variable_terms: HashMap::new(),
        };
        let mut params = Params::new();
        params.set_u32("rlimit", context.z3_rlimit());
        result.solver.set_params(&params);
        result.assert_argon_value_constructor_disjointness();
        result.assert_argon_value_constructor_axioms();
        result
    }

    #[must_use]
    pub fn solver(&self) -> &Solver {
        &self.solver
    }

    #[must_use]
    pub fn argon_value_sort(&self) -> &ArgonValueSort {
        &self.argon_value_sort
    }

    #[must_use]
    pub fn expected_type_sort(&self) -> &ExpectedTypeSort {
        &self.expected_type_sort
    }

    #[must_use]
    pub fn is_variant_of_enum_func(&self) -> &FuncDecl {
        &self.is_variant_of_enum
    }

    #[must_use]
    pub fn inhabited_func(&self) -> &FuncDecl {
        &self.inhabited
    }

    #[must_use]
    pub fn is_variant_of_enum(&self, enum_: &impl Ast, variant: &impl Ast) -> Bool {
        dynamic_to_bool(self.is_variant_of_enum.apply(&[enum_, variant]))
    }

    pub fn inhabited(&mut self, expr: &Expr<EC>) -> Bool {
        match expr {
            Expr::Builtin(Builtin::NeverType) => Bool::from_bool(false),
            Expr::ConjunctionType { lhs, rhs } => {
                let lhs = self.inhabited(lhs);
                let rhs = self.inhabited(rhs);
                Bool::and(&[&lhs, &rhs])
            }
            Expr::DisjunctionType { lhs, rhs } => {
                let lhs = self.inhabited(lhs);
                let rhs = self.inhabited(rhs);
                Bool::or(&[&lhs, &rhs])
            }
            Expr::FunctionType { a, r } => {
                let premise = self.inhabited(&a.var_type);
                let consequence = self.inhabited(r);
                premise.implies(&consequence)
            }
            Expr::EqualToType { lhs, rhs, .. } => {
                let lhs = self.expr_to_z3(lhs);
                let rhs = self.expr_to_z3(rhs);
                lhs.eq(&rhs)
            }
            Expr::BoxedType(t) => self.inhabited(t),
            _ => {
                let value = self.expr_to_z3(expr);
                dynamic_to_bool(self.inhabited.apply(&[&value]))
            }
        }
    }

    pub fn enum_variant_arg(&self, value: &impl Ast, index: usize) -> Dynamic {
        let args = self
            .argon_value_sort
            .value_accessors
            .enum_variant_literal_arguments
            .apply(&[value])
            .as_seq()
            .expect("enum_variant_literal_arguments should return a sequence");
        args.nth(index as u64)
    }

    pub fn enum_field(&mut self, value: &impl Ast, field: &Arc<dyn RecordField>) -> Dynamic {
        let fields = self
            .argon_value_sort
            .value_accessors
            .enum_variant_literal_fields
            .apply(&[value])
            .as_seq()
            .expect("enum_variant_literal_fields should return a sequence");
        fields.nth(record_field_index(field) as u64)
    }

    fn enum_cached_term(&mut self, enum_: &Arc<dyn Enum>) -> Dynamic {
        let id = self.enum_terms.len();
        self.enum_terms
            .entry(enum_.clone())
            .or_insert_with(|| {
                Dynamic::new_const(format!("argon_enum_{id}"), &self.argon_value_sort.enum_sort)
            })
            .clone()
    }

    fn enum_variant_cached_term(&mut self, variant: &Arc<dyn EnumVariant>) -> Dynamic {
        let id = self.enum_variant_terms.len();
        self.enum_variant_terms
            .entry(variant.clone())
            .or_insert_with(|| {
                Dynamic::new_const(
                    format!("argon_enum_variant_{id}"),
                    &self.argon_value_sort.enum_variant_sort,
                )
            })
            .clone()
    }

    fn instance_term(&mut self, instance: &EC::Instance) -> Dynamic {
        let id = self.instance_terms.len();
        self.instance_terms
            .entry(instance.clone())
            .or_insert_with(|| {
                Dynamic::new_const(
                    format!("argon_instance_{id}"),
                    &self.argon_value_sort.instance_sort,
                )
            })
            .clone()
    }

    fn record_term(&mut self, record: &EC::Record) -> Dynamic {
        let id = self.record_terms.len();
        self.record_terms
            .entry(record.clone())
            .or_insert_with(|| {
                Dynamic::new_const(
                    format!("argon_record_{id}"),
                    &self.argon_value_sort.record_sort,
                )
            })
            .clone()
    }

    fn variable_term(&mut self, variable: &Variable<EC>) -> Dynamic {
        let id = self.variable_terms.len();
        let mut is_new = false;

        let c = self
            .variable_terms
            .entry(variable.clone())
            .or_insert_with(|| {
                is_new = true;
                Dynamic::new_const(format!("argon_var_{id}"), &self.argon_value_sort.value)
            })
            .clone();

        if is_new {
            self.assert_variable_type(&c, variable.var_type());
        }

        c
    }

    pub fn enum_term(&mut self, enum_: &Arc<dyn Enum>) -> Dynamic {
        let term = self.enum_cached_term(enum_);
        self.assert_exactly_one_variant_of_enum(enum_.clone());
        term
    }

    pub fn enum_variant_term(&mut self, variant: &Arc<dyn EnumVariant>) -> Dynamic {
        self.enum_variant_cached_term(variant)
    }

    fn assert_variable_type(&mut self, c: &Dynamic, t: &Expr<EC>) {
        match t {
            Expr::Builtin(Builtin::IntType { integer_type }) => {
                let tester = match integer_type {
                    IntegerType::Int => &self.argon_value_sort.value_testers.int_literal,
                    IntegerType::I8 => &self.argon_value_sort.value_testers.i8_literal,
                    IntegerType::U8 => &self.argon_value_sort.value_testers.u8_literal,
                };
                self.solver.assert(dynamic_to_bool(tester.apply(&[c])));
            }
            Expr::Builtin(Builtin::StringType) => {
                self.solver.assert(dynamic_to_bool(
                    self.argon_value_sort
                        .value_testers
                        .string_literal
                        .apply(&[c]),
                ));
            }
            Expr::Builtin(Builtin::BoolType) => {
                self.solver.assert(dynamic_to_bool(
                    self.argon_value_sort.value_testers.bool_literal.apply(&[c]),
                ));
            }
            Expr::RecordType(record_type) => {
                self.solver.assert(dynamic_to_bool(
                    self.argon_value_sort
                        .value_testers
                        .record_literal
                        .apply(&[c]),
                ));

                let record_assertion = self
                    .argon_value_sort
                    .value_accessors
                    .record_literal_record
                    .apply(&[c])
                    .eq(&self.record_term(&record_type.record));

                self.solver.assert(record_assertion);
            }
            Expr::EnumType(enum_type) => {
                self.solver.assert(dynamic_to_bool(
                    self.argon_value_sort
                        .value_testers
                        .enum_variant_literal
                        .apply(&[c]),
                ));

                let enum_assertion = self
                    .argon_value_sort
                    .value_accessors
                    .enum_variant_literal_enum
                    .apply(&[c])
                    .eq(&self.enum_term(&enum_type.enum_));

                self.solver.assert(enum_assertion);
            }
            Expr::TraitType(_) => {
                self.solver.assert(dynamic_to_bool(
                    self.argon_value_sort.value_testers.new_instance.apply(&[c]),
                ));
            }
            Expr::Tuple { items } => {
                self.solver.assert(dynamic_to_bool(
                    self.argon_value_sort.value_testers.tuple.apply(&[c]),
                ));

                let items_term = self
                    .argon_value_sort
                    .value_accessors
                    .tuple_items
                    .apply(&[c])
                    .as_seq()
                    .expect("tuple_items should return a sequence");

                for (i, item) in items.iter().enumerate() {
                    let item_term = items_term.nth(i as u64);
                    self.assert_variable_type(&item_term, item);
                }
            }
            _ => {}
        }
    }

    fn assert_exactly_one_variant_of_enum(&mut self, enum_: Arc<dyn Enum>) {
        if !self.asserted_enums.insert(enum_.clone()) {
            return;
        }

        let enum_assertion_id = self.asserted_enums.len() - 1;
        let enum_term = self.enum_term(&enum_);
        let variants = enum_.variants();

        let variant = Dynamic::new_const(
            format!("argon_enum_variant_forall_{enum_assertion_id}"),
            &self.argon_value_sort.enum_variant_sort,
        );
        let membership = self.is_variant_of_enum(&enum_term, &variant);
        let declared_variant_matches = variants
            .iter()
            .map(|declared_variant| {
                let declared_variant = self.enum_variant_term(declared_variant);
                variant.eq(&declared_variant)
            })
            .collect::<Vec<_>>();
        let declared_variant_match_refs = declared_variant_matches.iter().collect::<Vec<_>>();
        let declared_variant_match = Bool::or(&declared_variant_match_refs);
        let body = membership.eq(&declared_variant_match);
        let pattern = Pattern::new(&[&membership]);

        self.solver
            .assert(ast::forall_const(&[&variant], &[&pattern], &body));

        for declared_variant in variants.iter() {
            let declared_variant = self.enum_variant_term(declared_variant);
            self.solver
                .assert(self.is_variant_of_enum(&enum_term, &declared_variant));
        }
    }

    fn assert_argon_value_constructor_disjointness(&self) {
        let value = Dynamic::new_const(
            "argon_value_constructor_disjointness_value",
            &self.argon_value_sort.value,
        );
        let testers = self.argon_value_sort.value_tester_terms(&value);
        let weighted_testers = testers.iter().map(|tester| (tester, 1)).collect::<Vec<_>>();

        self.solver.assert(ast::forall_const(
            &[&value],
            &[],
            &Bool::pb_le(&weighted_testers, 1),
        ));

        let lhs = Dynamic::new_const(
            "argon_value_constructor_equality_lhs",
            &self.argon_value_sort.value,
        );
        let rhs = Dynamic::new_const(
            "argon_value_constructor_equality_rhs",
            &self.argon_value_sort.value,
        );
        let lhs_testers = self.argon_value_sort.value_tester_terms(&lhs);
        let rhs_testers = self.argon_value_sort.value_tester_terms(&rhs);
        let same_constructor_results = Bool::and(
            &lhs_testers
                .iter()
                .zip(&rhs_testers)
                .map(|(lhs_tester, rhs_tester)| lhs_tester.eq(rhs_tester))
                .collect::<Vec<_>>(),
        );
        let assertion = lhs.eq(&rhs).implies(&same_constructor_results);

        self.solver
            .assert(ast::forall_const(&[&lhs, &rhs], &[], &assertion));
    }

    fn assert_argon_value_constructor_axioms(&self) {
        let sort = &self.argon_value_sort;

        self.assert_value_constructor_axiom(
            "bool_literal",
            &sort.value_constructors.bool_literal,
            &sort.value_testers.bool_literal,
            &[ConstructorAxiomArg {
                name: "value",
                sort: Sort::bool(),
                accessor: Some(&sort.value_accessors.bool_literal_value),
            }],
        );
        self.assert_value_constructor_axiom(
            "int_literal",
            &sort.value_constructors.int_literal,
            &sort.value_testers.int_literal,
            &[ConstructorAxiomArg {
                name: "value",
                sort: Sort::int(),
                accessor: Some(&sort.value_accessors.int_literal_value),
            }],
        );
        self.assert_value_constructor_axiom(
            "u8_literal",
            &sort.value_constructors.u8_literal,
            &sort.value_testers.u8_literal,
            &[ConstructorAxiomArg {
                name: "value",
                sort: Sort::bitvector(8),
                accessor: Some(&sort.value_accessors.u8_literal_value),
            }],
        );
        self.assert_value_constructor_axiom(
            "i8_literal",
            &sort.value_constructors.i8_literal,
            &sort.value_testers.i8_literal,
            &[ConstructorAxiomArg {
                name: "value",
                sort: Sort::bitvector(8),
                accessor: Some(&sort.value_accessors.i8_literal_value),
            }],
        );
        self.assert_value_constructor_axiom(
            "string_literal",
            &sort.value_constructors.string_literal,
            &sort.value_testers.string_literal,
            &[ConstructorAxiomArg {
                name: "value",
                sort: Sort::string(),
                accessor: Some(&sort.value_accessors.string_literal_value),
            }],
        );
        self.assert_value_constructor_axiom(
            "record_literal",
            &sort.value_constructors.record_literal,
            &sort.value_testers.record_literal,
            &[
                ConstructorAxiomArg {
                    name: "record",
                    sort: sort.record_sort.clone(),
                    accessor: Some(&sort.value_accessors.record_literal_record),
                },
                ConstructorAxiomArg {
                    name: "fields",
                    sort: sort.value_seq.clone(),
                    accessor: Some(&sort.value_accessors.record_literal_fields),
                },
            ],
        );
        self.assert_value_constructor_axiom(
            "enum_variant_literal",
            &sort.value_constructors.enum_variant_literal,
            &sort.value_testers.enum_variant_literal,
            &[
                ConstructorAxiomArg {
                    name: "enum",
                    sort: sort.enum_sort.clone(),
                    accessor: Some(&sort.value_accessors.enum_variant_literal_enum),
                },
                ConstructorAxiomArg {
                    name: "variant",
                    sort: sort.enum_variant_sort.clone(),
                    accessor: Some(&sort.value_accessors.enum_variant_literal_variant),
                },
                ConstructorAxiomArg {
                    name: "arguments",
                    sort: sort.value_seq.clone(),
                    accessor: Some(&sort.value_accessors.enum_variant_literal_arguments),
                },
                ConstructorAxiomArg {
                    name: "fields",
                    sort: sort.value_seq.clone(),
                    accessor: Some(&sort.value_accessors.enum_variant_literal_fields),
                },
            ],
        );
        self.assert_value_constructor_axiom(
            "new_instance",
            &sort.value_constructors.new_instance,
            &sort.value_testers.new_instance,
            &[
                ConstructorAxiomArg {
                    name: "instance",
                    sort: sort.instance_sort.clone(),
                    accessor: Some(&sort.value_accessors.new_instance_instance),
                },
                ConstructorAxiomArg {
                    name: "arguments",
                    sort: sort.value_seq.clone(),
                    accessor: Some(&sort.value_accessors.new_instance_arguments),
                },
            ],
        );
        self.assert_value_constructor_axiom(
            "tuple",
            &sort.value_constructors.tuple,
            &sort.value_testers.tuple,
            &[ConstructorAxiomArg {
                name: "items",
                sort: sort.value_seq.clone(),
                accessor: Some(&sort.value_accessors.tuple_items),
            }],
        );
        self.assert_value_constructor_axiom(
            "type",
            &sort.value_constructors.r#type,
            &sort.value_testers.r#type,
            &[ConstructorAxiomArg {
                name: "level",
                sort: sort.value.clone(),
                accessor: Some(&sort.value_accessors.type_level),
            }],
        );
        self.assert_value_constructor_axiom(
            "big_type",
            &sort.value_constructors.big_type,
            &sort.value_testers.big_type,
            &[ConstructorAxiomArg {
                name: "level",
                sort: Sort::int(),
                accessor: Some(&sort.value_accessors.big_type_level),
            }],
        );
        self.assert_value_constructor_axiom(
            "boxed",
            &sort.value_constructors.boxed,
            &sort.value_testers.boxed,
            &[
                ConstructorAxiomArg {
                    name: "type",
                    sort: sort.value.clone(),
                    accessor: Some(&sort.value_accessors.boxed_type),
                },
                ConstructorAxiomArg {
                    name: "value",
                    sort: sort.value.clone(),
                    accessor: Some(&sort.value_accessors.boxed_value),
                },
            ],
        );
    }

    fn assert_value_constructor_axiom(
        &self,
        name: &str,
        constructor: &FuncDecl,
        tester: &FuncDecl,
        args: &[ConstructorAxiomArg<'_>],
    ) {
        let values = args
            .iter()
            .map(|arg| {
                Dynamic::new_const(format!("argon_value_{name}_axiom_{}", arg.name), &arg.sort)
            })
            .collect::<Vec<_>>();
        let value_refs = values
            .iter()
            .map(|value| value as &dyn Ast)
            .collect::<Vec<_>>();
        let constructed = constructor.apply(&value_refs);

        let tester_assertion = dynamic_to_bool(tester.apply(&[&constructed]));

        let mut assertions = Vec::new();
        assertions.push(tester_assertion);
        assertions.extend(args.iter().zip(&values).filter_map(|(arg, value)| {
            arg.accessor
                .map(|accessor| accessor.apply(&[&constructed]).eq(value))
        }));

        let assertion_refs = assertions.iter().collect::<Vec<_>>();
        let assertion = Bool::and(&assertion_refs);

        if value_refs.is_empty() {
            self.solver.assert(assertion);
        } else {
            let pattern = Pattern::new(&[&constructed]);
            self.solver
                .assert(ast::forall_const(&value_refs, &[&pattern], &assertion));
        }

        let inverse_value = Dynamic::new_const(
            format!("argon_value_{name}_inverse_axiom_value"),
            &self.argon_value_sort.value,
        );
        if let Some(accessed_values) = args
            .iter()
            .map(|arg| {
                arg.accessor
                    .map(|accessor| accessor.apply(&[&inverse_value]))
            })
            .collect::<Option<Vec<_>>>()
        {
            let accessed_value_refs = accessed_values
                .iter()
                .map(|value| value as &dyn Ast)
                .collect::<Vec<_>>();
            let reconstructed = constructor.apply(&accessed_value_refs);
            let tester_assertion = dynamic_to_bool(tester.apply(&[&inverse_value]));
            let inverse_assertion = tester_assertion.implies(&inverse_value.eq(&reconstructed));
            let pattern = Pattern::new(&[&tester_assertion]);

            self.solver.assert(ast::forall_const(
                &[&inverse_value],
                &[&pattern],
                &inverse_assertion,
            ));
        }

        if value_refs.is_empty() {
            let values_other = args
                .iter()
                .map(|arg| {
                    Dynamic::new_const(
                        format!("argon_value_other_{name}_axiom_{}", arg.name),
                        &arg.sort,
                    )
                })
                .collect::<Vec<_>>();
            let value_refs_other = values_other
                .iter()
                .map(|value| value as &dyn Ast)
                .collect::<Vec<_>>();
            let constructed_other = constructor.apply(&value_refs_other);

            let args_equal = Bool::and(
                &values
                    .iter()
                    .zip(&values_other)
                    .map(|(value, value_other)| value.eq(value_other))
                    .collect::<Vec<_>>(),
            );

            let injective_assertion = constructed.eq(constructed_other).implies(args_equal);

            let both_value_refs = value_refs
                .iter()
                .chain(value_refs_other.iter())
                .copied()
                .collect::<Vec<_>>();

            self.solver.assert(ast::forall_const(
                &both_value_refs,
                &[],
                &injective_assertion,
            ));
        }
    }

    #[must_use]
    pub fn expr_to_z3(&mut self, expr: &Expr<EC>) -> Dynamic {
        self.expr_to_z3_value(expr)
            .unwrap_or_else(|| self.opaque_expr_value(expr))
    }

    #[must_use]
    pub fn opaque_value_for_expr(&self, expr: &Expr<EC>) -> Option<&Dynamic> {
        self.opaque_values.get(expr)
    }

    #[must_use]
    pub fn opaque_values(&self) -> &HashMap<Expr<EC>, Dynamic> {
        &self.opaque_values
    }

    fn expr_to_z3_value(&mut self, expr: &Expr<EC>) -> Option<Dynamic> {
        match expr {
            Expr::And(a, b) => {
                let a = self.expr_to_z3(a);
                let b = self.expr_to_z3(b);
                let a = self.bool_literal_value(&a);
                let b = self.bool_literal_value(&b);
                let value = Bool::and(&[&a, &b]);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.bool_literal,
                    &[&value],
                ))
            }
            Expr::BoolLiteral(value) => {
                let value = Bool::from_bool(*value);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.bool_literal,
                    &[&value],
                ))
            }
            Expr::Builtin(builtin) => match builtin {
                Builtin::IntNegate {
                    integer_type,
                    value,
                } => {
                    let value = self.expr_to_z3(value);
                    match integer_type {
                        IntegerType::Int => {
                            let value = self.int_literal_value(&value).unary_minus();
                            Some(self.wrap_int_literal(&value))
                        }
                        IntegerType::I8 => {
                            let value = self.i8_literal_value(&value).bvneg();
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self.u8_literal_value(&value).bvneg();
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntBitNot {
                    integer_type,
                    value,
                } => {
                    let value = self.expr_to_z3(value);
                    match integer_type {
                        IntegerType::Int => None,
                        IntegerType::I8 => {
                            let value = self.i8_literal_value(&value).bvnot();
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self.u8_literal_value(&value).bvnot();
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntConvert {
                    dest_type,
                    source_type,
                    value
                } => {
                    match (dest_type, source_type) {
                        (IntegerType::I8, IntegerType::I8)
                        | (IntegerType::U8, IntegerType::U8)
                        | (IntegerType::Int, IntegerType::Int) => {
                            self.expr_to_z3_value(value)
                        }

                        (IntegerType::I8, IntegerType::Int) => {
                            let value = self.expr_to_z3(value);
                            let value = self.int_literal_value(&value);
                            Some(self.wrap_i8_literal(&BV::from_int(&value, 8)))
                        }

                        (IntegerType::U8, IntegerType::Int) => {
                            let value = self.expr_to_z3(value);
                            let value = self.int_literal_value(&value);
                            Some(self.wrap_u8_literal(&BV::from_int(&value, 8)))
                        }

                        (IntegerType::Int, IntegerType::I8) => {
                            let value = self.expr_to_z3(value);
                            let value = self.i8_literal_value(&value);
                            Some(self.wrap_int_literal(&Int::from_bv(&value, true)))
                        }

                        (IntegerType::Int, IntegerType::U8) => {
                            let value = self.expr_to_z3(value);
                            let value = self.u8_literal_value(&value);
                            Some(self.wrap_int_literal(&Int::from_bv(&value, false)))
                        }

                        (IntegerType::I8, IntegerType::U8) => {
                            let value = self.expr_to_z3(value);
                            let value = self.u8_literal_value(&value);
                            Some(self.wrap_i8_literal(&value))
                        }

                        (IntegerType::U8, IntegerType::I8) => {
                            let value = self.expr_to_z3(value);
                            let value = self.i8_literal_value(&value);
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                },
                Builtin::IntAdd {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => {
                            let lhs = self.int_literal_value(&lhs);
                            let rhs = self.int_literal_value(&rhs);
                            let value = Int::add(&[lhs, rhs]);
                            Some(self.wrap_int_literal(&value))
                        }
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvadd(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvadd(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntSub {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => {
                            let lhs = self.int_literal_value(&lhs);
                            let rhs = self.int_literal_value(&rhs);
                            let value = Int::sub(&[lhs, rhs]);
                            Some(self.wrap_int_literal(&value))
                        }
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvsub(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvsub(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntMul {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => {
                            let lhs = self.int_literal_value(&lhs);
                            let rhs = self.int_literal_value(&rhs);
                            let value = Int::mul(&[lhs, rhs]);
                            Some(self.wrap_int_literal(&value))
                        }
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvmul(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvmul(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntBitAnd {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => None,
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvand(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvand(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntBitOr {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => None,
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvor(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvor(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntBitXor {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => None,
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvxor(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvxor(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntBitShiftLeft {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => None,
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvshl(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvshl(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntBitShiftRight {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    match integer_type {
                        IntegerType::Int => None,
                        IntegerType::I8 => {
                            let value = self
                                .i8_literal_value(&lhs)
                                .bvashr(&self.i8_literal_value(&rhs));
                            Some(self.wrap_i8_literal(&value))
                        }
                        IntegerType::U8 => {
                            let value = self
                                .u8_literal_value(&lhs)
                                .bvlshr(&self.u8_literal_value(&rhs));
                            Some(self.wrap_u8_literal(&value))
                        }
                    }
                }
                Builtin::IntEq {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let value = match integer_type {
                        IntegerType::Int => self
                            .int_literal_value(&lhs)
                            .eq(&self.int_literal_value(&rhs)),
                        IntegerType::I8 => {
                            self.i8_literal_value(&lhs).eq(&self.i8_literal_value(&rhs))
                        }
                        IntegerType::U8 => {
                            self.u8_literal_value(&lhs).eq(&self.u8_literal_value(&rhs))
                        }
                    };
                    Some(self.wrap_bool_literal(&value))
                }
                Builtin::IntLt {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let value = match integer_type {
                        IntegerType::Int => self
                            .int_literal_value(&lhs)
                            .lt(&self.int_literal_value(&rhs)),
                        IntegerType::I8 => self
                            .i8_literal_value(&lhs)
                            .bvslt(&self.i8_literal_value(&rhs)),
                        IntegerType::U8 => self
                            .u8_literal_value(&lhs)
                            .bvult(&self.u8_literal_value(&rhs)),
                    };
                    Some(self.wrap_bool_literal(&value))
                }
                Builtin::IntLe {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let value = match integer_type {
                        IntegerType::Int => self
                            .int_literal_value(&lhs)
                            .le(&self.int_literal_value(&rhs)),
                        IntegerType::I8 => self
                            .i8_literal_value(&lhs)
                            .bvsle(&self.i8_literal_value(&rhs)),
                        IntegerType::U8 => self
                            .u8_literal_value(&lhs)
                            .bvule(&self.u8_literal_value(&rhs)),
                    };
                    Some(self.wrap_bool_literal(&value))
                }
                Builtin::IntGt {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let value = match integer_type {
                        IntegerType::Int => self
                            .int_literal_value(&lhs)
                            .gt(&self.int_literal_value(&rhs)),
                        IntegerType::I8 => self
                            .i8_literal_value(&lhs)
                            .bvsgt(&self.i8_literal_value(&rhs)),
                        IntegerType::U8 => self
                            .u8_literal_value(&lhs)
                            .bvugt(&self.u8_literal_value(&rhs)),
                    };
                    Some(self.wrap_bool_literal(&value))
                }
                Builtin::IntGe {
                    integer_type,
                    lhs,
                    rhs,
                } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let value = match integer_type {
                        IntegerType::Int => self
                            .int_literal_value(&lhs)
                            .ge(&self.int_literal_value(&rhs)),
                        IntegerType::I8 => self
                            .i8_literal_value(&lhs)
                            .bvsge(&self.i8_literal_value(&rhs)),
                        IntegerType::U8 => self
                            .u8_literal_value(&lhs)
                            .bvuge(&self.u8_literal_value(&rhs)),
                    };
                    Some(self.wrap_bool_literal(&value))
                }
                Builtin::StringConcat { values } => {
                    let values = values
                        .iter()
                        .map(|value| {
                            let value = self.expr_to_z3(value);
                            self.string_literal_value(&value)
                        })
                        .collect::<Vec<_>>();
                    let value = match values.as_slice() {
                        [] => {
                            Z3String::from_str("").expect("empty string must be a valid Z3 string")
                        }
                        [value] => value.clone(),
                        values => Z3String::concat(values),
                    };
                    Some(self.wrap_string_literal(&value))
                }
                Builtin::StringEq { lhs, rhs } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let lhs = self.string_literal_value(&lhs);
                    let rhs = self.string_literal_value(&rhs);
                    let value = lhs.eq(&rhs);
                    Some(self.wrap_bool_literal(&value))
                }
                Builtin::BoolEq { lhs, rhs } => {
                    let lhs = self.expr_to_z3(lhs);
                    let rhs = self.expr_to_z3(rhs);
                    let lhs = self.bool_literal_value(&lhs);
                    let rhs = self.bool_literal_value(&rhs);
                    let value = lhs.eq(&rhs);
                    Some(self.wrap_bool_literal(&value))
                }
                _ => None,
            },
            Expr::IntLiteral(value) => {
                let value = z3_int_from_big_int(value);
                Some(self.wrap_int_literal(&value))
            }
            Expr::I8Literal(value) => {
                let value = BV::from_i64((*value).into(), 8);
                Some(self.wrap_i8_literal(&value))
            }
            Expr::U8Literal(value) => {
                let value = BV::from_u64((*value).into(), 8);
                Some(self.wrap_u8_literal(&value))
            }
            Expr::StringLiteral(value) => match Z3String::from_str(value) {
                Ok(value) => Some(self.wrap_string_literal(&value)),
                Err(_) => None,
            },
            Expr::EnumVariantLiteral {
                enum_type,
                variant: enum_variant,
                arguments,
                fields,
            } => {
                let enum_ = self.enum_term(&enum_type.enum_);
                let variant = self.enum_variant_term(enum_variant);
                let arguments = self.value_seq(arguments);
                let field_values =
                    self.field_value_seq(enum_variant.clone().fields().as_ref(), fields);
                let value = self.construct_value(
                    &self
                        .argon_value_sort
                        .value_constructors
                        .enum_variant_literal,
                    &[&enum_, &variant, &arguments, &field_values],
                );
                Some(value)
            }
            Expr::NewInstance {
                instance,
                arguments,
            } => {
                let instance_term = self.instance_term(instance);
                let arguments = self.value_seq(arguments);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.new_instance,
                    &[&instance_term, &arguments],
                ))
            }
            Expr::Not(value) => {
                let value = self.expr_to_z3(value);
                let value = self.bool_literal_value(&value).not();
                Some(self.wrap_bool_literal(&value))
            }
            Expr::Or(a, b) => {
                let a = self.expr_to_z3(a);
                let b = self.expr_to_z3(b);
                let a = self.bool_literal_value(&a);
                let b = self.bool_literal_value(&b);
                let value = Bool::or(&[&a, &b]);
                Some(self.wrap_bool_literal(&value))
            }
            Expr::RecordLiteral {
                record_type,
                fields,
            } => {
                let record_term = self.record_term(&record_type.record);
                let field_values =
                    self.field_value_seq(record_type.record.clone().fields().as_ref(), fields);
                let value = self.construct_value(
                    &self.argon_value_sort.value_constructors.record_literal,
                    &[&record_term, &field_values],
                );
                Some(value)
            }
            Expr::Tuple { items } => {
                let items = self.value_seq(items);
                Some(
                    self.construct_value(
                        &self.argon_value_sort.value_constructors.tuple,
                        &[&items],
                    ),
                )
            }
            Expr::Type(level) => {
                let level = self.expr_to_z3(level);
                Some(
                    self.construct_value(
                        &self.argon_value_sort.value_constructors.r#type,
                        &[&level],
                    ),
                )
            }
            Expr::BigType(level) => {
                let level = z3_int_from_big_int(level);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.big_type,
                    &[&level],
                ))
            }
            Expr::Variable(variable) => Some(self.variable_term(variable)),
            Expr::Box { t, value } => {
                let t = self.expr_to_z3(t);
                let value = self.expr_to_z3(value);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.boxed,
                    &[&t, &value],
                ))
            }
            _ => None,
        }
    }

    fn value_seq(&mut self, values: &[Expr<EC>]) -> Seq {
        let units = values
            .iter()
            .map(|value| {
                let value = self.expr_to_z3(value);
                Seq::unit(&value)
            })
            .collect::<Vec<_>>();
        concat_seq(&self.argon_value_sort.value, &units)
    }

    fn field_value_seq(
        &mut self,
        field_order: &[Arc<dyn RecordField>],
        values: &[RecordFieldLiteral<EC>],
    ) -> Seq {
        let units = field_order
            .iter()
            .filter_map(|field| {
                values
                    .iter()
                    .find(|value| &value.field == field)
                    .map(|value| {
                        let value = self.expr_to_z3(&value.value);
                        Seq::unit(&value)
                    })
            })
            .collect::<Vec<_>>();
        concat_seq(&self.argon_value_sort.value, &units)
    }

    fn bool_literal_value(&self, value: &impl Ast) -> Bool {
        self.argon_value_sort
            .value_accessors
            .bool_literal_value
            .apply(&[value])
            .as_bool()
            .expect("bool literal accessor must return a Z3 bool")
    }

    fn int_literal_value(&self, value: &impl Ast) -> Int {
        self.argon_value_sort
            .value_accessors
            .int_literal_value
            .apply(&[value])
            .as_int()
            .expect("int literal accessor must return a Z3 int")
    }

    fn u8_literal_value(&self, value: &impl Ast) -> BV {
        self.argon_value_sort
            .value_accessors
            .u8_literal_value
            .apply(&[value])
            .as_bv()
            .expect("u8 literal accessor must return a Z3 bitvector")
    }

    fn i8_literal_value(&self, value: &impl Ast) -> BV {
        self.argon_value_sort
            .value_accessors
            .i8_literal_value
            .apply(&[value])
            .as_bv()
            .expect("i8 literal accessor must return a Z3 bitvector")
    }

    fn string_literal_value(&self, value: &impl Ast) -> Z3String {
        self.argon_value_sort
            .value_accessors
            .string_literal_value
            .apply(&[value])
            .as_string()
            .expect("string literal accessor must return a Z3 string")
    }

    fn wrap_bool_literal(&self, value: &Bool) -> Dynamic {
        self.construct_value(
            &self.argon_value_sort.value_constructors.bool_literal,
            &[value],
        )
    }

    fn wrap_int_literal(&self, value: &Int) -> Dynamic {
        self.construct_value(
            &self.argon_value_sort.value_constructors.int_literal,
            &[value],
        )
    }

    fn wrap_u8_literal(&self, value: &BV) -> Dynamic {
        self.construct_value(
            &self.argon_value_sort.value_constructors.u8_literal,
            &[value],
        )
    }

    fn wrap_i8_literal(&self, value: &BV) -> Dynamic {
        self.construct_value(
            &self.argon_value_sort.value_constructors.i8_literal,
            &[value],
        )
    }

    fn wrap_string_literal(&self, value: &Z3String) -> Dynamic {
        self.construct_value(
            &self.argon_value_sort.value_constructors.string_literal,
            &[value],
        )
    }

    fn opaque_expr_value(&mut self, expr: &Expr<EC>) -> Dynamic {
        let id = self.opaque_values.len();
        self.opaque_values
            .entry(expr.clone())
            .or_insert_with(|| {
                Dynamic::new_const(format!("argon_opaque_{id}"), &self.argon_value_sort.value)
            })
            .clone()
    }

    fn construct_value(&self, constructor: &FuncDecl, args: &[&dyn Ast]) -> Dynamic {
        constructor.apply(args)
    }
}

pub struct ArgonValueSort {
    pub value: Sort,
    pub value_seq: Sort,
    pub value_constructors: ArgonValueConstructors,
    pub value_testers: ArgonValueTesters,
    pub value_accessors: ArgonValueAccessors,
    pub record_sort: Sort,
    pub enum_sort: Sort,
    pub enum_variant_sort: Sort,
    pub instance_sort: Sort,
}

pub struct ArgonValueConstructors {
    pub bool_literal: FuncDecl,
    pub int_literal: FuncDecl,
    pub i8_literal: FuncDecl,
    pub u8_literal: FuncDecl,
    pub string_literal: FuncDecl,
    pub record_literal: FuncDecl,
    pub enum_variant_literal: FuncDecl,
    pub new_instance: FuncDecl,
    pub tuple: FuncDecl,
    pub r#type: FuncDecl,
    pub big_type: FuncDecl,
    pub boxed: FuncDecl,
}

pub struct ArgonValueTesters {
    pub bool_literal: FuncDecl,
    pub int_literal: FuncDecl,
    pub i8_literal: FuncDecl,
    pub u8_literal: FuncDecl,
    pub string_literal: FuncDecl,
    pub record_literal: FuncDecl,
    pub enum_variant_literal: FuncDecl,
    pub new_instance: FuncDecl,
    pub tuple: FuncDecl,
    pub r#type: FuncDecl,
    pub big_type: FuncDecl,
    pub boxed: FuncDecl,
}

pub struct ArgonValueAccessors {
    pub bool_literal_value: FuncDecl,
    pub int_literal_value: FuncDecl,
    pub i8_literal_value: FuncDecl,
    pub u8_literal_value: FuncDecl,
    pub string_literal_value: FuncDecl,
    pub record_literal_record: FuncDecl,
    pub record_literal_fields: FuncDecl,
    pub enum_variant_literal_enum: FuncDecl,
    pub enum_variant_literal_variant: FuncDecl,
    pub enum_variant_literal_arguments: FuncDecl,
    pub enum_variant_literal_fields: FuncDecl,
    pub new_instance_instance: FuncDecl,
    pub new_instance_arguments: FuncDecl,
    pub tuple_items: FuncDecl,
    pub type_level: FuncDecl,
    pub big_type_level: FuncDecl,
    pub boxed_type: FuncDecl,
    pub boxed_value: FuncDecl,
}

pub struct ExpectedTypeSort {
    pub sort: Sort,
    pub constructors: ExpectedTypeConstructors,
    pub testers: ExpectedTypeTesters,
    pub accessors: ExpectedTypeAccessors,
}

pub struct ExpectedTypeConstructors {
    pub exact: FuncDecl,
    pub any_meta_type: FuncDecl,
}

pub struct ExpectedTypeTesters {
    pub exact: FuncDecl,
    pub any_meta_type: FuncDecl,
}

pub struct ExpectedTypeAccessors {
    pub exact_type: FuncDecl,
}

impl ArgonValueSort {
    #[must_use]
    pub fn new() -> Self {
        let value = Sort::uninterpreted("ArgonValue".into());
        let value_seq = Sort::seq(&value);
        let record_sort = Sort::uninterpreted("ArgonRecord".into());
        let enum_sort = Sort::uninterpreted("ArgonEnum".into());
        let enum_variant_sort = Sort::uninterpreted("ArgonEnumVariant".into());
        let instance_sort = Sort::uninterpreted("ArgonInstance".into());

        let value_constructors = ArgonValueConstructors {
            bool_literal: FuncDecl::new("argon_value_bool_literal", &[&Sort::bool()], &value),
            int_literal: FuncDecl::new("argon_value_int_literal", &[&Sort::int()], &value),
            i8_literal: FuncDecl::new("argon_value_i8_literal", &[&Sort::bitvector(8)], &value),
            u8_literal: FuncDecl::new("argon_value_u8_literal", &[&Sort::bitvector(8)], &value),
            string_literal: FuncDecl::new("argon_value_string_literal", &[&Sort::string()], &value),
            record_literal: FuncDecl::new(
                "argon_value_record_literal",
                &[&record_sort, &value_seq],
                &value,
            ),
            enum_variant_literal: FuncDecl::new(
                "argon_value_enum_variant_literal",
                &[&enum_sort, &enum_variant_sort, &value_seq, &value_seq],
                &value,
            ),
            new_instance: FuncDecl::new(
                "argon_value_new_instance",
                &[&instance_sort, &value_seq],
                &value,
            ),
            tuple: FuncDecl::new("argon_value_tuple", &[&value_seq], &value),
            r#type: FuncDecl::new("argon_value_type", &[&value], &value),
            big_type: FuncDecl::new("argon_value_big_type", &[&Sort::int()], &value),
            boxed: FuncDecl::new("argon_value_boxed", &[&value, &value], &value),
        };

        let value_testers = ArgonValueTesters {
            bool_literal: tester("argon_value_is_bool_literal", &value),
            int_literal: tester("argon_value_is_int_literal", &value),
            i8_literal: tester("argon_value_is_i8_literal", &value),
            u8_literal: tester("argon_value_is_u8_literal", &value),
            string_literal: tester("argon_value_is_string_literal", &value),
            record_literal: tester("argon_value_is_record_literal", &value),
            enum_variant_literal: tester("argon_value_is_enum_variant_literal", &value),
            new_instance: tester("argon_value_is_new_instance", &value),
            tuple: tester("argon_value_is_tuple", &value),
            r#type: tester("argon_value_is_type", &value),
            big_type: tester("argon_value_is_big_type", &value),
            boxed: tester("argon_value_is_boxed", &value),
        };

        let value_accessors = ArgonValueAccessors {
            bool_literal_value: accessor("argon_value_bool_literal_value", &value, &Sort::bool()),
            int_literal_value: accessor("argon_value_int_literal_value", &value, &Sort::int()),
            i8_literal_value: accessor("argon_value_i8_literal_value", &value, &Sort::bitvector(8)),
            u8_literal_value: accessor("argon_value_u8_literal_value", &value, &Sort::bitvector(8)),
            string_literal_value: accessor(
                "argon_value_string_literal_value",
                &value,
                &Sort::string(),
            ),
            record_literal_record: accessor(
                "argon_value_record_literal_record",
                &value,
                &record_sort,
            ),
            record_literal_fields: accessor(
                "argon_value_record_literal_fields",
                &value,
                &value_seq,
            ),
            enum_variant_literal_enum: accessor(
                "argon_value_enum_variant_literal_enum",
                &value,
                &enum_sort,
            ),
            enum_variant_literal_variant: accessor(
                "argon_value_enum_variant_literal_variant",
                &value,
                &enum_variant_sort,
            ),
            enum_variant_literal_arguments: accessor(
                "argon_value_enum_variant_literal_arguments",
                &value,
                &value_seq,
            ),
            enum_variant_literal_fields: accessor(
                "argon_value_enum_variant_literal_fields",
                &value,
                &value_seq,
            ),
            new_instance_instance: accessor(
                "argon_value_new_instance_instance",
                &value,
                &instance_sort,
            ),
            new_instance_arguments: accessor(
                "argon_value_new_instance_arguments",
                &value,
                &value_seq,
            ),
            tuple_items: accessor("argon_value_tuple_items", &value, &value_seq),
            type_level: accessor("argon_value_type_level", &value, &value),
            big_type_level: accessor("argon_value_big_type_level", &value, &Sort::int()),
            boxed_type: accessor("argon_value_boxed_type", &value, &value),
            boxed_value: accessor("argon_value_boxed_value", &value, &value),
        };

        Self {
            value,
            value_seq,
            value_constructors,
            value_testers,
            value_accessors,
            record_sort,
            enum_sort,
            enum_variant_sort,
            instance_sort,
        }
    }

    fn value_tester_terms(&self, value: &impl Ast) -> Vec<Bool> {
        [
            &self.value_testers.bool_literal,
            &self.value_testers.int_literal,
            &self.value_testers.i8_literal,
            &self.value_testers.u8_literal,
            &self.value_testers.string_literal,
            &self.value_testers.record_literal,
            &self.value_testers.enum_variant_literal,
            &self.value_testers.new_instance,
            &self.value_testers.tuple,
            &self.value_testers.r#type,
            &self.value_testers.big_type,
            &self.value_testers.boxed,
        ]
        .into_iter()
        .map(|tester| dynamic_to_bool(tester.apply(&[value])))
        .collect()
    }
}

impl ExpectedTypeSort {
    #[must_use]
    pub fn new(argon_value_sort: &Sort) -> Self {
        let expected_type = DatatypeBuilder::new(EXPECTED_TYPE)
            .variant(
                "exact",
                vec![("type", DatatypeAccessor::sort(argon_value_sort.clone()))],
            )
            .variant("any_meta_type", vec![])
            .finish();

        let DatatypeSort { sort, variants } = expected_type;
        let mut variants = variants.into_iter();
        let mut exact = next_variant(&mut variants);
        let any_meta_type = next_variant(&mut variants);

        Self {
            sort,
            constructors: ExpectedTypeConstructors {
                exact: exact.constructor,
                any_meta_type: any_meta_type.constructor,
            },
            testers: ExpectedTypeTesters {
                exact: exact.tester,
                any_meta_type: any_meta_type.tester,
            },
            accessors: ExpectedTypeAccessors {
                exact_type: next_accessor(&mut exact.accessors),
            },
        }
    }
}

fn tester(name: &str, value: &Sort) -> FuncDecl {
    FuncDecl::new(name, &[value], &Sort::bool())
}

fn accessor(name: &str, value: &Sort, result: &Sort) -> FuncDecl {
    FuncDecl::new(name, &[value], result)
}

fn concat_seq(element_sort: &Sort, values: &[Seq]) -> Seq {
    if values.is_empty() {
        return Seq::empty(element_sort);
    }

    let values = values.iter().collect::<Vec<_>>();
    Seq::concat(&values)
}

fn next_variant(variants: &mut impl Iterator<Item = DatatypeVariant>) -> DatatypeVariant {
    match variants.next() {
        Some(variant) => variant,
        None => panic!("Z3 datatype is missing an expected variant"),
    }
}

fn next_accessor(accessors: &mut Vec<FuncDecl>) -> FuncDecl {
    accessors.remove(0)
}

fn z3_int_from_big_int(value: &impl core::fmt::Display) -> Int {
    match Int::from_str(&value.to_string()) {
        Ok(value) => value,
        Err(error) => panic!("failed to create Z3 integer from Argon integer literal: {error:?}"),
    }
}

fn dynamic_to_bool(value: Dynamic) -> Bool {
    match value.as_bool() {
        Some(value) => value,
        None => panic!("Z3 predicate returned a non-bool value"),
    }
}

fn record_field_index(field: &Arc<dyn RecordField>) -> usize {
    let fields = match field.owning_record() {
        RecordFieldOwner::Record(record) => record.fields(),
        RecordFieldOwner::EnumVariant(variant) => variant.fields(),
    };
    fields
        .iter()
        .position(|candidate| candidate == field)
        .expect("record field should be present in its owner field list")
}

impl Default for ArgonValueSort {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::TestContext;
    use crate::{
        DefaultExprContext,
        test_utils::{TestEnum, TestEnumVariant},
    };
    use alloc::{sync::Arc, vec};
    use argon_expr::Builtin;
    use argon_util::UniqueIdentifier;
    use z3::{SatResult, SortKind};

    fn test_context() -> Context {
        TestContext::default().into()
    }

    fn bool_var() -> Variable<DefaultExprContext> {
        Variable::Local(Box::new(argon_expr::LocalVariable {
            id: UniqueIdentifier::new(),
            name: None,
            var_type: Expr::bool_type(),
            erasure_mode: argon_expr::ErasureMode::Concrete,
            is_witness: false,
            is_mutable: false,
        }))
    }

    fn int_expr(value: i64) -> Expr<DefaultExprContext> {
        Expr::IntLiteral(value.into())
    }

    fn string_expr(value: &str) -> Expr<DefaultExprContext> {
        Expr::StringLiteral(value.into())
    }

    fn int_expr_value(z3expr: &mut Z3Expr<DefaultExprContext>, value: i64) -> Int {
        let value = z3expr.expr_to_z3(&int_expr(value));
        z3expr.int_literal_value(&value)
    }

    fn string_expr_value(z3expr: &mut Z3Expr<DefaultExprContext>, value: &str) -> Z3String {
        let value = z3expr.expr_to_z3(&string_expr(value));
        z3expr.string_literal_value(&value)
    }

    fn bool_expr_value(z3expr: &mut Z3Expr<DefaultExprContext>, value: bool) -> Bool {
        let value = z3expr.expr_to_z3(&Expr::BoolLiteral(value));
        z3expr.bool_literal_value(&value)
    }

    fn dynamic_consts(args: &[(&str, Sort)]) -> Vec<Dynamic> {
        args.iter()
            .map(|(name, sort)| Dynamic::new_const(*name, sort))
            .collect()
    }

    fn dynamic_refs(values: &[Dynamic]) -> Vec<&dyn Ast> {
        values.iter().map(|value| value as &dyn Ast).collect()
    }

    fn assert_constructor_tester_axiom(
        constructor: &FuncDecl,
        tester: &FuncDecl,
        args: &[(&str, Sort)],
    ) {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let values = dynamic_consts(args);
        let refs = dynamic_refs(&values);
        let constructed = constructor.apply(&refs);
        let tested = dynamic_to_bool(tester.apply(&[&constructed]));

        z3expr.solver().assert(tested.not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    fn assert_constructor_accessor_axiom(
        constructor: &FuncDecl,
        accessor: &FuncDecl,
        args: &[(&str, Sort)],
        accessor_arg_index: usize,
    ) {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let values = dynamic_consts(args);
        let refs = dynamic_refs(&values);
        let constructed = constructor.apply(&refs);
        let accessed = accessor.apply(&[&constructed]);

        z3expr
            .solver()
            .assert(accessed.eq(&values[accessor_arg_index]).not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    fn assert_constructor_is_injective(
        constructor: &FuncDecl,
        args: &[(&str, Sort)],
        unequal_arg_index: usize,
    ) {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let lhs_values = dynamic_consts(args);
        let rhs_values = args
            .iter()
            .map(|(name, sort)| Dynamic::new_const(format!("{name}_rhs"), sort))
            .collect::<Vec<_>>();
        let lhs_refs = dynamic_refs(&lhs_values);
        let rhs_refs = dynamic_refs(&rhs_values);
        let lhs = constructor.apply(&lhs_refs);
        let rhs = constructor.apply(&rhs_refs);

        z3expr.solver().assert(lhs.eq(&rhs));
        z3expr.solver().assert(
            lhs_values[unequal_arg_index]
                .eq(&rhs_values[unequal_arg_index])
                .not(),
        );

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    fn assert_constructors_are_unequal(
        lhs_constructor: &FuncDecl,
        lhs_args: &[(&str, Sort)],
        rhs_constructor: &FuncDecl,
        rhs_args: &[(&str, Sort)],
    ) {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let lhs_values = dynamic_consts(lhs_args);
        let rhs_values = dynamic_consts(rhs_args);
        let lhs_refs = dynamic_refs(&lhs_values);
        let rhs_refs = dynamic_refs(&rhs_values);
        let lhs = lhs_constructor.apply(&lhs_refs);
        let rhs = rhs_constructor.apply(&rhs_refs);

        z3expr.solver().assert(lhs.eq(&rhs));

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn composite_constructors_use_seq_sorts() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();

        assert_eq!(Some(SortKind::Seq), sort.value_constructors.tuple.domain(0));
        assert_eq!(
            Some(SortKind::Seq),
            sort.value_constructors.new_instance.domain(1)
        );
        assert_eq!(
            Some(SortKind::Seq),
            sort.value_constructors.record_literal.domain(1)
        );
        assert_eq!(
            Some(SortKind::Seq),
            sort.value_constructors.enum_variant_literal.domain(2)
        );
        assert_eq!(
            Some(SortKind::Seq),
            sort.value_constructors.enum_variant_literal.domain(3)
        );
    }

    #[test]
    fn opaque_expr_conversion_reuses_cached_value() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let expr = Expr::Builtin(Builtin::IntBitAnd {
            integer_type: IntegerType::Int,
            lhs: Box::new(Expr::IntLiteral(1.into())),
            rhs: Box::new(Expr::IntLiteral(2.into())),
        });

        let first = z3expr.expr_to_z3(&expr);
        let second = z3expr.expr_to_z3(&expr);

        assert_eq!(first, second);
        assert_eq!(1, z3expr.opaque_values().len());
        assert_eq!(Some(&first), z3expr.opaque_value_for_expr(&expr));
    }

    #[test]
    fn int_builtin_operations_use_z3_int_operations() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        let neg = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntNegate {
            integer_type: IntegerType::Int,
            value: Box::new(int_expr(4)),
        }));
        let expected_neg = int_expr_value(&mut z3expr, 4).unary_minus();
        assert_eq!(z3expr.wrap_int_literal(&expected_neg), neg);

        let add = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntAdd {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(1)),
            rhs: Box::new(int_expr(2)),
        }));
        let lhs = int_expr_value(&mut z3expr, 1);
        let rhs = int_expr_value(&mut z3expr, 2);
        let expected_add = Int::add(&[lhs, rhs]);
        assert_eq!(z3expr.wrap_int_literal(&expected_add), add);

        let sub = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntSub {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(5)),
            rhs: Box::new(int_expr(3)),
        }));
        let lhs = int_expr_value(&mut z3expr, 5);
        let rhs = int_expr_value(&mut z3expr, 3);
        let expected_sub = Int::sub(&[lhs, rhs]);
        assert_eq!(z3expr.wrap_int_literal(&expected_sub), sub);

        let mul = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntMul {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(6)),
            rhs: Box::new(int_expr(7)),
        }));
        let lhs = int_expr_value(&mut z3expr, 6);
        let rhs = int_expr_value(&mut z3expr, 7);
        let expected_mul = Int::mul(&[lhs, rhs]);
        assert_eq!(z3expr.wrap_int_literal(&expected_mul), mul);
    }

    #[test]
    fn int_comparison_builtins_use_z3_int_comparisons() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        let eq = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntEq {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(1)),
            rhs: Box::new(int_expr(1)),
        }));
        let lhs = int_expr_value(&mut z3expr, 1);
        let rhs = int_expr_value(&mut z3expr, 1);
        let expected_eq = lhs.eq(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_eq), eq);

        let lt = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntLt {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(1)),
            rhs: Box::new(int_expr(2)),
        }));
        let lhs = int_expr_value(&mut z3expr, 1);
        let rhs = int_expr_value(&mut z3expr, 2);
        let expected_lt = lhs.lt(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_lt), lt);

        let le = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntLe {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(1)),
            rhs: Box::new(int_expr(2)),
        }));
        let lhs = int_expr_value(&mut z3expr, 1);
        let rhs = int_expr_value(&mut z3expr, 2);
        let expected_le = lhs.le(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_le), le);

        let gt = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntGt {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(2)),
            rhs: Box::new(int_expr(1)),
        }));
        let lhs = int_expr_value(&mut z3expr, 2);
        let rhs = int_expr_value(&mut z3expr, 1);
        let expected_gt = lhs.gt(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_gt), gt);

        let ge = z3expr.expr_to_z3(&Expr::Builtin(Builtin::IntGe {
            integer_type: IntegerType::Int,
            lhs: Box::new(int_expr(2)),
            rhs: Box::new(int_expr(1)),
        }));
        let lhs = int_expr_value(&mut z3expr, 2);
        let rhs = int_expr_value(&mut z3expr, 1);
        let expected_ge = lhs.ge(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_ge), ge);
    }

    #[test]
    fn string_and_bool_builtins_use_z3_operations() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        let concat = z3expr.expr_to_z3(&Expr::Builtin(Builtin::StringConcat {
            values: vec![string_expr("ab"), string_expr("cd"), string_expr("ef")],
        }));
        let lhs = string_expr_value(&mut z3expr, "ab");
        let mid = string_expr_value(&mut z3expr, "cd");
        let rhs = string_expr_value(&mut z3expr, "ef");
        let expected_concat = Z3String::concat(&[lhs, mid, rhs]);
        assert_eq!(z3expr.wrap_string_literal(&expected_concat), concat);

        let string_eq = z3expr.expr_to_z3(&Expr::Builtin(Builtin::StringEq {
            lhs: Box::new(string_expr("a")),
            rhs: Box::new(string_expr("a")),
        }));
        let lhs = string_expr_value(&mut z3expr, "a");
        let rhs = string_expr_value(&mut z3expr, "a");
        let expected_string_eq = lhs.eq(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_string_eq), string_eq);

        let bool_eq = z3expr.expr_to_z3(&Expr::Builtin(Builtin::BoolEq {
            lhs: Box::new(Expr::BoolLiteral(true)),
            rhs: Box::new(Expr::BoolLiteral(false)),
        }));
        let lhs = bool_expr_value(&mut z3expr, true);
        let rhs = bool_expr_value(&mut z3expr, false);
        let expected_bool_eq = lhs.eq(&rhs);
        assert_eq!(z3expr.wrap_bool_literal(&expected_bool_eq), bool_eq);
    }

    #[test]
    fn argon_value_constructor_testers_are_disjoint() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let value = Dynamic::new_const("value", &z3expr.argon_value_sort().value);
        let is_bool = dynamic_to_bool(
            z3expr
                .argon_value_sort()
                .value_testers
                .bool_literal
                .apply(&[&value]),
        );
        let is_int = dynamic_to_bool(
            z3expr
                .argon_value_sort()
                .value_testers
                .int_literal
                .apply(&[&value]),
        );

        z3expr.solver().assert(is_bool);
        z3expr.solver().assert(is_int);

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn enum_assertion_requires_exactly_one_declared_variant() {
        let variant_a = Arc::new(TestEnumVariant::new("A")) as Arc<dyn EnumVariant>;
        let variant_b = Arc::new(TestEnumVariant::new("B")) as Arc<dyn EnumVariant>;
        let enum_ = Arc::new(TestEnum::new(vec![variant_a.clone(), variant_b.clone()]));
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        let enum_term = z3expr.enum_term(&(enum_.clone() as Arc<dyn Enum>));
        let variant_a_term = z3expr.enum_variant_term(&variant_a);
        let variant_b_term = z3expr.enum_variant_term(&variant_b);
        z3expr
            .solver()
            .assert(z3expr.is_variant_of_enum(&enum_term, &variant_a_term).not());
        z3expr
            .solver()
            .assert(z3expr.is_variant_of_enum(&enum_term, &variant_b_term).not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn empty_enum_assertion_makes_membership_false() {
        let enum_ = Arc::new(TestEnum::new(vec![]));
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        let enum_term = z3expr.enum_term(&(enum_ as Arc<dyn Enum>));
        let variant =
            Dynamic::new_const("some_variant", &z3expr.argon_value_sort().enum_variant_sort);
        z3expr
            .solver()
            .assert(z3expr.is_variant_of_enum(&enum_term, &variant));

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn boolean_logical_operators_use_z3_bool_operations() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let a = bool_var();
        let b = bool_var();
        let a_expr = Expr::Variable(a);
        let b_expr = Expr::Variable(b);

        let a_value = z3expr.expr_to_z3(&a_expr);
        let b_value = z3expr.expr_to_z3(&b_expr);
        let a_bool = z3expr.bool_literal_value(&a_value);
        let b_bool = z3expr.bool_literal_value(&b_value);

        let expected_and_bool = Bool::and(&[&a_bool, &b_bool]);
        let expected_and = z3expr.construct_value(
            &z3expr.argon_value_sort.value_constructors.bool_literal,
            &[&expected_and_bool],
        );
        let actual_and = z3expr.expr_to_z3(&Expr::And(
            Box::new(a_expr.clone()),
            Box::new(b_expr.clone()),
        ));
        assert_eq!(expected_and, actual_and);

        let expected_or_bool = Bool::or(&[&a_bool, &b_bool]);
        let expected_or = z3expr.construct_value(
            &z3expr.argon_value_sort.value_constructors.bool_literal,
            &[&expected_or_bool],
        );
        let actual_or = z3expr.expr_to_z3(&Expr::Or(
            Box::new(a_expr.clone()),
            Box::new(b_expr.clone()),
        ));
        assert_eq!(expected_or, actual_or);

        let expected_not_bool = a_bool.not();
        let expected_not = z3expr.construct_value(
            &z3expr.argon_value_sort.value_constructors.bool_literal,
            &[&expected_not_bool],
        );
        let actual_not = z3expr.expr_to_z3(&Expr::Not(Box::new(a_expr)));
        assert_eq!(expected_not, actual_not);
    }

    #[test]
    fn constructors_imply_their_tester() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();

        assert_constructor_tester_axiom(
            &sort.value_constructors.bool_literal,
            &sort.value_testers.bool_literal,
            &[("value", Sort::bool())],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.int_literal,
            &sort.value_testers.int_literal,
            &[("value", Sort::int())],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.string_literal,
            &sort.value_testers.string_literal,
            &[("value", Sort::string())],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.record_literal,
            &sort.value_testers.record_literal,
            &[
                ("record", sort.record_sort.clone()),
                ("fields", sort.value_seq.clone()),
            ],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.enum_variant_literal,
            &sort.value_testers.enum_variant_literal,
            &[
                ("enum", sort.enum_sort.clone()),
                ("variant", sort.enum_variant_sort.clone()),
                ("arguments", sort.value_seq.clone()),
                ("fields", sort.value_seq.clone()),
            ],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.new_instance,
            &sort.value_testers.new_instance,
            &[
                ("instance", sort.instance_sort.clone()),
                ("arguments", sort.value_seq.clone()),
            ],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.tuple,
            &sort.value_testers.tuple,
            &[("items", sort.value_seq.clone())],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.r#type,
            &sort.value_testers.r#type,
            &[("level", sort.value.clone())],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.big_type,
            &sort.value_testers.big_type,
            &[("level", Sort::int())],
        );
        assert_constructor_tester_axiom(
            &sort.value_constructors.boxed,
            &sort.value_testers.boxed,
            &[("type", sort.value.clone()), ("value", sort.value.clone())],
        );
    }

    #[test]
    fn constructor_accessors_return_constructor_arguments() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();

        assert_constructor_accessor_axiom(
            &sort.value_constructors.bool_literal,
            &sort.value_accessors.bool_literal_value,
            &[("value", Sort::bool())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.int_literal,
            &sort.value_accessors.int_literal_value,
            &[("value", Sort::int())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.string_literal,
            &sort.value_accessors.string_literal_value,
            &[("value", Sort::string())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.record_literal,
            &sort.value_accessors.record_literal_record,
            &[
                ("record", sort.record_sort.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.record_literal,
            &sort.value_accessors.record_literal_fields,
            &[
                ("record", sort.record_sort.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            1,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.enum_variant_literal,
            &sort.value_accessors.enum_variant_literal_enum,
            &[
                ("enum", sort.enum_sort.clone()),
                ("variant", sort.enum_variant_sort.clone()),
                ("arguments", sort.value_seq.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.enum_variant_literal,
            &sort.value_accessors.enum_variant_literal_variant,
            &[
                ("enum", sort.enum_sort.clone()),
                ("variant", sort.enum_variant_sort.clone()),
                ("arguments", sort.value_seq.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            1,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.enum_variant_literal,
            &sort.value_accessors.enum_variant_literal_arguments,
            &[
                ("enum", sort.enum_sort.clone()),
                ("variant", sort.enum_variant_sort.clone()),
                ("arguments", sort.value_seq.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            2,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.enum_variant_literal,
            &sort.value_accessors.enum_variant_literal_fields,
            &[
                ("enum", sort.enum_sort.clone()),
                ("variant", sort.enum_variant_sort.clone()),
                ("arguments", sort.value_seq.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            3,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.new_instance,
            &sort.value_accessors.new_instance_instance,
            &[
                ("instance", sort.instance_sort.clone()),
                ("arguments", sort.value_seq.clone()),
            ],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.new_instance,
            &sort.value_accessors.new_instance_arguments,
            &[
                ("instance", sort.instance_sort.clone()),
                ("arguments", sort.value_seq.clone()),
            ],
            1,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.tuple,
            &sort.value_accessors.tuple_items,
            &[("items", sort.value_seq.clone())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.r#type,
            &sort.value_accessors.type_level,
            &[("level", sort.value.clone())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.big_type,
            &sort.value_accessors.big_type_level,
            &[("level", Sort::int())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.boxed,
            &sort.value_accessors.boxed_type,
            &[("type", sort.value.clone()), ("value", sort.value.clone())],
            0,
        );
        assert_constructor_accessor_axiom(
            &sort.value_constructors.boxed,
            &sort.value_accessors.boxed_value,
            &[("type", sort.value.clone()), ("value", sort.value.clone())],
            1,
        );
    }

    #[test]
    fn tester_reconstructs_int_literal_from_accessor() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();
        let value = Dynamic::new_const("value", &sort.value);
        let zero = sort
            .value_constructors
            .int_literal
            .apply(&[&Int::from_i64(0)]);
        let is_int = dynamic_to_bool(sort.value_testers.int_literal.apply(&[&value]));
        let value_int = z3expr.int_literal_value(&value);
        let zero_int = z3expr.int_literal_value(&zero);

        z3expr.solver().assert(is_int);
        z3expr.solver().assert(value_int.eq(&zero_int));
        z3expr.solver().assert(value.eq(&zero).not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn same_constructor_equality_requires_equal_arguments() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();

        assert_constructor_is_injective(
            &sort.value_constructors.int_literal,
            &[("value", Sort::int())],
            0,
        );
        assert_constructor_is_injective(
            &sort.value_constructors.boxed,
            &[("type", sort.value.clone()), ("value", sort.value.clone())],
            1,
        );
    }

    #[test]
    fn different_constructor_values_are_not_equal() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();

        assert_constructors_are_unequal(
            &sort.value_constructors.bool_literal,
            &[("value", Sort::bool())],
            &sort.value_constructors.int_literal,
            &[("value", Sort::int())],
        );
        assert_constructors_are_unequal(
            &sort.value_constructors.record_literal,
            &[
                ("record", sort.record_sort.clone()),
                ("fields", sort.value_seq.clone()),
            ],
            &sort.value_constructors.enum_variant_literal,
            &[
                ("enum", sort.enum_sort.clone()),
                ("variant", sort.enum_variant_sort.clone()),
                ("arguments", sort.value_seq.clone()),
                ("fields", sort.value_seq.clone()),
            ],
        );
    }

    #[test]
    fn record_literal_equality_compares_field_sequence() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();
        let record = Dynamic::new_const("record", &sort.record_sort);
        let lhs_fields = Seq::unit(
            &sort
                .value_constructors
                .int_literal
                .apply(&[&Int::from_i64(1)]),
        );
        let rhs_fields = Seq::unit(
            &sort
                .value_constructors
                .int_literal
                .apply(&[&Int::from_i64(2)]),
        );
        let lhs = sort
            .value_constructors
            .record_literal
            .apply(&[&record, &lhs_fields]);
        let rhs = sort
            .value_constructors
            .record_literal
            .apply(&[&record, &rhs_fields]);

        z3expr.solver().assert(lhs.eq(&rhs));
        z3expr.solver().assert(lhs_fields.eq(&rhs_fields).not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn enum_variant_literal_equality_compares_field_sequence() {
        let z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let sort = z3expr.argon_value_sort();
        let enum_ = Dynamic::new_const("enum", &sort.enum_sort);
        let variant = Dynamic::new_const("variant", &sort.enum_variant_sort);
        let arguments = Seq::empty(&sort.value);
        let lhs_fields = Seq::unit(
            &sort
                .value_constructors
                .int_literal
                .apply(&[&Int::from_i64(1)]),
        );
        let rhs_fields = Seq::unit(
            &sort
                .value_constructors
                .int_literal
                .apply(&[&Int::from_i64(2)]),
        );
        let lhs = sort.value_constructors.enum_variant_literal.apply(&[
            &enum_,
            &variant,
            &arguments,
            &lhs_fields,
        ]);
        let rhs = sort.value_constructors.enum_variant_literal.apply(&[
            &enum_,
            &variant,
            &arguments,
            &rhs_fields,
        ]);

        z3expr.solver().assert(lhs.eq(&rhs));
        z3expr.solver().assert(lhs_fields.eq(&rhs_fields).not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn variable_conversion_reuses_cached_value() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let expr = Expr::Variable(bool_var());

        let first = z3expr.expr_to_z3(&expr);
        let second = z3expr.expr_to_z3(&expr);

        assert_eq!(first, second);
    }

    #[test]
    fn inhabited_uses_predicate_for_opaque_types() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let expr = Expr::Builtin(Builtin::IntType {
            integer_type: IntegerType::Int,
        });
        let value = z3expr.expr_to_z3(&expr);

        let expected = dynamic_to_bool(z3expr.inhabited_func().apply(&[&value]));
        let actual = z3expr.inhabited(&expr);

        assert_eq!(expected, actual);
    }

    #[test]
    fn inhabited_reduces_structural_type_rules() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let lhs = Expr::Builtin(Builtin::IntType {
            integer_type: IntegerType::Int,
        });
        let rhs = Expr::Builtin(Builtin::BoolType);
        let lhs_inhabited = z3expr.inhabited(&lhs);
        let rhs_inhabited = z3expr.inhabited(&rhs);

        let conjunction = z3expr.inhabited(&Expr::ConjunctionType {
            lhs: Box::new(lhs.clone()),
            rhs: Box::new(rhs.clone()),
        });
        assert_eq!(Bool::and(&[&lhs_inhabited, &rhs_inhabited]), conjunction);

        let disjunction = z3expr.inhabited(&Expr::DisjunctionType {
            lhs: Box::new(lhs.clone()),
            rhs: Box::new(rhs.clone()),
        });
        assert_eq!(Bool::or(&[&lhs_inhabited, &rhs_inhabited]), disjunction);

        let function_type = z3expr.inhabited(&Expr::FunctionType {
            a: Box::new(argon_expr::ClosureParameterVariable {
                id: UniqueIdentifier::new(),
                var_type: lhs.clone(),
                name: None,
                is_mutable: false,
                erasure_mode: argon_expr::ErasureMode::Erased,
                is_witness: false,
            }),
            r: Box::new(rhs.clone()),
        });
        assert_eq!(lhs_inhabited.implies(&rhs_inhabited), function_type);
    }

    #[test]
    fn equal_to_type_is_inhabited_when_operands_are_equal() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let expr = Expr::EqualToType {
            r#type: Box::new(Expr::int_type()),
            lhs: Box::new(int_expr(1)),
            rhs: Box::new(int_expr(1)),
        };
        let inhabited = z3expr.inhabited(&expr);

        z3expr.solver().assert(inhabited.not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }
}
