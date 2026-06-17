use crate::{Context, Enum, EnumVariant, RecordField};
use alloc::format;
use alloc::string::ToString;
use alloc::sync::Arc;
use alloc::vec::Vec;
use argon_expr::{Expr, ExprContext, RecordFieldLiteral, Variable};
use core::str::FromStr;
use hashbrown::{HashMap, HashSet};
use z3::ast::{self, Ast, Bool, Dynamic, Int, Seq, String as Z3String};
use z3::{DatatypeAccessor, DatatypeBuilder, DatatypeSort, DatatypeVariant, FuncDecl, Params, Solver, Sort};

const EXPECTED_TYPE: &str = "ExpectedType";

pub struct Z3Expr<
    EC: ExprContext<Enum = Arc<dyn Enum>, EnumVariant = Arc<dyn EnumVariant>> + ?Sized,
> {
    solver: Solver,
    argon_value_sort: ArgonValueSort,
    expected_type_sort: ExpectedTypeSort,
    is_variant_of: FuncDecl,
    field_value: FuncDecl,
    opaque_values: HashMap<Expr<EC>, Dynamic>,
    opaque_expr_ids: HashMap<Expr<EC>, usize>,
    asserted_enums: HashSet<Arc<dyn Enum>>,
    enum_ids: HashMap<Pointer<dyn Enum>, usize>,
    enum_variant_ids: HashMap<Pointer<dyn EnumVariant>, usize>,
    record_field_ids: HashMap<EC::RecordField, usize>,
    instance_ids: HashMap<EC::Instance, usize>,
    record_ids: HashMap<argon_expr::RecordType<EC>, usize>,
    variable_ids: HashMap<Variable<EC>, usize>,
}

struct Pointer<T: ?Sized>(*const T);

impl<T: ?Sized> Pointer<T> {
    fn new(ptr: &Arc<T>) -> Self {
        Self(Arc::as_ptr(ptr))
    }
}

impl<T: ?Sized> core::hash::Hash for Pointer<T> {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T: ?Sized> PartialEq for Pointer<T> {
    fn eq(&self, other: &Self) -> bool {
        core::ptr::eq(self.0, other.0)
    }
}

impl<T: ?Sized> Eq for Pointer<T> {}

impl<EC: ExprContext<Enum = Arc<dyn Enum>, EnumVariant = Arc<dyn EnumVariant>, RecordField = Arc<dyn RecordField>> + ?Sized>
    Z3Expr<EC>
{
    #[must_use]
    pub fn new(context: Context) -> Self {
        let argon_value_sort = ArgonValueSort::new();
        let expected_type_sort = ExpectedTypeSort::new(&argon_value_sort.value);
        let is_variant_of = FuncDecl::new(
            "is_variant_of",
            &[
                &argon_value_sort.enum_sort,
                &argon_value_sort.enum_variant_sort,
            ],
            &Sort::bool(),
        );
        let field_value = FuncDecl::new(
            "argon_field_value",
            &[&argon_value_sort.value, &argon_value_sort.record_field_sort],
            &argon_value_sort.value,
        );

        let result = Self {
            solver: Solver::new(),
            argon_value_sort,
            expected_type_sort,
            is_variant_of,
            field_value,
            opaque_values: HashMap::new(),
            opaque_expr_ids: HashMap::new(),
            asserted_enums: HashSet::new(),
            enum_ids: HashMap::new(),
            enum_variant_ids: HashMap::new(),
            record_field_ids: HashMap::new(),
            instance_ids: HashMap::new(),
            record_ids: HashMap::new(),
            variable_ids: HashMap::new(),
        };
        let mut params = Params::new();
        params.set_u32("rlimit", context.z3_rlimit());
        result.solver.set_params(&params);
        result.assert_every_variant_has_exactly_one_enum();
        result.assert_argon_value_constructor_disjointness();
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
    pub fn is_variant_of_func(&self) -> &FuncDecl {
        &self.is_variant_of
    }

    #[must_use]
    pub fn field_value_func(&self) -> &FuncDecl {
        &self.field_value
    }

    #[must_use]
    pub fn is_variant_of(&self, enum_: &impl Ast, variant: &impl Ast) -> Bool {
        dynamic_to_bool(self.is_variant_of.apply(&[enum_, variant]))
    }

    pub fn field_value(&self, object: &impl Ast, field: &impl Ast) -> Dynamic {
        self.field_value.apply(&[object, field])
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
        let field_term = self.record_field_term(field);
        self.field_value(value, &field_term)
    }

    fn enum_id(&mut self, enum_: &Arc<dyn Enum>) -> usize {
        let ptr = Pointer::new(enum_);
        if let Some(&id) = self.enum_ids.get(&ptr) {
            id
        } else {
            let id = self.enum_ids.len();
            self.enum_ids.insert(ptr, id);
            id
        }
    }

    fn enum_variant_id(&mut self, variant: &Arc<dyn EnumVariant>) -> usize {
        let ptr = Pointer::new(variant);
        if let Some(&id) = self.enum_variant_ids.get(&ptr) {
            id
        } else {
            let id = self.enum_variant_ids.len();
            self.enum_variant_ids.insert(ptr, id);
            id
        }
    }

    fn enum_variant_id_count(&self) -> usize {
        self.enum_variant_ids.len()
    }

    fn record_field_id(&mut self, field: &EC::RecordField) -> usize {
        if let Some(&id) = self.record_field_ids.get(field) {
            id
        } else {
            let id = self.record_field_ids.len();
            self.record_field_ids.insert(field.clone(), id);
            id
        }
    }

    fn instance_id(&mut self, instance: &EC::Instance) -> usize {
        if let Some(&id) = self.instance_ids.get(instance) {
            id
        } else {
            let id = self.instance_ids.len();
            self.instance_ids.insert(instance.clone(), id);
            id
        }
    }

    fn record_id(&mut self, record: &argon_expr::RecordType<EC>) -> usize {
        if let Some(&id) = self.record_ids.get(record) {
            id
        } else {
            let id = self.record_ids.len();
            self.record_ids.insert(record.clone(), id);
            id
        }
    }

    fn variable_id(&mut self, variable: &Variable<EC>) -> usize {
        if let Some(&id) = self.variable_ids.get(variable) {
            id
        } else {
            let id = self.variable_ids.len();
            self.variable_ids.insert(variable.clone(), id);
            id
        }
    }

    pub fn enum_term(&mut self, enum_: &Arc<dyn Enum>) -> Dynamic {
        let sort = self.argon_value_sort.enum_sort.clone();
        let id = self.enum_id(enum_);
        Dynamic::new_const(format!("argon_enum_{id}"), &sort)
    }

    pub fn enum_variant_term(&mut self, variant: &Arc<dyn EnumVariant>) -> Dynamic {
        let sort = self.argon_value_sort.enum_variant_sort.clone();
        let id = self.enum_variant_id(variant);
        Dynamic::new_const(format!("argon_enum_variant_{id}"), &sort)
    }

    pub fn record_field_term(&mut self, field: &EC::RecordField) -> Dynamic {
        let sort = self.argon_value_sort.record_field_sort.clone();
        let id = self.record_field_id(field);
        Dynamic::new_const(format!("argon_record_field_{id}"), &sort)
    }

    fn opaque_expr_term(&mut self, prefix: &str, expr: &Expr<EC>, sort: &Sort) -> Dynamic {
        let id = self.opaque_expr_id(expr);
        Dynamic::new_const(format!("argon_{prefix}_{id}"), sort)
    }

    pub fn assert_exactly_one_variant_of_enum(&mut self, enum_: Arc<dyn Enum>) {
        if !self.asserted_enums.insert(enum_.clone()) {
            return;
        }

        let enum_term = self.enum_term(&enum_);
        let variants = enum_.variants();

        if variants.is_empty() {
            let variant_id = self.enum_variant_id_count();
            let variant = Dynamic::new_const(
                format!("argon_enum_variant_forall_{variant_id}"),
                &self.argon_value_sort.enum_variant_sort,
            );
            let body = self.is_variant_of(&enum_term, &variant).not();
            self.solver
                .assert(ast::forall_const(&[&variant], &[], &body));
            return;
        }

        let variant_memberships = variants
            .iter()
            .map(|variant| {
                let variant_term = self.enum_variant_term(variant);
                self.is_variant_of(&enum_term, &variant_term)
            })
            .collect::<Vec<_>>();
        let weighted_memberships = variant_memberships
            .iter()
            .map(|membership| (membership, 1))
            .collect::<Vec<_>>();

        self.solver.assert(Bool::pb_eq(&weighted_memberships, 1));
    }

    fn assert_every_variant_has_exactly_one_enum(&self) {
        let variant = Dynamic::new_const(
            "argon_variant_exactly_one_enum_variant",
            &self.argon_value_sort.enum_variant_sort,
        );
        let enum_ = Dynamic::new_const(
            "argon_variant_exactly_one_enum_enum",
            &self.argon_value_sort.enum_sort,
        );
        let exists_owner = ast::exists_const(&[&enum_], &[], &self.is_variant_of(&enum_, &variant));
        self.solver
            .assert(ast::forall_const(&[&variant], &[], &exists_owner));

        let variant = Dynamic::new_const(
            "argon_variant_unique_enum_variant",
            &self.argon_value_sort.enum_variant_sort,
        );
        let enum_a = Dynamic::new_const(
            "argon_variant_unique_enum_a",
            &self.argon_value_sort.enum_sort,
        );
        let enum_b = Dynamic::new_const(
            "argon_variant_unique_enum_b",
            &self.argon_value_sort.enum_sort,
        );
        let enum_a_matches = self.is_variant_of(&enum_a, &variant);
        let enum_b_matches = self.is_variant_of(&enum_b, &variant);
        let both_match = Bool::and(&[&enum_a_matches, &enum_b_matches]);
        let same_enum = enum_a.eq(&enum_b);
        self.solver.assert(ast::forall_const(
            &[&variant, &enum_a, &enum_b],
            &[],
            &both_match.implies(&same_enum),
        ));
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
            Expr::Error => {
                Some(self.construct_value(&self.argon_value_sort.value_constructors.error, &[]))
            }
            Expr::BoolLiteral(value) => {
                let value = Bool::from_bool(*value);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.bool_literal,
                    &[&value],
                ))
            }
            Expr::IntLiteral(value) => {
                let value = z3_int_from_big_int(value);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.int_literal,
                    &[&value],
                ))
            }
            Expr::StringLiteral(value) => match Z3String::from_str(value) {
                Ok(value) => Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.string_literal,
                    &[&value],
                )),
                Err(_) => None,
            },
            Expr::Closure { .. } => {
                let sort = self.argon_value_sort.closure_sort.clone();
                let closure = self.opaque_expr_term("closure", expr, &sort);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.closure,
                    &[&closure],
                ))
            }
            Expr::EnumVariantLiteral {
                enum_type,
                variant,
                arguments,
                fields,
            } => {
                let enum_ = self.enum_term(&enum_type.enum_);
                let variant = self.enum_variant_term(variant);
                let arguments = self.value_seq(arguments);
                let value = self.construct_value(
                    &self
                        .argon_value_sort
                        .value_constructors
                        .enum_variant_literal,
                    &[&enum_, &variant, &arguments],
                );
                self.assert_field_values(&value, fields);
                Some(value)
            }
            Expr::NewInstance {
                instance,
                arguments,
            } => {
                let id = self.instance_id(instance);
                let sort = self.argon_value_sort.instance_sort.clone();
                let instance_term = Dynamic::new_const(format!("argon_instance_{id}"), &sort);
                let arguments = self.value_seq(arguments);
                Some(self.construct_value(
                    &self.argon_value_sort.value_constructors.new_instance,
                    &[&instance_term, &arguments],
                ))
            }
            Expr::RecordLiteral {
                record_type,
                fields,
            } => {
                let id = self.record_id(record_type);
                let sort = self.argon_value_sort.record_sort.clone();
                let record_term = Dynamic::new_const(format!("argon_record_{id}"), &sort);
                let value = self.construct_value(
                    &self.argon_value_sort.value_constructors.record_literal,
                    &[&record_term],
                );
                self.assert_field_values(&value, fields);
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
            Expr::Variable(variable) => {
                let id = self.variable_id(variable);
                Some(Dynamic::new_const(
                    format!("argon_var_{id}"),
                    &self.argon_value_sort.value,
                ))
            }
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

    fn assert_field_values(&mut self, object: &Dynamic, fields: &[RecordFieldLiteral<EC>]) {
        for field in fields {
            let field_name = self.record_field_term(&field.field);
            let actual_value = self.field_value(object, &field_name);
            let expected_value = self.expr_to_z3(&field.value);
            self.solver.assert(actual_value.eq(expected_value));
        }
    }

    fn opaque_expr_value(&mut self, expr: &Expr<EC>) -> Dynamic {
        if let Some(value) = self.opaque_values.get(expr) {
            return value.clone();
        }

        let id = self.opaque_expr_id(expr);
        let value = Dynamic::new_const(
            format!("argon_opaque_{id}"),
            &self.argon_value_sort.value,
        );
        self.opaque_values.insert(expr.clone(), value.clone());
        value
    }

    fn opaque_expr_id(&mut self, expr: &Expr<EC>) -> usize {
        if let Some(&id) = self.opaque_expr_ids.get(expr) {
            id
        } else {
            let id = self.opaque_expr_ids.len();
            self.opaque_expr_ids.insert(expr.clone(), id);
            id
        }
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
    pub closure_sort: Sort,
    pub record_sort: Sort,
    pub enum_sort: Sort,
    pub enum_variant_sort: Sort,
    pub instance_sort: Sort,
    pub record_field_sort: Sort,
}

pub struct ArgonValueConstructors {
    pub error: FuncDecl,
    pub bool_literal: FuncDecl,
    pub int_literal: FuncDecl,
    pub string_literal: FuncDecl,
    pub closure: FuncDecl,
    pub record_literal: FuncDecl,
    pub enum_variant_literal: FuncDecl,
    pub new_instance: FuncDecl,
    pub tuple: FuncDecl,
    pub r#type: FuncDecl,
    pub big_type: FuncDecl,
    pub boxed: FuncDecl,
}

pub struct ArgonValueTesters {
    pub error: FuncDecl,
    pub bool_literal: FuncDecl,
    pub int_literal: FuncDecl,
    pub string_literal: FuncDecl,
    pub closure: FuncDecl,
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
    pub string_literal_value: FuncDecl,
    pub closure_closure: FuncDecl,
    pub record_literal_record: FuncDecl,
    pub enum_variant_literal_enum: FuncDecl,
    pub enum_variant_literal_variant: FuncDecl,
    pub enum_variant_literal_arguments: FuncDecl,
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
        let closure_sort = Sort::uninterpreted("ArgonClosure".into());
        let record_sort = Sort::uninterpreted("ArgonRecord".into());
        let enum_sort = Sort::uninterpreted("ArgonEnum".into());
        let enum_variant_sort = Sort::uninterpreted("ArgonEnumVariant".into());
        let instance_sort = Sort::uninterpreted("ArgonInstance".into());
        let record_field_sort = Sort::uninterpreted("ArgonRecordField".into());
        let field_array_sort = Sort::array(&record_field_sort, &value);


        let value_constructors = ArgonValueConstructors {
            error: FuncDecl::new("argon_value_error", &[], &value),
            bool_literal: FuncDecl::new("argon_value_bool_literal", &[&Sort::bool()], &value),
            int_literal: FuncDecl::new("argon_value_int_literal", &[&Sort::int()], &value),
            string_literal: FuncDecl::new("argon_value_string_literal", &[&Sort::string()], &value),
            closure: FuncDecl::new("argon_value_closure", &[&closure_sort], &value),
            record_literal: FuncDecl::new("argon_value_record_literal", &[&record_sort, &field_array_sort], &value),
            enum_variant_literal: FuncDecl::new(
                "argon_value_enum_variant_literal",
                &[&enum_sort, &enum_variant_sort, &value_seq, &field_array_sort],
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
            error: tester("argon_value_is_error", &value),
            bool_literal: tester("argon_value_is_bool_literal", &value),
            int_literal: tester("argon_value_is_int_literal", &value),
            string_literal: tester("argon_value_is_string_literal", &value),
            closure: tester("argon_value_is_closure", &value),
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
            string_literal_value: accessor(
                "argon_value_string_literal_value",
                &value,
                &Sort::string(),
            ),
            closure_closure: accessor("argon_value_closure_closure", &value, &closure_sort),
            record_literal_record: accessor(
                "argon_value_record_literal_record",
                &value,
                &record_sort,
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
            closure_sort,
            record_sort,
            enum_sort,
            enum_variant_sort,
            instance_sort,
            record_field_sort,
        }
    }

    fn value_tester_terms(&self, value: &impl Ast) -> Vec<Bool> {
        [
            &self.value_testers.error,
            &self.value_testers.bool_literal,
            &self.value_testers.int_literal,
            &self.value_testers.string_literal,
            &self.value_testers.closure,
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

impl Default for ArgonValueSort {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        DefaultExprContext,
        test_utils::{TestEnum, TestEnumVariant},
    };
    use argon_util::UniqueIdentifier;
    use alloc::{sync::Arc, vec};
    use argon_expr::Builtin;
    use z3::{SatResult, SortKind};
    use crate::test_utils::TestContext;

    fn test_context() -> Context {
        TestContext::default().into()
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
            Some(SortKind::Array),
            sort.value_constructors.record_literal.domain(1)
        );
        assert_eq!(
            Some(SortKind::Seq),
            sort.value_constructors.enum_variant_literal.domain(2)
        );
        assert_eq!(
            Some(SortKind::Array),
            sort.value_constructors.enum_variant_literal.domain(3)
        );
    }

    #[test]
    fn opaque_expr_conversion_reuses_cached_value() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let expr = Expr::Builtin {
            builtin: Builtin::IntAdd,
            arguments: vec![],
        };

        let first = z3expr.expr_to_z3(&expr);
        let second = z3expr.expr_to_z3(&expr);

        assert_eq!(first, second);
        assert_eq!(1, z3expr.opaque_values().len());
        assert_eq!(Some(&first), z3expr.opaque_value_for_expr(&expr));
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
        let enum_dyn = enum_.clone() as Arc<dyn Enum>;
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        z3expr.assert_exactly_one_variant_of_enum(enum_dyn);

        let enum_term = z3expr.enum_term(&(enum_.clone() as Arc<dyn Enum>));
        let variant_a_term = z3expr.enum_variant_term(&variant_a);
        let variant_b_term = z3expr.enum_variant_term(&variant_b);
        z3expr
            .solver()
            .assert(z3expr.is_variant_of(&enum_term, &variant_a_term).not());
        z3expr
            .solver()
            .assert(z3expr.is_variant_of(&enum_term, &variant_b_term).not());

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn empty_enum_assertion_makes_membership_false() {
        let enum_ = Arc::new(TestEnum::new(vec![]));
        let enum_dyn = enum_.clone() as Arc<dyn Enum>;
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());

        z3expr.assert_exactly_one_variant_of_enum(enum_dyn);

        let enum_term = z3expr.enum_term(&(enum_ as Arc<dyn Enum>));
        let variant =
            Dynamic::new_const("some_variant", &z3expr.argon_value_sort().enum_variant_sort);
        z3expr
            .solver()
            .assert(z3expr.is_variant_of(&enum_term, &variant));

        assert_eq!(SatResult::Unsat, z3expr.solver().check());
    }

    #[test]
    fn variable_conversion_reuses_cached_value() {
        let mut z3expr = Z3Expr::<DefaultExprContext>::new(test_context());
        let var = Variable::Local(Box::new(argon_expr::LocalVariable {
            id: UniqueIdentifier::new(),
            name: None,
            var_type: Expr::bool_type(),
            erasure_mode: argon_expr::ErasureMode::Concrete,
            is_witness: false,
            is_mutable: false,
        }));
        let expr = Expr::Variable(var);

        let first = z3expr.expr_to_z3(&expr);
        let second = z3expr.expr_to_z3(&expr);

        assert_eq!(first, second);
    }
}
