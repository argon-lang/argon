#![allow(dead_code, reason = "the IR type checker is not integrated yet")]

// Implementation plan:
// 1. Complete the bidirectional checking foundation: checking expressions and types,
//    creating and solving holes, normalization, unification, and resolution of partial
//    inference results.
// 2. Implement the remaining expressions in dependency order:
//    a. Finish the leaf expressions first: error recovery, `Type`, builtin types and
//       values, identifiers, variables, tuple-element loads, and function-result values.
//       These establish lookup, register reads, and the representation of type values.
//    b. Add type-forming and expected-type-driven expressions: function types, boxed
//       types, `as`, and closure literals. Resume partially inferred tuples and
//       interpolated strings once checking can push an expected type into their holes.
//    c. Add builtins and operator desugaring, then function-object calls. Next port
//       overload collection, applicability checks, generic argument substitution,
//       implicit arguments, and dispatch for functions, methods, static methods,
//       constructors, enum variants, and indexing.
//    d. Add aggregate construction and projection: record literals, record field loads
//       and stores, tuple projection, and enum-variant construction. Update the model
//       whenever an aggregate's compile-time value becomes known or is invalidated.
//    e. Add ownership-oriented wrappers and places: `use`, `shared`, box, unbox, mutable
//       assignment, borrowing, sharing, and erased aliases. Preserve place identity in
//       the emitted IR so the later ownership pass can diagnose moves and borrows.
//    f. Add blocks and local statements before branching expressions, including local
//       declarations, assignment, labels, assertions, return-position handling, and
//       finally regions. Then implement `if`/`else`, short-circuit operators, and model
//       joins, followed by loops, `while`, break, next, redo, retry, return, and raise.
//    g. Implement `is` and patterns before `match`; use them to add branch-local type
//       refinements, validate match arms, merge arm result types and model states, and
//       run exhaustiveness checking.
//    h. Finish the features that depend on the full resolver and control-flow model:
//       `summon`, ensures witnesses, and trait-object construction.
// 3. For every expression stage, emit declaration and operation instructions with
//    correct locations, erasure modes, program-point edges, known-value updates, and
//    recovery registers after diagnostics; add focused tests before moving to the next
//    stage.
// 4. Add pattern checking, branch refinements, implicit resolution, ensures clauses, and
//    exhaustiveness validation as called out by the expression stages above.
// 5. Port ownership, erasure, and purity validation to IR regions while preserving the
//    legacy checker's diagnostics and error-recovery behavior.
// 6. Add parity tests against the legacy checker, migrate source consumers to this
//    implementation, remove the temporary dead-code allowance, and retire the legacy
//    checker once parity is established.

use alloc::boxed::Box;
use alloc::format;
use alloc::sync::Arc;
use alloc::vec::Vec;
use argon_compiler::access::AccessToken;
use argon_compiler::{Context, DefaultExprContext, EffectInfo};
use argon_expr::{ErasureMode, ExprContext, IntegerType};
use argon_ir::{
    BuiltinType, ExpressionOwner, Instruction, InstructionNode, IrContextShifter, Pattern,
    ProgramPoint, Region, Register, RegisterDeclaration, RegisterOrigin, Value,
    default_shift_expression_owner, default_shift_region,
};
use argon_parser::ast;
use argon_parser::ast::FunctionLiteral;
use argon_parser::ast::StringFragment;
use argon_util::{CompileError, UniqueIdentifier};
use core::hash::{Hash, Hasher};
use core::mem;
use parse18_runtime::Location;
use parse18_runtime::WithLocation;

mod model;
pub mod scope;
mod shifter;

use model::Model;
use scope::{LocalScope, LocalVariableScope, Scope, ShiftedScope};
use shifter::{DefaultToTypeCheckContextShifter, TypeCheckToDefaultContextShifter};

pub fn type_check_type_expr(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
) -> Region<DefaultExprContext> {
    let _ = (context, options, e);
    todo!()
}

pub fn type_check_expr(
    context: Context,
    options: TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
    expected_type: &Region<DefaultExprContext>,
) -> Region<DefaultExprContext> {
    let TypeCheckOptions {
        access,
        scope,
        erasure_mode,
        effect_info,
        ensures_clauses,
    } = options;
    let shifted_scope = ShiftedScope::new(scope);
    let mut local_scope = LocalVariableScope::new(shifted_scope);
    let mut model = Model::new();
    let ensures_clauses = ensures_clauses
        .unwrap_or_default()
        .iter()
        .cloned()
        .map(default_to_type_check_region)
        .collect();

    let start_label = ProgramPoint::new();
    model.add_entry_point(start_label.clone());

    let mut checker = TypeChecker {
        context: context.clone(),
        access,
        scope: &mut local_scope,
        model: &mut model,
        erasure_check_mode: erasure_mode,
        ensures_clauses,
        return_position: true,
        enable_ownership: false,

        current_label: start_label.clone(),
        instructions: Vec::new(),
    };

    let expected_type = default_to_type_check_region(expected_type.clone());
    let result = checker.check(e, &expected_type);

    let body = Region {
        entry: start_label,
        instructions: mem::take(&mut checker.instructions),
        result,
    };

    check_ownership(&mut checker, &body);
    drop(checker);

    let result = TypeCheckToDefaultContextShifter {
        context: context.clone(),
        model,
    }
    .shift_region(body);

    check_erasure(context.clone(), &options, e, &result);
    check_purity(context, effect_info, e, &result);

    result
}

#[derive(Debug, Clone)]
struct TypeCheckExprContext;

impl ExprContext for TypeCheckExprContext {
    type Hole = Hole;
    type Function = <DefaultExprContext as ExprContext>::Function;
    type Method = <DefaultExprContext as ExprContext>::Method;
    type StaticMethod = <DefaultExprContext as ExprContext>::StaticMethod;
    type Record = <DefaultExprContext as ExprContext>::Record;
    type RecordField = <DefaultExprContext as ExprContext>::RecordField;
    type Enum = <DefaultExprContext as ExprContext>::Enum;
    type EnumVariant = <DefaultExprContext as ExprContext>::EnumVariant;
    type Trait = <DefaultExprContext as ExprContext>::Trait;
    type Instance = <DefaultExprContext as ExprContext>::Instance;
}

#[derive(Debug)]
struct HoleInfo {
    id: UniqueIdentifier,
    location: Location,
    hole_type: Region<TypeCheckExprContext>,
}

#[derive(Clone)]
struct Hole(Arc<HoleInfo>);

impl core::fmt::Debug for Hole {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("Hole").field(&self.0.id).finish()
    }
}

impl PartialEq for Hole {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Eq for Hole {}

impl Hash for Hole {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Arc::as_ptr(&self.0).hash(state);
    }
}

struct TypeChecker<'access, 'scope, 'model> {
    context: Context,
    access: &'access AccessToken,
    scope: &'scope mut dyn LocalScope<ExprContext = TypeCheckExprContext>,
    model: &'model mut Model,
    erasure_check_mode: ErasureMode,
    ensures_clauses: Vec<Region<TypeCheckExprContext>>,
    return_position: bool,
    enable_ownership: bool,

    current_label: ProgramPoint,

    instructions: Vec<InstructionNode<TypeCheckExprContext>>,
}

impl<'access, 'scope, 'model> TypeChecker<'access, 'scope, 'model> {
    fn check<'e>(
        &mut self,
        expr: &'e WithLocation<ast::Expr>,
        expected_type: &Region<TypeCheckExprContext>,
    ) -> Register {
        let _ = (expr, expected_type);
        todo!()
    }

    fn infer<'e>(&mut self, expr: &'e WithLocation<ast::Expr>) -> TypeInferResult<'e> {
        match &expr.value {
            ast::Expr::BoolLiteral(value) => TypeInferResult::Complete(self.emit_temporary(
                &expr.location,
                Value::bool_type(),
                Value::BoolLiteral(*value),
                |destination| Instruction::BoolLiteral {
                    destination,
                    value: *value,
                },
            )),

            ast::Expr::IntLiteral { value, suffix } => TypeInferResult::Complete(
                self.infer_integer_literal(&expr.location, value.clone(), suffix),
            ),

            ast::Expr::StringLiteral(value) => {
                self.infer_string_literal(&expr.location, &value.parts)
            }

            ast::Expr::BigType(level) => TypeInferResult::Complete(self.emit_temporary(
                &expr.location,
                Value::BigType(level + 1),
                Value::BigType(level.clone()),
                |destination| Instruction::BigType {
                    destination,
                    level: level.clone(),
                },
            )),

            ast::Expr::Paren(inner) => self.infer(inner),

            ast::Expr::FunctionLiteral(function_literal) => TypeInferResult::Closure {
                location: &expr.location,
                function_literal,
            },

            ast::Expr::Tuple { items } => self.infer_tuple(&expr.location, items),

            ast::Expr::UnaryOperation { op, a } => {
                if let ast::UnaryOperator::Minus = op.value
                    && let ast::Expr::IntLiteral { value, suffix } = &a.value
                {
                    return TypeInferResult::Complete(self.infer_integer_literal(
                        &expr.location,
                        -value.clone(),
                        suffix,
                    ));
                }

                todo!()
            }

            _ => {
                todo!()
            }
        }
    }

    fn declare_temporary(
        &mut self,
        location: &Location,
        r#type: Value<TypeCheckExprContext>,
    ) -> Register {
        let register = Register::new();
        let exit = ProgramPoint::new();
        self.model
            .fork_program_point(&self.current_label, exit.clone());
        self.instructions.push(InstructionNode {
            location: location.clone(),
            erasure_mode: ErasureMode::Concrete,
            exit: exit.clone(),
            instruction: Instruction::DeclareRegister {
                declaration: RegisterDeclaration {
                    register: register.clone(),
                    r#type,
                    name: None,
                    is_mutable: false,
                    erasure_mode: ErasureMode::Erased,
                    is_witness: false,
                    origin: RegisterOrigin::Temporary,
                },
            },
        });
        self.current_label = exit;
        register
    }

    fn emit_temporary(
        &mut self,
        location: &Location,
        inferred_type: Value<TypeCheckExprContext>,
        value: Value<TypeCheckExprContext>,
        make_instruction: impl FnOnce(Register) -> Instruction<TypeCheckExprContext>,
    ) -> InferredType {
        let register = self.declare_temporary(location, inferred_type.clone());
        let exit = ProgramPoint::new();
        self.model
            .fork_program_point(&self.current_label, exit.clone());
        self.model
            .set_register_value(&exit, register.clone(), value);
        self.instructions.push(InstructionNode {
            location: location.clone(),
            erasure_mode: ErasureMode::Concrete,
            exit: exit.clone(),
            instruction: make_instruction(register.clone()),
        });
        self.current_label = exit;

        InferredType {
            inferred_type,
            checked_expr: register,
        }
    }

    fn emit_unknown_temporary(
        &mut self,
        location: &Location,
        inferred_type: Value<TypeCheckExprContext>,
        make_instruction: impl FnOnce(Register) -> Instruction<TypeCheckExprContext>,
    ) -> InferredType {
        let register = self.declare_temporary(location, inferred_type.clone());
        let exit = ProgramPoint::new();
        self.model
            .fork_program_point(&self.current_label, exit.clone());
        self.instructions.push(InstructionNode {
            location: location.clone(),
            erasure_mode: ErasureMode::Concrete,
            exit: exit.clone(),
            instruction: make_instruction(register.clone()),
        });
        self.current_label = exit;

        InferredType {
            inferred_type,
            checked_expr: register,
        }
    }

    fn infer_string_literal<'e>(
        &mut self,
        location: &'e Location,
        string_parts: &'e [StringFragment],
    ) -> TypeInferResult<'e> {
        if let [part] = string_parts
            && let StringFragment::Text(text) = part
        {
            let value: Box<str> = text.as_str().into();
            return TypeInferResult::Complete(self.emit_temporary(
                location,
                Value::BuiltinType(BuiltinType::String),
                Value::StringLiteral(value.clone()),
                |destination| Instruction::StringLiteral { destination, value },
            ));
        }

        let string_type = Value::BuiltinType(BuiltinType::String);
        let mut parts = Vec::new();
        let mut registers = Vec::new();
        for (index, part) in string_parts.iter().enumerate() {
            let result = match part {
                StringFragment::Text(text) => {
                    let value: Box<str> = text.as_str().into();
                    TypeInferResult::Complete(self.emit_temporary(
                        location,
                        string_type.clone(),
                        Value::StringLiteral(value.clone()),
                        |destination| Instruction::StringLiteral { destination, value },
                    ))
                }
                StringFragment::Interpolate { value } => self.infer(value),
            };

            match result {
                TypeInferResult::Complete(inferred) => {
                    if inferred.inferred_type != string_type {
                        self.context
                            .reporter()
                            .report_error(CompileError::type_mismatch(
                                location.clone(),
                                format!("{:?}", string_type),
                                format!("{:?}", inferred.inferred_type),
                            ));
                    }
                    registers.push(inferred.checked_expr.clone());
                    parts.push(TypeInferResult::Complete(inferred));
                }
                partial => {
                    parts.push(partial);
                    return TypeInferResult::StringLiteral {
                        location,
                        parts,
                        remaining_parts: string_parts[index + 1..].iter().collect(),
                    };
                }
            }
        }

        TypeInferResult::Complete(self.emit_unknown_temporary(
            location,
            string_type,
            |destination| Instruction::Builtin {
                destination,
                builtin: argon_ir::Builtin::StringConcat { values: registers },
            },
        ))
    }

    fn infer_integer_literal(
        &mut self,
        location: &Location,
        value: num_bigint::BigInt,
        suffix: &ast::IntLiteralSuffix,
    ) -> InferredType {
        macro_rules! fixed_width_integer {
            ($rust_type:ty, $integer_type:expr, $value_variant:ident, $instruction:ident, $name:literal, $range:literal) => {{
                let inferred_type = Value::BuiltinType(BuiltinType::Int($integer_type));
                match <$rust_type>::try_from(&value) {
                    Ok(literal) => self.emit_temporary(
                        location,
                        inferred_type,
                        Value::$value_variant(literal),
                        |destination| Instruction::$instruction {
                            destination,
                            value: literal,
                        },
                    ),
                    Err(_) => {
                        self.context.reporter().report_error(
                            CompileError::integer_literal_out_of_range(
                                location.clone(),
                                $name,
                                $range,
                                &format!("{}", value),
                            ),
                        );
                        InferredType {
                            checked_expr: self.declare_temporary(location, inferred_type.clone()),
                            inferred_type,
                        }
                    }
                }
            }};
        }

        match suffix {
            ast::IntLiteralSuffix::None => self.emit_temporary(
                location,
                Value::BuiltinType(BuiltinType::Int(IntegerType::Int)),
                Value::IntLiteral(value.clone()),
                |destination| Instruction::IntLiteral { destination, value },
            ),
            ast::IntLiteralSuffix::Signed(8) => {
                fixed_width_integer!(i8, IntegerType::I8, I8Literal, I8Literal, "i8", "-128-127")
            }
            ast::IntLiteralSuffix::Unsigned(8) => {
                fixed_width_integer!(u8, IntegerType::U8, U8Literal, U8Literal, "u8", "0-255")
            }
            ast::IntLiteralSuffix::Signed(16) => fixed_width_integer!(
                i16,
                IntegerType::I16,
                I16Literal,
                I16Literal,
                "i16",
                "-32768-32767"
            ),
            ast::IntLiteralSuffix::Unsigned(16) => fixed_width_integer!(
                u16,
                IntegerType::U16,
                U16Literal,
                U16Literal,
                "u16",
                "0-65535"
            ),
            ast::IntLiteralSuffix::Signed(32) => fixed_width_integer!(
                i32,
                IntegerType::I32,
                I32Literal,
                I32Literal,
                "i32",
                "-2147483648-2147483647"
            ),
            ast::IntLiteralSuffix::Unsigned(32) => fixed_width_integer!(
                u32,
                IntegerType::U32,
                U32Literal,
                U32Literal,
                "u32",
                "0-4294967295"
            ),
            ast::IntLiteralSuffix::Signed(64) => fixed_width_integer!(
                i64,
                IntegerType::I64,
                I64Literal,
                I64Literal,
                "i64",
                "-9223372036854775808-9223372036854775807"
            ),
            ast::IntLiteralSuffix::Unsigned(64) => fixed_width_integer!(
                u64,
                IntegerType::U64,
                U64Literal,
                U64Literal,
                "u64",
                "0-18446744073709551615"
            ),
            ast::IntLiteralSuffix::Signed(bits) | ast::IntLiteralSuffix::Unsigned(bits) => {
                let prefix = if matches!(suffix, ast::IntLiteralSuffix::Signed(_)) {
                    "i"
                } else {
                    "u"
                };
                self.context
                    .reporter()
                    .report_error(CompileError::unknown_integer_suffix(
                        location.clone(),
                        format!("{}{}", prefix, bits),
                    ));
                let inferred_type = Value::BuiltinType(BuiltinType::Int(IntegerType::Int));
                InferredType {
                    checked_expr: self.declare_temporary(location, inferred_type.clone()),
                    inferred_type,
                }
            }
        }
    }

    fn infer_tuple<'e>(
        &mut self,
        location: &'e Location,
        items: &'e [WithLocation<ast::Expr>],
    ) -> TypeInferResult<'e> {
        let mut inferred = Vec::new();
        for (index, item) in items.iter().enumerate() {
            match self.infer(item) {
                TypeInferResult::Complete(element) => inferred.push(element),
                partial => {
                    let mut elements = inferred
                        .into_iter()
                        .map(TypeInferResult::Complete)
                        .collect::<Vec<_>>();
                    elements.push(partial);
                    return TypeInferResult::Tuple {
                        location,
                        elements,
                        remaining_elements: items[index + 1..].iter().collect(),
                    };
                }
            }
        }

        let inferred_type = Value::Tuple(
            inferred
                .iter()
                .map(|element| element.inferred_type.clone())
                .collect(),
        );
        let registers = inferred
            .iter()
            .map(|element| element.checked_expr.clone())
            .collect::<Vec<_>>();
        let value = Value::Tuple(
            registers
                .iter()
                .map(|register| {
                    self.model
                        .get_register_value(&self.current_label, register)
                        .cloned()
                        .unwrap_or_else(|| Value::Register(register.clone()))
                })
                .collect(),
        );

        TypeInferResult::Complete(self.emit_temporary(
            location,
            inferred_type,
            value,
            |destination| Instruction::Tuple {
                destination,
                elements: registers,
            },
        ))
    }
}

fn check_ownership(checker: &mut TypeChecker<'_, '_, '_>, body: &Region<TypeCheckExprContext>) {
    let _ = (checker, body);
    todo!()
}

fn check_erasure(
    context: Context,
    options: &TypeCheckOptions<'_>,
    e: &WithLocation<ast::Expr>,
    result: &Region<DefaultExprContext>,
) {
    let _ = (context, options, e, result);
    todo!()
}

fn check_purity(
    context: Context,
    effect_info: EffectInfo,
    e: &WithLocation<ast::Expr>,
    result: &Region<DefaultExprContext>,
) {
    let _ = (context, effect_info, e, result);
    todo!()
}

fn default_to_type_check_owner(
    owner: ExpressionOwner<DefaultExprContext>,
) -> ExpressionOwner<TypeCheckExprContext> {
    default_shift_expression_owner(&mut DefaultToTypeCheckContextShifter, owner)
}

fn default_to_type_check_region(
    region: Region<DefaultExprContext>,
) -> Region<TypeCheckExprContext> {
    default_shift_region(&mut DefaultToTypeCheckContextShifter, region)
}

pub struct TypeCheckOptions<'a> {
    access: &'a AccessToken,
    scope: &'a dyn Scope<ExprContext = DefaultExprContext>,
    erasure_mode: ErasureMode,
    effect_info: EffectInfo,
    ensures_clauses: Option<&'a [Region<DefaultExprContext>]>,
}

impl<'a> TypeCheckOptions<'a> {
    pub fn new(
        access: &'a AccessToken,
        scope: &'a dyn Scope<ExprContext = DefaultExprContext>,
        erasure_mode: ErasureMode,
    ) -> Self {
        Self {
            access,
            scope,
            erasure_mode,
            effect_info: EffectInfo::Effectful,
            ensures_clauses: None,
        }
    }

    pub fn with_effect_info(mut self, effect_info: EffectInfo) -> Self {
        self.effect_info = effect_info;
        self
    }

    pub fn with_ensures_clauses(
        mut self,
        ensures_clauses: &'a [Region<DefaultExprContext>],
    ) -> Self {
        self.ensures_clauses = Some(ensures_clauses);
        self
    }
}

#[derive(Debug, Clone)]
enum TypeInferResult<'e> {
    // Fully inferred type
    Complete(InferredType),

    // Types that need additional type information
    Closure {
        location: &'e Location,
        function_literal: &'e FunctionLiteral,
    },

    // The remaining cases are compound expressions that may have subexpressions that require additional type information
    Tuple {
        location: &'e Location,
        elements: Vec<TypeInferResult<'e>>,
        remaining_elements: Vec<&'e WithLocation<ast::Expr>>,
    },

    StringLiteral {
        location: &'e Location,
        parts: Vec<TypeInferResult<'e>>,
        remaining_parts: Vec<&'e StringFragment>,
    },

    Finally {
        location: &'e Location,
        body: Box<InferResultBlock<'e>>,
        finally_region: Region<TypeCheckExprContext>,
    },

    IfElse {
        location: &'e Location,
        condition: Box<TypeInferResult<'e>>,
        then_block: Box<InferResultBlock<'e>>,
        else_block: Box<InferResultBlock<'e>>,
    },

    Match {
        location: &'e Location,
        expression: Register,
        arms: Vec<InferResultMatchArm<'e>>,
    },
}

#[derive(Debug, Clone)]
struct InferResultBlock<'e> {
    region: Region<TypeCheckExprContext>,
    result: TypeInferResult<'e>,
}

#[derive(Debug, Clone)]
struct InferResultMatchArm<'e> {
    location: &'e Location,
    pattern: Pattern<TypeCheckExprContext>,
    body: InferResultBlock<'e>,
}

#[derive(Debug, Clone)]
struct InferredType {
    inferred_type: Value<TypeCheckExprContext>,
    checked_expr: Register,
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::string::ToString;
    use alloc::vec;
    use argon_compiler::test_utils::{TestContext, TestReporter};
    use argon_compiler::{DefaultExprContext, ModulePath, Tube, TubeName};
    use hashbrown::HashMap;
    use mitsein::vec1;
    use num_bigint::BigInt;
    use parse18_runtime::FilePosition;

    fn location() -> Location {
        Location {
            file: "test".into(),
            start: FilePosition { line: 0, column: 0 },
            end: FilePosition { line: 0, column: 0 },
        }
    }

    fn located(value: ast::Expr) -> WithLocation<ast::Expr> {
        WithLocation {
            location: location(),
            value,
        }
    }

    struct TestScope;

    impl scope::Scope for TestScope {
        type ExprContext = DefaultExprContext;

        fn lookup(&self, _: &ast::Identifier, _: &AccessToken) -> scope::Lookup<Self::ExprContext> {
            scope::Lookup::Empty
        }

        fn lookup_assign(
            &self,
            _: &ast::Identifier,
            _: &AccessToken,
        ) -> scope::Lookup<Self::ExprContext> {
            scope::Lookup::Empty
        }

        fn lookup_tube(&self, _: &TubeName) -> Option<Arc<Tube>> {
            None
        }

        fn given_assertions(
            &self,
            _: &mut dyn scope::ImplicitGivens<ExprContext = Self::ExprContext>,
        ) {
        }

        fn lookup_block_label(
            &self,
            _: &ast::Identifier,
        ) -> Option<scope::BlockLabelDeclaration<Self::ExprContext>> {
            None
        }

        fn latest_loop_labels(&self) -> Option<scope::LoopLabels<Self::ExprContext>> {
            None
        }

        fn latest_block_label(&self) -> Option<scope::BlockLabel<Self::ExprContext>> {
            None
        }

        fn function_result_value_type(&self) -> Option<Region<Self::ExprContext>> {
            None
        }

        fn known_variable_values(
            &self,
        ) -> HashMap<scope::Variable<Self::ExprContext>, Region<Self::ExprContext>> {
            HashMap::new()
        }
    }

    fn with_checker<R>(
        reporter: TestReporter,
        f: impl FnOnce(&mut TypeChecker<'_, '_, '_>) -> R,
    ) -> R {
        let context: Context = TestContext::new(reporter).into();
        let access = AccessToken::new(
            TubeName(vec1!["test".to_string()]),
            ModulePath(vec!["test".to_string()]),
        );
        let scope = TestScope;
        let shifted_scope = ShiftedScope::new(&scope);
        let mut local_scope = LocalVariableScope::new(shifted_scope);
        let mut model = Model::new();
        let entry = ProgramPoint::new();
        model.add_entry_point(entry.clone());
        let mut checker = TypeChecker {
            context,
            access: &access,
            scope: &mut local_scope,
            model: &mut model,
            erasure_check_mode: ErasureMode::Concrete,
            ensures_clauses: Vec::new(),
            return_position: false,
            enable_ownership: false,
            current_label: entry,
            instructions: Vec::new(),
        };
        f(&mut checker)
    }

    fn complete(result: TypeInferResult<'_>) -> InferredType {
        match result {
            TypeInferResult::Complete(inferred) => inferred,
            _ => panic!("expected complete inference"),
        }
    }

    #[test]
    fn infers_simple_literals_and_parentheses() {
        with_checker(TestReporter::default(), |checker| {
            let bool_result = complete(checker.infer(&located(ast::Expr::Paren(Box::new(
                located(ast::Expr::BoolLiteral(true)),
            )))));
            assert_eq!(bool_result.inferred_type, Value::bool_type());
            assert_eq!(
                checker
                    .model
                    .get_register_value(&checker.current_label, &bool_result.checked_expr),
                Some(&Value::BoolLiteral(true))
            );

            let int_result = complete(checker.infer(&located(ast::Expr::IntLiteral {
                value: BigInt::from(42),
                suffix: ast::IntLiteralSuffix::Signed(16),
            })));
            assert_eq!(
                int_result.inferred_type,
                Value::BuiltinType(BuiltinType::Int(IntegerType::I16))
            );
            assert!(matches!(
                checker.instructions.last().map(|node| &node.instruction),
                Some(Instruction::I16Literal { value: 42, .. })
            ));

            let string_result = complete(checker.infer(&located(ast::Expr::StringLiteral(
                ast::StringLiteral {
                    parts: vec![StringFragment::Text("hello world".to_string())],
                },
            ))));
            assert_eq!(
                string_result.inferred_type,
                Value::BuiltinType(BuiltinType::String)
            );
            assert!(matches!(
                checker.instructions.last().map(|node| &node.instruction),
                Some(Instruction::StringLiteral { value, .. }) if &**value == "hello world"
            ));

            let big_type_result =
                complete(checker.infer(&located(ast::Expr::BigType(BigInt::from(2)))));
            assert_eq!(
                big_type_result.inferred_type,
                Value::BigType(BigInt::from(3))
            );
            assert!(matches!(
                checker.instructions.last().map(|node| &node.instruction),
                Some(Instruction::BigType { level, .. }) if level == &BigInt::from(2)
            ));
        });
    }

    #[test]
    fn folds_negative_integer_literals() {
        with_checker(TestReporter::default(), |checker| {
            let result = complete(checker.infer(&located(ast::Expr::UnaryOperation {
                op: WithLocation {
                    location: location(),
                    value: ast::UnaryOperator::Minus,
                },
                a: Box::new(located(ast::Expr::IntLiteral {
                    value: BigInt::from(7),
                    suffix: ast::IntLiteralSuffix::None,
                })),
            })));

            assert!(matches!(
                checker.instructions.last().map(|node| &node.instruction),
                Some(Instruction::IntLiteral { value, .. }) if value == &BigInt::from(-7)
            ));
            assert_eq!(
                checker
                    .model
                    .get_register_value(&checker.current_label, &result.checked_expr),
                Some(&Value::IntLiteral(BigInt::from(-7)))
            );
        });
    }

    #[test]
    fn infers_interpolated_strings_as_string_concat() {
        with_checker(TestReporter::default(), |checker| {
            let result = complete(checker.infer(&located(ast::Expr::StringLiteral(
                ast::StringLiteral {
                    parts: vec![
                        StringFragment::Text("hello ".to_string()),
                        StringFragment::Interpolate {
                            value: Box::new(located(ast::Expr::StringLiteral(
                                ast::StringLiteral {
                                    parts: vec![StringFragment::Text("world".to_string())],
                                },
                            ))),
                        },
                    ],
                },
            ))));

            assert_eq!(
                result.inferred_type,
                Value::BuiltinType(BuiltinType::String)
            );
            assert!(matches!(
                checker.instructions.last().map(|node| &node.instruction),
                Some(Instruction::Builtin {
                    destination,
                    builtin: argon_ir::Builtin::StringConcat { values },
                }) if destination == &result.checked_expr && values.len() == 2
            ));
            assert_eq!(
                checker
                    .model
                    .get_register_value(&checker.current_label, &result.checked_expr),
                None
            );
        });
    }

    #[test]
    fn interpolated_string_preserves_partial_inference() {
        with_checker(TestReporter::default(), |checker| {
            let string = located(ast::Expr::StringLiteral(ast::StringLiteral {
                parts: vec![
                    StringFragment::Interpolate {
                        value: Box::new(located(ast::Expr::FunctionLiteral(
                            ast::FunctionLiteral {
                                parameter_name: None,
                                body: Box::new(located(ast::Expr::BoolLiteral(true))),
                            },
                        ))),
                    },
                    StringFragment::Text("tail".to_string()),
                ],
            }));

            let TypeInferResult::StringLiteral {
                parts,
                remaining_parts,
                ..
            } = checker.infer(&string)
            else {
                panic!("expected partial string inference")
            };
            assert!(matches!(
                parts.as_slice(),
                [TypeInferResult::Closure { .. }]
            ));
            assert_eq!(remaining_parts.len(), 1);
            assert!(checker.instructions.is_empty());
        });
    }

    #[test]
    fn reports_out_of_range_integer_and_leaves_recovery_register_uninitialized() {
        let reporter = TestReporter::default();
        with_checker(reporter.clone(), |checker| {
            let result = complete(checker.infer(&located(ast::Expr::IntLiteral {
                value: BigInt::from(256),
                suffix: ast::IntLiteralSuffix::Unsigned(8),
            })));

            assert_eq!(checker.instructions.len(), 1);
            assert!(matches!(
                checker.instructions[0].instruction,
                Instruction::DeclareRegister { .. }
            ));
            assert_eq!(
                checker
                    .model
                    .get_register_value(&checker.current_label, &result.checked_expr),
                None
            );
        });
        assert_eq!(reporter.errors().len(), 1);
    }

    #[test]
    fn infers_complete_tuple_and_records_its_value() {
        with_checker(TestReporter::default(), |checker| {
            let result = complete(checker.infer(&located(ast::Expr::Tuple {
                items: vec![
                    located(ast::Expr::BoolLiteral(true)),
                    located(ast::Expr::IntLiteral {
                        value: BigInt::from(5),
                        suffix: ast::IntLiteralSuffix::None,
                    }),
                ],
            })));

            assert_eq!(
                result.inferred_type,
                Value::Tuple(vec![
                    Value::bool_type(),
                    Value::BuiltinType(BuiltinType::Int(IntegerType::Int)),
                ])
            );
            assert!(matches!(
                checker.instructions.last().map(|node| &node.instruction),
                Some(Instruction::Tuple { elements, .. }) if elements.len() == 2
            ));
            assert_eq!(
                checker
                    .model
                    .get_register_value(&checker.current_label, &result.checked_expr),
                Some(&Value::Tuple(vec![
                    Value::BoolLiteral(true),
                    Value::IntLiteral(BigInt::from(5)),
                ]))
            );
        });
    }

    #[test]
    fn tuple_stops_after_first_partially_inferred_element() {
        with_checker(TestReporter::default(), |checker| {
            let function = ast::FunctionLiteral {
                parameter_name: None,
                body: Box::new(located(ast::Expr::BoolLiteral(true))),
            };
            let tuple = located(ast::Expr::Tuple {
                items: vec![
                    located(ast::Expr::FunctionLiteral(function)),
                    located(ast::Expr::BoolLiteral(false)),
                ],
            });

            let TypeInferResult::Tuple {
                elements,
                remaining_elements,
                ..
            } = checker.infer(&tuple)
            else {
                panic!("expected partial tuple inference")
            };
            assert!(matches!(
                elements.as_slice(),
                [TypeInferResult::Closure { .. }]
            ));
            assert_eq!(remaining_elements.len(), 1);
            assert!(checker.instructions.is_empty());
        });
    }
}
