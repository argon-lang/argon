use crate::vm::{AnyRegister, EmitResult, ExprEmitter, TokenEmitterCommon};
use alloc::boxed::Box;
use alloc::vec;
use alloc::vec::Vec;
use argon_compiler::DefaultExprContext;
use argon_compiler::expr_type::get_pattern_type;
use argon_expr::{Expr, Pattern};
use argon_format::vm as vf;
use num_bigint::BigUint;

pub(super) fn emit_pattern(
    emitter: &mut ExprEmitter<'_>,
    when_false_label: &vf::BlockId,
    value_reg: vf::RegisterId,
    pattern: &Pattern<DefaultExprContext>,
) -> EmitResult<()> {
    Ok(match pattern {
        Pattern::Error => {
            todo!("emit error pattern")
        }
        Pattern::Discard { .. } => {}
        Pattern::Tuple(elements) => {
            for (i, element) in elements.iter().enumerate() {
                let element_type = emitter.token_expr(&get_pattern_type(element))?;
                let element_reg = emitter.add_var(element_type);

                emitter.emit(vf::Instruction::TupleElement {
                    dest: Box::new(element_reg.clone()),
                    element_index: BigUint::from(i),
                    src: Box::new(value_reg.clone()),
                });

                emit_pattern(emitter, when_false_label, element_reg, element)?;
            }
        }
        Pattern::Binding(_, _) => {
            todo!()
        }
        Pattern::EnumVariant {
            enum_type,
            variant,
            args,
            fields,
        } => {
            let arg_patterns = args
                .iter()
                .map(|arg| -> EmitResult<_> {
                    let t = emitter.token_expr(&get_pattern_type(arg))?;
                    let arg_reg = emitter.add_var(t);
                    Ok((arg_reg, arg))
                })
                .collect::<Result<Vec<_>, _>>()?;

            let field_patterns = fields
                .iter()
                .map(|field| -> EmitResult<_> {
                    let t = emitter.token_expr(&get_pattern_type(&field.pattern))?;
                    let field_reg = emitter.add_var(t);
                    Ok((field_reg, field))
                })
                .collect::<Result<Vec<_>, _>>()?;

            let enum_type_token = emitter.token_expr(&Expr::EnumType(enum_type.clone()))?;
            let variant_id = BigUint::from(emitter.encoder.get_enum_variant_id(variant.clone()));
            let field_extractors = field_patterns
                .iter()
                .map(|(field_reg, field)| {
                    Box::new(vf::FieldExtractor {
                        r: Box::new(field_reg.clone()),
                        field_id: BigUint::from(
                            emitter.encoder.get_record_field_id(field.field.clone()),
                        ),
                    })
                })
                .collect();

            emitter.emit(vf::Instruction::IsEnumVariantOrBreak {
                not_variant_block_id: Box::new(when_false_label.clone()),
                enum_type: Box::new(enum_type_token),
                variant_id,
                value: Box::new(value_reg),
                args: arg_patterns
                    .iter()
                    .map(|(arg_reg, _)| Box::new(arg_reg.clone()))
                    .collect(),
                field_extractors,
            });

            for (arg_reg, arg) in arg_patterns {
                emit_pattern(emitter, when_false_label, arg_reg.clone(), arg)?;
            }

            for (field_reg, field) in field_patterns {
                emit_pattern(emitter, when_false_label, field_reg.clone(), &field.pattern)?;
            }
        }
        Pattern::String(s) => {
            let sr = emitter.expr(&Expr::StringLiteral(Box::from(s.as_str())), AnyRegister)?;

            let check_res = emitter.add_var(vf::Token::Builtin {
                b: Box::new(vf::BuiltinType::Bool {}),
                args: vec![],
            });

            emitter.emit(vf::Instruction::Builtin {
                op: vf::BuiltinOp::StringEq,
                tokens: vec![],
                registers: vec![
                    Box::new(check_res.clone()),
                    Box::new(sr),
                    Box::new(value_reg),
                ],
            });
            emitter.emit(vf::Instruction::BlockBreakUnless {
                block_id: Box::new(when_false_label.clone()),
                condition: Box::new(check_res),
            });
        }
        Pattern::Int(i) => {
            let sr = emitter.expr(&Expr::IntLiteral(i.clone()), AnyRegister)?;

            let check_res = emitter.add_var(vf::Token::Builtin {
                b: Box::new(vf::BuiltinType::Bool {}),
                args: vec![],
            });

            emitter.emit(vf::Instruction::Builtin {
                op: vf::BuiltinOp::IntEq,
                tokens: vec![],
                registers: vec![
                    Box::new(check_res.clone()),
                    Box::new(sr),
                    Box::new(value_reg),
                ],
            });
            emitter.emit(vf::Instruction::BlockBreakUnless {
                block_id: Box::new(when_false_label.clone()),
                condition: Box::new(check_res),
            });
        }
        Pattern::Bool(b) => {
            let sr = emitter.expr(&Expr::BoolLiteral(*b), AnyRegister)?;

            let check_res = emitter.add_var(vf::Token::Builtin {
                b: Box::new(vf::BuiltinType::Bool {}),
                args: vec![],
            });

            emitter.emit(vf::Instruction::Builtin {
                op: vf::BuiltinOp::BoolEq,
                tokens: vec![],
                registers: vec![
                    Box::new(check_res.clone()),
                    Box::new(sr),
                    Box::new(value_reg),
                ],
            });
            emitter.emit(vf::Instruction::BlockBreakUnless {
                block_id: Box::new(when_false_label.clone()),
                condition: Box::new(check_res),
            });
        }
    })
}
