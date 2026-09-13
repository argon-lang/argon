use crate::program::Program;
use argon_format_vm::vm as vf;
use argon_io::OutputStream;
use argon_vm::model::{ModuleExportEntry, ModuleModel};
use esexpr::{ESExpr, ESExprCodec};

fn q(s: &str) -> String {
    format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'"))
}
fn r(id: &vf::RegisterId) -> String {
    format!("$r{}", id.id)
}
fn list<T>(xs: &[T], f: impl Fn(&T) -> String) -> String {
    xs.iter().map(f).collect::<Vec<_>>().join(", ")
}

pub async fn emit_module<W: OutputStream + ?Sized>(
    p: &Program<'_>,
    module: &ModuleModel,
    package: &str,
    out: &mut W,
) -> Result<(), String> {
    write(out, "package ").await?;
    write(out, package).await?;
    write(
        out,
        ";\nuse 5.020;\nuse strict;\nuse warnings;\nuse Math::BigInt ();\nuse Argon::Runtime 0.1.0 ();\n\n",
    )
    .await?;
    let mut imports = p
        .model
        .module_info
        .keys()
        .map(|id| p.package_for_module(id))
        .collect::<Result<Vec<_>, _>>()?;
    imports.sort();
    imports.dedup();
    for import in imports {
        if import != package {
            write(out, "use ").await?;
            write(out, &import).await?;
            write(out, " ();\n").await?;
        }
    }
    write(out, "\n").await?;
    for e in &module.exports {
        let mut declaration = String::new();
        match e {
            ModuleExportEntry::FunctionDefinition(d) => emit_function(
                p,
                &mut declaration,
                &p.callable_name(&d.import)?,
                &d.signature,
                d.implementation.as_deref(),
                false,
            )?,
            ModuleExportEntry::RecordDefinition(d) => emit_record(p, &mut declaration, d)?,
            ModuleExportEntry::EnumDefinition(d) => emit_enum(p, &mut declaration, d)?,
            ModuleExportEntry::TraitDefinition(d) => emit_trait(p, &mut declaration, d)?,
            ModuleExportEntry::InstanceDefinition(d) => emit_instance(p, &mut declaration, d)?,
        }
        write(out, &declaration).await?;
    }
    write(out, "\n1;\n").await
}

async fn write<W: OutputStream + ?Sized>(out: &mut W, value: &str) -> Result<(), String> {
    out.write_all(value.as_bytes())
        .await
        .map_err(|error| error.to_string())
}

fn emit_function(
    p: &Program<'_>,
    out: &mut String,
    name: &str,
    sig: &vf::FunctionSignature,
    implementation: Option<&vf::FunctionImplementation>,
    receiver: bool,
) -> Result<(), String> {
    out.push_str(&format!("sub {name} {{\n"));
    let register_args = sig.parameters.len() + usize::from(receiver);
    if register_args > 0 {
        let mut sources = Vec::new();
        if receiver {
            sources.push("$_[0]".to_owned());
        }
        let values_start = usize::from(receiver) + sig.token_parameters.len();
        sources.extend((0..sig.parameters.len()).map(|i| format!("$_[{}]", values_start + i)));
        out.push_str(&format!(
            "  my ({}) = ({});\n",
            (0..register_args)
                .map(|i| format!("$r{i}"))
                .collect::<Vec<_>>()
                .join(", "),
            sources.join(", ")
        ));
    }
    if !sig.token_parameters.is_empty() {
        let first = usize::from(receiver);
        let last = first + sig.token_parameters.len() - 1;
        out.push_str(&format!("  my @__tokens = @_[{first}..{last}];\n"));
    }
    if receiver {
        out.push_str("  my @__parent_tokens = $r0->descriptor->tokens;\n");
    } else {
        out.push_str("  my @__parent_tokens = defined($Argon::Runtime::CURRENT_DESCRIPTOR) ? $Argon::Runtime::CURRENT_DESCRIPTOR->tokens : ();\n");
    }
    match implementation {
        Some(vf::FunctionImplementation::VmIr { body }) => {
            let start = register_args;
            let count = register_args + body.variables.variables.len();
            if count > start {
                out.push_str(&format!(
                    "  my ({});\n",
                    (start..count)
                        .map(|i| format!("$r{i}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ));
            }
            out.push_str("  my $__ok = eval {\n");
            emit_region(p, out, &body.region, 2)?;
            out.push_str("    1;\n  };\n  if (!$__ok) { my $__e=$@; if (ref($__e) eq 'Argon::Runtime::Control::Return') { return $__e->{value}; } if (ref($__e) eq 'Argon::Runtime::Control::Tail') { my $__f=$__e->{callee}; @_=@{$__e->{args}}; goto &$__f; } die $__e; }\n");
        }
        Some(vf::FunctionImplementation::Extern { r#extern }) => {
            let (imports, declarations, expression) = extern_parts(r#extern)?;
            let private = format!(
                "Argon::Generated::Extern::{}",
                name.bytes()
                    .fold(0u64, |a, b| a.wrapping_mul(131).wrapping_add(u64::from(b)))
            );
            out.push_str(&format!("  state $__extern = do {{ package {private}; {} {} {expression} }};\n  die Argon::Runtime::foreign_exception('Perl extern did not produce a coderef') unless ref($__extern) eq 'CODE';\n  return $__extern->(@_);\n",imports.join(" "),declarations.join(" ")));
        }
        None => out.push_str("  die Argon::Runtime::foreign_exception('abstract callable');\n"),
    }
    out.push_str("}\n\n");
    Ok(())
}

fn extern_parts(
    value: &noble_idl_runtime::Esexpr,
) -> Result<(Vec<String>, Vec<String>, String), String> {
    let ESExpr::Constructor(c) = value.encode_esexpr() else {
        return Err("malformed Perl extern metadata".into());
    };
    if c.name.to_string() != "perl-extern" {
        return Err("extern implementation is not Perl metadata".into());
    }
    fn item<'a>(c: &'a esexpr::ESExprConstructor<'a>, k: &str) -> Option<ESExpr<'a>> {
        c.kwargs
            .iter()
            .find_map(|(name, value)| (name.to_string() == k).then_some(value))
    }
    fn string(c: &esexpr::ESExprConstructor<'_>, k: &str) -> Result<String, String> {
        match item(c, k) {
            Some(ESExpr::Str(s)) => Ok(s.to_string()),
            _ => Err(format!("Perl extern is missing {k}")),
        }
    }
    fn strings(c: &esexpr::ESExprConstructor<'_>, k: &str) -> Result<Vec<String>, String> {
        let Some(ESExpr::Constructor(x)) = item(c, k) else {
            return Err(format!("Perl extern is missing {k}"));
        };
        if x.name.to_string() != "list" {
            return Err(format!("Perl extern {k} is not a list"));
        }
        x.args
            .iter()
            .map(|v| match v {
                ESExpr::Str(s) => Ok(s.to_string()),
                _ => Err(format!("Perl extern {k} contains a non-string")),
            })
            .collect()
    }
    Ok((
        strings(&c, "imports")?,
        strings(&c, "declarations")?,
        string(&c, "expression")?,
    ))
}

fn emit_methods(
    p: &Program<'_>,
    o: &mut String,
    pkg: &str,
    methods: &[Box<vf::MethodDefinition>],
) -> Result<Vec<(String, String)>, String> {
    let mut v = Vec::new();
    for m in methods {
        let mn = crate::names::mangled(&m.name, &m.erased_signature, &|x| p.import_symbol(x))?;
        let f = format!("{pkg}::__method_{}", m.method_id);
        emit_function(p, o, &f, &m.signature, m.implementation.as_deref(), true)?;
        v.push((mn, f));
    }
    Ok(v)
}
fn emit_statics(
    p: &Program<'_>,
    o: &mut String,
    pkg: &str,
    methods: &[Box<vf::StaticMethodDefinition>],
) -> Result<Vec<(String, String)>, String> {
    let mut v = Vec::new();
    for m in methods {
        let mn = crate::names::mangled(&m.name, &m.erased_signature, &|x| p.import_symbol(x))?;
        let f = format!("{pkg}::__static_{}", m.static_method_id);
        emit_function(p, o, &f, &m.signature, m.implementation.as_deref(), false)?;
        v.push((mn, f));
    }
    Ok(v)
}
fn map_code(xs: &[(String, String)]) -> String {
    xs.iter()
        .map(|(n, f)| format!("{}=>\\&{f}", q(n)))
        .collect::<Vec<_>>()
        .join(", ")
}
fn vtable_methods(
    p: &Program<'_>,
    table: &vf::Vtable,
    implementations: &[(String, String)],
) -> Result<Vec<(String, String)>, String> {
    let mut result = implementations.to_vec();
    for entry in &table.entries {
        if let vf::VtableTarget::Implementation { method_index } = entry.target.as_ref() {
            let index = method_index
                .to_string()
                .parse::<usize>()
                .map_err(|_| format!("invalid vtable method index {method_index}"))?;
            let target = implementations
                .get(index)
                .ok_or_else(|| format!("vtable method index {method_index} is out of range"))?;
            let slot = p
                .model
                .method_info
                .get(&entry.slot_method_id)
                .ok_or_else(|| format!("unknown vtable slot method {}", entry.slot_method_id))?;
            let name =
                crate::names::mangled(&slot.name, &slot.erased_signature, &|x| p.import_symbol(x))?;
            if let Some(existing) = result.iter_mut().find(|(key, _)| key == &name) {
                existing.1 = target.1.clone();
            } else {
                result.push((name, target.1.clone()));
            }
        }
    }
    Ok(result)
}
fn emit_specialize(
    o: &mut String,
    pkg: &str,
    kind: &str,
    fields: &[(String, bool)],
    methods: &[(String, String)],
    statics: &[(String, String)],
    variants: &str,
) {
    let fs = fields
        .iter()
        .map(|(n, _)| q(n))
        .collect::<Vec<_>>()
        .join(", ");
    o.push_str(&format!("package {pkg};\nour @ISA = ('Argon::Runtime::Value');\nsub specialize {{ shift; Argon::Runtime::descriptor({}, {}, \\@_, {{ value_class=>{}, fields=>[{fs}], methods=>{{{}}}, static=>{{{}}}, variants=>{{{variants}}} }}) }}\n",q(pkg),q(kind),q(pkg),map_code(methods),map_code(statics)));
    for (idx, (name, mutable)) in fields.iter().enumerate() {
        o.push_str(&format!("sub get_{name} {{ $_[0]->field({idx}) }}\n"));
        if *mutable {
            o.push_str(&format!(
                "sub set_{name} {{ $_[0]->set_field({idx}, $_[1]) }}\n"
            ));
        }
    }
    o.push_str(
        "sub descriptor { $_[0]{descriptor} }\nsub argument { $_[0]{arguments}[$_[1]] }\n\n",
    );
}
fn emit_record(p: &Program<'_>, o: &mut String, d: &vf::RecordDefinition) -> Result<(), String> {
    let pkg = p.type_name(&d.import)?;
    let methods = emit_methods(p, o, &pkg, &d.methods)?;
    let methods = vtable_methods(p, &d.vtable, &methods)?;
    let statics = emit_statics(p, o, &pkg, &d.static_methods)?;
    let fields = d
        .fields
        .iter()
        .map(|f| (crate::names::identifier(&f.name), bool::from(f.mutable)))
        .collect::<Vec<_>>();
    emit_specialize(o, &pkg, "record", &fields, &methods, &statics, "");
    Ok(())
}
fn emit_enum(p: &Program<'_>, o: &mut String, d: &vf::EnumDefinition) -> Result<(), String> {
    let pkg = p.type_name(&d.import)?;
    let methods = emit_methods(p, o, &pkg, &d.methods)?;
    let methods = vtable_methods(p, &d.vtable, &methods)?;
    let statics = emit_statics(p, o, &pkg, &d.static_methods)?;
    let mut variants = Vec::new();
    for v in &d.variants {
        let name = crate::names::identifier(&v.name);
        let vp = format!("{pkg}::Variant::{name}");
        let vm = emit_methods(p, o, &vp, &v.methods)?;
        let vm = vtable_methods(p, &v.vtable, &vm)?;
        let fields = v
            .fields
            .iter()
            .map(|f| (crate::names::identifier(&f.name), bool::from(f.mutable)))
            .collect::<Vec<_>>();
        emit_specialize(o, &vp, "variant", &fields, &vm, &[], "");
        variants.push(format!(
            "{}=>[{}, [{}], {{{}}}]",
            q(&name),
            q(&vp),
            fields
                .iter()
                .map(|x| q(&x.0))
                .collect::<Vec<_>>()
                .join(", "),
            map_code(&vm)
        ));
    }
    emit_specialize(
        o,
        &pkg,
        "enum",
        &[],
        &methods,
        &statics,
        &variants.join(", "),
    );
    Ok(())
}
fn emit_trait(p: &Program<'_>, o: &mut String, d: &vf::TraitDefinition) -> Result<(), String> {
    let pkg = p.type_name(&d.import)?;
    let methods = emit_methods(p, o, &pkg, &d.methods)?;
    let methods = vtable_methods(p, &d.vtable, &methods)?;
    let statics = emit_statics(p, o, &pkg, &d.static_methods)?;
    emit_specialize(o, &pkg, "trait", &[], &methods, &statics, "");
    Ok(())
}
fn emit_instance(
    p: &Program<'_>,
    o: &mut String,
    d: &vf::InstanceDefinition,
) -> Result<(), String> {
    let pkg = p.type_name(&d.import)?;
    let methods = emit_methods(p, o, &pkg, &d.methods)?;
    let methods = vtable_methods(p, &d.vtable, &methods)?;
    emit_specialize(o, &pkg, "instance", &[], &methods, &[], "");
    Ok(())
}

fn ind(n: usize) -> String {
    "  ".repeat(n)
}
fn emit_region(p: &Program<'_>, o: &mut String, x: &vf::Region, n: usize) -> Result<(), String> {
    match x {
        vf::Region::BasicBlock { instructions } => {
            for i in instructions {
                emit_instruction(p, o, i, n)?
            }
        }
        vf::Region::Sequence { regions } => {
            for x in regions {
                emit_region(p, o, x, n)?
            }
        }
        vf::Region::Block {
            block_id,
            flags,
            region,
        } => {
            let looped = flags.is_loop;
            o.push_str(&format!("{}while (1) {{\n", ind(n)));
            o.push_str(&format!("{}my $__ok = eval {{\n", ind(n + 1)));
            emit_region(p, o, region, n + 2)?;
            o.push_str(&format!("{}1; }};\n", ind(n + 2)));
            o.push_str(&format!("{}if (!$__ok) {{ my $__e=$@; if (ref($__e) eq 'Argon::Runtime::Control::Break' && $__e->{{id}} eq '{}') {{ last; }} if (ref($__e) eq 'Argon::Runtime::Control::Retry' && $__e->{{id}} eq '{}') {{ next; }} die $__e; }}\n",ind(n+1),block_id.id,block_id.id));
            if !looped {
                o.push_str(&format!("{}last;\n", ind(n + 1)))
            }
            o.push_str(&format!("{}}}\n", ind(n)));
        }
        vf::Region::IfElse {
            when_true_block_id,
            when_false_block_id,
            condition,
            when_true,
            when_false,
        } => {
            o.push_str(&format!("{}{{\n", ind(n)));
            o.push_str(&format!(
                "{}my ($__branch, $__transfer); my $__ok=eval {{\n",
                ind(n)
            ));
            emit_region(p, o, condition, n + 1)?;
            o.push_str(&format!("{}1; }};\n", ind(n + 1)));
            o.push_str(&format!("{}if (!$__ok) {{ my $__e=$@; if (ref($__e) eq 'Argon::Runtime::Control::Break') {{ $__branch=$__e->{{id}}; $__transfer=$__e; }} else {{ die $__e; }} }}\n",ind(n)));
            o.push_str(&format!(
                "{}if (!defined($__branch) || $__branch eq '{}') {{\n",
                ind(n),
                when_true_block_id.id
            ));
            emit_region(p, o, when_true, n + 1)?;
            o.push_str(&format!(
                "{}}} elsif (defined($__branch) && $__branch eq '{}') {{\n",
                ind(n),
                when_false_block_id.id
            ));
            emit_region(p, o, when_false, n + 1)?;
            o.push_str(&format!("{}}} else {{ die $__transfer; }}\n", ind(n)));
            o.push_str(&format!("{}}}\n", ind(n)));
        }
        vf::Region::Finally { action, ensuring } => {
            o.push_str(&format!("{}my $__action_ok=eval {{\n", ind(n)));
            emit_region(p, o, action, n + 1)?;
            o.push_str(&format!("{}1; }}; my $__pending=$@;\n", ind(n + 1)));
            o.push_str(&format!("{}my $__cleanup_ok=eval {{\n", ind(n)));
            emit_region(p, o, ensuring, n + 1)?;
            o.push_str(&format!("{}1; }}; my $__cleanup=$@; die $__cleanup unless $__cleanup_ok; die $__pending unless $__action_ok;\n",ind(n+1)));
        }
    };
    Ok(())
}

fn signal(
    o: &mut String,
    n: usize,
    kind: &str,
    id: &impl core::fmt::Display,
    cond: Option<String>,
) {
    o.push_str(&format!(
        "{}die bless({{id=>'{}'}}, 'Argon::Runtime::Control::{kind}'){};\n",
        ind(n),
        id,
        cond.map(|c| format!(" if {c}")).unwrap_or_default()
    ))
}
fn emit_call(o: &mut String, n: usize, d: &vf::FunctionResult, callee: String, args: String) {
    match d{
    vf::FunctionResult::Register{id}=>o.push_str(&format!("{}{} = ({callee})->({args});\n",ind(n),r(id))),
    vf::FunctionResult::Discard{}=>o.push_str(&format!("{}({callee})->({args});\n",ind(n))),
    vf::FunctionResult::ReturnValue{}=>o.push_str(&format!("{}my $__callee={callee}; my @__args=({args}); die bless({{callee=>$__callee,args=>\\@__args}}, 'Argon::Runtime::Control::Tail');\n",ind(n))),
}
}
fn token(p: &Program<'_>, t: &vf::Token) -> Result<String, String> {
    use vf::{BuiltinType as B, Token as T};
    Ok(match t {
        T::Builtin { b } => match b.as_ref() {
            B::Int { integer_type } => format!(
                "Argon::Runtime::canonical_token('int', '{}')",
                itype(*integer_type)
            ),
            B::Bool {} => "Argon::Runtime::canonical_token('bool')".into(),
            B::String {} => "Argon::Runtime::canonical_token('string')".into(),
            B::Never {} => "Argon::Runtime::canonical_token('never')".into(),
            B::Array { element_type } => format!(
                "Argon::Runtime::canonical_token('array', {})",
                token(p, element_type)?
            ),
            B::Conjunction { .. } | B::Disjunction { .. } => {
                return Err("conjunction/disjunction token is unsupported by Perl ABI".into());
            }
        },
        T::Function { input, output } => format!(
            "Argon::Runtime::canonical_token('function', {}, {})",
            token(p, input)?,
            token(p, output)?
        ),
        T::FunctionErased { output } => format!(
            "Argon::Runtime::canonical_token('function-erased', {})",
            token(p, output)?
        ),
        T::FunctionToken { token_kind, output } => format!(
            "Argon::Runtime::canonical_token('function-token', {}, {})",
            token(p, token_kind)?,
            token(p, output)?
        ),
        T::Record { record_id, args } => format!(
            "{}->specialize({})",
            p.type_name(
                &p.model
                    .record_info
                    .get(record_id)
                    .ok_or_else(|| format!("unknown record id {record_id}"))?
                    .import_specifier
            )?,
            args.iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        T::Enum { enum_id, args } => format!(
            "{}->specialize({})",
            p.type_name(
                &p.model
                    .enum_info
                    .get(enum_id)
                    .ok_or_else(|| format!("unknown enum id {enum_id}"))?
                    .import_specifier
            )?,
            args.iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        T::Trait { trait_id, args } => format!(
            "{}->specialize({})",
            p.type_name(
                &p.model
                    .trait_info
                    .get(trait_id)
                    .ok_or_else(|| format!("unknown trait id {trait_id}"))?
                    .import_specifier
            )?,
            args.iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        T::InstanceType { instance_id, args } => format!(
            "{}->specialize({})",
            p.type_name(
                &p.model
                    .instance_info
                    .get(instance_id)
                    .ok_or_else(|| format!("unknown instance id {instance_id}"))?
                    .import_specifier
            )?,
            args.iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        T::InstanceValue { instance_id, args } => format!(
            "{}->specialize({})->construct()",
            p.type_name(
                &p.model
                    .instance_info
                    .get(instance_id)
                    .ok_or_else(|| format!("unknown instance id {instance_id}"))?
                    .import_specifier
            )?,
            args.iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        T::ParentTokenParameter { index } => format!("$__parent_tokens[{index}]"),
        T::TokenParameter { index } => format!("$__tokens[{index}]"),
        T::RefCell { inner } => format!(
            "Argon::Runtime::canonical_token('ref', {})",
            token(p, inner)?
        ),
        T::Tuple { elements } => format!(
            "Argon::Runtime::canonical_token('tuple', {})",
            elements
                .iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?
                .join(", ")
        ),
        T::TypeInfo {} => "Argon::Runtime::canonical_token('type-info')".into(),
        T::Boxed {} => "Argon::Runtime::canonical_token('boxed')".into(),
    })
}
fn itype(t: vf::IntegerType) -> &'static str {
    match t {
        vf::IntegerType::Int => "int",
        vf::IntegerType::I8 => "i8",
        vf::IntegerType::U8 => "u8",
        vf::IntegerType::I16 => "i16",
        vf::IntegerType::U16 => "u16",
        vf::IntegerType::I32 => "i32",
        vf::IntegerType::U32 => "u32",
        vf::IntegerType::I64 => "i64",
        vf::IntegerType::U64 => "u64",
    }
}
fn norm(t: vf::IntegerType, e: String) -> String {
    match t {
        vf::IntegerType::Int => format!("Math::BigInt->new(''.({e}))"),
        vf::IntegerType::I8 => format!("Argon::Runtime::normalize_integer({e},8,1,0)"),
        vf::IntegerType::U8 => format!("Argon::Runtime::normalize_integer({e},8,0,0)"),
        vf::IntegerType::I16 => format!("Argon::Runtime::normalize_integer({e},16,1,0)"),
        vf::IntegerType::U16 => format!("Argon::Runtime::normalize_integer({e},16,0,0)"),
        vf::IntegerType::I32 => format!("Argon::Runtime::normalize_integer({e},32,1,0)"),
        vf::IntegerType::U32 => format!("Argon::Runtime::normalize_integer({e},32,0,0)"),
        vf::IntegerType::I64 => format!("Argon::Runtime::normalize_integer({e},64,1,1)"),
        vf::IntegerType::U64 => format!("Argon::Runtime::normalize_integer({e},64,0,1)"),
    }
}

fn emit_instruction(
    p: &Program<'_>,
    o: &mut String,
    i: &vf::Instruction,
    n: usize,
) -> Result<(), String> {
    use vf::Instruction as I;
    match i {
        I::BlockBreak { block_id } => signal(o, n, "Break", &block_id.id, None),
        I::BlockBreakIf {
            block_id,
            condition,
        } => signal(o, n, "Break", &block_id.id, Some(r(condition))),
        I::BlockBreakUnless {
            block_id,
            condition,
        } => signal(
            o,
            n,
            "Break",
            &block_id.id,
            Some(format!("!{}", r(condition))),
        ),
        I::BlockRetry { block_id } => signal(o, n, "Retry", &block_id.id, None),
        I::Box { dest, value } => line(
            o,
            n,
            format!("{} = Argon::Runtime::box({})", r(dest), r(value)),
        ),
        I::Builtin { op } => emit_builtin(o, op, n),
        I::ConstBool { dest, value } => line(
            o,
            n,
            format!("{} = {}", r(dest), usize::from(bool::from(*value))),
        ),
        I::ConstInt { dest, value } => line(
            o,
            n,
            format!("{} = Math::BigInt->new({})", r(dest), q(&value.to_string())),
        ),
        I::ConstI8 { dest, value } => line(o, n, format!("{} = {value}", r(dest))),
        I::ConstU8 { dest, value } => line(o, n, format!("{} = {value}", r(dest))),
        I::ConstI16 { dest, value } => line(o, n, format!("{} = {value}", r(dest))),
        I::ConstU16 { dest, value } => line(o, n, format!("{} = {value}", r(dest))),
        I::ConstI32 { dest, value } => line(o, n, format!("{} = {value}", r(dest))),
        I::ConstU32 { dest, value } => line(o, n, format!("{} = {value}", r(dest))),
        I::ConstI64 { dest, value } => line(
            o,
            n,
            format!("{} = Math::BigInt->new({})", r(dest), q(&value.to_string())),
        ),
        I::ConstU64 { dest, value } => line(
            o,
            n,
            format!("{} = Math::BigInt->new({})", r(dest), q(&value.to_string())),
        ),
        I::ConstString { dest, value } => line(o, n, format!("{} = {}", r(dest), q(value))),
        I::Move { dest, src } => line(o, n, format!("{} = {}", r(dest), r(src))),
        I::Tuple { dest, values } => {
            line(o, n, format!("{} = [{}]", r(dest), list(values, |x| r(x))))
        }
        I::TupleElement {
            element_index,
            dest,
            src,
        } => line(
            o,
            n,
            format!("{} = {}->[{}]", r(dest), r(src), element_index),
        ),
        I::NewReference { dest, value } => line(
            o,
            n,
            format!("{} = Argon::Runtime::reference({})", r(dest), r(value)),
        ),
        I::LoadReference { dest, r#ref } => {
            line(o, n, format!("{} = {}->get()", r(dest), r(r#ref)))
        }
        I::UpdateReference { r#ref, value } => {
            line(o, n, format!("{}->set({})", r(r#ref), r(value)))
        }
        I::Unbox { dest, value, .. } => line(o, n, format!("{} = {}->value()", r(dest), r(value))),
        I::LoadToken { dest, token: t } => line(o, n, format!("{} = {}", r(dest), token(p, t)?)),
        I::Raise { exception } => o.push_str(&format!("{}die {};\n", ind(n), r(exception))),
        I::Return { src } => o.push_str(&format!(
            "{}die bless({{value=>{}}}, 'Argon::Runtime::Control::Return');\n",
            ind(n),
            r(src)
        )),
        I::Unreachable {} => o.push_str(&format!(
            "{}die Argon::Runtime::foreign_exception('unreachable instruction executed');\n",
            ind(n)
        )),
        I::FunctionCall {
            function_id,
            dest,
            token_args,
            args,
        } => {
            let f = p.callable_name(
                &p.model
                    .function_info
                    .get(function_id)
                    .ok_or_else(|| format!("unknown function id {function_id}"))?
                    .import_specifier,
            )?;
            emit_call(
                o,
                n,
                dest,
                format!("\\&{f}"),
                call_args(p, token_args, args)?,
            )
        }
        I::FunctionObjectCall {
            dest,
            function,
            arg,
        } => emit_call(o, n, dest, r(function), r(arg)),
        I::FunctionObjectTokenCall {
            dest,
            function,
            arg,
        } => emit_call(o, n, dest, r(function), token(p, arg)?),
        I::FunctionObjectErasedCall { dest, function } => {
            emit_call(o, n, dest, r(function), String::new())
        }
        I::StaticMethodCall {
            static_method_id,
            owner_type,
            dest,
            token_args,
            args,
        } => {
            let info = p
                .model
                .static_method_info
                .get(static_method_id)
                .ok_or_else(|| format!("unknown static method id {static_method_id}"))?;
            let mn =
                crate::names::mangled(&info.name, &info.erased_signature, &|x| p.import_symbol(x))?;
            emit_call(
                o,
                n,
                dest,
                format!("{}->static_method({})", token(p, owner_type)?, q(&mn)),
                call_args(p, token_args, args)?,
            )
        }
        I::InstanceMethodCall {
            dest,
            method_id,
            instance_type,
            instance_object,
            token_args,
            args,
        } => {
            let info = p
                .model
                .method_info
                .get(method_id)
                .ok_or_else(|| format!("unknown method id {method_id}"))?;
            let mn =
                crate::names::mangled(&info.name, &info.erased_signature, &|x| p.import_symbol(x))?;
            let t = token(p, instance_type)?;
            let mut a = vec![r(instance_object)];
            a.extend(
                token_args
                    .iter()
                    .map(|x| token(p, x))
                    .collect::<Result<Vec<_>, _>>()?,
            );
            a.extend(args.iter().map(|x| r(x)));
            emit_call(
                o,
                n,
                dest,
                format!(
                    "sub {{ {t}->dispatch($_[0], {t}->method({}), @_[1..$#_]) }}",
                    q(&mn)
                ),
                a.join(", "),
            )
        }
        I::PartiallyAppliedFunction {
            function_id,
            dest,
            token_args,
            args,
        }
        | I::PartiallyAppliedTokenFunction {
            function_id,
            dest,
            token_args,
            args,
        }
        | I::PartiallyAppliedFunctionErased {
            function_id,
            dest,
            token_args,
            args,
        } => {
            let f = p.callable_name(
                &p.model
                    .function_info
                    .get(function_id)
                    .ok_or_else(|| format!("unknown function id {function_id}"))?
                    .import_specifier,
            )?;
            let captured = call_args(p, token_args, args)?;
            line(
                o,
                n,
                format!(
                    "{} = sub {{ {f}({captured}{} @_) }}",
                    r(dest),
                    if captured.is_empty() { "" } else { ", " }
                ),
            )
        }
        I::NewInstance {
            instance_id,
            dest,
            token_args,
            args,
        } => {
            let info = p
                .model
                .instance_info
                .get(instance_id)
                .ok_or_else(|| format!("unknown instance id {instance_id}"))?;
            line(
                o,
                n,
                format!(
                    "{} = {}->specialize({})->construct({})",
                    r(dest),
                    p.type_name(&info.import_specifier)?,
                    token_args
                        .iter()
                        .map(|x| token(p, x))
                        .collect::<Result<Vec<_>, _>>()?
                        .join(", "),
                    list(args, |x| r(x))
                ),
            )
        }
        I::LoadInstanceField {
            dest,
            instance_object,
            parameter_index,
            ..
        } => line(
            o,
            n,
            format!(
                "{} = {}->argument({parameter_index})",
                r(dest),
                r(instance_object)
            ),
        ),
        I::RecordLiteral {
            dest,
            record_type,
            fields,
        } => {
            let pairs = fields
                .iter()
                .map(|f| {
                    let x = p
                        .model
                        .record_field_info
                        .get(&f.field_id)
                        .ok_or_else(|| format!("unknown field id {}", f.field_id))?;
                    Ok(format!(
                        "{}=>{}",
                        q(&crate::names::identifier(&x.name)),
                        r(&f.value)
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            line(
                o,
                n,
                format!(
                    "{} = {}->construct({{{}}})",
                    r(dest),
                    token(p, record_type)?,
                    pairs.join(", ")
                ),
            )
        }
        I::RecordFieldLoad {
            field_id,
            dest,
            record_value,
        } => {
            let f = p
                .model
                .record_field_info
                .get(field_id)
                .ok_or_else(|| format!("unknown field id {field_id}"))?;
            line(
                o,
                n,
                format!(
                    "{} = {}->get_{}()",
                    r(dest),
                    r(record_value),
                    crate::names::identifier(&f.name)
                ),
            )
        }
        I::RecordFieldStore {
            field_id,
            record_value,
            field_value,
        } => {
            let f = p
                .model
                .record_field_info
                .get(field_id)
                .ok_or_else(|| format!("unknown field id {field_id}"))?;
            line(
                o,
                n,
                format!(
                    "{}->set_{}({})",
                    r(record_value),
                    crate::names::identifier(&f.name),
                    r(field_value)
                ),
            )
        }
        I::EnumVariantLiteral {
            dest,
            enum_type,
            variant_id,
            token_args,
            args,
            fields,
        } => {
            let vi = p
                .model
                .enum_variant_info
                .get(variant_id)
                .ok_or_else(|| format!("unknown variant id {variant_id}"))?;
            let pairs = fields
                .iter()
                .map(|f| {
                    let x = p
                        .model
                        .record_field_info
                        .get(&f.field_id)
                        .ok_or_else(|| format!("unknown field id {}", f.field_id))?;
                    Ok(format!(
                        "{}=>{}",
                        q(&crate::names::identifier(&x.name)),
                        r(&f.value)
                    ))
                })
                .collect::<Result<Vec<_>, String>>()?;
            let mut av = token_args
                .iter()
                .map(|x| token(p, x))
                .collect::<Result<Vec<_>, _>>()?;
            av.extend(args.iter().map(|x| r(x)));
            av.push(format!("{{{}}}", pairs.join(", ")));
            line(
                o,
                n,
                format!(
                    "{} = {}->variant({})->construct({})",
                    r(dest),
                    token(p, enum_type)?,
                    q(&crate::names::identifier(&vi.name)),
                    av.join(", ")
                ),
            )
        }
        I::IsEnumVariantOrBreak {
            not_variant_block_id,
            variant_id,
            value,
            args,
            field_extractors,
            ..
        } => {
            let vi = p
                .model
                .enum_variant_info
                .get(variant_id)
                .ok_or_else(|| format!("unknown variant id {variant_id}"))?;
            o.push_str(&format!(
                "{}if ({}->variant->name ne {}) {{\n",
                ind(n),
                r(value),
                q(&crate::names::identifier(&vi.name))
            ));
            signal(o, n + 1, "Break", &not_variant_block_id.id, None);
            o.push_str(&format!("{}}}\n", ind(n)));
            for (k, d) in args.iter().enumerate() {
                line(o, n, format!("{} = {}->argument({k})", r(d), r(value)))
            }
            for f in field_extractors {
                let fi = p
                    .model
                    .record_field_info
                    .get(&f.field_id)
                    .ok_or_else(|| format!("unknown field id {}", f.field_id))?;
                line(
                    o,
                    n,
                    format!(
                        "{} = {}->get_{}()",
                        r(&f.r),
                        r(value),
                        crate::names::identifier(&fi.name)
                    ),
                )
            }
        }
    };
    Ok(())
}

fn line(o: &mut String, n: usize, s: String) {
    o.push_str(&format!("{}{};\n", ind(n), s))
}
fn call_args(
    p: &Program<'_>,
    ts: &[Box<vf::Token>],
    vs: &[Box<vf::RegisterId>],
) -> Result<String, String> {
    let mut a = ts
        .iter()
        .map(|x| token(p, x))
        .collect::<Result<Vec<_>, _>>()?;
    a.extend(vs.iter().map(|x| r(x)));
    Ok(a.join(", "))
}
fn emit_builtin(o: &mut String, b: &vf::BuiltinOp, n: usize) {
    use vf::BuiltinOp as B;
    match b {
        B::IntNegate {
            integer_type,
            dest,
            value,
        } => line(
            o,
            n,
            format!(
                "{} = {}",
                r(dest),
                norm(*integer_type, format!("-{}", r(value)))
            ),
        ),
        B::IntBitNot {
            integer_type,
            dest,
            value,
        } => line(
            o,
            n,
            format!(
                "{} = {}",
                r(dest),
                norm(*integer_type, format!("~{}", r(value)))
            ),
        ),
        B::IntConvert {
            dest_type,
            dest,
            value,
            ..
        } => line(
            o,
            n,
            format!("{} = {}", r(dest), norm(*dest_type, r(value))),
        ),
        B::IntAdd {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "+", rhs),
        B::IntSub {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "-", rhs),
        B::IntMul {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "*", rhs),
        B::IntBitAnd {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "&", rhs),
        B::IntBitOr {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "|", rhs),
        B::IntBitXor {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "^", rhs),
        B::IntBitShiftLeft {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, "<<", rhs),
        B::IntBitShiftRight {
            integer_type,
            dest,
            lhs,
            rhs,
        } => ibin(o, n, *integer_type, dest, lhs, ">>", rhs),
        B::IntEq { dest, lhs, rhs, .. } => cmp(o, n, dest, lhs, "==", rhs),
        B::IntLt { dest, lhs, rhs, .. } => cmp(o, n, dest, lhs, "<", rhs),
        B::IntLe { dest, lhs, rhs, .. } => cmp(o, n, dest, lhs, "<=", rhs),
        B::IntGt { dest, lhs, rhs, .. } => cmp(o, n, dest, lhs, ">", rhs),
        B::IntGe { dest, lhs, rhs, .. } => cmp(o, n, dest, lhs, ">=", rhs),
        B::StringConcat { dest, args } => line(
            o,
            n,
            format!("{} = join('', {})", r(dest), list(args, |x| r(x))),
        ),
        B::StringEq { dest, lhs, rhs } => cmp(o, n, dest, lhs, "eq", rhs),
        B::BoolNot { dest, value } => line(o, n, format!("{} = {} ? 0 : 1", r(dest), r(value))),
        B::BoolEq { dest, lhs, rhs } => cmp(o, n, dest, lhs, "==", rhs),
        B::ArrayCreateUnsafeUninitialized { dest, length, .. } => line(
            o,
            n,
            format!("{} = Argon::Runtime::array_create({})", r(dest), r(length)),
        ),
        B::ArrayLength { dest, array, .. } => {
            line(o, n, format!("{} = scalar @{{{}}}", r(dest), r(array)))
        }
        B::ArrayGet {
            dest, array, index, ..
        } => line(
            o,
            n,
            format!(
                "{} = Argon::Runtime::array_get({}, {})",
                r(dest),
                r(array),
                r(index)
            ),
        ),
        B::ArraySet {
            array,
            index,
            value,
            ..
        } => line(
            o,
            n,
            format!(
                "Argon::Runtime::array_set({}, {}, {})",
                r(array),
                r(index),
                r(value)
            ),
        ),
    }
}
fn ibin(
    o: &mut String,
    n: usize,
    t: vf::IntegerType,
    d: &vf::RegisterId,
    l: &vf::RegisterId,
    op: &str,
    rh: &vf::RegisterId,
) {
    line(
        o,
        n,
        format!("{} = {}", r(d), norm(t, format!("{} {op} {}", r(l), r(rh)))),
    )
}
fn cmp(
    o: &mut String,
    n: usize,
    d: &vf::RegisterId,
    l: &vf::RegisterId,
    op: &str,
    rh: &vf::RegisterId,
) {
    line(
        o,
        n,
        format!("{} = ({} {op} {}) ? 1 : 0", r(d), r(l), r(rh)),
    )
}
