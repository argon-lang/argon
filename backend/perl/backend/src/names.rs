pub fn encode_component(value: &str) -> String {
    if value.is_empty() {
        return "_Q".into();
    }
    let mut out = String::new();
    for (index, byte) in value.as_bytes().iter().copied().enumerate() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' => out.push(char::from(byte)),
            b'0'..=b'9' if index != 0 => out.push(char::from(byte)),
            b'_' => out.push_str("__"),
            _ => out.push_str(&format!("_{byte:02X}")),
        }
    }
    out
}

pub fn decode_component(value: &str) -> Result<String, String> {
    if value == "_Q" {
        return Ok(String::new());
    }
    let bytes = value.as_bytes();
    let mut out = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'_' {
            out.push(bytes[i]);
            i += 1;
            continue;
        }
        if bytes.get(i + 1) == Some(&b'_') {
            out.push(b'_');
            i += 2;
            continue;
        }
        let hex = value.get(i + 1..i + 3).ok_or("truncated byte escape")?;
        out.push(u8::from_str_radix(hex, 16).map_err(|_| "invalid byte escape")?);
        i += 3;
    }
    let decoded = String::from_utf8(out).map_err(|_| "invalid UTF-8")?;
    if encode_component(&decoded) != value {
        return Err("non-canonical component".into());
    }
    Ok(decoded)
}

pub fn module_package(root: &[String], module: &[&str]) -> String {
    let mut parts = root.to_vec();
    parts.push("Module".into());
    if module.is_empty() {
        parts.push("_Root".into());
    } else {
        parts.extend(module.iter().map(|s| encode_component(s)));
    }
    parts.join("::")
}

use argon_format_vm::vm as vf;

pub fn identifier(value: &vf::Identifier) -> String {
    use vf::{BinaryOperator as B, Identifier as I, UnaryOperator as U};
    match value {
        I::Named { s } => encode_component(s),
        I::BinOp { op } => format!(
            "_O_{}",
            match op {
                B::Plus => "plus",
                B::Minus => "minus",
                B::Mul => "mul",
                B::Div => "div",
                B::Equal => "equal",
                B::NotEqual => "not_equal",
                B::LessThan => "less_than",
                B::LessThanEq => "less_than_eq",
                B::GreaterThan => "greater_than",
                B::GreaterThanEq => "greater_than_eq",
                B::BitOr => "bit_or",
                B::BitXor => "bit_xor",
                B::BitAnd => "bit_and",
                B::ShiftLeft => "shift_left",
                B::ShiftRight => "shift_right",
                B::Concat => "concat",
            }
        ),
        I::UnOp { op } => format!(
            "_U_{}",
            match op {
                U::Plus => "plus",
                U::Minus => "minus",
                U::BitNot => "bit_not",
                U::LogicalNot => "logical_not",
            }
        ),
        I::Index {} => "_I".into(),
        I::Extension { inner } => format!("_X_{}", identifier(inner)),
        I::Inverse { inner } => format!("_V_{}", identifier(inner)),
        I::Update { inner } => format!("_M_{}", identifier(inner)),
    }
}

pub fn erased_type(
    value: &vf::ErasedSignatureType,
    import_name: &impl Fn(&vf::ImportSpecifier) -> Result<String, String>,
) -> Result<String, String> {
    use vf::ErasedSignatureType as E;
    Ok(match value {
        E::Int {} => "b_int_e".into(),
        E::I8 {} => "b_i8_e".into(),
        E::U8 {} => "b_u8_e".into(),
        E::I16 {} => "b_i16_e".into(),
        E::U16 {} => "b_u16_e".into(),
        E::I32 {} => "b_i32_e".into(),
        E::U32 {} => "b_u32_e".into(),
        E::I64 {} => "b_i64_e".into(),
        E::U64 {} => "b_u64_e".into(),
        E::Bool {} => "b_bool_e".into(),
        E::String {} => "b_string_e".into(),
        E::Never {} => "b_never_e".into(),
        E::Array { element_type } => {
            format!("b_array_a{}_e", erased_type(element_type, import_name)?)
        }
        E::Function { input, output } => format!(
            "f_{}_r{}_e",
            erased_type(input, import_name)?,
            erased_type(output, import_name)?
        ),
        E::Record {
            record_import,
            args,
        } => {
            let n = import_name(record_import)?;
            let q = format!("{}{}", n.len(), format!("_{n}"));
            format!(
                "r_{q}_a{}_e",
                args.iter()
                    .map(|x| erased_type(x, import_name))
                    .collect::<Result<Vec<_>, _>>()?
                    .join("")
            )
        }
        E::Tuple { elements } => format!(
            "t_{}_e",
            elements
                .iter()
                .map(|x| erased_type(x, import_name))
                .collect::<Result<Vec<_>, _>>()?
                .join("")
        ),
        E::Erased {} => "z_".into(),
    })
}

pub fn mangled(
    name: &vf::Identifier,
    sig: &vf::ErasedSignature,
    import_name: &impl Fn(&vf::ImportSpecifier) -> Result<String, String>,
) -> Result<String, String> {
    let params = sig
        .params
        .iter()
        .map(|x| erased_type(x, import_name))
        .collect::<Result<Vec<_>, _>>()?
        .join("");
    Ok(format!(
        "{}__a{}__r{}",
        identifier(name),
        params,
        erased_type(&sig.result, import_name)?
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn examples_are_canonical() {
        for (plain, encoded) in [
            ("", "_Q"),
            ("Text", "Text"),
            ("snake_case", "snake__case"),
            ("2d", "_32d"),
            ("two words", "two_20words"),
            ("café", "caf_C3_A9"),
        ] {
            assert_eq!(encode_component(plain), encoded);
            assert_eq!(decode_component(encoded).as_deref(), Ok(plain));
        }
    }
    #[test]
    fn rejects_noncanonical() {
        assert!(decode_component("2d").is_err());
    }
}
