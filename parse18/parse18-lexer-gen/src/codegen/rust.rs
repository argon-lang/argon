use std::io::{self, Write};

use unicode_general_category::GeneralCategory;

use crate::fsm::{DFA, DFATransition};
use crate::regex::UnicodePropertySet;

pub struct RustSettings {
    pub module: Option<String>,
    pub visibility: String,
    pub type_name: String,
    pub token_type: String,
    pub acceptance_expr: bool,
    pub header_stmts: Vec<String>,
}

pub fn emit_rust<W: Write>(w: &mut W, dfa: DFA<String>, settings: &RustSettings) -> io::Result<()> {
    if let Some(module) = &settings.module {
        writeln!(w, "{}mod {} {{", settings.visibility, module)?;
    }

    let base_indent = if settings.module.is_some() { 1 } else { 0 };

    for stmt in &settings.header_stmts {
        emit_indent(w, base_indent)?;
        writeln!(w, "{}", stmt)?;
    }

    if !settings.header_stmts.is_empty() {
        writeln!(w)?;
    }

    emit_indent(w, base_indent)?;
    writeln!(w, "{}struct {};", settings.visibility, settings.type_name)?;
    writeln!(w)?;

    emit_indent(w, base_indent)?;
    writeln!(
        w,
        "#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]"
    )?;
    emit_indent(w, base_indent)?;
    writeln!(
        w,
        "{}enum {}State {{",
        settings.visibility, settings.type_name
    )?;
    for state_id in 0..dfa.states.len() {
        emit_indent(w, base_indent + 1)?;
        writeln!(w, "{},", state_name(state_id))?;
    }
    emit_indent(w, base_indent)?;
    writeln!(w, "}}")?;
    writeln!(w)?;

    emit_indent(w, base_indent)?;
    writeln!(w, "impl {} {{", settings.type_name)?;

    emit_indent(w, base_indent + 1)?;
    writeln!(
        w,
        "pub const INITIAL_STATE: {}State = {}State::{};",
        settings.type_name,
        settings.type_name,
        state_name(0)
    )?;
    writeln!(w)?;

    emit_indent(w, base_indent + 1)?;
    writeln!(
        w,
        "pub fn acceptance(state: {}State) -> parse18_runtime::LexerAcceptance<{}> {{",
        settings.type_name, settings.token_type
    )?;
    emit_indent(w, base_indent + 2)?;
    writeln!(w, "match state {{")?;
    for (state_id, state) in dfa.states.iter().enumerate() {
        emit_indent(w, base_indent + 3)?;
        match (&state.acceptance, state.is_reject) {
            (Some(acceptance), _) => {
                if settings.acceptance_expr {
                    writeln!(
                        w,
                        "{}State::{} => parse18_runtime::LexerAcceptance::Accept({acceptance}),",
                        settings.type_name,
                        state_name(state_id)
                    )?
                } else {
                    writeln!(
                        w,
                        "{}State::{} => parse18_runtime::LexerAcceptance::Accept({acceptance:?}),",
                        settings.type_name,
                        state_name(state_id)
                    )?
                }
            }
            (None, true) => writeln!(
                w,
                "{}State::{} => parse18_runtime::LexerAcceptance::Reject,",
                settings.type_name,
                state_name(state_id)
            )?,
            (None, false) => writeln!(
                w,
                "{}State::{} => parse18_runtime::LexerAcceptance::Pending,",
                settings.type_name,
                state_name(state_id)
            )?,
        }
    }
    emit_indent(w, base_indent + 2)?;
    writeln!(w, "}}")?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "}}")?;
    writeln!(w)?;

    emit_indent(w, base_indent + 1)?;
    writeln!(
        w,
        "pub fn step(state: {}State, c: char) -> {}State {{",
        settings.type_name, settings.type_name
    )?;
    emit_indent(w, base_indent + 2)?;
    writeln!(w, "match state {{")?;
    for (state_id, state) in dfa.states.iter().enumerate() {
        emit_indent(w, base_indent + 3)?;
        writeln!(
            w,
            "{}State::{} => {{",
            settings.type_name,
            state_name(state_id)
        )?;
        emit_transition(
            w,
            base_indent + 4,
            settings.type_name.as_str(),
            &state.transition,
        )?;
        emit_indent(w, base_indent + 3)?;
        writeln!(w, "}}")?;
    }
    emit_indent(w, base_indent + 2)?;
    writeln!(w, "}}")?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "}}")?;
    writeln!(w)?;

    emit_indent(w, base_indent)?;
    writeln!(w, "}}")?;

    writeln!(w)?;
    emit_indent(w, base_indent)?;
    writeln!(
        w,
        "impl parse18_runtime::Lexer for {} {{",
        settings.type_name
    )?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "type State = {}State;", settings.type_name)?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "type Token = {};", settings.token_type)?;
    writeln!(w)?;
    emit_indent(w, base_indent + 1)?;
    writeln!(
        w,
        "const INITIAL_STATE: Self::State = {}::INITIAL_STATE;",
        settings.type_name
    )?;
    writeln!(w)?;
    emit_indent(w, base_indent + 1)?;
    writeln!(
        w,
        "fn acceptance(state: Self::State) -> parse18_runtime::LexerAcceptance<Self::Token> {{"
    )?;
    emit_indent(w, base_indent + 2)?;
    writeln!(w, "{}::acceptance(state)", settings.type_name)?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "}}")?;
    writeln!(w)?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "fn step(state: Self::State, c: char) -> Self::State {{")?;
    emit_indent(w, base_indent + 2)?;
    writeln!(w, "{}::step(state, c)", settings.type_name)?;
    emit_indent(w, base_indent + 1)?;
    writeln!(w, "}}")?;
    emit_indent(w, base_indent)?;
    writeln!(w, "}}")?;

    if settings.module.is_some() {
        writeln!(w)?;
        writeln!(w, "}}")?;
    }

    Ok(())
}

fn emit_transition<W: Write>(
    w: &mut W,
    indent: usize,
    type_name: &str,
    transition: &DFATransition,
) -> io::Result<()> {
    match transition {
        DFATransition::Always(target) => {
            emit_indent(w, indent)?;
            writeln!(w, "{}State::{}", type_name, state_name(*target))?;
        }
        DFATransition::IfCharacter(c, a, b) => {
            emit_indent(w, indent)?;
            writeln!(w, "if c == '\\u{{{:X}}}' {{", u32::from(*c))?;
            emit_transition(w, indent + 1, type_name, a.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "}} else {{")?;
            emit_transition(w, indent + 1, type_name, b.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "}}")?;
        }
        DFATransition::IfCharacterRange(start, end, a, b) => {
            emit_indent(w, indent)?;
            writeln!(
                w,
                "if ('\\u{{{:X}}}'..='\\u{{{:X}}}').contains(&c) {{",
                u32::from(*start),
                u32::from(*end)
            )?;
            emit_transition(w, indent + 1, type_name, a.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "}} else {{")?;
            emit_transition(w, indent + 1, type_name, b.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "}}")?;
        }
        DFATransition::ForCategory {
            categories,
            fallback,
        } => {
            emit_indent(w, indent)?;
            writeln!(
                w,
                "match unicode_general_category::get_general_category(c) {{"
            )?;

            for (cats, t) in categories {
                emit_indent(w, indent + 1)?;
                for (i, cat) in cats.iter().copied().enumerate() {
                    if i > 0 {
                        write!(w, " | ")?;
                    }
                    write!(w, "{}", category_name(cat))?;
                }
                writeln!(w, " => {{")?;
                emit_transition(w, indent + 2, type_name, t)?;
                emit_indent(w, indent + 1)?;
                writeln!(w, "}}")?;
            }

            emit_indent(w, indent + 1)?;
            writeln!(w, "_ => {{")?;
            emit_transition(w, indent + 2, type_name, fallback.as_ref())?;
            emit_indent(w, indent + 1)?;
            writeln!(w, "}}")?;

            emit_indent(w, indent)?;
            writeln!(w, "}}")?;
        }
        DFATransition::IfProperty(prop, a, b) => {
            emit_indent(w, indent)?;
            writeln!(w, "if {} {{", property_condition(prop))?;
            emit_transition(w, indent + 1, type_name, a.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "}} else {{")?;
            emit_transition(w, indent + 1, type_name, b.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "}}")?;
        }
    }

    Ok(())
}

fn category_name(cat: GeneralCategory) -> &'static str {
    match cat {
        GeneralCategory::UppercaseLetter => {
            "unicode_general_category::GeneralCategory::UppercaseLetter"
        }
        GeneralCategory::LowercaseLetter => {
            "unicode_general_category::GeneralCategory::LowercaseLetter"
        }
        GeneralCategory::TitlecaseLetter => {
            "unicode_general_category::GeneralCategory::TitlecaseLetter"
        }
        GeneralCategory::ModifierLetter => {
            "unicode_general_category::GeneralCategory::ModifierLetter"
        }
        GeneralCategory::OtherLetter => "unicode_general_category::GeneralCategory::OtherLetter",
        GeneralCategory::NonspacingMark => {
            "unicode_general_category::GeneralCategory::NonspacingMark"
        }
        GeneralCategory::SpacingMark => "unicode_general_category::GeneralCategory::SpacingMark",
        GeneralCategory::EnclosingMark => {
            "unicode_general_category::GeneralCategory::EnclosingMark"
        }
        GeneralCategory::DecimalNumber => {
            "unicode_general_category::GeneralCategory::DecimalNumber"
        }
        GeneralCategory::LetterNumber => "unicode_general_category::GeneralCategory::LetterNumber",
        GeneralCategory::OtherNumber => "unicode_general_category::GeneralCategory::OtherNumber",
        GeneralCategory::MathSymbol => "unicode_general_category::GeneralCategory::MathSymbol",
        GeneralCategory::CurrencySymbol => {
            "unicode_general_category::GeneralCategory::CurrencySymbol"
        }
        GeneralCategory::ModifierSymbol => {
            "unicode_general_category::GeneralCategory::ModifierSymbol"
        }
        GeneralCategory::OtherSymbol => "unicode_general_category::GeneralCategory::OtherSymbol",
        GeneralCategory::ConnectorPunctuation => {
            "unicode_general_category::GeneralCategory::ConnectorPunctuation"
        }
        GeneralCategory::DashPunctuation => {
            "unicode_general_category::GeneralCategory::DashPunctuation"
        }
        GeneralCategory::OpenPunctuation => {
            "unicode_general_category::GeneralCategory::OpenPunctuation"
        }
        GeneralCategory::ClosePunctuation => {
            "unicode_general_category::GeneralCategory::ClosePunctuation"
        }
        GeneralCategory::InitialPunctuation => {
            "unicode_general_category::GeneralCategory::InitialPunctuation"
        }
        GeneralCategory::FinalPunctuation => {
            "unicode_general_category::GeneralCategory::FinalPunctuation"
        }
        GeneralCategory::OtherPunctuation => {
            "unicode_general_category::GeneralCategory::OtherPunctuation"
        }
        GeneralCategory::SpaceSeparator => {
            "unicode_general_category::GeneralCategory::SpaceSeparator"
        }
        GeneralCategory::LineSeparator => {
            "unicode_general_category::GeneralCategory::LineSeparator"
        }
        GeneralCategory::ParagraphSeparator => {
            "unicode_general_category::GeneralCategory::ParagraphSeparator"
        }
        GeneralCategory::Control => "unicode_general_category::GeneralCategory::Control",
        GeneralCategory::Format => "unicode_general_category::GeneralCategory::Format",
        GeneralCategory::Surrogate => "unicode_general_category::GeneralCategory::Surrogate",
        GeneralCategory::PrivateUse => "unicode_general_category::GeneralCategory::PrivateUse",
        GeneralCategory::Unassigned => "unicode_general_category::GeneralCategory::Unassigned",
        _ => panic!("Unsupported unicode category: {cat:?}"),
    }
}

fn property_condition(prop: &UnicodePropertySet) -> &'static str {
    match prop {
        UnicodePropertySet::Alphabetic => "parse18_runtime::is_alphabetic(c)",
        UnicodePropertySet::Uppercase => "parse18_runtime::is_uppercase(c)",
        UnicodePropertySet::Lowercase => "parse18_runtime::is_lowercase(c)",
        UnicodePropertySet::WhiteSpace => "parse18_runtime::is_whitespace(c)",
    }
}

fn emit_indent<W: Write>(w: &mut W, indent: usize) -> io::Result<()> {
    for _ in 0..indent {
        write!(w, "    ")?;
    }

    Ok(())
}

fn state_name(state_id: usize) -> String {
    format!("S{state_id}")
}
