use crate::grammar::{
    Grammar, GrammarTypes, LL1Conflict, LL1RuleSet, LL1RuleType, LL1RuleValue, LL1SymbolType,
    TerminalInfo,
};
use std::io::{self, Write};

pub struct RustSettings {
    pub module: Option<String>,
    pub visibility: String,
    pub impl_group: String,
    pub token_type: String,
    pub result_type: String,
    pub location_type: String,
    pub location_ctor: String,
    pub merge_locations_fn: String,
    pub header_stmts: Vec<String>,
}

pub fn emit_rust<W, G>(w: &mut W, grammar: Grammar<G>, settings: &RustSettings) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    let table = match grammar.table().to_ll1() {
        Ok(table) => table,
        Err(e) => {
            eprintln!("LL1 Errors");

            for conflict in &e.conflicts {
                match *conflict {
                    LL1Conflict::FirstFirst {
                        ruleset,
                        rule,
                        terminal,
                    } => {
                        let resolved_ruleset = &grammar.rulesets()[ruleset];
                        eprintln!(
                            "First/First conflict in ruleset {}.{} rule #{} with terminal {}",
                            resolved_ruleset.name(),
                            resolved_ruleset.offshoot_index(),
                            rule,
                            grammar.terminals()[terminal],
                        );
                    }
                    LL1Conflict::FirstFollow {
                        ruleset,
                        rule,
                        terminal,
                    } => {
                        let resolved_ruleset = &grammar.rulesets()[ruleset];
                        eprintln!(
                            "First/Follow conflict in ruleset {}.{} rule #{} with terminal {}",
                            resolved_ruleset.name(),
                            resolved_ruleset.offshoot_index(),
                            rule,
                            grammar.terminals()[terminal],
                        );
                    }
                    LL1Conflict::FirstFollowEnd { ruleset, rule } => {
                        let resolved_ruleset = &grammar.rulesets()[ruleset];
                        eprintln!(
                            "First/Follow conflict in ruleset {}.{} rule #{} with EOF",
                            resolved_ruleset.name(),
                            resolved_ruleset.offshoot_index(),
                            rule,
                        );
                    }
                }
            }

            panic!("Encountered LL1 errors");
        }
    };

    if let Some(module) = &settings.module {
        writeln!(w, "{}mod {} {{", settings.visibility, module)?;
    }

    let base_indent = usize::from(settings.module.is_some());

    for stmt in &settings.header_stmts {
        emit_indent(w, base_indent)?;
        writeln!(w, "{stmt}")?;
    }

    if !settings.header_stmts.is_empty() {
        writeln!(w)?;
    }

    emit_indent(w, base_indent)?;
    writeln!(w, "#[allow(clippy::all, reason=\"Generated code\")]")?;
    emit_indent(w, base_indent)?;
    writeln!(w, "#[allow(clippy::pedantic, reason=\"Generated code\")]")?;

    emit_indent(w, base_indent)?;
    writeln!(w, "{} {{", settings.impl_group)?;

    for ruleset in table.rulesets() {
        emit_ruleset(w, base_indent + 1, &ruleset, settings)?;
    }

    emit_indent(w, base_indent)?;
    writeln!(w, "}}")?;

    if settings.module.is_some() {
        writeln!(w, "}}")?;
    }

    Ok(())
}

fn emit_ruleset<W, G>(
    w: &mut W,
    indent: usize,
    ruleset: &LL1RuleSet<'_, '_, G>,
    settings: &RustSettings,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    let rule_name = rule_method_name(ruleset.rule_name(), ruleset.rule_offshoot_index());

    emit_indent(w, indent)?;
    write!(
        w,
        "{} fn {}(&mut self) -> {}<",
        settings.visibility, rule_name, settings.result_type
    )?;
    emit_type(w, &ruleset.rule_type(), settings)?;
    writeln!(w, "> {{")?;

    let error_rule = ruleset.rules().find(|r| {
        r.symbols()
            .next()
            .is_some_and(|sym| matches!(sym.symbol_type, LL1SymbolType::Error))
    });

    if let Some(error_rule_value) = error_rule {
        emit_indent(w, indent + 1)?;
        writeln!(w, "let __parse18_recover_value =")?;

        if let Some(lex_mode) = ruleset.lex_mode() {
            emit_indent(w, indent + 2)?;
            writeln!(w, "self.with_lex_mode({lex_mode}, |parser| {{")?;
            emit_ruleset_body(w, indent + 3, "parser", ruleset, settings)?;
            emit_indent(w, indent + 2)?;
            writeln!(w, "}});")?;
        } else {
            emit_ruleset_body(w, indent + 2, "self", ruleset, settings)?;
            writeln!(w, ";")?;
        }

        emit_indent(w, indent + 1)?;
        writeln!(w, "self.recover_with(")?;
        emit_indent(w, indent + 2)?;
        writeln!(w, "__parse18_recover_value,")?;
        emit_indent(w, indent + 2)?;
        write!(w, "|| ")?;
        emit_value(w, &mut 0, &error_rule_value.value(), settings)?;
        writeln!(w)?;

        emit_indent(w, indent + 1)?;
        writeln!(w, ")")?;
    } else if let Some(lex_mode) = ruleset.lex_mode() {
        emit_indent(w, indent + 1)?;
        writeln!(w, "self.with_lex_mode({lex_mode}, |parser| {{")?;
        emit_ruleset_body(w, indent + 2, "parser", ruleset, settings)?;
        emit_indent(w, indent + 1)?;
        writeln!(w, "}})")?;
    } else {
        emit_ruleset_body(w, indent + 1, "self", ruleset, settings)?;
    }

    emit_indent(w, indent)?;
    writeln!(w, "}}")?;
    writeln!(w)?;

    Ok(())
}

fn emit_ruleset_body<W, G>(
    w: &mut W,
    indent: usize,
    receiver: &str,
    ruleset: &LL1RuleSet<'_, '_, G>,
    settings: &RustSettings,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    emit_indent(w, indent)?;
    writeln!(w, "match {receiver}.peek() {{")?;

    for rule in ruleset.rules() {
        if rule
            .symbols()
            .next()
            .is_some_and(|sym| matches!(sym.symbol_type, LL1SymbolType::Error))
        {
            continue;
        }

        let terminals = rule.terminals().collect::<Vec<_>>();
        if terminals.is_empty() {
            continue;
        }

        emit_indent(w, indent + 1)?;
        write!(
            w,
            "::parse18_runtime::ParseResult::Success(token) if matches!(&token.value, "
        )?;
        for (i, term) in terminals.iter().enumerate() {
            if i > 0 {
                write!(w, " | ")?;
            }
            emit_terminal_or_eof_pattern_expr::<W, G>(w, *term, settings)?;
        }
        writeln!(w, ") => {{")?;
        emit_rule_body(
            w,
            indent + 2,
            receiver,
            &ruleset.rule_type(),
            &rule.value(),
            rule.symbols(),
            settings,
            &rule_method_name(ruleset.rule_name(), ruleset.rule_offshoot_index()),
        )?;
        emit_indent(w, indent + 1)?;
        writeln!(w, "}}")?;
    }

    emit_indent(w, indent + 1)?;

    let ruleset_method_name = rule_method_name(ruleset.rule_name(), ruleset.rule_offshoot_index());
    write!(
        w,
        "::parse18_runtime::ParseResult::Success(_) => {receiver}.error(\"{ruleset_method_name}\", &["
    )?;
    let mut has_prev_terminal = false;
    for rule in ruleset.rules() {
        for term in rule.terminals() {
            if has_prev_terminal {
                write!(w, ", ")?;
            }
            has_prev_terminal = true;
            emit_terminal_or_eof_category_expr::<W, G>(w, term)?;
        }
    }
    writeln!(w, "]),")?;

    emit_indent(w, indent + 1)?;
    writeln!(
        w,
        "::parse18_runtime::ParseResult::Failure => ::parse18_runtime::ParseResult::Failure,"
    )?;

    emit_indent(w, indent)?;
    writeln!(w, "}}")?;

    Ok(())
}

fn emit_rule_body<'a, W, G>(
    w: &mut W,
    indent: usize,
    receiver: &str,
    rule_type: &LL1RuleType<G>,
    value: &LL1RuleValue<G>,
    symbols: impl IntoIterator<Item = crate::grammar::LL1Symbol<'a, G>>,
    settings: &RustSettings,
    rule_name: &str,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    let symbols = symbols.into_iter().collect::<Vec<_>>();

    if symbols.is_empty() {
        emit_indent(w, indent)?;
        writeln!(
            w,
            "{receiver}.recover_with(::parse18_runtime::ParseResult::Failure, || {{"
        )?;
        emit_indent(w, indent + 1)?;
        write!(w, "let __parse18_value: ")?;
        emit_type(w, rule_type, settings)?;
        write!(w, " = ")?;
        emit_value(w, &mut 0, value, settings)?;
        writeln!(w, ";")?;
        emit_indent(w, indent + 1)?;
        writeln!(w, "__parse18_value")?;
        emit_indent(w, indent)?;
        write!(w, "}})")?;
        return Ok(());
    }

    emit_rule_body_chain(
        w, indent, receiver, rule_type, value, &symbols, settings, rule_name, 0,
    )?;

    writeln!(w)?;

    Ok(())
}

fn emit_rule_body_chain<'a, W, G>(
    w: &mut W,
    indent: usize,
    receiver: &str,
    rule_type: &LL1RuleType<G>,
    value: &LL1RuleValue<G>,
    symbols: &[crate::grammar::LL1Symbol<'a, G>],
    settings: &RustSettings,
    rule_name: &str,
    symbol_index: usize,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    let sym = &symbols[symbol_index];
    let var_name = if sym.discard {
        format!("_x{symbol_index}")
    } else {
        format!("x{symbol_index}")
    };

    emit_indent(w, indent)?;
    write!(w, "(")?;
    emit_symbol_parse_expr(
        w,
        indent + 1,
        receiver,
        sym,
        settings,
        rule_name,
        symbol_index,
    )?;

    if symbol_index + 1 == symbols.len() {
        writeln!(w, ").map(move |{var_name}| {{")?;
        emit_indent(w, indent + 1)?;
        write!(w, "let __parse18_value: ")?;
        emit_type(w, rule_type, settings)?;
        write!(w, " = ")?;
        let mut next_var_index = symbols.len();
        emit_value(w, &mut next_var_index, value, settings)?;
        writeln!(w, ";")?;
        emit_indent(w, indent + 1)?;
        write!(w, "__parse18_value")?;
        writeln!(w)?;
        emit_indent(w, indent)?;
        write!(w, "}})")?;
    } else {
        writeln!(w, ").flat_map(|{var_name}| {{")?;
        emit_rule_body_chain(
            w,
            indent + 1,
            receiver,
            rule_type,
            value,
            symbols,
            settings,
            rule_name,
            symbol_index + 1,
        )?;
        writeln!(w)?;
        emit_indent(w, indent)?;
        write!(w, "}})")?;
    }

    Ok(())
}

fn emit_symbol_parse_expr<'a, W, G>(
    w: &mut W,
    indent: usize,
    receiver: &str,
    sym: &crate::grammar::LL1Symbol<'a, G>,
    settings: &RustSettings,
    rule_name: &str,
    symbol_index: usize,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    if sym.with_location {
        writeln!(w, "{{")?;
        emit_indent(w, indent)?;
        write!(w, "let __parse18_with_location_{symbol_index} = ")?;
        emit_symbol_parse_expr_inner(
            w,
            indent + 1,
            receiver,
            &sym.symbol_type,
            settings,
            rule_name,
        )?;
        writeln!(w, ";")?;
        emit_indent(w, indent)?;
        writeln!(
            w,
            "{receiver}.with_location(__parse18_with_location_{symbol_index})"
        )?;
        emit_indent(w, indent - 1)?;
        write!(w, "}}")?;
    } else {
        emit_symbol_parse_expr_inner(w, indent, receiver, &sym.symbol_type, settings, rule_name)?;
    }

    Ok(())
}

fn emit_symbol_parse_expr_inner<W, G>(
    w: &mut W,
    indent: usize,
    receiver: &str,
    sym_type: &LL1SymbolType<'_, G>,
    settings: &RustSettings,
    rule_name: &str,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    match sym_type {
        LL1SymbolType::Terminal(term) => {
            writeln!(w, "match {receiver}.parser_next() {{")?;
            emit_indent(w, indent)?;
            if (**term).has_payload() {
                write!(
                    w,
                    "::parse18_runtime::ParseResult::Success(::parse18_runtime::WithRange {{ value: {}::{}(__parse18_payload), range }}) => ::parse18_runtime::ParseResult::Success(::parse18_runtime::WithRange {{ value: __parse18_payload, range }}),",
                    settings.token_type,
                    terminal_category_name::<G>(term),
                )?;
                writeln!(w)?;
            } else {
                write!(
                    w,
                    "::parse18_runtime::ParseResult::Success(token @ ::parse18_runtime::WithRange {{ value: "
                )?;
                emit_terminal_pattern_expr::<W, G>(w, term, settings)?;
                writeln!(
                    w,
                    ", .. }}) => ::parse18_runtime::ParseResult::Success(token),"
                )?;
            }

            emit_indent(w, indent)?;
            write!(w, "_ => {receiver}.error(\"{rule_name}\", &[")?;
            emit_terminal_category_expr::<W, G>(w, term)?;
            writeln!(w, "]),")?;
            emit_indent(w, indent - 1)?;
            write!(w, "}}")?;
        }
        LL1SymbolType::NonTerminal {
            name,
            offshoot_index,
        } => write!(
            w,
            "{receiver}.{}()",
            rule_method_name(name, *offshoot_index)
        )?,
        LL1SymbolType::Error => panic!("Encountered error symbol in LL1 table"),
    }

    Ok(())
}

fn emit_type<W, G>(w: &mut W, t: &LL1RuleType<G>, settings: &RustSettings) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    match t {
        LL1RuleType::ExternalType(t) => write!(w, "{t}")?,
        LL1RuleType::TerminalType(term) => {
            if (**term).has_payload() {
                write!(
                    w,
                    "{}",
                    (**term)
                        .payload_type()
                        .expect("payload terminals must define payload_type")
                )?;
            } else {
                write!(w, "{}", settings.token_type)?;
            }
        }
        LL1RuleType::Function(a, b) => {
            write!(w, "Box<dyn FnOnce(")?;
            emit_type(w, a.as_ref(), settings)?;
            write!(w, ") -> ")?;
            emit_type(w, b.as_ref(), settings)?;
            write!(w, ">")?;
        }
        LL1RuleType::Tuple(items) => match items.as_slice() {
            [] => write!(w, "()")?,
            [first] => {
                write!(w, "(")?;
                emit_type(w, first, settings)?;
                write!(w, ",)")?;
            }
            _ => {
                write!(w, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    emit_type(w, item, settings)?;
                }
                write!(w, ")")?;
            }
        },
        LL1RuleType::WithLocation(t) => {
            write!(w, "{}<", settings.location_type)?;
            emit_type(w, t.as_ref(), settings)?;
            write!(w, ">")?;
        }
    }

    Ok(())
}

fn emit_value<W, G>(
    w: &mut W,
    next_var_index: &mut usize,
    t: &LL1RuleValue<G>,
    settings: &RustSettings,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
    G::Terminal: 'static,
{
    match t {
        LL1RuleValue::SymbolValue(i) => write!(w, "x{i}")?,
        LL1RuleValue::ExternalFunction(f, args) => {
            if args.is_empty() {
                write!(w, "{f}()")?;
            } else {
                write!(w, "{f}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    emit_value(w, next_var_index, arg, settings)?;
                }
                write!(w, ")")?;
            }
        }
        LL1RuleValue::MakeTuple(items) => match items.as_slice() {
            [] => write!(w, "()")?,
            [first] => {
                write!(w, "(")?;
                emit_value(w, next_var_index, first, settings)?;
                write!(w, ",)")?;
            }
            _ => {
                write!(w, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(w, ", ")?;
                    }
                    emit_value(w, next_var_index, item, settings)?;
                }
                write!(w, ")")?;
            }
        },
        LL1RuleValue::GetTuple(tuple, i) => {
            write!(w, "(")?;
            emit_value(w, next_var_index, tuple, settings)?;
            write!(w, ").{i}")?;
        }
        LL1RuleValue::DropLocation(value) => {
            write!(w, "(")?;
            emit_value(w, next_var_index, value, settings)?;
            write!(w, ").value")?;
        }
        LL1RuleValue::BuildLocation {
            first,
            second,
            value,
        } => {
            write!(w, "{{ let __parse18_first = (")?;
            emit_value(w, next_var_index, first, settings)?;
            write!(w, ").location.clone(); let __parse18_second = (")?;
            emit_value(w, next_var_index, second, settings)?;
            write!(w, ").location.clone(); {}(", settings.location_ctor)?;
            emit_value(w, next_var_index, value, settings)?;
            write!(
                w,
                ", {}(__parse18_first, __parse18_second)) }}",
                settings.merge_locations_fn
            )?;
        }
        LL1RuleValue::Lambda {
            discard_param,
            param_type,
            body,
        } => {
            let var_index = *next_var_index;
            *next_var_index += 1;

            write!(w, "Box::new(move |")?;
            if *discard_param {
                write!(w, "_")?;
            } else {
                write!(w, "x{var_index}")?;
            }
            write!(w, ": ")?;
            emit_type(w, param_type, settings)?;
            write!(w, "| ")?;
            emit_value(w, next_var_index, body.as_ref(), settings)?;
            write!(w, ")")?;

            *next_var_index -= 1;
        }
        LL1RuleValue::Apply(f, a) => {
            write!(w, "(")?;
            emit_value(w, next_var_index, f.as_ref(), settings)?;
            write!(w, ")(")?;
            emit_value(w, next_var_index, a.as_ref(), settings)?;
            write!(w, ")")?;
        }
    }

    Ok(())
}

fn emit_indent<W: Write>(w: &mut W, indent: usize) -> io::Result<()> {
    for _ in 0..indent {
        write!(w, "    ")?;
    }
    Ok(())
}

fn emit_terminal_or_eof_category_expr<W, G>(w: &mut W, term: Option<&G::Terminal>) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
{
    match term {
        Some(term) => emit_terminal_category_expr::<W, G>(w, term)?,
        None => write!(
            w,
            "<Self as ::parse18_runtime::ParserRuntime>::TokenCategory::EndOfFile"
        )?,
    }

    Ok(())
}

fn emit_terminal_or_eof_pattern_expr<W, G>(
    w: &mut W,
    term: Option<&G::Terminal>,
    settings: &RustSettings,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
{
    match term {
        Some(term) => {
            write!(w, "&")?;
            emit_terminal_pattern_expr::<W, G>(w, term, settings)?;
        }
        None => write!(w, "&{}::EndOfFile", settings.token_type)?,
    }

    Ok(())
}

fn emit_terminal_pattern_expr<W, G>(
    w: &mut W,
    term: &G::Terminal,
    settings: &RustSettings,
) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
{
    write!(
        w,
        "{}::{}",
        settings.token_type,
        terminal_category_name::<G>(term)
    )?;
    if (*term).has_payload() {
        write!(w, "(..)")?;
    }

    Ok(())
}

fn emit_terminal_category_expr<W, G>(w: &mut W, term: &G::Terminal) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
{
    write!(
        w,
        "<Self as ::parse18_runtime::ParserRuntime>::TokenCategory::{}",
        terminal_category_name::<G>(term)
    )?;
    Ok(())
}

fn terminal_category_name<G>(term: &G::Terminal) -> String
where
    G: GrammarTypes<ExternalFunction = String, ExternalLexMode = String, ExternalRuleType = String>,
{
    term.to_string()
}

fn rule_method_name(name: &str, offshoot_index: usize) -> String {
    format!("{}_{}", to_snake_case(name), offshoot_index)
}

fn to_snake_case(value: &str) -> String {
    let mut result = String::with_capacity(value.len());
    let mut chars = value.chars().peekable();
    let mut prev_is_lower_or_digit = false;
    let mut prev_is_upper = false;

    while let Some(ch) = chars.next() {
        if ch.is_ascii_alphanumeric() {
            if ch.is_ascii_uppercase() {
                let next_is_lower = chars.peek().is_some_and(char::is_ascii_lowercase);
                if !result.is_empty()
                    && !result.ends_with('_')
                    && (prev_is_lower_or_digit || (prev_is_upper && next_is_lower))
                {
                    result.push('_');
                }
                result.push(ch.to_ascii_lowercase());
                prev_is_lower_or_digit = false;
                prev_is_upper = true;
            } else {
                result.push(ch);
                prev_is_lower_or_digit = true;
                prev_is_upper = false;
            }
        } else if !result.is_empty() && !result.ends_with('_') {
            result.push('_');
            prev_is_lower_or_digit = false;
            prev_is_upper = false;
        }
    }

    while result.ends_with('_') {
        result.pop();
    }

    result
}
