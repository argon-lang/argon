use crate::grammar::{Grammar, GrammarTypes, LL1Conflict, LL1RuleType, LL1RuleValue, LL1SymbolType};
use std::io::{self, Write};


pub struct ScalaSettings {
    pub package: String,
    pub access_modifier: String,
    pub class_name: String,
    pub base_class: String,
    pub token_type: String,
    pub header_stmts: Vec<String>,
}


pub fn emit_scala<W, G>(w: &mut W, grammar: Grammar<G>, settings: &ScalaSettings) -> io::Result<()>
    where
        W: Write,
        G: GrammarTypes<ExternalFunction=String, ExternalLexMode=String, ExternalRuleType=String>,
{
    let table = match grammar.table().to_ll1() {
        Ok(table) => table,
        Err(e) => {
            eprintln!("LL1 Errors");

            for conflict in &e.conflicts {
                match *conflict {
                    LL1Conflict::FirstFirst { ruleset, rule, terminal } => {
                        let resolved_ruleset = &grammar.rulesets()[ruleset];
                        eprintln!(
                            "First/First conflict in ruleset {}.{} rule #{} with terminal {}",
                            resolved_ruleset.name(),
                            resolved_ruleset.offshoot_index(),
                            rule,
                            grammar.terminals()[terminal],
                        );
                    }
                    LL1Conflict::FirstFollow { ruleset, rule, terminal } => {
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
        },
    };

    writeln!(w, "package {}", settings.package)?;

    for stmt in &settings.header_stmts {
        writeln!(w, "{}", stmt)?;
    }

    writeln!(w, "{}abstract class {} extends {} {{", settings.access_modifier, settings.class_name, settings.base_class)?;
    
    for ruleset in table.rulesets() {
        write!(w, "  protected def {}_{}: F[", ruleset.rule_name(), ruleset.rule_offshoot_index())?;
        emit_type(w, &ruleset.rule_type())?;
        writeln!(w, "] =")?;

        write!(w, "    ")?;
        if let Some(lex_mode) = ruleset.lex_mode() {
            write!(w, "withLexMode({})(", lex_mode)?;
        }
        writeln!(w, "peek.flatMap {{")?;

        for rule in ruleset.rules() {
            let mut has_terminal = false;
            for (i, term) in rule.terminals().enumerate() {
                has_terminal = true;
                if i == 0 {
                    write!(w, "      case ")?;
                }
                else {
                    write!(w, " | ")?;
                }

                match term {
                    Some(term) => write!(w, "_: {}", term)?,
                    None => write!(w, "_: EOFToken")?,
                }
            }

            if !has_terminal {
                continue;
            }

            writeln!(w, " =>")?;

            

            let mut i: usize = 0;
            for sym in rule.symbols() {
                if i == 0 {
                    writeln!(w, "        for")?;
                }



                write!(w, "          ")?;
                if sym.discard {
                    write!(w, "_")?;
                }
                else {
                    write!(w, "x{}", i)?;
                }
                write!(w, " <- ")?;

                if sym.with_location {
                    write!(w, "withLocation(")?;
                }

                match sym.symbol_type {
                    LL1SymbolType::Terminal(t) =>
                        write!(w, "terminal[{}]", t)?,

                    LL1SymbolType::NonTerminal { name, offshoot_index } =>
                        write!(w, "{}_{}", name, offshoot_index)?,
                }


                if sym.with_location {
                    write!(w, ")")?;
                }

                writeln!(w)?;

                i += 1;
            }

            if i == 0 {
                write!(w, "        pure(")?;
            }
            else {
                write!(w, "        yield (")?;
            }

            emit_value(w, &mut i, &rule.value())?;

            write!(w, ")")?;

            writeln!(w)?;

        }

        {
            write!(w, "      case _ => error")?;
            let mut has_prev_terminal = false;
            for r in ruleset.rules() {
                for term in r.terminals() {
                    if has_prev_terminal {
                        write!(w, " | ")?;
                    }
                    else {
                        write!(w, "[")?;
                        has_prev_terminal = true;
                    }

                    match term {
                        Some(term) => {
                            write!(w, "{}", term)?;
                        },
                        None => {
                            write!(w, "EOFToken")?;
                        },
                    }
                }
            }
            if has_prev_terminal {
                write!(w, "]")?;
            }
            else {
                write!(w, "[Nothing]")?;
            }

            writeln!(w)?;
        }

        write!(w, "    }}")?;
        if ruleset.lex_mode().is_some() {
            write!(w, ")")?;
        }
        writeln!(w)?;
    }

    writeln!(w, "}}")?;

    Ok(())
}

fn emit_type<W, G>(w: &mut W, t: &LL1RuleType<G>) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction=String, ExternalLexMode=String, ExternalRuleType=String>,
{
    match t {
        LL1RuleType::ExternalType(t) => write!(w, "{}", t)?,
        LL1RuleType::TerminalType(t) => write!(w, "{}", t)?,
        LL1RuleType::Function(a, b) => {
            write!(w, "(")?;
            emit_type(w, a.as_ref())?;
            write!(w, ") => ")?;
            emit_type(w, b.as_ref())?;
        },
        LL1RuleType::Tuple(items) => {
            let mut iter = items.into_iter();
            if let Some(first) = iter.next() {
                if let Some(second) = iter.next() {
                    write!(w, "(")?;
                    emit_type(w, first)?;
                    write!(w, ", ")?;
                    emit_type(w, second)?;
                    for item in iter {
                        write!(w, ", ")?;
                        emit_type(w, item)?;
                    }
                    write!(w, ")")?;
                }
                else {
                    write!(w, "Tuple1[")?;
                    emit_type(w, first)?;
                    write!(w, "]")?;
                }
            }
            else {
                write!(w, "Unit")?;
            }
        },
        LL1RuleType::WithLocation(t) => {
            write!(w, "WithLocation[")?;
            emit_type(w, t.as_ref())?;
            write!(w, "]")?;
        },
    }

    Ok(())
}


fn emit_value<W, G>(w: &mut W, next_var_index: &mut usize, t: &LL1RuleValue<G>) -> io::Result<()>
where
    W: Write,
    G: GrammarTypes<ExternalFunction=String, ExternalLexMode=String, ExternalRuleType=String>,
{
    match t {
        LL1RuleValue::SymbolValue(i) => {
            write!(w, "x{}", i)?
        },
        LL1RuleValue::ExternalFunction(f, args) => {
            write!(w, "{}(", f)?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }

                emit_value(w, next_var_index, arg)?;
            }
            write!(w, ")")?;
        },
        LL1RuleValue::MakeTuple(items) => {
            if items.len() == 1 {
                write!(w, "Tuple1")?;
            }

            write!(w, "(")?;
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    write!(w, ", ")?;
                }

                emit_value(w, next_var_index, item)?;
            }
            write!(w, ")")?;
        },
        LL1RuleValue::GetTuple(tuple, i) => {
            emit_value(w, next_var_index, tuple)?;
            write!(w, "._{}", i + 1)?;
        },
        LL1RuleValue::DropLocation(value) => {
            emit_value(w, next_var_index, value)?;
            write!(w, ".value")?;
        },
        LL1RuleValue::BuildLocation { first, second, value } => {
            write!(w, "dev.argon.util.WithLocation(")?;
            emit_value(w, next_var_index, &*value)?;
            write!(w, ", dev.argon.util.Location.merge(")?;
            emit_value(w, next_var_index, &*first)?;
            write!(w, ".location, ")?;
            emit_value(w, next_var_index, &*second)?;
            write!(w, ".location))")?;
        },
        LL1RuleValue::Lambda { discard_param, param_type, body } => {
            let var_index = *next_var_index;
            *next_var_index += 1;

            write!(w, "(")?;
            if *discard_param {
                write!(w, "_")?;
            }
            else {
                write!(w, "x{}", var_index)?;
            }
            write!(w, ": ")?;
            emit_type(w, param_type)?;
            write!(w, ") => ")?;
            emit_value(w, next_var_index, body.as_ref())?;

            *next_var_index -= 1;
        },
        LL1RuleValue::Apply(f, a) => {
            emit_value(w, next_var_index, f.as_ref())?;
            write!(w, "(")?;
            emit_value(w, next_var_index, a.as_ref())?;
            write!(w, ")")?;
        },
    }

    Ok(())
}

