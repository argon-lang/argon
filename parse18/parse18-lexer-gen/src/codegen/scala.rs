
use std::{io::Write, io};

use unicode_general_category::GeneralCategory;

use crate::fsm::{DFATransition, DFA};
use crate::regex::UnicodePropertySet;

pub struct ScalaSettings {
    pub package: String,
    pub access_modifier: String,
    pub object_name: String,
    pub token_type: String,

}


pub fn emit_scala<W: Write>(w: &mut W, dfa: DFA<String>, settings: &ScalaSettings) -> io::Result<()> {
    writeln!(w, "package {}", settings.package)?;
    writeln!(w, "{}object {} {{", settings.access_modifier, settings.object_name)?;
    writeln!(w, "  opaque type State = Int")?;
    writeln!(w, "  val initialState: State = 0")?;

    writeln!(w, "  def isAccept(state: State): Boolean =")?;
    {
        let mut iter = dfa.states.iter()
            .enumerate()
            .filter(|(_, state)| state.acceptance.is_some());

        if let Some((state_id, _)) = iter.next() {
            writeln!(w, "    state match {{")?;
            write!(w, "      case {}", state_id)?;
            for (state_id, _) in iter {
                write!(w, " | {}", state_id)?;
            }
            writeln!(w, " => true")?;
            writeln!(w, "      case _ => false")?;
            writeln!(w, "    }}")?;
        }
        else {
            writeln!(w, "    false")?;
        }
    }


    writeln!(w, "  def acceptance(state: State): {} =", settings.token_type)?;
    writeln!(w, "    state match {{")?;
    for (state_id, state) in dfa.states.iter().enumerate() {
        let Some(acceptance) = &state.acceptance else { continue; };
        writeln!(w, "      case {} => {}", state_id, acceptance)?;
    }
    writeln!(w, "      case _ => throw new RuntimeException(\"Invalid state\")")?;
    writeln!(w, "    }}")?;


    writeln!(w, "  def isReject(state: State): Boolean =")?;
    {
        let mut iter = dfa.states.iter()
            .enumerate()
            .filter(|(_, state)| state.is_reject);

        if let Some((state_id, _)) = iter.next() {
            writeln!(w, "    state match {{")?;
            write!(w, "      case {}", state_id)?;
            for (state_id, _) in iter {
                write!(w, " | {}", state_id)?;
            }
            writeln!(w, " => true")?;
            writeln!(w, "      case _ => false")?;
            writeln!(w, "    }}")?;
        }
        else {
            writeln!(w, "    false")?;
        }
    }

    writeln!(w, "  def step(state: State, c: Int): State =")?;
    writeln!(w, "    state match {{")?;
    for (state_id, state) in dfa.states.iter().enumerate() {
        writeln!(w, "      case {} =>", state_id)?;

        emit_transition(w, 4, &state.transition)?;
    }
    writeln!(w, "      case _ => throw new RuntimeException(\"Unknown state\")")?;
    writeln!(w, "    }}")?;



    writeln!(w, "  private def isWhitespace(c: Int): Boolean =")?;
    writeln!(w, "    val t = java.lang.Character.getType(c)")?;
    writeln!(w, "    t == java.lang.Character.SPACE_SEPARATOR ||")?;
    writeln!(w, "      t == java.lang.Character.LINE_SEPARATOR ||")?;
    writeln!(w, "      t == java.lang.Character.PARAGRAPH_SEPARATOR ||")?;
    writeln!(w, "      (c >= 0x9 && c <= 0xD) ||")?;
    writeln!(w, "      c == 0x85")?;
    writeln!(w, "  end isWhitespace")?;


    writeln!(w, "}}")?;

    Ok(())
}

fn emit_transition<W: Write>(w: &mut W, indent: u32, transition: &DFATransition) -> io::Result<()> {
    match transition {
        DFATransition::Always(target) => {
            emit_indent(w, indent)?;
            writeln!(w, "{}", target)?;
        },
        DFATransition::IfCharacter(c, a, b) => {
            emit_indent(w, indent)?;
            writeln!(w, "if c == 0x{:X} then", u32::from(*c) as i32)?;
            emit_transition(w, indent + 1, a.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "else")?;
            emit_transition(w, indent + 1, b.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "end if")?;
        },
        DFATransition::IfCharacterRange(start, end, a, b) => {
            emit_indent(w, indent)?;
            writeln!(w, "if c >= 0x{:X} && c <= 0x{:X} then", u32::from(*start) as i32, u32::from(*end) as i32)?;
            emit_transition(w, indent + 1, a.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "else")?;
            emit_transition(w, indent + 1, b.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "end if")?;
        },

        DFATransition::ForCategory { categories, fallback } => {
            emit_indent(w, indent)?;
            writeln!(w, "java.lang.Character.getType(c) match {{")?;

            for (cats, t) in categories {
                emit_indent(w, indent + 1)?;
                write!(w, "case ")?;
                for (i, cat) in cats.iter().copied().enumerate() {
                    if i > 0 {
                        write!(w, " | ")?;
                    }

                    let cat_name = match cat {
                        GeneralCategory::UppercaseLetter => "UPPERCASE_LETTER",
                        GeneralCategory::LowercaseLetter => "LOWERCASE_LETTER",
                        GeneralCategory::TitlecaseLetter => "TITLECASE_LETTER",
                        GeneralCategory::ModifierLetter => "MODIFIER_LETTER",
                        GeneralCategory::OtherLetter => "OTHER_LETTER",

                        GeneralCategory::NonspacingMark => "NON_SPACING_MARK",
                        GeneralCategory::SpacingMark => "COMBINING_SPACING_MARK",
                        GeneralCategory::EnclosingMark => "ENCLOSING_MARK",

                        GeneralCategory::DecimalNumber => "DECIMAL_DIGIT_NUMBER",
                        GeneralCategory::LetterNumber => "LETTER_NUMBER",
                        GeneralCategory::OtherNumber => "OTHER_NUMBER",

                        GeneralCategory::MathSymbol => "MATH_SYMBOL",
                        GeneralCategory::CurrencySymbol => "CURRENCY_SYMBOL",
                        GeneralCategory::ModifierSymbol => "MODIFIER_SYMBOL",
                        GeneralCategory::OtherSymbol => "OTHER_SYMBOL",

                        GeneralCategory::ConnectorPunctuation => "CONNECTOR_PUNCTUATION",
                        GeneralCategory::DashPunctuation => "DASH_PUNCTUATION",
                        GeneralCategory::OpenPunctuation => "START_PUNCTUATION",
                        GeneralCategory::ClosePunctuation => "END_PUNCTUATION",
                        GeneralCategory::InitialPunctuation => "INITIAL_QUOTE_PUNCTUATION",
                        GeneralCategory::FinalPunctuation => "FINAL_QUOTE_PUNCTUATION",
                        GeneralCategory::OtherPunctuation => "OTHER_PUNCTUATION",

                        GeneralCategory::SpaceSeparator => "SPACE_SEPARATOR",
                        GeneralCategory::LineSeparator => "LINE_SEPARATOR",
                        GeneralCategory::ParagraphSeparator => "PARAGRAPH_SEPARATOR",

                        GeneralCategory::Control => "CONTROL",
                        GeneralCategory::Format => "FORMAT",
                        GeneralCategory::Surrogate => "SURROGATE",
                        GeneralCategory::PrivateUse => "PRIVATE_USE",
                        GeneralCategory::Unassigned => "UNASSIGNED",
                        _ => panic!("Unsupported unicode category: {cat:?}"),
                    };

                    write!(w, "java.lang.Character.{}", cat_name)?;
                }

                writeln!(w, " =>")?;
                emit_transition(w, indent + 2, t)?;
            }

            
            emit_indent(w, indent + 1)?;
            writeln!(w, "case _ =>")?;
            emit_transition(w, indent + 2, fallback.as_ref())?;


            emit_indent(w, indent)?;
            writeln!(w, "}}")?;
        },

        DFATransition::IfProperty(prop, a, b) => {
            emit_indent(w, indent)?;
            let cond = match prop {
                UnicodePropertySet::Alphabetic => "java.lang.Character.isAlphabetic(c)",
                UnicodePropertySet::Uppercase => "java.lang.Character.isUpperCase(c)",
                UnicodePropertySet::Lowercase => "java.lang.Character.isLowerCase(c)",
                UnicodePropertySet::WhiteSpace => "isWhitespace(c)",
            };
            writeln!(w, "if {} then", cond)?;
            emit_transition(w, indent + 1, a.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "else")?;
            emit_transition(w, indent + 1, b.as_ref())?;
            emit_indent(w, indent)?;
            writeln!(w, "end if")?;
        },
    }

    Ok(())
}

fn emit_indent<W: Write>(w: &mut W, indent: u32) -> io::Result<()> {
    for _ in 0..indent {
        write!(w, "  ")?;
    }

    Ok(())
}
