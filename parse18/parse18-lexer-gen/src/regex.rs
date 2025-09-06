use std::ops::{Add, BitOr};

use regex_syntax::ast as rast;
use unicode_general_category::GeneralCategory;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Regex {
    Empty,
    CharClassSet(CharClassSet),
    Alternative(Vec<Regex>),
    Concat(Vec<Regex>),
    Repeat(Box<Regex>),
}



impl Add for Regex {
    type Output = Regex;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Regex::Empty, rhs) => rhs,
            (lhs, Regex::Empty) => lhs,
            (Regex::Concat(mut a), Regex::Concat(b)) => {
                a.extend(b);
                Regex::Concat(a)
            },
            (Regex::Concat(mut a), rhs) => {
                a.push(rhs);
                Regex::Concat(a)
            },
            (lhs, Regex::Concat(mut b)) => {
                b.insert(0, lhs);
                Regex::Concat(b)
            },
            (lhs, rhs) => {
                Regex::Concat(vec!(lhs, rhs))
            },
        }
    }
}

impl BitOr for Regex {
    type Output = Regex;

    fn bitor(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Regex::Alternative(mut a), Regex::Concat(b)) => {
                a.extend(b);
                Regex::Alternative(a)
            },
            (Regex::Alternative(mut a), rhs) => {
                a.push(rhs);
                Regex::Alternative(a)
            },
            (lhs, Regex::Alternative(mut b)) => {
                b.insert(0, lhs);
                Regex::Alternative(b)
            },
            (lhs, rhs) => {
                Regex::Alternative(vec!(lhs, rhs))
            },
        }
    }
}

impl Regex {
    pub fn optional(self) -> Self {
        Regex::Alternative(vec![ Regex::Empty, self ])
    }

    pub fn repeat(self) -> Self {
        Regex::Repeat(Box::new(self))
    }
    
    pub fn repeat1(self) -> Self {
        self.clone() + Regex::Repeat(Box::new(self))
    }

    pub fn repeat_exactly(self, m: u32) -> Self {
        let mut parts = (0..m).map(|_| self.clone()).collect::<Vec<_>>();
        if parts.is_empty() {
            Regex::Empty
        }
        else if parts.len() == 1 {
            parts.swap_remove(0)
        }
        else {
            Regex::Concat(parts)
        }
    }

    pub fn repeat_at_least(self, m: u32) -> Self {
        let mut parts = (0..m).map(|_| self.clone()).collect::<Vec<_>>();
        parts.push(self.repeat());
        Regex::Concat(parts)
    }

    pub fn repeat_bounded(self, m: u32, n: u32) -> Self {
        let r = self.clone().repeat_exactly(m);

        let mut optional_part = Regex::Empty;
        for _ in m..=n {
            optional_part = (self.clone() + optional_part).optional();
        }

        r + optional_part
    }

    pub fn str(s: &str) -> Self {
        let parts = s.chars().map(|c| {
            Regex::CharClassSet(
                CharClassSet::Class(CharClass::Char(c))
            )
        }).collect::<Vec<_>>();
        if parts.is_empty() {
            Regex::Empty
        }
        else {
            Regex::Concat(parts)
        }
    }

    pub fn pattern(s: &str) -> Self {
        let expr = rast::parse::Parser::new().parse(s).unwrap();
        Self::from_regex_ast(&expr)
    }

    fn from_regex_ast(expr: &rast::Ast) -> Self {
        use rast::*;
        match expr {
            Ast::Empty(_) => Regex::Empty,
            Ast::Literal(literal) => Regex::CharClassSet(
                CharClassSet::Class(CharClass::Char(literal.c))
            ),
            Ast::Dot(_) => Regex::CharClassSet(CharClassSet::Class(CharClass::Any)),

            Ast::ClassUnicode(class_unicode) => {
                Regex::CharClassSet(CharClassSet::from_regex_unicode_class(class_unicode))
            },

            Ast::ClassPerl(class_perl) => {
                Regex::CharClassSet(CharClassSet::from_regex_perl_class(class_perl))
            },

            Ast::ClassBracketed(class_bracketed) => {
                Regex::CharClassSet(CharClassSet::from_regex_bracketed(class_bracketed))
            },

            Ast::Repetition(repetition) => {
                if !repetition.greedy {
                    panic!("Non-greedy regex is not supported");
                }

                let inner = Self::from_regex_ast(&repetition.ast);

                match &repetition.op.kind {
                    RepetitionKind::ZeroOrOne => inner.optional(),
                    RepetitionKind::ZeroOrMore => inner.repeat(),
                    RepetitionKind::OneOrMore => inner.repeat1(),
                    RepetitionKind::Range(RepetitionRange::Exactly(m)) =>
                        inner.repeat_exactly(*m),
                    RepetitionKind::Range(RepetitionRange::AtLeast(m)) =>
                        inner.repeat_at_least(*m),

                    RepetitionKind::Range(RepetitionRange::Bounded(m, n)) =>
                        inner.repeat_bounded(*m, *n),
                }
            },

            Ast::Group(group) => Self::from_regex_ast(&group.ast),

            Ast::Alternation(alternation) =>
                Regex::Alternative(alternation.asts.iter().map(Self::from_regex_ast).collect()),

            Ast::Concat(concat) =>
                Regex::Concat(concat.asts.iter().map(Self::from_regex_ast).collect()),

            _ => panic!("Unsupported regex syntax: {:?}", expr),
        }
    }
}




#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CharClass {
    Any,
    Char(char),
    Range(char, char),
    Category(GeneralCategory),
    Property(UnicodePropertySet),
}



impl CharClass {
    pub fn contains_char(self, c: char) -> bool {
        match self {
            CharClass::Any => true,
            CharClass::Char(c2) => c == c2,
            CharClass::Range(a, b) => a <= c && c <= b,
            CharClass::Category(cat) => unicode_general_category::get_general_category(c) == cat,
            CharClass::Property(prop) => prop.contains_char(c),
        }
    }

    pub fn implies(self, other: Self) -> bool {
        match (self, other) {
            (_, CharClass::Any) => true,
            (CharClass::Any, _) => false,
            (CharClass::Char(c), _) => other.contains_char(c),
            (_, CharClass::Char(_)) => false,

            (CharClass::Range(a1, a2), CharClass::Range(b1, b2)) => a1 <= b1 && b2 <= a2,
            (CharClass::Range(a1, a2), _) => (a1..=a2).all(|c| other.contains_char(c)),
            (_, CharClass::Range(_, _)) => false,

            (CharClass::Category(a), CharClass::Category(b)) => a == b,
            (CharClass::Category(_), _) | (_, CharClass::Category(_)) => false,

            (CharClass::Property(a), CharClass::Property(b)) => a.implies(b),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnicodePropertySet {
    Alphabetic,
    Uppercase,
    Lowercase,
    WhiteSpace,
}

impl UnicodePropertySet {
    pub fn contains_char(self, c: char) -> bool {
        match self {
            UnicodePropertySet::Alphabetic => c.is_alphabetic(),
            UnicodePropertySet::Uppercase => c.is_uppercase(),
            UnicodePropertySet::Lowercase => c.is_lowercase(),
            UnicodePropertySet::WhiteSpace => c.is_whitespace(),
        }
    }

    pub fn implies(self, other: Self) -> bool {
        match (self, other) {
            (UnicodePropertySet::Uppercase, UnicodePropertySet::Alphabetic) => true,
            (UnicodePropertySet::Lowercase, UnicodePropertySet::Alphabetic) => true,
            (_, _) => self == other,
        }
    }

    pub fn implies_not(self, other: Self) -> bool {
        match (self, other) {
            (UnicodePropertySet::WhiteSpace, UnicodePropertySet::Alphabetic) => true,
            (UnicodePropertySet::WhiteSpace, UnicodePropertySet::Uppercase) => true,
            (UnicodePropertySet::WhiteSpace, UnicodePropertySet::Lowercase) => true,

            (UnicodePropertySet::Lowercase, UnicodePropertySet::Uppercase) => true,
            (UnicodePropertySet::Uppercase, UnicodePropertySet::Lowercase) => true,

            (UnicodePropertySet::Alphabetic, UnicodePropertySet::WhiteSpace) => true,
            (UnicodePropertySet::Uppercase, UnicodePropertySet::WhiteSpace) => true,
            (UnicodePropertySet::Lowercase, UnicodePropertySet::WhiteSpace) => true,

            (UnicodePropertySet::Alphabetic, UnicodePropertySet::Uppercase) => false,
            (UnicodePropertySet::Alphabetic, UnicodePropertySet::Lowercase) => false,
            (UnicodePropertySet::Uppercase, UnicodePropertySet::Alphabetic) => false,
            (UnicodePropertySet::Lowercase, UnicodePropertySet::Alphabetic) => false,

            (UnicodePropertySet::Alphabetic, UnicodePropertySet::Alphabetic) => false,
            (UnicodePropertySet::Uppercase, UnicodePropertySet::Uppercase) => false,
            (UnicodePropertySet::Lowercase, UnicodePropertySet::Lowercase) => false,
            (UnicodePropertySet::WhiteSpace, UnicodePropertySet::WhiteSpace) => false,
        }
    }
}



#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CharClassSet {
    Empty,
    Class(CharClass),
    Not(Box<CharClassSet>),
    Union(Box<CharClassSet>, Box<CharClassSet>),
    Intersection(Box<CharClassSet>, Box<CharClassSet>),
}

impl CharClassSet {
    fn from_regex_unicode_class(class_unicode: &rast::ClassUnicode) -> Self {
        let mut class = match &class_unicode.kind {
            rast::ClassUnicodeKind::OneLetter(letter) => {
                unicode_category_group(&letter.to_uppercase().to_string())    
            },
            rast::ClassUnicodeKind::Named(name) => {
                let name = normalize_prop(&name);
                binary_property(&name)
                    .or_else(|| unicode_category_group(&name))
                    .or_else(|| unicode_category(&name))
            },
            rast::ClassUnicodeKind::NamedValue { op, name, value } => {
                let name = normalize_prop(name);
                let value = normalize_prop(value);

                let mut class = match name.as_str() {
                    "General_Category" =>
                        unicode_category_group(&value)
                            .or_else(|| unicode_category(&name)),

                    _ => None,
                };

                if *op == rast::ClassUnicodeOpKind::NotEqual {
                    class = class.map(|c| CharClassSet::Not(Box::new(c)));
                }

                class
            },
        }.unwrap_or_else(|| panic!("Unknown unicode class: {:?}", &class_unicode.kind));

        if class_unicode.negated {
            class = CharClassSet::Not(Box::new(class));
        }

        class
    }

    fn from_regex_perl_class(class_perl: &rast::ClassPerl) -> Self {
        use rast::*;
        let mut class = match &class_perl.kind {
            ClassPerlKind::Digit => CharClassSet::Class(CharClass::Category(GeneralCategory::DecimalNumber)),
            ClassPerlKind::Space => CharClassSet::Class(CharClass::Property(UnicodePropertySet::WhiteSpace)),
            ClassPerlKind::Word => [
                CharClassSet::Class(CharClass::Property(UnicodePropertySet::Alphabetic)),
                unicode_category_group("M").unwrap(),
                CharClassSet::Class(CharClass::Category(GeneralCategory::DecimalNumber)),
                unicode_category("Pc").unwrap(),
                CharClassSet::Class(CharClass::Range('\u{202C}', '\u{202D}')),
            ]
                .into_iter()
                .reduce(|a, b| CharClassSet::Union(Box::new(a), Box::new(b)))
                .unwrap(),
        };

        if class_perl.negated {
            class = CharClassSet::Not(Box::new(class));
        }

        class
    }

    fn from_regex_ascii_class(class_ascii: &rast::ClassAscii) -> Self {
        match &class_ascii.kind {
            rast::ClassAsciiKind::Alnum => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Range('0', '9'))),
                Box::new(CharClassSet::Union(
                    Box::new(CharClassSet::Class(CharClass::Range('A', 'Z'))),
                    Box::new(CharClassSet::Class(CharClass::Range('a', 'z'))),
                )),
            ),
            rast::ClassAsciiKind::Alpha => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Range('A', 'Z'))),
                Box::new(CharClassSet::Class(CharClass::Range('a', 'z'))),
            ),
            rast::ClassAsciiKind::Ascii => CharClassSet::Class(CharClass::Range('\0', '\x7F')),
            rast::ClassAsciiKind::Blank => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Char(' '))),
                Box::new(CharClassSet::Class(CharClass::Char('\t'))),
            ),
            rast::ClassAsciiKind::Cntrl => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Range('\x00', '\x1F'))),
                Box::new(CharClassSet::Class(CharClass::Char('\x7F'))),
            ),
            rast::ClassAsciiKind::Digit => CharClassSet::Class(CharClass::Range('0', '9')),
            rast::ClassAsciiKind::Graph => CharClassSet::Class(CharClass::Range('!', '~')),
            rast::ClassAsciiKind::Lower => CharClassSet::Class(CharClass::Range('a', 'z')),
            rast::ClassAsciiKind::Print => CharClassSet::Class(CharClass::Range(' ', '~')),
            rast::ClassAsciiKind::Punct => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Range('!', '/'))),
                Box::new(CharClassSet::Union(
                    Box::new(CharClassSet::Class(CharClass::Range(':', '@'))),
                    Box::new(CharClassSet::Union(
                        Box::new(CharClassSet::Class(CharClass::Range('\\', '`'))),
                        Box::new(CharClassSet::Class(CharClass::Range('{', ']'))),
                    )),
                )),
            ),
            rast::ClassAsciiKind::Space => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Char('\t'))),
                Box::new(CharClassSet::Union(
                    Box::new(CharClassSet::Class(CharClass::Char('\n'))),
                    Box::new(CharClassSet::Union(
                        Box::new(CharClassSet::Class(CharClass::Char('\x0B'))),
                        Box::new(CharClassSet::Union(
                            Box::new(CharClassSet::Class(CharClass::Char('\x0C'))),
                            Box::new(CharClassSet::Union(
                                Box::new(CharClassSet::Class(CharClass::Char('\r'))),
                                Box::new(CharClassSet::Class(CharClass::Char(' '))),
                            )),
                        )),
                    )),
                )),
            ),
            rast::ClassAsciiKind::Upper => CharClassSet::Class(CharClass::Range('A', 'Z')),
            rast::ClassAsciiKind::Word => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Char('_'))),
                Box::new(CharClassSet::Union(
                    Box::new(CharClassSet::Class(CharClass::Range('0', '9'))),
                    Box::new(CharClassSet::Union(
                        Box::new(CharClassSet::Class(CharClass::Range('A', 'Z'))),
                        Box::new(CharClassSet::Class(CharClass::Range('a', 'z'))),
                    )),
                )),
            ),
            rast::ClassAsciiKind::Xdigit => CharClassSet::Union(
                Box::new(CharClassSet::Class(CharClass::Range('0', '9'))),
                Box::new(CharClassSet::Union(
                    Box::new(CharClassSet::Class(CharClass::Range('A', 'F'))),
                    Box::new(CharClassSet::Class(CharClass::Range('a', 'f'))),
                )),
            ),
        }
    }

    fn from_regex_bracketed(class_bracketed: &rast::ClassBracketed) -> Self {
        let mut class = CharClassSet::from_regex_class_set(&class_bracketed.kind);

        if class_bracketed.negated {
            class = CharClassSet::Not(Box::new(class));
        }

        class
    }

    fn from_regex_class_set_item(item: &rast::ClassSetItem) -> Self {
        use rast::*;
        match item {
            ClassSetItem::Empty(_) => CharClassSet::Empty,

            ClassSetItem::Literal(literal) =>
                CharClassSet::Class(CharClass::Char(literal.c)),
                
            ClassSetItem::Range(class_set_range) =>
                CharClassSet::Class(CharClass::Range(
                    class_set_range.start.c,
                    class_set_range.end.c,
                )),

            ClassSetItem::Unicode(class_unicode) =>
                CharClassSet::from_regex_unicode_class(class_unicode),

            ClassSetItem::Perl(class_perl) =>
                CharClassSet::from_regex_perl_class(class_perl),

            ClassSetItem::Ascii(class_ascii) =>
                CharClassSet::from_regex_ascii_class(class_ascii),

            ClassSetItem::Bracketed(class_bracketed) =>
                CharClassSet::from_regex_bracketed(class_bracketed),
            ClassSetItem::Union(class_set_union) =>
                class_set_union.items.iter()
                    .map(Self::from_regex_class_set_item)
                    .reduce(|a, b| CharClassSet::Union(Box::new(a), Box::new(b)))
                    .unwrap_or(CharClassSet::Empty),
        }
    }

    fn from_regex_class_set(class: &rast::ClassSet) -> Self {
        use rast::*;
        match class {
            ClassSet::Item(class_set_item) =>
                Self::from_regex_class_set_item(class_set_item),

            ClassSet::BinaryOp(class_set_binary_op) => {

                let a = Self::from_regex_class_set(&class_set_binary_op.lhs);
                let b = Self::from_regex_class_set(&class_set_binary_op.rhs);

                match class_set_binary_op.kind {
                    ClassSetBinaryOpKind::Intersection =>
                        CharClassSet::Intersection(Box::new(a), Box::new(b)),
                    ClassSetBinaryOpKind::Difference =>
                        CharClassSet::Intersection(Box::new(a), Box::new(CharClassSet::Not(Box::new(b)))),
                    ClassSetBinaryOpKind::SymmetricDifference =>
                        CharClassSet::Union(
                            Box::new(CharClassSet::Intersection(Box::new(a.clone()), Box::new(CharClassSet::Not(Box::new(b.clone()))))),
                            Box::new(CharClassSet::Intersection(Box::new(b), Box::new(CharClassSet::Not(Box::new(a))))),
                        ),
                }
            },
        }
    }
}




fn normalize_prop(class_name: &str) -> String {
    let mut s = String::new();
    s.reserve(class_name.len());

    for word in class_name.split([' ', '-', '_']) {
        if !s.is_empty() {
            s += "_";
        }

        let mut word = word.chars();
        if let Some(first) = word.next() {
            s.extend(first.to_uppercase());
            s.extend(word.flat_map(|c| c.to_lowercase()));
        }
    }

    s
}


fn unicode_category(s: &str) -> Option<CharClassSet> {
    let category = match s {
        "Lu" | "Uppercase_Letter" => GeneralCategory::UppercaseLetter,
        "Ll" | "Lowercase_Letter" => GeneralCategory::LowercaseLetter,
        "Lt" | "Titlecase_Letter" => GeneralCategory::TitlecaseLetter,
        "Lm" | "Modifier_Letter" => GeneralCategory::ModifierLetter,
        "Lo" | "Other_Letter" => GeneralCategory::OtherLetter,
        "Mn" | "Non_Spacing_Mark" => GeneralCategory::NonspacingMark,
        "Mc" | "Spacing_Combining_Mark" => GeneralCategory::SpacingMark,
        "Me" | "Enclosing_Mark" => GeneralCategory::EnclosingMark,
        "Nd" | "Decimal_Digit_Number" => GeneralCategory::UppercaseLetter,
        "Nl" | "Letter_Number" => GeneralCategory::UppercaseLetter,
        "No" | "Other_Number" => GeneralCategory::UppercaseLetter,
        "Sm" | "Math_Symbol" => GeneralCategory::UppercaseLetter,
        "Sc" | "Currency_Symbol" => GeneralCategory::UppercaseLetter,
        "Sk" | "Modifier_Symbol" => GeneralCategory::UppercaseLetter,
        "So" | "Other_Symbol" => GeneralCategory::UppercaseLetter,
        "Pc" | "Connector_Punctuation" => GeneralCategory::UppercaseLetter,
        "Pd" | "Dash_Punctuation" => GeneralCategory::UppercaseLetter,
        "Ps" | "Open_Punctuation" => GeneralCategory::UppercaseLetter,
        "Pe" | "Close_Punctuation" => GeneralCategory::UppercaseLetter,
        "Pi" | "Initial_Punctuation" => GeneralCategory::UppercaseLetter,
        "Pf" | "Final_Punctuation" => GeneralCategory::UppercaseLetter,
        "Po" | "Other_Punctuation" => GeneralCategory::UppercaseLetter,
        "Zs" | "Space_Separator" => GeneralCategory::UppercaseLetter,
        "Zl" | "Line_Separator" => GeneralCategory::UppercaseLetter,
        "Zp" | "Paragraph_Separator" => GeneralCategory::UppercaseLetter,
        "Cc" | "Control" => GeneralCategory::UppercaseLetter,
        "Cf" | "Format" => GeneralCategory::UppercaseLetter,
        "Cs" | "Surrogate" => GeneralCategory::UppercaseLetter,
        "Co" | "Private_Use" => GeneralCategory::UppercaseLetter,
        "Cn" | "Unassigned" => GeneralCategory::UppercaseLetter,
        _ => return None,
    };

    Some(CharClassSet::Class(CharClass::Category(category)))
}

fn unicode_category_group(s: &str) -> Option<CharClassSet> {
    let categories = match s {
        "L" | "Letter" => vec![
            GeneralCategory::UppercaseLetter,
            GeneralCategory::LowercaseLetter,
            GeneralCategory::TitlecaseLetter,
            GeneralCategory::ModifierLetter,
            GeneralCategory::OtherLetter,
        ],
        "M" | "Mark" => vec![
            GeneralCategory::NonspacingMark,
            GeneralCategory::SpacingMark,
            GeneralCategory::EnclosingMark,
        ],
        "N" | "Number" => vec![
            GeneralCategory::DecimalNumber,
            GeneralCategory::LetterNumber,
            GeneralCategory::OtherNumber,
        ],
        "S" | "Symbol" => vec![
            GeneralCategory::MathSymbol,
            GeneralCategory::CurrencySymbol,
            GeneralCategory::ModifierSymbol,
            GeneralCategory::OtherSymbol,
        ],
        "P" | "Punctuation" => vec![
            GeneralCategory::ConnectorPunctuation,
            GeneralCategory::DashPunctuation,
            GeneralCategory::OpenPunctuation,
            GeneralCategory::ClosePunctuation,
            GeneralCategory::InitialPunctuation,
            GeneralCategory::FinalPunctuation,
            GeneralCategory::OtherPunctuation,
        ],
        "Z" | "Separator" => vec![
            GeneralCategory::SpaceSeparator,
            GeneralCategory::LineSeparator,
            GeneralCategory::ParagraphSeparator,
        ],
        "C" | "Other" => vec![
            GeneralCategory::Control,
            GeneralCategory::Format,
            GeneralCategory::Surrogate,
            GeneralCategory::PrivateUse,
            GeneralCategory::Unassigned,
        ],
        _ => return None,
    };

    categories.into_iter()
        .map(CharClass::Category)
        .map(CharClassSet::Class)
        .reduce(|a, b| CharClassSet::Union(Box::new(a), Box::new(b)))
}

fn binary_property(s: &str) -> Option<CharClassSet> {
    let class = match s {
        "Alphabetic" => CharClass::Property(UnicodePropertySet::Alphabetic),
        "Uppercase" => CharClass::Property(UnicodePropertySet::Uppercase),
        "Lowercase" => CharClass::Property(UnicodePropertySet::Lowercase),
        "White_Space" => CharClass::Property(UnicodePropertySet::WhiteSpace),
        "Ascii" => CharClass::Range('\0', '\x7F'),
        "Any" => CharClass::Any,
        _ => return None,
    };

    Some(CharClassSet::Class(class))
}



