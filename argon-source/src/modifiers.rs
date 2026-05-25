use argon_compiler::Context;
use argon_compiler::access::AccessModifierGlobal;
use argon_expr::ErasureMode;
use argon_parser::ast::Modifier;
use argon_util::CompileError;
use nonempty_collections::NESlice;
use nonempty_collections::NonEmptyIterator;
use parse18_runtime::{Location, WithLocation};
use std::collections::HashMap;
use std::collections::hash_map::Entry;

pub struct ModifierParser<'a> {
    context: Context,
    modifiers: HashMap<Modifier, &'a Location>,
    fallback_location: &'a Location,
}

impl<'a> ModifierParser<'a> {
    pub fn new(
        context: Context,
        modifiers: &'a [WithLocation<Modifier>],
        fallback_location: &'a Location,
    ) -> ModifierParser<'a> {
        let mut modifier_group = HashMap::new();
        for modifier in modifiers {
            match modifier_group.entry(modifier.value) {
                Entry::Occupied(oe) => {
                    context
                        .reporter()
                        .report_error(CompileError::duplicate_modifier(
                            modifier.location.clone(),
                            oe.key().to_string(),
                        ))
                }
                Entry::Vacant(ve) => {
                    ve.insert(&modifier.location);
                }
            }
        }

        ModifierParser {
            context,
            modifiers: modifier_group,
            fallback_location,
        }
    }

    pub fn parse<T: Clone>(&mut self, spec: &ModifierSpec<T>) -> T {
        let mut relevant_modifiers = HashMap::new();
        self.modifiers.retain(|modifier, location| {
            let relevant = spec_has_modifier(spec, *modifier);
            if relevant {
                relevant_modifiers.insert(*modifier, *location);
            }

            !relevant
        });

        if let Some(value) = find_exact_match(spec, &relevant_modifiers) {
            return value;
        }

        self.context
            .reporter()
            .report_error(CompileError::invalid_modifier(
                relevant_modifiers
                    .iter()
                    .next()
                    .map(|(_, loc)| (*loc).clone())
                    .unwrap_or_else(|| self.fallback_location.clone()),
                relevant_modifiers
                    .keys()
                    .map(|modifier| modifier.to_string()),
            ));

        find_best_match(spec, &relevant_modifiers)
    }

    pub fn done(self) {
        if let Some(location) = self.modifiers.values().next() {
            self.context
                .reporter()
                .report_error(CompileError::invalid_modifier(
                    (*location).clone(),
                    self.modifiers.keys().map(|modifier| modifier.to_string()),
                ));
        }
    }
}

type ModifierSpec<T> = NESlice<'static, (&'static [Modifier], T)>;

pub const ACCESS_MODIFIER_GLOBAL: ModifierSpec<AccessModifierGlobal> =
    ModifierSpec::try_from_slice(&[
        (&[], AccessModifierGlobal::ModulePrivate),
        (&[Modifier::Public], AccessModifierGlobal::Public),
        (&[Modifier::Internal], AccessModifierGlobal::Internal),
        (
            &[Modifier::Private, Modifier::Internal],
            AccessModifierGlobal::ModulePrivate,
        ),
    ])
    .unwrap();

pub const IS_INLINE: ModifierSpec<bool> =
    ModifierSpec::try_from_slice(&[(&[Modifier::Inline], true), (&[], false)]).unwrap();

pub const IS_WITNESS: ModifierSpec<bool> =
    ModifierSpec::try_from_slice(&[(&[Modifier::Witness], true), (&[], false)]).unwrap();

pub const ERASURE_MODE: ModifierSpec<ErasureMode> = ModifierSpec::try_from_slice(&[
    (&[Modifier::Erased], ErasureMode::Erased),
    (&[Modifier::Token], ErasureMode::Token),
    (&[], ErasureMode::Concrete),
])
.unwrap();

fn spec_has_modifier<T>(spec: &ModifierSpec<T>, modifier: Modifier) -> bool {
    spec.iter()
        .any(|&(modifiers, _)| modifiers.contains(&modifier))
}

fn find_exact_match<T: Clone>(
    spec: &ModifierSpec<T>,
    modifiers: &HashMap<Modifier, &Location>,
) -> Option<T> {
    spec.iter()
        .filter(|(spec_modifiers, _)| {
            spec_modifiers
                .iter()
                .all(|modifier| modifiers.contains_key(modifier))
                && modifiers.iter().all(|(modifier, _)| {
                    spec_modifiers
                        .iter()
                        .any(|spec_modifier| modifier == spec_modifier)
                })
        })
        .map(|(_, value)| value.clone())
        .next()
}

fn find_best_match<T: Clone>(
    spec: &ModifierSpec<T>,
    modifiers: &HashMap<Modifier, &Location>,
) -> T {
    spec.nonempty_iter()
        .max_by_key(|(spec_modifiers, _)| {
            let common_modifiers = spec_modifiers
                .iter()
                .filter(|modifier| modifiers.contains_key(modifier))
                .count();
            let extra_modifiers = spec_modifiers.len() - common_modifiers;

            (common_modifiers, extra_modifiers)
        })
        .1
        .clone()
}

#[cfg(test)]
mod tests {
    use super::*;
    use argon_compiler::test_utils::{TestContext, TestReporter};
    use argon_util::ErrorCode;
    use parse18_runtime::FilePosition;
    use std::path::PathBuf;
    use std::sync::Arc;

    fn test_location(line: usize, column: usize) -> Location {
        Location {
            file: PathBuf::from("test.argon"),
            start: FilePosition { line, column },
            end: FilePosition {
                line,
                column: column + 1,
            },
        }
    }

    fn modifier(value: Modifier, line: usize, column: usize) -> WithLocation<Modifier> {
        WithLocation::new(value, test_location(line, column))
    }

    fn test_context() -> (Context, TestReporter) {
        let reporter = TestReporter::default();
        (Arc::new(TestContext::new(reporter.clone())), reporter)
    }

    #[test]
    fn access_modifier_defaults_to_module_private() {
        let (context, reporter) = test_context();
        let fallback_location = test_location(1, 1);
        let modifiers = [];
        let mut parser = ModifierParser::new(context, &modifiers, &fallback_location);

        let access = parser.parse(&ACCESS_MODIFIER_GLOBAL);
        parser.done();

        assert_eq!(access, AccessModifierGlobal::ModulePrivate);
        assert!(reporter.errors().is_empty());
    }

    #[test]
    fn parse_consumes_only_modifiers_relevant_to_spec() {
        let (context, reporter) = test_context();
        let fallback_location = test_location(1, 1);
        let modifiers = [
            modifier(Modifier::Public, 1, 1),
            modifier(Modifier::Inline, 1, 8),
        ];
        let mut parser = ModifierParser::new(context, &modifiers, &fallback_location);

        let access = parser.parse(&ACCESS_MODIFIER_GLOBAL);
        let is_inline = parser.parse(&IS_INLINE);
        parser.done();

        assert_eq!(access, AccessModifierGlobal::Public);
        assert!(is_inline);
        assert!(reporter.errors().is_empty());
    }

    #[test]
    fn duplicate_modifier_is_reported() {
        let (context, reporter) = test_context();
        let fallback_location = test_location(1, 1);
        let modifiers = [
            modifier(Modifier::Inline, 1, 1),
            modifier(Modifier::Inline, 1, 8),
        ];

        let mut parser = ModifierParser::new(context, &modifiers, &fallback_location);
        assert!(parser.parse(&IS_INLINE));
        parser.done();

        let errors = reporter.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::DuplicateModifier);
        assert!(errors[0].message.contains("inline"));
        assert_eq!(errors[0].location, Some(test_location(1, 8)));
    }

    #[test]
    fn invalid_modifier_combination_reports_error_and_returns_best_match() {
        let (context, reporter) = test_context();
        let fallback_location = test_location(1, 1);
        let modifiers = [modifier(Modifier::Private, 1, 1)];
        let mut parser = ModifierParser::new(context, &modifiers, &fallback_location);

        let access = parser.parse(&ACCESS_MODIFIER_GLOBAL);
        parser.done();

        assert_eq!(access, AccessModifierGlobal::ModulePrivate);
        let errors = reporter.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::InvalidModifier);
        assert!(errors[0].message.contains("private"));
        assert_eq!(errors[0].location, Some(test_location(1, 1)));
    }

    #[test]
    fn done_reports_unparsed_modifiers_at_their_own_location() {
        let (context, reporter) = test_context();
        let fallback_location = test_location(1, 1);
        let modifiers = [modifier(Modifier::Inline, 2, 4)];
        let mut parser = ModifierParser::new(context, &modifiers, &fallback_location);

        assert_eq!(
            parser.parse(&ACCESS_MODIFIER_GLOBAL),
            AccessModifierGlobal::ModulePrivate
        );
        parser.done();

        let errors = reporter.errors();
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].code, ErrorCode::InvalidModifier);
        assert!(errors[0].message.contains("inline"));
        assert_eq!(errors[0].location, Some(test_location(2, 4)));
    }
}
