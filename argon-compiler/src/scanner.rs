use std::sync::Arc;
use crate::{DefaultExprContext, EffectInfo, Function};
use argon_expr::{Expr, ExprContext, ExprScanner, FunctionArgument};

pub struct PurityScanner<EC: ?Sized> {
    found_impure_function_call: bool,
    _phantom: std::marker::PhantomData<*const EC>,
}

impl<EC> PurityScanner<EC>
    where EC: ExprContext<
        Function=Arc<dyn Function>,
    > + ?Sized
{
    pub fn new() -> Self {
        Self {
            found_impure_function_call: false,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn contains_impure_function_call(expr: &Expr<EC>) -> bool {
        let mut scanner = Self::new();
        scanner.scan(expr);
        scanner.found_impure_function_call()
    }

    pub fn found_impure_function_call(&self) -> bool {
        self.found_impure_function_call
    }
}

impl<EC> ExprScanner for PurityScanner<EC>
    where EC: ExprContext<
        Function=Arc<dyn Function>,
    > + ?Sized
{
    type EC = EC;

    fn scan(&mut self, expr: &Expr<Self::EC>) -> bool {
        match expr {
            Expr::FunctionCall {
                function,
                arguments,
            } => {
                if function.metadata().effect_info == EffectInfo::Effectful {
                    self.found_impure_function_call = true;
                    return false;
                }

                arguments
                    .iter()
                    .all(|argument: &FunctionArgument<Self::EC>| self.scan(&argument.arg))
            }
            _ => argon_expr::default_scan(self, expr),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::contains_impure_function_call;
    use crate::erased_sig::ImportSpecifier;
    use crate::signature::FunctionSignature;
    use crate::{
        DefaultExprContext, EffectInfo, Function, FunctionImplementation, FunctionMetadata, Unload,
    };
    use argon_expr::{ErasureMode, Expr, FunctionArgument};
    use argon_parser::ast::FunctionParameterListType;
    use std::sync::Arc;

    struct TestFunction {
        metadata: FunctionMetadata,
    }

    impl TestFunction {
        fn new(effect_info: EffectInfo) -> Self {
            Self {
                metadata: FunctionMetadata {
                    is_inline: false,
                    erasure_mode: ErasureMode::Concrete,
                    is_witness: false,
                    effect_info,
                },
            }
        }
    }

    impl Unload for TestFunction {
        fn unload(&self) {}
    }

    impl Function for TestFunction {
        fn metadata(&self) -> &FunctionMetadata {
            &self.metadata
        }

        fn import_specifier(self: Arc<Self>) -> ImportSpecifier {
            unimplemented!("not needed by scanner tests")
        }

        fn signature(self: Arc<Self>) -> Arc<FunctionSignature<DefaultExprContext>> {
            unimplemented!("not needed by scanner tests")
        }

        fn implementation(self: Arc<Self>) -> Option<Arc<FunctionImplementation>> {
            unimplemented!("not needed by scanner tests")
        }
    }

    fn function_call(
        effect_info: EffectInfo,
        arguments: Vec<Expr<DefaultExprContext>>,
    ) -> Expr<DefaultExprContext> {
        Expr::FunctionCall {
            function: Arc::new(TestFunction::new(effect_info)),
            arguments: arguments
                .into_iter()
                .map(|arg| FunctionArgument {
                    list_type: FunctionParameterListType::NormalList,
                    arg,
                })
                .collect(),
        }
    }

    #[test]
    fn detects_impure_function_call() {
        let expr = function_call(EffectInfo::Effectful, vec![]);

        assert!(contains_impure_function_call(&expr));
    }

    #[test]
    fn scans_pure_function_arguments() {
        let expr = function_call(
            EffectInfo::Pure,
            vec![function_call(EffectInfo::Effectful, vec![])],
        );

        assert!(contains_impure_function_call(&expr));
    }

    #[test]
    fn returns_false_when_all_function_calls_are_pure() {
        let expr = function_call(
            EffectInfo::Pure,
            vec![function_call(EffectInfo::Pure, vec![])],
        );

        assert!(!contains_impure_function_call(&expr));
    }
}
