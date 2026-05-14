pub trait ErrorReporter<E>: Clone {
    fn report_error(&self, error: E);
}
