package dev.argon.vm;

public sealed interface FunctionResult<T> {
    default T run() throws Throwable {
        FunctionResult<T> result = this;
        while(result instanceof Trampoline<T> step) {
            result = step;
        }

        return ((Value<T>)result).value();
    }

    public static record Value<T>(T value) implements FunctionResult<T> {}

    public static non-sealed interface Trampoline<T> extends FunctionResult<T> {
        FunctionResult<T> next() throws Throwable;
    }
}
