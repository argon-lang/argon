package dev.argon.runtime;

/**
 * A tuple with two elements.
 */
public record Tuple2<T0, T1>(T0 element0, T1 element1) implements TupleBase {
}
