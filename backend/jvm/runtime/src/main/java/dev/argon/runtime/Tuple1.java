package dev.argon.runtime;

/**
 * A tuple with one element.
 */
public record Tuple1<T0>(
	T0 element0
) implements TupleBase {
}
