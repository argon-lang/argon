package dev.argon.runtime;

/**
 * A tuple with three elements.
 */
public record Tuple3<T0, T1, T2>(
	T0 element0,
	T1 element1,
	T2 element2
) implements TupleBase {
}
