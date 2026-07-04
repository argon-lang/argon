package dev.argon.runtime;

/**
 * A tuple with four elements.
 */
public record Tuple4<T0, T1, T2, T3>(
	T0 element0,
	T1 element1,
	T2 element2,
	T3 element3
) implements TupleBase {
}
