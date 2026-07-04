package dev.argon.runtime;

/**
 * A tuple with five elements.
 */
public record Tuple5<T0, T1, T2, T3, T4>(
	T0 element0,
	T1 element1,
	T2 element2,
	T3 element3,
	T4 element4
) implements TupleBase {
}
