package dev.argon.runtime;

/**
 * A tuple with ten elements.
 */
public record Tuple10<T0, T1, T2, T3, T4, T5, T6, T7, T8, T9>(
	T0 element0,
	T1 element1,
	T2 element2,
	T3 element3,
	T4 element4,
	T5 element5,
	T6 element6,
	T7 element7,
	T8 element8,
	T9 element9
) implements TupleBase {
}
