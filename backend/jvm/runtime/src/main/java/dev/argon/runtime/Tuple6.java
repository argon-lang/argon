package dev.argon.runtime;

/**
 * A tuple with six elements.
 */
public record Tuple6<T0, T1, T2, T3, T4, T5>(T0 element0, T1 element1, T2 element2, T3 element3, T4 element4,
	T5 element5) implements TupleBase {
}
