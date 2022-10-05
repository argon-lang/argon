package dev.argon.argonvm;

import java.util.Arrays;
import java.util.Objects;

public class VMTuple {
	public VMTuple(Object[] elements) {
		this.elements = elements.clone();
	}

	private final Object[] elements;

	public int size() {
		return elements.length;
	}

	public Object get(int i) {
		return elements[i];
	}

	@Override
	public int hashCode() {
		return Objects.hash(elements);
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof VMTuple other && Arrays.equals(elements, other.elements);
	}
}
