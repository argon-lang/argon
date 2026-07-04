package dev.argon.runtime;

public class RefCell<T> {
	public RefCell(T value) {
		this.value = value;
	}

	private T value;

	public T get() {
		return value;
	}

	public void set(T value) {
		this.value = value;
	}
}
