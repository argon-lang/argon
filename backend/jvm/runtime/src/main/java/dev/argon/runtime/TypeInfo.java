package dev.argon.runtime;

import org.jspecify.annotations.Nullable;

import java.util.Arrays;
import java.util.Objects;

public final class TypeInfo implements Token {
	public TypeInfo(Class<?> type) {
		this.type = type;
		this.typeArgs = null;
	}

	public TypeInfo(Class<?> type, Token @Nullable... typeArgs) {
		this.type = type;
		this.typeArgs = typeArgs == null || typeArgs.length == 0 ? null : typeArgs.clone();
	}

	private final Class<?> type;
	private final Token @Nullable [] typeArgs;

	public Class<?> javaClass() {
		return type;
	}

	public int numTypeArgs() {
		return typeArgs == null ? 0 : typeArgs.length;
	}

	public Token typeArg(int index) {
		return typeArgsOrEmpty()[index];
	}

	@Override
	public boolean equals(@Nullable Object obj) {
		if(this == obj) {
			return true;
		}

		if(!(obj instanceof TypeInfo other)) {
			return false;
		}

		return Objects.equals(type, other.type) && Arrays.equals(typeArgsOrEmpty(), other.typeArgsOrEmpty());
	}

	@Override
	public int hashCode() {
		return 31 * Objects.hashCode(type) + Arrays.hashCode(typeArgsOrEmpty());
	}

	private Token[] typeArgsOrEmpty() {
		return typeArgs == null ? EMPTY_TYPE_ARGS : typeArgs;
	}

	private static final Token[] EMPTY_TYPE_ARGS = new Token[0];
}
