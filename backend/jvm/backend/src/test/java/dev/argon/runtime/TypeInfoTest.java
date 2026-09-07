package dev.argon.runtime;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

final class TypeInfoTest {
	@Test
	void equalsTreatsNullAndEmptyTypeArgsAsEquivalent() {
		var withoutArgs = new TypeInfo(String.class);
		var withEmptyArgs = new TypeInfo(String.class, new Token[0]);
		var withNullArgs = new TypeInfo(String.class, (Token[]) null);

		assertEquals(withoutArgs, withEmptyArgs);
		assertEquals(withoutArgs, withNullArgs);
		assertEquals(withoutArgs.hashCode(), withEmptyArgs.hashCode());
		assertEquals(withoutArgs.hashCode(), withNullArgs.hashCode());
	}

	@Test
	void equalsComparesTypeArgs() {
		var intType = new TypeInfo(Integer.class);
		var stringArray = new TypeInfo(Object[].class, new TypeInfo(String.class));
		var matchingStringArray = new TypeInfo(Object[].class, new TypeInfo(String.class));
		var intArray = new TypeInfo(Object[].class, intType);

		assertEquals(stringArray, matchingStringArray);
		assertEquals(stringArray.hashCode(), matchingStringArray.hashCode());
		assertNotEquals(stringArray, intArray);
		assertEquals(intType, intArray.typeArg(0));
	}
}
