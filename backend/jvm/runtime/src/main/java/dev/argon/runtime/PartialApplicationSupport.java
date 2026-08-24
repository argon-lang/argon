package dev.argon.runtime;

import java.lang.invoke.CallSite;
import java.lang.invoke.ConstantCallSite;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandleProxies;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.ArrayList;

public final class PartialApplicationSupport {
	private PartialApplicationSupport() {
	}

	public static CallSite tokenFunction(
		MethodHandles.Lookup lookup,
		String name,
		MethodType factoryType,
		MethodHandle target,
		int tokenArgumentCount
	) throws NoSuchMethodException, IllegalAccessException {
		var runtimeLookup = MethodHandles.lookup();
		var factory = runtimeLookup.findStatic(
			PartialApplicationSupport.class,
			"makeTokenFunction",
			MethodType.methodType(FunctionToken.class, MethodHandle.class, int.class, Object[].class)
		);
		factory = MethodHandles
			.insertArguments(factory, 0, target, tokenArgumentCount)
			.asCollector(Object[].class, factoryType.parameterCount())
			.asType(factoryType);
		return new ConstantCallSite(factory);
	}

	@SuppressWarnings("UnusedMethod")
	private static FunctionToken<?, ?> makeTokenFunction(MethodHandle target, int tokenArgumentCount, Object[] captures) {
		var targetWithTokenLast = tokenLastTarget(target, tokenArgumentCount);
		var boundTarget = MethodHandles
			.insertArguments(targetWithTokenLast, 0, captures)
			.asType(MethodType.methodType(Trampoline.class, Token.class));
		return (FunctionToken<?, ?>)MethodHandleProxies.asInterfaceInstance(FunctionToken.class, boundTarget);
	}

	private static MethodHandle tokenLastTarget(MethodHandle target, int tokenArgumentCount) {
		var targetType = target.type();
		var valueArgumentCount = targetType.parameterCount() - tokenArgumentCount - 1;
		if(valueArgumentCount < 0) {
			throw new IllegalArgumentException("Token argument count exceeds target parameter count");
		}

		var reorderedParameterTypes = new ArrayList<Class<?>>();
		for(int i = 0; i < tokenArgumentCount; ++i) {
			reorderedParameterTypes.add(targetType.parameterType(i));
		}
		for(int i = 0; i < valueArgumentCount; ++i) {
			reorderedParameterTypes.add(targetType.parameterType(tokenArgumentCount + 1 + i));
		}
		reorderedParameterTypes.add(targetType.parameterType(tokenArgumentCount));

		var reorder = new int[targetType.parameterCount()];
		for(int i = 0; i < tokenArgumentCount; ++i) {
			reorder[i] = i;
		}
		reorder[tokenArgumentCount] = tokenArgumentCount + valueArgumentCount;
		for(int i = 0; i < valueArgumentCount; ++i) {
			reorder[tokenArgumentCount + 1 + i] = tokenArgumentCount + i;
		}

		return MethodHandles.permuteArguments(
			target,
			MethodType.methodType(targetType.returnType(), reorderedParameterTypes),
			reorder
		);
	}

}
