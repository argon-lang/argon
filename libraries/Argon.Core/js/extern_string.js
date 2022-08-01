
export async function stringConcat(a, b) {
	return argonRuntime.trampoline.result(createString(a[stringValueSymbol] + b[stringValueSymbol]));
}

export async function stringEqual(a, b) {
	return argonRuntime.trampoline.result(createBool(a[stringValueSymbol] === b[stringValueSymbol]));
}

