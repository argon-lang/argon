
export function stringConcat(a, b) {
	return createString(a[stringValueSymbol] + b[stringValueSymbol]);
}

export function stringEqual(a, b) {
	return createBool(a[stringValueSymbol] === b[stringValueSymbol]);
}

