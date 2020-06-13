
export function Int_to_s(self) {
	return createString(self[intValueSymbol].toString());
}

export function puts(s) {
	console.log(s[stringValueSymbol]);
	return unitValue;
}

export function addInt(a, b) {
	return createInt(a[intValueSymbol] + b[intValueSymbol]);
}

export function subInt(a, b) {
	return createInt(a[intValueSymbol] - b[intValueSymbol]);
}

export function mulInt(a, b) {
	return createInt(a[intValueSymbol] * b[intValueSymbol]);
}

export function intEqual(a, b) {
	return createBool(a[intValueSymbol] === b[intValueSymbol]);
}

