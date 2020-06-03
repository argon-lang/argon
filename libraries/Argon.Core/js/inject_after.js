
export const unitValue = Object.freeze(Object.create(classes["Ar.Unit:"].constructor.prototype));

export function createString(str) {
	const obj = Object.create(classes["Ar.String:"].constructor.prototype);
	obj[stringValueSymbol] = str;
	return Object.freeze(obj);
};

export function createInt(i) {
	const obj = Object.create(classes["Ar.Int:"].constructor.prototype);
	obj[intValueSymbol] = i;
	return Object.freeze(obj);
}

export function createBool(b) {
	const obj = Object.create(classes["Ar.Bool:"].constructor.prototype);
	obj[boolValueSymbol] = b;
	return Object.freeze(obj);
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
