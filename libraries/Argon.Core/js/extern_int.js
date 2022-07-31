
export function Int_to_s(self) {
	return createString(self[intValueSymbol].toString());
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

export function divInt(a, b) {
	return createInt(a[intValueSymbol] / b[intValueSymbol]);
}

export function negateInt(a, b) {
	return createInt(-a[intValueSymbol]);
}

export function intEqual(a, b) {
	return createBool(a[intValueSymbol] === b[intValueSymbol]);
}

export function intLessThan(a, b) {
	return createBool(a[intValueSymbol] < b[intValueSymbol]);
}

export function intLessThanEq(a, b) {
	return createBool(a[intValueSymbol] <= b[intValueSymbol]);
}

export function intGreaterThan(a, b) {
	return createBool(a[intValueSymbol] > b[intValueSymbol]);
}

export function intGreaterThanEq(a, b) {
	return createBool(a[intValueSymbol] >= b[intValueSymbol]);
}

export function bitNotInt(a, b) {
	return createInt(~a[intValueSymbol]);
}

export function bitOrInt(a, b) {
	return createInt(a[intValueSymbol] | b[intValueSymbol]);
}

export function bitAndInt(a, b) {
	return createInt(a[intValueSymbol] & b[intValueSymbol]);
}

export function bitXOrInt(a, b) {
	return createInt(a[intValueSymbol] ^ b[intValueSymbol]);
}

export function shiftLeftInt(a, b) {
	return createInt(a[intValueSymbol] << b[intValueSymbol]);
}

export function shiftRightInt(a, b) {
	return createInt(a[intValueSymbol] >> b[intValueSymbol]);
}


