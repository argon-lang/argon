
export function Int_to_s(self) {
	return createString(self[intValueSymbol].toString());
}

export function Bool_to_s(self) {
 	return createString(self[boolValueSymbol].toString());
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


export function boolNot(a) {
	return createBool(!a[boolValueSymbol]);
}

export function boolEqual(a, b) {
	return createBool(a[boolValueSymbol] === b[boolValueSymbol]);
}



export function stringConcat(a, b) {
	return createString(a[stringValueSymbol] + b[stringValueSymbol]);
}

export function stringEqual(a, b) {
	return createBool(a[stringValueSymbol] === b[stringValueSymbol]);
}

