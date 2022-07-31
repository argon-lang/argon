
export function Nat_to_s(self) {
	return createString(self[natValueSymbol].toString());
}

export function addNat(a, b) {
	return createNat(a[natValueSymbol] + b[natValueSymbol]);
}

export function subNat(a, b) {
	return createNat(a[natValueSymbol] - b[natValueSymbol]);
}

export function mulNat(a, b) {
	return createNat(a[natValueSymbol] * b[natValueSymbol]);
}

export function divNat(a, b) {
	return createNat(a[natValueSymbol] / b[natValueSymbol]);
}

export function natEqual(a, b) {
	return createBool(a[natValueSymbol] === b[natValueSymbol]);
}

export function natLessThan(a, b) {
	return createBool(a[natValueSymbol] < b[natValueSymbol]);
}

export function natLessThanEq(a, b) {
	return createBool(a[natValueSymbol] <= b[natValueSymbol]);
}

export function natGreaterThan(a, b) {
	return createBool(a[natValueSymbol] > b[natValueSymbol]);
}

export function natGreaterThanEq(a, b) {
	return createBool(a[natValueSymbol] >= b[natValueSymbol]);
}

export function bitOrNat(a, b) {
	return createNat(a[natValueSymbol] | b[natValueSymbol]);
}

export function bitAndNat(a, b) {
	return createNat(a[natValueSymbol] & b[natValueSymbol]);
}

export function bitXOrNat(a, b) {
	return createNat(a[natValueSymbol] ^ b[natValueSymbol]);
}

export function shiftLeftNat(a, b) {
	return createNat(a[natValueSymbol] << b[natValueSymbol]);
}

export function shiftRightNat(a, b) {
	return createNat(a[natValueSymbol] >> b[natValueSymbol]);
}


