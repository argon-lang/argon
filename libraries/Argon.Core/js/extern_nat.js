
export async function Nat_to_s() {
	return argonRuntime.trampoline.result(createString(this[natValueSymbol].toString()));
}

export async function addNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] + b[natValueSymbol]));
}

export async function subNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] - b[natValueSymbol]));
}

export async function mulNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] * b[natValueSymbol]));
}

export async function divNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] / b[natValueSymbol]));
}

export async function natEqual(a, b) {
	return argonRuntime.trampoline.result(createBool(a[natValueSymbol] === b[natValueSymbol]));
}

export async function natLessThan(a, b) {
	return argonRuntime.trampoline.result(createBool(a[natValueSymbol] < b[natValueSymbol]));
}

export async function natLessThanEq(a, b) {
	return argonRuntime.trampoline.result(createBool(a[natValueSymbol] <= b[natValueSymbol]));
}

export async function natGreaterThan(a, b) {
	return argonRuntime.trampoline.result(createBool(a[natValueSymbol] > b[natValueSymbol]));
}

export async function natGreaterThanEq(a, b) {
	return argonRuntime.trampoline.result(createBool(a[natValueSymbol] >= b[natValueSymbol]));
}

export async function bitOrNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] | b[natValueSymbol]));
}

export async function bitAndNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] & b[natValueSymbol]));
}

export async function bitXOrNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] ^ b[natValueSymbol]));
}

export async function shiftLeftNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] << b[natValueSymbol]));
}

export async function shiftRightNat(a, b) {
	return argonRuntime.trampoline.result(createNat(a[natValueSymbol] >> b[natValueSymbol]));1
}


