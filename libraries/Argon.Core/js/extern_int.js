
export async function Int_to_s() {
	return argonRuntime.trampoline.result(createString(this[intValueSymbol].toString()));
}

export async function addInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] + b[intValueSymbol]));
}

export async function subInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] - b[intValueSymbol]));
}

export async function mulInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] * b[intValueSymbol]));
}

export async function divInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] / b[intValueSymbol]));
}

export async function negateInt(a, b) {
	return argonRuntime.trampoline.result(createInt(-a[intValueSymbol]));
}

export async function intEqual(a, b) {
	return argonRuntime.trampoline.result(createBool(a[intValueSymbol] === b[intValueSymbol]));
}

export async function intLessThan(a, b) {
	return argonRuntime.trampoline.result(createBool(a[intValueSymbol] < b[intValueSymbol]));
}

export async function intLessThanEq(a, b) {
	return argonRuntime.trampoline.result(createBool(a[intValueSymbol] <= b[intValueSymbol]));
}

export async function intGreaterThan(a, b) {
	return argonRuntime.trampoline.result(createBool(a[intValueSymbol] > b[intValueSymbol]));
}

export async function intGreaterThanEq(a, b) {
	return argonRuntime.trampoline.result(createBool(a[intValueSymbol] >= b[intValueSymbol]));
}

export async function bitNotInt(a, b) {
	return argonRuntime.trampoline.result(createInt(~a[intValueSymbol]));
}

export async function bitOrInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] | b[intValueSymbol]));
}

export async function bitAndInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] & b[intValueSymbol]));
}

export async function bitXOrInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] ^ b[intValueSymbol]));
}

export async function shiftLeftInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] << b[intValueSymbol]));
}

export async function shiftRightInt(a, b) {
	return argonRuntime.trampoline.result(createInt(a[intValueSymbol] >> b[intValueSymbol]));
}


