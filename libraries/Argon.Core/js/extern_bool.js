
export async function Bool_to_s(self) {
 	return argonRuntime.trampoline.result(createString(self[boolValueSymbol].toString()));
 }


export async function boolNot(a) {
	return argonRuntime.trampoline.result(createBool(!a[boolValueSymbol]));
}

export async function boolEqual(a, b) {
	return argonRuntime.trampoline.result(createBool(a[boolValueSymbol] === b[boolValueSymbol]));
}

