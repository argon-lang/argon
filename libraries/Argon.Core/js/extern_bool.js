
export function Bool_to_s(self) {
 	return createString(self[boolValueSymbol].toString());
 }


export function boolNot(a) {
	return createBool(!a[boolValueSymbol]);
}

export function boolEqual(a, b) {
	return createBool(a[boolValueSymbol] === b[boolValueSymbol]);
}

