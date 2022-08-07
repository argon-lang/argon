
const boolValueSymbol = Symbol();

export function createBool(b) {
	const obj = Object.create(Bool$v().prototype);
	obj[boolValueSymbol] = b;
	return Object.freeze(obj);
}

