
const boolValueSymbol = Symbol();

export function createBool(b) {
	const obj = Object.create(Bool$v);
	obj[boolValueSymbol] = b;
	return Object.freeze(obj);
}

