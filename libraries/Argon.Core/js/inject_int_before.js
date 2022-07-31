
const intValueSymbol = Symbol();

export function createInt(i) {
	const obj = Object.create(Int$v);
	obj[intValueSymbol] = i;
	return Object.freeze(obj);
}

