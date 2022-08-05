
export const stringValueSymbol = Symbol();

export function createString(str) {
	const obj = Object.create(String$v);
	obj[stringValueSymbol] = str;
	return Object.freeze(obj);
}

