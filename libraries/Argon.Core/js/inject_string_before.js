import { createBool } from "./Bool.js";

export const stringValueSymbol = Symbol();

export function createString(str) {
	const obj = Object.create(String$v().prototype);
	obj[stringValueSymbol] = str;
	return Object.freeze(obj);
}

