
import { createString } from "./String.js";
import { createBool } from "./Bool.js";

const intValueSymbol = Symbol();

export function createInt(i) {
	const obj = Object.create(Int$v().prototype);
	obj[intValueSymbol] = i;
	return Object.freeze(obj);
}

