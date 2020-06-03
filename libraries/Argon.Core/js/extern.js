
export function Int_to_s() {
	return createString(this[intValueSymbol].toString());
}

export function puts(s) {
	console.log(s[stringValueSymbol]);
	return unitValue;
}
