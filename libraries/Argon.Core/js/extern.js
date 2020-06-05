
export function Int_to_s(self) {
	return createString(self[intValueSymbol].toString());
}

export function puts(s) {
	console.log(s[stringValueSymbol]);
	return unitValue;
}
