
export async function puts(s) {
	console.log(s[stringValueSymbol]);
	return argonRuntime.trampoline.result(undefined);
}

