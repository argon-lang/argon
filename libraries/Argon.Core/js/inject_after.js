
const unitClass = currentModule.globalClass(["Ar"], "Unit", { parameterTypes: [] });
const stringClass = currentModule.globalClass(["Ar"], "String", { parameterTypes: [] });
const intClass = currentModule.globalClass(["Ar"], "Int", { parameterTypes: [] });
const boolClass = currentModule.globalClass(["Ar"], "Bool", { parameterTypes: [] });


export const unitValue = Object.freeze(Object.create(
	unitClass.createUninitializedInstance(unitClass.createClassObject())
));

export function createString(str) {
	const obj = stringClass.createUninitializedInstance(stringClass.createClassObject());
	obj[stringValueSymbol] = str;
	return Object.freeze(obj);
};

export function createInt(i) {
	const obj = intClass.createUninitializedInstance(intClass.createClassObject());
	obj[intValueSymbol] = i;
	return Object.freeze(obj);
}

export function createBool(b) {
	const obj = boolClass.createUninitializedInstance(boolClass.createClassObject());
	obj[boolValueSymbol] = b;
	return Object.freeze(obj);
}
