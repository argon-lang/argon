
export const traits = Object.create(null);
export const classes = Object.create(null);
export const functions = Object.create(null);

export const classConstructorSymbol = Symbol();

export function createTrait(creator) {
	const trait = Object.create(null);

	trait.marker = Symbol();
	
	lazyProperty(trait, "baseTraits", function() {
		return creator.baseTraits || [];
	});

	const methods = Object.create(null);
	if(creator.createMethods) creator.createMethods(methods);
	trait.methods = Object.freeze(methods);

	return Object.freeze(trait);
}

export function createClass(creator) {
	const cls = Object.create(null);

	lazyProperty(cls, "constructor", function() {
		
		const ctors = Object.create(null);
		if(creator.createConstructors) creator.createConstructors(ctors);
		Object.freeze(ctors);

		return createClassConstructor(creator.baseClass, ctors, function(proto) {
			const implTraits = creator.baseTraits;
			if(implTraits) {
				for(const trait of implTraits) {
					implementTrait(proto, trait);
				}
			}
		
		});
	});

	const methods = Object.create(null);
	if(creator.createMethods) creator.createMethods(methods);
	cls.methods = Object.freeze(methods);

	return Object.freeze(cls);
}

function lazyProperty(obj, name, valueFunc) {
	let value;
	Object.defineProperty(obj, name, {
		get() {
			if(value === undefined) {
				value = valueFunc();
			}
			return value;
		}
	});
}

function createClassConstructor(baseClass, ctors, init) {
	const func = function(ctorId, ...args) {
		ctors[ctorId].call(this, ...args);
	};

	if(baseClass) func.prototype = Object.create(baseClass.constructor);
	else func.prototype = Object.create(null);

	init(func.prototype);
	Object.freeze(func.prototype);
	return Object.freeze(func);
}

function implementTrait(obj, trait) {
	if(trait.marker in obj) {
		return;
	}

	obj[trait.marker] = true;
	for(const baseTrait of trait.baseTraits) {
		implementTrait(baseTrait);
	}
}


const stringValueSymbol = Symbol();

traits["Ar.Object"] = createTrait({});

classes["Ar.Object[TraitMetaClass]"] = createClass({
	get baseClass() { return classes["Ar.Trait"]; },
});

classes["Ar.Type"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
});

classes["Ar.Type[MetaClass]"] = createClass({
	get baseClass() { return classes["Ar.Class"]; },
});

classes["Ar.MetaClass"] = createClass({
	get baseTraits() { return classes["Ar.Class"]; },
});

classes["Ar.Trait"] = createClass({
	get baseClass() { return classes["Ar.Type"]; },
});

classes["Ar.Trait[MetaClass]"] = createClass({
	get baseClass() { return classes["Ar.Class"]; },
});

classes["Ar.Class"] = createClass({
	get baseClass() { return classes["Ar.Type"]; },
});

classes["Ar.Class[MetaClass]"] = createClass({
	get baseClass() { return classes["Ar.Class"]; },
});

classes["Ar.Unit"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
});

classes["Ar.Unit[MetaClass]"] = createClass({
	get baseClass() { return classes["Ar.Class"]; },
});

classes["Ar.String"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
});

classes["Ar.String[MetaClass]"] = createClass({
	get baseClass() { return classes["Ar.Class"]; },
});

functions["Ar.puts:($_:(Ar.String))->(Ar.Unit)"] = Object.freeze({
	value: function([str]) {
		console.log(str[stringValueSymbol]);
	},
});

export const unitValue = Object.freeze(Object.create(classes["Ar.Unit"].constructor.prototype));

export function createString(str) {
	const obj = Object.create(classes["Ar.String"].constructor.prototype);
	obj[stringValueSymbol] = str;
	return Object.freeze(obj);
};

Object.freeze(traits);
Object.freeze(classes);
Object.freeze(functions);


