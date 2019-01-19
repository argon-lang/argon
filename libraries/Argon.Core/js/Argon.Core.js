
export const traits = Object.create(null);
export const classes = Object.create(null);
export const functions = Object.create(null);

export const classConstructorSymbol = Symbol();

function createMethods(methodDescriptions, methods) {
    for(const description of methodDescriptions) {
        const methodInfo = Object.create(null);
        methodInfo.symbol = Symbol();
        methodInfo.value = description.value;
        Object.freeze(methodInfo)
        methods[description.descriptor] = methodInfo;
    }
}

export function createTrait(creator) {
	const trait = Object.create(null);

	trait.marker = Symbol();
	
	lazyProperty(trait, "baseTraits", function() {
		return creator.baseTraits || [];
	});

	const methods = Object.create(null);
	if(creator.methods) createMethods(creator.methods, methods);
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
            implementMethods(proto, methods);
		});
	});

	const methods = Object.create(null);
	if(creator.methods) createMethods(creator.methods, methods);
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

function implementMethods(obj, methods) {
    for(const method in methods) {
        const methodInfo = methods[method];
        obj[methodInfo.symbol] = methodInfo.value;
    }
}


const stringValueSymbol = Symbol();
const intValueSymbol = Symbol();
export const boolValueSymbol = Symbol();

traits["Ar.Object"] = createTrait({});

classes["Ar.Unit"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
});

classes["Ar.String"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
});

classes["Ar.Int"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
	methods: [
	    {
	        descriptor: "to_s:(Ar.Unit)->(Ar.String)",
	        value: function(param_0) {
                return createString(this[intValueSymbol].toString());
            },
	    },
	],
});

classes["Ar.Bool"] = createClass({
	get baseTraits() { return [ traits["Ar.Object"] ]; },
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

export function createInt(i) {
    const obj = Object.create(classes["Ar.Int"].constructor.prototype);
    obj[intValueSymbol] = i;
    return Object.freeze(obj);
}

export function createBool(b) {
    const obj = Object.create(classes["Ar.Bool"].constructor.prototype);
    obj[boolValueSymbol] = b;
    return Object.freeze(obj);
}

export function addInt(a, b) {
    return createInt(a[intValueSymbol] + b[intValueSymbol]);
}

export function subInt(a, b) {
    return createInt(a[intValueSymbol] - b[intValueSymbol]);
}

export function mulInt(a, b) {
    return createInt(a[intValueSymbol] * b[intValueSymbol]);
}

export function intEqual(a, b) {
    return createBool(a[intValueSymbol] === b[intValueSymbol]);
}

Object.freeze(traits);
Object.freeze(classes);
Object.freeze(functions);

