
export class FunctionType {
    constructor(param, res) {
        this.param = param;
        this.res = res;
    }
}

export class FunctionTypeErased {
    constructor(res) {
        this.res = res;
    }
}

export class FunctionTypeToken {
    constructor(param, res) {
        this.param = param;
        this.res = res;
    }
}

export const typeInfo = {};
export const erasedType = {};

export class RefCellType {
    constructor(inner) {
        this.inner = inner;
    }
}

export class ArrayType {
    constructor(inner) {
        this.inner = inner;
    }
}

export class UnreachableError extends Error {
    constructor() {
        super();
    }
}

export class RefCell {
    constructor(value) {
        this.value = value;
    }
}

export function createArray(t, length) {
    switch(t) {
        case "i8": return new Int8Array(length);
        case "u8": return new Uint8Array(length);
        case "i16": return new Int16Array(length);
        case "u16": return new Uint16Array(length);
        case "i32": return new Int32Array(length);
        case "u32": return new Uint32Array(length);
        case "i64": return new BigInt64Array(length);
        case "u64": return new BigUint64Array(length);
        default: return new Array(length);
    }
}



function isSameType(a, b) {
    if(a === b) {
        return true;
    }

    if(a instanceof Array && b instanceof Array) {
        if(a.length !== b.length) {
            return false;
        }

        for(let i = 0; i < a.length; ++i) {
            if(!isSameType(a[i], b[i])) {
                return false;
            }
        }

        return true;
    }

    if(a instanceof FunctionType && b instanceof FunctionType) {
        return isSameType(a.param, b.param) && isSameType(a.res, b.res);
    }

    if(a instanceof FunctionTypeErased && b instanceof FunctionTypeErased) {
        return isSameType(a.res, b.res);
    }

    if(a instanceof FunctionTypeToken && b instanceof FunctionTypeToken) {
        return isSameType(a.param, b.param) && isSameType(a.res, b.res);
    }

    if(a instanceof RefCellType && b instanceof RefCellType) {
        return isSameType(a.inner, b.inner);
    }

    if(a instanceof ArrayType && b instanceof ArrayType) {
        return isSameType(a.inner, b.inner);
    }

    return false;
}

function createNamedConstructor(name) {
    const o = {
        [name]: function() {},
    };

    return o[name];
}

function specialize(options = {}) {
    const specializations = [];

    function getSpecialization(...tokenArgs) {
        const o = options.getInstancePrototype ? options.getInstancePrototype(this, tokenArgs) : this;
        for(const spec of specializations) {
            if(isSameType(spec.tokenArgs, tokenArgs)) {
                return spec.specializedClass;
            }
        }

        const specializedClass = function(...args) {
            o.call(this, ...args);
        };
        specializedClass.prototype = Object.create(o.prototype);
        storeTokenArgs(this, specializedClass, tokenArgs)

        specializations.push({
            tokenArgs,
            specializedClass,
        });

        if(options.customize) {
            options.customize(specializedClass);
        }

        return specializedClass;
    }
    
    return getSpecialization;
}

function bindStaticMethods(methods, tokenArgs) {
    const boundMethods = Object.create(null);
    for(const name of Object.keys(methods)) {
        if(Object.hasOwn(methods, name)) {
            boundMethods[name] = methods[name].bind(undefined, ...tokenArgs);
        }
    }
    return boundMethods;
}

function defineTokenArgSymbols(t, info) {
    t.tokenParameterSymbols = Array.from({ length: info.tokenParameterCount }, () => Symbol());
}

function storeTokenArgs(t, c, args) {
    for(let i = 0; i < args.length; ++i) {
        Object.defineProperty(
            c.prototype,
            t.tokenParameterSymbols[i],
            {
                value: args[i],
                writable: false,
            },
        );
    }
}

function loadTokenArgs(t, c) {
    return t.tokenParameterSymbols.map(symbol => c.prototype[symbol]);
}

export function createRecordType(recordInfo) {
    const recordTypeConstructorObj = {
        [recordInfo.name]: function(values) {
            for(const field of recordInfo.fields) {
                let fieldValue;
                if(field.name in values) {
                    fieldValue = values[field.name];
                }
                else if("defaultValue" in field) {
                    fieldValue = field.defaultValue;
                }
                else {
                    throw new Error("Missing field: " + field.name);
                }

                if(field.mutable) {
                    this["field_" + field.name] = fieldValue;
                }
                else {
                    Object.defineProperty(
                        this,
                        "field_" + field.name,
                        {
                            value: fieldValue,
                            writable: false,
                        },
                    );
                }
            }
        },
    };

    const recordType = recordTypeConstructorObj[recordInfo.name];

    recordType.specialize = specialize({
        customize(specialized) {
            specialized.staticMethods = bindStaticMethods(recordInfo.staticMethods, loadTokenArgs(recordType, specialized));
            specialized.methods = Object.create(null);
            applyVTable(specialized, recordInfo.methods, recordInfo.vtable);
        },
    });

    defineTokenArgSymbols(recordType, recordInfo);

    return recordType;
}

export function createEnumType(enumInfo) {
    const enumType = createNamedConstructor(enumInfo.name);

    function setupVariants(classObj) {
        classObj.variants = Object.create(null);
        for(const variantName of Object.keys(enumInfo.variants)) {
            if(!Object.hasOwn(enumInfo.variants, variantName)) {
                continue;
            }

            const variantClass = createEnumVariant(classObj, enumInfo.variants[variantName]);
            variantClass.methods = Object.create(classObj.methods);
            applyVTable(variantClass, enumInfo.variants[variantName].methods, enumInfo.variants[variantName].vtable);
            classObj.variants[variantName] = variantClass;
        }
    }

    enumType.specialize = specialize({
        customize(specialization) {
            specialization.staticMethods = bindStaticMethods(enumInfo.staticMethods, loadTokenArgs(enumType, specialization));
            specialization.methods = Object.create(null);
            applyVTable(specialization, enumInfo.methods, enumInfo.vtable);
            setupVariants(specialization);
        },
    });

    defineTokenArgSymbols(enumType, enumInfo);

    return enumType;
}

function createEnumVariant(proto, variant) {
    const variantClass = function(...args) {
        if(args.length - 1 !== variant.argCount) {
            throw new Error(`Invalid arguments count expected ${variant.argCount + 1}, actual ${args.length}`);
        }

        for(let i = 0; i < variant.argCount; ++i) {
            Object.defineProperty(
                this,
                `args_${i}`,
                {
                    value: args[i],
                    writable: false,
                },
            );
        }

        const fieldValues = args[args.length - 1];

        for(const field of variant.fields) {
            let fieldValue;
            if(field.name in fieldValues) {
                fieldValue = fieldValues[field.name];
            }
            else if("defaultValue" in field) {
                fieldValue = field.defaultValue;
            }
            else {
                throw new Error("Missing field: " + field.name);
            }

            if(field.mutable) {
                this["field_" + field.name] = fieldValue;
            }
            else {
                Object.defineProperty(
                    this,
                    "field_" + field.name,
                    {
                        value: fieldValue,
                        writable: false,
                    },
                );
            }
        }

        proto.call(this);
    };

    variantClass.prototype = Object.create(proto.prototype);
    variantClass.prototype.constructor = variantClass;

    return variantClass;
}

export function createTraitType(traitInfo) {
    const traitType = createNamedConstructor(traitInfo.name);

    switch(traitInfo.special) {
        case "exception": {
            traitType.prototype = Object.create(Error.prototype);
            break;
        }
    }

    traitType.specialize = specialize({
        customize(c) {
            c.staticMethods = bindStaticMethods(traitInfo.staticMethods, loadTokenArgs(traitType, c));
            c.methods = Object.create(null);
            applyVTable(c, traitInfo.methods, traitInfo.vtable);

            switch(traitInfo.special) {
                case "exception": {
                    Object.defineProperty(
                        c.prototype,
                        "message",
                        {
                            get() {
                                return resolve(this[c.methods["message$a$r$bstring$a$e"]]());
                            }
                        }
                    );
                    break;
                }
            }
        },
    });

    defineTokenArgSymbols(traitType, traitInfo);

    return traitType;
}


function abstractMethodImplementation() {
    throw new Error("Abstract Method Called");
}

function ambiguousMethodImplementation() {
    throw new Error("Ambiguous Method Implementation");
}


function applyVTable(c, methods, vtable) {
    const methodImpls = [];

    for(const [name, methodImpl] of Object.entries(methods)) {
        const sym = Symbol();
        if(c.methods !== undefined) {
            c.methods[name] = sym;
        }
        const method = methodImpl.method;
        methodImpls.push(method);
        if(method === null) {
            c.prototype[sym] = abstractMethodImplementation;
        }
        else if(typeof method === "function") {
            c.prototype[sym] = method;
        }
        else {
            throw new Error("A method must be null (abstract) or a function. Actual: " + typeof(method));
        }
    }

    for(const entry of vtable) {
        let method;
        switch(entry.target.type) {
            case "abstract":
                method = abstractMethodImplementation;
                break;

            case "ambiguous":
                method = ambiguousMethodImplementation;
                break;

            case "implementation":
                method = methodImpls[entry.target.methodIndex];
                if(method === null || method === undefined) {
                    throw new Error("Invalid target method index");
                }

                break;

            default:
                throw new Error("Unexpected target type");
        }

        c.prototype[entry.slotMethodSymbol.call(c)] = method;
    }
}

export function createInstanceDefinition(instanceInfo) {
    function createInstanceConstructor(base) {
        const constructor = function(...args) {
            if(args.length !== instanceInfo.argCount) {
                throw new Error(`Invalid arguments count expected ${instanceInfo.argCount}, actual ${args.length}`);
            }

            for(let i = 0; i < instanceInfo.argCount; ++i) {
                Object.defineProperty(
                    this,
                    `args_${i}`,
                    {
                        value: args[i],
                        writable: false,
                    },
                );
            }

            base.call(this);
        };
        
        constructor.prototype = Object.create(base.prototype);

        return constructor;
    }

    const singletonSymbol = Symbol();

    const inst = {};
    inst.specialize = specialize({
        getInstancePrototype(_inst, tokenArgs) {
            return createInstanceConstructor(instanceInfo.baseConstructor(...tokenArgs));
        },
        customize(specialized) {
            specialized.methods = Object.create(null);
            applyVTable(specialized, instanceInfo.methods, instanceInfo.vtable);

            if(instanceInfo.argCount === 0) {
                specialized.create = function(...args) {
                    let instance = this[singletonSymbol];
                    if(instance === undefined) {
                        instance = new this(...args);
                        this[singletonSymbol] = instance;
                    }
                    return instance;
                };
            }
            else {
                specialized.create = function(...args) {
                    return new this(...args);
                };
            }
        },
    });

    defineTokenArgSymbols(inst, instanceInfo);

    return inst;
}


class TrampolineDelay {
    constructor(delay) {
        this.delay = delay;
    }
}

export function delay(f) {
    return new TrampolineDelay(f);
}

export function resolve(value) {
    while(value instanceof TrampolineDelay) {
        const delay = value.delay;
        value = delay();
    }
    return value;
}
