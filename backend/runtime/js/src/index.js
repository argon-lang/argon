
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

    return false;
}

function specialize(options = {}) {
    const specializations = [];

    function getSpecialization(...typeArgs) {
        const o = options.getInstancePrototype ? options.getInstancePrototype(this, typeArgs) : this;
        for(const spec of specializations) {
            if(isSameType(spec.typeArgs, typeArgs)) {
                return spec.specializedClass;
            }
        }

        const specializedClass = function(...args) {
            o.call(this, ...args);
        };
        specializedClass.prototype = Object.create(o.prototype);
        specializedClass.typeArgs = typeArgs;

        if(options.customize) {
            options.customize(specializedClass);
        }

        specializations.push({
            typeArgs,
            specializedClass,
        });

        return specializedClass;
    }
    
    return getSpecialization;
}

function lazyFunctionBuilder(create) {
    let f = null;
    return function(...args) {
        if(f === null) {
            f = create();
        }

        if(new.target) {
            return new f(...args);
        }
        else {
            return f(...args);
        }
    }
}


export function createRecordType(recordInfo) {
    const recordType = function(values) {
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
    };

    if(recordInfo.tokenParameterCount !== 0) {
        recordType.specialize = specialize();
    }

    return recordType;
}

export function createEnumType(enumInfo) {
    const enumType = function() {};

    function setupVariants(classObj) {
        classObj.variants = Object.create(null);
        for(const variantName of Object.keys(enumInfo.variants)) {
            if(!Object.hasOwn(enumInfo.variants, variantName)) {
                continue;
            }

            classObj.variants[variantName] = createEnumVariant(classObj, enumInfo.variants[variantName]);
        }
    }

    if(enumInfo.tokenParameterCount === 0) {
        setupVariants(enumType);
    }
    else {
        enumType.specialize = specialize({
            customize(specialization) {
                setupVariants(specialization);
            },
        });
    }

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

    variantClass.prototype = Object.create(proto);

    return variantClass;
}

export function createTraitType(traitInfo) {
    const traitType = function() {};

    if(traitInfo.tokenParameterCount === 0) {
        traitType.methods = Object.create(null);
        applyVTable(traitType, traitInfo.methods, traitInfo.vtable);
    }
    else {
        traitType.specialize = specialize({
            customize(c) {
                c.methods = Object.create(null);
                applyVTable(c, traitInfo.methods, traitInfo.vtable);
            },
        });
    }

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
        c.methods[name] = sym;
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

    if(instanceInfo.tokenParameterCount === 0) {
        return lazyFunctionBuilder(() => {
            const inst = createInstanceConstructor(instanceInfo.baseConstructor());
            inst.methods = Object.create(null);
            applyVTable(inst, instanceInfo.methods, instanceInfo.vtable);
            return inst;
        });
    }
    else {
        const inst = {};
        inst.specialize = specialize({
            getInstancePrototype(_inst, typeArgs) {
                return createInstanceConstructor(instanceInfo.baseConstructor(...typeArgs));
            },
            customize(specialized) {
                specialized.methods = Object.create(null);
                applyVTable(specialized, instanceInfo.methods, instanceInfo.vtable);
            },
        });
        return inst;
    }
}


