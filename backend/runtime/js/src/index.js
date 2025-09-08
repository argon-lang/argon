
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

export const erasedType = {};

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

    return false;
}

function specialize(options = {}) {
    const specializations = [];

    function getSpecialization(...typeArgs) {
        const o = this;
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

        const customize = options.customize;
        if(customize) {
            customize(specializedClass);
        }

        specializations.push({
            typeArgs,
            specializedClass,
        });

        return specializedClass;
    }
    
    return getSpecialization;
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

    if(recordInfo.typeParameterCount !== 0) {
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

    if(enumInfo.typeParameterCount === 0) {
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



