
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

export function specialize() {
    const o = this;
    const specializations = [];

    function getSpecialization(typeArgs) {
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

        specializations.push({
            typeArgs,
            specializedClass,
        });
    }
    
    return function(...typeArgs) {
        return getSpecialization(typeArgs);
    };
}
