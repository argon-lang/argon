

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
