export function createTrait(baseTraitsFunc, methods, metaClassFunc) {
    Object.freeze(methods);
    let baseTraitsCache = undefined;
    let metaClassCache = undefined;
    return Object.freeze({
        traitSymbol: Symbol(),
        get baseTraits() {
            return baseTraitsCache = baseTraitsCache || baseTraitsFunc();
        },
        methods: methods,
        get metaClass() {
            return metaClassCache = metaClassCache || metaClassFunc();
        },
    });
}
function implementTraits(proto, directTraits) {
    const traits = [...directTraits];
    while (true) {
        const trait = traits.shift();
        if (!trait) {
            break;
        }
        const traitSymbol = trait.traitSymbol;
        if (traitSymbol in proto) {
            continue;
        }
        Object.defineProperty(proto, traitSymbol, { value: true });
        traits.push(...trait.baseTraits);
    }
}
export function createClass(baseClassFunc, baseTraitsFunc, constructors, methods, metaClassFunc) {
    Object.freeze(constructors);
    Object.freeze(methods);
    let baseClassCache = undefined;
    let baseTraitsCache = undefined;
    let ctorCache = undefined;
    let metaClassCache = undefined;
    return Object.freeze({
        get classConstructorFunction() {
            if (!ctorCache) {
                const ctor = function (overloadSymbol, ...args) {
                    constructors[overloadSymbol].call(this, ...args);
                };
                const baseClass = this.baseClass;
                const baseTraits = this.baseTraits;
                const proto = Object.create(baseClass ? baseClass.classConstructorFunction : null);
                for (const methodSymbol in methods) {
                    Object.defineProperty(proto, methodSymbol, { value: methods[methodSymbol] });
                }
                implementTraits(proto, baseTraits);
                ctor.prototype = proto;
                ctorCache = ctor;
            }
            return ctorCache;
        },
        get baseClass() {
            if (baseClassCache === undefined) {
                baseClassCache = baseClassFunc();
            }
            return baseClassCache;
        },
        get baseTraits() {
            return baseTraitsCache = baseTraitsCache || baseTraitsFunc();
        },
        classConstructors: constructors,
        methods: methods,
        get metaClass() {
            return metaClassCache = metaClassCache || metaClassFunc();
        },
    });
}
export var globalNamespace;
(function (globalNamespace) {
    let Ar;
    (function (Ar) {
        Ar.Object = createTrait(() => [], {}, () => createClass(() => globalNamespace.Ar.Class, () => [], {}, {}, () => globalNamespace.Ar.MetaClass));
        Ar.Type = createClass(() => null, () => [globalNamespace.Ar.Object], {}, {}, () => globalNamespace.Ar.MetaClass);
        Ar.Trait = createClass(() => globalNamespace.Ar.Type, () => [], {}, {}, () => globalNamespace.Ar.MetaClass);
        Ar.Class = createClass(() => globalNamespace.Ar.Type, () => [], {}, {}, () => globalNamespace.Ar.MetaClass);
        Ar.MetaClass = createClass(() => globalNamespace.Ar.Class, () => [], {}, {}, () => globalNamespace.Ar.MetaClass);
    })(Ar = globalNamespace.Ar || (globalNamespace.Ar = {}));
})(globalNamespace || (globalNamespace = {}));
