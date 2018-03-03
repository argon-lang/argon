
interface TraitInfo {
    readonly traitSymbol: symbol;
    readonly baseTraits: ReadonlyArray<TraitInfo>;
    readonly methods: MethodSet;
    readonly metaClass: ClassInfo;
}

interface ClassInfo {
    readonly classConstructorFunction: (this: any, overloadSymbol: symbol, ...args: Array<any>) => void;
    readonly baseClass: ClassInfo | null;
    readonly baseTraits: ReadonlyArray<TraitInfo>;
    readonly classConstructors: ConstructorSet;
    readonly methods: MethodSet;
    readonly metaClass: ClassInfo;
}

type ConstructorSet = any;
type MethodSet = any;

export function createTrait(
    baseTraitsFunc: () => ReadonlyArray<TraitInfo>,
    methods: MethodSet,
    metaClassFunc: () => ClassInfo,
): TraitInfo {
    Object.freeze(methods);

    let baseTraitsCache: ReadonlyArray<TraitInfo> | undefined = undefined;
    let metaClassCache: ClassInfo | undefined = undefined;

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

function implementTraits(proto: {}, directTraits: ReadonlyArray<TraitInfo>) {
    const traits = [...directTraits];

    while(true) {
        const trait = traits.shift();
        if(!trait) {
            break;
        }

        const traitSymbol = trait.traitSymbol;

        if(traitSymbol in proto) {
            continue;
        }

        Object.defineProperty(proto, traitSymbol, { value: true });

        traits.push(...trait.baseTraits);
    }
}

export function createClass(
    baseClassFunc: () => ClassInfo | null,
    baseTraitsFunc: () => ReadonlyArray<TraitInfo>,
    constructors: ConstructorSet,
    methods: MethodSet,
    metaClassFunc: () => ClassInfo
): ClassInfo {

    Object.freeze(constructors);
    Object.freeze(methods);

    let baseClassCache: ClassInfo | null | undefined = undefined;
    let baseTraitsCache: ReadonlyArray<TraitInfo> | undefined = undefined;
    let ctorCache: ((overloadSymbol: symbol, ...args: Array<any>) => void) | undefined = undefined;
    let metaClassCache: ClassInfo | undefined = undefined;

    return Object.freeze({
        get classConstructorFunction() {
            if(!ctorCache) {
                const ctor = function(this: any, overloadSymbol: symbol, ...args: Array<any>) {
                    constructors[overloadSymbol].call(this, ...args);
                }

                const baseClass = this.baseClass;
                const baseTraits = this.baseTraits;

                const proto = Object.create(baseClass ? baseClass.classConstructorFunction : null);

                for(const methodSymbol in methods) {
                    Object.defineProperty(proto, methodSymbol, { value: methods[methodSymbol] });
                }

                implementTraits(proto, baseTraits);

                ctor.prototype = proto;
                ctorCache = ctor;
            }

            return ctorCache;
        },

        get baseClass() {
            if(baseClassCache === undefined) {
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

export namespace globalNamespace {
    export namespace Ar {

        export const Object = createTrait(
            () => [],
            {},
            () => createClass(
                () => globalNamespace.Ar.Class,
                () => [],
                {},
                {},
                () => globalNamespace.Ar.MetaClass
            )
        );
    
        export const Type = createClass(
            () => null,
            () => [ globalNamespace.Ar.Object ],
            {},
            {},
            () => globalNamespace.Ar.MetaClass
        );
    
        export const Trait = createClass(
            () => globalNamespace.Ar.Type,
            () => [],
            {},
            {},
            () => globalNamespace.Ar.MetaClass
        );
    
        export const Class = createClass(
            () => globalNamespace.Ar.Type,
            () => [],
            {},
            {},
            () => globalNamespace.Ar.MetaClass
        );
    
        export const MetaClass = createClass(
            () => globalNamespace.Ar.Class,
            () => [],
            {},
            {},
            () => globalNamespace.Ar.MetaClass
        );

    }
}
