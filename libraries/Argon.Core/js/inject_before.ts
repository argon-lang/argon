
export const stringValueSymbol = Symbol();
export const intValueSymbol = Symbol();
export const boolValueSymbol = Symbol();

export type NamespacePath = ReadonlyArray<string>;

export interface BlankType {
    readonly type: "blank";
}

export interface TraitType {
    readonly type: "trait";
    readonly arTrait: ArTrait;
    readonly arguments: ReadonlyArray<SigType>;
}

export interface ClassType {
    readonly type: "class";
    readonly arClass: ArClass;
    readonly arguments: ReadonlyArray<SigType>;
}

export interface DataConstructorType {
    readonly type: "data-constructor";
    readonly dataConstructor: DataConstructor;
    readonly arguments: ReadonlyArray<SigType>;
}

export interface TupleType {
    readonly type: "tuple";
    readonly elements: ReadonlyArray<SigType>;
}

export interface FunctionType {
    readonly type: "function";
    readonly argumentType: SigType;
    readonly resultType: SigType;
}

export type SigType = BlankType | TraitType | ClassType | DataConstructorType | TupleType | FunctionType;

export interface ParameterOnlySignature {
    readonly parameterTypes: ReadonlyArray<SigType>;
}

export interface Signature extends ParameterOnlySignature {
    readonly resultType: SigType;
}

export interface GlobalClassDescriptor {
    readonly type: "global";
    readonly ns: NamespacePath;
    readonly name: string | symbol;
    readonly sig: ParameterOnlySignature;
}

export type GlobalTraitDescriptor = GlobalClassDescriptor;
export type GlobalDataConstructorDescriptor = GlobalClassDescriptor;

export type ClassDescriptor = GlobalClassDescriptor;
export type TraitDescriptor = GlobalTraitDescriptor;

export interface GlobalFunctionDescriptor {
    readonly type: "global";
    readonly ns: NamespacePath;
    readonly name: string | symbol;
    readonly sig: Signature;
}

export interface OperatorName {
    readonly type: "operator";
    readonly name: string;
}

export type GlobalName = string | symbol | OperatorName;

function globalNameEqual(a: GlobalName, b: GlobalName): boolean {
    if(typeof(a) === "string" && typeof(b) === "string") {
        return a === b;
    }
    else if(typeof(a) === "symbol" && typeof(b) === "symbol") {
        return a === b;
    }
    else if(typeof(a) === "object" && typeof(b) === "object") {
        if(a.type === "operator" && b.type === "operator") {
            return a.name === b.name;
        }
        else {
            return false;
        }
    }
    else {
        return false;
    }
}

export interface ArModule {
    globalClass(ns: NamespacePath, name: GlobalName, sig: ParameterOnlySignature): ArClass;
    globalTrait(ns: NamespacePath, name: GlobalName, sig: ParameterOnlySignature): ArTrait;
    globalDataConstructor(ns: NamespacePath, name: GlobalName, sig: ParameterOnlySignature): DataConstructor;
    globalFunction(ns: NamespacePath, name: GlobalName, sig: Signature): ArFunc;
}

export interface ArTrait {
    implement(proto: {}): void;
    isInstance(value: any): boolean;
    copyTraitParameters(src: any, dest: any): void;
    createTraitObject(...traitParams: readonly any[]): void;
    initializeClassObject(classObject: any, traitParams: () => readonly any[]): void;
    getParameter(obj: any, index: number): any;

    method(name: NamedMethodName, sig: Signature): ArMethod;
    staticMethod(name: NamedMethodName, sig: Signature): ArMethod;
}

export interface MutatorMemberName {
    readonly type: "mutator";
    readonly name: string;
}

export enum SpecialMethodName {
    Call = 1,
}

export type NamedMethodName = string | MutatorMemberName | SpecialMethodName;
export type MethodName = NamedMethodName | null;

export interface ArClass {
    inherit(): {};
    isInstance(value: any): boolean;
    createUninitializedInstance(classObject: any): any;
    copyClassParameters(src: any, dest: any): void;
    createClassObject(...classParams: readonly any[]): any;
    initializeSubClassObject(classObject: any, classParams: () => readonly any[]): void;
    getParameter(obj: any, index: number): any;

    field(name: string): ClassField;
    method(name: NamedMethodName, sig: Signature): ArMethod;
    staticMethod(name: NamedMethodName, sig: Signature): ArMethod;
    constructor(sig: ParameterOnlySignature): ClassConstructor;
}

export interface ClassField {
    initialize(obj: any, value: any): void;
    read(obj: any): any;
    write(obj: any, value: any): void;
}

export interface DataConstructor {
    isInstance(value: any): boolean;
    createTypeObject(...typeParams: readonly any[]): any;
    createInstance(typeObject: any, ...args: any[]): any;
    method(name: NamedMethodName, sig: Signature): ArMethod;
}

export interface ArFunc {
    invoke(...args: any[]): any;
}

export interface ArMethod {
    override(proto: {}, implementation: (instance: any, ...args: any[]) => any): void;
    invoke(instance: any, ...args: any[]): any;
    invokeNonVirtual: (instance: any,  ...args: any[]) => any;
}

export interface ClassConstructor {
    initializeInstance(instance: any, ...args: any[]): void;
    invoke(classObject: any, ...args: any[]): any;
}


export interface ModuleCreator {
    globalClasses?: ReadonlyArray<GlobalClassDescriptor & { create(module: ArModule): ArClass; }>;
    globalTraits?: ReadonlyArray<GlobalTraitDescriptor & { create(module: ArModule): ArTrait; }>;
    globalDataConstructors?: ReadonlyArray<GlobalDataConstructorDescriptor & { create(module: ArModule): DataConstructor; }>;
    globalFunctions?: ReadonlyArray<GlobalFunctionDescriptor & { create(module: ArModule): ArFunc; }>;
}

function namespacePathEqual(ns1: NamespacePath, ns2: NamespacePath): boolean {
    if(ns1.length !== ns2.length) {
        return false;
    }

    for(let i = 0; i < ns1.length; ++i) {
        if(ns1[i] !== ns2[i]) {
            return false;
        }
    }

    return true;
}

function sigTypeEqual(t1: SigType, t2: SigType): boolean {
    function typeArrayEqual(a1: ReadonlyArray<SigType>, a2: ReadonlyArray<SigType>): boolean {
        if(a1.length !== a2.length) {
            return false;
        }

        for(let i = 0; i < a1.length; ++i) {
            if(!sigTypeEqual(a1[i], a2[i])) {
                return false;
            }
        }

        return true;
    }

    if(t1.type === "blank" && t2.type === "blank") {
        return true;
    }
    else if(t1.type === "class" && t2.type === "class") {
        return t1.arClass === t2.arClass && typeArrayEqual(t1.arguments, t2.arguments);
    }
    else if(t1.type === "trait" && t2.type === "trait") {
        return t1.arTrait === t2.arTrait && typeArrayEqual(t1.arguments, t2.arguments);
    }
    else if(t1.type === "data-constructor" && t2.type === "data-constructor") {
        return t1.dataConstructor === t2.dataConstructor && typeArrayEqual(t1.arguments, t2.arguments);
    }
    else if(t1.type === "tuple" && t2.type === "tuple") {
        return typeArrayEqual(t1.elements, t2.elements);
    }
    else if(t1.type === "function" && t2.type === "function") {
        return sigTypeEqual(t1.argumentType, t2.argumentType) && sigTypeEqual(t1.resultType, t2.resultType);
    }
    else {
        return false;
    }
}

function signatureEqual(sig1: Signature, sig2: Signature): boolean {
    return paramSignatureEqual(sig1, sig2) && sigTypeEqual(sig1.resultType, sig2.resultType);
}

function paramSignatureEqual(sig1: ParameterOnlySignature, sig2: ParameterOnlySignature): boolean {
    if(sig1.parameterTypes.length !== sig2.parameterTypes.length) {
        return false;
    }

    for(let i = 0; i < sig1.parameterTypes.length; ++i) {
        if(!sigTypeEqual(sig1.parameterTypes[i], sig2.parameterTypes[i])) {
            return false;
        }
    }

    return true;

}

function cacheValue<TSelf, TDesc, T>(self: TSelf, cache: Array<[TDesc, T]>, creators: ReadonlyArray<TDesc & { create(self: TSelf): T }> | undefined, matches: (desc: TDesc) => boolean, errorMessage: string): T {
    for(let [desc, value] of cache) {
        if(matches(desc)) {
            return value;
        }
    }

    if(creators !== undefined) {
        for(let creator of creators) {
            if(matches(creator)) {
                let value = creator.create(self);
                let desc = { ...creator };
                delete desc.create;
                cache.push([ desc, value ]);
                return value;
            }
        }
    }

    throw new Error(errorMessage);
}

export function createModule(creator: ModuleCreator): ArModule {
    const classCache: Array<[GlobalClassDescriptor, ArClass]> = [];
    const traitCache: Array<[GlobalTraitDescriptor, ArTrait]> = [];
    const dataCtorCache: Array<[GlobalDataConstructorDescriptor, DataConstructor]> = [];
    const funcCache: Array<[GlobalFunctionDescriptor, ArFunc]> = [];

    return {
        globalClass(ns: NamespacePath, name: GlobalName, sig: ParameterOnlySignature): ArClass {

            function matches(desc: GlobalClassDescriptor): boolean {
                return namespacePathEqual(desc.ns, ns) && globalNameEqual(desc.name, name) && paramSignatureEqual(desc.sig, sig);
            }

            return cacheValue(this, classCache, creator.globalClasses, matches, "Could not find class");
        },

        globalTrait(ns: NamespacePath, name: GlobalName, sig: ParameterOnlySignature): ArTrait {

            function matches(desc: GlobalTraitDescriptor): boolean {
                return namespacePathEqual(desc.ns, ns) && globalNameEqual(desc.name, name) && paramSignatureEqual(desc.sig, sig);
            }

            return cacheValue(this, traitCache, creator.globalTraits, matches, "Could not find trait");
        },

        globalDataConstructor(ns: NamespacePath, name: GlobalName, sig: ParameterOnlySignature): DataConstructor {

            function matches(desc: GlobalDataConstructorDescriptor): boolean {
                return namespacePathEqual(desc.ns, ns) && globalNameEqual(desc.name, name) && paramSignatureEqual(desc.sig, sig);
            }

            return cacheValue(this, dataCtorCache, creator.globalDataConstructors, matches, "Could not find data constructor");
        },

        globalFunction(ns: NamespacePath, name: GlobalName, sig: Signature): ArFunc {

            function matches(desc: GlobalFunctionDescriptor): boolean {
                return namespacePathEqual(desc.ns, ns) && globalNameEqual(desc.name, name) && signatureEqual(desc.sig, sig);
            }

            return cacheValue(this, funcCache, creator.globalFunctions, matches, "Could not find function");
        },
    };
}


export interface ConstructorBinding {
    readonly sig: ParameterOnlySignature;
}

export interface MethodBinding {
    readonly name: MethodName;
    readonly sig: Signature;
}

export interface FieldCreator {
    readonly name: string;
    readonly mutability: "mutable" | "non-mutable";
}

export interface BaseTypeInfo<T> {
    baseType: T;
    parameterMapping(...params: readonly any[]): readonly any[];
}

function defineMethod<TSelf>(this: TSelf, methodArr: Array<[MethodBinding, ArMethod]>, creator: MethodBinding & { create(self: TSelf): ArMethod; }): ArMethod {
    const binding: MethodBinding = {
        name: creator.name,
        sig: creator.sig,
    };

    const method = creator.create(this);
    methodArr.push([ binding, method ]);
    return method;
}

const methodMatches = (name: NamedMethodName, sig: Signature) => (binding: MethodBinding): boolean => {
    if(binding.name === null) {
        return false;
    }
    else if(typeof(binding.name) === "object" && typeof(name) === "object") {
        if(binding.name.type === "mutator" && name.type === "mutator") {
            if(binding.name.name !== name.name) {
                return false;
            }
        }
        else {
            return false;
        }
    }
    else if(binding.name !== name) {
        return false;
    }

    return signatureEqual(binding.sig, sig);
};

export interface ClassCreator {
    readonly baseClass?: BaseTypeInfo<ArClass>;
    readonly baseTraits?: ReadonlyArray<BaseTypeInfo<ArTrait>>;

    readonly constructors?: ReadonlyArray<ConstructorBinding & { create(cls: ArClass): ClassConstructor; }>;
    readonly methods?: ReadonlyArray<MethodBinding & { create(cls: ArClass): ArMethod; }>;
    readonly staticMethods?: ReadonlyArray<MethodBinding & { create(cls: ArClass): ArMethod; }>;
    readonly fields?: ReadonlyArray<FieldCreator>;
    
    loadVTable(proto: {}): void;
}

export function createClass(creator: ClassCreator): ArClass {
    const constructorCache: Array<[ConstructorBinding, ClassConstructor]> = [];
    const methods: Array<[MethodBinding, ArMethod]> = [];
    const staticMethods: Array<[MethodBinding, ArMethod]> = [];

    const classParamSymbol = Symbol();
    
    const fields: Map<string, ClassField> = new Map((creator.fields || []).map(fieldCreator => {
        const symbol = Symbol();
        
        const field: ClassField = {
            initialize: fieldCreator.mutability === "mutable"
                ? function(obj: any, value: any): void { obj[symbol] = value; }
                : function(obj: any, value: any): void {
                    Object.defineProperty(obj, symbol, { configurable: false, writable: false, value });
                },

            read(obj: any): any {
                return obj[symbol];
            },
            write(obj: any, value: any): void {
                obj[symbol] = value;
            },
        };

        return [fieldCreator.name, field]
    }));

    let classPrototype: {} | null = null;
    let baseClass: BaseTypeInfo<ArClass> | null = null;
    let baseTraits: ReadonlyArray<BaseTypeInfo<ArTrait>> = [];

    function getPrototype(this: ArClass): {} {
        if(classPrototype !== null) {
            return classPrototype;
        }

        baseClass = creator.baseClass || null;
        baseTraits = creator.baseTraits || [];

        const proto = baseClass === null ? Object.create(null) : baseClass.baseType.inherit();
        classPrototype = proto;
        
        for(let baseTrait of baseTraits) {
            baseTrait.baseType.implement(proto);
        }

        for(let methodCreator of creator.methods || []) {
            const method = defineMethod.call(this, methods, methodCreator);
        }

        creator.loadVTable(proto);

        Object.freeze(proto);

        return proto;
    }

    let staticPrototype: {} | null = null;
    function getStaticPrototype(this: ArClass): {} {
        if(staticPrototype !== null) {
            return staticPrototype;
        }

        const proto = Object.create(null);
        
        for(let methodCreator of creator.staticMethods || []) {
            const method = defineMethod.call(this, staticMethods, methodCreator);
            method.override(proto, method.invokeNonVirtual);
        }

        staticPrototype = proto;
        return proto;
    }

    return {
        inherit(): {} {
            return Object.create(getPrototype.call(this));
        },

        isInstance(value: any): boolean {
            return Object.isPrototypeOf.call(getPrototype.call(this), value);
        },

        createUninitializedInstance(classObject: any): any {
            const instance = Object.create(getPrototype.call(this));
            this.copyClassParameters(classObject, instance);
            return instance;
        },

        copyClassParameters(src: any, dest: any): void {
            Object.defineProperty(dest, classParamSymbol, { configurable: false, writable: false, value: src[classParamSymbol] });
            baseClass?.baseType.copyClassParameters(src, dest);
            for(let baseTrait of baseTraits) {
                baseTrait.baseType.copyTraitParameters(src, dest);
            }
        },

        constructor(sig: ParameterOnlySignature): ClassConstructor {
            function matches(binding: ConstructorBinding): boolean {
                return paramSignatureEqual(binding.sig, sig);
            }
    
            return cacheValue(this, constructorCache, creator.constructors, matches, "Could not find constructor");
        },

        createClassObject(...classParams: readonly any[]): any {
            const instance = Object.create(getStaticPrototype.call(this));
            this.initializeSubClassObject(instance, () => classParams);
            return instance;
        },

        initializeSubClassObject(classObject: any, classParams: () => readonly any[]): any {
            getPrototype.call(this);

            Object.defineProperty(classObject, classParamSymbol, { configurable: false, get: classParams });
            if(baseClass !== null) {
                baseClass.baseType.initializeSubClassObject(classObject, () => baseClass!.parameterMapping(...classParams()))
            }
            for(let baseTrait of baseTraits) {
                baseTrait.baseType.initializeClassObject(classObject, () => baseTrait.parameterMapping(...classParams()));
            }
        },

        getParameter(obj: any, index: number): any {
            return obj[classParamSymbol][index];
        },

        method(name: NamedMethodName, sig: Signature): ArMethod {
            getPrototype.call(this);

            for(let [binding, method] of methods) {
                if(methodMatches(name, sig)(binding)) {
                    return method;
                }
            }

            throw new Error("Could not find method");
        },

        staticMethod(name: NamedMethodName, sig: Signature): ArMethod {
            getStaticPrototype.call(this);

            for(let [binding, method] of staticMethods) {
                if(methodMatches(name, sig)(binding)) {
                    return method;
                }
            }

            throw new Error("Could not find static method");
        },

        field(name: string): ClassField {
            const field = fields.get(name);
            if(field === undefined) {
                throw new Error("Could not find field");
            }

            return field;
        },
    };
}


export interface MethodCreator {
    implementation(instance: any, ...args: any[]): any;
}

export function createMethod(creator: MethodCreator): ArMethod {
    const symbol = Symbol();

    const impl = creator.implementation;

    return {
        invoke(instance: any, ...args: any[]): any {
            return instance[symbol](...args);
        },

        invokeNonVirtual: impl,

        override(proto: {}, implementation: (instance: any, ...args: any[]) => any[]): void {
            Object.defineProperty(proto, symbol, { configurable: false, writable: false, value: function(...args: any[]) { return implementation(this, ...args); } });
        },
    };
}

export interface ClassConstructorCreator {
    implementation(instance: any, ...args: any[]): void;
}

export function createClassConstructor(creator: ClassConstructorCreator, cls: ArClass): ClassConstructor {
    return {
        initializeInstance: creator.implementation,

        invoke(classObject: any, ...args: any[]): any {
            const instance = cls.createUninitializedInstance(classObject);
            this.initializeInstance(instance, ...args);
            return instance;
        },
    };
}

export interface TraitCreator {
    readonly baseTraits?: ReadonlyArray<BaseTypeInfo<ArTrait>>;

    readonly methods?: ReadonlyArray<MethodBinding & { create(trait: ArTrait): ArMethod; }>;
    readonly staticMethods?: ReadonlyArray<MethodBinding & { create(trait: ArTrait): ArMethod; }>;
}

export function createTrait(creator: TraitCreator): ArTrait {
    const symbol = Symbol();
    const traitParamSymbol = Symbol();
    
    const methodCache: Array<[MethodBinding, ArMethod]> = [];
    const staticMethods: Array<[MethodBinding, ArMethod]> = [];

    let baseTraits: ReadonlyArray<BaseTypeInfo<ArTrait>> | null = null;
    function getBaseTraits(): ReadonlyArray<BaseTypeInfo<ArTrait>> {
        if(baseTraits !== null) {
            return baseTraits;
        }

        const bt = creator.baseTraits || [];
        baseTraits = bt;
        return bt;
    }

    let staticPrototype: {} | null = null;
    function getStaticPrototype(this: ArTrait): {} {
        if(staticPrototype !== null) {
            return staticPrototype;
        }

        const proto = Object.create(null);
        
        for(let methodCreator of creator.staticMethods || []) {
            const method = defineMethod.call(this, staticMethods, methodCreator);
            method.override(proto, method.invokeNonVirtual);
        }

        staticPrototype = proto;
        return proto;
    }

    return {
        implement(proto: {}): void {
            Object.defineProperty(proto, symbol, { configurable: false, writable: false, value: true });
        },

        isInstance(value: any): boolean {
            return traitParamSymbol in value;
        },

        copyTraitParameters(src: any, dest: any): void {
            if(traitParamSymbol in dest) {
                return;
            }

            Object.defineProperty(dest, traitParamSymbol, { configurable: false, writable: false, value: src[traitParamSymbol] });
            for(let baseTrait of getBaseTraits()) {
                baseTrait.baseType.copyTraitParameters(src, dest);
            }
        },

        createTraitObject(...traitParams: readonly any[]): void {
            const instance = Object.create(getStaticPrototype.call(this));
            this.initializeClassObject(instance, () => traitParams);
            return instance;
        },

        initializeClassObject(classObject: any, traitParams: () => readonly any[]): void {
            if(traitParamSymbol in classObject) {
                return;
            }

            Object.defineProperty(classObject, traitParamSymbol, { configurable: false, get: traitParams });
            
            for(let baseTrait of getBaseTraits()) {
                baseTrait.baseType.initializeClassObject(classObject, () => baseTrait.parameterMapping(...traitParams()));
            }
        },

        getParameter(obj: any, index: number): any {
            return obj[traitParamSymbol][index];
        },
    
        method(name: NamedMethodName, sig: Signature): ArMethod {

            const matches = methodMatches(name, sig);

            return cacheValue(this, methodCache, creator.methods, matches, "Could not find method");

        },

        staticMethod(name: NamedMethodName, sig: Signature): ArMethod {
            getStaticPrototype.call(this);

            for(let [binding, method] of staticMethods) {
                if(methodMatches(name, sig)(binding)) {
                    return method;
                }
            }

            throw new Error("Could not find static method");
        }
    };
}



export interface DataConstructorCreator {
    readonly instanceTrait: BaseTypeInfo<ArTrait>;
    constructor(instance: any, ...args: any[]): void;

    readonly methods?: ReadonlyArray<MethodBinding & { create(dataCtor: DataConstructor): ArMethod; }>;
    
    loadVTable(proto: {}): void;
}

export function createDataConstructor(creator: DataConstructorCreator): DataConstructor {
    const methods: Array<[MethodBinding, ArMethod]> = [];

    const classParamSymbol = Symbol();

    let ctorPrototype: {} | null = null;
    let instanceTrait: BaseTypeInfo<ArTrait> | null = null;

    function getPrototype(this: DataConstructor): {} {
        if(ctorPrototype !== null) {
            return ctorPrototype;
        }
        
        const proto = Object.create(null);
        
        const instTrait = creator.instanceTrait;
        ctorPrototype = proto;
        instanceTrait = instTrait;
        
        instTrait.baseType.implement(proto);

        for(let methodCreator of creator.methods || []) {
            defineMethod.call(this, methods, methodCreator);
        }

        creator.loadVTable(proto);

        Object.freeze(proto);
        return proto;
    }

    return {

        isInstance(value: any): boolean {
            return Object.isPrototypeOf.call(getPrototype.call(this), value);
        },

        createInstance(typeObject: any, ...args: any[]): any {
            const instance = Object.create(getPrototype.call(this));
            Object.defineProperty(instance, classParamSymbol, { configurable: false, writable: false, value: typeObject[classParamSymbol] });
            instanceTrait!.baseType.copyTraitParameters(typeObject, instance);
            creator.constructor(instance, ...args);
            return instance;
        },

        createTypeObject(...classParams: readonly any[]): any {
            getPrototype.call(this);
            const instance = Object.create(null);
            Object.defineProperty(instance, classParamSymbol, { configurable: false, writable: false, value: Object.freeze([...classParams]) });
            instanceTrait!.baseType.initializeClassObject(instance, () => instanceTrait!.parameterMapping(...classParams));
            return instance;
        },

        method(name: NamedMethodName, sig: Signature): ArMethod {
            getPrototype.call(this);

            for(let [binding, method] of methods) {
                if(methodMatches(name, sig)(binding)) {
                    return method;
                }
            }

            throw new Error("Could not find method");
        },
    };
}


export interface FunctionCreator {
    implementation(...args: any[]): any;
}

export function createFunction(creator: FunctionCreator): ArFunc {
    return {
        invoke: creator.implementation,
    };
}

