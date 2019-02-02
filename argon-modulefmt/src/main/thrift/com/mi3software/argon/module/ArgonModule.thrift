namespace java com.mi3software.argon.module

struct Namespace {
    1: list<string> parts;
}

struct FileSpec {
    1: required i32 fileID;
    2: required string name;
}

struct UnnamedGlobalName {
    1: required i32 fileID;
    2: required i32 index;
}

union GlobalName {
    1: string normalName;
    2: UnnamedGlobalName unnamed;
}

union Type {
    2: TraitType traitType;
    3: ClassType classType;
    4: ConstructorInstanceType constructorInstanceType;
}

struct ParameterElement {
    1: string name;
    2: Type paramType;
}

struct Parameter {
    1: list<ParameterElement> elements;
}

struct EffectInfo {
    1: bool isPure;
}

enum AccessModifier {
    Invalid = 0;
    Public = 1;
    Internal = 2;
    Protected = 3;
    ProtectedInternal = 4;
    Private = 5;
    PrivateInternal = 6;
}

struct ErrorType {

}

struct TraitType {
    1: i32 traitId;
    2: list<Type> typeArguments;
}

struct ClassType {
    1: i32 classId;
    2: list<Type> typeArguments;
}

struct ConstructorInstanceType {
    1: i32 constructorId;
    2: list<Type> typeArguments;
}

struct ModuleReference {
    1: string name;
}

union TraitDescriptor {
    1: TraitDescriptorInNamespace inNamespace;
}

struct TraitDescriptorInNamespace {
    1: FileSpec fileSpec;
    2: Namespace ns;
    3: GlobalName name;
    4: AccessModifier accessModifier;
}

struct TraitSignature {
    1: list<Parameter> parameters;
    2: list<TraitType> baseTraits;
}

struct TraitReference {
    1: i32 moduleId;
    2: TraitDescriptor descriptor;
}

struct TraitDefinition {
    1: TraitDescriptor descriptor;
    2: bool isSealed;
    3: TraitSignature signature;
}

union Trait {
    1: TraitReference traitRef;
    2: TraitDefinition traitDef;
}


struct ClassField {
    1: bool isMutable;
    2: string name;
    3: Type fieldType;
}

union ClassDescriptor {
    1: ClassDescriptorInNamespace inNamespace;
}

struct ClassDescriptorInNamespace {
    1: FileSpec fileSpec;
    2: Namespace ns;
    3: GlobalName name;
    4: AccessModifier accessModifier;
}

struct ClassDescriptorMetaClass {
    1: ClassDescriptor ownerClass;
}

struct ClassDescriptorTraitMetaClass {
    1: TraitDescriptor ownerTrait;
}

struct ClassReference {
    1: i32 moduleId;
    2: ClassDescriptor descriptor;
}

struct ClassDefinition {
    1: ClassDescriptor descriptor;
    2: bool isOpen;
    3: bool isSealed;
    4: bool isAbstract;
    5: ClassSignature signature;
    6: list<ClassField> fields;
}

struct ClassSignature {
    1: list<Parameter> parameters;
    2: optional ClassType baseClass;
    3: list<TraitType> baseTraits;
}

union Class {
    1: ClassReference classRef;
    2: ClassDefinition classDef;
}



union MethodOwnerDescriptor {
    1: TraitDescriptor traitDescriptor;
    2: TraitDescriptor traitObjectDescriptor;
    3: ClassDescriptor classDescriptor;
    4: ClassDescriptor classObjectDescriptor;
}



union DataConstructorDescriptor {
    1: DataConstructorDecscriptorInNamespace inNamespace;
}

struct DataConstructorDecscriptorInNamespace {
    1: FileSpec fileSpec;
    2: Namespace ns;
    3: GlobalName name;
    4: AccessModifier accessModifier;
}

struct DataConstructorSignature {
    1: list<Parameter> parameters;
    2: TraitType instanceType;
}

struct DataConstructorReference {
    1: i32 moduleId;
    2: DataConstructorDescriptor descriptor;
}

struct DataConstructorDefinition {
    1: DataConstructorDescriptor descriptor;
    2: bool isSealed;
    3: DataConstructorSignature signature;
    4: i32 metaClassId;
}

union DataConstructor {
    1: DataConstructorReference dataCtorRef;
    2: DataConstructorDefinition dataCtorDef;
}



union Expression {
    1: BindVariable bindVariable;
    2: ClassConstructorCall classConstructorCall;
    3: ConstructorCall constructorCall;
    4: FunctionCall functionCall;
    5: IfElse ifElse;
    6: IgnoreValue ignoreValue;
    7: LoadConstantBool loadConstantBool;
    8: LoadConstantInt loadConstantInt;
    9: LoadConstantString loadConstantString;
    10: LoadLambda loadLambda;
    11: LoadTypeValue loadTypeValue;
    12: LoadVariable loadVariable;
    13: MethodCall methodCall;
    14: OperationAdd operationAdd;
    15: OperationAdd operationSub;
    16: OperationAdd operationMul;
    17: OperationAdd operationDiv;
    18: OperationAdd operationEquals;
    19: OperationAdd operationNotEquals;
    20: PatternMatch patternMatch;
    21: Sequence sequence;
    22: StoreVariable storeVariable;
    23: TupledValues tupledValues;
    24: Upcast upcast;
}



union FunctionDescriptor {
    1: FunctionDescriptorInNamespace inNamespace;
}

struct FunctionDescriptorInNamespace {
    1: FileSpec fileSpec;
    2: Namespace ns;
    3: GlobalName name;
    4: AccessModifier accessModifier;
}

struct FunctionSignature {
    1: list<Parameter> parameters;
    2: Type returnType;
}

struct FunctionImplementation {
    1: list<LocalVariable> localVariables;
    2: Expression body;
}

struct FunctionReference {
    1: i32 moduleId;
    2: FunctionDescriptor descriptor;
}

struct FunctionDefinition {
    1: FunctionDescriptor descriptor;
    2: EffectInfo effects;
    3: FunctionSignature signature;
    4: optional FunctionImplementation implementation;
}

union Function {
    1: FunctionReference funcRef;
    2: FunctionDefinition funcDef;
}



enum SpecialMemberName {
    Call = 0;
    New = 1;
}

union MemberName {
    1: string name;
    2: i32 unnamedMemberIndex;
    3: SpecialMemberName specialMemberName;
}

struct MethodDescriptor {
    1: MemberName name;
    2: AccessModifier accessModifier;
    3: MethodOwnerDescriptor instanceType;
}

struct MethodSignature {
    1: list<Parameter> parameters;
    2: Type returnType;
}

struct MethodImplementation {
    1: list<LocalVariable> localVariables;
    2: Expression body;
}

struct MethodReference {
    1: i32 moduleId;
    2: MethodDescriptor descriptor;
}

struct MethodDefinition {
    1: MethodDescriptor descriptor;
    2: EffectInfo effects;
    3: bool isVirtual;
    4: bool isAbstract;
    5: bool isImplicitOverride;
    6: bool isFinal;
    7: MethodSignature signature;
    8: optional MethodImplementation implementation;
}

union Method {
    1: MethodReference methodRef;
    2: MethodDefinition methodDef;
}



struct ClassConstructor {
    1: AccessModifier accessModifier;
    2: EffectInfo effects;
    3: i32 instanceClassId;
    4: ClassConstructorSignature signature;
    8: ClassConstructorImplementation implementation;
}

struct ClassConstructorSignature {
    1: list<Parameter> parameters;
}

union ClassConstructorInitExpression {
    1: Expression expression;
    2: ClassConstructorFieldInitialization fieldInit;
}

struct ClassConstructorFieldInitialization {
    1: ClassField field;
    2: Expression value;
}

struct ClassConstructorBaseConstructorCall {
    1: ClassType baseClassType;
    2: i32 baseConstructor;
    3: list<Expression> arguments;
}

struct ClassConstructorImplementation {
    1: list<LocalVariable> localVariables;
    2: list<ClassConstructorInitExpression> initExprs;
    3: ClassConstructorBaseConstructorCall baseCtorCall;
    4: Expression postInitExpr;
}



struct LocalVariable {
    1: bool isMutable;
    2: Type variableType;
    3: string name;
}

union VariableReference {
    1: LocalVariableReference localVariableReference;
    2: FieldReference fieldReference;
}

union LocalVariableOwner {
    1: i32 functionId;
    2: i32 methodId;
    3: i32 classConstructorId;
}

struct LocalVariableReference {
    1: LocalVariableOwner owner;
    2: i32 localVariableId;
}

struct FieldReference {
    1: i32 classId;
    2: i32 fieldId;
}

union ParameterOwner {
    1: i32 traitId;
    2: i32 classId;
    3: i32 constructorId;
    4: i32 functionId;
    5: i32 methodId;
    6: i32 classConstructorId;
}

struct ParameterReference {
    1: ParameterOwner owner;
    2: i32 parameterId;
    3: i32 parameterElementId;
}






struct BindVariable {
    1: i32 localVariableId;
    2: Expression value;
    3: Expression next;
}

struct ClassConstructorCall {
    1: ClassType classType;
    2: i32 classConstructorId;
    3: list<Expression> arguments;
}

struct ConstructorCall {
    1: ConstructorInstanceType constructorType;
    2: list<Expression> arguments;
}

struct FunctionCall {
    1: i32 functionId;
    2: list<Expression> arguments;
}

struct IfElse {
    1: Expression condition;
    2: Expression ifBody;
    3: Expression elseBody;
}

struct IgnoreValue {
    1: Expression value;
}

struct LoadConstantBool {
    1: bool value;
}

struct LoadConstantInt {
    // Uses little endian representation.
    1: binary value;
}

struct LoadConstantString {
    1: string value;
}

struct LoadLambda {
    // A lambda id is unique per function
    1: i32 lambdaId;
    2: Type variableType;
    3: string name;
    4: Expression body;
}

struct LoadTypeValue {
    1: Type loadTypeOf;
}

struct LoadVariable {
    1: VariableReference variable;
}

struct MethodCall {
    1: i32 methodId;
    2: Expression boundObject;
    3: list<Expression> arguments;
}

struct OperationAdd {
    1: Expression left;
    2: Expression right;
}

struct OperationSub {
    1: Expression left;
    2: Expression right;
}

struct OperationMul {
    1: Expression left;
    2: Expression right;
}

struct OperationDiv {
    1: Expression left;
    2: Expression right;
}

struct OperationEquals {
    1: Expression left;
    2: Expression right;
}

struct OperationNotEquals {
    1: Expression left;
    2: Expression right;
}

union Pattern {
    1: DiscardPattern discardPattern;
    2: BindingPattern bindingPattern;
    3: TypeTestPattern typeTestPattern;
}

struct DiscardPattern {
}

struct BindingPattern {
    1: VariableReference variable;
}

struct TypeTestPattern {
    1: VariableReference variable;
}

struct PatternMatchCase {
    1: Pattern pattern;
    2: Expression body;
}

struct PatternMatch {
    1: Expression value;
    2: list<PatternMatchCase> cases;
}

struct Sequence {
    1: Expression first;
    2: Expression second;
}

struct StoreVariable {
    1: VariableReference variable;
    2: Expression value;
}

struct TupledValue {
    1: Type entryType;
    2: Expression value;
}

struct TupledValues {
    1: list<TupledValue> values;
}

struct Upcast {
    1: Type outerType;
    2: Type innerType;
    3: Expression inner;
}


struct Module {
    1: required i32 formatVersion;
    2: required string name;
    5: bool isInterfaceOnly;

    10: list<ModuleReference> referencedModules;
    11: list<Trait> traits;
    12: list<Class> classes;
    13: list<DataConstructor> dataConstructors;
    14: list<Function> functions;
    15: list<Method> methods;
    16: list<ClassConstructor> classConstructors;
}
