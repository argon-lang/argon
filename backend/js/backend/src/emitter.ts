import type {ModuleExportEntry, ModuleInfo, ModuleModel, ProgramModel} from "./program-model.js";
import { encodeTubePathComponent, ensureExhaustive, getModuleId, getModulePathExternalUrl, getModulePathUrl, modulePathEquals, tubePackageName, urlEncodeIdentifier } from "./util.js"

import type * as estree from "estree";
import type * as ir from "@argon-lang/js-backend-api/vm.js";
import { Identifier } from "@argon-lang/js-backend-api/vm.js";
import { JsExtern } from "@argon-lang/js-backend-api";
import type { IterableElement, JsonObject, JsonValue, ReadonlyDeep } from "type-fest";

import { name as isValidIdName } from "estree-util-is-identifier-name";


export interface OutputModuleInfo {
    readonly modulePath: ir.ModulePath;
    emitJsProgram(): ReadonlyDeep<estree.Program>;
}

export interface EmitOptions {
    readonly program: ProgramModel,
    readonly executable?: string | undefined,
}


abstract class TokenEmitter {
    constructor(moduleEmitter: ModuleEmitter) {
        this.#moduleEmitter = moduleEmitter;
    }

    readonly #moduleEmitter: ModuleEmitter;


    buildTokenValue(t: ir.Token): estree.Expression {
        switch(t.$type) {
            case "builtin":
                switch(t.b.$type) {
                    case "bool":
                        return {
                            type: "Literal",
                            value: "boolean",
                        };

                    case "int":
                        return {
                            type: "Literal",
                            value: t.b.integerType === "int" ? "bigint" : "number",
                        };

                    case "string":
                        return {
                            type: "Literal",
                            value: "string",
                        };

                    case "array": {
                        return {
                            type: "NewExpression",
                            callee: this.#moduleEmitter.getArgonRuntimeExport("ArrayType"),
                            arguments: [
                                this.buildTokenValue(t.b.elementType),
                            ],
                        };
                    }

                    case "never":
                        return this.#moduleEmitter.getArgonRuntimeExport("neverType");

                    case "conjunction":
                    case "disjunction":
                        throw new Error("Not implemented buildTypeInfo builtin " + t.b.$type);
                }

            case "function":
                return {
                    type: "NewExpression",
                    callee: this.#moduleEmitter.getArgonRuntimeExport("FunctionType"),
                    arguments: [
                        this.buildTokenValue(t.input),
                        this.buildTokenValue(t.output),
                    ],
                };

            case "function-erased":
                return {
                    type: "NewExpression",
                    callee: this.#moduleEmitter.getArgonRuntimeExport("FunctionTypeErased"),
                    arguments: [
                        this.buildTokenValue(t.output),
                    ],
                };

            case "function-token":
                return {
                    type: "NewExpression",
                    callee: this.#moduleEmitter.getArgonRuntimeExport("FunctionTypeToken"),
                    arguments: [
                        this.buildTokenValue(t.tokenKind),
                        this.buildTokenValue(t.output),
                    ],
                };

            case "instance-value":
            {
                const instanceInfo = this.#moduleEmitter.options.program.getInstanceInfo(t.instanceId);
                const instanceExpr = this.#moduleEmitter.getImportExpr(instanceInfo.importSpecifier);

                let specializedExpr: estree.Expression = {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: instanceExpr,
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTokenValue(arg)),
                };

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: specializedExpr,
                        property: {
                            type: "Identifier",
                            name: "create",
                        },
                    },
                    arguments: [],
                };
            }

            case "record":
            {
                const rec = this.#moduleEmitter.options.program.getRecordInfo(t.recordId);

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.#moduleEmitter.getImportExpr(rec.importSpecifier),
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTokenValue(arg)),
                };
            }

            case "enum":
            {
                const rec = this.#moduleEmitter.options.program.getEnumInfo(t.enumId);

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.#moduleEmitter.getImportExpr(rec.importSpecifier),
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTokenValue(arg)),
                };
            }

            case "parent-token-parameter":
                return this.getParentTypeParam(t.index);

            case "ref-cell":
                return {
                    type: "NewExpression",
                    callee: this.#moduleEmitter.getArgonRuntimeExport("RefCellType"),
                    arguments: [
                        this.buildTokenValue(t.inner),
                    ],
                };

            case "trait":
            {
                const trt = this.#moduleEmitter.options.program.getTraitInfo(t.traitId);

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.#moduleEmitter.getImportExpr(trt.importSpecifier),
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTokenValue(arg)),
                };
            }

            case "instance-type":
            {
                const inst = this.#moduleEmitter.options.program.getInstanceInfo(t.instanceId);

                return {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.#moduleEmitter.getImportExpr(inst.importSpecifier),
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: t.args.map(arg => this.buildTokenValue(arg)),
                };
            }


            case "tuple":
                return {
                    type: "ArrayExpression",
                    elements: t.elements.map(item => this.buildTokenValue(item)),
                };

            case "token-parameter":
                return this.getTypeParam(t.index);

            case "type-info":
                return this.#moduleEmitter.getArgonRuntimeExport("typeInfo");

            case "boxed":
                return this.#moduleEmitter.getArgonRuntimeExport("erasedType");
        }
    }

    protected abstract getTypeParam(index: bigint): estree.Expression;
    protected abstract getParentTypeParam(index: bigint): estree.Expression;

}

class VTableTokenEmitter extends TokenEmitter {
    constructor(
        moduleEmitter: ModuleEmitter,
        private readonly parentExpr: estree.Expression,
    ) {
        super(moduleEmitter);
    }


    protected override getParentTypeParam(index: bigint): estree.Expression {
        return {
            type: "MemberExpression",
            computed: true,
            optional: false,
            object: {
                type: "MemberExpression",
                computed: false,
                optional: false,
                object: {
                    type: "ThisExpression",
                },
                property: {
                    type: "Identifier",
                    name: "prototype",
                },
            },
            property: {
                type: "MemberExpression",
                computed: true,
                optional: false,
                object: {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: this.parentExpr,
                    property: {
                        type: "Identifier",
                        name: "tokenParameterSymbols",
                    },
                },
                property: {
                    type: "Literal",
                    value: Number(index),
                },
            },
        };
    }

    protected override getTypeParam(_index: bigint): estree.Identifier {
        throw new Error("Method type parameters not supported in vtable");
    }
}

class InstanceBaseConstructorTokenEmitter extends TokenEmitter {
    protected getParentTypeParam(_index: bigint): estree.Expression {
        throw new Error("Parent type parameters not supported in instance base constructor");
    }

    protected getTypeParam(index: bigint): estree.Expression {
        return {
            type: "Identifier",
            name: `t${index}`,
        };
    }

}

class BlockTokenEmitter extends TokenEmitter {
    constructor(
        moduleEmitter: ModuleEmitter,
        private readonly parentExpr: estree.Expression | undefined,
    ) {
        super(moduleEmitter);
    }

    protected getParentTypeParam(index: bigint): estree.Expression {
        if(this.parentExpr === undefined) {
            throw new Error("Parent type parameters not supported in this block");
        }

        return {
            type: "MemberExpression",
            computed: true,
            optional: false,
            object: {
                type: "ThisExpression",
            },
            property: {
                type: "MemberExpression",
                computed: true,
                optional: false,
                object: {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: this.parentExpr,
                    property: {
                        type: "Identifier",
                        name: "tokenParameterSymbols",
                    },
                },
                property: {
                    type: "Literal",
                    value: Number(index),
                },
            },
        };
    }

    protected getTypeParam(index: bigint): estree.Expression {
        return {
            type: "Identifier",
            name: `t${index}`,
        };
    }
}


abstract class EmitterBase {
    constructor(
        readonly options: EmitOptions,
    ) {}



    /*
        Name encoding

        Full Export Name (used to reference types in signatures)
        Tube$dName$sModule$sName$s<export name>

        Export Name (identifier exported from module)
        global           - <name>$a<args>$r<result>$e
        synthetic nested - <parent>$k<index>

        Identifier
        <none>    - $_
        named     - the name, but with all $ encoded as $$ and any invalid characters URL encoded, using $XX
        binop     - The constructor name of the operator, but prefixed with $b and - replaced with _
        unop      - Same as binop, but using $u as a prefix
        index     - $n
        extension - $x<name>
        inverse   - $i<name>
        update    - $m<name>


        Erased signature types
        builtin  - $b<int|bool|...>$a<args>$e
        function - $f<arg>$r<result>$e
        record   - $r<full export name>$a<args>$e
        tuple    - $t<items>$e
        erased   - $_

    */

    protected abstract getImportId(source: string): string;

    protected getFullExportName(importSpec: ir.ImportSpecifier): string {
        const moduleId = getModuleId(importSpec);

        const moduleInfo = this.options.program.getModuleInfo(moduleId);
        const tubeInfo = this.options.program.getTubeInfo(moduleInfo.tubeId);
        
        const tubePart = [ tubeInfo.tubeName.head, ...tubeInfo.tubeName.tail ]
            .map(s => this.getExportNameForId({ $type: "named", s }))
            .join("$d");

        const modulePart = moduleInfo.path.path
            .map(s => this.getExportNameForId({ $type: "named", s }) + "$s")
            .join("");

        return tubePart + "$s" + modulePart + this.getExportNameForImport(importSpec);
    }

    protected getExportNameForIdSig(id: Identifier | undefined, sig: ir.ErasedSignature): string {
            return this.getExportNameForId(id) +
                "$a" + sig.params.map(arg => this.getExportNameForType(arg)).join("") +
                "$r" + this.getExportNameForType(sig.result);
    }

    protected getExportNameForImport(importSpec: ir.ImportSpecifier): string {
        switch(importSpec.$type) {
            case "global":
                return this.getExportNameForId(importSpec.name) +
                    "$a" + importSpec.sig.params.map(arg => this.getExportNameForType(arg)).join("") +
                    "$r" + this.getExportNameForType(importSpec.sig.result);

            case "local":
                return this.getExportNameForImport(importSpec.parent) + "$k" + importSpec.index;
        }
    }

    protected getExportNameForId(id: Identifier | undefined): string {
        switch(id?.$type) {
            case undefined:
                return "$_";

            case "named":
                return urlEncodeIdentifier(id.s);

            case "bin-op":
                return "$b" + id.op.replaceAll("-", "_");

            case "un-op":
                return "$u" + id.op.replaceAll("-", "_");

            case "index":
                return "$n";

            case "extension":
                return "$x" + this.getExportNameForId(id.inner);

            case "inverse":
                return "$i" + this.getExportNameForId(id.inner);

            case "update":
                return "$m" + this.getExportNameForId(id.inner);
        }
    }

    protected getExportNameForType(t: ir.ErasedSignatureType): string {
        switch(t.$type) {
            case "int":
            case "i8":
            case "u8":
            case "bool":
            case "string":
            case "never":
                return "$b" + t.$type + "$a$e";

            case "array":
                return "$barray$a" + this.getExportNameForType(t.elementType) + "$e";

            case "function":
                return "$f" + this.getExportNameForType(t.input) + "$r" + this.getExportNameForType(t.output) + "$e";

            case "record":
                return "$r" + this.getFullExportName(t.recordImport) +
                    "$a" + t.args.map(arg => this.getExportNameForType(arg)).join("") +
                    "$e";

            case "tuple":
                return "$t" + t.elements.map(elem => this.getExportNameForType(elem)).join("") +
                    "$e";

            case "erased":
                return "$_";
        }
    }
}

export function* emitTube(options: EmitOptions): Iterable<OutputModuleInfo> {
    for(const module of options.program.modules) {
        yield {
            modulePath: module.path,
            emitJsProgram() {
                const modEmitter = new ModuleEmitter(options, module);
                return modEmitter.emit();
            },
        };
    }
}

class ModuleEmitter extends EmitterBase {
    constructor(
        options: EmitOptions,
        private readonly module: ModuleModel,
    ) {
        super(options);
    }

    private readonly imports: string[] = [];
    private readonly moduleStatements: ReadonlyDeep<IterableElement<estree.Program["body"]>>[] = [];

    emit(): ReadonlyDeep<estree.Program> {
        for(const entry of this.module.exports) {
            this.emitEntry(entry);
        }

        const body: ReadonlyDeep<IterableElement<estree.Program["body"]>>[] = [];

        this.imports.forEach((path, i) => {
            body.push({
                type: "ImportDeclaration",
                specifiers: [
                    {
                        type: "ImportNamespaceSpecifier",
                        local: {
                            type: "Identifier",
                            name: "import" + i,
                        },
                    },
                ],
                source: {
                    type: "Literal",
                    value: path,
                },
                attributes: [],
            })
        });

        body.push(...this.moduleStatements);

        return {
            type: "Program",
            sourceType: "module",
            body,
        };
    }

    addDeclaration(declaration: ReadonlyDeep<estree.Declaration>): void {
        this.moduleStatements.push({
            type: "ExportNamedDeclaration",
            declaration,
            specifiers: [],
            attributes: [],
        });
    }

    override getImportId(source: string): string {
        let index = this.imports.indexOf(source);
        if(index < 0) {
            index = this.imports.length;
            this.imports.push(source);
        }

        return "import" + index;
    }

    getArgonRuntimeExport(name: string): estree.Expression {
        return {
            type: "MemberExpression",
            computed: false,
            optional: false,
            object: {
                type: "Identifier",
                name: this.getImportId("@argon-lang/runtime"),
            },
            property: {
                type: "Identifier",
                name,
            },
        };
    }


    private getImportSource(moduleInfo: ModuleInfo): string {
        if(moduleInfo.tubeId === 0n) {
            let hasRemoved = false;
            const parentModulePathParts = this.module.path.path.slice(0, Math.max(0, this.module.path.path.length - 1));
            const targetModulePathParts = moduleInfo.path.path.slice();

            while(parentModulePathParts.length > 0 && targetModulePathParts.length > 1 && parentModulePathParts[0] === targetModulePathParts[0]) {
                hasRemoved = true;
                parentModulePathParts.shift();
                targetModulePathParts.shift();
            }

            let prefix: string;
            if(parentModulePathParts.length === 0) {
                if(targetModulePathParts.length === 0) {
                    // No segments could have been removed and we want to reach the root of the package.
                    // This means we go up one level.
                    return "../";
                }

                prefix = "./";
            }
            else {
                prefix = "../".repeat(parentModulePathParts.length);
            }

            if(hasRemoved) {
                return prefix + targetModulePathParts.map(encodeTubePathComponent).join("/") + ".js";
            }
            else {
                return prefix + getModulePathUrl({ path: targetModulePathParts });
            }
        }
        else {
            const tubeInfo = this.options.program.getTubeInfo(moduleInfo.tubeId);
            const tubePackage = tubePackageName(tubeInfo);

            const modulePathUrl = getModulePathExternalUrl(moduleInfo.path);
            
            if(modulePathUrl === "") {
                return tubePackage;
            }

            return tubePackage + "/" + modulePathUrl;
        }
    }

    getImportExpr(importSpec: ir.ImportSpecifier): estree.Expression {
        const moduleId = getModuleId(importSpec);
        const exportName = this.getExportNameForImport(importSpec);

        const moduleInfo = this.options.program.getModuleInfo(moduleId);
        if(moduleInfo.tubeId === 0n && modulePathEquals(moduleInfo.path, this.module.path)) {
            return {
                type: "Identifier",
                name: exportName,
            };
        }

        const importSource = this.getImportSource(moduleInfo);
        const importId = this.getImportId(importSource);

        return {
            type: "MemberExpression",
            object: {
                type: "Identifier",
                name: importId,
            },
            property: {
                type: "Identifier",
                name: exportName,
            },
            computed: false,
            optional: false,
        };
    }

    private emitEntry(entry: ModuleExportEntry): void {
        switch(entry.$type) {
            case "function-definition":
                this.emitFunction(entry.definition);
                break;

            case "record-definition":
                this.emitRecord(entry.definition);
                break;

            case "enum-definition":
                this.emitEnum(entry.definition);
                break;

            case "trait-definition":
                this.emitTrait(entry.definition);
                break;

            case "instance-definition":
                this.emitInstance(entry.definition);
                break;

            default:
                ensureExhaustive(entry);
        }
    }

    private emitFunction(func: ir.FunctionDefinition): void {
        if(func.implementation === undefined) {
            throw new Error("Missing function implementation");
        }

        const impl = this.emitFunctionImpl(false, func.signature, func.implementation, undefined);

        if(impl.type == "FunctionDeclaration") {
            this.addDeclaration({
                ...impl,
                id: {
                    type: "Identifier",
                    name: this.getExportNameForImport(func.import),
                },
            });
        }
        else {
            this.addDeclaration({
                type: "VariableDeclaration",
                kind: "const",
                declarations: [
                    {
                        type: "VariableDeclarator",
                        id: {
                            type: "Identifier",
                            name: this.getExportNameForImport(func.import),
                        },
                        init: impl,
                    }
                ],
            });
        }
    }

    private emitFunctionImpl(useThis: boolean, signature: ir.FunctionSignature, impl: ir.FunctionImplementation, parentExpr: estree.Expression | undefined): ReadonlyDeep<estree.MaybeNamedFunctionDeclaration | estree.Expression> {
        switch(impl.$type) {
            case "vm-ir":
            {
                const params: estree.Pattern[] = [];

                const regOffset = useThis ? 1 : 0;

                for(const i of signature.tokenParameters.keys()) {
                    params.push({
                        type: "Identifier",
                        name: `t${i}`,
                    });
                }

                for(const i of signature.parameters.keys()) {
                    params.push({
                        type: "Identifier",
                        name: `r${regOffset + i}`,
                    });
                }

                const varStmts: estree.Statement[] = [];

                if(useThis) {
                    varStmts.push({
                        type: "VariableDeclaration",
                        kind: "const",
                        declarations: [
                            {
                                type: "VariableDeclarator",
                                id: {
                                    type: "Identifier",
                                    name: "r0",
                                },
                                init: {
                                    type: "ThisExpression",
                                },
                            },
                        ],
                    });
                }

                const varOffset = signature.parameters.length + regOffset;
                for(let varIndex = 0; varIndex < impl.body.variables.variables.length; ++varIndex) {
                    varStmts.push({
                        type: "VariableDeclaration",
                        kind: "let",
                        declarations: [
                            {
                                type: "VariableDeclarator",
                                id: {
                                    type: "Identifier",
                                    name: `r${varOffset + varIndex}`,
                                },
                            },
                        ],
                    });
                }

                const blockEmitter = new BlockEmitter(this, parentExpr);
                blockEmitter.emitRegion(impl.body.region);

                const block = blockEmitter.toBlock();
                block.body.unshift(...varStmts);

                return {
                    type: "FunctionDeclaration",
                    id: null,
                    params: params,
                    body: block,
                };
            }
                
            
            case "extern":
                const externRes = JsExtern.codec.decode(impl.extern);
                if(!externRes.success) {
                    throw new Error("Could not decode extern: " + externRes.message + " " + JSON.stringify(externRes.path));
                }

                const extern = externRes.value;

                return this.getExprForImports(extern);

            default:
                ensureExhaustive(impl);
        }
    }


    private getExprForImports(extern: JsExtern): ReadonlyDeep<estree.Expression> {
        const node = extern.declaration;
        const body: ReadonlyDeep<estree.Statement>[] = [];

        for(const importedId of extern.imports) {
            const mappedId = this.getImportId(importedId.source);

            let value: estree.Expression;
            if(importedId.member) {
                const memberIsValidId = isValidIdName(importedId.member);
                const property: estree.Expression = memberIsValidId
                    ? { type: "Identifier", name: importedId.member }
                    : { type: "Literal", value: importedId.member };

                value = {
                    type: "MemberExpression",
                    object: {
                        type: "Identifier",
                        name: mappedId,
                    },
                    property: property,
                    computed: false,
                    optional: false,
                };
            }
            else {
                value = {
                    type: "Identifier",
                    name: mappedId,
                };
            }

            body.push({
                type: "VariableDeclaration",
                kind: "const",
                declarations: [
                    {
                        type: "VariableDeclarator",
                        id: {
                            type: "Identifier",
                            name: importedId.localAlias,
                        },
                        init: value,
                    },
                ],
            });
        }

        const funcExpr: ReadonlyDeep<estree.FunctionExpression> = {
            type: "FunctionExpression",
            id: node.id,
            params: node.params,
            body: node.body,
            generator: node.generator,
            async: node.async,
        };

        if(body.length === 0) {
            return funcExpr;
        }
        else {
            body.push({
                type: "ReturnStatement",
                argument: funcExpr,
            });

            return {
                type: "CallExpression",
                callee: {
                    type: "FunctionExpression",
                    params: [],
                    body: {
                        type: "BlockStatement",
                        body,
                    },
                },
                arguments: [],
                optional: false,
            };
        }
    }

    private emitRecord(rec: ir.RecordDefinition): void {
        const name = this.getExportNameForImport(rec.import);

        this.addDeclaration({
            type: "VariableDeclaration",
            kind: "const",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: {
                        type: "Identifier",
                        name,
                    },
                    init: {
                        type: "CallExpression",
                        optional: false,
                        callee: this.getArgonRuntimeExport("createRecordType"),
                        arguments: [
                            jsonToExpression({
                                name,
                                tokenParameterCount: rec.signature.tokenParameters.length,
                                fields: rec.fields.map(field => ({
                                    name: this.getExportNameForId(field.name),
                                    mutable: field.mutable,
                                })),
                            }),
                        ],
                    },
                },
            ],
        });
    }

    private emitEnum(enumDef: ir.EnumDefinition): void {
        const name = this.getExportNameForImport(enumDef.import);

        const variantsObj: JsonObject = Object.create(null);
        for(const variant of enumDef.variants) {
            variantsObj[this.getExportNameForId(variant.name)] = {
                argCount: variant.signature.tokenParameters.length + variant.signature.parameters.length,
                fields: variant.fields.map(field => ({
                    name: this.getExportNameForId(field.name),
                    mutable: field.mutable,
                }))
            };
        }

        this.addDeclaration({
            type: "VariableDeclaration",
            kind: "const",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: {
                        type: "Identifier",
                        name,
                    },
                    init: {
                        type: "CallExpression",
                        optional: false,
                        callee: this.getArgonRuntimeExport("createEnumType"),
                        arguments: [
                            jsonToExpression({
                                name,
                                tokenParameterCount: enumDef.signature.tokenParameters.length,
                                variants: variantsObj,
                            }),
                        ],
                    },
                },
            ],
        });
    }

    private emitTrait(traitDef: ir.TraitDefinition): void {
        const name = this.getExportNameForImport(traitDef.import);
        const nameId: estree.Expression = {
            type: "Identifier",
            name,
        };

        const methodFuncs = this.emitMethods(traitDef.methods, nameId);
        const vtable = this.emitVTable(traitDef.vtable, nameId);

        const traitInfoProps: ReadonlyDeep<estree.Property>[] = [
            {
                type: "Property",
                computed: false,
                shorthand: false,
                method: false,
                kind: "init",
                key: {
                    type: "Identifier",
                    name: "name",
                },
                value: {
                    type: "Literal",
                    value: name,
                }
            },
            {
                type: "Property",
                computed: false,
                shorthand: false,
                method: false,
                kind: "init",
                key: {
                    type: "Identifier",
                    name: "tokenParameterCount",
                },
                value: {
                    type: "Literal",
                    value: traitDef.signature.tokenParameters.length,
                },
            },
            {
                type: "Property",
                computed: false,
                shorthand: false,
                method: false,
                kind: "init",
                key: {
                    type: "Identifier",
                    name: "methods",
                },
                value: methodFuncs,
            },
            {
                type: "Property",
                computed: false,
                shorthand: false,
                method: false,
                kind: "init",
                key: {
                    type: "Identifier",
                    name: "vtable",
                },
                value: vtable,
            },
        ];


        const metadata = this.options.program.metadata;
        if(
            traitDef.import.$type === "global" &&
            metadata.name.head == "Argon" &&
            metadata.name.tail.length === 1 &&
            metadata.name.tail[0] === "Core" &&
            this.module.path.path.length === 1 &&
            this.module.path.path[0] === "Exception" &&
            traitDef.signature.parameters.length === 0 &&
            traitDef.import.name.$type === "named" &&
            traitDef.import.name.s === "Exception"
        ) {
            traitInfoProps.push({
                type: "Property",
                computed: false,
                shorthand: false,
                method: false,
                kind: "init",
                key: {
                    type: "Identifier",
                    name: "special",
                },
                value: {
                    type: "Literal",
                    value: "exception",
                },
            })
        }

        const traitInfo: ReadonlyDeep<estree.Expression> = {
            type: "ObjectExpression",
            properties: traitInfoProps,
        };


        this.addDeclaration({
            type: "VariableDeclaration",
            kind: "const",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: nameId,
                    init: {
                        type: "CallExpression",
                        optional: false,
                        callee: this.getArgonRuntimeExport("createTraitType"),
                        arguments: [
                            traitInfo,
                        ],
                    },
                },
            ],
        });


    }

    private emitMethods(methods: readonly ir.MethodDefinition[], parentExpr: estree.Expression): ReadonlyDeep<estree.Expression> {
        const methodFuncs: ReadonlyDeep<estree.Property>[] = [];

        for(const methodDef of methods) {
            const methodName = this.getExportNameForIdSig(methodDef.name, methodDef.erasedSignature);
            const useSimpleName = isValidIdName(methodName) && methodName !== "__proto__";

            const methodImpl = this.emitMethodBody(methodDef, parentExpr);

            methodFuncs.push({
                type: "Property",
                computed: !useSimpleName,
                shorthand: false,
                method: false,
                kind: "init",
                key: useSimpleName ? {
                    type: "Identifier",
                    name: methodName,
                } : {
                    type: "Literal",
                    value: methodName,
                },
                value: {
                    type: "ObjectExpression",
                    properties: [
                        {
                            type: "Property",
                            computed: false,
                            shorthand: false,
                            method: false,
                            kind: "init",
                            key: {
                                type: "Identifier",
                                name: "method",
                            },
                            value: methodImpl,
                        },
                    ],
                },
            });
        }

        return {
            type: "ObjectExpression",
            properties: methodFuncs,
        };
    }

    private emitMethodBody(methodDef: ir.MethodDefinition, parentExpr: estree.Expression): ReadonlyDeep<estree.Expression> {
        if(methodDef.implementation === undefined) {
            return {
                type: "Literal",
                value: null,
            };
        }
        
        const impl = this.emitFunctionImpl(true, methodDef.signature, methodDef.implementation, parentExpr);

        if(impl.type == "FunctionDeclaration") {
            return {
                type: "FunctionExpression",
                params: impl.params,
                body: impl.body,
            };
        }
        else {
            return impl;
        }
    }

    private emitVTable(vtable: ir.Vtable, parentExpr: estree.Expression): ReadonlyDeep<estree.Expression> {
        const entries: ReadonlyDeep<estree.Expression>[] = [];

        for(const entry of vtable.entries) {
            const methodInfo = this.options.program.getMethodInfo(entry.slotMethodId);

            const methodSymbolExpr: ReadonlyDeep<estree.Expression> = {
                type: "MemberExpression",
                computed: false,
                optional: false,
                
                object: {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: new VTableTokenEmitter(this, parentExpr).buildTokenValue(entry.slotInstanceType),
                    property: {
                        type: "Identifier",
                        name: "methods",
                    },
                },

                property: {
                    type: "Identifier",
                    name: this.getExportNameForIdSig(methodInfo.name, methodInfo.signature),
                },
            };

            let methodTarget: JsonValue;
            switch(entry.target.$type) {
                case "abstract":
                    methodTarget = {
                        type: "abstract",
                    };
                    break;

                case "ambiguous":
                    methodTarget = {
                        type: "ambiguous",
                    };
                    break;

                case "implementation":
                    methodTarget = {
                        type: "implementation",
                        methodIndex: Number(entry.target.methodIndex),
                    };
                    break;

                default:
                    ensureExhaustive(entry.target);
            }

            entries.push({
                type: "ObjectExpression",
                properties: [
                    {
                        type: "Property",
                        computed: false,
                        shorthand: false,
                        method: false,
                        kind: "init",
                        key: {
                            type: "Identifier",
                            name: "slotMethodSymbol",
                        },
                        value: {
                            type: "FunctionExpression",
                            params: [],
                            body: {
                                type: "BlockStatement",
                                body: [
                                    {
                                        type: "ReturnStatement",
                                        argument: methodSymbolExpr,
                                    },
                                ],
                            },
                        },
                    },
                    {
                        type: "Property",
                        computed: false,
                        shorthand: false,
                        method: false,
                        kind: "init",
                        key: {
                            type: "Identifier",
                            name: "target",
                        },
                        value: jsonToExpression(methodTarget),
                    },
                ],
            });
        }

        return {
            type: "ArrayExpression",
            elements: entries,
        };
    }

    private emitInstance(instanceDef: ir.InstanceDefinition): void {
        const name = this.getExportNameForImport(instanceDef.import);
        const nameId: estree.Expression = {
            type: "Identifier",
            name,
        };

        const methodFuncs = this.emitMethods(instanceDef.methods, nameId);
        const vtable = this.emitVTable(instanceDef.vtable, nameId);

        const instanceInfo: ReadonlyDeep<estree.Expression> = {
            type: "ObjectExpression",
            properties: [
                {
                    type: "Property",
                    computed: false,
                    shorthand: false,
                    method: false,
                    kind: "init",
                    key: {
                        type: "Identifier",
                        name: "name",
                    },
                    value: {
                        type: "Literal",
                        value: name,
                    }
                },
                {
                    type: "Property",
                    computed: false,
                    shorthand: false,
                    method: false,
                    kind: "init",
                    key: {
                        type: "Identifier",
                        name: "tokenParameterCount",
                    },
                    value: {
                        type: "Literal",
                        value: instanceDef.signature.tokenParameters.length,
                    },
                },
                {
                    type: "Property",
                    computed: false,
                    shorthand: false,
                    method: false,
                    kind: "init",
                    key: {
                        type: "Identifier",
                        name: "argCount",
                    },
                    value: {
                        type: "Literal",
                        value: instanceDef.signature.parameters.length,
                    },
                },
                {
                    type: "Property",
                    computed: false,
                    shorthand: false,
                    method: false,
                    kind: "init",
                    key: {
                        type: "Identifier",
                        name: "methods",
                    },
                    value: methodFuncs,
                },
                {
                    type: "Property",
                    computed: false,
                    shorthand: false,
                    method: false,
                    kind: "init",
                    key: {
                        type: "Identifier",
                        name: "vtable",
                    },
                    value: vtable,
                },
                {
                    type: "Property",
                    computed: false,
                    shorthand: false,
                    method: true,
                    kind: "init",
                    key: {
                        type: "Identifier",
                        name: "baseConstructor",
                    },
                    value: {
                        type: "FunctionExpression",
                        params: instanceDef.signature.tokenParameters.map((_, i) => ({
                            type: "Identifier",
                            name: `t${i}`,
                        })),
                        body: {
                            type: "BlockStatement",
                            body: [
                                {
                                    type: "ReturnStatement",
                                    argument: new InstanceBaseConstructorTokenEmitter(this).buildTokenValue(instanceDef.signature.returnType),
                                }
                            ],
                        },
                    },
                },
            ],
        };


        this.addDeclaration({
            type: "VariableDeclaration",
            kind: "const",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: nameId,
                    init: {
                        type: "CallExpression",
                        optional: false,
                        callee: this.getArgonRuntimeExport("createInstanceDefinition"),
                        arguments: [
                            instanceInfo,
                        ],
                    },
                },
            ],
        });
    }
}

class BlockEmitter extends EmitterBase {
    constructor(
        private moduleEmitter: ModuleEmitter,
        private readonly parentExpr: estree.Expression | undefined,
    ) {
        super(moduleEmitter.options);
        this.tokenEmitter = new BlockTokenEmitter(this.moduleEmitter, this.parentExpr);
    }

    readonly stmts: estree.Statement[] = [];
    private readonly tokenEmitter: TokenEmitter;

    protected override getImportId(source: string): string {
        return this.moduleEmitter.getImportId(source);
    }

    private nestedBlockEmitter(): BlockEmitter {
        return new BlockEmitter(this.moduleEmitter, this.parentExpr);
    }

    private emitNestedBlock(block: ir.Region): estree.BlockStatement {
        const nestedEmitter = this.nestedBlockEmitter();
        nestedEmitter.emitRegion(block);
        return nestedEmitter.toBlock();
    }

    emitRegion(region: ir.Region): void {
        switch(region.$type) {
            case "basic-block":
                this.emitBasicBlock(region);
                break;

            case "block":
                this.emitBlockRegion(region);
                break;

            case "sequence":
                for(const subRegion of region.regions) {
                    this.emitRegion(subRegion);
                }
                break;

            case "if-else":
                this.emitIfElseRegion(region);
                break;

            case "finally":
                this.emitFinallyRegion(region);
                break;

            default:
                region satisfies never;
        }
    }

    toBlock(): estree.BlockStatement {
        return {
            type: "BlockStatement",
            body: this.stmts,
        };
    }

    private emitBasicBlock(bb: ir.Region & { $type: "basic-block" }): void {
        for(const insn of bb.instructions) {
            this.emitInstruction(insn);
        }
    }

    private emitBlockRegion(region: ir.Region & { $type: "block" }): void {
        const body = this.emitNestedBlock(region.region);

        let loopStmt: estree.Statement;
        if(region.flags.hasRetry || region.flags.isLoop) {
            if(!region.flags.isLoop) {
                body.body.push({
                    type: "BreakStatement",
                });
            }

            loopStmt = {
                type: "ForStatement",
                init: null,
                test: null,
                update: null,
                body,
            };
        }
        else {
            loopStmt = body;
        }

        if(region.flags.hasRetry || region.flags.hasBreak) {
            const label = this.getLabel(region.blockId);

            loopStmt = {
                type: "LabeledStatement",
                label,
                body: loopStmt,
            }
        }

        this.stmts.push(loopStmt);
    }

    private emitIfElseRegion(region: ir.Region & { $type: "if-else" }): void {
        const conditionBlock = this.emitNestedBlock(region.condition);

        const whenTrueLabel = this.getLabel(region.whenTrueBlockId);
        const whenFalseLabel = this.getLabel(region.whenFalseBlockId);

        const condVar: estree.Identifier = {
            type: "Identifier",
            name: "cond_" + whenTrueLabel.name,
        };

        this.stmts.push({
            type: "VariableDeclaration",
            kind: "let",
            declarations: [
                {
                    type: "VariableDeclarator",
                    id: condVar,
                    init: {
                        type: "Literal",
                        value: true,
                    },
                },
            ],
        });

        if(!(conditionBlock.body.length > 0 && conditionBlock.body[conditionBlock.body.length - 1]!.type === "BreakStatement")) {
            conditionBlock.body.push({
                type: "BreakStatement",
                label: whenTrueLabel,
            });
        }


        this.stmts.push({
            type: "LabeledStatement",
            label: whenTrueLabel,
            body: {
                type: "BlockStatement",
                body: [
                    {
                        type: "LabeledStatement",
                        label: whenFalseLabel,
                        body: conditionBlock
                    },
                    {
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            left: condVar,
                            operator: "=",
                            right: {
                                type: "Literal",
                                value: false,
                            },
                        },
                    },
                ],
            },
        });

        if(region.whenFalse.$type == "sequence" && region.whenFalse.regions.length === 0) {
            this.stmts.push({
                type: "IfStatement",
                test: condVar,
                consequent: this.emitNestedBlock(region.whenTrue),
            });
        }
        else if(region.whenTrue.$type == "sequence" && region.whenTrue.regions.length === 0) {
            this.stmts.push({
                type: "IfStatement",
                test: {
                    type: "UnaryExpression",
                    prefix: true,
                    operator: "!",
                    argument: condVar,
                },
                consequent: this.emitNestedBlock(region.whenFalse),
            });
        }
        else {
            this.stmts.push({
                type: "IfStatement",
                test: condVar,
                consequent: this.emitNestedBlock(region.whenTrue),
                alternate: this.emitNestedBlock(region.whenFalse),
            });
        }
    }

    private emitFinallyRegion(region: ir.Region & { $type: "finally" }): void {
        this.stmts.push({
            type: "TryStatement",
            block: this.emitNestedBlock(region.action),
            finalizer: this.emitNestedBlock(region.ensuring),
        });
    }

    private emitInstruction(insn: ir.Instruction): void {
        const stmts = this.stmts;

        const assign = (dest: ir.RegisterId, value: estree.Expression) => {
            stmts.push({
                type: "ExpressionStatement",
                expression: {
                    type: "AssignmentExpression",
                    left: this.getReg(dest),
                    operator: "=",
                    right: value,
                }
            });
        };

        const functionOutput = (dest: ir.FunctionResult, value: estree.Expression) => {
            let stmt: estree.Statement;
            switch(dest.$type) {
                case "discard":
                    stmt = {
                        type: "ExpressionStatement",
                        expression: {
                            type: "CallExpression",
                            optional: false,
                            callee: this.moduleEmitter.getArgonRuntimeExport("resolve"),
                            arguments: [value],
                        },
                    };
                    break;

                case "register":
                    stmt = {
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            left: this.getReg(dest.id),
                            operator: "=",
                            right: {
                                type: "CallExpression",
                                optional: false,
                                callee: this.moduleEmitter.getArgonRuntimeExport("resolve"),
                                arguments: [value],
                            },
                        },
                    };
                    break;

                case "return-value":
                    stmt = {
                        type: "ReturnStatement",
                        argument: {
                            type: "CallExpression",
                            optional: false,
                            callee: this.moduleEmitter.getArgonRuntimeExport("delay"),
                            arguments: [
                                {
                                    type: "ArrowFunctionExpression",
                                    params: [],
                                    expression: true,
                                    body: value,
                                },
                            ],
                        },
                    };
                    break;
            }

            stmts.push(stmt);
        }

        switch(insn.$type) {
            case "block-break": {
                const label = this.getLabel(insn.blockId);

                this.stmts.push({
                    type: "BreakStatement",
                    label,
                });

                break;
            }

            case "block-break-if":
            case "block-break-unless": {
                const label = this.getLabel(insn.blockId);

                const test = this.getReg(insn.condition);
                const condition: estree.Expression = insn.$type === "block-break-if"
                    ? test
                    : {
                        type: "UnaryExpression",
                        operator: "!",
                        prefix: true,
                        argument: test,
                    };

                this.stmts.push({
                    type: "IfStatement",
                    test: condition,
                    consequent: {
                        type: "BreakStatement",
                        label,
                    },
                    alternate: null,
                });

                break;
            }

            case "block-retry": {
                const label = this.getLabel(insn.blockId);

                this.stmts.push({
                    type: "ContinueStatement",
                    label,
                });

                break;
            }

            case "box":
                assign(insn.dest, this.getReg(insn.value));
                break;

            case "builtin": {
                const op = insn.op;

                const unary = (operator: estree.UnaryOperator, op: ir.BuiltinOp & { readonly value: ir.RegisterId, readonly dest: ir.RegisterId, readonly integerType?: ir.IntegerType }) => {
                    const value = op.integerType === undefined
                        ? this.getReg(op.value)
                        : this.coerceInteger(op.integerType, this.getReg(op.value));
                    const expr: estree.Expression = {
                        type: "UnaryExpression",
                        prefix: true,
                        operator,
                        argument: value,
                    };
                    assign(op.dest, op.integerType === undefined ? expr : this.wrapInteger(op.integerType, expr));
                };

                const binary = (
                    operator: estree.BinaryOperator,
                    op: ir.BuiltinOp & { readonly lhs: ir.RegisterId, readonly rhs: ir.RegisterId, readonly dest: ir.RegisterId, readonly integerType?: ir.IntegerType },
                    isComparison: boolean = false,
                ) => {
                    const left = op.integerType === undefined
                        ? this.getReg(op.lhs)
                        : this.coerceInteger(op.integerType, this.getReg(op.lhs));
                    const right = op.integerType === undefined
                        ? this.getReg(op.rhs)
                        : this.coerceInteger(op.integerType, this.getReg(op.rhs));
                    const expr: estree.Expression = {
                        type: "BinaryExpression",
                        left,
                        operator,
                        right,
                    };
                    assign(op.dest, isComparison || op.integerType === undefined ? expr : this.wrapInteger(op.integerType, expr));
                };

                switch(op.$type) {
                    case "int-negate":
                        unary("-", op);
                        break;

                    case "int-bit-not":
                        unary("~", op);
                        break;

                    case "int-convert": {
                        const value = this.coerceInteger(op.sourceType, this.getReg(op.value));

                        if(op.sourceType === op.destType) {
                            assign(op.dest, value);
                        }
                        else if(op.sourceType === "int" && op.destType !== "int") {
                            assign(op.dest, this.wrapInteger(op.destType, value));
                        }
                        else {
                            assign(op.dest, {
                                type: "CallExpression",
                                optional: false,
                                callee: {
                                    type: "Identifier",
                                    name: "BigInt",
                                },
                                arguments: [value],
                            });
                        }
                        break;
                    }

                    case "bool-not":
                        unary("!", op);
                        break;

                    case "int-add":
                        binary("+", op);
                        break;

                    case "int-sub":
                        binary("-", op);
                        break;

                    case "int-mul":
                        binary("*", op);
                        break;

                    case "int-bit-and":
                        binary("&", op);
                        break;

                    case "int-bit-or":
                        binary("|", op);
                        break;

                    case "int-bit-xor":
                        binary("^", op);
                        break;

                    case "int-bit-shift-left":
                        binary("<<", op);
                        break;

                    case "int-bit-shift-right":
                        binary(">>", op);
                        break;

                    case "string-concat": {
                        const args = op.args.map(arg => this.getReg(arg));
                        let expr: estree.Expression = {
                            type: "Literal",
                            value: "",
                        };

                        for(const arg of args) {
                            if(expr.type === "Literal" && expr.value === "") {
                                expr = arg;
                            }
                            else {
                                expr = {
                                    type: "BinaryExpression",
                                    left: expr,
                                    operator: "+",
                                    right: arg,
                                };
                            }
                        }

                        assign(op.dest, expr);
                        break;
                    }

                    case "int-eq":
                    case "string-eq":
                    case "bool-eq":
                        binary("===", op, false);
                        break;

                    case "int-lt":
                        binary("<", op, false);
                        break;

                    case "int-le":
                        binary("<=", op, false);
                        break;

                    case "int-gt":
                        binary(">", op, false);
                        break;

                    case "int-ge":
                        binary(">=", op, false);
                        break;

                    case "array-create-unsafe-uninitialized": {
                        this.stmts.push({
                            type: "IfStatement",
                            test: {
                                type: "BinaryExpression",
                                operator: ">",
                                left: this.getReg(op.length),
                                right: {
                                    type: "CallExpression",
                                    optional: false,
                                    callee: {
                                        type: "MemberExpression",
                                        computed: false,
                                        optional: false,
                                        object: {
                                            type: "Identifier",
                                            name: "globalThis",
                                        },
                                        property: {
                                            type: "Identifier",
                                            name: "BigInt",
                                        },
                                    },
                                    arguments: [
                                        {
                                            type: "MemberExpression",
                                            computed: false,
                                            optional: false,
                                            object: {
                                                type: "MemberExpression",
                                                computed: false,
                                                optional: false,
                                                object: {
                                                    type: "Identifier",
                                                    name: "globalThis",
                                                },
                                                property: {
                                                    type: "Identifier",
                                                    name: "Number",
                                                },
                                            },
                                            property: {
                                                type: "Identifier",
                                                name: "MAX_SAFE_INTEGER",
                                            },
                                        },
                                    ],
                                },
                            },
                            consequent: {
                                type: "BlockStatement",
                                body: [
                                    {
                                        type: "ThrowStatement",
                                        argument: {
                                            type: "NewExpression",
                                            callee: {
                                                type: "MemberExpression",
                                                computed: false,
                                                optional: false,
                                                object: {
                                                    type: "Identifier",
                                                    name: "globalThis",
                                                },
                                                property: {
                                                    type: "Identifier",
                                                    name: "Error",
                                                },
                                            },
                                            arguments: [
                                                {
                                                    type: "Literal",
                                                    value: "Array too large",
                                                },
                                            ],
                                        },
                                    },
                                ],
                            },
                        });

                        assign(op.dest, {
                            type: "NewExpression",
                            callee: {
                                type: "MemberExpression",
                                computed: false,
                                optional: false,
                                object: {
                                    type: "Identifier",
                                    name: "globalThis",
                                },
                                property: {
                                    type: "Identifier",
                                    name: "Array",
                                },
                            },
                            arguments: [
                                {
                                    type: "CallExpression",
                                    optional: false,
                                    callee: {
                                        type: "MemberExpression",
                                        computed: false,
                                        optional: false,
                                        object: {
                                            type: "Identifier",
                                            name: "globalThis",
                                        },
                                        property: {
                                            type: "Identifier",
                                            name: "Number",
                                        },
                                    },
                                    arguments: [
                                        this.getReg(op.length),
                                    ],
                                },
                            ],
                        });
                        break;
                    }

                    case "array-length":
                        assign(op.dest, {
                            type: "MemberExpression",
                            object: this.getReg(op.array),
                            property: {
                                type: "Identifier",
                                name: "length",
                            },
                            computed: false,
                            optional: false,
                        });
                        break;

                    case "array-get":
                        assign(op.dest, {
                            type: "MemberExpression",
                            object: this.getReg(op.array),
                            property: this.getReg(op.index),
                            computed: true,
                            optional: false,
                        });
                        break;

                    case "array-set":
                        stmts.push({
                            type: "ExpressionStatement",
                            expression: {
                                type: "AssignmentExpression",
                                left: {
                                    type: "MemberExpression",
                                    object: this.getReg(op.array),
                                    property: this.getReg(op.index),
                                    computed: true,
                                    optional: false,
                                },
                                operator: "=",
                                right: this.getReg(op.value),
                            }
                        });
                        break;

                    default:
                        op satisfies never;
                }
                break;
            }

            case "const-bool":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                });
                break;

            case "const-int":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                    bigint: insn.value.toString()
                });
                break;

            case "const-i8":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                });
                break;

            case "const-u8":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                });
                break;

            case "const-string":
                assign(insn.dest, {
                    type: "Literal",
                    value: insn.value,
                });
                break;

            case "enum-variant-literal":
            {
                const args: estree.Expression[] = [];
                for(const tokenArg of insn.tokenArgs) {
                    args.push(this.buildTokenValue(tokenArg));
                }

                for(const arg of insn.args) {
                    args.push(this.getReg(arg));
                }

                args.push({
                    type: "ObjectExpression",
                    properties: insn.fields.map(field => {
                        const fieldInfo = this.options.program.getRecordFieldInfo(field.fieldId);
                        const id = this.getExportNameForId(fieldInfo.name);
                        const isValid = isValidIdName(id);

                        return {
                            type: "Property",
                            computed: !isValid,
                            method: false,
                            shorthand: false,
                            kind: "init",
                            key: isValid ? {
                                type: "Identifier",
                                name: id,
                            } : {
                                type: "Literal",
                                value: id,
                            },
                            value: this.getReg(field.value),
                        };
                    }),
                });

                assign(insn.dest, {
                    type: "NewExpression",
                    callee: this.getVariantClass(insn.enumType, insn.variantId),
                    arguments: args,
                });

                break;
            }

            case "function-call":
            {
                const functionInfo = this.options.program.getFunctionInfo(insn.functionId);
                const funcExpr = this.moduleEmitter.getImportExpr(functionInfo.importSpecifier);

                const args: estree.Expression[] = [];
                for(const tokenArg of insn.tokenArgs) {
                    args.push(this.buildTokenValue(tokenArg));
                }

                for(const arg of insn.args) {
                    args.push(this.getReg(arg));
                }

                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: funcExpr,
                    arguments: args,
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "function-object-call":
            {
                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: this.getReg(insn.function),
                    arguments: [ this.getReg(insn.arg) ],
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "function-object-token-call":
            {
                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: this.getReg(insn.function),
                    arguments: [ this.buildTokenValue(insn.arg) ],
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "function-object-erased-call":
            {
                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: this.getReg(insn.function),
                    arguments: [],
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "instance-method-call":
            {
                const methodInfo = this.options.program.getMethodInfo(insn.methodId);

                const methodSymbolExpr: estree.Expression = {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    
                    object: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: this.buildTokenValue(insn.instanceType),
                        property: {
                            type: "Identifier",
                            name: "methods",
                        },
                    },

                    property: {
                        type: "Identifier",
                        name: this.getExportNameForIdSig(methodInfo.name, methodInfo.signature),
                    },
                };

                const args: estree.Expression[] = [];
                for(const tokenArg of insn.tokenArgs) {
                    args.push(this.buildTokenValue(tokenArg));
                }

                for(const arg of insn.args) {
                    args.push(this.getReg(arg));
                }

                const callExpr: estree.Expression = {
                    type: "CallExpression",
                    callee: {
                        type: "MemberExpression",
                        computed: true,
                        optional: false,
                        object: this.getReg(insn.instanceObject),
                        property: methodSymbolExpr,
                    },
                    arguments: args,
                    optional: false,
                };

                functionOutput(insn.dest, callExpr);
                break;
            }

            case "is-enum-variant-or-break":
                stmts.push({
                    type: "IfStatement",
                    test: {
                        type: "UnaryExpression",
                        operator: "!",
                        prefix: true,
                        argument: {
                            type: "BinaryExpression",
                            operator: "instanceof",
                            left: this.getReg(insn.value),
                            right: this.getVariantClass(insn.enumType, insn.variantId),
                        },
                    },
                    consequent: {
                        type: "BreakStatement",
                        label: this.getLabel(insn.notVariantBlockId),
                    },
                });

                for(const [i, arg] of insn.args.entries()) {
                    stmts.push({
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: this.getReg(arg),
                            right: {
                                type: "MemberExpression",
                                computed: false,
                                optional: false,
                                object: this.getReg(insn.value),
                                property: {
                                    type: "Identifier",
                                    name: `args_${i}`,
                                },
                            }
                        }
                    });
                }
                for(const fieldExtractor of insn.fieldExtractors) {
                    const fieldInfo = this.options.program.getRecordFieldInfo(fieldExtractor.fieldId);

                    stmts.push({
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: this.getReg(fieldExtractor.r),
                            right: {
                                type: "MemberExpression",
                                computed: false,
                                optional: false,
                                object: this.getReg(insn.value),
                                property: {
                                    type: "Identifier",
                                    name: `field_${this.getExportNameForId(fieldInfo.name)}`,
                                },
                            }
                        }
                    });
                }
                break;

            case "load-reference":
                assign(insn.dest, {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: this.getReg(insn.ref),
                    property: {
                        type: "Identifier",
                        name: "value",
                    },
                });
                break;

            case "load-token":
                assign(insn.dest, this.buildTokenValue(insn.token));
                break;

            case "load-instance-field":
                assign(insn.dest, {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: this.getReg(insn.instanceObject),
                    property: {
                        type: "Identifier",
                        name: `args_${insn.parameterIndex}`,
                    },
                });
                break;

            case "move":
                assign(insn.dest, this.getReg(insn.src));
                break;

            case "new-instance":
            {
                const instanceInfo = this.options.program.getInstanceInfo(insn.instanceId);
                const instanceExpr = this.moduleEmitter.getImportExpr(instanceInfo.importSpecifier);

                const tokenArgs: estree.Expression[] = [];
                for(const tokenArg of insn.tokenArgs) {
                    tokenArgs.push(this.buildTokenValue(tokenArg));
                }

                const args: estree.Expression[] = [];
                for(const arg of insn.args) {
                    args.push(this.getReg(arg));
                }

                const specializedExpr: estree.Expression = {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: instanceExpr,
                        property: {
                            type: "Identifier",
                            name: "specialize",
                        },
                    },
                    arguments: tokenArgs,
                };

                const createExpr: estree.Expression = {
                    type: "CallExpression",
                    optional: false,
                    callee: {
                        type: "MemberExpression",
                        computed: false,
                        optional: false,
                        object: specializedExpr,
                        property: {
                            type: "Identifier",
                            name: "create",
                        },
                    },
                    arguments: args,
                };

                assign(insn.dest, createExpr);
                break;
            }

            case "new-reference":
                assign(insn.dest, {
                    type: "NewExpression",
                    callee: this.moduleEmitter.getArgonRuntimeExport("RefCell"),
                    arguments: [this.getReg(insn.value)],
                });
                break;

            case "partially-applied-function":
            case "partially-applied-token-function":
            case "partially-applied-function-erased":
            {
                const argName: estree.Identifier = {
                    type: "Identifier",
                    name: "arg",
                };

                // Build token args for call
                const args: estree.Expression[] = [];
                for(const tokenArg of insn.tokenArgs) {
                    args.push(this.buildTokenValue(tokenArg));
                }

                if(insn.$type === "partially-applied-token-function") {
                    args.push(argName);
                }

                // Build concrete args for call
                const block: estree.Statement[] = [];

                for(const arg of insn.args) {
                    const name: estree.Identifier = {
                        type: "Identifier",
                        name: "capture" + block.length,
                    };

                    block.push({
                        type: "VariableDeclaration",
                        kind: "const",
                        declarations: [
                            {
                                type: "VariableDeclarator",
                                id: name,
                                init: this.getReg(arg),
                            },
                        ],
                    });

                    args.push(name);
                }

                if(insn.$type === "partially-applied-function") {
                    args.push(argName);
                }

                // Build call
                const functionInfo = this.options.program.getFunctionInfo(insn.functionId);
                const funcExpr = this.moduleEmitter.getImportExpr(functionInfo.importSpecifier);

                let params: estree.Pattern[] = [];
                if(insn.$type !== "partially-applied-function-erased") {
                    params.push(argName);
                }

                const lambda: estree.ArrowFunctionExpression = {
                    type: "ArrowFunctionExpression",
                    expression: true,
                    body: {
                        type: "CallExpression",
                        callee: funcExpr,
                        arguments: args,
                        optional: false,
                    },
                    params,
                };

                if(block.length === 0) {
                    assign(insn.dest, lambda);
                }
                else {
                    block.push({
                        type: "ExpressionStatement",
                        expression: {
                            type: "AssignmentExpression",
                            operator: "=",
                            left: this.getReg(insn.dest),
                            right: lambda,
                        },
                    });

                    stmts.push({
                        type: "BlockStatement",
                        body: block,
                    })
                }
                break;
            }

            case "raise":
                stmts.push({
                    type: "ThrowStatement",
                    argument: this.getReg(insn.exception),
                });
                break;

            case "record-field-load":
            {
                const fieldInfo = this.options.program.getRecordFieldInfo(insn.fieldId);
                assign(insn.dest, {
                    type: "MemberExpression",
                    computed: false,
                    optional: false,
                    object: this.getReg(insn.recordValue),
                    property: {
                        type: "Identifier",
                        name: "field_" + this.getExportNameForId(fieldInfo.name),
                    },
                });
                break;
            }

            case "record-field-store":
            {
                const fieldInfo = this.options.program.getRecordFieldInfo(insn.fieldId);
                stmts.push({
                    type: "ExpressionStatement",
                    expression: {
                        type: "AssignmentExpression",
                        operator: "=",
                        left: {
                            type: "MemberExpression",
                            computed: false,
                            optional: false,
                            object: this.getReg(insn.recordValue),
                            property: {
                                type: "Identifier",
                                name: "field_" + this.getExportNameForId(fieldInfo.name),
                            },
                        },
                        right: this.getReg(insn.fieldValue),
                    },
                });
                break;
            }

            case "record-literal":
                assign(insn.dest, {
                    type: "NewExpression",
                    callee: this.buildTokenValue(insn.recordType),
                    arguments: [
                        {
                            type: "ObjectExpression",
                            properties: insn.fields.map(field => {
                                const fieldInfo = this.options.program.getRecordFieldInfo(field.fieldId);
                                const id = this.getExportNameForId(fieldInfo.name);
                                const isValid = isValidIdName(id);

                                return {
                                    type: "Property",
                                    computed: !isValid,
                                    method: false,
                                    shorthand: false,
                                    kind: "init",
                                    key: isValid ? {
                                        type: "Identifier",
                                        name: id,
                                    } : {
                                        type: "Literal",
                                        value: id,
                                    },
                                    value: this.getReg(field.value),
                                };
                            }),
                        },
                    ],
                });
                break;

            case "return":
                stmts.push({
                    type: "ReturnStatement",
                    argument: this.getReg(insn.src),
                });
                break;

            case "tuple":
                if(insn.values.length === 0) {
                    assign(insn.dest, {
                        type: "Identifier",
                        name: "undefined",
                    });
                }
                else {
                    assign(insn.dest, {
                        type: "ArrayExpression",
                        elements: insn.values.map(element => this.getReg(element)),
                    });
                }
                break;

            case "tuple-element":
                assign(insn.dest, {
                    type: "MemberExpression",
                    object: this.getReg(insn.src),
                    property: {
                        type: "Literal",
                        value: Number(insn.elementIndex),
                    },
                    computed: true,
                    optional: false,
                });
                break;

            case "unbox":
                assign(insn.dest, this.getReg(insn.value));
                break;

            case "unreachable":
                stmts.push({
                    type: "ThrowStatement",
                    argument: {
                        type: "NewExpression",
                        callee: this.moduleEmitter.getArgonRuntimeExport("UnreachableError"),
                        arguments: [],
                    }
                });
                break;

            case "update-reference":
                stmts.push({
                    type: "ExpressionStatement",
                    expression: {
                        type: "AssignmentExpression",
                        operator: "=",
                        left: {
                            type: "MemberExpression",
                            computed: false,
                            optional: false,
                            object: this.getReg(insn.ref),
                            property: {
                                type: "Identifier",
                                name: "value",
                            },
                        },
                        right: this.getReg(insn.value),
                    },
                });
                break;

            default:
                ensureExhaustive(insn);
        }
    }

    private getVariantClass(enumType: ir.Token, variantId: bigint): estree.Expression {
        const variantInfo = this.options.program.getEnumVariantInfo(variantId);


        const variantName = this.getExportNameForId(variantInfo.name);
        const nameValid = isValidIdName(variantName) && variantName !== "__proto__";

        return {
            type: "MemberExpression",
            computed: !nameValid,
            optional: false,

            object: {
                type: "MemberExpression",
                computed: false,
                optional: false,

                object: this.buildTokenValue(enumType),
                property: {
                    type: "Identifier",
                    name: "variants",
                },
            },

            property: nameValid ? {
                type: "Identifier",
                name: variantName,
            } : {
                type: "Literal",
                value: variantName,
            },
        };
    }


    private getReg(reg: ir.RegisterId): estree.Identifier {
        return {
            type: "Identifier",
            name: `r${reg.id}`,
        };
    }

    private wrapInteger(integerType: ir.IntegerType, value: estree.Expression): estree.Expression {
        if(integerType === "int") {
            return value;
        }

        const byteValue: estree.Expression = {
            type: "BinaryExpression",
            left: value,
            operator: "&",
            right: {
                type: "Literal",
                value: 0xFF,
            },
        };

        if(integerType === "u8") {
            return byteValue;
        }

        return {
            type: "BinaryExpression",
            left: {
                type: "BinaryExpression",
                left: byteValue,
                operator: "<<",
                right: {
                    type: "Literal",
                    value: 24,
                },
            },
            operator: ">>",
            right: {
                type: "Literal",
                value: 24,
            },
        };
    }

    private coerceInteger(integerType: ir.IntegerType, value: estree.Expression): estree.Expression {
        return this.wrapInteger(integerType, value);
    }

    private getLabel(label: ir.BlockId): estree.Identifier {
        return {
            type: "Identifier",
            name: `block_${label.id}`,
        };
    }

    private buildTokenValue(token: ir.Token): estree.Expression {
        return this.tokenEmitter.buildTokenValue(token);
    }
}

function jsonToExpression(expr: JsonValue): estree.Expression {
    switch(typeof expr) {
        case "string":
        case "number":
        case "boolean":
            return {
                type: "Literal",
                value: expr,
            };

        case "object":
            if(expr === null) {
                return {
                    type: "Literal",
                    value: null,
                };
            }
            else if(expr instanceof Array) {
                return {
                    type: "ArrayExpression",
                    elements: expr.map(jsonToExpression),
                };
            }
            else {
                const properties: estree.Property[] = [];
                for(const key of Object.keys(expr)) {
                    const value = jsonToExpression(expr[key]!);
                    if(key === "__proto__" || !isValidIdName(key)) {
                        properties.push({
                            type: "Property",
                            computed: true,
                            shorthand: false,
                            method: false,
                            kind: "init",
                            key: {
                                type: "Literal",
                                value: key,
                            },
                            value,
                        });
                    }
                    else {
                        properties.push({
                            type: "Property",
                            computed: false,
                            shorthand: false,
                            method: false,
                            kind: "init",
                            key: {
                                type: "Identifier",
                                name: key,
                            },
                            value,
                        });
                    }
                }
                return {
                    type: "ObjectExpression",
                    properties,
                };
            }

        default:
            ensureExhaustive(expr);
    }
}
