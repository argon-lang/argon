import { EmitModuleCommon } from "./emit_module_common.js";
import { serializeModulePath } from "./path.js";
import type * as tube from "@argon-lang/plugin-api/tube";
import type * as estree from "estree";
import { ExprEmitter } from "./expr_emitter.js";


export const argonRuntimeName = "argonRuntime";


export class ModuleEmitter extends EmitModuleCommon {
    async program(): Promise<estree.Program> {
        const exports = (
            await Promise.all(
                this.module.elements.flatMap(nameGroup =>
                    nameGroup.declaredElements.map(element => this.#elementDeclaration(nameGroup.name, element))
                )
            )
        ).filter<estree.ExportNamedDeclaration>((decl: estree.ExportNamedDeclaration | null): decl is estree.ExportNamedDeclaration => decl !== null);

        const moduleOptions = this.options?.modules?.get(serializeModulePath(this.moduleName.path));

        const injectBefore = moduleOptions?.inject_before ? (await moduleOptions.inject_before.asModule()).body : [];
        const injectAfter = moduleOptions?.inject_after ? (await moduleOptions.inject_after.asModule()).body : [];

        const jsImports: readonly estree.ImportDeclaration[] = [...this.imports]
            .map(([importPath, nameMapping]) => ({
                type: "ImportDeclaration",
                source: {
                    type: "Literal",
                    value: importPath,
                },
                specifiers: [...nameMapping].map(([ exportedName, importedName ]) => ({
                    type: "ImportSpecifier",
                    imported: {
                        type: "Identifier",
                        name: exportedName,
                    },
                    local: {
                        type: "Identifier",
                        name: importedName,
                    },
                })),
            }));

        return {
            type: "Program",
            sourceType: "module",
            body: [
                {
                    type: "ImportDeclaration",
                    source: {
                        type: "Literal",
                        value: "@argon-lang/runtime"
                    },
                    specifiers: [
                        {
                            type: "ImportNamespaceSpecifier",
                            local: {
                                type: "Identifier",
                                name: argonRuntimeName,
                            },
                        },
                    ],
                },

                ...jsImports,
                ...injectBefore,
                ...exports,
                ...injectAfter,
            ],
        }
    }

    async #elementDeclaration(name: tube.Identifier | null, element: tube.ModuleDefinition.ElementDeclaration): Promise<estree.ExportNamedDeclaration | null> {
        const exprEmitter = new ExprEmitter(this.options, this.emitTubeInfo, this.metadata, this.moduleName, this.module);

        switch(element.type) {
            case "ClassDeclaration":
                return await exprEmitter.classExport(name, element, await this.tube.getClass(element.id));
            
            case "TraitDeclaration":
                return await exprEmitter.emitTrait(name, element, await this.tube.getTrait(element.id));
            
            case "FunctionDeclaration":
                return await exprEmitter.emitFunction(name, element, await this.tube.getFunction(element.id));
        }
    }

    
}

