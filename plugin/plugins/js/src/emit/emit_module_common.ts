import type { EmitTubeInfo, MethodMemberGroup } from "@argon-lang/plugin-api/tube";
import type * as tube from "@argon-lang/plugin-api/tube";
import type { JSOptions } from "../options.js";
import { EmitTubeCommon } from "./emit_tube_common.js";
import { getModuleImportPath, isSameModule } from "./path.js";
import { getOverloadExportName } from "./identifier.js";
import type { ExternImpl } from "../extern.js";

interface MethodInfo {
    isStatic: boolean;
    name: tube.Identifier | null;
    signature: tube.ErasedSignature;
}

export abstract class EmitModuleCommon extends EmitTubeCommon {
    constructor(
        options: JSOptions,
        emitTubeInfo: EmitTubeInfo<ExternImpl, ExternImpl, ExternImpl>,
        metadata: tube.Metadata,
        public readonly moduleName: tube.ModuleName,
        public readonly module: tube.ModuleDefinition,
    ) {
        super(options, emitTubeInfo, metadata);
    }

    // Map from import path to import name mapping.
    // Import name mapping maps exported name to the imported name.
    #nextImportId = 0;
    protected readonly imports = new Map<string, Map<string, string>>();

    getImportName(specifier: tube.ImportSpecifier): string {
        const exportName = getOverloadExportName(specifier.identifier, specifier.signature!);
        if(isSameModule(specifier.moduleName, this.moduleName)) {
            return exportName;
        }
        else {
            const importPath = getModuleImportPath(this, specifier.moduleName);

            let nameMapping = this.imports.get(importPath);
            if(nameMapping === undefined) {
                nameMapping = new Map<string, string>();
                this.imports.set(importPath, nameMapping);
            }
            
            let importName = nameMapping.get(exportName);
            if(importName === undefined) {
                importName = `arimport${this.#nextImportId}`;
                ++this.#nextImportId;
                nameMapping.set(exportName, importName);
            }

            return importName;
        }
    }

    ensureRawImportName(module: tube.ModuleName, name: string): void {
        const importPath = getModuleImportPath(this, module);

        let nameMapping = this.imports.get(importPath);
        if(nameMapping === undefined) {
            nameMapping = new Map<string, string>();
            this.imports.set(importPath, nameMapping);
        }
        
        if(!nameMapping.has(name)) {
            nameMapping.set(name, name);
        }
    }

    async #findDeclarationImport(id: bigint, declType: tube.ModuleDefinition.ElementDeclaration.Type): Promise<tube.ImportSpecifier> {
        for(const modulePath of await this.tube.allModulePaths()) {
            const module = await this.tube.getModule(modulePath);
            for(const elementGroup of module.elements) {
                for(const element of elementGroup.declaredElements) {
                    if(element.type === declType && element.id === id) {
                        return {
                            moduleName: {
                                tube: this.moduleName.tube,
                                path: modulePath,
                            },
                            identifier: elementGroup.name,
                            signature: element.signature,
                        };
                    }
                }
            }
        }

        throw new Error("Could not find declaration of id");
    }
    
    protected async getClassImportSpecifier(id: bigint): Promise<tube.ImportSpecifier> {
        if(id < this.metadata.references.externalClasses.length) {
            return this.metadata.references.externalClasses[Number(id)]!.specifier;
        }
        else {
            return await this.#findDeclarationImport(id, "ClassDeclaration");
        }
    }
    
    protected async getTraitImportSpecifier(id: bigint): Promise<tube.ImportSpecifier> {
        if(id < this.metadata.references.externalTraits.length) {
            return this.metadata.references.externalTraits[Number(id)]!.specifier;
        }
        else {
            return await this.#findDeclarationImport(id, "TraitDeclaration");
        }
    }

    protected async getFunctionImportSpecifier(id: bigint): Promise<tube.ImportSpecifier> {
        if(id < this.metadata.references.externalFunctions.length) {
            return this.metadata.references.externalFunctions[Number(id)]!.specifier;
        }
        else {
            return await this.#findDeclarationImport(id, "FunctionDeclaration");
        }
    }

    protected async getClassConstructorErasedSignature(id: bigint, classType: tube.Expr.ClassType): Promise<tube.ErasedSignature> {
        if(id < this.metadata.references.externalClassConstructors.length) {
            return this.metadata.references.externalClassConstructors[Number(id)]!.erasedSignature;
        }
        else {
            const classDef = await this.tube.getClass(classType.id);
            const ctorMember = classDef.constructors.find(ctor => ctor.id === id);
            if(ctorMember === undefined) {
                throw new Error("Could not find specified constructor id");
            }

            return ctorMember.signature;
        }
    }

    protected async getMethodInfo(id: bigint, ownerType: tube.MethodOwnerType): Promise<MethodInfo> {
        if(id < this.metadata.references.externalMethods.length) {
            const refInfo = this.metadata.references.externalMethods[Number(id)]!;
            return {
                isStatic: refInfo.ownerType === "ClassStatic" || refInfo.ownerType === "TraitStatic",
                name: refInfo.identifier,
                signature: refInfo.erasedSignature,
            };
        }

        let decl: tube.ClassDefinition | tube.TraitDefinition;
        switch(ownerType.$case) {
            case "ClassType":
                decl = await this.tube.getClass(ownerType.id);
                break;

            case "TraitType":
                decl = await this.tube.getTrait(ownerType.id);
                break;
        }

        let isStatic: boolean;
        
        let methodMemberPair = decl.methods
            .flatMap(nameGroup =>
                nameGroup.methods.map(methodMember => ({ nameGroup, methodMember }))
            )
            .find(({ methodMember }) => methodMember.id === id);

        if(methodMemberPair === undefined) {
            isStatic = true;

            methodMemberPair = decl.staticMethods
                .flatMap(nameGroup =>
                    nameGroup.methods.map(methodMember => ({ nameGroup, methodMember }))
                )
                .find(({ methodMember }) => methodMember.id === id);
            
            if(methodMemberPair === undefined) {
                throw new Error("Could not find specified method id");
            }
        }
        else {
            isStatic = false;
        }

        return {
            isStatic,
            name: methodMemberPair.nameGroup.name,
            signature: methodMemberPair.methodMember.signature
        };
    }


}
