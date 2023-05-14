import ts from "typescript";
import * as path from "node:path";
import * as fs from "node:fs/promises";
import { Writable } from "node:stream";


export type ImportInfo =
    | { isModule: false, path: string, realName: string, } 
    | { isModule: true, path: string, };

export abstract class CodeGenerator {
    constructor() {
        [this.#packageName, this.inputFile, this.#outputFile] = (() => {
            const [packageName, inputFile, outputFile] = process.argv.slice(2);
            if(packageName === undefined || inputFile === undefined || outputFile === undefined) {
                throw new Error("Missing arguments");
            }
            return [packageName, inputFile, outputFile];
        })();
    }

    readonly #packageName: string;
    protected readonly inputFile: string;
    readonly #outputFile: string;
    readonly #nameMapping = new Map<string, ImportInfo>();

    getPackageForFile(filename: string): string {
        const name = path.basename(filename);
        if(name === "tube.js" || name === "tube.gen.js" || name == "tube.ts") {
            return `${this.#packageName}.tube`;
        }
        else {
            return this.#packageName;
        }
    }

    getIdName(id: ts.Identifier): string {
        if(typeof(id.escapedText) !== "string") {
            throw new Error("Invalid identifier");
        }
        return id.escapedText;
    }

    lookupName(name: string): ImportInfo | undefined {
        return this.#nameMapping.get(name);
    }

    async generate(): Promise<void> {
        const sourceFile = ts.createSourceFile(
            this.inputFile,
            await fs.readFile(this.inputFile, { encoding: "utf-8" }),
            {
                languageVersion: ts.ScriptTarget.ES2022,
                impliedNodeFormat: ts.ModuleKind.ESNext,
            },
        );

        this.#defineNames(sourceFile);

        const file = await fs.open(this.#outputFile, "w");
        try {
            const stream: WritableStream<string> = Writable.toWeb(file.createWriteStream({ encoding: "utf-8" }));
            const out = stream.getWriter();
            await this.generateStart(out);
            
            const genActions: (() => Promise<void>)[] = [];

            ts.forEachChild(sourceFile, node => {
                genActions.push(() => this.generateNode(out, node));
            });

            for(const action of genActions) {
                await action();
            }

            await this.generateEnd(out);
        }
        finally {
            await file.close();
        }
    }

    #defineNames(sourceFile: ts.SourceFile) {
        const inputFileModulePath = `./${path.basename(this.inputFile, ".ts")}.js`;

        ts.forEachChild(sourceFile, node => {
            if(ts.isImportDeclaration(node)) {
                if(!ts.isStringLiteral(node.moduleSpecifier)) {
                    throw new Error("Could not determine import path");
                }

                const modulePath = node.moduleSpecifier.text;
                if(node.importClause?.namedBindings !== undefined) {
                    switch(node.importClause.namedBindings.kind) {
                        case ts.SyntaxKind.NamespaceImport:
                        {
                            const name = this.getIdName(node.importClause.namedBindings.name);
                            this.#nameMapping.set(name, { isModule: true, path: modulePath });
                            break;
                        }

                        case ts.SyntaxKind.NamedImports:
                            for(const specifier of node.importClause.namedBindings.elements) {
                                const realName = this.getIdName(specifier.propertyName ?? specifier.name);
                                this.#nameMapping.set(this.getIdName(specifier.name), { isModule: false, path: modulePath, realName })
                            }
                            break;
                    }
                }
            }
            else if(ts.isInterfaceDeclaration(node) || ts.isTypeAliasDeclaration(node)) {
                const name = this.getIdName(node.name);
                this.#nameMapping.set(name, { isModule: false, path: inputFileModulePath, realName: name })
            }
            else if(ts.isModuleDeclaration(node) && ts.isIdentifier(node.name)) {
                const name = this.getIdName(node.name);
                this.#nameMapping.set(name, { isModule: false, path: inputFileModulePath, realName: name })
            }
            else if(ts.isExportDeclaration(node) || node.kind === ts.SyntaxKind.EndOfFileToken) {

            }
            else {
                console.log(node);
                throw new Error("Unsupported node type");
            }
        });
    }

    protected async generateNode(out: WritableStreamDefaultWriter<string>, node: ts.Node): Promise<void> {
        if(ts.isInterfaceDeclaration(node)) {
            await this.generateInterface(out, node);
        }
        else if(ts.isTypeAliasDeclaration(node)) {
            await this.generateTypeAlias(out, node);
        }
        else if(ts.isModuleDeclaration(node)) {
            await this.generateNamespace(out, node);
        }
        else if(
            ts.isImportDeclaration(node) ||
            ts.isExportDeclaration(node) ||
            node.kind === ts.SyntaxKind.EndOfFileToken
        ) {
        }
        else {
            console.log(node);
            throw new Error("Unsupported node type");
        }
    }

    protected abstract generateStart(out: WritableStreamDefaultWriter<string>): Promise<void>;



    protected abstract generateInterface(out: WritableStreamDefaultWriter<string>, node: ts.InterfaceDeclaration): Promise<void>;
    protected abstract generateTypeAlias(out: WritableStreamDefaultWriter<string>, node: ts.TypeAliasDeclaration): Promise<void>;
    protected abstract generateNamespace(out: WritableStreamDefaultWriter<string>, node: ts.ModuleDeclaration): Promise<void>;

    protected abstract generateEnd(out: WritableStreamDefaultWriter<string>): Promise<void>;
}







