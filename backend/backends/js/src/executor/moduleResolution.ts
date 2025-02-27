import type * as estree from "estree";
import type { PackageJson, ReadonlyDeep } from "type-fest";

export interface ESMResolution {
    resolved: URL;
    format: string | undefined;
}

export class ModuleResolutionError extends Error {}
export class NonFileUrlError extends ModuleResolutionError {}
export class InvalidModuleSpecifierError extends ModuleResolutionError {}
export class UnsupportedDirectoryImport extends ModuleResolutionError {}
export class ModuleNotFoundError extends ModuleResolutionError {}
export class PackagePathNotExportedError extends ModuleResolutionError {}
export class PackageImportNotDefinedError extends ModuleResolutionError {}
export class InvalidPackageTargetError extends ModuleResolutionError {}
export class InvalidPackageConfigurationError extends ModuleResolutionError {}


export class ModuleResolution {

    defaultConditions: string[] = ["import"];
    
    private packageJsonFiles = new Map<string, ReadonlyDeep<PackageJson>>();
    private sourceFiles = new Map<string, ReadonlyDeep<estree.Program>>();
    private directories = new Set<string>([ "/" ]);


    addSourceFile(path: string, content: ReadonlyDeep<estree.Program>): void {
        if(!path.startsWith("/")) {
            path = "/" + path;
        }

        this.sourceFiles.set(path, content);
        this.addDirectory(this.getPathParent(path));
    }

    addPackageJsonFile(path: string, content: ReadonlyDeep<PackageJson>): void {
        if(!path.startsWith("/")) {
            path = "/" + path;
        }

        this.packageJsonFiles.set(path, content);
        this.addDirectory(this.getPathParent(path));
    }

    addDirectory(path: string): void {
        while(path.endsWith("/")) {
            path = path.substring(0, path.length - 1);
        }

        if(!path.startsWith("/")) {
            path = "/" + path;
        }

        while(path.length > 1) {
            this.directories.add(path);
            path = this.getPathParent(path);
        }
    }


    esmResolve(specifier: string, parentURL: URL): ESMResolution {
        const resolved = (() => {
            const specUrl = URL.parse(specifier);
            if(specUrl != null) {
                return specUrl;
            }
    
            if(specifier.startsWith("/") || specifier.startsWith("./") || specifier.startsWith("../")) {
                return new URL(specifier, parentURL);
            }
            else if(specifier.startsWith("#")) {
                return this.packageImportsResolve(specifier, parentURL, this.defaultConditions);
            }
            else {
                return this.packageResolve(specifier, parentURL);
            }
        })();
    
        if(resolved.protocol !== "file:") {
            throw new NonFileUrlError(`Protocols other than file are not supported: ${resolved.protocol}`);
        }

        const resolvedStr = resolved.toString();
        if(resolvedStr.indexOf("%2F") >= 0 || resolvedStr.indexOf("%2f") >= 0 || resolvedStr.indexOf("%5C") >= 0 || resolvedStr.indexOf("%5c") >= 0) {
            throw new InvalidModuleSpecifierError(`Specifier contains invalid characters ${resolvedStr}`);
        }

        if(this.isDirectory(resolved.pathname)) {
            throw new UnsupportedDirectoryImport(`Directory cannot be imported: ${resolved}`);
        }

        if(!this.sourceFiles.has(resolved.pathname)) {
            throw new ModuleNotFoundError(`Module not found at ${resolved}`);
        }

        const format = this.esmFileFormat(resolved);

        return { resolved, format };
    }

    packageResolve(packageSpecifier: string, parentURL: URL): URL {
        if(packageSpecifier === "") {
            throw new InvalidModuleSpecifierError("Package specifier was empty");
        }

        // Not checking for node builtin modules because we won't have any.

        let packageName: string;
        if(packageSpecifier.startsWith("@")) {
            const firstSlash = packageSpecifier.indexOf("/");
            if(firstSlash < 0) {
                throw new InvalidModuleSpecifierError();
            }

            const secondSlash = packageSpecifier.indexOf("/", firstSlash + 1);
            if(secondSlash < 0) {
                packageName = packageSpecifier;
            }
            else {
                packageName = packageSpecifier.substring(0, secondSlash);
            }
        }
        else {
            const slash = packageSpecifier.indexOf("/");
            if(slash < 0) {
                packageName = packageSpecifier;
            }
            else {
                packageName = packageSpecifier.substring(0, slash);
            }
        }

        if(packageName.startsWith(".") || packageName.indexOf("\\") >= 0 || packageName.indexOf("%") >= 0) {
            throw new InvalidModuleSpecifierError("Package name contains invalid characters: " + packageName);
        }

        const packageSubpath = "." + packageSpecifier.substring(packageName.length);
        if(packageSubpath.endsWith("/")) {
            throw new InvalidModuleSpecifierError();
        }

        const selfUrl = this.packageSelfResolve(packageName, packageSubpath, parentURL);
        if(selfUrl !== undefined) {
            return selfUrl;
        }

        while(!this.isRootUrl(parentURL)) {
            const packageURL = new URL("node_modules/" + packageName + "/", parentURL);
            parentURL = new URL("..", parentURL);

            if(!this.isDirectory(packageURL.pathname)) {
                continue;
            }

            const pjson = this.readPackageJson(packageURL);
            if(pjson !== null) {
                if(pjson.exports !== undefined && pjson.exports !== null) {
                    return this.packageExportsResolve(packageURL, packageSubpath, pjson.exports, this.defaultConditions);
                }
            }

            if(packageSubpath === ".") {
                if(typeof pjson?.main === "string") {
                    return new URL(pjson.main, packageURL);
                }
            }

            return new URL(packageSubpath, packageURL);
        }

        throw new ModuleNotFoundError("Could not find module: " + packageSpecifier);
    }

    packageSelfResolve(packageName: string, packageSubpath: string, parentURL: URL): URL | undefined {
        const packageURL = this.lookupPackageScope(parentURL);
        if(packageURL === null) {
            return undefined;
        }

        const pjson = this.readPackageJson(packageURL);
        if(pjson === null || pjson.exports === null || pjson.exports === undefined) {
            return undefined;
        }

        if(pjson.name === packageName) {
            return this.packageExportsResolve(packageURL, packageSubpath, pjson.exports, this.defaultConditions);
        }

        return undefined;
    }

    packageExportsResolve(packageURL: URL, subpath: string, exports: {}, conditions: readonly string[]): URL {
        let hasDot = false;
        let hasNonDot = false;
        if(typeof exports === "object" && !(exports instanceof Array)) {
            const keys = Object.keys(exports);
            for(const key of keys) {
                if(key.startsWith(".")) {
                    hasDot = true;
                }
                else {
                    hasNonDot = true;
                }
            }

            if(hasDot && hasNonDot) {
                throw new InvalidModuleSpecifierError("Exports object contains entries both starting with and without a dot.");
            }
        }

        if(subpath === ".") {
            let mainExport: unknown = undefined;
            if(typeof exports === "string" || exports instanceof Array || (typeof exports === "object" && !hasDot)) {
                mainExport = exports;
            }
            else if(typeof exports === "object" && exports !== null && "." in exports) {
                mainExport = exports["."];
            }

            if(mainExport !== undefined) {
                const resolved = this.packageTargetResolve(packageURL, mainExport, null, false, conditions);
                if(resolved !== null && resolved !== undefined) {
                    return resolved;
                }
            }
        }
        else if(hasDot) {
            const resolved = this.packageImportsExportsResolve(subpath, exports, packageURL, false, conditions);
            if(resolved !== null && resolved !== undefined) {
                return resolved;
            }
        }

        throw new PackagePathNotExportedError("Subpath was not exported: " + subpath);
    }

    packageImportsResolve(specifier: string, parentURL: URL, conditions: readonly string[]): URL {
        if(specifier === "#" || specifier.startsWith("#/")) {
            throw new InvalidModuleSpecifierError("Invalid import specifier: " + specifier);
        }

        const packageURL = this.lookupPackageScope(parentURL);
        if(packageURL != null) {
            const pjson = this.readPackageJson(packageURL);

            if(typeof pjson === "object" && pjson != null && "imports" in pjson && typeof pjson.imports === "object" && pjson.imports !== null) {
                const resolved = this.packageImportsExportsResolve(specifier, pjson.imports, packageURL, true, conditions);
                if(resolved !== null && resolved !== undefined) {
                    return resolved;
                }
            }
        }

        throw new PackageImportNotDefinedError("Import specifier was not defined: " + specifier);
    }

    packageImportsExportsResolve(matchKey: string, matchObj: object, packageURL: URL, isImports: boolean, conditions: readonly string[]): URL | undefined | null {
        if(typeof matchObj !== "object" || matchObj === null) {
            return null;
        }

        if(matchKey in matchObj && matchKey.indexOf("*") < 0) {
            const target: unknown = (matchObj as any)[matchKey];

            if(target !== undefined) {
                return this.packageTargetResolve(packageURL, target, null, isImports, conditions);
            }
        }

        const expansionKeys = Object.keys(matchObj)
            .filter(k => {
                const firstIndex = k.indexOf("*");
                if(firstIndex < 0) {
                    return false;
                }

                return k.indexOf("*", firstIndex + 1) < 0;
            })
            .sort((keyA, keyB) => {
                const baseLengthA = keyA.indexOf("*");
                const baseLengthB = keyB.indexOf("*");
                if(baseLengthA > baseLengthB) return -1;
                if(baseLengthB > baseLengthA) return 1;
                if(keyA.length > keyB.length) return -1;
                if(keyB.length > keyA.length) return -1;
                return 0;
            });

        for(const expansionKey of expansionKeys) {
            const starIndex = expansionKey.indexOf("*");
            const patternBase = expansionKey.substring(0, starIndex);
            if(matchKey.startsWith(patternBase) && matchKey !== patternBase) {
                const patternTrailer = expansionKey.substring(starIndex + 1);
                if(patternTrailer.length === 0 || (matchKey.endsWith(patternTrailer) && matchKey.length >= expansionKey.length)) {
                    const target: unknown = (matchObj as any)[matchKey];
                    const patternMatch = matchKey.substring(patternBase.length, matchKey.length - patternTrailer.length);
                    return this.packageTargetResolve(packageURL, target, patternMatch, isImports, conditions);
                }
            }
        }

        return null;
    }

    packageTargetResolve(packageURL: URL, target: unknown, patternMatch: string | null, isImports: boolean, conditions: readonly string[]): URL | undefined | null {
        if(typeof target === "string") {
            if(!target.startsWith("./")) {
                if(!isImports || target.startsWith("../") || target.startsWith("/") || URL.canParse(target)) {
                    throw new InvalidPackageTargetError("Invalid package target: " + target);
                }

                if(patternMatch !== null) {
                    return this.packageResolve(target.replaceAll("*", patternMatch), new URL(packageURL.toString() + "/"));
                }

                return this.packageResolve(target, packageURL);
            }

            if(this.hasBadSegment(target.substring(2))) {
                throw new InvalidModuleSpecifierError("Invalid module specifier: " + target);
            }

            const resolvedTarget = new URL(target, packageURL);
            if(patternMatch === null) {
                return resolvedTarget;
            }

            if(this.hasBadSegment(patternMatch)) {
                throw new InvalidModuleSpecifierError("Invalid module specifier: " + patternMatch);
            }

            return new URL(resolvedTarget.toString().replaceAll("*", patternMatch));
        }
        else if(target === null) {
            return null;
        }
        else if(target instanceof Array) {
            let res: InvalidPackageTargetError | undefined | null = null;
            
            for(const targetValue of target) {
                try {
                    const resolved = this.packageTargetResolve(packageURL, targetValue, patternMatch, isImports, conditions);
                    if(resolved === undefined) {
                        res = resolved;
                        continue;
                    }
                    return resolved;
                }
                catch(e) {
                    if(e instanceof InvalidPackageTargetError) {
                        res = e;
                        continue;
                    }
                    throw e;
                }
            }

            if(res === undefined || res === null) {
                return res;
            }

            throw res;
        }
        else if(typeof target === "object") {
            if(Object.keys(target).some(k => this.isNumericKey(k))) {
                throw new InvalidPackageConfigurationError();
            }

            for(const [p, targetValue] of Object.entries(target)) {
                if(p === "default" || conditions.indexOf(p) >= 0) {
                    const resolved = this.packageTargetResolve(packageURL, targetValue, patternMatch, isImports, conditions);
                    if(resolved === undefined) {
                        continue;
                    }

                    return resolved;
                }
            }

            return undefined;
        }
        else {
            throw new InvalidPackageTargetError("Invalid package target: " + JSON.stringify(target));
        }
    }

    esmFileFormat(url: URL): string | undefined {
        const s = url.toString();

        if(s.endsWith(".mjs")) return "module";
        if(s.endsWith(".cjs")) return "commonjs";
        if(s.endsWith(".json")) return "json";

        const packageUrl = this.lookupPackageScope(url);
        if(packageUrl === null) {
            return undefined;
        }

        const pjson = packageUrl == null ? null : this.readPackageJson(packageUrl);

        let packageType: string | null = null;
        if(pjson?.type === "module" || pjson?.type === "commonjs") {
            packageType = pjson.type;
        }
        
        if(s.endsWith(".js")) {
            if(packageType !== null) {
                return packageType;
            }

            if(this.detectModuleSyntax(url)) {
                return "module";
            }
            else {
                return "commonjs";
            }
        }

        if(s.match(/\.[^\.\\/]+$/)) {
            if(this.detectModuleSyntax(url)) {
                return "module";
            }
            else {
                return "commonjs";
            }
        }

        return undefined;        
    }

    lookupPackageScope(url: URL): URL | null {
        let scopeURL = url;

        while(!this.isRootUrl(scopeURL)) {
            if(scopeURL.pathname.endsWith("/node_modules/") || scopeURL.pathname.endsWith("/node_modules")) {
                return null;
            }

            const pjsonURL = new URL("package.json", scopeURL);
            if(this.packageJsonFiles.has(pjsonURL.pathname)) {
                return scopeURL;
            }

            scopeURL = new URL("..", scopeURL);
        }

        return null;
    }

    readPackageJson(packageURL: URL): ReadonlyDeep<PackageJson> | null {
        const pjsonURL = new URL("package.json", packageURL);
        if(!this.packageJsonFiles.has(pjsonURL.pathname)) {
            return null;
        }

        return this.packageJsonFiles.get(pjsonURL.pathname)!;
    }

    private hasBadSegment(s: string): boolean {
        const segments = s.split(/\/|\\/);
        return segments.some(seg => {
            seg = decodeURIComponent(seg).toLowerCase();
            return seg === "" || seg === "." || seg === ".." || seg === "node_modules";
        });
    }



    // stubbing for now
    detectModuleSyntax(_url: URL): boolean {
        return true;
    }


    private isDirectory(path: string): boolean {
        if(path !== "/" && path.endsWith("/")) {
            path = path.substring(0, path.length - 1);
        }

        return this.directories.has(path);
    }

    private getPathParent(path: string): string {
        const index = path.lastIndexOf("/");
        if(index > 0) {
            return path.substring(0, index);
        }
        else {
            return "/";
        }
    }

    private isRootUrl(url: URL): boolean {
        return url.toString() == new URL("..", url).toString();
    }

    private isNumericKey(k: string): boolean {
        let n: bigint;
        try {
            n = BigInt(k);
        }
        catch(_) {
            return false;
        }

        if(k !== n.toString()) {
            return false;
        }

        return n >= BigInt(Number.MIN_SAFE_INTEGER) && n <= BigInt(Number.MAX_SAFE_INTEGER);
    }
}


