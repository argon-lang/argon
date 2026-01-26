import type * as ir from "@argon-lang/js-backend-api/vm.js";

export function ensureExhaustive(x: never): never {
    throw new Error("Expected exhaustive check, but found unexpected case: " + x);
}

export function urlEncodeIdentifier(s: string): string {
    return s
    .replace("$", "$$")
    .replace(/^[^$_\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}]/u, c => encodeIdentifierChar(c))
    .replace(/[^$_\p{Lu}\p{Ll}\p{Lt}\p{Lm}\p{Lo}\p{Nl}\u200C\u200D\p{Mn}\p{Mc}\p{Nd}\p{Pc}]/u, c => encodeIdentifierChar(c));
}

function encodeIdentifierChar(c: string): string {
    let s = "";
    for(const b of new TextEncoder().encode(c)) {
        s += "$" + b.toString(16).padStart(2, '0');
    }
    return s;
}


export function getModuleOutputFileParts(modulePath: ir.ModulePath): [ string[], string ] {
    if(modulePath.path.length === 0) {
        return [ [], "index.js" ];
    }
    else if(modulePath.path.length === 1 && modulePath.path[0]!.match(/^_*index$/)) {
        return [ [], "_" + encodeTubePathComponent(modulePath.path[0]!) + ".js" ];
    }
    else {
        const parts = modulePath.path.map(part => encodeTubePathComponent(part));
        const fileName = parts[parts.length - 1]! + ".js";
        return [ parts.slice(0, parts.length - 1), fileName ];
    }
}


export function getModulePathUrl(modulePath: ir.ModulePath): string {
    if(modulePath.path.length === 0) {
        return "";
    }
    else if(modulePath.path.length === 1 && modulePath.path[0]!.match(/^_*index$/)) {
        return "_" + encodeTubePathComponent(modulePath.path[0]!) + ".js";
    }
    else {
        return modulePath.path.map(part => encodeTubePathComponent(part)).join("/") + ".js";
    }
}

export function getModulePathExternalUrl(modulePath: ir.ModulePath): string {
    return modulePath.path.map(part => encodeTubePathComponent(part)).join("/");
}

export function encodeTubePathComponent(part: string): string {
    return encodeURIComponent(part).replaceAll(".", "%2E").replaceAll("%", "$");
}

export function tubePackageName(name: ir.TubeName): string {
    return "@argon-tube/" + [ name.head, ...name.tail ].map(encodeTubePathComponent).join(".");
}


export function tubeNameEquals(a: ir.TubeName, b: ir.TubeName): boolean {
    return a.head == b.head && a.tail.length === b.tail.length && a.tail.every((aPart, i) => aPart === b.tail[i]);
}

export function modulePathEquals(a: ir.ModulePath, b: ir.ModulePath): boolean {
    return a.path.every((aPart, i) => aPart === b.path[i]);
}


export function getModuleId(importSpec: ir.ImportSpecifier): bigint {
    switch(importSpec.$type) {
        case "global":
            return importSpec.moduleId;

        case "local":
            return getModuleId(importSpec.parent);
    }
}
