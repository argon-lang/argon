import type * as ir from "./vm-format.js";

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
    if(c.length == 1) {
        return "$" + c.charCodeAt(0).toString(16).padStart(2, '0');
    }

    let s = "";
    for(const b of new TextEncoder().encode(c)) {
        s += "$" + b.toString(16).padStart(2, '0');
    }
    return s;
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

export function encodeTubePathComponent(part: string): string {
    return encodeURIComponent(part).replace(".", "%2E");
}



export function tubeNameEquals(a: ir.TubeName, b: ir.TubeName): boolean {
    return a.head == b.head && a.tail.length === b.tail.length && a.tail.every((aPart, i) => aPart === b.tail[i]);
}

export function modulePathEquals(a: ir.ModulePath, b: ir.ModulePath): boolean {
    return a.path.every((aPart, i) => aPart === b.path[i]);
}
