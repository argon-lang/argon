import type * as tube from "@argon-lang/plugin-api/tube";
import type { EmitModuleCommon } from "./emit_module_common.js";


function escapeModulePathSegment(seg: string): string {
    return seg.replace("%", "%25").replace("/", "%2F");
}

function escapeTubeSegment(seg: string): string {
    return escapeModulePathSegment(seg).replace(".", "%2E");
}


export interface ModuleFile {
    readonly dir: readonly string[];
    readonly file: string;
    readonly path: tube.ModulePath;
}

export function getModuleFileName(path: tube.ModulePath): ModuleFile {
    if(path.path.length == 0) {
        return {
            dir: [],
            file: "index",
            path,
        };
    }
    else if(path.path.length == 1 && path.path[0]!.startsWith("_")) {
        return {
            dir: [],
            file: "_" + escapeModulePathSegment(path.path[0]!),
            path,
        };
    }
    else if(path.path.length == 1 && path.path[0] === "index") {
        return {
            dir: [],
            file: "_index",
            path,
        };
    }
    else {
        return {
            dir: path.path.slice(1).map(escapeModulePathSegment),
            file: escapeModulePathSegment(path.path[0]!),
            path,
        };
    }
}


function serializeTubeName(tubeName: tube.TubeName): string {
    return tubeName.name.map(escapeTubeSegment).join(".");
}

export function serializeModulePath(modulePath: tube.ModulePath): string {
    return modulePath.path.map(escapeModulePathSegment).join("/");
}



export function isSameTube(a: tube.TubeName, b: tube.TubeName): boolean {
    if(a.name.length !== b.name.length) {
        return false;
    }

    for(let i = 0; i < a.name.length; ++i) {
        if(a.name[i] !== b.name[i]) {
            return false;
        }
    }

    return true;
}

export function isSameModulePath(a: tube.ModulePath, b: tube.ModulePath) {
    if(a.path.length !== b.path.length) {
        return false;
    }

    for(let i = 0; i < a.path.length; ++i) {
        if(a.path[i] !== b.path[i]) {
            return false;
        }
    }

    return true;
}

export function isSameModule(a: tube.ModuleName, b: tube.ModuleName): boolean {
    return isSameTube(a.tube, b.tube) &&
        isSameModulePath(a.path, a.path);
}


export function getTubeImportPath(emitter: EmitModuleCommon, tubeName: tube.TubeName): string {
    const serTubeName = serializeTubeName(tubeName);
    const path = emitter.options?.tubes?.get(serTubeName)?.import_path;
    
    if(path === undefined) {
        throw new Error("Import Path not specified: " + serTubeName);
    }

    return path;
}

function isRelativePathSegment(seg: string | undefined): boolean {
    return seg === "." || seg === "..";
}

function normalizeDir(dir: readonly string[]): readonly string[] {
    const res: string[] = [];

    for(const part of dir) {
        if(part == ".") {
            continue;
        }
        else if(part == "..") {
            if(res.length > 0 && !isRelativePathSegment(res[res.length - 1])) {
                res.pop();
            }
            else {
                res.push(part);
            }
        }
        else {
            res.push(part);
        }
    }

    return res;
}

export function getModuleImportPath(emitter: EmitModuleCommon, moduleName: tube.ModuleName): string {
    const isLocal = isSameTube(emitter.moduleName.tube, moduleName.tube);

    let tubeImportPath: readonly string[] = isLocal
        ? [ "." ]
        : getTubeImportPath(emitter, moduleName.tube).split("/");

    const fileName = getModuleFileName(moduleName.path);

    let dir: readonly string[];
    if(isRelativePathSegment(tubeImportPath[0])) {
        const packageRoot = new Array<string>(getModuleFileName(emitter.moduleName.path).dir.length).fill("..");
        const normDir = normalizeDir([ ...packageRoot, ...tubeImportPath, ...fileName.dir ]);
        if(isRelativePathSegment(normDir[0])) {
            dir = normDir;
        }
        else {
            dir = [ ".", ...normDir ];
        }
    }
    else {
        dir = [ ...tubeImportPath, ...normalizeDir(fileName.dir) ];
    }

    return dir.join("/") + "/" + fileName.file + (isLocal ? ".js" : "");
}
