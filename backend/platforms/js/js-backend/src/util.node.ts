import { encodeTubePathComponent } from "./util.js";
import type * as ir from "./vm-format.js";
import * as path from "node:path";

export function getModuleOutputFile(outputDir: string, modulePath: ir.ModulePath): string {
    if(modulePath.path.length === 0) {
        return path.join(outputDir, "index.js");
    }
    else if(modulePath.path.length === 1 && modulePath.path[0]!.match(/^_*index$/)) {
        return path.join(outputDir, "_" + encodeTubePathComponent(modulePath.path[0]!) + ".js");
    }
    else {
        return path.join(outputDir, ...modulePath.path.map(part => encodeTubePathComponent(part))) + ".js";
    }
}
