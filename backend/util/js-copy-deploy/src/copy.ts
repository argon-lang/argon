import * as fs from 'node:fs/promises';
import * as path from 'node:path';
import { type PathPredicate, readNpmIgnore } from "./ignores.js";
import {listNonDevModules} from "./modules.js";

async function copy(src: string, dest: string, subpath: string, ignorePredicate: PathPredicate): Promise<void> {
    const stats = await fs.stat(src);

    if(ignorePredicate(subpath, stats.isDirectory())) {
        return;
    }

    if (stats.isSymbolicLink()) {
        const realPath = await fs.readlink(src);
        await copy(realPath, dest, subpath, ignorePredicate);
    }
    else if (stats.isDirectory()) {
        await fs.mkdir(dest, {recursive: true});
        const entries = await fs.readdir(src);

        for(const entry of entries) {
            await copy(path.join(src, entry), path.join(dest, entry), path.join(subpath, entry), ignorePredicate);
        }
    }
    else {
        await fs.copyFile(src, dest);
    }
}

async function copyPackageNoRec(src: string, dest: string): Promise<void> {
    const ignorePredicate = await readNpmIgnore(path.join(src, ".npmignore"));

    await copy(src, dest, "", ignorePredicate);
}

export async function copyPackage(src: string, dest: string): Promise<void> {
    await copyPackageNoRec(src, dest);

    for(const pkg of await listNonDevModules(src)) {
        const destPkg = path.join(dest, pkg);
        await fs.mkdir(destPkg, { recursive: true });
        await copyPackageNoRec(path.join(src, pkg), destPkg);
    }
}


