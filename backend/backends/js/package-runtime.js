import * as fs from "node:fs/promises";
import * as path from "node:path";

/**
 * 
 * @param {string} dir 
 */
async function buildDirMap(dir) {
    const dirMap = {};

    /**
     * 
     * @param {string} prefix 
     * @param {string} dir 
     */
    async function addDir(prefix, dir) {
        for(const entry of await fs.readdir(dir)) {
            const entryPath = path.join(dir, entry);
            if((await fs.stat(entryPath)).isDirectory()) {
                await addDir(prefix + entry + "/", entryPath);
            }
            else {
                dirMap[prefix + entry] = await fs.readFile(entryPath, { encoding: "utf-8" });
            }
        }
    }

    await addDir("/", dir);

    return dirMap;
}

const runtimeDirMap = await buildDirMap(path.join(import.meta.dirname, "../../runtime/js/"));

const runtimeSource = "export default " + JSON.stringify(runtimeDirMap);

await fs.writeFile("src/executor/argon-runtime.ts", runtimeSource);

