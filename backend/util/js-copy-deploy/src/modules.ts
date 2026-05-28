


import * as fs from 'fs/promises';
import * as path from 'path';


export async function listNonDevModules(projectRoot: string): Promise<string[]> {
    const lockPath = path.join(projectRoot, "package-lock.json");
    const raw = await fs.readFile(lockPath, "utf8");
    const lock = JSON.parse(raw);

    const results = new Set<string>();

    if (lock && typeof lock === 'object' && lock.packages && typeof lock.packages === 'object') {
        for (const [pkgPath, pkgInfo] of Object.entries<unknown>(lock.packages)) {
            if(!pkgPath.startsWith("node_modules/")) {
                continue;
            }

            if(typeof pkgInfo === "object" && pkgInfo !== null && "dev" in pkgInfo && pkgInfo.dev === true) {
                continue;
            }

            try {
                await fs.access(path.join(projectRoot, pkgPath));
            }
            catch {
                continue;
            }

            results.add(pkgPath);
        }
    }

    return Array.from(results).sort();
}
