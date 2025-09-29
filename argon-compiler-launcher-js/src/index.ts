import { compilerDriver } from "./argon.js";
import * as path from "node:path";

process.exitCode = await compilerDriver.runCommand({
    pluginDirectories: [
        path.join(import.meta.dirname, "../../backends"),
    ],
    arguments: process.argv.slice(2),
});
