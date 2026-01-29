import {compilerDriver as driver} from "./argon.js";
import type {CompilerDriverOptions} from "@argon-lang/node-driver-api";
import {loadBackendFactories} from "./backends.js";
import {convertCommand} from "./options.js";
import {runRPC} from "./rpc.js";

const backendFactories = await loadBackendFactories(driver);
let useRpc = false;
try {
    const backendMetadata = backendFactories.map(f => f.metadata);

    const args = process.argv.slice(2);
    const commandStr = driver.parseCommandLineArguments(backendMetadata, args);
    const command = convertCommand(commandStr);

    if(command.$type === "rpc") {
        useRpc = true;
    }
    else {
        const options: CompilerDriverOptions = {
            backendFactories,
            command,
        }

        process.exitCode = await driver.runCommand(options);
    }
}
finally {
    for(const factory of backendFactories) {
        await factory.close();
    }
}

if(useRpc) {
    await runRPC();
}


