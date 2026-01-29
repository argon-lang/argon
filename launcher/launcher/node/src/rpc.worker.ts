import { parentPort as parentPort0 } from "node:worker_threads";
import type {DriverCommand} from "@argon-lang/node-driver-api/command.js";
import { convertCommand } from "./options.js";
import { compilerDriver as driver } from "./argon.js";
import { loadBackendFactories } from "./backends.js";
import type {WorkerResponse} from "./rpc.worker.types.js";

if(parentPort0 === null) {
    throw new Error("This file should only be imported in a worker thread.");
}

const parentPort = parentPort0;


parentPort.on("message", runCommand);

async function runCommand(command: DriverCommand<string, string, string, string>): Promise<void> {
    let responseMessage: WorkerResponse
    try {
        const liveCommand = convertCommand(command);
        const backendFactories = await loadBackendFactories(driver);
        const options = { backendFactories, command: liveCommand };
        const exitCode = await driver.runCommand(options);
        responseMessage = { exitCode };
    }
    catch(e) {
        console.error(e);
        responseMessage = { exitCode: 1 };
    }
    parentPort.postMessage(responseMessage)
}

