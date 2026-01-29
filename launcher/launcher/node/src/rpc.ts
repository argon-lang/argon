import {ESXChannelServer} from "@argon-lang/esexpr-multichannel";
import {Worker} from "node:worker_threads";
import type {ESXChannelServerConnection} from "@argon-lang/esexpr-multichannel";
import type {WorkerResponse} from "./rpc.worker.types.js";
import {CommandResult, DriverCommand} from "@argon-lang/node-driver-api/command.js";
import * as esexpr from "@argon-lang/esexpr";


export async function runRPC(): Promise<void> {
    const workerURL = new URL("./rpc.worker.js", import.meta.url);

    async function doWrite(data: Uint8Array): Promise<void> {
        return new Promise((resolve, reject) => {
            process.stdout.write(data, err => {
                if (err) reject(err);
                else resolve();
            });
        });
    }
    const connectionThreads: Promise<void>[] = [];
    const workers: CompilerWorker[] = [];
    const availableWorkers: CompilerWorker[] = [];

    const driverCommandCodec = DriverCommand.codec(
        esexpr.strCodec,
        esexpr.strCodec,
        esexpr.strCodec,
        esexpr.strCodec,
    );

    async function getAvailableWorker(): Promise<CompilerWorker> {
        let worker = availableWorkers.shift();
        if(worker === undefined) {
            const workerThread = new Worker(workerURL, {
                stdin: true,
                stdout: true,
                stderr: true,

            });

            worker = new CompilerWorker(workerThread, worker => availableWorkers.push(worker));
            workers.push(worker);
        }
        return worker;
    }

    async function processConnection(conn: ESXChannelServerConnection): Promise<void> {
        while(true) {
            const commandExpr = await conn.read();
            if(commandExpr === undefined) break;

            const commandRes = driverCommandCodec.decode(commandExpr);
            if(!commandRes.success) {
                console.error("Could not parse driver command", commandRes.message);
                continue;
            }

            const worker = await getAvailableWorker();
            try {
                worker.runCommand(conn, commandRes.value);
            }
            catch(e) {
                console.error("Error running command", e);
            }
        }
    }

    try {
        const server = ESXChannelServer.create(process.stdin, doWrite);

        while(true) {
            const conn = await server.accept();
            if(conn === undefined) break;

            connectionThreads.push(processConnection(conn));
        }

        await Promise.all(connectionThreads);
    }
    finally {
        for(const worker of workers) {
            await worker.worker.terminate();
        }
    }
}

class CompilerWorker {
    constructor(
        public readonly worker: Worker,
        onComplete: (worker: CompilerWorker) => void,
    ) {
        this.#onComplete = onComplete;
        worker.stdin?.end();
        worker.stdout.on("data", data => this.#processOutput(data));
        worker.stderr.on("data", data => this.#processOutput(data));
        worker.on("message", message => this.#processMessage(message));
    }

    #onComplete: (worker: CompilerWorker) => void;

    #currentConnection: ESXChannelServerConnection | undefined = undefined;
    readonly #textDecoder = new TextDecoder();
    #lastWasBinary = false;

    runCommand(conn: ESXChannelServerConnection, command: DriverCommand<string, string, string, string>) {
        if(this.#currentConnection !== undefined) {
            throw new Error("Cannot run command while another command is running");
        }

        this.#currentConnection = conn;
        this.worker.postMessage(command);
    }

    #processOutput(data: unknown): void {
        let value: string;
        if(data instanceof Uint8Array) {
            value = this.#textDecoder.decode(data, { stream: true });
            this.#lastWasBinary = true;
        }
        else {
            if(this.#lastWasBinary) {
                this.#lastWasBinary = false;
                value = this.#textDecoder.decode() + String(data);
            }
            else {
                value = String(data);
            }
        }

        const response: CommandResult = {
            $type: "output",
            text: value,
        };

        void this.#currentConnection?.write(CommandResult.codec.encode(response));
    }

    #processMessage(message: WorkerResponse): void {
        void this.#completeCommand(message.exitCode);
    }

    async #completeCommand(exitCode: number): Promise<void> {
        const conn = this.#currentConnection;
        this.#currentConnection = undefined;
        if(conn === undefined) return;

        const response: CommandResult = {
            $type: "exit",
            code: exitCode,
        };

        await conn.write(CommandResult.codec.encode(response));

        this.#onComplete(this);
    }

}



