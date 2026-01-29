import type {DriverCommand} from "@argon-lang/node-driver-api/command.js";

export interface WorkerRequest {
    command: DriverCommand<string, string, string, string>,
}

export interface WorkerResponse {
    exitCode: number;
}
