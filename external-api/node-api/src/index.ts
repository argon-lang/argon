import { rpcStreamTransport, standardRpcConnection, binaryProtocol } from "gcrpc-runtime";
import { ServerFunctions, FileInfo, ServerFunctionCallHandler, MethodCallHandler } from "./api.gen";
import { WritableStream } from "memory-streams";
import * as vm from "vm";
import { Console } from "console";

const serverFunctions: ServerFunctions = {
	async executeJS(entrypoint: string, files: Array<FileInfo>): Promise<string> {
		const stdout = new WritableStream();
		const sandboxConsole = new Console(stdout);

		const sandbox = Object.create(null);
		sandbox.console = Object.create(null);

		vm.createContext(sandbox);

		vm.runInContext(`
			(function(outerConsole) {
				for(let m in outerConsole) {
					console[m] = function(...args) { return outerConsole[m](...args); };
				}
			})
		`, sandbox)(sandboxConsole);

		async function linker(specifier: string, referencingModule: vm.SourceTextModule): Promise<vm.SourceTextModule> {
			const fileInfo = files.find(fi => fi.name === specifier);
			if(fileInfo === undefined) {
				throw new Error("Unknown module \"" + specifier + "\"");
			}

			return new vm.SourceTextModule(fileInfo.content, {
				context: referencingModule.context,
			});
		}

		const mainModule = new vm.SourceTextModule(`
		import * as arCore from "Argon.Core";
		import * as mainModule from "${entrypoint}";

		mainModule.functions["main:(Ar.Unit)->(Ar.Unit)"].value(arCore.unitValue)
		`, {
			context: sandbox,
		});

		await mainModule.link(linker);
		mainModule.instantiate();
		await mainModule.evaluate();
		
		return stdout.toString();
	},
};

const callHandler = ServerFunctionCallHandler(MethodCallHandler)(serverFunctions);

const transport = rpcStreamTransport(process.stdin, process.stdout);
const conn = standardRpcConnection(transport, binaryProtocol, callHandler);

conn.startBackground();
