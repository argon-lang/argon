import { rpcStreamTransport, standardRpcConnection, binaryProtocol } from "gcrpc-runtime";
import { ServerFunctions, FileInfo, ServerFunctionCallHandler, MethodCallHandler } from "./api.gen";
import * as vm from "vm";

const serverFunctions: ServerFunctions = {
	async executeJS(entrypoint: string, files: Array<FileInfo>): Promise<string> {
		let output = "";
		const sandbox = vm.createContext({
			console: {
				log: function(message: any): void {
					output += String(message) + "\n";
				},
			},
		});

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
		
		return output;
	},
};

const callHandler = ServerFunctionCallHandler(MethodCallHandler)(serverFunctions);

const transport = rpcStreamTransport(process.stdin, process.stdout);
const conn = standardRpcConnection(transport, binaryProtocol, callHandler);

conn.startBackground();
