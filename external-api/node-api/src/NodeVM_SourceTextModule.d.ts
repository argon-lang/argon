import * as vm from "vm";

declare module "vm" {
	interface SourceTextModuleOptions {
		url?: string;
		context?: vm.Context;
		lineOffset?: number;
		columnOffset?: number;
		initializeImportMeta?: (meta: any, module: SourceTextModule) => void;
		importModuleDynamically?: (specifier: string, module: SourceTextModule) => {};
	}

	interface SourceTextModuleEvaluateOptions {
		timeout?: number;
		breakOnSigint?: boolean;
	}

	class SourceTextModule {
		constructor(code: string, options?: SourceTextModuleOptions);
		readonly dependencySpecifiers: string[];
		readonly error: any;
		context: vm.Context;
		evaluate(options?: SourceTextModuleEvaluateOptions): Promise<any>;
		instantiate(): void;
		link(linker: (specifier: string, referencingModule: SourceTextModule) => Promise<SourceTextModule>): Promise<void>;
		linkingStatus: "unlinked" | "linking" | "linked" | "errored";
		namespace: {};
		status: "uninstantiated" | "instantiating" | "instantiated" | "evaluating" | "evaluated" | "errored";
		url: string;
	}
}