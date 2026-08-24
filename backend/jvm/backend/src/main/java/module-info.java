module dev.argon.backend {
	requires dev.argon.backend.api;
	requires info.picocli;

	exports dev.argon.backend;
	exports dev.argon.backend.codegen;
	exports dev.argon.backend.externs;
	exports dev.argon.backend.io;
}
