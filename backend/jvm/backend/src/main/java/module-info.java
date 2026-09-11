module dev.argon.backend {
	requires dev.argon.backend.api;
	requires info.picocli;
	requires static org.jspecify;

	exports dev.argon.backend;
	exports dev.argon.backend.codegen;
	exports dev.argon.backend.externs;
	exports dev.argon.backend.io;
	exports dev.argon.backend.bridge;
}
