module dev.argon.backend.api {
	requires transitive dev.argon.esexpr;
	requires transitive dev.argon.nobleidl.runtime;
	requires static org.jspecify;

	exports dev.argon.backend.api;
	exports dev.argon.jvmbackendmetadata;
	exports dev.argon.tasks;
	exports dev.argon.vm;
}
