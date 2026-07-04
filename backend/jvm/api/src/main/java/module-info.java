module dev.argon.backend.api {
	requires transitive dev.argon.esexpr;
	requires transitive dev.argon.nobleidl.runtime;

	exports dev.argon.backend.api;
	exports dev.argon.jvmbackendmetadata;
	exports dev.argon.vm;
}
