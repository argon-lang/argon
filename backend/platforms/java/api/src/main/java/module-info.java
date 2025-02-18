module dev.argon.backend.api {
	requires transitive org.jetbrains.annotations;
	requires transitive dev.argon.nobleidl.runtime;
	requires transitive dev.argon.esexpr;
	requires transitive org.graalvm.polyglot;

	exports dev.argon.backend.api;
	exports dev.argon.vm.api;
}
