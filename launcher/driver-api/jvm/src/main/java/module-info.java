module dev.argon.driver.api {
	requires static org.jetbrains.annotations;
	requires transitive dev.argon.nobleidl.runtime;
	requires transitive dev.argon.esexpr;
	requires transitive dev.argon.backend.api;
	
	exports dev.argon.driver.api;
	exports dev.argon.driver.api.command;
}
