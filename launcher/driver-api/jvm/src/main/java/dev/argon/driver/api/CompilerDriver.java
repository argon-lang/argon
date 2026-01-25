package dev.argon.driver.api;

import dev.argon.backend.api.metadata.BackendMetadata;
import dev.argon.driver.api.command.DriverCommand;

import java.util.List;

public interface CompilerDriver {
	BackendMetadata parseMetadata(String metadata) throws BackendMetadataParseException;
	DriverCommand parseCommandLineArguments(List<BackendMetadata> backends, String[] args);
    int runCommand(CompilerDriverOptions options) throws Throwable;
}
