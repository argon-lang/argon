package dev.argon.driver.api;

import java.util.List;
import dev.argon.backend.api.BackendFactory;
import dev.argon.driver.api.command.DriverCommand;

public final class CompilerDriverOptions {
    public CompilerDriverOptions(
		List<BackendFactory> backendFactories,
		DriverCommand command
    ) {
        this.backendFactories = backendFactories;
        this.command = command;
    }
    
    private final List<BackendFactory> backendFactories;
    private final DriverCommand command;
    
	public List<BackendFactory> getBackendFactories() {
		return backendFactories;
	}
	
    public DriverCommand getCommand() {
        return command;
    }
    
}
