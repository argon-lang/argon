package dev.argon.driver.api;

public final class CompilerDriverOptions {
    public CompilerDriverOptions(String[] arguments) {
        this.arguments = arguments;
    }
    
    private final String[] arguments;
    
    public String[] getArguments() {
        return arguments;
    }
    
}
