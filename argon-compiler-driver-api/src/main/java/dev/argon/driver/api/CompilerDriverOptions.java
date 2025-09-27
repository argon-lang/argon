package dev.argon.driver.api;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public final class CompilerDriverOptions {
    public CompilerDriverOptions(String[] arguments) {
        this.arguments = arguments;
    }
    
    private final List<Path> pluginDirectories = new ArrayList<>();
    private final String[] arguments;
    
    public void addPluginDirectory(Path path) {
        pluginDirectories.add(path);
    }
    
    public List<Path> getPluginDirectories() {
        return pluginDirectories;
    }
    
    public String[] getArguments() {
        return arguments;
    }
    
}
