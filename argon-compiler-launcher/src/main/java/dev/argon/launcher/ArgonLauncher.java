package dev.argon.launcher;

import dev.argon.driver.CompilerDriver;
import dev.argon.driver.api.CompilerDriverOptions;

public final class ArgonLauncher {
    private ArgonLauncher() { }
    
    
    public static void main(String[] args) {
        var options = new CompilerDriverOptions(args);
        new CompilerDriver().runCommand(options);
    }
    
}
