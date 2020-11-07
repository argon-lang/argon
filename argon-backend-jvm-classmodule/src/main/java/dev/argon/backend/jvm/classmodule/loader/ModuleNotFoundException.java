package dev.argon.backend.jvm.classmodule.loader;

public class ModuleNotFoundException extends Exception {
    public ModuleNotFoundException(String moduleName) {
        this.moduleName = moduleName;
    }


    private final String moduleName;

    @Override
    public String getMessage() {
        return "Could not find Argon module: " + moduleName;
    }
}
