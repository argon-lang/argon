package dev.argon.module.java_format;

public class ModuleFormatVersion {
    public static int currentVersion() {
        return Metadata.getDescriptor().getOptions().getExtension(ArgonModule.currentFormatVersion);
    }
}
