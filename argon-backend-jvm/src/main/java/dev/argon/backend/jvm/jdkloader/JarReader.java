package dev.argon.backend.jvm.jdkloader;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;
import java.io.InputStream;
import java.util.jar.Manifest;

public interface JarReader {

    @NotNull String getJarName();
    @Nullable Manifest getManifest() throws IOException;
    @Nullable InputStream getEntryStream(@NotNull String path) throws IOException;

}
