package dev.argon.backend.jvm.jdkloader;

import org.jetbrains.annotations.Nullable;

import java.io.IOException;

public interface JavaLibraryIterator extends AutoCloseable {

    @Nullable JavaLibrary next() throws IOException;

}
