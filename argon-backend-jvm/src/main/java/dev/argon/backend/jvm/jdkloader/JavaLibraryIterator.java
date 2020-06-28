package dev.argon.backend.jvm.jdkloader;

import java.io.IOException;

public interface JavaLibraryIterator extends AutoCloseable {

    JavaLibrary next() throws IOException;

}
