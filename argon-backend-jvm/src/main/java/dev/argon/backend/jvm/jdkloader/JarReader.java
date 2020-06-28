package dev.argon.backend.jvm.jdkloader;

import java.io.IOException;
import java.io.InputStream;
import java.util.jar.Manifest;

public interface JarReader {

    String getJarName();
    Manifest getManifest() throws IOException;
    InputStream getEntryStream(String path) throws IOException;

}
