package dev.argon.backend.jsApi;

import org.apache.commons.compress.utils.SeekableInMemoryByteChannel;
import org.graalvm.polyglot.io.FileSystem;
import org.jetbrains.annotations.NotNull;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.FileAttribute;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

final class MemoryFileSystem implements FileSystem {
    public MemoryFileSystem(Map<String, String> files) {
        fileStore = new HashMap<>(files);
    }

    private final Map<String, String> fileStore;

    @Override
    public Path parsePath(URI uri) {
        return Path.of(uri);
    }

    @Override
    public Path parsePath(String path) {
        return Path.of(path);
    }

    @Override
    public void checkAccess(Path path, Set<? extends AccessMode> modes, LinkOption... linkOptions) throws IOException {
        if (!fileStore.containsKey(getFullPath(path)) && !isDirectory(path)) {
            throw new FileNotFoundException();
        }
    }

    @Override
    public void createDirectory(Path dir, FileAttribute<?>... attrs) throws IOException {
        throw new IOException("File system is read only");
    }

    @Override
    public void delete(Path path) throws IOException {
        throw new IOException("File system is read only");
    }

    @Override
    public SeekableByteChannel newByteChannel(Path path, Set<? extends OpenOption> options, FileAttribute<?>... attrs) throws IOException {
        var content = fileStore.get(getFullPath(path));
        if (content == null) {
            throw new FileNotFoundException();
        }

        return new SeekableInMemoryByteChannel(content.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public DirectoryStream<Path> newDirectoryStream(Path dir, DirectoryStream.Filter<? super Path> filter) throws IOException {
        throw new UnsupportedOperationException();
    }

    @Override
    public Path toAbsolutePath(Path path) {
        return Path.of("/").resolve(path);
    }

    @Override
    public Path toRealPath(Path path, LinkOption... linkOptions) throws IOException {
        return path;
    }

    @Override
    public Map<String, Object> readAttributes(Path path, String attributes, LinkOption... options) throws IOException {
        return Map.of();
    }

    static String getFullPath(Path path) {
        var pathSegIterable = new Iterable<String>() {
            @Override
            public @NotNull Iterator<String> iterator() {
                final var pathIterator = path.normalize().iterator();
                return new Iterator<>() {
                    @Override
                    public boolean hasNext() {
                        return pathIterator.hasNext();
                    }

                    @Override
                    public String next() {
                        return pathIterator.next().toString();
                    }
                };
            }
        };

        return "/" + String.join("/", pathSegIterable);
    }

    private boolean isDirectory(Path path) {
        var dir = getFullPath(path) + "/";

        for (var entry : fileStore.entrySet()) {
            if (entry.getKey().startsWith(dir)) {
                return true;
            }
        }

        return false;
    }
}
