package dev.argon.plugin.adapter.js;

import dev.argon.plugin.api.BinaryResource;
import dev.argon.plugin.api.DirectoryEntry;
import dev.argon.plugin.api.DirectoryResource;
import org.graalvm.polyglot.HostAccess;
import org.graalvm.polyglot.Value;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.IOException;

public class WrapDirectoryResource<E extends Throwable> {

    public WrapDirectoryResource(@NotNull JSEnv<E> env, DirectoryResource<E, ? extends BinaryResource<E>> directoryResource) {
        this.env = env;
        this.directoryResource = directoryResource;
        fileName = directoryResource.fileName().orElse(null);
    }


    private final JSEnv<E> env;
    private final DirectoryResource<E, ? extends BinaryResource<E>> directoryResource;


    @HostAccess.Export
    public final String resourceType = "directory";

    @HostAccess.Export
    public final @Nullable String fileName;

    @HostAccess.Export
    public Value contents() throws E {
        return StreamAsAsyncIterable.toAsyncIterable(env, () -> directoryResource.contents().map(this::wrapDirectoryEntry));
    }

    @HostAccess.Export
    public @Nullable GetNumEntriesFunction<E> numEntries() {
        if(directoryResource.numEntries().isEmpty()) {
            return null;
        }
        else {
            return () -> {
                var n = directoryResource.numEntries().get().get();

                env.lock.lock();
                try {
                    return env.context.eval("js", "BigInt").execute(n.toString());
                }
                finally {
                    env.lock.unlock();
                }
            };
        }
    }

    @FunctionalInterface
    public static interface GetNumEntriesFunction<E extends Throwable> {
        Value get() throws E, IOException, InterruptedException;
    }

    private Value wrapDirectoryEntry(DirectoryEntry<E, ? extends BinaryResource<E>> entry) {
        env.lock.lock();
        try {
            if(entry instanceof DirectoryEntry.File<E, ? extends BinaryResource<E>> fileEntry) {
                return env.context.asValue(new WrapDirectoryEntryFile<E>(env, fileEntry));
            }
            else if(entry instanceof DirectoryEntry.Subdirectory<E, ? extends BinaryResource<E>> dirEntry) {
                return env.context.asValue(new WrapDirectoryEntrySubdirectory<E>(env, dirEntry));
            }
            else {
                throw new RuntimeException("Unexpected entry");
            }
        }
        finally {
            env.lock.unlock();
        }
    }

    private static final class WrapDirectoryEntryFile<E extends Throwable> {

        public WrapDirectoryEntryFile(JSEnv<E> env, DirectoryEntry.File<E, ? extends BinaryResource<E>> entry) {
            name = entry.name();
            resource = new WrapBinaryResource<>(env, entry.resource());
        }

        @HostAccess.Export
        public final @NotNull String entryType = "file";

        @HostAccess.Export
        public final @NotNull String name;

        @HostAccess.Export
        public final @NotNull WrapBinaryResource<E> resource;
    }

    private static final class WrapDirectoryEntrySubdirectory<E extends Throwable> {

        public WrapDirectoryEntrySubdirectory(JSEnv<E> env, DirectoryEntry.Subdirectory<E, ? extends BinaryResource<E>> entry) {
            name = entry.name();
            resource = new WrapDirectoryResource<>(env, entry.resource());
        }

        @HostAccess.Export
        public final @NotNull String entryType = "file";

        @HostAccess.Export
        public final @NotNull String name;

        @HostAccess.Export
        public final @NotNull WrapDirectoryResource<E> resource;
    }

}
