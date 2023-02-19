package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

public sealed interface DirectoryEntry<E extends Throwable, FileResource extends BinaryResource<E>> {
    @NotNull String name();
    
    public static record Subdirectory<E extends Throwable, FileResource extends BinaryResource<E>>(@NotNull String name, @NotNull DirectoryResource<E, FileResource> resource) implements DirectoryEntry<E, FileResource> {}
    public static record File<E extends Throwable, FileResource extends BinaryResource<E>>(@NotNull String name, @NotNull FileResource resource) implements DirectoryEntry<E, FileResource> {}
}
