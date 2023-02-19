package dev.argon.plugin.api;

import org.jetbrains.annotations.NotNull;

import java.util.Optional;

public sealed interface FileSystemResource<E extends Throwable> permits BinaryResource, DirectoryResource {
    @NotNull Optional<String> fileName();
}
