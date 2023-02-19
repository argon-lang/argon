package dev.argon.plugins.tube;

import dev.argon.plugin.api.BinaryResource;
import dev.argon.plugin.api.options.Options;
import dev.argon.plugin.api.options.OptionsDecoding;
import org.jetbrains.annotations.NotNull;

@Options(decoding = OptionsDecoding.OUTPUT)
public record TubeOutput<E extends Throwable>(
    @NotNull BinaryResource<E> implementationModule,
    @NotNull BinaryResource<E> interfaceModule
) {}
