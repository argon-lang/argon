package dev.argon.plugins.tube;

import dev.argon.plugin.api.options.Options;
import dev.argon.plugin.api.options.OptionsDecoding;

@Options(decoding = OptionsDecoding.NORMAL)
public record TubeOptions<E>() {
}
