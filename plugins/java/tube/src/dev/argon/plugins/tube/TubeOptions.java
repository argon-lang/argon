package dev.argon.plugins.tube;

import dev.argon.plugin.api.options.*;
import java.nio.file.Path;
import java.util.List;
import org.checkerframework.checker.nullness.qual.*;

@Options(decoding = OptionsDecoding.NORMAL)
record TubeOptions<E extends Exception>(
) {
}
