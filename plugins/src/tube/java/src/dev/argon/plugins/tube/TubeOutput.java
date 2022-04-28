package dev.argon.plugins.tube;

import dev.argon.plugin.api.options.*;
import java.nio.file.Path;
import java.util.List;
import org.checkerframework.checker.nullness.qual.*;

@Options(decoding = OptionsDecoding.OUTPUT)
record TubeOutput<E extends Exception>(
    @Option(name = "definition-tube", description = "Generates a tube that contains definitions.")
    TubeResource<E> definitionTube,

    @Option(name = "interface-tube", description = "Generates a tube that contains only the interfaces.")
    TubeResource<E> interfaceTube
) {
}
