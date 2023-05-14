package dev.argon.plugin.api;

import dev.argon.plugin.api.options.OptionCodec;
import dev.argon.plugin.api.options.OutputHandler;
import dev.argon.plugin.api.tube.SerializedTubePlus;
import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Optional;

public interface Plugin<E extends Throwable, Options, Output, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation> {
    @NotNull OptionCodec<E, Options> optionCodec();
    @NotNull OutputHandler<E, Output> outputHandler();



    @NotNull Output emitTube(@NotNull SerializedTubePlus<E, ExternMethodImplementation, ExternFunctionImplementation, ExternClassConstructorImplementation> tube) throws E;
    
    
    @NotNull Optional<ExternMethodImplementation> loadExternMethod(@NotNull Options options, @NotNull String id) throws E;
    @NotNull Optional<ExternFunctionImplementation> loadExternFunction(@NotNull Options options, @NotNull String id) throws E;
    @NotNull Optional<ExternClassConstructorImplementation> loadExternClassConstructor(@NotNull Options options, @NotNull String id) throws E;
    
    
    @NotNull Map<String, TubeLoader<E, ?>> tubeLoaders();


}
