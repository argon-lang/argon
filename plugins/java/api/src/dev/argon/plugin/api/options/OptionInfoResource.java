package dev.argon.plugin.api.options;

import dev.argon.plugin.api.resource.*;
import java.io.IOException;

public non-sealed interface OptionInfoResource<E extends Exception, T, TOptions, TOptionsBuilder> extends OptionInfo<E, T, TOptions, TOptionsBuilder> {
    TOptionsBuilder addOptionValue(TOptionsBuilder prev, BinaryResource<E> value) throws InvalidOptionValueException, DuplicateOptionValueException;
}
