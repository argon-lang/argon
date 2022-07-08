package dev.argon.plugin.api.options;

import java.io.IOException;

public non-sealed interface OptionInfoValue<E extends Exception, T, TOptions, TOptionsBuilder> extends OptionInfo<E, T, TOptions, TOptionsBuilder> {
    TOptionsBuilder addOptionValue(TOptionsBuilder prev, String value) throws InvalidOptionValueException, DuplicateOptionValueException;
}
