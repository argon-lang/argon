package dev.argon.plugin.api.options;

import java.util.Set;

public interface OptionHandler<E extends Exception, TOptions, TOptionsBuilder> {
  Set<OptionInfo<E, ?, TOptions, TOptionsBuilder>> options();
  
  TOptionsBuilder createBuilder();
  TOptions build(TOptionsBuilder builder) throws MissingOptionException;
}
