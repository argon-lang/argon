package dev.argon.plugin.api.options;

import java.util.Set;
import dev.argon.plugin.api.resource.Resource;

public interface OutputHandler<E extends Exception, TOptions> {
  Set<OutputInfo<? extends Resource<E>, TOptions>> options();
}
