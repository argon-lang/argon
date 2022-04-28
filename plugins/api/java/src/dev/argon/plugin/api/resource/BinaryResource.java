package dev.argon.plugin.api.resource;

import dev.argon.plugin.api.*;
import java.nio.channels.SeekableByteChannel;
import java.io.InputStream;
import java.io.IOException;
import org.checkerframework.checker.nullness.qual.*;

public non-sealed abstract class BinaryResource<E extends Exception> extends Resource<E> {
  public abstract InputStream asInputStream() throws IOException, E, PluginException;

  public @Nullable SeekableByteChannel asSeekableByteChannel() throws IOException, E, PluginException {
    return null;
  }
}
