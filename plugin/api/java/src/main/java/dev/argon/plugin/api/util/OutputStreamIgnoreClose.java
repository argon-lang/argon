package dev.argon.plugin.api.util;

import org.jetbrains.annotations.NotNull;

import java.io.FilterOutputStream;
import java.io.OutputStream;

public class OutputStreamIgnoreClose extends FilterOutputStream {
    public OutputStreamIgnoreClose(@NotNull OutputStream out) {
        super(out);
    }

    @Override
    public void close() {}
}
