package dev.argon.plugin.api_v1;

import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.List;

public abstract class PluginOptionValue {
    private PluginOptionValue() {}

    public static final class StringValue {
        public StringValue(@NotNull String value) {
            this.value = value;
        }

        private final @NotNull String value;

        public @NotNull String getValue() {
            return value;
        }
    }

    public static final class SingleFileValue {
        public SingleFileValue(@NotNull String fileName) {
            this.fileName = fileName;
        }

        private final @NotNull String fileName;

        public @NotNull String getFileName() {
            return fileName;
        }
    }

    public static final class FileListValue {
        public FileListValue(@NotNull List<@NotNull String> files) {
            this.files = Collections.unmodifiableList(List.copyOf(files));
        }

        private final @NotNull List<@NotNull String> files;

        public @NotNull List<@NotNull String> getFiles() {
            return files;
        }
    }
}
