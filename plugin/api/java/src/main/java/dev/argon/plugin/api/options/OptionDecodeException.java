package dev.argon.plugin.api.options;

import org.jetbrains.annotations.NotNull;

public class OptionDecodeException extends Exception {
    public OptionDecodeException(@NotNull String message) {
        super(message);
        this.message = message;
    }

    private final @NotNull String message;
    
    @Override
    public @NotNull String getMessage() {
        return message;
    }
}
