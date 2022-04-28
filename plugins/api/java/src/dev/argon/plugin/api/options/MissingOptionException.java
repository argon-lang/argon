package dev.argon.plugin.api.options;

public final class MissingOptionException extends Exception {
    public MissingOptionException(String optionName) {
        super("Missing option value: " + optionName);
        this.optionName = optionName;
    }

    private final String optionName;

    public String optionName() {
        return optionName;
    }
}
