package dev.argon.plugin.api.options;

public final class InvalidOptionValueException extends Exception {
    public InvalidOptionValueException(String optionName) {
        super("Invalid option value: " + optionName);
        this.optionName = optionName;
    }

    private final String optionName;

    public String optionName() {
        return optionName;
    }
}
