package dev.argon.plugin.api.options;

public final class DuplicateOptionValueException extends Exception {
    public DuplicateOptionValueException(String optionName) {
        super("Duplicate option value: " + optionName);
        this.optionName = optionName;
    }

    private final String optionName;

    public String optionName() {
        return optionName;
    }
}
