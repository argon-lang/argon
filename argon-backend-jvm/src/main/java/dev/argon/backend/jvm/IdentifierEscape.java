package dev.argon.backend.jvm;

public class IdentifierEscape {
    private IdentifierEscape() {}


    public static String escapeIdentifier(String identifier) {
        return identifier
            .replace("\\", "\\-")
            .replace("/", "\\|")
            .replace(".", "\\,")
            .replace(";", "\\?")
            .replace("$", "\\%")
            .replace("<", "\\^")
            .replace(">", "\\_")
            .replace("[", "\\{")
            .replace("]", "\\}")
            .replace(":", "\\!");
    }

}
