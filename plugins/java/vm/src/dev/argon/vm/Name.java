package dev.argon.vm;

import java.util.List;
import java.util.ArrayList;
import org.checkerframework.checker.nullness.qual.Nullable;

public class Name {
    public Name(List<String> parts) {
        this.parts = new ArrayList<>(parts);
    }

    public Name(dev.argon.vm.format.Name.V1 name) {
        parts = new ArrayList<>(name.parts().size());
        for(int i = 0; i < name.parts().size(); ++i) {
            parts.add(name.parts().get(i));
        }
    }

    public static Name parse(String name) {
        return new Name(List.of(name.split("(?<!\\.)\\.(?!:\\.)")));
    }

    private final List<String> parts;

    @Override
    public boolean equals(@Nullable Object o) {
        if(o instanceof Name other) {
            return parts.equals(other.parts);
        }
        else {
            return false;
        }
    }

    @Override
    public int hashCode() {
        return parts.hashCode();
    }

    @Override
    public String toString() {
        return String.join(".", parts);
    }
}
