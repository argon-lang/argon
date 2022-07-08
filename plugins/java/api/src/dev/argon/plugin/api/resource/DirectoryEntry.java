package dev.argon.plugin.api.resource;

import java.util.stream.Stream;
import java.io.IOException;

public record DirectoryEntry<E extends Exception>(String name, Resource<E> resource) {
}
