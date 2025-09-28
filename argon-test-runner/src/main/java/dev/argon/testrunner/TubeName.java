package dev.argon.testrunner;

import com.google.common.collect.ImmutableList;

public record TubeName(
    ImmutableList<String> segments
) {
}
