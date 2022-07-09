package dev.argon.vm;

import java.io.IOException;

@FunctionalInterface
public interface TubeLoader {
    Tube load(Runtime runtime, int tubeNum) throws IOException;
}
