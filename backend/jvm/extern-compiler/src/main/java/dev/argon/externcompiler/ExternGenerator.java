package dev.argon.externcompiler;

import java.util.Collection;

public interface ExternGenerator {
	Collection<byte[]> generate();
}
