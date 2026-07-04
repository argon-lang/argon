package dev.argon.backend.io;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;

public interface OutputFile {
	OutputStream open() throws IOException;

	void delete() throws IOException;

	static OutputFile fromPath(Path path) {
		return new PathOutputFile(path);
	}

	record PathOutputFile(Path path) implements OutputFile {
		@Override
		public OutputStream open() throws IOException {
			var parent = path.getParent();
			if(parent != null) {
				Files.createDirectories(parent);
			}

			return Files.newOutputStream(path);
		}

		@Override
		public void delete() throws IOException {
			Files.deleteIfExists(path);
		}
	}
}

