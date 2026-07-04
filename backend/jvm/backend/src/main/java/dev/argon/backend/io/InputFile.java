package dev.argon.backend.io;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

public interface InputFile {
	String fileName();

	InputStream open() throws IOException;

	static InputFile fromPath(Path path) {
		return new PathInputFile(path);
	}

	record PathInputFile(Path path) implements InputFile {
		@Override
		public String fileName() {
			var fileName = path.getFileName();
			return fileName == null ? path.toString() : fileName.toString();
		}

		@Override
		public InputStream open() throws IOException {
			return Files.newInputStream(path);
		}
	}
}
