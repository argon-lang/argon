package dev.argon.driver.launcher;

import dev.argon.backend.api.BinaryResource;
import dev.argon.nobleidl.runtime.util.IOErrorType;
import dev.argon.nobleidl.runtime.util.InputStreamWithError;

import java.io.IOException;
import java.io.InterruptedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.function.Function;

class PathBinaryResource extends BinaryResource<IOException> {
	public PathBinaryResource(Path path) {
		this.path = path;
	}

	private final Path path;

	@Override
	public Optional<String> fileName() {
		return Optional.ofNullable(path.getFileName()).map(Path::toString);
	}
	
	@Override
	public <EE extends IOException> InputStreamWithError<EE> asInputStream(IOErrorType<IOException, EE> errorType) throws InterruptedIOException, EE {
		try {
			try {
				var is = Files.newInputStream(path);
				return InputStreamWithError.ofInputStream(is)
					.convertError(
						IOErrorType.fromClass(IOException.class),
						Function.identity(),
						errorType
					);
			}
			catch(IOException e) {
				throw errorType.toThrowable(e);
			}
		}
		catch(InterruptedException e) {
			var ex = new InterruptedIOException(e.getMessage());
			ex.setStackTrace(e.getStackTrace());
			throw ex;
		}
	}
}
