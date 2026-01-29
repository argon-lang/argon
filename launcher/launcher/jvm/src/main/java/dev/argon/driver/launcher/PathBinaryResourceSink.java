package dev.argon.driver.launcher;

import dev.argon.backend.api.BinaryResourceSink;
import dev.argon.backend.api.BinaryWriter;
import dev.argon.backend.api.ScopedResource;
import dev.argon.nobleidl.runtime.ErrorType;
import org.jspecify.annotations.NonNull;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;

class PathBinaryResourceSink implements BinaryResourceSink<IOException> {
	public PathBinaryResourceSink(Path path) {
		this.path = path;
	}

	private final Path path;

	@Override
	public @NonNull <E_E extends Throwable> ScopedResource<IOException, BinaryWriter<IOException>> sink(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException {
		return new IOExceptionBinaryWriterScopedResource();
	}

	private class IOExceptionBinaryWriterScopedResource implements ScopedResource<IOException, BinaryWriter<IOException>> {
		private OutputStream os;
		private OutputStreamBinaryWriter writer;

		@Override
		public synchronized @NonNull <E_E extends Throwable> BinaryWriter<IOException> get(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException, E_E {
			try {
				if(os == null) {
					os = Files.newOutputStream(path);
				}

				if(writer == null) {
					writer = new OutputStreamBinaryWriter(os);
				}

				return writer;
			}
			catch(IOException e) {
				throw errorType_e.toThrowable(e);
			}
		}

		@Override
		public synchronized <E_E extends Throwable> void close(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException {
			if(os != null) {
				try {
					os.close();
				}
				catch(IOException e) {
					throw new UncheckedIOException(e);
				}
			}
		}
	}
}
