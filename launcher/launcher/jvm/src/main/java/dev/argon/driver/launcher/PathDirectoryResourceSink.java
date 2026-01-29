package dev.argon.driver.launcher;

import dev.argon.backend.api.BinaryWriter;
import dev.argon.backend.api.DirectoryResourceSink;
import dev.argon.backend.api.DirectoryWriter;
import dev.argon.backend.api.ScopedResource;
import dev.argon.nobleidl.runtime.ErrorType;
import org.jspecify.annotations.NonNull;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

class PathDirectoryResourceSink implements DirectoryResourceSink<IOException> {
	public PathDirectoryResourceSink(Path path) {
		this.path = path;
	}

	private final Path path;

	@Override
	public @NonNull <E_E extends Throwable> ScopedResource<IOException, DirectoryWriter<IOException>> sink(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException {
		return new ScopedResource<IOException, DirectoryWriter<IOException>>() {
			private boolean hasCreatedDir = false;

			@Override
			public synchronized @NonNull <E_E extends Throwable> DirectoryWriter<IOException> get(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException, E_E {
				if(!hasCreatedDir) {
					try {
						Files.createDirectories(path);
					} catch(IOException e) {
						throw errorType_e.toThrowable(e);
					}
					hasCreatedDir = true;
				}

				return new IOExceptionDirectoryWriter();
			}

			@Override
			public <E_E extends Throwable> void close(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException {}
		};
	}

	private class IOExceptionDirectoryWriter implements DirectoryWriter<IOException> {
		@Override
		public @NonNull <E_E extends Throwable> ScopedResource<IOException, BinaryWriter<IOException>> write(ErrorType<IOException, ? extends E_E> errorType_e, @NonNull List<String> dirs, @NonNull String fileName) throws InterruptedException {
			return new ScopedResource<IOException, BinaryWriter<IOException>>() {
				private OutputStream os;
				
				@Override
				public synchronized @NonNull <E_E extends Throwable> BinaryWriter<IOException> get(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException, E_E {
					try {
						if(os == null) {
							var parentDir = path;
							for(var dir : dirs) {
								parentDir = parentDir.resolve(dir);
							}
							var filePath = parentDir.resolve(fileName);

							if(!filePath.startsWith(path)) {
								throw new IOException("File path is outside of the directory");
							}
							
							os = Files.newOutputStream(filePath);
						}
						
						return new OutputStreamBinaryWriter(os);
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
			};
		}
	}
}
