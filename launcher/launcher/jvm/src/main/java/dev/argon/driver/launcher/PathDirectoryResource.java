package dev.argon.driver.launcher;

import dev.argon.backend.api.DirectoryEntry;
import dev.argon.backend.api.DirectoryResource;
import dev.argon.backend.api.ScopedResource;
import dev.argon.backend.api.Stream;
import dev.argon.nobleidl.runtime.ErrorType;
import org.jspecify.annotations.NonNull;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.StreamSupport;

class PathDirectoryResource implements DirectoryResource<IOException> {
	public PathDirectoryResource(Path path) {
		this.path = path;
	}

	private final Path path;

	@Override
	public @NonNull <E_E extends Throwable> ScopedResource<IOException, Stream<IOException, DirectoryEntry<IOException>>> contents(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException {
		return new ScopedResource<IOException, Stream<IOException, DirectoryEntry<IOException>>>() {

			private java.util.stream.Stream<Path> stream;
			private PathStream pathStream;

			@Override
			public synchronized @NonNull <E_E extends Throwable> Stream<IOException, DirectoryEntry<IOException>> get(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException, E_E {
				try {
					if(stream == null) {
						stream = Files.walk(path);
					}

					if(pathStream == null) {
						pathStream = new PathStream(path, stream.iterator());
					}

					return pathStream;
				}
				catch(IOException ex) {
					throw errorType_e.toThrowable(ex);
				}
			}

			@Override
			public synchronized <E_E extends Throwable> void close(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException {
				if(stream != null) {
					stream.close();
				}
			}
		};
	}

	private static class PathStream implements Stream<IOException, DirectoryEntry<IOException>> {
		public PathStream(Path baseDir, java.util.Iterator<Path> iterator) {
			this.baseDir = baseDir;
			this.iterator = iterator;
		}
		
		private final Path baseDir;
		private final java.util.Iterator<Path> iterator;

		@Override
		public @NonNull <E_E extends Throwable> List<DirectoryEntry<IOException>> next(ErrorType<IOException, ? extends E_E> errorType_e) throws InterruptedException, E_E {
			try {
				while(iterator.hasNext()) {
					var path = iterator.next();					
					if(Files.isDirectory(path)) {
						continue;
					}
					
					List<String> dirs;
					var parentDir = path.getParent();
					if(parentDir == null) {
						dirs = List.of();
					}
					else {
						var relParentDir = baseDir.relativize(parentDir);
						if(relParentDir.equals(Path.of(""))) {
							dirs = List.of();
						}
						else {
							dirs = StreamSupport.stream(relParentDir.spliterator(), false)
								.map(Path::toString)
								.toList();
						}
					}
					
					var resource = new PathBinaryResource(path);
					
					var entry = new DirectoryEntry<>(dirs, path.getFileName().toString(), resource);
					return List.of(entry);
				}
				
				return List.of();
			}
			catch(UncheckedIOException ex) {
				if(ex.getCause() instanceof IOException ioe) {
					throw errorType_e.toThrowable(ioe);
				}
				else {
					throw ex;
				}
			}
		}
	}
}
