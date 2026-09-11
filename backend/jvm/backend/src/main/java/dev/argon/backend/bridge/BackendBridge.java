package dev.argon.backend.bridge;

import dev.argon.backend.codegen.Codegen;
import dev.argon.backend.externs.PlatformMetadata;
import dev.argon.backend.io.InputFile;
import dev.argon.backend.io.OutputFile;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;

/** In-memory entry points used by the native task runner. */
public final class BackendBridge {
	private BackendBridge() {
	}

	public static byte[] platformMetadata(String[] fileNames, byte[][] contents) throws Exception {
		if(fileNames.length != contents.length) {
			throw new IllegalArgumentException("file name and content counts differ");
		}

		var files = new ArrayList<InputFile>(fileNames.length);
		for(int i = 0; i < fileNames.length; ++i) {
			files.add(new MemoryInputFile(fileNames[i], contents[i]));
		}

		var output = new MemoryOutputFile();
		PlatformMetadata.platformMetadata(new PlatformMetadata.JVMPlatformMetadataOptions(files, output));
		return output.bytes();
	}

	public static byte[] codegen(String fileName, byte[] contents, boolean executable) throws Exception {
		var output = new MemoryOutputFile();
		Codegen.codegen(new Codegen.JVMCodegenOptions(new MemoryInputFile(fileName, contents), output, executable));
		return output.bytes();
	}

	private static final class MemoryInputFile implements InputFile {
		private final String fileName;
		private final byte[] contents;

		MemoryInputFile(String fileName, byte[] contents) {
			this.fileName = fileName;
			this.contents = contents;
		}

		@Override
		public String fileName() {
			return fileName;
		}

		@Override
		public InputStream open() {
			return new ByteArrayInputStream(contents);
		}
	}

	private static final class MemoryOutputFile implements OutputFile {
		private ByteArrayOutputStream output = new ByteArrayOutputStream();

		@Override
		public OutputStream open() {
			output = new ByteArrayOutputStream();
			return output;
		}

		@Override
		public void delete() {
			output.reset();
		}

		byte[] bytes() {
			return output.toByteArray();
		}
	}
}
