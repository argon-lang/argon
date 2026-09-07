package dev.argon.backend.externs;

import dev.argon.backend.api.PlatformMetadataResult;
import dev.argon.backend.io.InputFile;
import dev.argon.backend.io.OutputFile;
import dev.argon.esexpr.ESExprBinaryWriter;

import java.io.IOException;
import java.util.List;

public final class PlatformMetadata {
	private PlatformMetadata() {
	}

	public record JVMPlatformMetadataOptions(List<InputFile> externFiles, OutputFile outputFile) {
	}

	public static void platformMetadata(JVMPlatformMetadataOptions options) throws IOException {
		var metadata = ExternMetadataScanner.platformMetadata(options.externFiles());

		boolean completed = false;
		try {
			try(var output = options.outputFile().open()) {
				new ESExprBinaryWriter(output).write(PlatformMetadataResult.codec().encode(metadata));
			}

			completed = true;
		}
		finally {
			if(!completed) {
				options.outputFile().delete();
			}
		}
	}
}
