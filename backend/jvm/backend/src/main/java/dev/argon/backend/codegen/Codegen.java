package dev.argon.backend.codegen;

import dev.argon.backend.io.InputFile;
import dev.argon.backend.io.OutputFile;
import dev.argon.backend.ir.IrReader;
import dev.argon.esexpr.DecodeException;
import dev.argon.esexpr.SyntaxException;

import java.io.IOException;
import java.util.zip.ZipOutputStream;

public class Codegen {
	private Codegen() {}

	public record JVMCodegenOptions(
		InputFile inputFile,
		OutputFile outputFile,
		boolean executable
	) {}

	public static void codegen(JVMCodegenOptions options) throws SyntaxException, IOException, DecodeException {
		var program = IrReader.readFile(options.inputFile);

		boolean completed = false;
		try {
			try(var zos = new ZipOutputStream(options.outputFile.open())) {
				var emitter = new Emitter(program, zos);
				if(options.executable) {
					emitter.markEmitExecutable();
				}
				emitter.emit();
			}

			completed = true;
		}
		finally {
			if(!completed) {
				options.outputFile.delete();
			}
		}


	}

}
