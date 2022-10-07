package dev.argon.argonvm.engine;

import dev.argon.argonvm.Program;
import dev.argon.argonvm.format.Argonvm;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

public class Main {
	public static void main(String[] args) throws Throwable {
		Argonvm.Program programProto;
		try(InputStream is = Files.newInputStream(Path.of(args[0]))) {
			programProto = Argonvm.Program.parseFrom(is);
		}

		var program = Program.load(programProto, new StandardLibrary());
		var vm = new VM(program.program());
		vm.execute(program.entrypoint());
	}
}
