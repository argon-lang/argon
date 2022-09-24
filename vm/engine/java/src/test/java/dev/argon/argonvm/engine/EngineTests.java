package dev.argon.argonvm.engine;

import dev.argon.argonvm.Program;
import dev.argon.argonvm.parser.AsmParser;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class EngineTests {
	@TestFactory
	Collection<DynamicTest> assemblyTests() throws Throwable {
		List<DynamicTest> tests = new ArrayList<>();

		URI uri = EngineTests.class.getResource("/dev/argon/argonvm/engine/testcases").toURI();
		Path path = Path.of(uri);
		for(var iterator = Files.walk(path).iterator(); iterator.hasNext(); ) {
			var testCasePath = iterator.next();
			if(!testCasePath.getFileName().toString().endsWith(".arvm")) {
				continue;
			}

			tests.add(createTest(testCasePath));
		}

		return tests;
	}

	private DynamicTest createTest(Path path) throws Throwable {
		String text = Files.readString(path, StandardCharsets.UTF_8);
		String expectedOutput = Files.readString(path.resolveSibling(path.getFileName() + ".output.txt"), StandardCharsets.UTF_8);

		return DynamicTest.dynamicTest(path.getFileName().toString(), () -> {
			var lib = new TestStandardLibrary();
			var serProgram = AsmParser.parse(text, lib);
			var program = Program.load(serProgram, lib);
			var vm = new VM(program.program());
			vm.execute(program.entrypoint());

			Assertions.assertEquals(expectedOutput, lib.output.toString());
		});
	}
}
