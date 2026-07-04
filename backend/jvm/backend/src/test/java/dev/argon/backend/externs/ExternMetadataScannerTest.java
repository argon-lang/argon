package dev.argon.backend.externs;

import dev.argon.backend.api.Extern;
import dev.argon.backend.io.InputFile;
import dev.argon.jvmbackendmetadata.JvmExtern;
import dev.argon.jvmbackendmetadata.JvmPlatformTubeMetadata;
import dev.argon.vm.ModulePath;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import javax.tools.ToolProvider;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

final class ExternMetadataScannerTest {
	@TempDir
	private Path tempDir;

	@Test
	void scansPublicStaticExternFunction() throws Exception {
		var classes = compileSources(
			source(
				"test/externs/Externs.java",
				"""
				package test.externs;

				import dev.argon.runtime.ExternFunction;

				public final class Externs {
					@ExternFunction("custom_name")
					public static String externMethod(int value) {
						return Integer.toString(value);
					}
				}
				"""
			)
		);

		var result = ExternMetadataScanner.platformMetadata(inputFiles(classes));

		assertEquals("jvm", result.platform());
		var extern = assertInstanceOf(Extern.ExternFunction.class, result.externs().map().get("custom_name"));
		assertEquals("custom_name", extern.name());

		var implementation = assertInstanceOf(
			JvmExtern.JvmFunction.class,
			JvmExtern.codec().decode(extern.implementation())
		);
		assertEquals("test/externs/Externs", implementation._class());
		assertEquals("(I)Ljava/lang/String;", implementation.descriptor());

		var tubeMetadata = JvmPlatformTubeMetadata.codec().decode(result.tubeMetadata());
		assertTrue(tubeMetadata.additionalClasses().isPresent());
		assertEquals(1, tubeMetadata.additionalClasses().orElseThrow().size());
	}

	@Test
	void usesMethodNameWhenExternFunctionValueIsEmpty() throws Exception {
		var classes = compileSources(
			source(
				"test/externs/Externs.java",
				"""
				package test.externs;

				import dev.argon.runtime.ExternFunction;

				public final class Externs {
					@ExternFunction
					public static void defaultName() {
					}
				}
				"""
			)
		);

		var result = ExternMetadataScanner.platformMetadata(inputFiles(classes));

		assertTrue(result.externs().map().containsKey("defaultName"));
	}

	@Test
	void rejectsAnnotatedNonStaticMethod() throws Exception {
		var classes = compileSources(
			source(
				"test/externs/Externs.java",
				"""
				package test.externs;

				import dev.argon.runtime.ExternFunction;

				public final class Externs {
					@ExternFunction("bad")
					public void bad() {
					}
				}
				"""
			)
		);

		var ex = assertThrows(
			IllegalArgumentException.class,
			() -> ExternMetadataScanner.platformMetadata(inputFiles(classes))
		);
		assertTrue(ex.getMessage().contains("must be a public static method"));
	}

	@Test
	void rejectsAnnotatedNonPublicMethod() throws Exception {
		var classes = compileSources(
			source(
				"test/externs/Externs.java",
				"""
				package test.externs;

				import dev.argon.runtime.ExternFunction;

				public final class Externs {
					@ExternFunction("bad")
					static void bad() {
					}
				}
				"""
			)
		);

		var ex = assertThrows(
			IllegalArgumentException.class,
			() -> ExternMetadataScanner.platformMetadata(inputFiles(classes))
		);
		assertTrue(ex.getMessage().contains("must be a public static method"));
	}

	@Test
	void moduleInfoSetsModuleName() throws Exception {
		var classes = compileSources(
			source(
				"module-info.java",
				"""
				module scanner.fixture {
				}
				"""
			)
		);

		var result = ExternMetadataScanner.platformMetadata(inputFiles(classes));
		var tubeMetadata = JvmPlatformTubeMetadata.codec().decode(result.tubeMetadata());

		assertEquals(Optional.of("scanner.fixture"), tubeMetadata.moduleName());
	}

	@Test
	void argonModulePackageInfoAddsModulePackageMetadata() throws Exception {
		var classes = compileSources(
			source(
				"test/externs/package-info.java",
				"""
				@dev.argon.runtime.ArgonModule({"Argon", "Core", "Puts"})
				package test.externs;
				"""
			)
		);

		var result = ExternMetadataScanner.platformMetadata(inputFiles(classes));
		var tubeMetadata = JvmPlatformTubeMetadata.codec().decode(result.tubeMetadata());
		var moduleMetadata = tubeMetadata.moduleMetadata().orElseThrow();

		assertEquals(1, moduleMetadata.size());
		assertEquals(new ModulePath(List.of("Argon", "Core", "Puts")), moduleMetadata.getFirst().modulePath());
		assertEquals(Optional.of("test.externs"), moduleMetadata.getFirst().packageName());
	}

	private Path compileSources(SourceFile... sources) throws IOException {
		var sourceRoot = tempDir.resolve("src-" + System.nanoTime());
		var classes = tempDir.resolve("classes-" + System.nanoTime());
		Files.createDirectories(classes);

		var sourceFiles = new ArrayList<String>();
		for(var source : sources) {
			var path = sourceRoot.resolve(source.relativePath());
			Files.createDirectories(path.getParent());
			Files.writeString(path, source.contents(), StandardCharsets.UTF_8);
			sourceFiles.add(path.toString());
		}

		var compiler = ToolProvider.getSystemJavaCompiler();
		if(compiler == null) {
			throw new IllegalStateException("Tests must run on a JDK");
		}

		var args = new ArrayList<String>();
		args.add("-classpath");
		args.add(System.getProperty("java.class.path"));
		args.add("-d");
		args.add(classes.toString());
		args.addAll(sourceFiles);

		var exitCode = compiler.run(null, null, null, args.toArray(String[]::new));
		if(exitCode != 0) {
			throw new IllegalStateException("javac failed with exit code " + exitCode);
		}

		return classes;
	}

	private static List<InputFile> inputFiles(Path classes) throws IOException {
		try(var stream = Files.walk(classes)) {
			return stream
				.filter(Files::isRegularFile)
				.filter(path -> path.getFileName().toString().endsWith(".class"))
				.sorted()
				.map(InputFile::fromPath)
				.toList();
		}
	}

	private static SourceFile source(String relativePath, String contents) {
		return new SourceFile(relativePath, contents);
	}

	private record SourceFile(String relativePath, String contents) {
	}
}
