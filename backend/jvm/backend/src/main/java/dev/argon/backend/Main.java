package dev.argon.backend;

import dev.argon.backend.codegen.Codegen;
import dev.argon.backend.externs.ExternMetadataScanner;
import dev.argon.backend.io.InputFile;
import dev.argon.backend.io.OutputFile;
import dev.argon.esexpr.ESExprBinaryWriter;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Model.CommandSpec;
import picocli.CommandLine.Mixin;
import picocli.CommandLine.Option;
import picocli.CommandLine.ParameterException;
import picocli.CommandLine.Spec;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.stream.Stream;

public final class Main {
	private Main() {
	}

	static void main(String[] args) {
		int exitCode = run(args);

		if(exitCode != 0) {
			System.exit(exitCode);
		}
	}

	private static int run(String[] args) {
		return new CommandLine(new RootCommand())
			.setCommandName("argon-jvm-backend")
			.setParameterExceptionHandler((ParameterException ex, String[] ignored) -> {
				System.err.println("argon-jvm-backend: " + ex.getMessage());
				var commandLine = ex.getCommandLine();
				commandLine.usage(System.err);
				return 1;
			})
			.setExecutionExceptionHandler((ex, commandLine, parseResult) -> {
				System.err.println("argon-jvm-backend: " + ex.getMessage());
				ex.printStackTrace(System.err);
				return commandLine.getCommandSpec().exitCodeOnExecutionException();
			})
			.execute(args);
	}

	private static int runPlatformMetadata(JvmPlatformMetadataCommand command) throws IOException {
		var metadata = ExternMetadataScanner.platformMetadata(expandInputFiles(command.externFiles));

		try(var output = OutputFile.fromPath(command.outputFile).open()) {
			new ESExprBinaryWriter(output).write(dev.argon.backend.api.PlatformMetadataResult.codec().encode(metadata));
		}

		return 0;
	}

	private static List<InputFile> expandInputFiles(List<Path> paths) throws IOException {
		var files = new ArrayList<InputFile>();

		for(var path : paths) {
			for(var classfilePath : expandClassfilePath(path)) {
				files.add(InputFile.fromPath(classfilePath));
			}
		}

		return List.copyOf(files);
	}

	private static List<Path> expandClassfilePath(Path path) throws IOException {
		if(Files.isDirectory(path)) {
			try(Stream<Path> stream = Files.walk(path)) {
				return stream
					.filter(Files::isRegularFile)
					.filter(Main::isClassfilePath)
					.sorted(Comparator.comparing(Path::toString))
					.toList();
			}
		}

		if(!Files.isRegularFile(path)) {
			throw new IllegalArgumentException("Extern classfile path does not exist: " + path);
		}

		if(!isClassfilePath(path)) {
			throw new IllegalArgumentException("Extern path is not a .class file: " + path);
		}

		return List.of(path);
	}

	private static boolean isClassfilePath(Path path) {
		var fileName = path.getFileName();
		return fileName != null && fileName.toString().endsWith(".class");
	}

	private static int runCodegen(JvmCodegenCommand command) throws Exception {
		Codegen.codegen(new Codegen.JVMCodegenOptions(
			InputFile.fromPath(command.input),
			OutputFile.fromPath(command.output)
		));

		return 0;
	}

	@Command(
		subcommands = {
			PlatformMetadataCommand.class,
			CodegenCommand.class,
		}
	)
	private static final class RootCommand implements Callable<Integer> {
		@Mixin
		private HelpOptions helpOptions;

		@Spec
		private CommandSpec spec;

		@Override
		public Integer call() {
			spec.commandLine().usage(System.err);
			return 1;
		}
	}

	@Command(
		name = "platform-metadata",
		description = "Load platform-specific metadata",
		subcommands = {
			JvmPlatformMetadataCommand.class,
		}
	)
	private static final class PlatformMetadataCommand implements Callable<Integer> {
		@Mixin
		private HelpOptions helpOptions;

		@Spec
		private CommandSpec spec;

		@Override
		public Integer call() {
			spec.commandLine().usage(System.err);
			return 1;
		}
	}

	@Command(
		name = "jvm",
		description = "Load platform metadata for JVM"
	)
	private static final class JvmPlatformMetadataCommand implements Callable<Integer> {
		@Mixin
		private HelpOptions helpOptions;

		@Option(names = "--extern", description = "JVM extern file")
		private List<Path> externFiles = new ArrayList<>();

		@Option(names = { "-o", "--output-file" }, required = true, description = "Output platform metadata file")
		private Path outputFile;

		@Override
		public Integer call() throws IOException {
			return runPlatformMetadata(this);
		}
	}

	@Command(
		name = "codegen",
		description = "Generate code from Argon VM IR",
		subcommands = {
			JvmCodegenCommand.class,
		}
	)
	private static final class CodegenCommand implements Callable<Integer> {
		@Mixin
		private HelpOptions helpOptions;

		@Spec
		private CommandSpec spec;

		@Override
		public Integer call() {
			spec.commandLine().usage(System.err);
			return 1;
		}
	}

	@Command(
		name = "jvm",
		description = "Generate JVM code from Argon VM IR"
	)
	private static final class JvmCodegenCommand implements Callable<Integer> {
		@Mixin
		private HelpOptions helpOptions;

		@Option(names = { "-i", "--input" }, required = true, description = "Input Argon VM IR file")
		private Path input;

		@Option(names = { "-o", "--output" }, required = true, description = "Output JAR file")
		private Path output;

		@Override
		public Integer call() throws Exception {
			return runCodegen(this);
		}
	}

	private static final class HelpOptions {
		@Option(names = { "-h", "--help" }, usageHelp = true, description = "Show this help message")
		private boolean help;
	}
}
