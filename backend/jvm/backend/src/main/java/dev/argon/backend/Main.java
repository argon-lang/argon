package dev.argon.backend;

import dev.argon.backend.codegen.Codegen;
import dev.argon.backend.externs.PlatformMetadata;
import dev.argon.backend.io.InputFile;
import dev.argon.backend.io.OutputFile;
import dev.argon.esexpr.ESExprBinaryWriter;
import dev.argon.tasks.TaskMessage;
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
		return new CommandLine(new RootCommandOptions()).setCommandName("argon-jvm-backend")
			.setCaseInsensitiveEnumValuesAllowed(true)
			.setParameterExceptionHandler((ParameterException ex, String[] ignored) -> {
				System.err.println("argon-jvm-backend: " + ex.getMessage());
				var commandLine = ex.getCommandLine();
				commandLine.usage(System.err);
				return 1;
			}).setExecutionExceptionHandler((ex, commandLine, parseResult) -> {
				if(parseResult.matchedOptionValue("--output-format", OutputFormat.TEXT) == OutputFormat.ESEXPR) {
					writeTaskError("argon-jvm-backend: " + ex.getMessage());
				}
				else {
					System.err.println("argon-jvm-backend: " + ex.getMessage());
					ex.printStackTrace(System.err);
				}
				return commandLine.getCommandSpec().exitCodeOnExecutionException();
			}).execute(args);
	}

	private static void writeTaskError(String message) {
		try {
			new ESExprBinaryWriter(System.out)
				.write(TaskMessage.codec().encode(new TaskMessage.TaskError(message)));
			System.out.flush();
		}
		catch(IOException ex) {
			System.err.println("argon-jvm-backend: failed to write task error: " + ex.getMessage());
		}
	}

	public enum OutputFormat {
		TEXT,
		ESEXPR,
	}

	private static int runPlatformMetadata(JvmPlatformMetadataCommand command) throws IOException {
		PlatformMetadata.platformMetadata(new PlatformMetadata.JVMPlatformMetadataOptions(
			expandInputFiles(command.externFiles), OutputFile.fromPath(command.outputFile)));

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
				return stream.filter(Files::isRegularFile).filter(Main::isClassfilePath)
					.sorted(Comparator.comparing(Path::toString)).toList();
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
		Codegen.codegen(new Codegen.JVMCodegenOptions(InputFile.fromPath(command.input),
			OutputFile.fromPath(command.output), command.executable));

		return 0;
	}

	@Command(subcommands = {PlatformMetadataCommandOptions.class, CodegenCommandOptions.class,})
	@SuppressWarnings("NullAway")
	public static final class RootCommandOptions implements Callable<Integer> {
		@Option(names = "--output-format", defaultValue = "text", scope = CommandLine.ScopeType.INHERIT,
			description = "Task message output format: ${COMPLETION-CANDIDATES}")
		public OutputFormat outputFormat;

		@Mixin
		public HelpOptions helpOptions;

		@Spec
		public CommandSpec spec;

		@Override
		public Integer call() {
			spec.commandLine().usage(System.err);
			return 1;
		}
	}

	@Command(name = "platform-metadata", description = "Load platform-specific metadata", subcommands = {
			JvmPlatformMetadataCommand.class,})
	@SuppressWarnings("NullAway")
	public static final class PlatformMetadataCommandOptions implements Callable<Integer> {
		@Mixin
		public HelpOptions helpOptions;

		@Spec
		public CommandSpec spec;

		@Override
		public Integer call() {
			spec.commandLine().usage(System.err);
			return 1;
		}
	}

	@Command(name = "jvm", description = "Load platform metadata for JVM")
	@SuppressWarnings("NullAway")
	public static final class JvmPlatformMetadataCommand implements Callable<Integer> {
		@Mixin
		public HelpOptions helpOptions;

		@Option(names = "--extern", description = "JVM extern file")
		public List<Path> externFiles = new ArrayList<>();

		@Option(names = {"-o", "--output-file"}, required = true, description = "Output platform metadata file")
		public Path outputFile;

		@Override
		public Integer call() throws IOException {
			return runPlatformMetadata(this);
		}
	}

	@Command(name = "codegen", description = "Generate code from Argon VM IR", subcommands = {JvmCodegenCommand.class,})
	@SuppressWarnings("NullAway")
	public static final class CodegenCommandOptions implements Callable<Integer> {
		@Mixin
		public HelpOptions helpOptions;

		@Spec
		public CommandSpec spec;

		@Override
		public Integer call() {
			spec.commandLine().usage(System.err);
			return 1;
		}
	}

	@Command(name = "jvm", description = "Generate JVM code from Argon VM IR")
	@SuppressWarnings("NullAway")
	public static final class JvmCodegenCommand implements Callable<Integer> {
		@Mixin
		public HelpOptions helpOptions;

		@Option(names = {"-i", "--input"}, required = true, description = "Input Argon VM IR file")
		public Path input;

		@Option(names = {"-o", "--output"}, required = true, description = "Output JAR file")
		public Path output;

		@Option(names = "--executable", description = "Generate an executable Main class")
		public boolean executable;

		@Override
		public Integer call() throws Exception {
			return runCodegen(this);
		}
	}

	public static final class HelpOptions {
		@Option(names = {"-h", "--help"}, usageHelp = true, description = "Show this help message")
		public boolean help;
	}
}
