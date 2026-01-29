package dev.argon.testrunner;

import dev.argon.driver.api.command.CompilerDriverOptionValue;
import dev.argon.driver.api.command.CompilerDriverOptionValueAtom;
import dev.argon.driver.api.command.CompilerDriverOutput;
import dev.argon.driver.api.command.DriverCommand;
import org.apache.commons.io.IOUtils;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

public final class DriverCommandExecutorCLI implements DriverCommandExecutor {
	public DriverCommandExecutorCLI(RunnerContext context) {
		this.context = context;
	}
	
	private final RunnerContext context;
	
	
	@Override
	public CommandExecutionResult execute(
		DriverCommand<String, String, String, String> command
	) throws Exception {
		var pb = new ProcessBuilder();
		
		var executable = context.distDir().resolve("argon");

		List<String> cmd = new ArrayList<>();
		cmd.add(executable.toString());
		buildArguments(command, cmd);

		pb.command(cmd);

		pb.redirectInput(ProcessBuilder.Redirect.PIPE);
		pb.redirectOutput(ProcessBuilder.Redirect.PIPE);
		pb.redirectErrorStream(true);
		
		var process = pb.start();

		process.getOutputStream().close();
		String output = IOUtils.toString(process.getInputStream(), StandardCharsets.UTF_8);
		var exitCode = process.waitFor();

		return new CommandExecutionResult(exitCode, output);
	}

	private static void buildArguments(DriverCommand<String, String, String, String> command, List<String> args) {
		switch(command) {
			case DriverCommand.HelpCommand(var _, var _) -> {
				args.add("--help");
			}
			
			case DriverCommand.VersionCommand() -> {
				args.add("--version");
			}
			
			case DriverCommand.ListBackendsCommand() -> {
				args.add("backends");
			}

			case DriverCommand.CompileCommand<String, String, String, String> compileCommand -> {
				args.add("compile");
				
				args.add("--name");
				args.add(LibraryUtils.getLibraryName(compileCommand.tubeName()));
				
				args.add("-i");
				args.add(compileCommand.inputDir());
				
				args.add("-o");
				args.add(compileCommand.outputFile());
				
				for(var refTube : compileCommand.referencedTubes()) {
					args.add("-r");
					args.add(refTube);
				}
				
				for(var platform : compileCommand.supportedPlatforms()) {
					args.add("--platform");
					args.add(platform);
				}
				
				for(var platformOptsEntry : compileCommand.platformOptions().map().entrySet()) {
					var platform = platformOptsEntry.getKey();
					
					for(var platformOptEntry : platformOptsEntry.getValue().map().entrySet()) {
						var library = platformOptEntry.getKey();
						
						String optionSwitch = "--" + platform + "-" + library;
						encodeOption(platformOptEntry.getValue(), optionSwitch, args);
					}
				}
			}
			
			case DriverCommand.GenIrCommand<String, String, String, String> genIrCommand -> {
				args.add("genir");

				args.add("-i");
				args.add(genIrCommand.inputFile());

				args.add("-o");
				args.add(genIrCommand.outputFile());

				for(var refTube : genIrCommand.referencedTubes()) {
					args.add("-r");
					args.add(refTube);
				}

				args.add("--platform");
				args.add(genIrCommand.platform());
			} 

			case DriverCommand.CodegenCommand<String, String, String, String> codegenCommand -> {
				args.add("codegen");
				
				args.add(codegenCommand.backend());

				args.add("-i");
				args.add(codegenCommand.inputFile());

				for(var refTube : codegenCommand.referencedTubes()) {
					args.add("-r");
					args.add(refTube);
				}
				
				for(var codegenOpt : codegenCommand.platformOptions().map().entrySet()) {
					String optionSwitch = "--" + codegenCommand.backend() + "-" + codegenOpt.getKey();
					encodeOption(codegenOpt.getValue(), optionSwitch, args);
				}
				
				for(var outputOpt : codegenCommand.platformOutputOptions().map().entrySet()) {
					String optionSwitch = "--" + codegenCommand.backend() + "-" + outputOpt.getKey();
					args.add(optionSwitch);
					args.add(getOutputValue(outputOpt.getValue()));
				}
			}
		}
	}

	private static void encodeOption(CompilerDriverOptionValue<String, String> option, String optionSwitch, List<String> args) {
		switch(option) {
			case CompilerDriverOptionValue.Single(var optValue) -> {
				encodeOptionValueAtom(optValue, optionSwitch, args);
			}
			case CompilerDriverOptionValue.Many(var head, var tail) -> {
				encodeOptionValueAtom(head, optionSwitch, args);
				
				for(var optValue : tail) {
					encodeOptionValueAtom(optValue, optionSwitch, args);
				}
			}
		}
	}
	
	private static void encodeOptionValueAtom(CompilerDriverOptionValueAtom<String, String> value, String optionSwitch, List<String> args) {
		switch(value) {
			case CompilerDriverOptionValueAtom.String(var s) -> {
				args.add(optionSwitch);
				args.add(s);
			}
			case CompilerDriverOptionValueAtom.Bool(var b) -> {
				if(b) {
					args.add(optionSwitch);
				}
			}
			case CompilerDriverOptionValueAtom.File(var f) -> {
				args.add(optionSwitch);
				args.add(f);
			}
			case CompilerDriverOptionValueAtom.Directory(var dir) -> {
				args.add(optionSwitch);
				args.add(dir);
			}
		}
	}
	
	private static String getOutputValue(CompilerDriverOutput<String, String> output) {
		return switch(output) {
			case CompilerDriverOutput.File(var f) -> f;
			case CompilerDriverOutput.Directory(var dir) -> dir;
		};
	}
}
