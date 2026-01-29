package dev.argon.testrunner.tests;

import com.fasterxml.jackson.dataformat.xml.XmlMapper;
import dev.argon.testrunner.*;
import org.apache.commons.io.file.PathUtils;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.StreamSupport;

public abstract class CompilerTestSuiteBase implements CompilerTestSuite {

	private RunnerContext context;
	private DriverCommandExecutor commandExecutor;
	private OutputProgramRunner runner;
	private TestCaseRunner testCaseRunner;

	private RunnerContext getContext() {
		return context;
	}

	private synchronized TestCaseRunner getTestCaseRunner() throws Exception {
		if(context == null) {
			context = createContext();
		}
		
		if(commandExecutor == null) {
			commandExecutor = createCommandExecutor(getContext());
		}
		
		if(runner == null) {
			runner = createProgramRunner(getContext());
		}
		
		if(testCaseRunner == null) {
			testCaseRunner = new TestCaseRunner(context, commandExecutor, runner);
		}
		return testCaseRunner;
	}
	
	@Override
	public TestResult runTestCase(GroupedTestCase testCase) throws Throwable {
		return getTestCaseRunner().executeTestCase(testCase);
	}
		

	@Override
	public synchronized void close() throws Exception {
		if(commandExecutor != null) {
			commandExecutor.close();
		}

		if (testCaseRunner != null) {
			testCaseRunner.close();
		}
	}

	private static final Path TEST_CASE_DIR = Path.of("testcases");

	@Override
	public List<GroupedTestCase> loadTestCases() {
		try {
			var testCases = new ArrayList<GroupedTestCase>();
			loadTestCases(TEST_CASE_DIR, TEST_CASE_DIR, testCases);
			return testCases;
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}

	private void loadTestCases(Path baseDir, Path path, List<GroupedTestCase> testCases) throws IOException {
		XmlMapper mapper = new XmlMapper();
		if (Files.isDirectory(path)) {
			try (var dirStream = Files.list(path)) {
				var it = dirStream.iterator();
				while (it.hasNext()) {
					var p = it.next();

					if (Files.isDirectory(p) || p.getFileName().toString().endsWith(".xml")) {
						loadTestCases(baseDir, p, testCases);
					}
				}
			}

		} else {
			var relPath = baseDir.relativize(path);
			var relDir = relPath.getParent();

			List<String> group;
			if (relDir == null) {
				group = List.of();
			} else {
				group = StreamSupport.stream(relDir.spliterator(), false)
					.map(Path::toString)
					.toList();
			}

			var testCase = mapper.readValue(path.toFile(), TestCase.class);
			var groupedTestCase = new GroupedTestCase(group, PathUtils.getBaseName(path), testCase);
			if (useTestCase(groupedTestCase)) {
				testCases.add(groupedTestCase);
			}
		}
	}

	private RunnerContext createContext() {
		var hostPlatform = hostPlatform();
		return new RunnerContext(
			hostPlatform,
			backend(),
			Path.of("libraries"),
			Path.of("backend"),
			Path.of("dist").resolve("argon-" + hostPlatform.platformId())
		);
	}

	
	public String getSuiteName() {
		return getClass().getSimpleName();
	}
	
	// Allow test suites to select a subset of test cases
	protected boolean useTestCase(GroupedTestCase testCase) {
		return true;
	}


}
