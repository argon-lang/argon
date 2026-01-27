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

public abstract class CompilerTestBase {

	private static final Path TEST_CASE_DIR = Path.of("testcases");

	private List<GroupedTestCase> loadTestCases() {
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

	// Allow test suites to select a subset of test cases
	protected boolean useTestCase(GroupedTestCase testCase) {
		return true;
	}

	protected abstract RunnerContext createContext();
	protected abstract OutputProgramRunner createProgramRunner();
	protected abstract DriverCommandExecutor createCommandExecutor();
	
	
	

}
