package dev.argon.testrunner;

import java.util.List;

public class GroupedTestCase {
    
    public GroupedTestCase(List<String> group, String baseName, TestCase testCase) {
        this.group = group;
	    this.baseName = baseName;
	    this.testCase = testCase;
    }
    
    private final List<String> group;
	private final String baseName;
	private final TestCase testCase;

    public List<String> getGroup() {
        return group;
    }

	public String getBaseName() {
		return baseName;
	}

    public TestCase getTestCase() {
        return testCase;
    }
	
	
	public ExpectedTestResult getExpectedResult() {
		var expectedError = testCase.getExpectedError();
		if(expectedError != null) {
			return new ExpectedTestResult.CompileError(expectedError);
		}
		
		var expectedOutput = testCase.getExpectedOutput();
		if(expectedOutput == null) {
			throw new IllegalStateException("Expected output or error must be specified");
		}
		
		return new ExpectedTestResult.Executed(expectedOutput);
	}

}
