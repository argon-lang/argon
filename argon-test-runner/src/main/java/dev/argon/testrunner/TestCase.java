package dev.argon.testrunner;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlElementWrapper;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlRootElement;
import dev.argon.vm.api.TubeName;

import java.util.List;

@JacksonXmlRootElement(localName = "ArgonTest")
public class TestCase {

    @JacksonXmlProperty(localName = "Name")
    private String name;

    @JacksonXmlElementWrapper(useWrapping = false)
    @JacksonXmlProperty(localName = "InputSource")
    private List<InputSource> inputSources = List.of();

    @JacksonXmlProperty(localName = "ExpectedOutput")
    private String expectedOutput;

    @JacksonXmlProperty(localName = "ExpectedError")
    private String expectedError;
    
    @JacksonXmlElementWrapper(localName = "Libraries")
    @JacksonXmlProperty(localName = "Library")
    private List<String> libraries;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<InputSource> getInputSources() {
        return inputSources;
    }

    public void setInputSources(List<InputSource> inputSources) {
        this.inputSources = inputSources;
    }

    public String getExpectedOutput() {
        return expectedOutput;
    }

    public void setExpectedOutput(String expectedOutput) {
        this.expectedOutput = expectedOutput;
    }

    public String getExpectedError() {
        return expectedError;
    }

    public void setExpectedError(String expectedError) {
        this.expectedError = expectedError;
    }
    
    public List<String> getLibraries() {
		return libraries;
    }
	
	public List<TubeName> getLibraryTubeNamesOrDefault() {
		if(libraries == null) {
			return defaultLibraries();
		}

		return libraries.stream().map(LibraryUtils::parseLibraryName).toList();
	}

    public void setLibraries(List<String> libraries) {
        this.libraries = libraries;
    }
	
	public static List<TubeName> defaultLibraries() {
		return List.of(
			LibraryUtils.LIBRARY_ARGON_CORE
		);
	}
    
    @Override
    public String toString() {
        return "TestCase{" +
                "name='" + name + '\'' +
                ", inputSources=" + inputSources +
                ", expectedOutput='" + expectedOutput + '\'' +
                ", expectedError='" + expectedError + '\'' +
                ", libraries=" + libraries +
                '}';
    }
}
