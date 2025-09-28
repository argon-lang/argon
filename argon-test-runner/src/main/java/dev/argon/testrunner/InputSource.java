package dev.argon.testrunner;

import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlProperty;
import com.fasterxml.jackson.dataformat.xml.annotation.JacksonXmlText;

public class InputSource {

    @JacksonXmlProperty(isAttribute = true, localName = "name")
    private String name;

    // The text content between <InputSource> ... </InputSource>
    @JacksonXmlText
    private String value;

    // getters and setters
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }
    
    @Override
    public String toString() {
        return "InputSource{" +
                "name='" + name + '\'' +
                ", value='" + value + '\'' +
                '}';
    }
}
