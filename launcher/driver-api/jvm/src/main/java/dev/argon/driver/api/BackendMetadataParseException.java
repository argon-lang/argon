package dev.argon.driver.api;

public class BackendMetadataParseException extends Exception {
	public BackendMetadataParseException(String message) {
		super(message);
	}
	
	public BackendMetadataParseException(String message, Throwable cause) {
		super(message, cause);
	}
	
	public BackendMetadataParseException(Throwable cause) {
		super(cause);
	}
	
	public BackendMetadataParseException() {}
}
