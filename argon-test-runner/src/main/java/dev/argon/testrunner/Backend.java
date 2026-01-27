package dev.argon.testrunner;

public enum Backend {
	JS("js"),
	;
	
	Backend(String id) {
		this.id = id;
	}
	
	private final String id;
	
	public String backendId() {
		return id;
	}
}
