package dev.argon.testrunner;

public enum HostPlatform {
	JVM("jvm"),
	JS("js"),
	;
	
	HostPlatform(String id) {
		this.id = id;
	}
	
	private final String id;
	
	public String platformId() {
		return id;
	}
}
