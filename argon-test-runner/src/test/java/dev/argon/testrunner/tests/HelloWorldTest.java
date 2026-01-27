package dev.argon.testrunner.tests;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;

public class HelloWorldTest {
	@Test
	void testHelloWorld() {
		System.out.println("Hello, world!");
		System.out.println("Working dir: " + Path.of(".").toAbsolutePath().normalize());
	}
}
