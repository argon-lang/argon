plugins {
    application
    alias(libs.plugins.errorprone)
}

description = "Argon compiler backend for the JVM"

dependencies {
    compileOnly(libs.jspecify)
	implementation(project(":api"))
	implementation(libs.picocli)

	testImplementation(project(":runtime"))
	testImplementation(libs.junit.jupiter)
	testRuntimeOnly(libs.junit.platform.launcher)
}

application {
	mainClass.set("dev.argon.backend.Main")
}

tasks.test {
	useJUnitPlatform()
}
