plugins {
    application
}

description = "Argon compiler backend for the JVM"

dependencies {
	implementation(project(":api"))
	implementation("info.picocli:picocli:4.7.7")

	testImplementation(project(":runtime"))
	testImplementation("org.junit.jupiter:junit-jupiter:5.10.3")
	testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

application {
	mainClass.set("dev.argon.backend.Main")
}

tasks.test {
	useJUnitPlatform()
}
