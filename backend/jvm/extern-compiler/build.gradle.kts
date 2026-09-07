plugins {
    application
}

description = "Compiler for Argon JVM extern sources"

dependencies {
    testImplementation(libs.junit.jupiter)
    testRuntimeOnly(libs.junit.platform.launcher)
}

application {
    mainModule.set("dev.argon.externcompiler")
    mainClass.set("dev.argon.externcompiler.Main")
}

tasks.test {
    useJUnitPlatform()
}
