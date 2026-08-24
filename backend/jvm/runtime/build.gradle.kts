plugins {
    `java-library`
    alias(libs.plugins.errorprone)
}

description = "Argon JVM runtime"

dependencies {
    compileOnly(libs.jspecify)
}
