import dev.argon.nobleidl.gradleplugin.NobleIDLCodeGenTask

plugins {
    `java-library`
    id("dev.argon.nobleidl") version "0.1.0-SNAPSHOT"
    alias(libs.plugins.errorprone)
}

description = "Argon JVM backend API"

dependencies {
    compileOnly(libs.jspecify)
    api(libs.esexpr.runtime)
    api(libs.nobleidl.runtime)
    implementation(libs.jawawasm.runtime)
    annotationProcessor(libs.esexpr.generator)
}

tasks.withType<NobleIDLCodeGenTask>().configureEach {
    generateGraalJSAdapters.set(false)
    inputFiles.setFrom(
        rootProject.file("../nobleidl/api/metadata.nidl"),
        rootProject.file("../nobleidl/jvm/jvm-platform-metadata.nidl"),
        rootProject.file("../nobleidl/vm/vm.nidl"),
    )
}
