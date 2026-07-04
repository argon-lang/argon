import dev.argon.nobleidl.gradleplugin.NobleIDLCodeGenTask

plugins {
    `java-library`
    id("dev.argon.nobleidl") version "0.1.0-SNAPSHOT"
}

description = "Argon JVM backend API"

dependencies {
    api("dev.argon.esexpr:esexpr-java-runtime:0.5.1-SNAPSHOT")
    api("dev.argon.nobleidl:nobleidl-java-runtime:0.2.0-SNAPSHOT")
    implementation("dev.argon.jawawasm:jawawasm-runtime:0.2.0-SNAPSHOT")
    annotationProcessor("dev.argon.esexpr:esexpr-generator:0.5.1-SNAPSHOT")
}

tasks.withType<NobleIDLCodeGenTask>().configureEach {
    generateGraalJSAdapters.set(false)
    inputFiles.setFrom(
        rootProject.file("../nobleidl/api/metadata.nidl"),
        rootProject.file("../nobleidl/jvm/jvm-platform-metadata.nidl"),
        rootProject.file("../nobleidl/vm/vm.nidl"),
    )
}
