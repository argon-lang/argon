import net.ltgt.gradle.errorprone.errorprone

plugins {
    alias(libs.plugins.errorprone) apply false
}

allprojects {
    group = "dev.argon"
    version = "0.1.0"

    repositories {
        mavenLocal()
        mavenCentral()
    }
}

subprojects {
    plugins.withType<JavaPlugin> {
        extensions.configure<JavaPluginExtension> {
            toolchain {
                languageVersion = JavaLanguageVersion.of(25)
            }
        }
    }

    plugins.withId("net.ltgt.errorprone") {
        dependencies {
            "errorprone"(libs.nullaway)
            "errorprone"(libs.errorprone)
        }

        tasks.withType<JavaCompile>().configureEach {
            options.errorprone {
                disable("RefactorSwitch")
                excludedPaths.set(".*/build/(?:generated|nobleidl/gen)/.*")
                option("NullAway:OnlyNullMarked", "true")
                option("NullAway:JSpecifyExperimental", "true")
                option("NullAway:JSpecifyMode", "true")
                error("NullAway")
                error("RequireExplicitNullMarking")
            }
        }
    }
}
