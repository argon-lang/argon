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
}
