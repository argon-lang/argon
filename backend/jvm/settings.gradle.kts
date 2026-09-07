pluginManagement {
    repositories {
        mavenLocal()
        mavenCentral()
        gradlePluginPortal()
    }
}

rootProject.name = "argon-jvm-backend"

include(
    "runtime",
    "api",
    "backend",
    "extern-compiler",
)
