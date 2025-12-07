plugins {
    kotlin("jvm") version "2.0.21"
}

group = "xyz.colmmurphy"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.arrow-kt:arrow-core:2.2.0")
    implementation("io.arrow-kt:arrow-fx-coroutines:2.2.0")
    implementation("io.arrow-kt:arrow-collectors:2.2.0")

    testImplementation(kotlin("test"))
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(21)
}