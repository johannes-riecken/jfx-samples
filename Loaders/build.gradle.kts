plugins {
    id("java")
    id("org.openjfx.javafxplugin") version("0.0.13")
}

repositories {
    mavenCentral()
}

javafx {
    version = "22-ea+11"
    modules = listOf("javafx.controls", "javafx.fxml", "javafx.graphics", "javafx.media", "javafx.swing", "javafx.web")
}

dependencies {
    implementation(project(":Importers"))
    testImplementation(platform("org.junit:junit-bom:5.10.0"))
    testImplementation("org.junit.jupiter:junit-jupiter")
    implementation(libs.org.openjfx.javafx.controls)
    implementation(libs.org.openjfx.javafx.fxml)
    implementation(libs.org.openjfx.javafx.graphics)
    implementation(libs.org.openjfx.javafx.swing)
    implementation(libs.org.openjfx.javafx.web)
}

tasks.test {
    useJUnitPlatform()
}