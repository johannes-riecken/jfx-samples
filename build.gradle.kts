plugins {
    `java-library`
}

repositories {
    mavenLocal()
    maven {
        url = uri("https://repo.maven.apache.org/maven2/")
    }
}

dependencies {
    implementation(libs.jakarta.json.bind.jakarta.json.bind.api)
    implementation(libs.org.eclipse.yasson)
    implementation(libs.org.openjfx.javafx.controls)
    implementation(libs.org.openjfx.javafx.fxml)
    implementation(libs.org.openjfx.javafx.graphics)
    implementation(libs.org.openjfx.javafx.swing)
    implementation(libs.org.openjfx.javafx.web)
    implementation(libs.org.apache.lucene.lucene.queryparser)
    implementation(libs.org.apache.lucene.lucene.grouping)
    testImplementation(libs.org.junit.jupiter.junit.jupiter)
    testImplementation(libs.junit.junit)
}

java.sourceCompatibility = JavaVersion.VERSION_18
