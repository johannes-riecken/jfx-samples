module com.javafx.experiments.importers.max {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.graphics;
    requires javafx.media;
    requires javafx.web;
    requires javafx.swing;
    requires com.javafx.experiments.importers;

    opens com.javafx.experiments.loaders.max to org.eclipse.yasson, java.base;

}