module Loaders {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.graphics;
    requires javafx.media;
    requires javafx.web;
    requires javafx.swing;
    requires Importers;
    requires java.logging;

    opens com.javafx.experiments.loaders.max to java.base;

}