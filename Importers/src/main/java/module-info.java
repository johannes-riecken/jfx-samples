module com.javafx.experiments.importers {
    requires javafx.controls;
    requires javafx.fxml;
    requires javafx.graphics;
    requires javafx.media;
    requires javafx.web;
    requires javafx.swing;

    requires java.desktop;
    requires java.logging;
    requires org.apache.lucene.queryparser;
    requires org.apache.lucene.core;
    requires org.apache.lucene.queries;
    requires org.apache.lucene.sandbox;
    requires org.apache.lucene.grouping;
    requires jakarta.json.bind;
    uses com.javafx.experiments.importers.Importer;
    provides com.javafx.experiments.importers.Importer with com.javafx.experiments.importers.obj.ObjOrPolyObjImporter;


    exports com.javafx.experiments.importers.maya to javafx.fxml, com.javafx.experiments.importers.max;
//    opens com.javafx.experiments.shape3d to javafx.fxml;
    opens com.javafx.experiments.shape3d to javafx.fxml;
    exports com.javafx.experiments.importers;
//    opens com.javafx.experiments.jfx3dviewer to java.base, org.eclipse.yasson;
    opens com.javafx.experiments.importers to java.base, javafx.fxml, org.eclipse.yasson;
    exports com.javafx.experiments.importers.obj;
    exports com.javafx.experiments.shape3d;
    exports com.javafx.experiments.utils3d.animation;
    exports com.javafx.experiments.importers.max;
    opens com.javafx.experiments.importers.max to java.base, org.eclipse.yasson;
    exports com.javafx.experiments.importers.maya.values;
    exports com.javafx.experiments.importers.maya.parser;
    exports com.javafx.experiments.utils3d.geom;
}
