# Package Structure

[← Back to index](README.md)

The Maya importer code is split across two Gradle modules with a clear separation between the **scene model** and the **JavaFX bridge**.

## Module Layout

```
jfx-samples/
├── Importers/                          # Core scene model module
│   └── src/main/java/com/javafx/experiments/importers/maya/
│       ├── doc/                        # This documentation
│       ├── parser/
│       │   └── MParser.java            # .ma file parser
│       ├── types/                      # MDataType implementations (21 files)
│       ├── values/                     # MData interfaces (20 files)
│       │   └── impl/                   # MData concrete classes (18 files)
│       ├── MEnv.java                   # Scene environment + all MNodeType defs
│       ├── MNode.java                  # Node instance
│       ├── MNodeType.java              # Node type schema
│       ├── MAttribute.java             # Attribute definition
│       ├── MObject.java                # Base class for all model objects
│       ├── MPath.java                  # Attribute path resolver
│       ├── MConnection.java            # Connection record
│       ├── MayaGroup.java              # JavaFX transform group
│       ├── Joint.java                  # JavaFX joint group
│       ├── Xform.java                  # Generic transform group
│       ├── Frame.java                  # Frame-based Duration helper
│       └── MayaAnimationCurveInterpolator.java
│
└── Loaders/                            # JavaFX integration module
    └── src/main/java/com/javafx/experiments/loaders/maya/
        ├── MayaImporter.java           # Importer plugin entry point
        └── Loader.java                 # MEnv → JavaFX converter
```

## Module Dependencies

### `Importers` module (`module-info.java`)

```java
module Importers {
    requires javafx.controls;
    requires javafx.graphics;
    // ...

    exports com.javafx.experiments.importers.maya to javafx.fxml, Loaders;
    exports com.javafx.experiments.importers.maya.values;
    exports com.javafx.experiments.importers.maya.parser;
    exports com.javafx.experiments.importers;
}
```

The `maya` package is exported specifically to the `Loaders` module. The `values` and `parser` sub-packages are also exported for external use.

### `Loaders` module

Depends on `Importers` and uses the exported `maya` packages. `MayaImporter` implements the `Importer` interface from `com.javafx.experiments.importers`.

## Package Responsibilities

### `com.javafx.experiments.importers.maya`

The root package contains the **scene graph model** and **JavaFX transform primitives**:

| Class | Role |
|-------|------|
| `MEnv` | Central scene database; registers all types; manages nodes and connections |
| `MNode` | Runtime node instance with attribute values and parent/child links |
| `MNodeType` | Schema defining attributes, supertypes, and default initialization |
| `MAttribute` | Attribute definition (long name, short name, data type) |
| `MObject` | Abstract base with `name` and `env` reference |
| `MPath` | Resolves dotted attribute paths with array indexing |
| `MConnection` | Immutable source/target path pair |
| `MayaGroup` | JavaFX `Group` modeling a Maya transform node |
| `Joint` | JavaFX `Group` modeling a Maya joint node |
| `Xform` | Simpler transform `Group` with configurable rotate order |
| `Frame` | `Duration` subclass using 24 FPS frame convention |
| `MayaAnimationCurveInterpolator` | Custom `Interpolator` for Maya tangent curves |

`MEnv.java` is the largest file (~2800 lines). It contains the `MEnv` class plus **all** `MNodeType` subclass definitions as package-private inner/static classes (e.g., `TransformNodeType`, `MeshNodeType`, `JointNodeType`, `animCurve`, `SkinClusterNodeType`, etc.).

### `com.javafx.experiments.importers.maya.parser`

| Class | Role |
|-------|------|
| `MParser` | Tokenizer and MEL command dispatcher for `.ma` files |

### `com.javafx.experiments.importers.maya.types`

One class per Maya data type. Each extends `MDataType` and implements `createData()` to produce the corresponding `MData` value object.

| Type class | Maya type name |
|------------|----------------|
| `MBoolType` | `bool` |
| `MIntType` | `int` |
| `MFloatType` | `float` |
| `MFloat2Type` | `float2` |
| `MFloat3Type` | `float3` |
| `MStringType` | `string` |
| `MFloatArrayType` | `float[]` |
| `MFloat2ArrayType` | `float2[]` |
| `MFloat3ArrayType` | `float3[]` |
| `MIntArrayType` | `int[]` |
| `MInt3ArrayType` | `int3[]` |
| `MArrayType` | Generic array wrapper |
| `MCompoundType` | Compound (multi-field) type |
| `MPointerType` | `Message` (connection pointer) |
| `MPolyFaceType` | Polygon face data |
| `MMatrixType` | `matrix` (4×4 float array) |
| `MComponentListType` | Component list |
| `MNurbsCurveType` | NURBS curve |
| `MCharacterMappingType` | Character mapping |
| `MAttributeAliasType` | Attribute alias |

`MEnv` also registers **type aliases** (e.g., `double` → `float`, `double3` → `float3`, `time` → `float`).

### `com.javafx.experiments.importers.maya.values`

Interfaces and abstract classes for runtime attribute values. Each mirrors a type in `types/`:

| Value interface | Implementation (`values/impl/`) |
|-----------------|-------------------------------|
| `MData` (base interface) | `MDataImpl` |
| `MBool` | `MBoolImpl` |
| `MInt` | `MIntImpl` |
| `MFloat` | `MFloatImpl` |
| `MFloat2` | `MFloat2Impl` |
| `MFloat3` | `MFloat3Impl` |
| `MString` | `MStringImpl` |
| `MFloatArray` | `MFloatArrayImpl` |
| `MFloat2Array` | `MFloat2ArrayImpl` |
| `MFloat3Array` | `MFloat3ArrayImpl` |
| `MIntArray` | `MIntArrayImpl` |
| `MInt3Array` | `MInt3ArrayImpl` |
| `MArray` | `MArrayImpl` |
| `MCompound` | `MCompoundImpl` |
| `MPointer` | `MPointerImpl` |
| `MPolyFace` | `MPolyFaceImpl` |
| `MComponentList` | `MComponentListImpl` |
| `MNurbsCurve` | `MNurbsCurveImpl` |
| `MCharacterMapping` | `MCharacterMappingImpl` |
| `MAttributeAlias` | `MAttributeAliasImpl` |

### `com.javafx.experiments.loaders.maya`

| Class | Role |
|-------|------|
| `MayaImporter` | `Importer` plugin; orchestrates `Loader`; assembles root group and timeline |
| `Loader` | Converts `MEnv` nodes to JavaFX `Node` objects; builds mesh geometry and animation key frames |

`Loader` is package-private (`class Loader`, not `public`). Only `MayaImporter` is the public API from this package.

## External Dependencies

The importer relies on additional packages from the `Importers` module:

| Package | Usage |
|---------|-------|
| `com.javafx.experiments.shape3d` | `PolygonMesh`, `PolygonMeshView`, `SkinningMesh` |
| `com.javafx.experiments.utils3d.geom` | `Vec3f` for 3D vector math |
| `com.javafx.experiments.importers` | `Importer` interface, `SmoothingGroups` |
| `com.javafx.experiments.importers.Importer3D` | Facade for loading any 3D format |

## Further Reading

- [Scene Graph Model](scene-graph.md) — How `MEnv` organizes types and nodes
- [Data Model](data-model.md) — Type/value pairing details
- [JavaFX Bridge](javafx-bridge.md) — How `Loader` uses these packages