# Maya Importer Architecture Documentation

This folder documents the architecture of the Maya (`.ma`) importer in the JavaFX 3D Samples project. The importer reads Autodesk Maya ASCII scene files, builds an in-memory representation of the Maya DAG (Directed Acyclic Graph), and converts selected nodes into a JavaFX 3D scene graph with optional animation.

## Documentation Index

| Document | Description |
|----------|-------------|
| [Overview](overview.md) | End-to-end pipeline, module boundaries, and design goals |
| [Package Structure](package-structure.md) | Source layout, module exports, and dependencies |
| [Parser](parser.md) | `.ma` file tokenization and MEL command dispatch |
| [Scene Graph Model](scene-graph.md) | `MEnv`, `MNode`, `MNodeType`, and `MAttribute` |
| [Data Model](data-model.md) | `MDataType` / `MData` type system and value parsing |
| [Paths and Connections](paths-and-connections.md) | Attribute path resolution and `connectAttr` graph |
| [JavaFX Bridge](javafx-bridge.md) | `MayaImporter`, `Loader`, and scene conversion |
| [Transforms and Animation](transforms-and-animation.md) | `MayaGroup`, `Joint`, animation curves, and timelines |
| [Mesh and Materials](mesh-and-materials.md) | Geometry conversion, shading, and skin clusters |

## Quick Start

```java
// Via the generic 3D importer facade
Node root = Importer3D.load("file:///path/to/scene.ma");

// With animation
Pair<Node, Timeline> result = Importer3D.loadIncludingAnimation("file:///path/to/scene.ma", false);
Node root = result.getKey();
Timeline timeline = result.getValue();
```

The Maya importer is registered for the `ma` extension. `Importer3D` discovers it through `ServiceLoader` or a well-known class name fallback (`com.javafx.experiments.loaders.maya.MayaImporter`).

## Architecture at a Glance

```
.ma file
   │
   ▼
MParser ──► MEnv (scene database)
   │              │
   │              ├── MNode / MNodeType / MAttribute
   │              ├── MData values (typed attributes)
   │              └── MConnection graph (connectAttr)
   │
   ▼
Loader (JavaFX bridge)
   │
   ├── MayaGroup / Joint (transform hierarchy)
   ├── MeshView / PolygonMeshView (geometry)
   ├── PhongMaterial (shading)
   ├── SkinningMesh (deformation)
   └── Timeline + KeyFrames (animation)
   │
   ▼
MayaImporter.getRoot() / getTimeline()
```

## Two-Module Split

The Maya importer spans two Java modules:

- **`Importers`** — Core Maya scene model (`com.javafx.experiments.importers.maya.*`). Parses `.ma` files and maintains the DAG. Exported to the `Loaders` module.
- **`Loaders`** — JavaFX integration (`com.javafx.experiments.loaders.maya.*`). Converts the scene model into JavaFX `Node` and `Timeline` objects.

See [Package Structure](package-structure.md) for details.

## Related Reading

- [Overview](overview.md) for the full loading pipeline
- [Parser](parser.md) for supported MEL commands
- [JavaFX Bridge](javafx-bridge.md) for how nodes are resolved and converted