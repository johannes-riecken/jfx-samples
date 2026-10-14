# JavaFX Bridge

[← Back to index](README.md)

The JavaFX bridge converts the parsed Maya scene database (`MEnv`) into JavaFX 3D objects. It lives in the `Loaders` module and consists of `MayaImporter` (public API) and `Loader` (conversion engine).

## Component Overview

```
MayaImporter (implements Importer)
    │
    ├── load(url, asPolygonMesh)
    │       └── Loader.load(url, asPolygonMesh)
    │               ├── MParser.parse(url)  →  MEnv
    │               ├── loadModel()         →  cache node type references
    │               └── for each MNode: resolveNode(n)
    │
    ├── getRoot()     → MayaGroup (root container)
    └── getTimeline() → Timeline (animation)
```

## MayaImporter

Public entry point implementing the `Importer` interface.

### API

```java
public class MayaImporter extends Importer {
    boolean isSupported(String extension);  // Returns true for "ma"
    void load(String url, boolean asPolygonMesh);
    MayaGroup getRoot();
    Timeline getTimeline();
    Set<Node> getMeshParents();
}
```

### Load Sequence

1. Creates a `Loader` and calls `loader.load(url, asPolygonMesh)`
2. Iterates `loader.loaded` values — adds top-level nodes (no JavaFX parent) to `root` `MayaGroup`
3. Skips empty `MeshView` instances (zero vertices)
4. Sets `DepthTest.ENABLE` on all added nodes
5. Builds `Timeline` from `loader.keyFrameMap`:
   - Each `Map.Entry<Float, List<KeyValue>>` becomes a `KeyFrame`
   - Time is `Duration.millis(frame * 1000)` (frames treated as seconds at 1:1, then scaled to milliseconds)

### Root Assembly

Only nodes without a JavaFX parent are added to the root. The DAG parent relationships established during `Loader` processing determine which nodes are top-level. Default Maya cameras (`persp`, `top`, `front`, `side`) are excluded in `processTransformType`.

## Loader

Package-private conversion engine. Maps `MNode` → JavaFX `Node` through a dispatch table.

### State

```java
MEnv env;
Map<MNode, Node> loaded;                    // Conversion cache
Map<Float, List<KeyValue>> keyFrameMap;     // Animation data (TreeMap, sorted by time)
Map<Node, MNode> meshParents;                // Mesh-to-parent tracking
```

Cached node type references (resolved once in `loadModel()`):

```java
MNodeType transformType, jointType, meshType, cameraType;
MNodeType animCurve, animCurveTA, animCurveUA, animCurveUL, animCurveUT, animCurveUU;
MNodeType lambertType, phongType, blinnType, fileType;
MNodeType skinClusterType, blendShapeType, groupPartsType, shadingEngineType;
```

### Load Pipeline

```java
public void load(URL url, boolean asPolygonMesh) {
    env = new MEnv();
    new MParser(env).parse(url);
    loadModel();
    for (MNode n : env.getNodes()) {
        resolveNode(n);
    }
}
```

### Node Resolution

`resolveNode(MNode)` is the core dispatch loop:

```
resolveNode(n)
    │
    ├── Already in loaded? → return cached Node
    │
    └── processNode(n)
            ├── Resolve parent(s) first (recursive)
            └── Dispatch by node type:
                    ├── skinCluster  → processClusterType
                    ├── mesh         → processMeshType
                    ├── joint        → processJointType
                    ├── transform    → processTransformType
                    ├── animCurve*   → processAnimCurve
                    └── (other)      → no-op
```

Parent resolution ensures the JavaFX scene tree is built top-down. Each processor creates a JavaFX node, stores it in `loaded`, and adds it as a child of the resolved parent `Group`.

### Processors

| Processor | Input | Output | Parent attachment |
|-----------|-------|--------|-------------------|
| `processTransformType` | `transform` node | `MayaGroup` | Added to parent `Group` |
| `processJointType` | `joint` node | `Joint` | Added to parent `Group` |
| `processMeshType` | `mesh` node | `MeshView` or `PolygonMeshView` | Added to parent transform `Group` |
| `processClusterType` | `skinCluster` node | `SkinningMesh` on output mesh | Replaces mesh on existing `PolygonMeshView` |
| `processAnimCurve` | `animCurve*` node | `KeyValue` entries in `keyFrameMap` | No JavaFX node created |

See dedicated docs for details:

- [Transforms and Animation](transforms-and-animation.md) — Transform and animation processors
- [Mesh and Materials](mesh-and-materials.md) — Mesh and skinning processors

### Node-to-JavaFX Mapping

The `loaded` map serves as a conversion cache and lookup table:

```java
MNode (Maya)  ──►  Node (JavaFX)
pCube1 (transform)  →  MayaGroup
joint1 (joint)      →  Joint
pCubeShape1 (mesh)  →  MeshView / PolygonMeshView
animCurve1          →  null (animation data only)
skinCluster1        →  null (modifies existing mesh)
```

### Playback Range

```java
startFrame = round(env.getPlaybackStart() - 1);
endFrame   = round(env.getPlaybackEnd() - 1);
```

Used as bounds for animation curve conversion.

## Integration with Importer3D

`Importer3D` discovers `MayaImporter` via:

1. `ServiceLoader<Importer>` from importer JARs
2. Fallback class loading: `com.javafx.experiments.loaders.maya.MayaImporter`

```java
importer.load(fileUrl, asPolygonMesh);
return new Pair<>(importer.getRoot(), importer.getTimeline());
```

## Error Handling

- Parse errors in `Loader.load()` are wrapped in `RuntimeException`
- `processMeshType` declares `throws RuntimeException`
- Missing textures log at `SEVERE` level via `java.util.logging.Logger`
- Skinning on triangle meshes logs an `INFO` message suggesting polygon mode

## Debug Flags

```java
Loader.DEBUG = false;  // Verbose node/mesh/material logging
Loader.WARN  = false;  // Error URL logging
MayaImporter.DEBUG = Loader.DEBUG;
MayaImporter.WARN  = Loader.WARN;
```

## Further Reading

- [Transforms and Animation](transforms-and-animation.md) — `MayaGroup`, `Joint`, animation curve conversion
- [Mesh and Materials](mesh-and-materials.md) — Geometry and shading conversion
- [Overview](overview.md) — Full pipeline context