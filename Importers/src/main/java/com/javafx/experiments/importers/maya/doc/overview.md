# Overview

[← Back to index](README.md)

The Maya importer reads **Maya ASCII** (`.ma`) scene files and produces a **JavaFX 3D scene graph** with optional **animation timeline**. It is part of the broader `Importer3D` framework that supports multiple 3D formats (`ma`, `obj`, `dae`, `ase`, `fxml`).

## Design Goals

1. **Parse, don't execute** — The importer interprets a subset of Maya MEL commands embedded in `.ma` files. It does not run Maya; it reconstructs enough of the scene DAG to drive JavaFX rendering.
2. **Mirror Maya's node model** — Nodes have types with attribute schemas, values are typed data objects, and `connectAttr` wires form a dependency graph analogous to Maya's DG.
3. **Selective conversion** — Only node types needed for 3D display (transforms, joints, meshes, materials, animation curves, skin clusters) are converted to JavaFX. Many registered node types exist solely so parsing succeeds without errors.
4. **JavaFX-native output** — Transforms map to `MayaGroup`/`Joint` with JavaFX `Transform` objects; meshes become `TriangleMesh` or `PolygonMesh`; animation becomes `Timeline` + `KeyFrame`/`KeyValue` pairs.

## End-to-End Pipeline

```
┌─────────────┐     ┌──────────┐     ┌──────────────┐     ┌───────────────┐
│  .ma file   │────►│  MParser │────►│    MEnv      │────►│    Loader     │
│  (on disk)  │     │          │     │  (scene DB)  │     │  (converter)  │
└─────────────┘     └──────────┘     └──────────────┘     └───────┬───────┘
                                                                    │
                                                                    ▼
                                                          ┌─────────────────┐
                                                          │  MayaImporter   │
                                                          │  root: MayaGroup│
                                                          │  timeline       │
                                                          └─────────────────┘
```

### Phase 1: Parse (`MParser`)

`MParser` reads the `.ma` file line by line, tokenizes MEL syntax, and dispatches commands (`createNode`, `setAttr`, `connectAttr`, `parent`, etc.). Each command mutates the `MEnv` scene database. See [Parser](parser.md).

### Phase 2: Scene Database (`MEnv`)

`MEnv` holds:

- A registry of **data types** and **node types** (schemas)
- A set of **node instances** (`MNode`) with attribute values (`MData`)
- A **connection graph** recording `connectAttr` relationships
- **Playback range** metadata from script nodes

See [Scene Graph Model](scene-graph.md) and [Data Model](data-model.md).

### Phase 3: Convert (`Loader`)

`Loader` walks every `MNode` in the environment and dispatches to type-specific processors:

| Maya node type | JavaFX output |
|----------------|---------------|
| `transform` | `MayaGroup` |
| `joint` | `Joint` |
| `mesh` | `MeshView` or `PolygonMeshView` |
| `skinCluster` | `SkinningMesh` (polygon mode only) |
| `animCurve*` | `KeyValue` entries in a `TreeMap` |
| Shading nodes | `PhongMaterial` on mesh views |

See [JavaFX Bridge](javafx-bridge.md), [Mesh and Materials](mesh-and-materials.md), and [Transforms and Animation](transforms-and-animation.md).

### Phase 4: Assemble (`MayaImporter`)

`MayaImporter` wraps `Loader` results:

- Top-level nodes (no JavaFX parent) are added to a root `MayaGroup`
- Key frames from `Loader.keyFrameMap` are assembled into a `Timeline`
- The result is exposed via `getRoot()` and `getTimeline()`

## Entry Points

### `Importer3D` (facade)

```java
Importer3D.load(fileUrl);                          // Node only
Importer3D.loadIncludingAnimation(fileUrl, false); // Node + Timeline
```

`Importer3D` selects the importer by file extension and delegates to `MayaImporter` for `.ma` files.

### `MayaImporter` (direct)

```java
MayaImporter importer = new MayaImporter();
importer.load(url, asPolygonMesh);
MayaGroup root = importer.getRoot();
Timeline timeline = importer.getTimeline();
```

The `asPolygonMesh` flag controls whether meshes are loaded as `PolygonMesh` (supports skinning) or triangulated `TriangleMesh`.

## Key Architectural Patterns

### Visitor Pattern

`MEnv.Visitor` provides a traversal hook over the scene database (environment, data types, node types, nodes, attributes). Used by `MEnv.dump()` for debugging.

### Type Hierarchy with Inheritance

`MNodeType` supports multiple supertypes (`addSuperType`). Attribute lookup walks the inheritance chain. `MNode.isInstanceOf(type)` checks assignability through supertypes.

### Lazy Attribute Materialization

`MNode.getAttr()` creates `MData` instances on first access via `MAttribute.getType().createData()`. Values are populated later by `setAttr` parsing.

### Path-Based Attribute Access

Attribute references like `pCube1.translateX` or `skinCluster1.wl[0].w` are resolved through `MPath`, which decomposes dot-notation and array indices into a chain of component operations. See [Paths and Connections](paths-and-connections.md).

## Limitations

- **Partial MEL support** — Unrecognized commands are logged and skipped. `disconnectAttr`, `currentUnit`, `fileInfo`, and `requires` are no-ops.
- **No binary `.mb` files** — Only ASCII `.ma` format is supported.
- **Triangle mesh skinning** — Skin deformation requires polygon mesh mode (`asPolygonMesh = true`).
- **Incomplete node coverage** — Physics nodes (Bullet, PhysX), particle systems, and many utility nodes are registered but not converted to JavaFX.
- **String escaping** — The tokenizer does not fully unescape quoted strings.

## Further Reading

- [Parser](parser.md) — MEL command reference
- [Scene Graph Model](scene-graph.md) — Node and type system
- [JavaFX Bridge](javafx-bridge.md) — Conversion details