# Mesh and Materials

[← Back to index](README.md)

The mesh and materials subsystem converts Maya polygon mesh nodes into JavaFX `TriangleMesh` or `PolygonMesh` geometry, resolves shading networks into `PhongMaterial`, and applies skin cluster deformation via `SkinningMesh`.

## Mesh Conversion Pipeline

```
MNode (mesh type)
    │
    ▼
Loader.processMeshType()
    ├── Resolve shading network → PhongMaterial
    ├── convertToFXMesh() → read mesh attributes
    └── buildMeshData() → TriangleMesh or PolygonMesh
    │
    ▼
MeshView or PolygonMeshView (attached to parent MayaGroup)
```

## Mesh Attributes

`convertToFXMesh()` reads these attributes from the mesh `MNode`:

| Attribute | Short Name | Type | Purpose |
|-----------|------------|------|---------|
| `vertices` | `vt` | `float3[]` | Vertex positions |
| `faceConnect` | `fc` | `polyFaces` | Face connectivity (edge-based) |
| `pointTweak` | `pt` | `float3[]` | Per-vertex position offsets |
| `edgeData` | `ed` | `int3[]` | Edge connectivity and smooth flags |
| `uvSet` | `uvst` | compound array | UV coordinate sets |
| `currentUVSet` | `cuvs` | `string` | Active UV set name |
| `normals` | `n` | `float3[]` | Per-vertex normals |

### UV Coordinates

UV sets are stored as compound arrays. Each entry has:

- `uvsn` — UV set name (string)
- `uvsp` — UV coordinates (`float2[]`)

The loader matches `cuvs` against `uvsn` to select the active UV channel. V coordinates are flipped (`1 - v`) to match JavaFX texture coordinate conventions.

### Vertex Position

Final vertex positions combine base coordinates with point tweaks:

```java
position = vt[i] + pt[i]   // if pt exists and has data for index i
position = vt[i]             // otherwise
```

## Face Processing

Maya stores faces as **edge loops** in `MPolyFace`. The loader:

1. Walks each face's edge list (positive = forward, negative = reversed edge)
2. Resolves edge endpoints via `edgeData` (edge-vertex mapping)
3. Collects per-face vertex indices and UV indices
4. Determines smooth/flat shading per edge via `edgeIsSmooth()`

### Output Modes

Controlled by the `asPolygonMesh` flag passed to `MayaImporter.load()`:

| Mode | JavaFX type | Face format |
|------|-------------|-------------|
| `asPolygonMesh = false` (default) | `TriangleMesh` via `MeshView` | Triangulated faces |
| `asPolygonMesh = true` | `PolygonMesh` via `PolygonMeshView` | N-gon faces preserved |

#### TriangleMesh Path

1. N-gon faces are fan-triangulated from the first vertex
2. Face format: `[v0, t0, v1, t1, v2, t2, ...]` (3 verts × 2 components per triangle)
3. Smoothing groups computed via `SmoothingGroups.calcSmoothGroups()` using face normals
4. Normals derived from vertex positions when not explicitly provided

#### PolygonMesh Path

1. Faces stored as `[v0, t0, v1, t1, ...]` pairs (vertex index, UV index per corner)
2. Smoothing groups assigned per face
3. Supports N-gon faces natively

## Material Resolution

`processMeshType()` traces the shading network via connection queries:

```
mesh.iog ──connect──► shadingEngine.ss ◄──connect── lambert1 (or phong1)
```

### Diffuse (Lambert)

| Source | Attribute | Maps to |
|--------|-----------|---------|
| `lambert.c` (color) | `float3` | `PhongMaterial.diffuseColor` (× diffuse intensity `dc`) |
| `file` node on `c` | `ftn` (file texture name) | `PhongMaterial.diffuseMap` |

### Normal Map (Bump2d)

```
lambert.n ◄── bump2d.bv ◄── file.ftn
```

Maps to `PhongMaterial.bumpMap`.

### Specular (Phong)

| Source | Attribute | Maps to |
|--------|-----------|---------|
| `phong.sc` (specular color) | `float3` | `PhongMaterial.specularColor` |
| `phong.cp` (cosine power) | `float` | `PhongMaterial.specularPower` (÷ 33) |
| `file` node on `sc` | `ftn` | `PhongMaterial.specularMap` |

### Texture Loading

`loadImageFromFtnAttr()` resolves texture file paths:

1. Try as absolute/local file path
2. Fall back to URL relative to the scene file's location
3. Load via `javafx.scene.image.Image`

### Defaults

- No diffuse color → `Color.GRAY`
- No specular → `specularColor = null`
- Diffuse texture present → `diffuseColor = Color.WHITE` (texture provides color)

## Skin Cluster (Deformation)

`processClusterType()` handles `skinCluster` nodes for skeletal deformation.

### Prerequisites

Skinning requires **polygon mesh mode** (`asPolygonMesh = true`). Triangle mesh skinning is not supported; the loader logs an info message and copies static geometry instead.

### Data Flow

```
skinCluster
    ├── ma[] ◄── joint nodes (influence objects)
    ├── ip ◄── input mesh (original geometry)
    ├── og ──► output mesh (deformed geometry)
    ├── pm[] ── bindPreMatrix per joint
    ├── gm ── bindGlobalMatrix
    └── wl[] ── per-vertex weight lists
         └── w[] ── weight per joint influence
```

### Processing Steps

1. **Collect joints** — Resolve `ma[i]` connections to `joint` nodes; build `List<Joint>`
2. **Resolve meshes** — Find input mesh (`ip`) and output mesh (`og`) through the deformation chain
3. **Hide original** — Set the original input mesh invisible
4. **Bind matrices** — Convert `pm[]` (per-joint) and `gm` (global) from `float[]` to `Affine`
5. **Weights** — Extract `wl[i].w[j]` into a `float[jointCount][vertexCount]` array
6. **Create SkinningMesh** — Wrap the source `PolygonMesh` with weights, bind matrices, and joint references
7. **Animation timer** — `SkinningMeshTimer` (extends `AnimationTimer`) calls `mesh.update()` each frame, started/stopped based on scene attachment

### SkinningMesh

From `com.javafx.experiments.shape3d.SkinningMesh`:

```java
new SkinningMesh(
    sourceMesh,        // PolygonMesh geometry
    weights,           // float[joints][vertices]
    bindPreMatrix,     // Affine per joint
    bindGlobalMatrix,  // Affine global bind
    jointNodes,        // List<Joint>
    jointForest        // Root children with joint hierarchies
);
```

The `SkinningMesh` reads live joint transforms each frame and deforms vertex positions accordingly.

### Mesh Chain Resolution

The loader provides helper methods to trace deformation chains:

| Method | Purpose |
|--------|---------|
| `resolveInputMesh(cluster)` | Find the mesh feeding into the cluster |
| `resolveOutputMesh(cluster)` | Find the mesh output by the cluster |
| `resolveOrigInputMesh(cluster)` | Find the original undeformed mesh |

These navigate through intermediate nodes (`groupParts`, `blendShape`, etc.) via connection queries.

## Edge Data

Maya edge data (`ed` attribute) stores triplets per edge:

```
[vertexA, vertexB, smoothFlag]
```

- Positive edge index in face loops → traverse forward (A→B)
- Negative edge index → traverse reversed (B→A), index = `-edge - 1`
- `smoothFlag != 0` → smooth shading across this edge

## Smoothing Groups

For `TriangleMesh`, `SmoothingGroups.calcSmoothGroups()` computes per-face smoothing group indices based on:

- Face normals (computed from vertex positions)
- Edge smooth flags from Maya edge data
- Adjacent face normal comparison

## Further Reading

- [JavaFX Bridge](javafx-bridge.md) — Processor dispatch and node resolution
- [Paths and Connections](paths-and-connections.md) — Shading network connection tracing
- [Data Model](data-model.md) — `MPolyFace` and array value details
- [Transforms and Animation](transforms-and-animation.md) — Joint transforms driving skinning