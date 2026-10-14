# Paths and Connections

[← Back to index](README.md)

Maya scenes wire attributes together through `connectAttr` commands. The importer models these as `MPath` (attribute address) and `MConnection` (directed edge) objects, managed by `MEnv`.

## MPath — Attribute Path Resolution

`MPath` resolves Maya-style attribute references like `pCube1.translateX`, `skinCluster1.wl[0].w`, or `|group|mesh.vtx[3]`.

### Construction

Two constructors:

```java
MPath(MEnv env, String path);   // Full path: "nodeName.attr[0].field"
MPath(MNode node, String path); // Relative: ".attr[0].field" (node already known)
```

Full paths split at the first `.` to extract the node name, then resolve via `env.findNode(nodeName)`. DAG paths with `|` separators are handled by `findNode`.

### Component Model

A path is a sequence of **components** applied to a starting node:

```
MPath
├── node: MNode
└── components: List<Component>
    ├── Select(".attrName")    — Field or attribute selection
    ├── Index([n])             — Single array index
    └── Slice([start:end])     — Array range
```

Components are ordered: `Index < Slice < Select` for comparison purposes.

### Resolution (`apply()`)

```java
MData apply() {
    MData data = components.get(0).apply(node);  // First component selects attribute on node
    return apply(1, data);                       // Remaining components drill into data
}
```

- `Select.apply(MNode)` → `node.getAttrDirect(name)`
- `Select.apply(MData)` → `data.getData(name)` (compound field)
- `Index.apply(MData)` → `data.getData(index)` (grows array via `setSize`)
- `Slice.apply(MData)` → `data.getData(start, end)`

### Canonicalization

Before adding a `Select` component, the path checks whether the current data is an array type. If so, it inserts an `Index(0)` to canonicalize connections that target an array without an explicit index:

```
.iog.og[n]  →  .iog[0].og[n]
```

Attribute names are also canonicalized through `MNodeType.getCanonicalName()`:

```
tx  →  t.x
```

### Path Utilities

| Method | Purpose |
|--------|---------|
| `getTargetNode()` | The node this path starts from |
| `getTargetAttribute(env)` | The `MAttribute` at the attribute offset |
| `getComponentSelector()` | Dotted selector string (e.g., `t[0]`) |
| `getLastPathIndex()` | Last `[n]` index, or -1 |
| `isPrefixOf(other)` | Whether this path is a prefix of another |
| `getParentPath()` | Path with last component removed |
| `isValid()` | Whether the target node exists |
| `compareTo(other)` | Lexicographic ordering for sorting |

### Example

Path: `pCube1.translateX`

```
node = findNode("pCube1")
components = [Select("t"), Select("x")]   // after canonicalization of "translateX" → "t.x"
apply():
  Select("t").apply(node) → MFloat3 (translate attribute)
  Select("x").apply(MFloat3) → MFloat (x component)
```

## MConnection — Connection Record

```java
public class MConnection {
    MPath sourcePath;   // Output attribute
    MPath targetPath;   // Input attribute
}
```

Immutable pair of resolved paths. Equality and hash code are based on both paths.

### Comparators

- `SOURCE_PATH_COMPARATOR` — Sort connections by source path
- `TARGET_PATH_COMPARATOR` — Sort connections by target path

Used by `MNode.getConnectionsFrom/To()` to return sorted connection lists.

## Connection Graph (MEnv)

`MEnv` maintains a **bidirectional connection index**:

```java
Map<MNode, Set<MConnection>> connections;     // Outgoing per node
Map<MNode, Set<MConnection>> invConnections;   // Incoming per node
```

### Recording Connections

`connectAttr(String src, String target)`:

1. Parse both strings into `MPath` objects
2. Verify both target nodes exist (silently return if not)
3. Create `MConnection(srcPath, targetPath)`
4. Add to `connections[srcNode]` and `invConnections[targetNode]`

Additionally, `MParser.doConnectAttr()` handles `MPointer` (Message) types by setting bidirectional `MPointer.setTarget()` references.

### Querying Connections

| Method | Returns |
|--------|---------|
| `getConnectionsFrom(MPath)` | Connections where source path matches (with sub-path checking) |
| `getConnectionsTo(MPath)` | Connections where target path matches |
| `getPathsConnectingFrom(MPath)` | Target paths of outgoing connections |
| `getPathsConnectingTo(MPath)` | Source paths of incoming connections |
| `getIncomingConnections(MNode)` | All connections targeting this node |
| `getOutgoingConnections(MNode)` | All connections sourced from this node |

**Sub-path matching:** When `checkSubPaths` is true (default), a query path `node.attr` matches connections to `node.attr[0].field` because the connection target is a sub-path of the query.

### MNode Connection Helpers

`MNode` provides higher-level queries used extensively by `Loader`:

```java
// Find the node connected TO this attribute FROM a specific type
MNode getIncomingConnectionToType("ss", "lambert");
// e.g., shadingEngine.ss ← lambert1.outColor

// Find the node connected FROM this attribute TO a specific type
MNode getOutgoingConnectionToType("iog", "shadingEngine", true);
// e.g., mesh.iog → shadingEngine1

// List all paths connecting out of an attribute
List<MPath> getPathsConnectingFrom("o");
// e.g., animCurve1.o → transform1.t[0]
```

## How Connections Drive Conversion

The `Loader` traces Maya's DG through connections:

### Shading Network

```
mesh.iog ──► shadingEngine.ss ◄── lambert1.oc
                                  lambert1.c ◄── file1.oc (diffuse texture)
                                  bump2d1.bv ◄── file2.oc (normal map)
```

`processMeshType` follows `iog → shadingEngine → lambert/phong → file` to build `PhongMaterial`.

### Skin Cluster

```
skinCluster.ma[i] ◄── joint[i]
skinCluster.og ──► outputMesh
skinCluster.ip ◄── inputMesh
```

`processClusterType` resolves joint influences, input/output meshes, bind matrices, and per-vertex weights.

### Animation

```
animCurve1.o ──► transform1.t[0]
animCurve2.o ──► transform1.r[1]
```

`processAnimCurve` follows output connections to find the target `MayaGroup` or `Joint` property and builds `KeyValue` entries.

## MPointer (Message Type)

Maya's `Message` attribute type represents a connection endpoint without data. `MPointer` stores an `MPath` target:

```java
public interface MPointer extends MData {
    void setTarget(MPath path);
    MPath getTarget();
}
```

When `connectAttr` links two `Message` attributes, both pointers are set to reference each other. This is used for DAG connections like `skinCluster.ma → joint.worldMatrix`.

## Further Reading

- [Scene Graph Model](scene-graph.md) — Node and attribute structure
- [Parser](parser.md) — `connectAttr` command handling
- [JavaFX Bridge](javafx-bridge.md) — How `Loader` uses connection queries