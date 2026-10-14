# Scene Graph Model

[← Back to index](README.md)

The scene graph model reconstructs Maya's DAG (Directed Acyclic Graph) and DG (Dependency Graph) in Java. The central classes are `MEnv`, `MNode`, `MNodeType`, and `MAttribute`.

## Class Hierarchy

```
MObject (abstract)
├── MEnv
├── MNodeType (abstract)
│   ├── TransformNodeType
│   ├── MeshNodeType
│   ├── JointNodeType
│   ├── animCurve / animCurveTA / ...
│   ├── SkinClusterNodeType
│   └── ... (~110 node type classes in MEnv.java)
├── MNode
├── MAttribute
└── MDataType (abstract, in types/ package)
```

Every model object carries a `name` and a reference to its `MEnv`.

## MEnv — Scene Environment

`MEnv` is the **central scene database**. A new instance is created per file load.

### Registries

| Registry | Type | Purpose |
|----------|------|---------|
| `dataTypes` | `Map<String, MDataType>` | Schema for attribute value types |
| `nodeTypes` | `Map<String, MNodeType>` | Schema for node types |
| `nodes` | `Set<MNode>` | All node instances |
| `connections` | `Map<MNode, Set<MConnection>>` | Outgoing connections per node |
| `invConnections` | `Map<MNode, Set<MConnection>>` | Incoming connections per node |

### Construction

The `MEnv()` constructor registers all data types, type aliases, and node types. This is a large static initialization (~160 lines) defining the Maya type system subset the importer understands. See [Data Model](data-model.md) for the type list.

### Key Operations

```java
MNode createNode(String typeName, String name);  // Create and register a node
MNode findNode(String name);                     // Lookup by name or DAG path (|parent|child)
void addNode(MNode node);                        // Register an existing node
void connectAttr(String src, String target);     // Record a connection
MDataType findDataType(String name);             // Resolve type (with aliases)
MNodeType findNodeType(String name);             // Resolve node type schema
```

### Playback Range

```java
void setPlaybackRange(float min, float max);
float getPlaybackStart();
float getPlaybackEnd();
```

Set by the parser from playback script nodes. Used by `Loader` to determine animation frame bounds.

### Visitor

`MEnv.Visitor` supports traversing the entire scene:

```java
visitor.visitEnv(env);
  → for each MDataType: visitDataType
  → for each MNodeType: visitNodeType → visitAttribute (per attribute)
  → for each MNode: visitNode → visitNodeAttribute (per value)
```

`MEnv.dump(PrintStream)` uses a visitor to print the full scene contents.

## MNodeType — Node Schema

`MNodeType` defines the **attribute schema** for a class of Maya nodes.

### Structure

```java
Map<String, MAttribute> attributes;           // By long name
Map<String, MAttribute> attributesByShortName; // By short name
List<MNodeType> superTypes;                   // Inheritance chain
Map<String, String> canonicalNames;           // Alias → canonical short name
```

### Inheritance

Node types form a DAG of supertypes (not single inheritance). For example:

```
DagNode → Shape → GeometryShape → DeformableShape → Mesh
Transform → (extends GeometryShape)
Joint → Transform
```

`isAssignableFrom(other)` walks `other`'s supertype chain to check compatibility.

### Attribute Lookup

`getAttribute(name)` resolves in order:

1. Long name in local `attributes`
2. Short name in `attributesByShortName`
3. Recursive search through supertypes
4. Canonical name alias resolution

### Node Creation

```java
MNode createNode(String name) {
    MNode n = doCreateNode(name);   // Default: new MNode(env, this, name)
    env.addNode(n);
    doInitNode(n);                  // Walk supertypes, call initNode() for defaults
    return n;
}
```

Subclasses override `initNode(MNode)` to set default attribute values (e.g., `TransformNodeType` sets scale to `(1,1,1)`).

### Alias System

Maya uses many short attribute aliases (`tx` → `translate.x`). `addAlias("tx", "t.x")` registers these. `getCanonicalName(name)` resolves aliases recursively through supertypes.

## MNode — Node Instance

`MNode` is a **runtime node** with actual attribute values and DAG relationships.

### State

```java
MNodeType nodeType;              // Current type (may gain local extensions)
Map<String, MData> values;       // Attribute values keyed by short name
List<MNode> parentNodes;         // DAG parents
List<MNode> childNodes;          // DAG children
boolean hasLocalType;            // Whether a per-node type extension exists
```

### Local Type Extensions

When `addAttr` is called at runtime (via the parser), the node creates a **local type** that extends its original type:

```java
MNodeType getLocalType() {
    if (!hasLocalType) {
        MNodeType localType = new MNodeType(env, getFullName()) {{
            addSuperType(superType);
        }};
        nodeType = localType;
    }
    return nodeType;
}
```

This allows per-instance dynamic attributes without polluting the global type registry.

### Attribute Access

```java
MData getAttr(String name);          // Via MPath resolution (supports dot notation)
MData getAttr(MAttribute attribute);  // Direct, lazy-creates value
MData getAttrDirect(String attrName); // By name on node type, lazy-creates
void setAttr(String name, MData value);
```

`getAttr` is the primary access method. It constructs an `MPath` and calls `apply()` to traverse compound fields and array indices.

### DAG Navigation

```java
String getFullName();    // "|parent|child|grandchild" Maya-style path
MNode getChildNode(String name);
void setParentNode(MNode parent);
void parent(MNode parent);
```

### Connection Queries

`MNode` delegates to `MEnv` for connection graph queries:

```java
List<MConnection> getConnectionsFrom(String attr);
List<MConnection> getConnectionsTo(String attr);
MNode getIncomingConnectionToType(String fromAttr, String nodeType);
MNode getOutgoingConnectionToType(String fromAttr, String nodeType);
```

These are heavily used by `Loader` to trace shading networks, skin cluster inputs, and animation curve outputs.

## MAttribute — Attribute Definition

```java
String name;          // Long name (e.g., "translate")
String shortName;     // Short name (e.g., "t")
MDataType dataType;   // Value type
MNodeType declaringNodeType;
int childIndex;       // For compound array children (via addChild)
```

`addChild()` increments `childIndex` and is used when `addAttr -p <parent>` creates indexed compound children.

## Registered Node Types (Summary)

`MEnv` registers approximately 110 node types. Key categories:

| Category | Examples |
|----------|----------|
| **DAG / Transform** | `transform`, `joint`, `camera` |
| **Geometry** | `mesh`, `nurbsCurve`, `nurbsSurface` |
| **Deformation** | `skinCluster`, `blendShape`, `tweak`, `groupParts` |
| **Animation** | `animCurve`, `animCurveTA`, `animCurveTL`, `animCurveTU`, `animCurveUA`, `animCurveUL`, `animCurveUT`, `animCurveUU` |
| **Shading** | `lambert`, `phong`, `blinn`, `file`, `shadingEngine`, `bump2d` |
| **Constraints** | `pointConstraint`, `parentConstraint`, `orientConstraint`, `aimConstraint` |
| **Dynamics** | `particle`, `field`, `emitter`, `rigidBody` |
| **Physics plugins** | `dRigidBody`, `nxRigidBody` (Bullet, PhysX) |
| **Poly modeling** | `polyCube`, `polySphere`, `polyCylinder`, `polyNormal` |
| **Utility** | `multiplyDivide`, `plusMinusAverage`, `condition`, `choice` |

Most types exist so `createNode` succeeds during parsing. Only a subset is converted to JavaFX — see [JavaFX Bridge](javafx-bridge.md).

## Further Reading

- [Data Model](data-model.md) — Attribute value types
- [Paths and Connections](paths-and-connections.md) — Connection graph and path resolution
- [Parser](parser.md) — How nodes and attributes are populated from `.ma` files