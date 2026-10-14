# Data Model

[← Back to index](README.md)

The data model provides a **type/value system** for Maya attribute data. Types define schemas (`MDataType`); values hold runtime data (`MData`). The pairing follows a factory pattern: each `MDataType.createData()` produces its corresponding `MData` implementation.

## Architecture

```
MDataType (schema)                    MData (runtime value)
├── MBoolType          ──createData()──►  MBool / MBoolImpl
├── MIntType           ──createData()──►  MInt / MIntImpl
├── MFloatType         ──createData()──►  MFloat / MFloatImpl
├── MFloat3Type        ──createData()──►  MFloat3 / MFloat3Impl
├── MStringType        ──createData()──►  MString / MStringImpl
├── MFloatArrayType    ──createData()──►  MFloatArray / MFloatArrayImpl
├── MArrayType         ──createData()──►  MArray / MArrayImpl
├── MCompoundType      ──createData()──►  MCompound / MCompoundImpl
├── MPolyFaceType      ──createData()──►  MPolyFace / MPolyFaceImpl
└── ...
```

Both `MDataType` and `MData` extend `MObject`, giving them `name` and `env` references.

## MDataType (types/ package)

Abstract base in `types/MDataType.java`:

```java
public abstract class MDataType extends MObject {
    public abstract MData createData();
}
```

Each concrete type class:

1. Registers itself with `MEnv.addDataType()` in the `MEnv()` constructor
2. Implements `createData()` to instantiate the matching `*Impl` class from `values/impl/`

### Registered Types

| Registered name | Java class | Aliases |
|-----------------|------------|---------|
| `bool` | `MBoolType` | — |
| `int` | `MIntType` | `short`, `long`, `enum`, `function` |
| `float` | `MFloatType` | `double`, `time` |
| `float2` | `MFloat2Type` | `double2` |
| `float3` | `MFloat3Type` | `double3` |
| `string` | `MStringType` | — |
| `float[]` | `MFloatArrayType` | `double[]` |
| `float2[]` | `MFloat2ArrayType` | — |
| `float3[]` | `MFloat3ArrayType` | `double3[]` |
| `int[]` | `MIntArrayType` | `long[]`, `enum[]` |
| `int3[]` | `MInt3ArrayType` | `long3[]` |
| `string[]` | `MArrayType(string)` | `stringArray` |
| `Message` | `MPointerType` | — |
| `matrix` | `MMatrixType` | `fltMatrix` |
| `polyFaces` | `MPolyFaceType` | — |
| `componentList` | `MComponentListType` | — |
| `nurbsCurve` | `MNurbsCurveType` | — |
| `characterMapping` | `MCharacterMappingType` | — |
| `AttributeAlias` | `MAttributeAliasType` | — |

Aliases are registered via `MEnv.aliasDataType(alias, canonicalName)`, mapping multiple Maya type names to a single `MDataType`.

### MArrayType

`MArrayType` is a **parametric wrapper** around another `MDataType`:

```java
new MArrayType(env, env.findDataType("Message"))  // Message[]
new MArrayType(env, t)  // where t is any MDataType
```

Used for multi-connection attributes like `skinCluster.ma` (joint array) and `animCurve.ktv` (keyframe array).

### MCompoundType

Represents multi-field compound attributes. Fields are added via `addField(name, type, defaultValue)`. Used implicitly by node type definitions for structured attributes (e.g., keyframe compounds with `kt` and `kv` fields).

## MData (values/ package)

The `MData` interface defines the contract for all attribute values:

```java
public interface MData {
    MEnv getEnv();
    MDataType getType();
    void setSize(int size);
    void parse(List<String> values);       // Parse from MEL setAttr tokens
    void parse(Iterator<String> iter);
    void parse(String field, List<String> values);
    MData getData(String path);            // Compound field access
    MData getFieldData(String name);       // Named field access
    MData getData(int index);              // Array index access
    MData getData(int start, int end);     // Array slice access
}
```

### Parsing

When `MParser.doSetAttr()` encounters value tokens, it calls `data.parse(value)`. Each implementation interprets tokens according to Maya conventions:

- **Scalars** (`MFloat`, `MInt`, `MBool`) — Parse a single token
- **Vectors** (`MFloat3`) — Parse 3 tokens as x, y, z
- **Arrays** (`MFloatArray`, `MIntArray`) — Parse `-size N` then N values; grow via `setSize`
- **Compounds** (`MCompound`) — Delegate to child fields by name
- **Arrays of compounds** (`MArray`) — Parse indexed children
- **Polygon faces** (`MPolyFace`) — Parse Maya face descriptor syntax (vertex/UV/edge counts per face)

### Key Value Types

#### Scalars

| Interface | Accessors | Notes |
|-----------|-----------|-------|
| `MFloat` | `get()`, `set(float)` | Also serves `double` via alias |
| `MInt` | `get()`, `set(int)` | Also serves `enum` |
| `MBool` | `get()`, `set(boolean)` | |
| `MString` | `get()`, `set(String)` | |

#### Vectors

| Interface | Accessors |
|-----------|-----------|
| `MFloat2` | `get()`, `getX()`, `getY()` |
| `MFloat3` | `get()`, `getX()`, `getY()`, `getZ()` |

#### Arrays

| Interface | Accessors |
|-----------|-----------|
| `MFloatArray` | `get()`, `get(int)`, `getSize()`, `setSize(int)` |
| `MFloat2Array` | `get()`, `getSize()` |
| `MFloat3Array` | `get()`, `getSize()` |
| `MIntArray` | `get()`, `get(int)`, `getSize()` |
| `MInt3Array` | `get()`, `getSize()` |
| `MArray` | `get(int)`, `getSize()`, `size()` |

Raw array data is stored as `float[]` or `int[]` with interleaved components (e.g., `float3` array stores `[x0,y0,z0, x1,y1,z1, ...]`).

#### Compounds

`MCompound` holds named child `MData` fields. Accessed via `getData(name)` or `getFieldData(name)`. Used for:

- UV sets (`uvsn`, `uvsp` fields)
- Keyframe data (`kt` = key time, `kv` = key value)
- Weight lists (`w` field in skin cluster weights)

#### Special Types

| Interface | Purpose |
|-----------|---------|
| `MPointer` | `Message` type; holds an `MPath` target for connection endpoints |
| `MPolyFace` | Polygon face connectivity data for mesh nodes |
| `MComponentList` | Lists of mesh components (vertices, edges, faces) |
| `MNurbsCurve` | NURBS curve control points and knots |

## Implementation Pattern (values/impl/)

Each `*Impl` class:

1. Extends or implements the corresponding `MData` interface
2. Stores a reference to its `MDataType`
3. Implements `parse()` to consume MEL value tokens
4. Implements indexed/field access methods

Example flow for `setAttr ".tx" 5.0`:

```
MNode.getAttr("tx")
  → MPath canonicalizes "tx" to "t.x"
  → MPath.apply() selects attribute "t", then field "x"
  → Returns MFloat from compound MFloat3
  → MFloat.parse(["5.0"]) sets value to 5.0
```

## Lazy Creation

Attribute values are **not** pre-allocated when a node is created. `MNode.getAttr(MAttribute)` creates the `MData` on first access:

```java
MData data = values.get(attribute.getShortName());
if (data == null) {
    data = attribute.getType().createData();
    values.put(attribute.getShortName(), data);
}
return data;
```

Default values are set by `MNodeType.initNode()` during node creation, which accesses attributes (triggering lazy creation) and sets initial values.

## Further Reading

- [Scene Graph Model](scene-graph.md) — How attributes attach to nodes
- [Paths and Connections](paths-and-connections.md) — How `MPath` navigates compound and array data
- [Parser](parser.md) — How `setAttr` tokens reach `MData.parse()`
- [Mesh and Materials](mesh-and-materials.md) — How mesh attribute values are consumed