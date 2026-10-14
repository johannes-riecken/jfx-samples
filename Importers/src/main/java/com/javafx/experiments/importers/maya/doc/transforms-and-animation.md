# Transforms and Animation

[← Back to index](README.md)

The Maya importer maps transform and joint nodes to JavaFX `Group` subclasses with explicit transform chains, and converts Maya animation curves into JavaFX `Timeline` key frames.

## Transform Nodes

### MayaGroup — Transform Node

`MayaGroup` extends `javafx.scene.Group` and models a Maya **transform** node. Its transform chain mirrors Maya's pre-multiply matrix decomposition:

```
[T][RT][RP][R][RA][RP⁻¹][ST][SP][SH][S][SP⁻¹]
```

Simplified to the transforms Maya actually uses:

```
[T][RPT][RP][RZ][RY][RX][RP⁻¹][SPT][SP][S][SP⁻¹]
```

| JavaFX Transform | Maya Attribute | Short Name |
|------------------|----------------|------------|
| `t` (Translate) | translate | `t` |
| `rpt` (Translate) | rotatePivotTranslate | `rpt` |
| `rp` (Translate) | rotatePivot | `rp` |
| `rz, ry, rx` (Rotate) | rotate Z/Y/X | `rz, ry, rx` |
| `rpi` (Translate) | rotatePivot inverse | `-rp` |
| `spt` (Translate) | scalePivotTranslate | `spt` |
| `sp` (Translate) | scalePivot | `sp` |
| `s` (Scale) | scale | `s` |
| `spi` (Translate) | scalePivot inverse | `-sp` |

`Loader.processTransformType()` reads these attributes from the `MNode` and sets the corresponding JavaFX transform properties. Default Maya scene cameras (`persp`, `top`, `front`, `side`) are skipped.

A copy constructor `MayaGroup(MayaGroup)` clones all transforms without copying children.

### Joint — Joint Node

`Joint` extends `Group` and models a Maya **joint** node. Its transform chain:

```
[T][IS][JOZ][JOY][JOX][RZ][RY][RX][S]
```

| JavaFX Transform | Maya Attribute | Short Name |
|------------------|----------------|------------|
| `t` (Translate) | translate | `t` |
| `is` (Scale) | inverse parent scale | computed from `ssc` |
| `joz, joy, jox` (Rotate) | jointOrient Z/Y/X | `jo` |
| `rz, ry, rx` (Rotate) | rotate Z/Y/X | `rz, ry, rx` |
| `s` (Scale) | scale | `s` |

**Segment Scale Compensate (`ssc`):** When `ssc` is true, `is` (inverse scale) is set to `1/parent.scale` to counteract parent scaling. When false, `is` is identity `(1,1,1)`.

`Loader.processJointType()` populates all transforms and adds the joint to its parent `Group`.

### Xform — Generic Transform

`Xform` is a simpler transform `Group` not directly used by `Loader` but available for manual scene construction. It supports configurable `RotateOrder` (XYZ, XZY, YXZ, YZX, ZXY, ZYX) and provides convenience setters (`setTx`, `setRotate`, `setScale`, `setPivot`).

## Animation System

### Overview

```
animCurve node (in MEnv)
    │
    ▼
Loader.processAnimCurve()
    │
    ▼
Loader.convertAnimCurveRange()
    │
    ▼
keyFrameMap: TreeMap<Float, List<KeyValue>>
    │
    ▼
MayaImporter assembles Timeline with KeyFrames
```

### Animation Curve Types

| Maya Type | Unit | Notes |
|-----------|------|-------|
| `animCurve` | Generic | |
| `animCurveTA` | Angle | Rotation curves; values converted to degrees |
| `animCurveTL` | Linear | |
| `animCurveTU` | Unitless | |
| `animCurveUA` | Unitless absolute | Key times in seconds |
| `animCurveUL` | Linear absolute | Key times in seconds |
| `animCurveUT` | Time absolute | Key times in seconds |
| `animCurveUU` | Unitless absolute | Key times in seconds |

### Target Properties

`processAnimCurve` follows the `o` (output) connection to find the driven node and attribute:

**MayaGroup targets:**

| Path selector | JavaFX property |
|---------------|-----------------|
| `t[0]`, `t[1]`, `t[2]` | `t.xProperty()`, `t.yProperty()`, `t.zProperty()` |
| `s[0]`, `s[1]`, `s[2]` | `s.xProperty()`, `s.yProperty()`, `s.zProperty()` |
| `r[0]`, `r[1]`, `r[2]` | `rx.angleProperty()`, `ry.angleProperty()`, `rz.angleProperty()` |
| `rp[0]`, `rp[1]`, `rp[2]` | `rp.xProperty()`, `rp.yProperty()`, `rp.zProperty()` |
| `sp[0]`, `sp[1]`, `sp[2]` | `sp.xProperty()`, `sp.yProperty()`, `sp.zProperty()` |

**Joint targets:**

| Path selector | JavaFX property |
|---------------|-----------------|
| `t[0]`, `t[1]`, `t[2]` | `t.x/y/zProperty()` |
| `s[0]`, `s[1]`, `s[2]` | `s.x/y/zProperty()` |
| `jo[0]`, `jo[1]`, `jo[2]` | `jox/joy/joz.angleProperty()` |
| `r[0]`, `r[1]`, `r[2]` | `rx/ry/rz.angleProperty()` |

### Keyframe Conversion (`convertAnimCurveRange`)

Reads from the animCurve node:

| Attribute | Short Name | Content |
|-----------|------------|---------|
| `keyTanValue` | `ktv` | Array of compounds: `{kt: keyTime, kv: keyValue}` |
| `keyInTangent` | `kit` | In-tangent type per key |
| `keyOutTangent` | `kot` | Out-tangent type per key |
| `keyInX` | `kix` | In-tangent X component |
| `keyInY` | `kiy` | In-tangent Y component |
| `keyOutX` | `kox` | Out-tangent X component |
| `keyOutY` | `koy` | Out-tangent Y component |
| `tangent` | `tan` | Global tangent mode |

#### Tangent Types

| Value | Name | Behavior |
|-------|------|----------|
| 1 | Fixed | Not handled |
| 2 | Linear | Tangent points toward adjacent key |
| 3 | Flat | Zero Y component on tangent |
| 5 | Stepped | Immediate jump to next value |
| 9 | Spline | Slope matches adjacent keys |
| 10 | Clamped | Spline unless near adjacent value, then linear |
| 16 | Plateau | Flat at local extrema, spline otherwise |

#### Interpolation

Two interpolator modes:

1. **Tangent interpolator** (default) — Uses `MayaAnimationCurveInterpolator` with cubic Bezier control points derived from Maya's `kix/kiy/kox/koy` tangent data
2. **Original interpolator** — Fallback using computed tangent slopes

`MayaAnimationCurveInterpolator` implements cubic Bezier interpolation:

```
B(t) = (1-t)³·P0 + 3(1-t)²t·P1 + 3(1-t)t²·P2 + t³·P3
```

Where `P0 = startValue`, `P3 = endValue`, and `P1/P2` are offset by `p1Delta`/`p2Delta` from the tangent data.

#### Frame Timing

- Default: key times are in **frames** at 24 FPS
- `animCurveUA/UL/UT/UU`: key times are in **seconds**
- One-frame adjustment: if the first key time is not 0, subtract 1 frame from all keys
- An `EPSILON` (0.000001) fudge factor ensures key frames land on discrete timeline units

### Timeline Assembly

`MayaImporter.load()` converts the `keyFrameMap` into a `Timeline`:

```java
timeline = new Timeline();
for (Map.Entry<Float, List<KeyValue>> e : loader.keyFrameMap.entrySet()) {
    timeline.getKeyFrames().add(
        new KeyFrame(
            Duration.millis(e.getKey() * 1000f),
            e.getValue().toArray(new KeyValue[e.getValue().size()])
        )
    );
}
```

Multiple `KeyValue` objects at the same time are grouped into a single `KeyFrame`.

### Frame Helper

`Frame` extends `Duration` with a 24 FPS convention:

```java
Frame(int frames) → Duration of frames/24 seconds
Frame.valueOf(double frames) → static factory
```

Used internally for frame-to-time conversion.

## Further Reading

- [JavaFX Bridge](javafx-bridge.md) — How processors are invoked
- [Paths and Connections](paths-and-connections.md) — How animCurve output connections are traced
- [Mesh and Materials](mesh-and-materials.md) — Skinning animation via `SkinningMesh`