# Parser

[← Back to index](README.md)

`MParser` (`parser/MParser.java`) reads Maya ASCII (`.ma`) files and populates an `MEnv` scene database by dispatching MEL commands.

## Responsibilities

1. **Read input** — From a `URL` (with optional `file` command references) or `InputStream`
2. **Tokenize** — Split lines into MEL tokens, handling quoted strings and semicolon-terminated commands
3. **Dispatch** — Execute recognized commands against the `MEnv`
4. **Track state** — Maintain the currently selected node, playback script context, and referenced files

## Parsing Flow

```
BufferedReader.readLine()
        │
        ▼
   Tokenizer (per line)
        │
        ▼
   Accumulate tokens until ';'
        │
        ▼
   execute(commandArgs)
        │
        ▼
   switch(command) → doCreateNode / doSetAttr / ...
```

Commands can span multiple tokens on a single line. A semicolon (`;`) terminates a command and triggers execution.

## Tokenizer

`MParser` contains an inner `Tokenizer` class that handles Maya-specific lexical rules:

- **Whitespace** separates tokens outside of quoted strings
- **Quoted strings** (`"..."`) become single tokens; interior spaces are preserved
- **Backslash escaping** is tracked but not fully implemented for string contents
- **Comments** (`//`) skip the remainder of the line
- **Semicolons** end the current command (the semicolon itself is not included in the last token)

## Supported Commands

| Command | Handler | Description |
|---------|---------|-------------|
| `createNode` | `doCreateNode` | Instantiate a node of a given type with optional name, parent, shared flag |
| `setAttr` | `doSetAttr` | Set attribute values on the selected node (or playback range via script node) |
| `addAttr` | `doAddAttr` | Add a dynamic attribute to the selected node |
| `connectAttr` | `doConnectAttr` | Wire an output attribute to an input attribute |
| `parent` | `doParent` | Establish DAG parent-child relationships |
| `select` | `doSelect` | Set the currently selected node for subsequent `setAttr`/`addAttr` |
| `file` | `doFile` | Recursively parse a referenced `.ma` file |
| `requires` | `doRequires` | No-op (plugin requirements ignored) |
| `disconnectAttr` | `doDisconnectAttr` | No-op |
| `currentUnit` | `doCurrentUnit` | No-op |
| `fileInfo` | `doFileInfo` | No-op |
| *(other)* | default | Logged as unrecognized, skipped |

## Command Details

### `createNode`

```
createNode -n "pCube1" -p "group1" mesh;
```

Arguments parsed:

| Flag | Meaning |
|------|---------|
| `-n` / `-name` | Node name |
| `-p` / `-parent` | Parent node name |
| `-s` / `-shared` | Skip creation if a node with this name already exists |
| `-ss` / `-skipSelect` | Do not auto-select the new node |

The node type is the first non-flag token (e.g., `mesh`, `transform`, `joint`).

Special cases:

- **`script` nodes** set `inPlaybackScriptNode = true`; no `MNode` is created. Subsequent `setAttr .b` commands parse playback range options.
- **Unknown types** — If `env.findNodeType(type)` returns null, `createNode` returns null and selection is cleared.

### `setAttr`

```
setAttr ".vt[0:2]" -type "float3" 1 2 3 4 5 6;
```

The parser:

1. Identifies the target attribute (first non-flag argument, typically starting with `.`)
2. Collects remaining tokens as values
3. Resolves the attribute on `selectedNode` via `getAttr(target.substring(1))`
4. Optionally sets array size via `-s` / `-size`
5. Calls `data.parse(value)` to populate the `MData` object

If no node is selected, the command is silently ignored.

**Playback script handling:** When inside a playback script node, `setAttr .b` parses `playbackOptions` to extract `-min` and `-max` frame values and calls `env.setPlaybackRange(min, max)`.

### `addAttr`

```
addAttr -ln "myAttr" -sn "ma" -dt "double" -dv 1.0;
```

Adds a dynamic attribute to the selected node. Type aliases are normalized (e.g., `doubleAngle` → `double`, `fltMatrix` → `matrix`). If `-dv` (default value) is present, the value is parsed immediately.

### `connectAttr`

```
connectAttr "pCube1.tx" "pCube2.tx";
```

1. Strips leading `:` from source paths (namespace prefix)
2. Creates `MPath` objects for source and target
3. Calls `env.connectAttr(src, dst)`
4. If either endpoint is an `MPointer` (Message type), sets bidirectional target references

### `parent`

```
parent -add "instance1" "source1";
```

Links `instance1` as a child of the node resolved from `source1` via `MPath`.

### `file`

```
file -import -mergeNamespaces -options "v=0;" "referenced.ma";
```

Recursively parses referenced `.ma` files relative to the current input URL. A `HashSet<String> refs` prevents duplicate parsing of the same reference.

### `select`

```
select -r "pCube1";
```

Sets `selectedNode` to the node resolved from the first non-flag path argument.

## Error Handling

Errors are thrown as `RuntimeException` with context:

```
<error message>, current command <command>, file <url>, line <lineNo>
```

Parse failures in `setAttr` value parsing are caught and silently ignored (empty catch block).

## Relationship to Other Components

```
MParser
   │ uses
   ├── MEnv          (scene database mutations)
   ├── MNode         (node creation, attribute access)
   ├── MPath         (path resolution for connectAttr, select, parent)
   └── MData         (value parsing via setAttr)
```

After parsing completes, `Loader` consumes the populated `MEnv`. The parser itself has no knowledge of JavaFX.

## Further Reading

- [Scene Graph Model](scene-graph.md) — What `createNode` and `setAttr` populate
- [Paths and Connections](paths-and-connections.md) — How `connectAttr` paths are resolved
- [Data Model](data-model.md) — How `MData.parse()` interprets value tokens