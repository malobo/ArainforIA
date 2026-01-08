---
description: View a specific spec file
---

# Spec View

Display the contents of a specification file.

## Usage

```text
/spec-view <spec-name>
```

## Available Specs

- **system-context**: Architecture overview
- **data-schema**: Database tables and fields
- **dependencies**: Components and libraries

## Steps

1. Map spec name to file:
   - system-context → `.skills/specs/system-context.md`
   - data-schema → `.skills/specs/data-schema.yaml`
   - dependencies → `.skills/specs/dependencies.md`
2. Read and display file contents
3. Highlight key sections

## Examples

```text
/spec-view data-schema      # Show database schema
/spec-view dependencies     # Show component inventory
/spec-view system-context   # Show architecture
```
