---
description: Update specs from source code analysis
---

# Spec Update

Analyze source code and update specification files.

## Usage

```text
/spec-update [mode] [--project=name]
```

## Modes

- **default** (no argument): Incremental update, detect changes since last update
- **full**: Complete rescan, regenerate all specs
- **--project=NAME**: Specify project to analyze

## Steps

1. Read current specs from `.skills/specs/`
2. Based on mode:
   - **Incremental**: Compare file dates, analyze only modified files
   - **Full**: Scan all `.dpr`, `.pas`, `.dfm` files
3. Extract:
   - Units from .dpr
   - Tables from DataModules
   - Dependencies from uses clauses
4. Compare with existing specs
5. Generate delta of changes
6. Update affected spec files
7. Report changes to user

## Output

```markdown
## Specs Actualizados

### data-schema.yaml
- ✅ Añadida tabla CONTRATOS
- ⚠️ Modificado campo CLIENTES.EMAIL

### dependencies.md
- ✅ Añadido mormot.net.async

Última actualización: [timestamp]
```

## Examples

```text
/spec-update                    # Incremental
/spec-update full               # Full rescan
/spec-update --project=FACARAVF # Specific project
```
