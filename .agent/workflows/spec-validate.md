---
description: Validate specs consistency with source code
---

# Spec Validate

Check if specifications are synchronized with source code without modifying them.

## Usage

```text
/spec-validate [--fix]
```

## Options

- **--fix**: Automatically fix issues (runs /spec-update)

## Steps

1. Read all specs from `.skills/specs/`
2. Scan source files (.dpr, .pas, .dfm)
3. Compare:
   - Tables in code vs data-schema.yaml
   - Dependencies in uses vs dependencies.md
   - Architecture patterns vs system-context.md
4. Report discrepancies
5. If --fix, run /spec-update

## Output

```markdown
## Validación de Specs

### data-schema.yaml
✅ 12 tablas documentadas
⚠️ 2 tablas no documentadas: CONTRATOS, TARIFAS
❌ 1 tabla obsoleta: TEMP_IMPORT

### dependencies.md
✅ 8 dependencias verificadas
⚠️ Falta: mormot.net.async

### system-context.md
✅ Arquitectura coherente

---
Estado: ⚠️ Desincronizado
Ejecuta `/spec-update` para corregir
```

## Status Icons

- ✅ Synchronized
- ⚠️ Missing items (code has items not in specs)
- ❌ Obsolete items (specs have items not in code)
