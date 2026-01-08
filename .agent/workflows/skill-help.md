---
description: Show help and documentation for a specific skill
---

# Skill Help

Display documentation and usage examples for a skill.

## Usage

```
/skill-help <skill-name>
```

## Steps

1. Find the skill file by name
2. Read and parse the skill contents
3. Extract key sections:
   - Description
   - Triggers
   - Inputs/Outputs
   - Procedure summary
   - Examples
4. Present in a concise format

## Output Format

```
## <Skill Name>

**Descripción**: Brief description

**Triggers**: keyword1, keyword2, keyword3

**Categoría**: category/subcategory

**Complejidad**: X/10

### Uso Rápido
[First example from the skill]

### Más Información
[Link to full skill file]
```
