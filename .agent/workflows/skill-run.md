---
description: Execute a specific skill by name
---

# Skill Run

Execute a skill from the `.skills/` directory.

## Usage

```
/skill-run <skill-name> [parameters]
```

## Steps

1. Parse the skill name from the command
2. Search for the skill in these locations (in order):
   - `.skills/core/**/<skill-name>.md`
   - `.skills/domain/**/<skill-name>.md`
   - `.skills/workflows/**/<skill-name>.md`
3. If not found, check `.skills/registry/aliases.md` for alternative names
4. Load the skill file and read its contents
5. Follow the procedure defined in the skill
6. Apply the AI Context instructions
7. Generate output in the specified format

## Parameters

- `skill-name`: Name of the skill (kebab-case)
- Additional parameters are passed to the skill

## Examples

```
/skill-run generate-api-client
/skill-run convert-sql-paradox
/skill-run validate-dfm-integrity
```
