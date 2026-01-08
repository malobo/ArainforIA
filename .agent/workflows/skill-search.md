---
description: Search for skills by keyword or category
---

# Skill Search

Search for skills matching a query.

## Usage

```
/skill-search <query>
```

## Steps

1. Read `.skills/INDEX.md` and `.skills/registry/aliases.md`
2. Search for matches in:
   - Skill names
   - Triggers
   - Descriptions
   - Aliases
3. Rank results by relevance
4. Present top matches with context

## Output Format

```
## Resultados para "<query>"

1. **skill-name** (category)
   - Triggers: matching-trigger, other-trigger
   - Match: [where the query matched]

2. **another-skill** (category)
   - Triggers: ...
```
