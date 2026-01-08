---
description: Execute a pipeline of multiple skills
---

# Skill Pipeline

Execute a predefined pipeline that combines multiple skills.

## Usage

```
/skill-pipeline <pipeline-name> [parameters]
```

## Available Pipelines

See `.skills/workflows/development/skill-composer.md` for pipeline definitions.

### Predefined Pipelines

1. **nueva-funcionalidad**: Create a new feature
   - analyze-project-structure → generate-boilerplate → implement-design-pattern → generate-unit-tests

2. **integracion-api**: API integration workflow
   - generate-json-dto → generate-api-client → validate-dfm-integrity

3. **migracion-verifactu**: Verifactu compliance
   - analyze-delphi-unit → validate-verifactu-implementation → generate-verifactu-xml → generate-gmprint-invoice

4. **mantenimiento-bd**: Database maintenance
   - validate-paradox-table → create-database-migration → convert-sql-paradox

## Steps

1. Parse pipeline name from command
2. Load pipeline definition from skill-composer.md
3. Execute each skill in sequence
4. Pass outputs between skills as needed
5. Report progress after each step
6. Summarize results at completion

## Example

```
/skill-pipeline nueva-funcionalidad --name=Proveedores
```
