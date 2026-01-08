---
name: skill-aliases
version: 1.0.0
category: registry
created: 2026-01-08
description: Mapeo de aliases a skills canónicas
---

# Aliases de Skills

Este archivo define nombres alternativos para invocar skills.

## Formato

```yaml
alias: skill-canonica
```

## Aliases Activos

### Análisis

| Alias | Skill Real |
| ----- | ---------- |
| `analizar-codigo` | `analyze-delphi-unit` |
| `revisar-codigo` | `analyze-delphi-unit` |
| `code-review` | `analyze-delphi-unit` |
| `validar-proyecto` | `analyze-project-structure` |
| `estructura` | `analyze-project-structure` |

### Base de Datos

| Alias | Skill Real |
| ----- | ---------- |
| `sql-paradox` | `convert-sql-paradox` |
| `convertir-query` | `convert-sql-paradox` |
| `tabla-corrupta` | `validate-paradox-table` |
| `reparar-paradox` | `validate-paradox-table` |
| `migracion-bd` | `create-database-migration` |

### Generación

| Alias | Skill Real |
| ----- | ---------- |
| `crear-dto` | `generate-json-dto` |
| `json-dto` | `generate-json-dto` |
| `cliente-api` | `generate-api-client` |
| `rest-client` | `generate-api-client` |
| `boilerplate` | `generate-boilerplate` |

### Verifactu

| Alias | Skill Real |
| ----- | ---------- |
| `xml-factura` | `generate-verifactu-xml` |
| `factura-aeat` | `generate-verifactu-xml` |
| `validar-verifactu` | `validate-verifactu-implementation` |

### Impresión

| Alias | Skill Real |
| ----- | ---------- |
| `imprimir` | `generate-gmprint-invoice` |
| `factura-pdf` | `generate-gmprint-invoice` |
| `ticket` | `generate-gmprint-invoice` |

### Formularios

| Alias | Skill Real |
| ----- | ---------- |
| `dfm-roto` | `validate-dfm-integrity` |
| `reparar-dfm` | `validate-dfm-integrity` |
| `form-corrupto` | `validate-dfm-integrity` |

---

## Uso por la IA

Cuando el usuario use un alias, la IA debe:

1. Reconocer el alias
2. Cargar la skill canónica
3. Ejecutar con el contexto proporcionado

```text
Usuario: "Usa la skill sql-paradox para convertir esta query"
IA: (internamente carga convert-sql-paradox.md)
```
