# Índice de Skills de Arainfor

**Sistema Next-Gen (Context Enhanced)**  
**Versión 1.9.0 - 2026-01-08**

---

## Core (Fundamentales) · 18 skills

### Conectores Críticos

- ✨ [Delphi RAG Connector](core/delphi_rag.md) 🔴 **CRÍTICO**
  - *Triggers: `como hago en delphi`, `componente desconocido`, `error de compilacion`*
- ✨ [Notion MCP Connector](core/notion.md) 🔴 **CRÍTICO**
  - *Triggers: `objetivo del proyecto`, `memoria del proyecto`, `roadmap`*
- ✨ [Convenciones Delphi](core/delphi.md) **ALTA**
  - *Triggers: `escribe codigo delphi`, `crear unidad pas`, `refactorizar delphi`*
- ✨ [Normativa VERIFACTU](core/verifactu.md) **ALTA**
  - *Triggers: `verifactu`, `ley antifraude`, `encadenamiento facturas`*
- ✨ [mORMot 2 Framework](core/mormot.md) **ALTA**
  - *Triggers: `mormot`, `firmar factura`, `verifactu signature`*

### Análisis · 5 skills

- [analyze-project-structure](core/analysis/analyze-project-structure.md) - Mapear dependencias
- [sync-skills-registry](core/analysis/sync-skills-registry.md) - Sincronizar registro
- [validate-dfm-integrity](core/analysis/validate-dfm-integrity.md) - Validar DFM
- [validate-skill-format](core/analysis/validate-skill-format.md) - Validar formato skill
- [validate-system-health](core/analysis/validate-system-health.md) - Salud del sistema

### Generación · 4 skills

- [generate-boilerplate](core/generation/generate-boilerplate.md) - Código base
- [generate-json-dto](core/generation/generate-json-dto.md) - DTOs para JSON
- [generate-readme](core/documentation/generate-readme.md) - Documentación README
- [generate-unit-tests](core/generation/generate-unit-tests.md) - Tests unitarios

### Refactoring · 2 skills

- [extract-method](core/refactoring/extract-method.md) - Extraer método
- [refactor-to-mvp](core/refactoring/refactor-to-mvp.md) - Separar lógica (MVP)

### Integración Notion · 6 skills

- [create-notion-issue](core/integration/create-notion-issue.md)
- [log-development-activity](core/integration/log-development-activity.md)
- [query-notion-knowledge](core/integration/query-notion-knowledge.md)
- [sync-notion-skills](core/integration/sync-notion-skills.md)
- [sync-project-docs](core/integration/sync-project-docs.md)
- [update-task-status](core/integration/update-task-status.md)

---

## Domain (Específicas) · 22 skills

### Delphi · 10 skills

- [analyze-delphi-unit](domain/delphi/analyze-delphi-unit.md) - Analizar unidad
- [components-inventory](domain/delphi/components-inventory.md) - Inventario componentes
- [create-rest-endpoint](domain/delphi/create-rest-endpoint.md) - Crear API REST
- [debug-memory-leak](domain/delphi/debug-memory-leak.md) - Fugas de memoria
- [debug-runtime-error](domain/delphi/debug-runtime-error.md) 🆕 - Errores runtime
- [generate-api-client](domain/delphi/generate-api-client.md) - Cliente REST
- [generate-exception-handler](domain/delphi/generate-exception-handler.md) 🆕 - Try/Except
- [generate-singleton](domain/delphi/generate-singleton.md) 🆕 - Patrón Singleton
- [implement-design-pattern](domain/delphi/implement-design-pattern.md) - Patrones diseño
- [optimize-vcl-form](domain/delphi/optimize-vcl-form.md) - Optimizar formularios

### Database · 9 skills

- [convert-sql-paradox](domain/database/convert-sql-paradox.md) - SQL a Paradox
- [create-database-migration](domain/database/create-database-migration.md) - Migraciones
- [debug-connection-issue](domain/database/debug-connection-issue.md) - Conexiones
- [generate-data-export](domain/database/generate-data-export.md) - Exportar datos
- [generate-gmprint-invoice](domain/database/generate-gmprint-invoice.md) - Facturas
- [generate-report-template](domain/database/generate-report-template.md) - Informes
- [generate-stored-procedure](domain/database/generate-stored-procedure.md) 🆕 - SP/Triggers
- [implement-audit-trail](domain/database/implement-audit-trail.md) - Auditoría
- [validate-paradox-table](domain/database/validate-paradox-table.md) - Reparar Paradox

### Verifactu · 3 skills

- [generate-verifactu-xml](domain/verifactu/generate-verifactu-xml.md) - XML AEAT
- [validate-nif-cif](domain/verifactu/validate-nif-cif.md) 🆕 - Validar NIF/CIF
- [validate-verifactu-implementation](domain/verifactu/validate-verifactu-implementation.md) - Cumplimiento

### Hardware · 1 skill

- [zksdk-biometrico](domain/hardware/zksdk-biometrico.md) 🆕 - Lectores huella ZKTeco

---

## Proyectos · 3 skills

- ✨ [ARAFAC / Aracostes](projects/arafac.md) **ALTA** - Facturación Verifactu
- ✨ [ERPW](projects/erpw.md) **ALTA** - ERP Central
- ✨ [TPVARA](projects/tpvara.md) **ALTA** - Terminal Punto de Venta

---

## Workflows · 6 skills

### Compilación

- [compilation](workflows/compilation.md) - Build Delphi

### Desarrollo · 2 skills

- [skill-composer](workflows/development/skill-composer.md) - Pipelines
- [sync-mobile-data](workflows/development/sync-mobile-data.md) - Sync móvil

### Migración

- [migrate-bde-firedac](workflows/migration/migrate-bde-firedac.md) - BDE → FireDAC

### Testing · 1 skill

- [generate-unit-test](workflows/testing/generate-unit-test.md) 🆕 - DUnit tests

---

## Infraestructura

- [Aliases](registry/aliases.md) - Nombres alternativos
- [Templates](templates/) - Plantillas para nuevas skills

---

**Total: 50+ skills** | [CHANGELOG](CHANGELOG.md) | [AGENTES.md](../AGENTES.md)
