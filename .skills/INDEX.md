# Índice de Skills Disponibles

> **Última actualización**: 2026-01-07T12:30:00+01:00  
> **Total de skills**: 23 (13 core + 8 domain + 2 workflows)  
> **Versión del sistema**: 1.6.0

## 📊 Resumen por Categoría

| Categoría | Subcategoría | Skills | Estado |
| --------- | ------------ | ------ | ------ |
| Core | Analysis | 3 | ✅ Activo ⭐ |
| Core | Generation | 2 | ✅ Activo |
| Core | Refactoring | 1 | ✅ Activo |
| Core | Documentation | 1 | ✅ Activo |
| Core | Integration | 6 | ✅ Activo ⭐NUEVO |
| Domain | Delphi | 7 | ✅ Activo ⭐EXPANDIDO |
| Domain | Database | 2 | ✅ Activo |
| Domain | Verifactu | 3 | ✅ Activo |
| Workflows | Development | 1 | ✅ Activo ⭐NUEVO |
| Workflows | Deployment | 1 | ✅ Activo |
| Workflows | Testing | 1 | 🔨 En Proceso |

## 📈 Estadísticas

```text
┌─────────────────────────────────────────────────────────┐
│  SISTEMA DE SKILLS v1.6.0                               │
│  ████████████████████████████████ 100% OPERATIVO       │
│                                                         │
│  Total Skills: 27                                       │
│  ├── Core: 13                                           │
│  │   ├── analysis×3, generation×2, refactoring, docs    │
│  │   └── integration×6 (Notion Connectivity ⭐)         │
│  ├── Domain: 12 (delphi×7, database×2, verifactu×3)     │
│  └── Workflows: 2 (development, deployment)             │
│                                                         │
│  Complejidad:                                           │
│  ├── Baja (1-3): 10 skills                              │
│  ├── Media (4-6): 12 skills                             │
│  ├── Alta (7-9): 5 skills                               │
│                                                         │
│  Tokens Estimados Total: 18,000 - 30,000                │
│                                                         │
│  🔗 Notion: Conectado via MCP                           │
└─────────────────────────────────────────────────────────┘
```

---

## 🎯 Skills por Categoría

### Core - Analysis

#### 1. validate-skill-format ✅

**Ruta**: `core/analysis/validate-skill-format.md`  
**Versión**: 1.1.0 | **Complejidad**: 3/10 | **Tokens**: 400-600

**Descripción**: Valida que un archivo de skill sigue el formato estándar del sistema.

**Inputs**:

- `skill_path` (string, requerido): Ruta al archivo .md
- `strict_mode` (boolean, opcional): Modo estricto
- `auto_fix` (boolean, opcional): Sugerir correcciones

**Invocación rápida**:

```yaml
@skill:core/analysis/validate-skill-format
skill_path: ".skills/domain/delphi/analyze-delphi-unit.md"
```

#### 1b. sync-skills-registry ✅

**Ruta**: `core/analysis/sync-skills-registry.md`  
**Versión**: 1.1.0 | **Complejidad**: 2/10 | **Tokens**: 200-400

**Descripción**: Sincroniza automáticamente el índice de skills con los archivos reales del sistema.

**Inputs**:

- `skills_root` (string, opcional): Ruta raíz de skills
- `update_index` (boolean, opcional): Actualizar index.json
- `dry_run` (boolean, opcional): Solo mostrar cambios

**Invocación rápida**:

```yaml
@skill:core/analysis/sync-skills-registry
dry_run: true
```

#### 1c. validate-system-health ✅

**Ruta**: `core/analysis/validate-system-health.md`  
**Versión**: 1.0.0 | **Complejidad**: 4/10 | **Tokens**: 800-1200

**Descripción**: Realiza un diagnóstico completo del "Sistema de Skills", verificando registro, archivos y metadatos.

**Invocación rápida**:

```yaml
@skill:core/analysis/validate-system-health
verbose: true
```

---

### Core - Generation

#### 2. generate-boilerplate ✅

**Ruta**: `core/generation/generate-boilerplate.md`  
**Versión**: 1.1.0 | **Complejidad**: 4/10 | **Tokens**: 500-800

**Descripción**: Genera código boilerplate para diferentes tipos de componentes Delphi.

**Invocación rápida**:

```yaml
@skill:core/generation/generate-boilerplate
component_type: "class"
name: "TFacturaService"
```

#### 2b. generate-unit-tests ✅

**Ruta**: `core/generation/generate-unit-tests.md`  
**Versión**: 1.0.0 | **Complejidad**: 5/10 | **Tokens**: 1000-1500

**Descripción**: Crea esqueletos de tests unitarios (DUnit/DUnitX) para clases existentes.

---

### Core - Integration (Notion) ⭐ NUEVO

#### 3. sync-notion-skills ✅

**Ruta**: `core/integration/sync-notion-skills.md`  
**Versión**: 1.0.0 | **Complejidad**: 5/10 | **Tokens**: 800-1200

**Descripción**: Sincroniza la documentación de skills local con una base de datos de Notion.

#### 3b. log-development-activity ✅

**Ruta**: `core/integration/log-development-activity.md`  
**Versión**: 1.0.0 | **Complejidad**: 3/10 | **Tokens**: 300-500

**Descripción**: Registra automáticamente cambios y progreso en el diario de desarrollo de Notion.

#### 3c. create-notion-issue ✅

**Ruta**: `core/integration/create-notion-issue.md`  
**Versión**: 1.0.0 | **Complejidad**: 2/10 | **Tokens**: 300-500

**Descripción**: Crea un ticket o issue en la base de datos de tareas de Notion desde el editor.

---

### Domain - Delphi

#### 4. delphi-expert-context (Router) ✅

**Ruta**: `domain/delphi/delphi-expert-context.md`  
**Versión**: 1.1.0 | **Complejidad**: 1/10 | **Tokens**: 400-600

**Descripción**: Punto de entrada maestro para el conocimiento experto en Delphi.

#### 4b. implement-design-pattern ✅

**Ruta**: `domain/delphi/implement-design-pattern.md`  
**Versión**: 1.0.0 | **Complejidad**: 5/10 | **Tokens**: 800-1500

**Descripción**: Implementa patrones de diseño GoF idiomáticos para Delphi.

#### 4c. debug-memory-leak ✅

**Ruta**: `domain/delphi/debug-memory-leak.md`  
**Versión**: 1.0.0 | **Complejidad**: 7/10 | **Tokens**: 1500-2000

**Descripción**: Guía paso a paso y herramientas para detectar y corregir fugas de memoria en Delphi.

---

### Domain - Database

#### 5. create-database-migration ✅

**Ruta**: `domain/database/create-database-migration.md`  
**Versión**: 1.0.0 | **Complejidad**: 6/10 | **Tokens**: 600-1000

**Descripción**: Genera scripts de migración para bases de datos Paradox con versionamiento.

#### 5b. generate-crud-forms ✅

**Ruta**: `domain/database/generate-crud-forms.md`  
**Versión**: 1.0.0 | **Complejidad**: 6/10 | **Tokens**: 1200-1800

**Descripción**: Genera formularios de mantenimiento (CRUD) conectados a tablas específicas.

---

### Domain - Verifactu

#### 6. validate-verifactu-implementation ✅

**Ruta**: `domain/verifactu/validate-verifactu-implementation.md`  
**Versión**: 1.0.0 | **Complejidad**: 7/10 | **Tokens**: 1000-1500

**Descripción**: Valida que la implementación Verifactu cumple con RD 1007/2023.

---

### Workflows - Development

#### 7. full-feature-development ✅

**Ruta**: `workflows/development/full-feature-development.md`  
**Versión**: 1.0.0 | **Complejidad**: 9/10 | **Tokens**: 3000-5000

**Descripción**: Orquesta todo el ciclo de vida de una nueva feature: requisitos, diseño, tests, impl, y docs.

---

### Workflows - Deployment

#### 8. deploy-verifactu-update ✅

**Ruta**: `workflows/deployment/deploy-verifactu-update.md`  
**Versión**: 1.1.0 | **Complejidad**: 8/10 | **Tokens**: 2000-3000

**Descripción**: Workflow completo para desplegar actualizaciones críticas de Verifactu.

---

## 🏷️ Índice por Tags

| Tag | Skills |
| --- | ------ |
| **analysis** | validate-skill-format, analyze-delphi-unit, validate-system-health |
| **delphi** | analyze-delphi-unit, generate-boilerplate, implement-design-pattern |
| **notion** | sync-notion-skills, log-development-activity, create-notion-issue |
| **testing** | generate-unit-tests |
| **refactoring** | extract-method, debug-memory-leak |
| **database** | create-database-migration, generate-crud-forms |
| **verifactu** | validate-verifactu-implementation, deploy-verifactu-update |

---

## 🔄 Sincronización

Este índice se sincroniza automáticamente con:

- `registry/index.json` - Índice JSON programático
- `registry/metadata/*.json` - Metadatos individuales

Para validar sincronización:

```yaml
@skill:core/analysis/validate-system-health
```

---

**Versión del índice**: 1.6.0  
**Última actualización**: 2026-01-07T13:30:00+01:00  
**Skills activas**: 27 | **Skills Totales en File System**: ~65 archivos
