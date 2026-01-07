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
| Core | Integration | 6 | ✅ Activo |
| Domain | Delphi | 4 | ✅ Activo |
| Domain | Database | 2 | ✅ Activo |
| Domain | Verifactu | 2 | ✅ Activo ⭐NUEVO |
| Workflows | Development | 1 | ✅ Activo ⭐NUEVO |
| Workflows | Deployment | 1 | ✅ Activo |
| Workflows | Testing | 0 | 🔨 Planificado |

## 📈 Estadísticas

```text
┌─────────────────────────────────────────────────────────┐
│  SISTEMA DE SKILLS v1.6.0                               │
│  ████████████████████████████████ 100% OPERATIVO       │
│                                                         │
│  Total Skills: 23                                       │
│  ├── Core: 13                                           │
│  │   ├── analysis×3 ⭐, generation×2, refactoring, docs    │
│  │   └── integration×6                                    │
│  ├── Domain: 8 (delphi×4, database×2, verifactu×2 ⭐)     │
│  └── Workflows: 2 (development, deployment)             │
│                                                         │
│  Complejidad:                                           │
│  ├── Baja (1-3): 8 skills                              │
│  ├── Media (4-6): 10 skills                            │
│  └── Alta (7-9): 5 skills                              │
│                                                         │
│  Tokens Estimados Total: 14,000 - 23,000                │
│                                                         │
│  🔗 Notion: Conectado via MCP                           │
└─────────────────────────────────────────────────────────┘
```

---

## 🎯 Skills por Categoría

### Core - Analysis

#### 1. validate-skill-format ✅

**Ruta**: `core/analysis/validate-skill-format.md`  
**Versión**: 1.0.0 | **Complejidad**: 3/10 | **Tokens**: 400-600

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
**Versión**: 1.0.0 | **Complejidad**: 2/10 | **Tokens**: 200-400

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

---

### Core - Generation

#### 2. generate-boilerplate ✅

**Ruta**: `core/generation/generate-boilerplate.md`  
**Versión**: 1.0.0 | **Complejidad**: 4/10 | **Tokens**: 500-800

**Descripción**: Genera código boilerplate para diferentes tipos de componentes Delphi.

**Inputs**:

- `component_type` (string, requerido): [class|unit|form|procedure|interface|record]
- `name` (string, requerido): Nombre del componente
- `language` (string, opcional): [delphi|pascal]
- `options` (object, opcional): Opciones de generación

**Invocación rápida**:

```yaml
@skill:core/generation/generate-boilerplate
component_type: "class"
name: "TFacturaService"
```

---

### Core - Refactoring

#### 3. extract-method ✅

**Ruta**: `core/refactoring/extract-method.md`  
**Versión**: 1.0.0 | **Complejidad**: 5/10 | **Tokens**: 600-900

**Descripción**: Refactoriza código extrayendo un bloque a un nuevo método.

**Inputs**:

- `source_code` (string, requerido): Código fuente
- `selection_start` (integer, requerido): Línea inicio
- `selection_end` (integer, requerido): Línea fin
- `new_method_name` (string, requerido): Nombre del nuevo método
- `target_visibility` (string, opcional): [private|protected|public]

**Invocación rápida**:

```yaml
@skill:core/refactoring/extract-method
source_code: "..."
selection_start: 10
selection_end: 25
new_method_name: "ValidarDatos"
```

---

### Core - Documentation

#### 4. generate-readme ✅

**Ruta**: `core/documentation/generate-readme.md`  
**Versión**: 1.0.0 | **Complejidad**: 3/10 | **Tokens**: 400-600

**Descripción**: Genera README.md profesional para proyectos analizando su estructura.

**Inputs**:

- `project_path` (string, requerido): Ruta al proyecto
- `template` (string, opcional): [minimal|standard|detailed|github]
- `language` (string, opcional): [es|en]
- `badges` (boolean, opcional): Incluir badges

**Invocación rápida**:

```yaml
@skill:core/documentation/generate-readme
project_path: "D:/ARAINFORIA/FACARAVF"
template: "standard"
```

---

### Domain - Delphi

#### 5. analyze-delphi-unit ✅

**Ruta**: `domain/delphi/analyze-delphi-unit.md`  
**Versión**: 1.0.0 | **Complejidad**: 4/10 | **Tokens**: 800-1200

**Descripción**: Analiza una unidad (.pas) de Delphi para identificar estructura, dependencias, complejidad y posibles mejoras.

**Inputs**:

- `unit_path` (string, requerido): Ruta al archivo .pas
- `depth` (string, opcional): [basic|detailed|deep]
- `focus` (array, opcional): [structure|dependencies|quality|security]

**Invocación rápida**:

```yaml
@skill:domain/delphi/analyze-delphi-unit
unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/uVerifactu.pas"
depth: "detailed"
```

---

### Domain - Database

#### 6. create-database-migration ✅

**Ruta**: `domain/database/create-database-migration.md`  
**Versión**: 1.0.0 | **Complejidad**: 6/10 | **Tokens**: 600-1000

**Descripción**: Genera scripts de migración para bases de datos Paradox con versionamiento y rollback.

**Inputs**:

- `migration_name` (string, requerido): Nombre de la migración
- `target_table` (string, requerido): Tabla a modificar
- `changes` (array, requerido): Lista de cambios
- `generate_rollback` (boolean, opcional): Generar rollback

**Invocación rápida**:

```yaml
@skill:domain/database/create-database-migration
migration_name: "add_verifactu_fields"
target_table: "Facturas"
changes: [{"type": "add_column", "name": "HashActual", "datatype": "CHAR(64)"}]
```

---

### Domain - Verifactu

#### 7. validate-verifactu-implementation ✅

**Ruta**: `domain/verifactu/validate-verifactu-implementation.md`  
**Versión**: 1.0.0 | **Complejidad**: 7/10 | **Tokens**: 1000-1500

**Descripción**: Valida que la implementación de Verifactu cumple con todos los requisitos del Real Decreto 1007/2023.

**Inputs**:

- `project_path` (string, requerido): Ruta al proyecto
- `validation_level` (string, opcional): [basic|standard|exhaustive]
- `generate_report` (boolean, opcional): Generar reporte

**Invocación rápida**:

```yaml
@skill:domain/verifactu/validate-verifactu-implementation
project_path: "D:/ARAINFORIA/FACARAVF"
validation_level: "exhaustive"
```

---

### Workflows - Deployment

#### 8. deploy-verifactu-update ✅

**Ruta**: `workflows/deployment/deploy-verifactu-update.md`  
**Versión**: 1.0.0 | **Complejidad**: 8/10 | **Tokens**: 2000-3000 | **Duración**: 30-45 min

**Descripción**: Workflow completo para desplegar actualizaciones relacionadas con Verifactu en producción de forma segura y controlada.

**Inputs**:

- `version` (string, requerido): Versión a desplegar
- `environment` (string, requerido): [development|staging|production]
- `executable_path` (string, requerido): Ruta al ejecutable
- `migration_scripts` (array, requerido): Scripts de migración
- `skip_backup` (boolean, opcional): Saltar backup (solo dev)

**Skills utilizadas**:

- `@skill:domain/database/backup-database`
- `@skill:domain/database/create-database-migration`
- `@skill:domain/verifactu/validate-verifactu-implementation`

**Invocación rápida**:

```yaml
@skill:workflows/deployment/deploy-verifactu-update
version: "2.1.0"
environment: "production"
executable_path: "D:/Build/FACARAVF_v2.1.0.exe"
```

---

## 🏷️ Índice por Tags

| Tag | Skills |
| --- | ------ |
| **analysis** | validate-skill-format, analyze-delphi-unit |
| **clean-code** | extract-method |
| **code-review** | analyze-delphi-unit |
| **compliance** | validate-verifactu-implementation |
| **database** | create-database-migration |
| **delphi** | analyze-delphi-unit, generate-boilerplate |
| **deployment** | deploy-verifactu-update |
| **documentation** | generate-readme |
| **generation** | generate-boilerplate |
| **markdown** | generate-readme |
| **meta** | validate-skill-format, sync-skills-registry |
| **maintenance** | sync-skills-registry |
| **migration** | create-database-migration |
| **production** | deploy-verifactu-update |
| **quality** | validate-skill-format |
| **refactoring** | extract-method |
| **solid** | extract-method |
| **validation** | validate-skill-format, validate-verifactu-implementation |
| **verifactu** | validate-verifactu-implementation, deploy-verifactu-update |

---

## 🔍 Búsqueda Rápida por Necesidad

| Necesito... | Skill Recomendada |
| ----------- | ----------------- |
| Analizar código Delphi | `domain/delphi/analyze-delphi-unit` |
| Generar código nuevo | `core/generation/generate-boilerplate` |
| Refactorizar código | `core/refactoring/extract-method` |
| Documentar proyecto | `core/documentation/generate-readme` |
| Validar una skill | `core/analysis/validate-skill-format` |
| Sincronizar registro | `core/analysis/sync-skills-registry` |
| Modificar base de datos | `domain/database/create-database-migration` |
| Validar Verifactu | `domain/verifactu/validate-verifactu-implementation` |
| Desplegar a producción | `workflows/deployment/deploy-verifactu-update` |

---

## 📋 Tabla de Complejidad

| Complejidad | Skill | Tokens |
| ----------- | ----- | ------ |
| ⭐⭐ (2) | sync-skills-registry | 200-400 |
| ⭐⭐⭐ (3) | validate-skill-format | 400-600 |
| ⭐⭐⭐ (3) | generate-readme | 400-600 |
| ⭐⭐⭐⭐ (4) | generate-boilerplate | 500-800 |
| ⭐⭐⭐⭐ (4) | analyze-delphi-unit | 800-1200 |
| ⭐⭐⭐⭐⭐ (5) | extract-method | 600-900 |
| ⭐⭐⭐⭐⭐⭐ (6) | create-database-migration | 600-1000 |
| ⭐⭐⭐⭐⭐⭐⭐ (7) | validate-verifactu-implementation | 1000-1500 |
| ⭐⭐⭐⭐⭐⭐⭐⭐ (8) | deploy-verifactu-update | 2000-3000 |

---

## 📚 Recursos

### Documentación

- [README](./README.md) - Visión general
- [QUICKSTART](./QUICKSTART.md) - Inicio rápido
- [GUIDELINES](./GUIDELINES.md) - Guías de creación
- [AI_GUIDE](./AI_GUIDE.md) - Guía para IAs
- [STRUCTURE](./STRUCTURE.md) - Estructura del sistema

### Plantillas

- [skill-template.md](./templates/skill-template.md)
- [workflow-template.md](./templates/workflow-template.md)

### Registro

- [index.json](./registry/index.json) - Índice JSON
- [metadata/](./registry/metadata/) - Metadatos individuales

---

## 🔄 Sincronización

Este índice se sincroniza automáticamente con:

- `registry/index.json` - Índice JSON programático
- `registry/metadata/*.json` - Metadatos individuales

Para validar sincronización:

```yaml
@skill:core/analysis/validate-skill-format
skill_path: "INDEX.md"
```

---

**Versión del índice**: 1.6.0  
**Última actualización**: 2026-01-07T12:30:00+01:00  
**Skills activas**: 23 | **Planificadas**: 1 | **Deprecated**: 0  
**Cobertura de categorías**: 100% (11/11 subcategorías con skills)
