# Estructura del Sistema de Skills

## 📁 Árbol de Directorios Completo

```
.skills/
│
├── 📄 README.md                          # Visión general del sistema
├── 📄 GUIDELINES.md                      # Guías de creación de skills
├── 📄 QUICKSTART.md                      # Inicio rápido
├── 📄 INDEX.md                           # Índice completo de skills
├── 📄 CHANGELOG.md                       # Historial de cambios
├── 📄 STRUCTURE.md                       # Este archivo
├── 📄 WELCOME.md                         # Bienvenida e introducción
├── 📄 AI_GUIDE.md                        # Guía para asistentes de IA
├── 📄 .gitignore                         # Archivos ignorados por git
│
├── 📁 core/                              # ★ Skills fundamentales (13 skills)
│   │
│   ├── 📁 analysis/                      # Análisis (3 skills)
│   │   ├── 📄 README.md
│   │   ├── 📄 validate-skill-format.md   ✅ v1.1.0 - Validar formato
│   │   ├── 📄 sync-skills-registry.md    ✅ v1.1.0 - Sincronizar registro
│   │   └── 📄 validate-system-health.md  ✅ v1.0.0 - Diagnóstico del sistema
│   │
│   ├── 📁 generation/                    # Generación (2 skills)
│   │   ├── 📄 README.md
│   │   ├── 📄 generate-boilerplate.md    ✅ v1.1.0 - Generar código
│   │   └── 📄 generate-unit-tests.md     ✅ v1.0.0 - Generar tests
│   │
│   ├── 📁 refactoring/                   # Refactorización (1 skill)
│   │   ├── 📄 README.md
│   │   └── 📄 extract-method.md          ✅ v1.1.0 - Extraer método
│   │
│   ├── 📁 documentation/                 # Documentación (1 skill)
│   │   ├── 📄 README.md
│   │   └── 📄 generate-readme.md         ✅ v1.1.0 - Generar README
│   │
│   └── 📁 integration/                   # Integración (6 skills)
│       ├── 📄 README.md
│       ├── 📄 sync-notion-skills.md      ✅ v1.0.0 - Sync Notion
│       ├── 📄 log-development-activity.md ✅ v1.0.0 - Log actividad
│       ├── 📄 create-notion-issue.md     ✅ v1.0.0 - Crear issue
│       ├── 📄 sync-project-docs.md       ✅ v1.0.0 - Sync docs
│       ├── 📄 query-notion-knowledge.md  ✅ v1.0.0 - Consultar Notion
│       └── 📄 update-task-status.md      ✅ v1.0.0 - Actualizar tarea
│
├── 📁 domain/                            # ★ Skills del dominio (12 skills)
│   │
│   ├── 📁 delphi/                        # Delphi (7 skills)
│   │   ├── 📁 contexts/                  # Contextos expertos
│   │   │   ├── 📄 delphi-core-context.md ✅ v1.1.0
│   │   │   ├── 📄 delphi-vcl-context.md  ✅ v1.1.0
│   │   │   └── 📄 delphi-db-context.md   ✅ v1.1.0
│   │   ├── 📄 analyze-delphi-unit.md     ✅ v1.1.0 - Análisis unidades
│   │   ├── 📄 delphi-expert-context.md   ✅ v1.1.0 - Router contextos
│   │   ├── 📄 implement-design-pattern.md ✅ v1.0.0 - Patrones diseño
│   │   └── 📄 debug-memory-leak.md       ✅ v1.0.0 - Debug leaks
│   │
│   ├── 📁 database/                      # Base de datos (2 skills)
│   │   ├── 📄 create-database-migration.md ✅ v1.0.0 - Migraciones
│   │   └── 📄 generate-crud-forms.md     ✅ v1.0.0 - Generar CRUD
│   │
│   └── 📁 verifactu/                     # Verifactu (3 skills)
│       ├── 📁 contexts/
│       │   └── 📄 delphi-verifactu-context.md ✅ v1.1.0
│       ├── 📄 validate-verifactu-implementation.md ✅ v1.0.0 - Validación
│       └── 📄 verifactu-expert-context.md ⚠️ v1.0.0 - Deprecated
│
├── 📁 workflows/                         # ★ Flujos de trabajo (2 workflows)
│   │
│   ├── 📁 development/                   # Desarrollo (1 workflow)
│   │   └── 📄 full-feature-development.md ✅ v1.0.0 - Feature completa
│   │
│   ├── 📁 deployment/                    # Despliegue (1 workflow)
│   │   ├── 📄 README.md
│   │   └── 📄 deploy-verifactu-update.md ✅ v1.1.0 - Despliegue completo
│   │
│   ├── 📁 testing/                       # Testing (En proceso)
│   │   ├── 📄 README.md
│   │   └── 📄 run-test-suite.md          🔨 v0.1.0 - Ejecutar tests
│   │
│   └── 📁 migration/                     # Migración (0 workflows)
│       └── 📄 README.md
│
├── 📁 templates/                         # ★ Plantillas
│   ├── 📄 skill-template.md              # Plantilla para skills
│   └── 📄 workflow-template.md           # Plantilla para workflows
│
└── 📁 registry/                          # ★ Registro centralizado
    ├── 📄 index.json                     # Índice JSON maestro
    └── 📁 metadata/                      # Metadatos individuales
        └── [27 archivos .json]
```

## 📊 Estadísticas del Sistema

### Conteo de Archivos

| Categoría | Archivos | Tamaño Aprox. |
|-----------|----------|---------------|
| Documentación principal | 9 | ~60 KB |
| Skills core | 13 | ~80 KB |
| Skills domain | 12 | ~100 KB |
| Workflows | 2 | ~30 KB |
| Plantillas | 2 | ~10 KB |
| Metadatos JSON | 27 | ~25 KB |
| **TOTAL** | **~65** | **~305 KB** |

### Distribución de Skills

```
┌─────────────────────────────────────────────────────────┐
│  DISTRIBUCIÓN DE SKILLS (Total: 27)                     │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  Core (13)       ████████████████████░░░░░░  48%       │
│  ├─ analysis     █████░░░░░░░░░░░░░░░░░░░░  11%       │
│  ├─ generation   ███░░░░░░░░░░░░░░░░░░░░░░   7%       │
│  ├─ refactoring  ██░░░░░░░░░░░░░░░░░░░░░░░   4%       │
│  ├─ documentation██░░░░░░░░░░░░░░░░░░░░░░░   4%       │
│  └─ integration  ██████████░░░░░░░░░░░░░░░  22%       │
│                                                         │
│  Domain (12)     ██████████████████░░░░░░░░  44%       │
│  ├─ delphi       ███████████░░░░░░░░░░░░░░  26%       │
│  ├─ database     ███░░░░░░░░░░░░░░░░░░░░░░   7%       │
│  └─ verifactu    █████░░░░░░░░░░░░░░░░░░░░  11%       │
│                                                         │
│  Workflows (2)   ███░░░░░░░░░░░░░░░░░░░░░░   7%       │
│  ├─ development  ██░░░░░░░░░░░░░░░░░░░░░░░   4%       │
│  └─ deployment   ██░░░░░░░░░░░░░░░░░░░░░░░   4%       │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

## 🎯 Cobertura del Sistema

### Subcategorías Cubiertas

| Subcategoría | Estado | Skills |
|--------------|--------|--------|
| core/analysis | ✅ Cubierta | 3 |
| core/generation | ✅ Cubierta | 2 |
| core/refactoring | ✅ Cubierta | 1 |
| core/documentation | ✅ Cubierta | 1 |
| core/integration | ✅ Cubierta | 6 |
| domain/delphi | ✅ Cubierta | 7 |
| domain/database | ✅ Cubierta | 2 |
| domain/verifactu | ✅ Cubierta | 3 |
| workflows/development| ✅ Cubierta | 1 |
| workflows/deployment | ✅ Cubierta | 1 |
| workflows/testing | 🔨 Proceso | 1 |
| workflows/migration | 🔨 Pendiente | 0 |

**Cobertura Total**: 10/12 subcategorías (83%)
