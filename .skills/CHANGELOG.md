# Changelog - Sistema de Skills

Todos los cambios notables en el sistema de skills serán documentados en este archivo.

El formato está basado en [Keep a Changelog](https://keepachangelog.com/es-ES/1.0.0/),
y este proyecto adhiere a [Semantic Versioning](https://semver.org/lang/es/).

## [2.0.1] - 2026-01-08

### Mejorado

#### Automatización de Specs

- **update-specs v2.0**: 3 modos de operación (full, incremental, validate)
- Detección de cambios incremental (compara fechas)
- Formato de deltas para histórico de cambios

#### Nuevos Comandos Slash

- `/spec-update` - Actualizar specs desde código
- `/spec-validate` - Validar coherencia specs vs código
- `/spec-view <nombre>` - Ver contenido de spec

---

## [2.0.0] - 2026-01-08

### Añadido

#### Sistema de Especificaciones (Specs) - OpenSpec Integrado

- **specs/README.md**: Guía del sistema de especificaciones
- **specs/system-context.md**: Arquitectura y flujos del sistema
- **specs/data-schema.yaml**: Esquema de tablas y campos
- **specs/dependencies.md**: Componentes y dependencias

#### Sistema de Propuestas de Cambio

- **changes/README.md**: Flujo estructurado de cambios

#### Nueva Skill

- **update-specs** (core/analysis): Actualizar specs automáticamente desde código

### Cambiado

- **INDEX.md**: Añadida sección Specs, versión 2.0.0
- Metodología inspirada en [OpenSpec](https://github.com/Fission-AI/OpenSpec)

---

## [1.9.0] - 2026-01-08

### Añadido

#### Nuevas Skills (7)

- **validate-nif-cif** (verifactu): Validación local y AEAT de NIF/CIF
- **debug-runtime-error** (delphi): Diagnóstico Access Violation, etc.
- **generate-exception-handler** (delphi): Templates try/except/finally
- **generate-stored-procedure** (database): SP, funciones y triggers SQL
- **generate-singleton** (delphi): Patrón Singleton thread-safe
- **zksdk-biometrico** (hardware): Lectores huella ZKTeco
- **generate-unit-test** (testing): Tests DUnit

#### Nuevas Secciones

- **Hardware**: Skills para dispositivos físicos
- **Testing**: Workflows de pruebas

### Cambiado

- **INDEX.md**: Reorganizado con contadores por sección, orden alfabético
- **Total skills**: 50+ (de 45+)

---

## [1.8.1] - 2026-01-08

### Añadido (Mejoras OpenSpec)

- **AGENTS.md**: Guía para asistentes de IA para entender el sistema de skills automáticamente.
- **Comandos Slash** (.agent/workflows/):
  - `/skill-list` - Listar todas las skills
  - `/skill-run` - Ejecutar skill específica
  - `/skill-help` - Ver ayuda de skill
  - `/skill-search` - Buscar skills por palabra clave
  - `/skill-pipeline` - Ejecutar pipelines compuestos

### Inspiración

Mejoras basadas en [OpenSpec](https://github.com/Fission-AI/OpenSpec) - Spec-driven development para IAs.

---

## [1.8.0] - 2026-01-08

### Añadido

#### Fase 1: Skills Específicas del Stack

- **generate-verifactu-xml** (domain/verifactu): Generación de XML Verifactu para AEAT.
- **validate-paradox-table** (domain/database): Diagnóstico y reparación de tablas Paradox.
- **generate-gmprint-invoice** (domain/database): Templates de impresión con GmPrintSuite.
- **analyze-project-structure** (core/analysis): Mapeo de dependencias de proyectos.

#### Fase 2: Infraestructura

- **skill-aliases** (registry): Sistema de nombres alternativos para skills.
- **skill-composer** (workflows/development): Composición de skills en pipelines.

#### Fase 3: Skills Pendientes

- **generate-report-template** (domain/database): Plantillas de informes genéricos.
- **generate-data-export** (domain/database): Exportación a CSV, JSON, Excel.
- **implement-audit-trail** (domain/database): Sistema de auditoría de cambios.
- **migrate-bde-firedac** (workflows/migration): Workflow de migración BDE→FireDAC.
- **optimize-vcl-form** (domain/delphi): Optimización de formularios VCL.
- **refactor-to-mvp** (core/refactoring): Guía de refactorización a MVP.
- **create-rest-endpoint** (domain/delphi): Creación de APIs REST con mORMot2.
- **sync-mobile-data** (workflows/development): Sincronización móvil↔desktop.
- **debug-connection-issue** (domain/database): Diagnóstico de conexiones BD.

### Cambiado

- **INDEX.md**: Reorganizado con subsecciones por categoría.
- **Total skills**: 45+ (15 nuevas en esta versión).

---

## [1.7.0] - 2026-01-08

### Añadido

#### Nuevas Skills para Desarrollo Híbrido Delphi

- **convert-sql-paradox** (domain/database): Conversión de SQL estándar a Paradox/BDE.
  - Tabla de conversiones completa
  - Ejemplos prácticos con alternativas
  - Documentación de limitaciones

- **generate-api-client** (domain/delphi): Generación de clientes REST con mORMot2.
  - Templates para autenticación (API Key, OAuth2, Certificados)
  - Manejo de errores y reintentos
  - Ejemplo específico para AEAT

- **validate-dfm-integrity** (core/analysis): Validación de archivos DFM.
  - Checklist de validación completo
  - Solución de errores comunes
  - Script de limpieza automática

- **generate-json-dto** (core/generation): Generación de DTOs para JSON.
  - Templates con mORMot2 y System.JSON
  - Mapeo de tipos JSON → Delphi
  - Validación de DTOs

### Cambiado

- **INDEX.md**: Reorganizado con nueva sección Domain y subsecciones.
- **Versión**: Actualizada a 1.7.0.

---

## [1.6.0] - 2026-01-07

### Añadido

#### Nuevas Skills de Integración (Notion)

- **sync-notion-skills** (core/integration): Sincronización bidireccional de documentación.
- **log-development-activity**: Registro automático en bases de datos Notion.
- **create-notion-issue**: Creación rápida de tickets desde el entorno.
- **query-notion-knowledge**: Búsqueda semántica en base de conocimiento.

#### Nuevas Skills Core y Domain

- **validate-system-health** (core/analysis): Herramienta de autodiagnóstico.
- **generate-unit-tests** (core/generation): Scaffolding de tests DUnit.
- **implement-design-pattern** (domain/delphi): Implementación de patrones GoF.
- **debug-memory-leak** (domain/delphi): Guías de debugging avanzado.
- **generate-crud-forms** (domain/database): Generación de ABMs completos.

#### Workflows Avanzados

- **full-feature-development** (workflows/development): Ciclo completo de desarrollo.

### Cambiado

- **Estructura**: Reorganización de carpetas para incluir `core/integration`.
- **Registro**: Actualización masiva de metadatos (27 skills totales).
- **Documentación**: Sincronización completa de INDEX y STRUCTURE.

## [1.1.0] - 2026-01-07

#### Nuevas Skills Core

- **validate-skill-format** (core/analysis): Valida formato de skills
  - Verificación de frontmatter YAML
  - Validación de secciones requeridas
  - Sistema de scoring (0-100)
  - Modo estricto y auto-fix

- **generate-boilerplate** (core/generation): Genera código boilerplate Delphi
  - Soporte para class, unit, form, procedure, interface, record
  - Opciones de error handling, logging, comments
  - Templates personalizables

- **extract-method** (core/refactoring): Refactorización de código
  - Detección automática de parámetros
  - Soporte para procedures y functions
  - Generación de documentación XML
  - Análisis de tipos de retorno

- **generate-readme** (core/documentation): Generación de README
  - Templates: minimal, standard, detailed, github
  - Detección automática de tipo de proyecto
  - Soporte para español e inglés
  - Generación de badges

- **sync-skills-registry** (core/analysis): Sincronización de registro
  - Detección automática de cambios
  - Actualización de index.json
  - Modo dry-run disponible

#### Mejoras del Sistema

- **Registro JSON completo**: index.json ahora incluye todas las skills con metadatos
- **Metadatos individuales**: Carpeta registry/metadata/ con JSON por skill
- **Estadísticas automáticas**: Conteo por categoría, complejidad y tokens
- **Índice por tags**: Búsqueda rápida por etiquetas
- **Compatibilidad IA**: Lista expandida de modelos compatibles

### Cambiado

- **index.json**: Actualizado de estructura vacía a completa
- **INDEX.md**: Reorganizado con todas las skills y estadísticas
- **.gitignore**: Permitir archivos en registry/metadata/

### Estadísticas Actualizadas

- **Total de skills**: 9 (5 core + 3 domain + 1 workflow)
- **Cobertura de subcategorías**: 80% (8/10 con skills)
- **Tokens estimados totales**: 5,500 - 9,100

---

## [1.0.0] - 2026-01-07

### Añadido

- **Estructura inicial del sistema de skills**
  - Directorios organizados por categorías (core, domain, workflows)
  - Sistema de plantillas para crear nuevas skills
  - Registro centralizado de skills en JSON
  
- **Documentación completa**
  - README.md con visión general del sistema
  - GUIDELINES.md con mejores prácticas
  - QUICKSTART.md para inicio rápido
  - AI_GUIDE.md para asistentes de IA
  - STRUCTURE.md con mapa detallado
  - WELCOME.md con introducción
  - INDEX.md con catálogo de skills
  - Plantillas detalladas (skill-template.md, workflow-template.md)

- **Skills de Dominio**
  - `analyze-delphi-unit`: Análisis completo de unidades .pas
  - `create-database-migration`: Migraciones Paradox con rollback
  - `validate-verifactu-implementation`: Validación de cumplimiento Verifactu

- **Workflows**
  - `deploy-verifactu-update`: Despliegue completo con 9 fases

- **Sistema de Registro**
  - index.json con metadatos de todas las skills
  - Categorización por tipo y tags
  - Versionamiento semántico
  - Compatibilidad con múltiples modelos de IA

### Estructura de Directorios

```
.skills/
├── core/
│   ├── analysis/
│   │   ├── validate-skill-format.md    ← NUEVO v1.1.0
│   │   └── sync-skills-registry.md     ← NUEVO v1.1.0
│   ├── generation/
│   │   └── generate-boilerplate.md     ← NUEVO v1.1.0
│   ├── refactoring/
│   │   └── extract-method.md           ← NUEVO v1.1.0
│   └── documentation/
│       └── generate-readme.md          ← NUEVO v1.1.0
├── domain/
│   ├── delphi/
│   │   └── analyze-delphi-unit.md
│   ├── database/
│   │   └── create-database-migration.md
│   └── verifactu/
│       └── validate-verifactu-implementation.md
├── workflows/
│   └── deployment/
│       └── deploy-verifactu-update.md
├── templates/
│   ├── skill-template.md
│   └── workflow-template.md
└── registry/
    ├── index.json
    └── metadata/                         ← NUEVO v1.1.0
        ├── analyze-delphi-unit.json
        ├── create-database-migration.json
        ├── deploy-verifactu-update.json
        ├── extract-method.json
        ├── generate-boilerplate.json
        ├── generate-readme.json
        ├── validate-skill-format.json
        └── validate-verifactu-implementation.json
```

### Métricas Actuales

- **Archivos de documentación**: 9
- **Skills activas**: 9
- **Plantillas**: 2
- **Metadatos individuales**: 8
- **Líneas de documentación**: ~6,000
- **Categorías principales**: 3
- **Subcategorías**: 10

---

## Formato de Changelog

### Tipos de Cambios

- **Añadido**: Para nuevas funcionalidades
- **Cambiado**: Para cambios en funcionalidad existente
- **Obsoleto**: Para funcionalidades que serán eliminadas
- **Eliminado**: Para funcionalidades eliminadas
- **Corregido**: Para corrección de bugs
- **Seguridad**: Para vulnerabilidades

### Versionamiento

- **MAJOR** (X.0.0): Cambios incompatibles en la estructura
- **MINOR** (0.X.0): Nueva funcionalidad compatible (skills nuevas)
- **PATCH** (0.0.X): Correcciones menores y mejoras de documentación

---

## Próximos Cambios Planificados

### v1.2.0 (Planificado)

- [ ] `workflows/testing/run-test-suite`: Suite completa de testing
- [ ] `workflows/migration/migrate-database`: Migración entre DBs
- [ ] `domain/delphi/generate-unit-tests`: Generación de tests

### v1.3.0 (Planificado)

- [ ] Sistema de métricas de uso
- [ ] Historial de ejecuciones
- [ ] Integración con CI/CD

---

**Mantenedor**: Sistema de Skills  
**Última actualización**: 2026-01-07T11:19:56+01:00  
**Próxima revisión**: 2026-04-07
