# LOG UNIFICADO DE DESARROLLO Y SISTEMA DE SKILLS

## MEJORAS PLANIFICADAS Y ESTRATÉGICAS

# Mejoras del Sistema de Skills con OpenSpec

Este documento resume las ventajas estratégicas de evolucionar el sistema actual de `.skills` (Base de Conocimiento) hacia un modelo híbrido que incorpore **OpenSpec** (Desarrollo Dirigido por Especificaciones).

## 1. Memoria de Estado ("Stateful Intelligence")

* **Situación Actual:** El sistema es reactivo; conoce la técnica pero no el estado actual del proyecto (qué tablas existen, qué rutas de API están implementadas, etc.).
* **Mejora con OpenSpec:** Se introducen los `specs/` como memoria a largo plazo. La IA consulta la "verdad" del proyecto antes de proponer cambios, evitando redundancias o conflictos arquitectónicos.

## 2. Eficiencia de Tokens ("Token Density")

* **Situación Actual:** Para obtener contexto, a menudo es necesario leer múltiples archivos fuente (`.pas`, `.dfm`), lo que consume rápidamente la ventana de contexto.
* **Mejora con OpenSpec:** Los archivos en `specs/` (YAML/Markdown) actúan como resúmenes técnicos de alta densidad. La IA entiende la arquitectura leyendo un solo archivo de especificación en lugar de diez archivos de código.

## 3. Seguridad en Refactorizaciones (Protocolo `changes/`)

* **Situación Actual:** Los cambios se aplican directamente al código. Un error en un sistema crítico (como Verifactu) puede ser difícil de detectar y revertir.
* **Mejora con OpenSpec:** Obliga a crear una **Propuesta de Cambio** (`openspec/changes/`) antes de la ejecución. Esto permite al usuario validar el "plano" técnico (archivos afectados, lógica de negocio, riesgos) antes de modificar el código fuente.

## 4. Estandarización de Interfaces entre Agentes

* **Situación Actual:** Al delegar tareas a sub-agentes, estos deben re-investigar el contexto, lo que puede llevar a inconsistencias.
* **Mejora con OpenSpec:** Los `specs/` sirven como el lenguaje común. Los agentes comparten un mapa de arquitectura unificado, asegurando que todos "hablen el mismo idioma" técnico y respeten las mismas reglas de diseño.

## 5. Documentación Viva ("Self-Documenting System")

* **Situación Actual:** La documentación técnica suele ser un esfuerzo separado que tiende a quedar obsoleta respecto al código real.
* **Mejora con OpenSpec:** La especificación es parte del flujo de trabajo. Si el código cambia, el spec debe actualizarse. Esto garantiza que el proyecto siempre tenga una documentación técnica técnica precisa y actualizada para futuros desarrolladores o IAs.

---

**Conclusión:** La integración de OpenSpec transforma el asistente de un programador que "sabe Delphi" a un **Arquitecto de Sistemas** que entiende profundamente la realidad específica de **FACARAVF**.

---

## HISTORIAL DE IMPLEMENTACIÓN DEL SISTEMA DE SKILLS

# Log de Implementación: Sistema de Skills de Inteligencia Artificial

**Fecha de Inicio:** Enero 2026
**Ubicación:** `c:\Arainfor\.skills\`
**Versión Actual:** 2.0 (Next-Gen Context Engineering)

## 1. Fase Inicial: Estructura y Fundamentos

**Objetivo:** Crear un repositorio centralizado de "habilidades" para dotar de contexto a los agentes de IA.

* **Estructura de Directorios:**
  * `core/`: Conocimiento crítico (Delphi, Verifactu).
  * `projects/`: Contexto de aplicaciones (ARAFAC, ERPW, TPVARA).
  * `workflows/`: Guías de procedimientos (Compilación).
* **Archivos Base:**
  * `_template.md`: Plantilla estándar.
  * `README.md`: Instrucciones de uso.

## 2. Fase de Refinamiento (Nivel 9/10)

**Objetivo:** Mejorar la organización y priorización del conocimiento.

* **Metadatos Agregados:** Campo `priority` (critical, high, medium).
* **Automatización:** Creación de `generate_index.py`.
  * Escanea todos los `.md`.
  * Ordena por prioridad.
  * Genera `index.md` automáticamente.

## 3. Fase "Next-Gen": Context Engineering (Estado del Arte)

**Objetivo:** Adaptar el sistema a las mejores prácticas de LLMs (Anthropic/Google DeepMind) para evitar alucinaciones y mejorar la adherencia a instrucciones.

* **Formato XML-Enhanced:**
  * Implementación de tags semánticos: `<context>`, `<instruction>`, `<examples>`.
  * Esto permite a la IA distinguir claramente entre datos informativos y órdenes ejecutivas.
* **Sistema de Triggers:**
  * Añadido campo `triggers` (lista de frases) al Frontmatter YAML.
  * Permite la carga dinámica de skills basada en keywords del usuario.
* **Validación Automática (`validate_skills.py`):**
  * Script tipo "Linter" que impide la existencia de skills rotos.
  * Verifica: YAML válido, Triggers presentes, Tags XML obligatorios.

## 4. Sincronización de Conocimiento Externo

**Objetivo:** Que otros cerebros (RAG, Notion) conozcan la existencia de este sistema.

* **RAG (Memoria Técnica):**
  * Creado `C:\Arainfor\DelphiDoc\AI_Skills_System.md`.
  * Ejecutada ingesta (`ingest_docs.py`) para indexar estos conceptos en la base de datos vectorial.
* **Notion (Memoria Funcional):**
  * Debido a limitaciones de API, se generó `CONTENIDO_NOTION_SKILLS.md`.
  * Listo para ser copiado manualmente a la base de conocimiento corporativa.

---

## 5. Fase de Enriquecimiento Técnico (mORMot & Modern Delphi)

**Objetivo:** Elevar la calidad técnica de las respuestas del RAG incorporando frameworks avanzados y estándares modernos.

* **Conocimiento Ingestado (RAG):**
  * **Modern Delphi Best Practices:** Documento "Fuente de Verdad" con reglas sobre Inline Vars, Clean Code, y prohibición de `with`.
* **Nuevo Skill Crítico:** `core/mormot.md`
  * **Criptografía Verifactu:** Uso nativo de PKCS#11/X.509.
  * **Logging Avanzado:** Estandarización de `mormot.core.log`.
  * **PDF/A:** Generación legal nativa.
  * **Templating:** Mustache para separar lógica de vista.

---
**Estado Final Fase 3:** El sistema es autónomo, robusto, y ahora **experto en arquitectura moderna** (mORMot 2 + Clean Code).

## 4. Fase de Expansión Híbrida y Limpieza (07-01-2026)

**Objetivo:** Adaptar el sistema para desarrollo híbrido (Delphi+Cloud) y refinar el inventario de herramientas.

* **Nuevas Skills:**
  * `domain/hybrid/hybrid-sync.md`: Estrategia de sincronización Delphi <-> PHP/MySQL.
  * `domain/delphi/components-inventory.md`: Inventario de terceros (mORMot, Zeos, etc.).
* **Mejoras:**
  * `core/mormot.md`: Ampliado con `mormot.net.client` (HTTP/REST) y optimización JSON.
  * **Limpieza**: Eliminadas dependencias de componentes de pago (TMS) para garantizar portabilidad.
* **Infraestructura:**
  * Soporte de subcategorías en indexador (`generate_index.py`).
  * Sincronización completa con `ArainforIA`.

---

## LOG DETALLADO DE SESIONES DE DESARROLLO

# Log de Desarrollo - Sistema de Skills

## Sesión: 2026-01-07

### Revisión del Sistema de Skills

**Objetivo**: Analizar la estructura y contenido de `.skills` y proponer mejoras.

**Estado Actual**:
* Estructura de directorios clara (`core`, `domain`, `registry`, `workflows`).
* Documentación extensiva (`README`, `GUIDELINES`, `AI_GUIDE`).
* Formato de Skills consistente (Frontmatter YAML + Markdown).
* Indice centralizado en JSON.

**Análisis de Archivos Revisados**:

1. `core/analysis/validate-skill-format.md`: Buena definición de validación.
2. `domain/delphi/analyze-delphi-unit.md`: Procedimiento claro para análisis estático.
3. `workflows/deployment/deploy-verifactu-update.md`: Workflow robusto con rollback.

### Optimizaciones Implementadas (v1.1)

#### 1. Contexto de Alta Densidad (`## AI Context`)

Se ha añadido un bloque `AI Context` en las skills principales y el template.
* **Beneficio**: Instrucciones directas al System Prompt para ahorrar tokens.
* **Estado**: Implementado en Template, Guidelines, `validate-skill-format`, `analyze-delphi-unit`, `deploy-verifactu-update`.

#### 2. Mapeo Explícito de Herramientas (`## Tool Mapping`)

Se ha añadido la sección `Tool Mapping` para vincular acciones abstractas con herramientas del CLI.
* **Beneficio**: Elimina ambigüedad en ejecución.
* **Estado**: Implementado en Template, Guidelines, `validate-skill-format`, `analyze-delphi-unit`, `deploy-verifactu-update`.

#### 3. Scripts Parametrizados

Se han introducido variables `{{VAR}}` en los ejemplos de código.
* **Beneficio**: Facilita la automatización segura.
* **Estado**: Implementado en `deploy-verifactu-update`.

#### 4. Validación de Integridad (JSON Schema)

Se ha creado un esquema JSON para validar el registro.
* **Archivo**: `.skills/registry/schema.json`
* **Estado**: Creado.

### Próximos Pasos

- Migrar el resto de skills al formato v1.1 progresivamente.
* Integrar la validación de esquema en `sync-skills-registry`.

## Sesión: 2026-01-07 (Continuación)

### Integración Híbrida (Desktop + Cloud)

**Objetivo**: Extender el sistema para soportar sincronización bidireccional entre Delphi y PHP/MySQL.

**Acciones Realizadas**:

1. **Arquitectura**: Creado `CLOUD_INTEGRATION.md` definiendo el modelo de sincronización Multi-Entidad (Clientes, Partes, Pedidos).
2. **Skills de Dominio**:
    * `delphi-cloud-context`: Especializado en `System.Net.HttpClient` y JSON.
    * `php-mysql-context`: Especializado en Backend API REST y MySQL.
3. **Motor de Sincronización (Delphi)**:
    * `uSincroService.pas`: Motor que procesa la cola `SincroControl` y envía a la nube.
    * `uSincroTrigger.pas`: Clase auxiliar para inyectar cambios en la cola desde el evento `AfterPost`.
4. **Integración en Proyecto**: Modificado `ARAVF.01/DataModule1.pas` para registrar cambios en `CLIENTES` y `FACTURAS`.

**Estado de Versionado**:
* Sistema de Skills y Arquitectura: Sincronizado en repositorio raíz `ARAINFORIA`.
* Código fuente Delphi: Ubicado en carpetas locales, no versionado en la raíz por política de separación de repositorios.

### Tareas Pendientes (Próxima Sesión)

- [ ] Diseñar la estructura de la API PHP (Controladores MVC) para la sincronización.
* [ ] Crear el esquema SQL para la base de datos MySQL en la nube.
* [ ] Implementar autenticación JWT en el backend.
  
## PROPUESTA DE NUEVAS SKILLS (PENDIENTE DE IMPLEMENTACION)  
# 📋 Propuesta de Mejoras al Sistema de Skills

**Fecha**: 2026-01-08  
**Versión actual**: 1.6.0  
**Objetivo**: Optimizar el desarrollo híbrido con Delphi

---

## 🚀 Skills de Alta Prioridad (Impacto Inmediato)

### 1. `convert-sql-paradox` (domain/database)

Convierte consultas SQL estándar a sintaxis compatible con Paradox/BDE y viceversa.

| Campo | Valor |
|-------|-------|
| **Triggers** | `convertir sql`, `paradox query`, `sql a paradox` |
| **Uso** | Migración de consultas, optimización de queries |
| **Complejidad** | 4/10 |

### 2. `generate-api-client` (domain/delphi)

Genera código cliente REST para consumir APIs (AEAT, bancos, servicios externos).

| Campo | Valor |
|-------|-------|
| **Triggers** | `consumir api`, `cliente rest`, `llamada http` |
| **Uso** | Integraciones con AEAT, servicios de facturación electrónica |
| **Complejidad** | 6/10 |

### 3. `migrate-bde-firedac` (workflows/migration)

Workflow paso a paso para migrar de BDE/Paradox a FireDAC/SQL.

| Campo | Valor |
|-------|-------|
| **Triggers** | `migrar bde`, `eliminar paradox`, `modernizar datos` |
| **Uso** | Modernización gradual del acceso a datos |
| **Complejidad** | 8/10 |

### 4. `optimize-vcl-form` (domain/delphi)

Analiza formularios VCL para detectar problemas de rendimiento y UX.

| Campo | Valor |
|-------|-------|
| **Triggers** | `formulario lento`, `optimizar form`, `mejorar ui` |
| **Uso** | Mejora de formularios legacy |
| **Complejidad** | 5/10 |

---

## 🔧 Skills de Prioridad Media (Productividad)

### 5. `generate-report-template` (domain/database)

Genera plantillas de informes para GmPrintSuite.

| Campo | Valor |
|-------|-------|
| **Triggers** | `crear informe`, `plantilla impresion`, `reporte nuevo` |
| **Uso** | Creación rápida de facturas, tickets, listados |
| **Complejidad** | 5/10 |

### 6. `validate-dfm-integrity` (core/analysis)

Valida archivos .dfm para detectar referencias rotas, componentes faltantes.

| Campo | Valor |
|-------|-------|
| **Triggers** | `dfm roto`, `componente no encontrado`, `error form` |
| **Uso** | Debugging de formularios corruptos |
| **Complejidad** | 4/10 |

### 7. `generate-data-export` (domain/database)

Genera código para exportar datos a Excel, CSV, JSON, XML.

| Campo | Valor |
|-------|-------|
| **Triggers** | `exportar excel`, `generar csv`, `sacar datos` |
| **Uso** | Funcionalidades de exportación estándar |
| **Complejidad** | 4/10 |

### 8. `implement-audit-trail` (domain/database)

Implementa sistema de auditoría de cambios en tablas.

| Campo | Valor |
|-------|-------|
| **Triggers** | `auditar cambios`, `historial registros`, `log modificaciones` |
| **Uso** | Trazabilidad para cumplimiento normativo |
| **Complejidad** | 6/10 |

---

## 🎯 Skills Avanzadas (Arquitectura)

### 9. `refactor-to-mvp` (core/refactoring)

Guía para separar lógica de negocio de UI (Model-View-Presenter).

| Campo | Valor |
|-------|-------|
| **Triggers** | `separar logica`, `mvp pattern`, `desacoplar form` |
| **Uso** | Modernización de arquitectura |
| **Complejidad** | 7/10 |

### 10. `create-rest-endpoint` (domain/delphi)

Crea endpoints REST con mORMot2 para exponer funcionalidad.

| Campo | Valor |
|-------|-------|
| **Triggers** | `crear api`, `endpoint rest`, `servicio web` |
| **Uso** | Exposición de servicios para apps móviles/web |
| **Complejidad** | 7/10 |

### 11. `debug-connection-issue` (domain/database)

Diagnóstico de problemas de conexión a BD (BDE, Paradox, SQL Server).

| Campo | Valor |
|-------|-------|
| **Triggers** | `no conecta`, `error conexion`, `bd bloqueada` |
| **Uso** | Troubleshooting de conectividad |
| **Complejidad** | 5/10 |

---

## 📱 Skills para Desarrollo Híbrido

### 12. `sync-mobile-data` (workflows/development)

Workflow para sincronización de datos entre app móvil y desktop.

| Campo | Valor |
|-------|-------|
| **Triggers** | `sincronizar movil`, `datos offline`, `sync app` |
| **Uso** | Integración con apps React Native/Flutter |
| **Complejidad** | 8/10 |

### 13. `generate-json-dto` (core/generation)

Genera DTOs (Data Transfer Objects) para comunicación JSON.

| Campo | Valor |
|-------|-------|
| **Triggers** | `crear dto`, `json structure`, `serializar objeto` |
| **Uso** | Comunicación entre Delphi y front-ends modernos |
| **Complejidad** | 4/10 |

---

## 📊 Resumen

| Prioridad | Cantidad | Complejidad Promedio |
|-----------|----------|---------------------|
| Alta | 4 | 5.75 |
| Media | 4 | 4.75 |
| Avanzada | 3 | 6.33 |
| Híbrido | 2 | 6.0 |
| **Total** | **13** | **5.5** |

---

## 🔗 Próximos Pasos

1. Revisar priorización con el usuario
2. Seleccionar 3-4 skills para implementar primero
3. Crear plan de implementación detallado
4. Implementar skills seleccionadas
5. Actualizar INDEX.md y registry
