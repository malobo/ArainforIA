---
name: sync-notion-skills
version: 1.0.0
category: core/integration
tags: [notion, sync, integration, mcp, automation]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 5
estimated_tokens: 800-1200
type: automation
---

# Sincronización de Skills con Notion

## Descripción

Skill para sincronizar el sistema de skills local con Notion, permitiendo
visualizar, gestionar y trackear el uso de skills desde Notion.

## Objetivo

Mantener un dashboard en Notion sincronizado con el sistema de skills,
registrar uso de skills y facilitar la gestión del conocimiento.

## Invocación

```
@skill:core/integration/sync-notion-skills
action: "sync" | "log" | "create-dashboard"
```

---

## Funcionalidades

### 1. Crear Dashboard de Skills en Notion

Crea una base de datos en Notion con todas las skills:

```json
{
  "action": "create-dashboard",
  "parent_page_id": "2e008dcb-066b-8127-8e09-f1a1d6548259",
  "include_stats": true
}
```

**Resultado**: Base de datos con columnas para:

- Nombre, Categoría, Complejidad, Estado, Tokens, Tags, Descripción

### 2. Sincronizar Skills Locales → Notion

Lee `.skills/registry/index.json` y actualiza la base de datos de Notion:

```json
{
  "action": "sync",
  "direction": "local-to-notion",
  "update_existing": true
}
```

### 3. Registrar Uso de Skill

Añade un registro en Notion cada vez que se ejecuta una skill:

```json
{
  "action": "log",
  "skill_name": "delphi-expert-context",
  "result": "success",
  "notes": "Ayudó con implementación de Factory Pattern"
}
```

### 4. Consultar Estado desde Notion

Lee el estado de skills desde Notion (por si se actualizaron manualmente):

```json
{
  "action": "sync",
  "direction": "notion-to-local"
}
```

---

## Implementación

### Paso 1: Buscar o Crear Dashboard

```
1. Buscar en Notion página "Sistema de Skills IA"
2. Si existe, obtener ID
3. Si no existe base de datos de skills, crearla
```

### Paso 2: Estructura de la Base de Datos

```javascript
// Propiedades de la base de datos
{
  "Nombre": { "type": "title" },
  "Categoría": { 
    "type": "select",
    "options": ["core/analysis", "core/generation", "core/refactoring", 
                "core/documentation", "core/integration",
                "domain/delphi", "domain/database", "domain/verifactu",
                "workflows/deployment"]
  },
  "Complejidad": { "type": "number" },
  "Estado": {
    "type": "status",
    "options": ["Activo", "Planificado", "Deprecated"]
  },
  "Tokens Est.": { "type": "number" },
  "Tags": { "type": "multi_select" },
  "Última Vez": { "type": "date" },
  "Uso Total": { "type": "number" },
  "Ruta": { "type": "rich_text" },
  "Descripción": { "type": "rich_text" }
}
```

### Paso 3: Sincronización

```
Para cada skill en registry/index.json:
  1. Buscar en Notion por nombre
  2. Si existe: Actualizar propiedades
  3. Si no existe: Crear nueva página
  4. Añadir contenido del skill.md como cuerpo de la página
```

### Paso 4: Logging

```
Al ejecutar cualquier skill:
  1. Incrementar contador "Uso Total"
  2. Actualizar "Última Vez"
  3. (Opcional) Añadir comentario con detalles
```

---

## API de Notion Utilizada

| Operación | Endpoint MCP |
|-----------|--------------|
| Buscar skills | `API-post-search` |
| Crear página | `API-post-page` |
| Actualizar página | `API-patch-page` |
| Crear base de datos | `API-create-a-data-source` |
| Añadir contenido | `API-patch-block-children` |
| Registrar uso | `API-create-a-comment` |

---

## Beneficios

### Para el Usuario

- ✅ Dashboard visual de todas las skills
- ✅ Ver uso histórico de cada skill
- ✅ Gestionar skills desde Notion (móvil, web)
- ✅ Compartir skills con equipo
- ✅ Integrar con otras páginas de Notion

### Para la IA

- ✅ Conocer qué skills se usan más
- ✅ Priorizar mejoras en skills populares
- ✅ Detectar skills obsoletas
- ✅ Sugerir nuevas skills basado en patrones

### Para el Proyecto FACARAVF

- ✅ Vincular skills con tareas de Verifactu
- ✅ Trackear progreso del proyecto
- ✅ Documentación centralizada

---

## Ejemplo de Uso

### Crear Dashboard Inicial

```
@skill:core/integration/sync-notion-skills
action: "create-dashboard"
```

Resultado en Notion:

```
📊 Skills ARAINFORIA
├── 🔍 validate-skill-format [core/analysis] ⭐3 ✅Activo
├── 🔄 sync-skills-registry [core/analysis] ⭐2 ✅Activo
├── 🏗️ generate-boilerplate [core/generation] ⭐4 ✅Activo
├── ✂️ extract-method [core/refactoring] ⭐5 ✅Activo
├── 📝 generate-readme [core/documentation] ⭐3 ✅Activo
├── 🔗 sync-notion-skills [core/integration] ⭐5 ✅Activo  ← NUEVO
├── 🔬 analyze-delphi-unit [domain/delphi] ⭐4 ✅Activo
├── 🧠 delphi-expert-context [domain/delphi] ⭐1 ✅Activo
├── 🗃️ create-database-migration [domain/database] ⭐6 ✅Activo
├── ✅ validate-verifactu-implementation [domain/verifactu] ⭐7 ✅Activo
└── 🚀 deploy-verifactu-update [workflows/deployment] ⭐8 ✅Activo
```

### Registrar Uso de Skill

```
@skill:core/integration/sync-notion-skills
action: "log"
skill_name: "delphi-expert-context"
result: "success"
notes: "Implementé el patrón Builder para facturas"
```

---

## Inputs

| Nombre | Tipo | Requerido | Descripción |
|--------|------|-----------|-------------|
| `action` | string | ✅ | sync, log, create-dashboard |
| `parent_page_id` | string | Opcional | ID de página padre en Notion |
| `skill_name` | string | Para log | Nombre de la skill usada |
| `result` | string | Para log | success, error, partial |
| `notes` | string | Opcional | Notas adicionales |
| `direction` | string | Para sync | local-to-notion, notion-to-local |

## Outputs

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `success` | boolean | Operación exitosa |
| `database_id` | string | ID de la base de datos creada |
| `synced_count` | number | Número de skills sincronizadas |
| `errors` | array | Errores si los hubo |

---

## Métricas de Éxito

- [ ] Dashboard creado en Notion
- [ ] Todas las skills sincronizadas
- [ ] Uso registrado correctamente
- [ ] Sin errores de API

## Notas

- Requiere conexión MCP con Notion activa
- La página "Sistema de Skills IA" ya existe (ID: 2e008dcb-066b-8127-8e09-f1a1d6548259)
- Se integra con el sistema de skills existente

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
- Funciones: create-dashboard, sync, log

---

**Última revisión**: 2026-01-07  
**Estado**: stable
