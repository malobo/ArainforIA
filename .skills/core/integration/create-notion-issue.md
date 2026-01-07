---
name: create-notion-issue
version: 1.0.0
category: core/integration
tags: [notion, bugs, issues, tracking, quality]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 2
estimated_tokens: 300-500
type: automation
---

# Crear Issue/Bug en Notion

## Descripción

Crea rápidamente un issue, bug o tarea en la base de datos de Issues de Notion
directamente desde el IDE.

## Invocación

```
@skill:core/integration/create-notion-issue
titulo: "Error en validación de NIF con letra ñ"
tipo: "bug"
severidad: "alta"
archivo: "uVerifactu.pas"
linea: 245
descripcion: "La función ValidarNIF falla cuando el NIF contiene caracteres especiales"
```

---

## Inputs

| Nombre | Tipo | Requerido | Descripción |
|--------|------|-----------|-------------|
| `titulo` | string | ✅ | Título del issue |
| `tipo` | string | ✅ | bug, feature, improvement, task |
| `severidad` | string | Opcional | critica, alta, media, baja |
| `proyecto` | string | Opcional | Proyecto afectado |
| `archivo` | string | Opcional | Archivo relacionado |
| `linea` | number | Opcional | Línea del código |
| `descripcion` | string | Opcional | Descripción detallada |
| `asignado` | string | Opcional | Persona asignada |
| `etiquetas` | array | Opcional | Tags adicionales |

## Outputs

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `success` | boolean | Creación exitosa |
| `issue_id` | string | ID del issue |
| `url` | string | URL en Notion |
| `numero` | number | Número de issue |

---

## Procedimiento

### Paso 1: Validar Datos

```
1. Verificar título no vacío
2. Validar tipo (bug/feature/improvement/task)
3. Asignar severidad por defecto si no se especifica
```

### Paso 2: Crear Issue

```
1. Crear página en base de datos "Issues"
2. Asignar propiedades:
   - Título
   - Estado: "Abierto"
   - Tipo
   - Severidad
   - Proyecto
   - Fecha creación
3. Añadir contenido:
   - Descripción
   - Ubicación (archivo:línea)
   - Pasos para reproducir (si es bug)
```

### Paso 3: Vincular

```
1. Si hay archivo, vincular con documentación
2. Notificar (si hay asignado)
3. Retornar URL
```

---

## Plantilla de Issue en Notion

```markdown
# 🐛 [BUG-042] Error en validación de NIF

**Estado**: 🔴 Abierto
**Severidad**: Alta
**Proyecto**: FACARAVF
**Creado**: 2026-01-07

## Ubicación
- Archivo: `uVerifactu.pas`
- Línea: 245

## Descripción
La función ValidarNIF falla cuando el NIF contiene caracteres especiales.

## Pasos para Reproducir
1. Abrir formulario de cliente
2. Introducir NIF con ñ
3. Error en validación

## Notas
[Espacio para comentarios]
```

---

## Notas

- Genera número de issue automático (BUG-001, FEAT-001, etc.)
- Integra con el flujo de trabajo existente en Notion
- Permite seguimiento desde Dashboard de Skills

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
