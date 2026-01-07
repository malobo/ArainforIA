---
name: update-task-status
version: 1.0.0
category: core/integration
tags: [notion, tasks, status, project-management, workflow]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 2
estimated_tokens: 300-500
type: automation
---

# Actualizar Estado de Tarea en Notion

## Descripción

Actualiza el estado de tareas en Notion directamente desde el IDE,
permitiendo gestionar el flujo de trabajo sin cambiar de contexto.

## Invocación

```
@skill:core/integration/update-task-status
tarea: "FASE 3: Implementación Hash"
estado: "En Progreso"
progreso: 75
notas: "Completado hash SHA-256, pendiente validación de cadena"
```

---

## Inputs

| Nombre | Tipo | Requerido | Descripción |
|--------|------|-----------|-------------|
| `tarea` | string | ✅ | Nombre o ID de la tarea |
| `estado` | string | ✅ | Nuevo estado |
| `progreso` | number | Opcional | Porcentaje 0-100 |
| `notas` | string | Opcional | Notas de actualización |
| `tiempo_dedicado` | number | Opcional | Minutos trabajados |
| `bloqueos` | string | Opcional | Problemas encontrados |
| `siguiente_paso` | string | Opcional | Próxima acción |

## Estados Disponibles

- `Pendiente` / `To Do`
- `En Progreso` / `In Progress`
- `En Revisión` / `Review`
- `Bloqueado` / `Blocked`
- `Completado` / `Done`
- `Cancelado` / `Cancelled`

## Outputs

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `success` | boolean | Actualización exitosa |
| `tarea_id` | string | ID de la tarea |
| `estado_anterior` | string | Estado previo |
| `url` | string | URL en Notion |

---

## Procedimiento

### Paso 1: Buscar Tarea

```
1. Buscar por nombre en bases de datos de tareas
2. Si hay múltiples, mostrar opciones
3. Validar que la tarea existe
```

### Paso 2: Actualizar

```
1. Cambiar propiedad de estado
2. Actualizar fecha de modificación
3. Si hay progreso, actualizar barra
4. Si hay notas, añadir comentario
```

### Paso 3: Registrar

```
1. Añadir entrada al historial de la tarea
2. Si estado=Completado, registrar en log
3. Notificar si hay asignados
```

---

## Ejemplo de Uso

**Comando**:

```
@skill:core/integration/update-task-status
tarea: "FASE 3"
estado: "Completado"
notas: "Hash SHA-256 implementado y testeado"
```

**Resultado en Notion**:

```
✅ Tarea actualizada

📋 FASE 3: Implementación Hash Encadenado
├── Estado: Pendiente → ✅ Completado
├── Fecha: 2026-01-07 11:57
└── Nota añadida: "Hash SHA-256 implementado y testeado"

📊 Progreso del Proyecto FACARAVF:
████████████░░░░░░░░ 60% (3/5 fases)
```

---

## Integración con Proyectos

```
📁 FACARAVF - Tareas Verifactu
├── ✅ FASE 1: Estructura de Datos
├── ✅ FASE 2: Generación de Huella
├── ✅ FASE 3: Hash Encadenado ← Actualizada
├── 🔄 FASE 4: Firma Digital
└── ⏳ FASE 5: XML AEAT
```

---

## Atajos Rápidos

```
# Marcar como completada
@update-task "FASE 3" done

# Marcar como en progreso
@update-task "FASE 4" wip

# Marcar como bloqueada
@update-task "FASE 5" blocked "Esperando certificado digital"
```

---

## Notas

- Busca en todas las bases de datos de tareas
- Soporta nombres parciales
- Se integra con log-development-activity

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
