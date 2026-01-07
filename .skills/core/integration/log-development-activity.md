---
name: log-development-activity
version: 1.0.0
category: core/integration
tags: [notion, logging, tracking, history, automation]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 3
estimated_tokens: 400-600
type: automation
---

# Log de Actividad de Desarrollo

## Descripción

Registra automáticamente las actividades de desarrollo en una base de datos de Notion,
creando un historial completo y buscable del trabajo realizado.

## Invocación

```
@skill:core/integration/log-development-activity
proyecto: "FACARAVF"
descripcion: "Implementado patrón Builder para facturas"
archivos: ["uFactura.pas", "uFacturaBuilder.pas"]
skill_usada: "delphi-expert-context"
tiempo_minutos: 30
```

---

## Inputs

| Nombre | Tipo | Requerido | Descripción |
|--------|------|-----------|-------------|
| `proyecto` | string | ✅ | Nombre del proyecto |
| `descripcion` | string | ✅ | Descripción de la actividad |
| `archivos` | array | Opcional | Archivos modificados |
| `skill_usada` | string | Opcional | Skill utilizada |
| `tiempo_minutos` | number | Opcional | Tiempo dedicado |
| `tipo` | string | Opcional | feature, bugfix, refactor, docs |
| `notas` | string | Opcional | Notas adicionales |

## Outputs

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `success` | boolean | Registro exitoso |
| `page_id` | string | ID de la página creada |
| `url` | string | URL en Notion |

---

## Procedimiento

### Paso 1: Verificar/Crear Base de Datos

```
1. Buscar base de datos "Historial de Desarrollo" en Notion
2. Si no existe, crearla con propiedades:
   - Fecha (date)
   - Proyecto (select)
   - Descripción (title)
   - Archivos (rich_text)
   - Skill (select)
   - Tiempo (number)
   - Tipo (select)
```

### Paso 2: Crear Registro

```
1. Crear nueva página en la base de datos
2. Rellenar propiedades con datos proporcionados
3. Añadir notas como contenido de la página
```

### Paso 3: Confirmar

```
1. Retornar URL del registro creado
2. Actualizar contador de actividad
```

---

## Ejemplo de Registro en Notion

```
📝 Historial de Desarrollo

| Fecha | Proyecto | Descripción | Archivos | Skill | Tiempo |
|-------|----------|-------------|----------|-------|--------|
| 07/01 | FACARAVF | Patrón Builder facturas | 2 | delphi-expert | 30min |
| 07/01 | FACARAVF | Fix validación NIF | 1 | analyze-delphi | 15min |
| 06/01 | Aracostes | Migración BD | 3 | db-migration | 45min |
```

---

## Notas

- Se ejecuta automáticamente al completar tareas significativas
- Integra con sync-notion-skills para tracking de uso de skills
- Permite generar reportes de productividad

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
