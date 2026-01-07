---
name: validate-system-health
version: 1.0.0
category: core/analysis
complexity: 4
tokens_estimate: 800-1200
tags: [validation, health-check, diagnostics, maintenance, system]
requires: []
dependencies: []
---

# 🩺 Validate System Health

## Descripción

Realiza un diagnóstico completo del "Sistema de Skills", verificando la integridad del registro, la existencia de los archivos, la consistencia de versiones y la corrección de los metadatos.

## Cuándo Usar

- Después de actualizar o añadir nuevas skills manualmente.
- Si el sistema parece comportarse de forma extraña (skills no encontradas).
- Como parte de una rutina de mantenimiento mensual.
- Antes de compartir o exportar el sistema de skills.

## Inputs

No requiere inputs obligatorios.

| Parámetro | Tipo | Requerido | Descripción |
| :--- | :--- | :---: | :--- |
| `fix_issues` | boolean | ❌ | Intentar corregir problemas menores automáticamente (default: false) |
| `verbose` | boolean | ❌ | Mostrar salida detallada (default: true) |

## Salida (Reporte)

La skill genera un reporte en consola (y opcionalmente en un archivo de log) con:

- **Estado General**: ✅ SANO / ⚠️ ADVERTENCIA / ❌ ERROR
- **Estadísticas**: Skills encontradas vs. registradas.
- **Lista de Errores**: Archivos perdidos, JSON inválido, etc.
- **Recomendaciones**: Pasos para arreglar los problemas.

## Proceso de Validación

1. **Validar `registry/index.json`**:
    - Sintaxis JSON válida.
    - Estructura de esquema correcta (categorías, subcategorías).
    - Conteo de skills coincide con el total declarado.

2. **Validar Archivos Físicos**:
    - Para cada skill en el registro, verificar que el archivo `.md` existe en la ruta especificada.
    - Verificar que existe el archivo de metadata `.json` correspondiente.

3. **Validar Consistencia de Versiones**:
    - Comparar versión en `registry/index.json` con `WELCOME.md` e `INDEX.md`.
    - Verificar fechas de actualización.

4. **Validar Frontmatter**:
    - Leer el frontmatter YAML de cada skill.
    - Verificar que `name` y `category` coinciden con el registro.

## Ejemplo de Ejecución

```yaml
@skill:core/analysis/validate-system-health
option: verbose
```

**Salida Esperada:**

```text
🔍 INICIANDO DIAGNÓSTICO DEL SISTEMA DE SKILLS...

1. [OK] Registry JSON válido (v1.5.0)
2. [OK] Total skills declaradas: 22
3. [OK] Total skills encontradas en registry: 22
4. [OK] Consistencia de versiones (WELCOME.md, INDEX.md)

VERIFICANDO ARCHIVOS FÍSICOS...
✅ core/generation/generate-unit-tests.md
✅ domain/delphi/implement-design-pattern.md
...
✅ workflows/development/full-feature-development.md

RESULTADO:
🟢 SISTEMA SANO - Todo funciona correctamente.
```

## Solución de Problemas Comunes

| Error | Causa Probable | Solución |
| :--- | :--- | :--- |
| `FileNotFound` | Archivo movido o borrado | Restaurar archivo o actualizar ruta en registry |
| `VersionMismatch` | Se editó un archivo sin los otros | Ejecutar `sync-skills-registry` |
| `JsonParsingError` | Coma faltante o sintaxis error | Corregir `registry/index.json` con un validador |

## Historial de Cambios

| Versión | Fecha | Cambios |
| :--- | :--- | :--- |
| 1.0.0 | 2026-01-07 | Versión inicial |
