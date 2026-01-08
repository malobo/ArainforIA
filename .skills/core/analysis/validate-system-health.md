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

 Realiza un diagnóstico completo del "Sistema de Skills", verificando la integridad del registro OpenSpec, la existencia de los archivos, la consistencia de versiones y la corrección de los metadatos.

## Cuándo Usar

- Después de actualizar o añadir nuevas skills manualmente.
- Si el sistema parece comportarse de forma extraña (skills no encontradas).
- Como parte de una rutina de mantenimiento mensual.
- Antes de compartir o exportar el sistema de skills.

## Inputs

 No requiere inputs obligatorios.

 | Parámetro | Tipo | Requerido | Descripción |
 | :--- | :--- | :---: | :--- |
 | `fix_issues` | boolean | ❌ | Intentar corregir problemas menores automáticamente con `sync_openspec.py` (default: false) |
 | `verbose` | boolean | ❌ | Mostrar salida detallada (default: true) |

## Salida (Reporte)

 La skill genera un reporte en consola (y opcionalmente en un archivo de log) con:

- **Estado General**: ✅ SANO / ⚠️ ADVERTENCIA / ❌ ERROR
- **Estadísticas**: Skills encontradas vs. registradas en YAML.
- **Lista de Errores**: Archivos perdidos, YAML inválido, triggers faltantes.
- **Recomendaciones**: Pasos para arreglar los problemas (ej: ejecutar scripts).

## Proceso de Validación

 1. **Validar `registry/tools.yaml`**:
     - Sintaxis YAML válida.
     - Estructura OpenSpec correcta (`tools` list, `parameters`).
     - Conteo de skills coincide con el total declarado.

 2. **Validar Archivos Físicos**:
     - Para cada skill en el registro, verificar que el archivo `.md` existe en la ruta relativa correcta.
     - Verificar existencia de la carpeta `scripts/` y sus componentes clave (`sync_openspec.py`).

 3. **Validar Consistencia de Versiones**:
     - Comparar versión en `registry/tools.yaml` con `.skills/README.md`.
     - Verificar fechas de actualización.

 4. **Validar Frontmatter y Tags**:
     - Leer el frontmatter YAML de cada skill.
     - Verificar existencia de triggers.
     - Verificar presencia de tags Next-Gen: `<context>`, `<instruction>`, `<examples>`.

## Ejemplo de Ejecución

 ```yaml
 @skill:core/analysis/validate-system-health
 option: verbose
 ```

 **Salida Esperada:**

 ```text
 🔍 INICIANDO DIAGNÓSTICO DEL SISTEMA DE SKILLS...
 
 1. [OK] Registry YAML válido (OpenSpec)
 2. [OK] Total skills declaradas: 70
 3. [OK] Total skills encontradas en disco: 70
 4. [OK] Estructura de carpetas (scripts/, templates/) correcta
 
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
 | `FileNotFound` | Archivo movido o borrado | Restaurar archivo o ejecutar `scripts/sync_openspec.py` |
 | `VersionMismatch` | Se editó un archivo sin los otros | Actualizar `README.md` o ejecutar sync |
 | `YamlParsingError` | Indentación incorrecta | Corregir `registry/tools.yaml` con un validador |

## Historial de Cambios

| Versión | Fecha | Cambios |
| :--- | :--- | :--- |
| 1.0.0 | 2026-01-07 | Versión inicial |
