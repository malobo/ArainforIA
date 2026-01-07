---
name: sync-skills-registry
version: 1.0.0
category: core/analysis
tags: [meta, sync, automation, maintenance]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 2
estimated_tokens: 200-400
---

# Sincronizar Registro de Skills

## Descripción

Sincroniza automáticamente el índice de skills con los archivos reales del sistema, detectando cambios y actualizando metadatos.

## Objetivo

Mantener el registro `index.json` y los metadatos individuales siempre actualizados con las skills reales del sistema.

## Inputs

- **skills_root** (string, opcional): Ruta raíz de skills (default: ".skills")
- **update_index** (boolean, opcional): Actualizar index.json (default: true)
- **update_metadata** (boolean, opcional): Actualizar metadatos individuales (default: true)
- **dry_run** (boolean, opcional): Solo mostrar cambios sin aplicar (default: false)

## Outputs

- **changes_detected** (array): Lista de cambios detectados
- **files_updated** (array): Archivos actualizados
- **new_skills** (array): Skills nuevas encontradas
- **removed_skills** (array): Skills eliminadas

## Precondiciones

- Acceso de escritura a carpeta .skills
- Skills siguen formato estándar

## Postcondiciones

- index.json actualizado
- Metadatos sincronizados
- INDEX.md actualizado

## Procedimiento

### Paso 1: Escanear Directorios

Recorrer todas las subcarpetas de skills:

- core/analysis/, core/generation/, core/refactoring/, core/documentation/
- domain/delphi/, domain/database/, domain/verifactu/
- workflows/deployment/, workflows/testing/, workflows/migration/

**Validación**: Todas las carpetas escaneadas

### Paso 2: Detectar Skills

Para cada archivo .md encontrado (excepto README.md):

- Parsear frontmatter YAML
- Extraer metadatos
- Comparar con registro actual

**Validación**: Skills detectadas correctamente

### Paso 3: Identificar Cambios

Comparar con index.json actual:

- Skills nuevas (en disco pero no en registro)
- Skills eliminadas (en registro pero no en disco)
- Skills modificadas (version o updated diferente)

**Validación**: Cambios identificados

### Paso 4: Actualizar Registro

Si not dry_run:

- Añadir skills nuevas a index.json
- Eliminar skills removidas
- Actualizar metadatos de skills modificadas
- Regenerar estadísticas

**Validación**: Registro actualizado

### Paso 5: Actualizar Metadatos Individuales

Para cada skill nueva o modificada:

- Generar/actualizar archivo en registry/metadata/
- Incluir todos los campos del frontmatter
- Añadir campos de tracking (usage_count, last_used)

**Validación**: Metadatos sincronizados

### Paso 6: Generar Reporte

Crear resumen de cambios:

- Skills añadidas
- Skills eliminadas
- Skills actualizadas
- Estadísticas actuales

**Validación**: Reporte generado

## Ejemplos de Uso

### Ejemplo 1: Sincronización Completa

**Contexto**: Después de añadir nuevas skills

**Input**:

```
skills_root: ".skills"
update_index: true
update_metadata: true
```

**Output**:

```json
{
  "changes_detected": [
    {"type": "new", "skill": "core/analysis/detect-code-smells"},
    {"type": "modified", "skill": "domain/delphi/analyze-delphi-unit"}
  ],
  "files_updated": [
    "registry/index.json",
    "registry/metadata/detect-code-smells.json",
    "registry/metadata/analyze-delphi-unit.json"
  ],
  "new_skills": ["detect-code-smells"],
  "removed_skills": []
}
```

### Ejemplo 2: Verificación sin Cambios (Dry Run)

**Contexto**: Verificar estado actual

**Input**:

```
dry_run: true
```

**Output**:

```json
{
  "changes_detected": [],
  "message": "Sistema sincronizado. No hay cambios pendientes."
}
```

## Manejo de Errores

### Error 1: Frontmatter inválido

**Síntoma**: No se puede parsear una skill
**Causa**: YAML mal formado
**Solución**: Usar validate-skill-format para corregir

### Error 2: Permisos de escritura

**Síntoma**: No se pueden actualizar archivos
**Causa**: Sin permisos en carpeta
**Solución**: Verificar permisos de escritura

## Optimizaciones

### Optimización de Tokens

- Solo procesar archivos modificados (por fecha)
- Cachear estructura de directorios

## Dependencias

### Skills Requeridas

- `@skill:core/analysis/validate-skill-format` - Validar formato antes de registrar

## Métricas de Éxito

- [ ] Todas las skills detectadas
- [ ] index.json sincronizado
- [ ] Metadatos actualizados
- [ ] Estadísticas correctas

## Notas

- Ejecutar después de añadir/modificar skills
- Ideal para integrar en pre-commit hook

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
- Sincronización completa de registro
- Modo dry-run

---

**Última revisión**: 2026-01-07  
**Estado**: stable
