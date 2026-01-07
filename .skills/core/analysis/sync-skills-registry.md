---
name: sync-skills-registry
version: 1.1.0
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

Sincroniza automáticamente el índice de skills con los archivos reales del sistema, detectando cambios y validando contra el esquema JSON.

## Objetivo

Mantener `index.json` y los metadatos sincronizados, asegurando que ninguna skill rompa la integridad del sistema.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Registry Manager. Scan directory tree. Parse YAML frontmatter. Validate against `.skills/registry/schema.json`. Update `index.json`.
> **OUTPUT_FORMAT**: JSON Report (Changes, Errors, Stats).
> **CRITICAL**: Do not register skills with invalid YAML.

## Inputs

- **skills_root** (string, opcional): Ruta raíz (default: ".skills")
- **update_index** (boolean, opcional): Actualizar index.json (default: true)
- **dry_run** (boolean, opcional): Solo mostrar cambios (default: false)

## Outputs

- **changes_detected** (array): Lista de cambios
- **files_updated** (array): Archivos modificados
- **validation_errors** (array): Errores de esquema encontrados

## Tool Mapping

- **Escanear**: `list_directory` (recursive)
- **Leer Metadatos**: `read_file` (extract frontmatter)
- **Validar Esquema**: `read_file` (registry/schema.json) + Internal JSON Validator
- **Escribir Índice**: `write_file` (registry/index.json)

## Precondiciones
- El archivo `registry/schema.json` debe existir.

## Procedimiento
1. Cargar esquema de validación.
2. Recorrer directorios buscando `*.md`.
3. Parsear frontmatter y validar contra esquema.
4. Comparar con `index.json` actual.
5. Generar reporte de discrepancias.
6. Guardar cambios (si `!dry_run`).

## Changelog
### v1.1.0 (2026-01-07)
- Añadido AI Context.
- Validación contra `schema.json`.