---
name: generate-boilerplate
version: 1.1.0
category: core/generation
tags: [generation, boilerplate, code, templates]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 4
estimated_tokens: 500-800
---

# Generar Boilerplate

## Descripción

Genera estructuras de código base (clases, unidades, tests) siguiendo los estándares del proyecto y el lenguaje.

## Objetivo

Acelerar el desarrollo eliminando la escritura de código repetitivo, asegurando convenciones de nomenclatura y estructura desde el inicio.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Senior Developer. Generate clean, compilable boilerplate. Follow language idioms (e.g., Pascal naming: TClass, IInterface). Include standard sections (interface/implementation).
> **OUTPUT_FORMAT**: Code block with filename hint.
> **STYLE**: Strict, minimalistic, documented.

## Inputs

- **component_type** (string): [class|unit|form|interface|test]
- **name** (string): Nombre del artefacto (ej: `TFacturaService`)
- **language** (string, opcional): [delphi|python|javascript] (default: context dependent)
- **inheritance** (string, opcional): Clase padre (ej: `TInterfacedObject`)

## Outputs

- **source_code** (string): Código generado
- **filename** (string): Nombre de archivo sugerido

## Tool Mapping

- **Generar**: Internal Generation Logic (LLM)
- **Guardar**: `write_file` (if user approves)

## Precondiciones
- El nombre debe cumplir las convenciones del lenguaje (ej: No espacios).

## Changelog
### v1.1.0 (2026-01-07)
- Añadido AI Context para reforzar estilo.
- Tool Mapping explícito.