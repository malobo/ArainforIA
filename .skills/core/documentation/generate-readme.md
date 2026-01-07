---
name: generate-readme
version: 1.1.0
category: core/documentation
tags: [documentation, readme, markdown, auto-doc]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 3
estimated_tokens: 400-600
---

# Generar README

## Descripción

Genera documentación de alto nivel (README.md) para proyectos o carpetas, analizando la estructura de archivos y el propósito del código.

## Objetivo

Proporcionar documentación clara, profesional y estandarizada que explique "Qué es", "Cómo instalar" y "Cómo usar" el proyecto.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Technical Writer. Analyze file structure (`tree`). Identify tech stack. Write concise, professional Markdown. Include: Title, Description, Installation, Usage.
> **OUTPUT_FORMAT**: Markdown (GitHub flavor).

## Inputs

- **path** (string): Ruta del directorio a documentar
- **focus** (string, opcional): [developer|user] (default: developer)

## Outputs

- **markdown_content** (string): Contenido del README

## Tool Mapping

- **Analizar Estructura**: `list_directory` (recursive)
- **Leer Archivos Clave**: `read_file` (package.json, .dpr, requirements.txt)
- **Escribir**: `write_file` (README.md)

## Changelog
### v1.1.0 (2026-01-07)
- AI Context de Technical Writer.
- Optimización de escaneo de archivos.