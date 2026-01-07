---
name: delphi-core-context
version: 1.1.0
category: domain/delphi
tags: [delphi, language, syntax, object-pascal]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 600-900
type: context
activation: auto
---

# Contexto de Lenguaje Delphi (Core)

## Descripción

Contexto especializado en la sintaxis, características del lenguaje Object-Pascal y convenciones de código para Delphi 11, 12 y 13.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Delphi Language Expert. Focus on modern syntax (inline variables, records, generics, anonymous methods). Enforce standard naming conventions (T, I, F, A prefixes).
> **OUTPUT_FORMAT**: Standard Pascal code following ARAINFORIA style.

## Base de Conocimiento

### Versiones y Características
- **Delphi 11 Alexandria**: Inline variables, Type inference, Custom managed records, Binary literals.
- **Delphi 12 Athens**: Multiline strings (heredocs), For-in loop improvements, JSON improvements.
- **Delphi 13**: Mantener compatibilidad 11+; mejoras en LSP y compilador.

### Sintaxis Moderna
- **Inline Variables**: `var I := 10;` (dentro de `begin..end`).
- **Generics**: `TList<T>`, `TArray<T>`, Constraints.
- **Anonymous Methods**: `reference to procedure(const S: string);`.
- **Custom Managed Records**: Operadores `Initialize`, `Finalize`, `Assign`.

### Convenciones (Style Guide)
- **Prefijos**: `T` (Clase/Tipo), `I` (Interface), `F` (Campo privado), `A` (Parámetro), `E` (Excepción).
- **Indentación**: 2 espacios.
- **Estructura**: `interface`, `implementation`, `initialization`, `finalization`.

## Tool Mapping

- **Análisis**: `analyze-delphi-unit`
- **Generación**: `generate-boilerplate`

## Changelog

### v1.1.0 (2026-01-07)
- Extracción desde master context.
- Actualización a formato v1.1.
- Foco en Core RTL y Sintaxis.
