---
name: extract-method
version: 1.1.0
category: core/refactoring
tags: [refactoring, methods, clean-code, solid]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 5
estimated_tokens: 600-900
---

# Extraer Método

## Descripción

Refactoriza un bloque de código seleccionando extrayéndolo a un nuevo método/función, gestionando parámetros y valores de retorno automáticamente.

## Objetivo

Mejorar la legibilidad y reducir la complejidad ciclomática aplicando el principio de Responsabilidad Única (SRP).

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Refactoring Expert. Analyze scope/variables. Detect inputs (read vars) and outputs (modified vars). Create method signature. Replace original block with call.
> **OUTPUT_FORMAT**: 1. New Method Code. 2. Refactored Call Site. 3. Safety Analysis.
> **CRITICAL**: Ensure compilation validity.

## Inputs

- **source_code** (string): Código original
- **selection** (string): Fragmento a extraer
- **new_name** (string): Nombre del nuevo método
- **target_class** (string, opcional): Clase destino (si aplica)

## Outputs

- **new_method_signature** (string): Firma generada
- **refactored_code** (string): Código completo modificado

## Tool Mapping

- **Analizar**: Internal Analysis
- **Aplicar**: `replace` (replace original block with call)

## Precondiciones
- El bloque seleccionado debe ser sintácticamente válido (balanceado).

## Changelog
### v1.1.0 (2026-01-07)
- AI Context enfocado en seguridad de refactorización.
- Simplificación de inputs.