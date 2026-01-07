---
name: delphi-vcl-context
version: 1.1.0
category: domain/delphi
tags: [delphi, vcl, ui, forms, devexpress, tms]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 600-900
type: context
activation: auto
---

# Contexto VCL y UI Delphi

## Descripción

Contexto especializado en el framework VCL (Visual Component Library), diseño de formularios, manejo de componentes y librerías de terceros (DevExpress, TMS, Skia).

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Delphi UI Expert. Focus on VCL Best Practices (ownership, modal forms, event handling). Support DevExpress (cxGrid) and TMS components.
> **OUTPUT_FORMAT**: DFM structure knowledge + Pascal UI logic.

## Base de Conocimiento

### VCL Best Practices
- **Manejo de Formularios**: Uso de `try..finally` para `Free`. Evitar variables globales de formulario (`Application.CreateForm` solo para el Main).
- **High DPI**: Awareness activado en Delphi 11+. Uso de `VirtualImageList`.
- **Custom Painting**: Overriding de `Paint` en componentes custom.

### Componentes de Terceros
- **DevExpress**: `cxGrid` (vistas, columnas, data-binding runtime), `dxBarManager`.
- **TMS**: `AdvStringGrid`, `AdvEdit`, validación visual.
- **Skia4Delphi**: Renderizado gráfico de alto rendimiento (integrado en D12+).

### Reportes
- **FastReport**: `TfrxReport`, paso de parámetros, exportación a PDF.

## Tool Mapping

- **Generación UI**: `generate-crud-forms`
- **Análisis Formulario**: (En desarrollo)

## Changelog

### v1.1.0 (2026-01-07)
- Extracción desde master context.
- Foco en VCL, Terceros y Reportes.
