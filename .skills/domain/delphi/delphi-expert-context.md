---
name: delphi-expert-context
version: 1.1.0
category: domain/delphi
tags: [delphi, expert, master, context]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 400-600
type: context
activation: auto
---

# Contexto Maestro Delphi Expert

## Descripción

Este es el punto de entrada maestro para el conocimiento experto en Delphi. En lugar de cargar todo el conocimiento a la vez, este contexto orquesta el acceso a módulos especializados.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Master Delphi Architect. Identify the specific domain needed (Core, VCL, DB, or Verifactu) and reference the specialized context skill.
> **OUTPUT_FORMAT**: Orchestrated response using specialized knowledge.

## Módulos Especializados

Para optimizar el uso de tokens, el conocimiento se ha dividido en los siguientes contextos:

1. **`delphi-core-context`**: Sintaxis, RTL, Records, Generics, Convenciones.
    * *Uso*: Dudas de lenguaje, refactorización core, algoritmos.
2. **`delphi-vcl-context`**: Formularios, Componentes, UI (DevExpress, FastReport).
    * *Uso*: Diseño de interfaces, manejo de eventos visuales, reportes.
3. **`delphi-db-context`**: Persistencia, BDE/Paradox, FireDAC, SQL, Repositorios.
    * *Uso*: Consultas, transacciones, diseño de base de datos.
4. **`delphi-cloud-context`**: Integración HTTP, REST API, JSON, FireDAC MySQL.
    * *Uso*: Conexión con APIs PHP, subida de datos a la nube, consumo de servicios web.
5. **`delphi-verifactu-context`**: Normativa RD 1007/2023, Hashing, QR, XML AEAT.
    * *Uso*: Cumplimiento legal, integridad de facturas, firma digital.

## Activación Inteligente

* Si trabajas con **UI**: Carga `delphi-vcl-context`.
* Si trabajas con **Datos Locales**: Carga `delphi-db-context`.
* Si trabajas con **Nube/API**: Carga `delphi-cloud-context`.
* Si trabajas con **Lógica Core**: Carga `delphi-core-context`.
* Si trabajas en **Verifactu**: Carga `delphi-verifactu-context`.

## Changelog

### v1.1.0 (2026-01-07)

- **OPTIMIZACIÓN CRÍTICA**: El contexto se ha modularizado para ahorrar tokens.
* Convertido en "Master Router".
* Referencia a 4 nuevos sub-contextos especializados.
