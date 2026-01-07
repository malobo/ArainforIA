---
name: delphi-db-context
version: 1.1.0
category: domain/delphi
tags: [delphi, database, sql, bde, firedac, repository]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 700-1000
type: context
activation: auto
---

# Contexto de Base de Datos Delphi

## Descripción

Contexto especializado en acceso a datos, transacciones y arquitectura de persistencia. Soporta tanto legado (BDE/Paradox) como moderno (FireDAC).

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Delphi DB Architect. Support BDE/Paradox (Legacy) and FireDAC (Modern). Enforce Repository and Unit of Work patterns. SQL optimization for desktop DBs.
> **OUTPUT_FORMAT**: Robust DB code with error handling and transactions.

## Base de Conocimiento

### Acceso a Datos
- **BDE (Legacy)**: `TTable`, `TQuery`, `TDatabase`. Uso de alias BDE. Manejo de bloqueos en Paradox.
- **FireDAC (Modern)**: `TFDQuery`, `TFDTable`, `TFDConnection`. Array DML para inserciones masivas.
- **Transacciones**: `StartTransaction`, `Commit`, `Rollback`.

### Arquitectura de Datos
- **Repository Pattern**: Abstracción de la base de datos para desacoplar la lógica de negocio.
- **Unit of Work**: Gestión de transacciones complejas que afectan a múltiples repositorios.
- **Data Modules**: Organización de componentes de datos centralizados.

### Migración
- Estrategias para mover lógica de `DBTables` (BDE) a `FireDAC.Comp.Client`.

## Tool Mapping

- **Migración**: `create-database-migration`
- **Generación CRUD**: `generate-crud-forms`

## Changelog

### v1.1.0 (2026-01-07)
- Extracción desde master context.
- Inclusión de patrones Repository/UoW.
