---
id: skill-migrate-bde-firedac
name: Migración BDE a FireDAC
version: 1.0.0
category: workflows/migration
priority: high
last_updated: 2026-01-08
triggers:
  - "migrar bde"
  - "eliminar paradox"
  - "modernizar datos"
  - "usar firedac"
---

# 🚀 Workflow: Migración BDE a FireDAC

<context>
Este workflow detalla los pasos seguros para migrar componentes de acceso a datos legacy (TTable, TQuery del BDE) a la suite moderna FireDAC (TFDTable, TFDQuery), eliminando la dependencia del BDE Administrator.
</context>

<instruction>
El proceso de migración debe seguir estas fases:

## 1. Preparación de Conexión

* Reemplazar `TDatabase` por `TFDConnection`.
* Configurar el driver (SQLite, MSSQL, MySQL) en `TFDPhys...DriverLink`.
* Añadir `TFDGUIxWaitCursor`.

## 2. Reemplazo de Componentes

| BDE Component | FireDAC Component | Notas |
| :--- | :--- | :--- |
| `TTable` | `TFDTable` | Puede requerir cambiar `TableName`. |
| `TQuery` | `TFDQuery` | Verificar sintaxis SQL (ver `skill-convert-sql-paradox`). |
| `TStoredProc` | `TFDStoredProc` | |
| `TUpdateSQL` | `TFDUpdateSQL` | |

## 3. Ajuste de Código

* **Mapeo de Tipos**: Revisar campos `TFloatField` vs `TFMTBCDField`. FireDAC es más estricto con la precisión numérico.
* **Transacciones**: FireDAC maneja transacciones de forma diferente. Usar `TFDTransaction` explícito si es necesario, o `TxOptions`.
* **CachedUpdates**: FireDAC usa `CachedUpdates` de forma similar, verificar `ApplyUpdates`.

## 4. Limpieza

* Eliminar `BDE` de la cláusula `uses`.
* Eliminar referencias a `DBTables`.
</instruction>

<examples>
User: "Ayúdame a migrar este DataModule que usa TQuery"
Agent: "Perfecto. Primero, añade un TFDConnection configurado a tu DataModule. Luego, reemplazaremos los TQuery uno por uno. ¿Puedes mostrarme el primer TQuery y su SQL?"
</examples>
