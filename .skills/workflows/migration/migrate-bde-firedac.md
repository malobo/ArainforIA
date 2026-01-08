---
name: migrate-bde-firedac
version: 1.0.0
category: workflows/migration
tags: [migracion, bde, firedac, paradox, modernizar]
author: ARAINFORIA
created: 2026-01-08
complexity: 8
triggers:
  - "migrar bde"
  - "eliminar paradox"
  - "modernizar datos"
  - "firedac migracion"
---

# Migración BDE → FireDAC

## Descripción

Workflow paso a paso para migrar de BDE/Paradox a FireDAC.

## Fases del Workflow

### Fase 1: Preparación

1. **Inventario de componentes BDE**

   ```text
   Buscar en el proyecto:
   - TTable → TFDTable
   - TQuery → TFDQuery
   - TDatabase → TFDConnection
   - TSession → (no necesario)
   - TBatchMove → TFDBatchMove
   ```

2. **Backup completo**

   ```text
   - Copiar todos los archivos .pas, .dfm
   - Exportar todas las tablas .DB a SQL/CSV
   ```

### Fase 2: Mapeo de Componentes

| BDE | FireDAC | Notas |
| --- | ------- | ----- |
| `TTable` | `TFDTable` | Casi 1:1 |
| `TQuery` | `TFDQuery` | Cambios menores |
| `TDatabase` | `TFDConnection` | Configuración diferente |
| `TDataSource` | `TDataSource` | Sin cambios |
| `TDBGrid` | `TDBGrid` | Sin cambios |

### Fase 3: Cambios en Uses

```pascal
// Antes (BDE)
uses
  DBTables, BDE;

// Después (FireDAC)
uses
  FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Comp.Client,
  FireDAC.Comp.DataSet;
```

### Fase 4: Configurar Conexión

```pascal
// FireDAC para Paradox
FDConnection1.DriverName := 'ODBC';
FDConnection1.Params.Values['DataSource'] := 'ParadoxDSN';

// O usar driver nativo (si disponible)
FDConnection1.DriverName := 'Paradox';
FDConnection1.Params.Values['DatabaseName'] := 'C:\MisDatos';
```

### Fase 5: Cambios de Código

```pascal
// ANTES (BDE)
Table1.DatabaseName := 'ALIAS_BD';
Table1.TableName := 'CLIENTES.DB';
Table1.Open;

// DESPUÉS (FireDAC)
FDTable1.Connection := FDConnection1;
FDTable1.TableName := 'CLIENTES';
FDTable1.Open;
```

```pascal
// ANTES (BDE Query)
Query1.DatabaseName := 'ALIAS_BD';
Query1.SQL.Text := 'SELECT * FROM Clientes';
Query1.Open;

// DESPUÉS (FireDAC)
FDQuery1.Connection := FDConnection1;
FDQuery1.SQL.Text := 'SELECT * FROM Clientes';
FDQuery1.Open;
```

### Fase 6: Verificación

- [ ] Todas las tablas abren correctamente
- [ ] Las consultas devuelven datos
- [ ] Los filtros funcionan
- [ ] Las inserciones/actualizaciones funcionan
- [ ] Los reportes se generan correctamente

## Problemas Comunes

| Problema | Solución |
| -------- | -------- |
| "Table does not exist" | Verificar TableName sin extensión |
| "Driver not found" | Instalar driver ODBC Paradox |
| "Field type mismatch" | Revisar tipos de datos |

---

**Estado**: stable
