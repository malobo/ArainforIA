---
name: sync-mobile-data
version: 1.0.0
category: workflows/development
tags: [sincronizacion, movil, offline, hibrido]
author: ARAINFORIA
created: 2026-01-08
complexity: 8
triggers:
  - "sincronizar movil"
  - "datos offline"
  - "sync app"
  - "app movil"
---

# Sincronización Datos Móvil ↔ Desktop

## Descripción

Workflow para sincronizar datos entre aplicación móvil y backend Delphi.

## Arquitectura

```text
┌─────────────┐         ┌─────────────┐         ┌─────────────┐
│  App Móvil  │◀──────▶│  API REST   │◀──────▶│  BD Local   │
│  (Offline)  │  Sync   │  (mORMot)   │         │  (Paradox)  │
└─────────────┘         └─────────────┘         └─────────────┘
```

## Tabla de Sincronización

```sql
CREATE TABLE SYNC_LOG (
  ID            INTEGER PRIMARY KEY,
  TABLA         VARCHAR(50),
  ID_REGISTRO   INTEGER,
  OPERACION     VARCHAR(10),  -- INSERT, UPDATE, DELETE
  TIMESTAMP_LOCAL TIMESTAMP,
  TIMESTAMP_SYNC  TIMESTAMP,
  ESTADO        VARCHAR(20),  -- PENDIENTE, SINCRONIZADO, ERROR
  CONFLICTO     BOOLEAN
);
```

## Estrategia de Sincronización

### 1. Timestamp-based Sync

```pascal
type
  TSyncRequest = record
    UltimaSync: TDateTime;
    Tabla: RawUtf8;
  end;

  TSyncResponse = record
    Registros: TVariantDynArray;
    UltimaSync: TDateTime;
    TotalCambios: Integer;
  end;

function GetCambiosDesde(const Tabla: string; 
  Desde: TDateTime): TSyncResponse;
begin
  Query.SQL.Text := 
    'SELECT * FROM ' + Tabla + 
    ' WHERE FechaModificacion > :Desde';
  Query.ParamByName('Desde').AsDateTime := Desde;
  Query.Open;
  // ... llenar Response
end;
```

### 2. Resolución de Conflictos

```pascal
type
  TConflictResolution = (
    crServerWins,   // Servidor tiene prioridad
    crClientWins,   // Cliente tiene prioridad
    crNewerWins,    // El más reciente gana
    crManual        // Requiere intervención
  );

function ResolverConflicto(const Local, Remoto: TRegistro;
  Estrategia: TConflictResolution): TRegistro;
begin
  case Estrategia of
    crServerWins: Result := Remoto;
    crClientWins: Result := Local;
    crNewerWins:
      if Local.FechaModificacion > Remoto.FechaModificacion then
        Result := Local
      else
        Result := Remoto;
  end;
end;
```

### 3. API de Sincronización

```pascal
// Endpoint: POST /api/sync
function TApiServer.PostSync(Ctxt: TRestServerUriContext): Integer;
var
  Request: TSyncRequest;
  Response: TSyncResponse;
begin
  RecordLoadJson(Request, Ctxt.InputUtf8, TypeInfo(TSyncRequest));
  
  Response := GetCambiosDesde(Request.Tabla, Request.UltimaSync);
  
  // Procesar cambios del cliente
  ProcesarCambiosCliente(Ctxt.InputUtf8);
  
  Ctxt.Returns(RecordSaveJson(Response, TypeInfo(TSyncResponse)));
  Result := HTTP_SUCCESS;
end;
```

## Checklist de Implementación

- [ ] Añadir campo `FechaModificacion` a todas las tablas
- [ ] Crear tabla `SYNC_LOG`
- [ ] Implementar endpoints de sync
- [ ] Definir estrategia de conflictos
- [ ] Manejar modo offline

---

**Estado**: stable
