---
name: implement-audit-trail
version: 1.0.0
category: domain/database
tags: [auditoria, historial, log, trazabilidad]
author: ARAINFORIA
created: 2026-01-08
complexity: 6
triggers:
  - "auditar cambios"
  - "historial registros"
  - "log modificaciones"
  - "trazabilidad"
---

# Implementar Auditoría de Cambios

## Descripción

Implementa sistema de auditoría para registrar cambios en tablas.

## Estructura de Tabla Auditoría

```sql
CREATE TABLE AUD_LOG (
  ID          INTEGER PRIMARY KEY,
  TABLA       VARCHAR(50),
  OPERACION   VARCHAR(10),  -- INSERT, UPDATE, DELETE
  ID_REGISTRO INTEGER,
  CAMPO       VARCHAR(50),
  VALOR_ANT   VARCHAR(255),
  VALOR_NUE   VARCHAR(255),
  USUARIO     VARCHAR(50),
  FECHA       TIMESTAMP,
  EQUIPO      VARCHAR(50)
);
```

## Clase de Auditoría

```pascal
type
  TAuditoria = class
  private
    FUsuario: string;
    FEquipo: string;
    FQuery: TQuery;
  public
    constructor Create(AQuery: TQuery);
    
    procedure RegistrarCambio(const Tabla, Operacion: string;
      IdRegistro: Integer; const Campo, ValorAnt, ValorNuevo: string);
    procedure AuditarDataSet(DS: TDataSet; const Tabla: string);
  end;

constructor TAuditoria.Create(AQuery: TQuery);
begin
  FQuery := AQuery;
  FUsuario := GetWindowsUser;
  FEquipo := GetComputerName;
end;

procedure TAuditoria.RegistrarCambio(const Tabla, Operacion: string;
  IdRegistro: Integer; const Campo, ValorAnt, ValorNuevo: string);
begin
  FQuery.SQL.Text := 
    'INSERT INTO AUD_LOG (TABLA, OPERACION, ID_REGISTRO, ' +
    'CAMPO, VALOR_ANT, VALOR_NUE, USUARIO, FECHA, EQUIPO) ' +
    'VALUES (:T, :O, :ID, :C, :VA, :VN, :U, :F, :E)';
  FQuery.ParamByName('T').AsString := Tabla;
  FQuery.ParamByName('O').AsString := Operacion;
  FQuery.ParamByName('ID').AsInteger := IdRegistro;
  FQuery.ParamByName('C').AsString := Campo;
  FQuery.ParamByName('VA').AsString := Copy(ValorAnt, 1, 255);
  FQuery.ParamByName('VN').AsString := Copy(ValorNuevo, 1, 255);
  FQuery.ParamByName('U').AsString := FUsuario;
  FQuery.ParamByName('F').AsDateTime := Now;
  FQuery.ParamByName('E').AsString := FEquipo;
  FQuery.ExecSQL;
end;

procedure TAuditoria.AuditarDataSet(DS: TDataSet; const Tabla: string);
var
  I: Integer;
  IdReg: Integer;
begin
  IdReg := DS.FieldByName('ID').AsInteger;
  
  case DS.State of
    dsInsert:
      RegistrarCambio(Tabla, 'INSERT', IdReg, '*', '', 'Nuevo registro');
      
    dsEdit:
      for I := 0 to DS.FieldCount - 1 do
        if DS.Fields[I].OldValue <> DS.Fields[I].Value then
          RegistrarCambio(Tabla, 'UPDATE', IdReg,
            DS.Fields[I].FieldName,
            VarToStr(DS.Fields[I].OldValue),
            VarToStr(DS.Fields[I].Value));
  end;
end;
```

## Uso en Eventos

```pascal
procedure TForm1.TableBeforePost(DataSet: TDataSet);
begin
  Auditoria.AuditarDataSet(DataSet, 'CLIENTES');
end;

procedure TForm1.TableBeforeDelete(DataSet: TDataSet);
begin
  Auditoria.RegistrarCambio('CLIENTES', 'DELETE',
    DataSet.FieldByName('ID').AsInteger, '*', 
    'Registro eliminado', '');
end;
```

---

**Estado**: stable
