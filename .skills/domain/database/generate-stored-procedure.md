---
name: generate-stored-procedure
version: 1.0.0
category: domain/database
tags: [sql, stored, procedure, funcion, trigger]
author: ARAINFORIA
created: 2026-01-08
complexity: 5
triggers:
  - "stored procedure"
  - "procedimiento sql"
  - "crear funcion sql"
  - "trigger"
---

# Generar Stored Procedure

## Descripción

Templates para crear procedimientos almacenados, funciones y triggers SQL.

## Stored Procedure Básico (SQL Server / Firebird)

```sql
CREATE PROCEDURE SP_ACTUALIZAR_CLIENTE (
  @ID_CLIENTE INTEGER,
  @NOMBRE VARCHAR(100),
  @NIF VARCHAR(15)
)
AS
BEGIN
  UPDATE CLIENTES
  SET NOMBRE = @NOMBRE,
      NIF = @NIF,
      FECHA_MODIFICACION = GETDATE()
  WHERE ID = @ID_CLIENTE;
END;
```

## Función SQL

```sql
CREATE FUNCTION FN_CALCULAR_IVA (
  @BASE DECIMAL(18,2),
  @PORCENTAJE DECIMAL(5,2)
)
RETURNS DECIMAL(18,2)
AS
BEGIN
  RETURN @BASE * @PORCENTAJE / 100;
END;
```

## Llamar desde Delphi

```pascal
procedure TDataModule.ActualizarCliente(Id: Integer; 
  const Nombre, NIF: string);
begin
  Query.SQL.Text := 'EXEC SP_ACTUALIZAR_CLIENTE :ID, :NOMBRE, :NIF';
  Query.ParamByName('ID').AsInteger := Id;
  Query.ParamByName('NOMBRE').AsString := Nombre;
  Query.ParamByName('NIF').AsString := NIF;
  Query.ExecSQL;
end;

function TDataModule.CalcularIVA(Base, Porcentaje: Currency): Currency;
begin
  Query.SQL.Text := 'SELECT dbo.FN_CALCULAR_IVA(:BASE, :PORC) AS RESULTADO';
  Query.ParamByName('BASE').AsCurrency := Base;
  Query.ParamByName('PORC').AsCurrency := Porcentaje;
  Query.Open;
  Result := Query.FieldByName('RESULTADO').AsCurrency;
end;
```

## Trigger de Auditoría

```sql
CREATE TRIGGER TR_CLIENTES_AUDIT
ON CLIENTES
AFTER INSERT, UPDATE, DELETE
AS
BEGIN
  -- Para INSERT
  IF EXISTS (SELECT 1 FROM inserted) AND NOT EXISTS (SELECT 1 FROM deleted)
  BEGIN
    INSERT INTO AUD_LOG (TABLA, OPERACION, REGISTRO_ID, FECHA)
    SELECT 'CLIENTES', 'INSERT', ID, GETDATE() FROM inserted;
  END
  
  -- Para UPDATE
  IF EXISTS (SELECT 1 FROM inserted) AND EXISTS (SELECT 1 FROM deleted)
  BEGIN
    INSERT INTO AUD_LOG (TABLA, OPERACION, REGISTRO_ID, FECHA)
    SELECT 'CLIENTES', 'UPDATE', ID, GETDATE() FROM inserted;
  END
  
  -- Para DELETE
  IF NOT EXISTS (SELECT 1 FROM inserted) AND EXISTS (SELECT 1 FROM deleted)
  BEGIN
    INSERT INTO AUD_LOG (TABLA, OPERACION, REGISTRO_ID, FECHA)
    SELECT 'CLIENTES', 'DELETE', ID, GETDATE() FROM deleted;
  END
END;
```

---

**Estado**: stable
