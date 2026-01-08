---
name: generate-exception-handler
version: 1.0.0
category: domain/delphi
tags: [exception, try, except, finally, errores]
author: ARAINFORIA
created: 2026-01-08
complexity: 4
triggers:
  - "try except"
  - "manejar error"
  - "capturar excepcion"
  - "finally"
---

# Generar Exception Handler

## Descripción

Templates para manejo estructurado de excepciones en Delphi.

## Templates Estándar

### Try-Except Básico

```pascal
try
  // Código que puede fallar
  Operacion;
except
  on E: Exception do
  begin
    // Manejar error
    ShowMessage('Error: ' + E.Message);
    // O re-lanzar
    raise;
  end;
end;
```

### Try-Finally (Recursos)

```pascal
Lista := TStringList.Create;
try
  Lista.LoadFromFile('datos.txt');
  ProcesarLista(Lista);
finally
  Lista.Free; // SIEMPRE se ejecuta
end;
```

### Try-Except-Finally Combinado

```pascal
Lista := TStringList.Create;
try
  try
    Lista.LoadFromFile('datos.txt');
    ProcesarLista(Lista);
  except
    on E: Exception do
      LogError(E.Message);
  end;
finally
  Lista.Free;
end;
```

### Excepciones Múltiples

```pascal
try
  ConectarBD;
  EjecutarQuery;
except
  on E: EDatabaseError do
    ShowMessage('Error de BD: ' + E.Message);
  on E: EConvertError do
    ShowMessage('Error de conversión: ' + E.Message);
  on E: Exception do
    ShowMessage('Error inesperado: ' + E.Message);
end;
```

### Excepción Personalizada

```pascal
type
  EVerifactuError = class(Exception)
  public
    Codigo: Integer;
    constructor Create(const Msg: string; ACodigo: Integer);
  end;

constructor EVerifactuError.Create(const Msg: string; ACodigo: Integer);
begin
  inherited Create(Msg);
  Codigo := ACodigo;
end;

// Uso
raise EVerifactuError.Create('NIF inválido', 1001);
```

### Función con Resultado + Error

```pascal
type
  TOperationResult = record
    Success: Boolean;
    ErrorMsg: string;
    Data: Variant;
  end;

function OperacionSegura: TOperationResult;
begin
  Result.Success := False;
  Result.ErrorMsg := '';
  try
    Result.Data := RealizarOperacion;
    Result.Success := True;
  except
    on E: Exception do
      Result.ErrorMsg := E.Message;
  end;
end;
```

### Handler Global (Application)

```pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.OnException := AppException;
end;

procedure TForm1.AppException(Sender: TObject; E: Exception);
begin
  LogError(E.ClassName + ': ' + E.Message);
  ShowMessage('Ha ocurrido un error. Ver log para detalles.');
end;
```

---

**Estado**: stable
