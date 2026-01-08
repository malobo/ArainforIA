---
name: generate-report-template
version: 1.0.0
category: domain/database
tags: [informe, reporte, listado, gmprint]
author: ARAINFORIA
created: 2026-01-08
complexity: 5
triggers:
  - "crear informe"
  - "plantilla impresion"
  - "reporte nuevo"
  - "listado clientes"
---

# Generar Plantilla de Informe

## Descripción

Genera código para informes/listados genéricos con GmPrintSuite.

## Template Base

```pascal
procedure GenerarListado(Visor: TGmPreview; Query: TDataSet;
  const Titulo: string; const Columnas: array of string);
var
  Y: Extended;
  Anchos: array of Integer;
  TotalAncho, I: Integer;
begin
  // Calcular anchos proporcionales
  TotalAncho := 190; // A4 menos márgenes
  SetLength(Anchos, Length(Columnas));
  for I := 0 to High(Columnas) do
    Anchos[I] := TotalAncho div Length(Columnas);
    
  Visor.BeginDoc;
  try
    Y := 10;
    
    // Título
    Visor.Canvas.Font.Size := 14;
    Visor.Canvas.Font.Style := [fsBold];
    Visor.TextOutCenter(105, Y, Titulo);
    Y := Y + 10;
    
    // Cabecera de columnas
    Visor.Canvas.Font.Size := 9;
    Visor.Canvas.Brush.Color := clLtGray;
    Visor.FillRect(10, Y, 200, Y + 6);
    
    var X: Extended := 10;
    for I := 0 to High(Columnas) do
    begin
      Visor.TextOut(X + 2, Y + 1, Columnas[I]);
      X := X + Anchos[I];
    end;
    Y := Y + 8;
    
    // Datos
    Visor.Canvas.Brush.Style := bsClear;
    Visor.Canvas.Font.Style := [];
    
    Query.First;
    while not Query.Eof do
    begin
      X := 10;
      for I := 0 to Query.FieldCount - 1 do
      begin
        if I <= High(Anchos) then
        begin
          Visor.TextOut(X + 2, Y, Query.Fields[I].AsString);
          X := X + Anchos[I];
        end;
      end;
      Y := Y + 5;
      
      // Salto de página
      if Y > 280 then
      begin
        Visor.NewPage;
        Y := 20;
      end;
      
      Query.Next;
    end;
    
    // Pie con fecha
    Visor.Canvas.Font.Size := 7;
    Visor.TextOut(10, 290, 'Generado: ' + DateTimeToStr(Now));
    
  finally
    Visor.EndDoc;
  end;
end;
```

## Uso

```pascal
GenerarListado(GmPreview1, qryClientes, 
  'Listado de Clientes',
  ['Código', 'Nombre', 'NIF', 'Teléfono']);
```

---

**Estado**: stable
