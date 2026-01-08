---
name: generate-data-export
version: 1.0.0
category: domain/database
tags: [exportar, excel, csv, json, xml]
author: ARAINFORIA
created: 2026-01-08
complexity: 4
triggers:
  - "exportar excel"
  - "generar csv"
  - "sacar datos"
  - "exportar json"
---

# Generar Exportación de Datos

## Descripción

Genera código para exportar datos a diferentes formatos (CSV, JSON, XML, Excel).

## Exportar a CSV

```pascal
procedure ExportarCSV(Query: TDataSet; const FileName: string;
  Separador: Char = ';'; ConCabeceras: Boolean = True);
var
  F: TextFile;
  I: Integer;
  Linea: string;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    // Cabeceras
    if ConCabeceras then
    begin
      Linea := '';
      for I := 0 to Query.FieldCount - 1 do
      begin
        if I > 0 then Linea := Linea + Separador;
        Linea := Linea + '"' + Query.Fields[I].FieldName + '"';
      end;
      WriteLn(F, Linea);
    end;
    
    // Datos
    Query.First;
    while not Query.Eof do
    begin
      Linea := '';
      for I := 0 to Query.FieldCount - 1 do
      begin
        if I > 0 then Linea := Linea + Separador;
        Linea := Linea + '"' + StringReplace(
          Query.Fields[I].AsString, '"', '""', [rfReplaceAll]) + '"';
      end;
      WriteLn(F, Linea);
      Query.Next;
    end;
  finally
    CloseFile(F);
  end;
end;
```

## Exportar a JSON (mORMot2)

```pascal
uses
  mormot.core.json;

function ExportarJSON(Query: TDataSet): RawUtf8;
var
  Arr: TDocVariantData;
  Row: TDocVariantData;
  I: Integer;
begin
  Arr.InitArray([]);
  
  Query.First;
  while not Query.Eof do
  begin
    Row.InitObject([]);
    for I := 0 to Query.FieldCount - 1 do
      Row.AddValue(Query.Fields[I].FieldName, 
                   Query.Fields[I].AsVariant);
    Arr.AddItem(Variant(Row));
    Query.Next;
  end;
  
  Result := Arr.ToJson;
end;
```

## Exportar a Excel (OLE)

```pascal
uses
  ComObj;

procedure ExportarExcel(Query: TDataSet; const FileName: string);
var
  Excel, Workbook, Sheet: Variant;
  Row, Col: Integer;
begin
  Excel := CreateOleObject('Excel.Application');
  try
    Workbook := Excel.Workbooks.Add;
    Sheet := Workbook.Worksheets[1];
    
    // Cabeceras
    for Col := 0 to Query.FieldCount - 1 do
      Sheet.Cells[1, Col + 1] := Query.Fields[Col].FieldName;
    
    // Datos
    Row := 2;
    Query.First;
    while not Query.Eof do
    begin
      for Col := 0 to Query.FieldCount - 1 do
        Sheet.Cells[Row, Col + 1] := Query.Fields[Col].AsVariant;
      Inc(Row);
      Query.Next;
    end;
    
    Workbook.SaveAs(FileName);
  finally
    Excel.Quit;
  end;
end;
```

---

**Estado**: stable
