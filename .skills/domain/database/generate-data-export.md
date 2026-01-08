---
id: skill-generate-data-export
name: Generar Exportación Datos (Excel/CSV)
version: 1.0.0
category: domain/database
priority: medium
last_updated: 2026-01-08
triggers:
  - "exportar excel"
  - "generar csv"
  - "sacar datos"
  - "exportar query"
---

# 📤 Generar Exportación de Datos

<context>
Esta skill genera código Delphi para exportar datasets (TQuery/TTable) a formatos comunes de intercambio, principalmente CSV (texto) y Excel (vía OLE o CSV compatible).
</context>

<instruction>
Al generar código de exportación:

1. **Formato CSV (Rápido y Compatible)**:
    * Usar `TStringList`.
    * Iterar DataSet.
    * Concatenar campos con `;` o `,`.
    * Escapar comillas dobles si es necesario.
2. **Excel OLE (Lento pero con Formato)**:
    * Usar `ComObj`, `CreateOleObject('Excel.Application')`.
    * Solo recomendar para reportes estéticos, no para volúmenes grandes de datos.
3. **Rendimiento**:
    * Usar `DisableControls` / `EnableControls` en el DataSet antes de iterar.

</instruction>

<examples>
User: "Genera una exportación a CSV para la tabla de Facturas"
Agent:
```delphi
procedure ExportarFacturasCSV(Dataset: TDataSet; const FileName: string);
var
  SL: TStringList;
  Line: string;
begin
  SL := TStringList.Create;
  try
    Dataset.DisableControls;
    try
      Dataset.First;
      // Cabecera
      SL.Add('ID;FECHA;CLIENTE;TOTAL');

      // Datos
      while not Dataset.Eof do
      begin
        Line := Format('%d;%s;%s;%f', [
          Dataset.FieldByName('ID').AsInteger,
          DateToStr(Dataset.FieldByName('FECHA').AsDateTime),
          Dataset.FieldByName('CLIENTE').AsString,
          Dataset.FieldByName('TOTAL').AsFloat
        ]);
        SL.Add(Line);
        Dataset.Next;
      end;
    finally
      Dataset.EnableControls;
    end;
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

```
</examples>
