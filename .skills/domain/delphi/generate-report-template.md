---
id: skill-generate-report-template
name: Generar Plantilla Informe (GmPrintSuite)
version: 1.0.0
category: domain/delphi
priority: medium
last_updated: 2026-01-08
triggers:
  - "crear informe"
  - "plantilla impresion"
  - "reporte nuevo"
  - "gmprintsuite"
---

# 📄 Generar Plantilla Informe (GmPrintSuite)

<context>
Esta skill ayuda a generar el código Pascal necesario para construir un informe programático usando `GmPrintSuite`. A diferencia de QuickReport o FastReport, GmPrintSuite a menudo requiere dibujar elementos (Label, Text, Shape) mediante código o definir una estructura de secciones.
</context>

<instruction>
Al generar código para GmPrintSuite:

1. **Estructura Base**:
    * Utilizar objetos `TGmReport`, `TGmPage`, `TGmBand` (si aplica la versión).
    * Definir unidades en `mm` (`Unidad := gmMillimeters`) para precisión.
2. **Elementos Comunes**:
    * `GmReport.Canvas.TextOut(X, Y, Texto)`
    * `GmReport.Canvas.Rectangle(Reg)`
3. **Iteración de Datos**:
    * Generar bucle `while not DataSet.Eof` para imprimir líneas de detalle, controlando el salto de página manual si es necesario.

</instruction>

<examples>
User: "Crea un informe de lista de clientes para GmPrintSuite"
Agent:
```delphi
procedure TFrMain.ImprimirListaClientes;
var
  Y: Double;
begin
  GmReport1.BeginDoc;
  try
    GmReport1.Canvas.Font.Name := 'Arial';
    GmReport1.Canvas.Font.Size := 10;
    Y := 10;

    // Cabecera
    GmReport1.Canvas.Font.Style := [fsBold];
    GmReport1.Canvas.TextOut(10, Y, 'LISTADO DE CLIENTES');
    Y := Y + 10;
    
    // Detalle
    GmReport1.Canvas.Font.Style := [];
    TableClientes.First;
    while not TableClientes.Eof do
    begin
      if Y > 280 then // Control salto página A4
      begin
        GmReport1.NewPage;
        Y := 10;
      end;
      
      GmReport1.Canvas.TextOut(10, Y, TableClientes.FieldByName('CODIGO').AsString);
      GmReport1.Canvas.TextOut(30, Y, TableClientes.FieldByName('NOMBRE').AsString);
      
      Y := Y + 5;
      TableClientes.Next;
    end;
  finally
    GmReport1.EndDoc;
  end;
end;

```
</examples>
