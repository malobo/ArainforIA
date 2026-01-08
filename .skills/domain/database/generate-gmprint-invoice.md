---
name: generate-gmprint-invoice
version: 1.0.0
category: domain/database
tags: [gmprint, impresion, factura, ticket, reporte]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 5
estimated_tokens: 500-700
triggers:
  - "imprimir factura"
  - "gmprint template"
  - "formato impresion"
  - "diseño informe"
  - "ticket venta"
---

# Generar Plantilla GmPrintSuite

## Descripción

Genera código para imprimir facturas y tickets usando GmPrintSuite (GmGridPrint/GmPreview).

## AI Context

> **SYSTEM_INSTRUCTION**: Generate GmPrintSuite printing code for invoices and reports.
> **OUTPUT_FORMAT**: Complete Delphi procedure with positioning and formatting.
> **TOKEN_STRATEGY**: Provide reusable template, explain coordinate system.

## Sistema de Coordenadas GmPrint

```text
┌─────────────────────────────────────────┐
│ (0,0)                          (210,0)  │  ← Unidades en mm (A4)
│                                         │
│     MargenIzq = 10mm                    │
│     ┌───────────────────────────┐       │
│     │  ÁREA IMPRIMIBLE          │       │
│     │  (10,10) a (200,287)      │       │
│     └───────────────────────────┘       │
│                                         │
│ (0,297)                      (210,297)  │
└─────────────────────────────────────────┘
```

## Template: Factura Completa

```pascal
procedure ImprimirFactura(Visor: TGmPreview; const Factura: TFactura);
var
  Y: Extended;
  LineaActual: Integer;
begin
  Visor.BeginDoc;
  try
    // === CABECERA ===
    Y := 10; // Empezar a 10mm del borde superior
    
    // Logo (si existe)
    if FileExists(RutaLogo) then
      Visor.Draw(10, Y, RutaLogo, 30, 15); // X, Y, archivo, ancho, alto
    
    // Datos empresa emisora
    Visor.Canvas.Font.Size := 14;
    Visor.Canvas.Font.Style := [fsBold];
    Visor.TextOut(50, Y, 'NOMBRE EMPRESA S.L.');
    
    Visor.Canvas.Font.Size := 9;
    Visor.Canvas.Font.Style := [];
    Y := Y + 6;
    Visor.TextOut(50, Y, 'CIF: ' + Factura.NIFEmisor);
    Y := Y + 4;
    Visor.TextOut(50, Y, Factura.DireccionEmisor);
    
    // Número de factura (derecha)
    Visor.Canvas.Font.Size := 12;
    Visor.Canvas.Font.Style := [fsBold];
    Visor.TextOutRight(200, 10, 'FACTURA');
    Visor.Canvas.Font.Size := 10;
    Visor.TextOutRight(200, 16, Factura.Serie + '-' + Factura.Numero);
    Visor.TextOutRight(200, 22, FormatDateTime('dd/mm/yyyy', Factura.Fecha));
    
    // === DATOS CLIENTE ===
    Y := 45;
    Visor.Canvas.Pen.Width := 1;
    Visor.Rectangle(10, Y, 200, Y + 25); // Marco cliente
    
    Visor.Canvas.Font.Style := [fsBold];
    Visor.TextOut(12, Y + 2, 'CLIENTE:');
    Visor.Canvas.Font.Style := [];
    Visor.TextOut(12, Y + 7, Factura.NombreCliente);
    Visor.TextOut(12, Y + 12, 'NIF: ' + Factura.NIFCliente);
    Visor.TextOut(12, Y + 17, Factura.DireccionCliente);
    
    // === LÍNEAS DE DETALLE ===
    Y := 80;
    
    // Cabecera de tabla
    Visor.Canvas.Brush.Color := clLtGray;
    Visor.FillRect(10, Y, 200, Y + 6);
    Visor.Canvas.Brush.Style := bsClear;
    
    Visor.Canvas.Font.Style := [fsBold];
    Visor.TextOut(12, Y + 1, 'CONCEPTO');
    Visor.TextOutRight(120, Y + 1, 'CANT.');
    Visor.TextOutRight(150, Y + 1, 'PRECIO');
    Visor.TextOutRight(198, Y + 1, 'IMPORTE');
    
    Y := Y + 8;
    Visor.Canvas.Font.Style := [];
    
    // Líneas de factura
    for LineaActual := 0 to Factura.Lineas.Count - 1 do
    begin
      with Factura.Lineas[LineaActual] do
      begin
        Visor.TextOut(12, Y, Copy(Concepto, 1, 50)); // Truncar si muy largo
        Visor.TextOutRight(120, Y, FormatFloat('#,##0.00', Cantidad));
        Visor.TextOutRight(150, Y, FormatFloat('#,##0.00', Precio));
        Visor.TextOutRight(198, Y, FormatFloat('#,##0.00', Cantidad * Precio));
      end;
      Y := Y + 5;
      
      // Salto de página si necesario
      if Y > 250 then
      begin
        Visor.NewPage;
        Y := 20;
      end;
    end;
    
    // === TOTALES ===
    Y := Y + 10;
    Visor.Line(120, Y, 200, Y); // Línea separadora
    Y := Y + 3;
    
    Visor.TextOut(120, Y, 'Base Imponible:');
    Visor.TextOutRight(198, Y, FormatFloat('#,##0.00 €', Factura.BaseImponible));
    Y := Y + 5;
    
    Visor.TextOut(120, Y, Format('IVA (%d%%):', [Factura.PorcentajeIVA]));
    Visor.TextOutRight(198, Y, FormatFloat('#,##0.00 €', Factura.CuotaIVA));
    Y := Y + 5;
    
    Visor.Canvas.Font.Style := [fsBold];
    Visor.Canvas.Font.Size := 11;
    Visor.TextOut(120, Y, 'TOTAL:');
    Visor.TextOutRight(198, Y, FormatFloat('#,##0.00 €', Factura.Total));
    
    // === PIE VERIFACTU (si aplica) ===
    Y := 270;
    Visor.Canvas.Font.Size := 7;
    Visor.Canvas.Font.Style := [];
    Visor.TextOut(10, Y, 'Factura generada por sistema VERIFACTU');
    Visor.TextOut(10, Y + 3, 'Hash: ' + Copy(Factura.HashVerifactu, 1, 16) + '...');
    
    // QR Code (si disponible)
    if Factura.QRCodePath <> '' then
      Visor.Draw(170, Y - 20, Factura.QRCodePath, 25, 25);
      
  finally
    Visor.EndDoc;
  end;
end;
```

## Template: Ticket Simplificado

```pascal
procedure ImprimirTicket(Visor: TGmPreview; const Ticket: TTicket);
var
  Y: Extended;
  AnchoTicket: Extended;
begin
  AnchoTicket := 80; // 80mm típico para tickets
  
  Visor.BeginDoc;
  try
    Visor.Canvas.Font.Name := 'Courier New'; // Monoespaciada
    Visor.Canvas.Font.Size := 8;
    
    Y := 5;
    
    // Cabecera centrada
    Visor.TextOutCenter(AnchoTicket / 2, Y, Ticket.NombreEmpresa);
    Y := Y + 4;
    Visor.TextOutCenter(AnchoTicket / 2, Y, 'CIF: ' + Ticket.CIF);
    Y := Y + 4;
    Visor.TextOutCenter(AnchoTicket / 2, Y, Ticket.Direccion);
    Y := Y + 6;
    
    // Línea separadora
    Visor.TextOut(2, Y, StringOfChar('-', 40));
    Y := Y + 4;
    
    // Líneas
    for var I := 0 to Ticket.Lineas.Count - 1 do
    begin
      with Ticket.Lineas[I] do
      begin
        Visor.TextOut(2, Y, Copy(Descripcion, 1, 20));
        Visor.TextOutRight(AnchoTicket - 2, Y, FormatFloat('#,##0.00', Importe));
      end;
      Y := Y + 4;
    end;
    
    // Total
    Y := Y + 2;
    Visor.TextOut(2, Y, StringOfChar('=', 40));
    Y := Y + 4;
    Visor.Canvas.Font.Style := [fsBold];
    Visor.TextOut(2, Y, 'TOTAL:');
    Visor.TextOutRight(AnchoTicket - 2, Y, FormatFloat('#,##0.00 €', Ticket.Total));
    
  finally
    Visor.EndDoc;
  end;
end;
```

## Métodos Útiles de GmPreview

| Método | Uso |
| ------ | --- |
| `TextOut(X, Y, Texto)` | Texto alineado a la izquierda |
| `TextOutRight(X, Y, Texto)` | Texto alineado a la derecha |
| `TextOutCenter(X, Y, Texto)` | Texto centrado |
| `Line(X1, Y1, X2, Y2)` | Línea |
| `Rectangle(X1, Y1, X2, Y2)` | Rectángulo |
| `FillRect(X1, Y1, X2, Y2)` | Rectángulo relleno |
| `Draw(X, Y, File, W, H)` | Imagen |
| `NewPage` | Nueva página |

---

**Estado**: stable  
**Última revisión**: 2026-01-08
