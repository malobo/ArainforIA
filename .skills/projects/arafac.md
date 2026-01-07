---
id: skill-project-arafac
name: Proyecto ARAFAC / Aracostes
version: 2.0
category: project
priority: high
last_updated: 2026-01-06
triggers:
  - "aracostes"
  - "arafac"
  - "facturacion verifactu"
  - "nueva arquitectura"
---

# Proyecto ARAFAC

<context>
Aplicación de Facturación moderna (piloto), diseñada desde cero para VERIFACTU. Alias: "Aracostes".
Ruta: `C:\Arainfor\ARAFAC\`
Stack: Delphi (FireDAC + SQLite). `Costes.s3db`.
</context>

<instruction>
1. **Separación UI/Lógica**: Mantener lógica fuera de `Arainfor.pas` siempre que sea posible.
2. **Tablas Nuevas**: Usar siempre las tablas `Facturas` y `FacturasLineas` para nuevos desarrollos, no las legacy.
3. **Componentes**: Utilizar `GmGridPrint` para informes y `TBarcode` para QR.
</instruction>

<examples>
User: "¿Dónde está la base de datos de Aracostes?"
Assistant: "La base de datos SQLite está en `D:\ARAIA\ARAFAC\DATOS\Costes.s3db`. Se accede vía `Bases.pas`."
</examples>
