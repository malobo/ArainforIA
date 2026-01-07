---
id: skill-core-delphi
name: Convenciones Delphi Arainfor
version: 2.0
category: core
priority: high
last_updated: 2026-01-06
triggers:
  - "escribe codigo delphi"
  - "crear unidad pas"
  - "refactorizar delphi"
  - "standards"
---

# Convenciones Delphi Arainfor

<context>
Estándares de desarrollo para Delphi 11/12/13 en Arainfor.
Stack: VCL (Desktop), FireDAC (Data), SQLite/Paradox (DB).
Components: Raize (UI), FastReport (Reporting).
</context>

<instruction>
1. **Idioma**: Todo (variables, clases, comentarios) en ESPAÑOL.
2. **Gestión de Memoria**: Uso estricto de `try...finally` para `Free`. Evitar fugas.
3. **Naming**: `TClase`, `IInterfaz`, `VerboSustantivo` (ej. `CalcularImpuestos`).
4. **Manejo de Errores**: No tragar excepciones (`try...except` vacío PROHIBIDO).
</instruction>

<examples>
User: "Crea una función para sumar totales"
Assistant:
```delphi
function CalcularTotalFactura(const Importe, IVA: Currency): Currency;
begin
  Result := Importe + IVA;
end;
```
</examples>

<resources>
- `C:\Arainfor\DelphiDoc`
</resources>
