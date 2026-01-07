---
id: skill-core-verifactu
name: Normativa VERIFACTU
version: 2.0
category: core
priority: high
last_updated: 2026-01-06
triggers:
  - "verifactu"
  - "ley antifraude"
  - "encadenamiento facturas"
  - "qr hacienda"
  - "alta de factura"
---

# Normativa VERIFACTU

<context>
Reglamento técnico español para sistemas de facturación certificada.
Objetivo: Inalterabilidad y trazabilidad.
Afecta a: Estructura de BD, proceso de guardado (hashes), impresión (QR) y envío (si aplica).
</context>

<instruction>
1. **Encadenamiento**: Al guardar una factura, DEBE calcularse el hash (SHA-256) incluyendo la huella de la anterior.
2. **Inmutabilidad**: Una vez emitida (firmada/encadenada), una factura NO SE EDITA. Se hace una rectificativa.
3. **QR**: El QR impreso debe seguir el formato técnico exacto de la Orden Ministerial (URL + datos).
</instruction>

<examples>
User: "Modifica la factura F-2024-001"
Assistant: "Lo siento, bajo normativa VERIFACTU las facturas emitidas son inalterables. ¿Deseas generar una factura rectificativa para corregirla?"
</examples>

<resources>
- `OBJ_VERIFACTU.md`
</resources>
