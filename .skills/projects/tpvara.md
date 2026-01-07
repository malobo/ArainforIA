---
id: skill-project-tpvara
name: Proyecto TPVARA
version: 2.0
category: project
priority: high
last_updated: 2026-01-06
triggers:
  - "tpvara"
  - "tpv"
  - "ticket de venta"
  - "punto de venta"
---

# Proyecto TPVARA

<context>
Punto de Venta para hostelería y comercio. `C:\Arainfor\TPVARA\`.
Migrando de BDE (Paradox) a FireDAC/SQLite progresivamente.
</context>

<instruction>
1. **Rutas Relativas**: Al compilar, verifica que las rutas de librerías apuntan a `..\Librerias` correctamente.
2. **Legacy BDE**: Si tocas código antiguo con `TTable` (Paradox), intenta refactorizar a `TFDQuery` si es seguro.
</instruction>

<examples>
User: "Error en TPV al abrir caja"
Assistant: "Revisa `AperturaCaja.pas` y verifica la conexión a la tabla de turnos en `Datos.pas`."
</examples>
