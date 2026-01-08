---
id: sync-openspec-registry
name: Sincronizar Registro OpenSpec
version: 2.0.0
category: core/integration
priority: high
last_updated: 2026-01-08
triggers:
  - "sincronizar registro"
  - "actualizar tools.yaml"
  - "generar openspec"
  - "indexar herramientas"
---

# [Sincronizar Registro OpenSpec]

<context>
Mantiene el catálogo centralizado de capacidades (`.skills/registry/tools.yaml`) sincronizado con los archivos Markdown distribuidos.
Es fundamental para que el agente tenga una "Conciencia de sus Capacidades" formal y estructurada bajo el estándar OpenSpec.
</context>

<instruction>
Ejecuta el script de Python dedicado para escanear y regenerar el registro.

1. Navega a `.skills`.
2. Ejecuta `python scripts/sync_openspec.py`.
3. Valida que se haya creado/actualizado `registry/tools.yaml`.
4. Informa del número de herramientas indexadas.
</instruction>

<examples>
User: "Actualiza el registro de skills"
Assistant: "Ejecutando script de sincronización..."
(Ejecuta: python scripts/sync_openspec.py)
Output: "Total tools indexed: 42"
Assistant: "Registro actualizado correctamente con 42 herramientas."
</examples>

<resources>
- [Script Python](../scripts/sync_openspec.py)
- [Registro Generado](../registry/tools.yaml)
</resources>
