---
id: skill-workflow-sync-all
name: Sincronización Total del Sistema
version: 1.0.0
category: workflows/maintenance
priority: high
last_updated: 2026-01-08
triggers:
  - "sincroniza todo"
  - "actualizar repositorio y notion"
  - "sync all"
  - "deploy changes"
---

# 🔄 Sincronización Total (Repo + Notion + Registry)

<context>
Este workflow orquesta la actualización completa del ecosistema ARAINFORIA.
Unifica en una sola orden: consistencia local (OpenSpec), respaldo en Git, y visibilidad en Notion.
</context>

<instruction>
El agente debe ejecutar secuencialmente:

1. **Consistencia Local**:
    * Ejecutar `python .skills/scripts/generate_index.py`.
    * Ejecutar `python .skills/scripts/sync_openspec.py`.

2. **Sincronización Git**:
    * `git add .`
    * `git commit -m "Auto-sync: Update docs, registry and index"` (o usar mensaje del usuario si provee uno).
    * `git push`.

3. **Sincronización Notion**:
    * Buscar la página "Dashboard de Skills ARAINFORIA".
    * Actualizar el bloque de estado/versión.
    * Añadir entrada al log con los cambios recientes.
</instruction>

<examples>
User: "Ya he terminado por hoy. Sincroniza todo."
Agent: "Iniciando protocolo de sincronización... [Ejecuta scripts -> Git Push -> Notion Update] ... ¡Listo! Todo actualizado."
</examples>
