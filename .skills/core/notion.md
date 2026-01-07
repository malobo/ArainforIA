---
id: skill-notion-mcp
name: Notion MCP Connector
version: 2.0
category: core
priority: critical
last_updated: 2026-01-06
triggers:
  - "objetivo del proyecto"
  - "memoria del proyecto"
  - "roadmap"
  - "registra esto"
  - "guarda esta solucion"
---

# Notion MCP Connector

<context>
Conector al Workspace "Antigravity" en Notion. Actúa como la "Memoria a Largo Plazo" del proyecto y repositorio de documentación funcional.
Contiene: Especificaciones, Hoja de Ruta, Base de Datos de Conocimiento Técnico.
</context>

<instruction>
1. **Lectura Preventiva**: Antes de preguntar al usuario por objetivos o requerimientos, busca en Notion.
2. **Escritura Estructurada**: Al registrar soluciones (`create-a-page`), usa títulos claros y tags si es posible.
3. **Integridad de IDs**: Nunca inventes IDs. Usa `search` para encontrar los IDs reales de páginas o bases de datos (ej. "Base de Conocimiento").
</instruction>

<examples>
User: "¿Qué tenemos que hacer en el módulo de Facturación?"
Assistant: (Call tool `notion-mcp-server_API-post-search` with query="Módulo Facturación objetivos requisitos")

User: "Guarda esta solución para el error de FireDAC"
Assistant: (Call tool `notion-mcp-server_API-create-a-page` parent={"page_id": "ID_ENCONTRADO"} properties={...})
</examples>
