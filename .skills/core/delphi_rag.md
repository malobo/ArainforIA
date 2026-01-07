---
id: skill-delphi-rag
name: Delphi RAG Connector
version: 2.0
category: core
priority: critical
last_updated: 2026-01-06
triggers:
  - "como hago en delphi"
  - "componente desconocido"
  - "error de compilacion"
  - "documentacion delphi"
  - "firedac configuration"
---

# Delphi RAG Connector

<context>
Sistema de Recuperación Aumentada (RAG) para consultar documentación técnica de Delphi (versiones 11, 12, 13) y bases de conocimiento internas de Arainfor.
El índice FAISS local contiene detalles que NO están en tu entrenamiento base (versiones específicas, guías de estilo internas).
</context>

<instruction>
1. **Consulta OBLIGATORIA**: Antes de asumir cómo funciona un componente o escribir código complejo, CONSULTA EL RAG.
2. **Prioridad Absoluta**: La información devuelta por `delphi-rag` prevalece sobre tu conocimiento general.
3. **Manejo de Fallos**: Si el RAG no devuelve resultados útiles, infórmalo explícitamente: "La documentación local no contiene detalles sobre X, procederé con prácticas estándar".
</instruction>

<examples>
User: "¿Cómo configuro una transacción en FireDAC?"
Assistant: (Call tool `delphi-rag_query_delphi_rag` with query="FireDAC transaction setup configuration TFDTransaction")

User: "Error E2003 Undeclared identifier"
Assistant: (Call tool `delphi-rag_query_delphi_rag` with query="Error E2003 undeclared identifier solution")
</examples>
