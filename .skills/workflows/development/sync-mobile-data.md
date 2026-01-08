---
id: skill-sync-mobile-data
name: Workflow Sincronización Móvil
version: 1.0.0
category: workflows/development
priority: high
last_updated: 2026-01-08
triggers:
  - "sincronizar movil"
  - "datos offline"
  - "sync app"
  - "subir datos nube"
---

# 📲 Workflow: Sincronización Móvil/Nube

<context>
Este workflow describe el algoritmo robusto para mantener consistencia de datos entre la base de datos local (Delphi/FireDAC) y la API Cloud (MySQL). Maneja conflictos básicos y asegura atomicidad.
</context>

<instruction>
El proceso de sincronización sigue el patrón "Smart Sync":

## 1. Fase de Subida (Upstream)

1. **Identificar Cambios**:
    * Hacer consulta a tablas locales con `LastUpdate > LastSyncTime`.
    * Generar JSON DTOs (`skill-generate-json-dto`).
2. **Enviar Lote**:
    * POST `/api/sync/up`.
    * Incluir Token JWT.
3. **Confirmación**:
    * Si API responde 200 OK -> Actualizar `LastSyncTime` local.

## 2. Fase de Bajada (Downstream)

1. **Solicitar Cambios**:
    * GET `/api/sync/down?since=<LastSyncTime>`.
2. **Procesar Datos**:
    * Para cada registro recibido:
        * Buscar por ID (UUID o Clave Natural).
        * Si existe y `Local.LastUpdate < Cloud.LastUpdate` -> UPDATE.
        * Si no existe -> INSERT.

## 3. Manejo de Conflictos

* Estrategia: **"La Nube Gana"** (por defecto) o **"Última Escritura Gana"** (Time-based).
* En caso de error crítico, registrar en tabla local `SINCRO_LOG`.
</instruction>

<examples>
User: "¿Cómo implemento la subida de Clientes modificados?"
Agent: "Primero, selecciona los clientes donde `FechaModificacion > UltimoSincro`. Luego, recórrelos creando objetos `TClienteDTO`. Serializa la lista a JSON y envíala con un `THTTPClient.Post` a `/api/sync/up`. Si recibes un 200 OK, guarda la fecha/hora actual como nuevo punto de control."
</examples>
