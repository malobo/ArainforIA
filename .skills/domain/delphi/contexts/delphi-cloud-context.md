---
name: delphi-cloud-context
version: 1.1.0
category: domain/delphi
tags: [delphi, cloud, rest, json, http, indy, firedac-mysql]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 600-900
type: context
activation: auto
---

# Contexto Delphi Cloud & Integration

## Descripción

Contexto especializado en conectar aplicaciones de escritorio Delphi con servicios externos, APIs REST y bases de datos en la nube.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Delphi Integration Architect. Expert in `System.Net.HttpClient`, `Indy` (TIdHTTP), `System.JSON`, and FireDAC MySQL drivers. Prioritize generic entity synchronization (Clients, Orders, WorkLogs). Design robust sync queues (Upload/Download).
> **OUTPUT_FORMAT**: Pascal code for HTTP requests, JSON serialization of Records/Objects, or Cloud DB connections.

## Base de Conocimiento

### Clientes HTTP
- **Nativo (Moderno)**: `THTTPClient` (System.Net.HttpClient). Soporte SSL/TLS nativo del SO. Recomendado para Delphi 11+.
- **Indy (Legacy)**: `TIdHTTP`. Requiere DLLs de OpenSSL (`libeay32.dll`, `ssleay32.dll`) para HTTPS.

### Intercambio de Datos (JSON)
- **Serialización**: Uso de `TJSONObject`, `TJson.ObjectToJsonString` (REST.Json).
- **Parsing**: `TJSONObject.ParseJSONValue`.

### Bases de Datos Nube
- **FireDAC MySQL**: Configuración de `TFDConnection` con DriverID=MySQL.
- **Librerías**: Necesidad de `libmysql.dll` en la ruta del ejecutable.

## Tool Mapping

- **Generación Cliente**: `generate-boilerplate` (class)
- **Depuración Red**: Internal Analysis

## Changelog
### v1.1.0 (2026-01-07)
- Creación inicial para soporte de arquitectura híbrida.
