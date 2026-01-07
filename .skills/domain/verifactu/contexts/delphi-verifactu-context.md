---
name: delphi-verifactu-context
version: 1.1.0
category: domain/verifactu
tags: [verifactu, aeat, compliance, hash, qr, delphi]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 800-1200
type: context
activation: auto
---

# Contexto Verifactu (Normativa Española)

## Descripción

Contexto especializado en el cumplimiento del Real Decreto 1007/2023 (Reglamento Verifactu). Incluye lógica de hashing, encadenamiento, generación de QR y comunicaciones con AEAT.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Verifactu Compliance Expert. Enforce SHA-256 chaining. Validate NIF/CIF. Support AEAT XML schemas and QR generation per tech specs.
> **OUTPUT_FORMAT**: Secure, compliant code following RD 1007/2023.

## Base de Conocimiento

### Integridad y Encadenamiento
- **SHA-256**: Uso de `System.Hash.THashSHA2`.
- **Encadenamiento**: Concatenación de campos obligatorios + `HashAnterior` para generar `HashActual`.
- **Inalterabilidad**: Registros deben ser inalterables (Log de eventos Verifactu).

### Formatos Obligatorios
- **Código QR**: URL AEAT + parámetros (NIF, Serie, Fecha, Importe). Uso de `DelphiZXingQRCode`.
- **XML AEAT**: Esquemas Sii/Verifactu. Uso de `Xml.XMLDoc`.
- **Validación NIF**: Algoritmo 23 (TRWAGMYFPDXBNJZSQVHLCKE).

### Comunicaciones
- **Indy / OpenSSL**: Conexión TLS 1.2+ para envío de lotes a la sede electrónica.

## Tool Mapping

- **Validación**: `validate-verifactu-implementation`
- **Despliegue**: `deploy-verifactu-update`

## Changelog

### v1.1.0 (2026-01-07)
- Extracción desde master context.
- Foco exclusivo en cumplimiento legal y técnica Verifactu.
