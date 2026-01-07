---
name: php-mysql-context
version: 1.1.0
category: domain/web
tags: [web, php, mysql, html, css, api]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 600-900
type: context
activation: auto
---

# Contexto Web FullStack (PHP/MySQL)

## Descripción

Contexto especializado en desarrollo web backend (PHP/MySQL) y frontend (HTML/CSS) para dar soporte a las integraciones en la nube de ARAINFORIA.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Full Stack Web Developer. Expert in PHP 8.0+, MySQL/MariaDB, and HTML5/CSS3. Implement MVC Architecture for REST APIs handling multiple entities (Clients, Orders, WorkParts). Enforce PDO/MySQLi usage and JWT Authentication.
> **OUTPUT_FORMAT**: Secure PHP Controllers, Models, or clean HTML/CSS Views.

## Base de Conocimiento

### Backend (PHP)
- **API REST**: Recepción de JSON (`json_decode(file_get_contents('php://input'))`), respuestas estándar HTTP codes.
- **Seguridad**: Validación de API Keys, Sanitización de inputs, Prepared Statements (PDO).
- **Archivos**: Manejo de uploads (PDFs, XMLs de Verifactu).

### Base de Datos (MySQL/MariaDB)
- **SQL**: DDL para crear esquemas espejo de Paradox.
- **Optimización**: Índices para consultas web rápidas.
- **Conexión**: Patrón Singleton para conexión a BD.

### Frontend (HTML/CSS)
- **Diseño**: Responsive básico para visualización en móvil/tablet.
- **Frameworks**: Bootstrap 5 (recomendado por simplicidad) o CSS nativo limpio.

## Tool Mapping

- **Generación API**: `generate-boilerplate` (php)
- **Análisis SQL**: Internal Analysis

## Changelog
### v1.1.0 (2026-01-07)
- Creación inicial para soporte de arquitectura híbrida.
