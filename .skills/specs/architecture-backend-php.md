---
id: spec-architecture-backend-php
title: Arquitectura Backend Híbrido (PHP/MySQL)
version: 1.0.0
last_updated: 2026-01-08
---

# ☁️ Arquitectura Backend Híbrido

Este documento define la estructura técnica de la API REST en PHP que servirá como "espejo" en la nube para el ERP Delphi.

## 1. Principios de Diseño

* **Simpleza (KISS)**: Sin frameworks pesados. PHP Nativo + Router ligero.
* **Portabilidad**: Debe funcionar en hostings compartidos (cPanel/Plesk).
* **Seguridad**: Autenticación JWT (JSON Web Tokens) requerida para todos los endpoints de escritura.

## 2. Estructura de Directorios

```text
/public
    index.php           # Punto de entrada único (Front Controller)
    .htaccess           # Redirección de todo tráfico a index.php
/src
    /Config             # Database.php, AppConfig.php
    /Controllers        # AuthController.php, ClientesController.php, SincroController.php
    /Models             # User.php, Cliente.php, Pedido.php
    /Helpers            # JwtHelper.php, ResponseHelper.php
    Router.php          # Enrutador básico
/vendor                 # Librerías (composer: firewall, jwt)
```

## 3. Esquema de Base de Datos (MySQL)

Las tablas espejo tendrán la misma PK que Delphi pero añadirán campos de control de nube.

### Tabla `cloud_users` (Acceso API)

| Campo | Tipo | Notas |
| :--- | :--- | :--- |
| `id` | INT AI | |
| `api_key` | VARCHAR(64) | Identificador del terminal Delphi |
| `api_secret` | VARCHAR(255) | Hash (bcrypt) |
| `role` | ENUM | 'admin', 'terminal', 'movil' |

### Tablas de Negocio (Ej: `clientes`)

| Campo | Tipo | Notas |
| :--- | :--- | :--- |
| `id` | INT | ID original de Delphi (NO AUTO_INC) |
| `codigo` | VARCHAR | |
| `nombre` | VARCHAR | |
| `...` | ... | Resto de campos |
| `last_sync` | TIMESTAMP | Auditoría de sincronización |
| `source` | ENUM | 'desktop', 'mobile' |

## 4. Endpoints API (v1)

### Autenticación

* `POST /api/auth/login`
  * Input: `{ api_key, api_secret }`
  * Output: `{ token: "jwt_string...", expires_in: 3600 }`

### Sincronización

* `POST /api/sync/up` (Subida Delphi -> Nube)
  * Input: `{ entity: "clientes", data: [ ...JSON objects... ] }`
  * Acción: `UPSERT` (Insertar o Actualizar basado en ID).
* `GET /api/sync/down` (Bajada Nube -> Delphi)
  * Input: `?entity=pedidos&since=2024-01-01T12:00:00`
  * Output: `[ ...registros modificados en la nube... ]`

## 5. Seguridad JWT

El token debe enviarse en el header:
`Authorization: Bearer <token>`
Payload del token:

```json
{
  "sub": "terminal-01",
  "role": "terminal",
  "iat": 1704720000,
  "exp": 1704723600
}
```
