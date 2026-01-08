---
spec: dependencies
version: 1.0.0
last_updated: 2026-01-08
updated_by: AI
status: current
---

# Dependencias del Sistema

## Componentes Core Delphi

| Componente | Versión | Propósito |
|------------|---------|-----------|
| VCL | Delphi | Framework UI desktop |
| RTL | Delphi | Runtime Library |
| BDE | Legacy | Acceso Paradox (a deprecar) |
| FireDAC | Delphi | Acceso datos (futuro) |

## Frameworks Externos

### mORMot 2

| Unidad | Uso |
|--------|-----|
| `mormot.core.base` | Tipos base, utilidades |
| `mormot.core.json` | Serialización JSON |
| `mormot.core.text` | Procesamiento texto |
| `mormot.crypt.core` | Hashing SHA-256 |
| `mormot.crypt.secure` | Firmas digitales |
| `mormot.net.client` | Cliente HTTP/REST |

### GmPrintSuite

| Componente | Uso |
|------------|-----|
| `TGmPreview` | Vista previa impresión |
| `TGmGridPrint` | Impresión de grids |
| Canvas units | gmMillimeters, gmPixels |

### ZKSDK

| Componente | Uso |
|------------|-----|
| `zkemkeeper_TLB` | Lectores biométricos ZKTeco |
| `TZKEM` | Control de dispositivo |

## Componentes de Terceros

| Componente | Propósito | Estado |
|------------|-----------|--------|
| DelphiZXIngQRCode | Generación QR | Activo |
| FBarcode | Códigos de barras | Pendiente revisar |
| RxLib | Componentes legacy | A migrar |

## Dependencias de Sistema

| Requisito | Versión | Notas |
|-----------|---------|-------|
| Windows | 7+ | 32/64 bit |
| BDE | 5.x | Solo para Paradox legacy |
| .NET Framework | - | No requerido |

## Archivos de Configuración

| Archivo | Propósito |
|---------|-----------|
| `config.ini` | Configuración general |
| `conexion.ini` | Rutas de BD |
| `usuarios.ini` | Credenciales locales |

## Servicios Externos

| Servicio | URL | Propósito |
|----------|-----|-----------|
| AEAT Verifactu | `www1.agenciatributaria.gob.es` | Envío facturas |
| AEAT NIF | `www1.agenciatributaria.gob.es/wlpl/BURT-JDIT` | Validación NIF |

---

**Última revisión**: 2026-01-08
