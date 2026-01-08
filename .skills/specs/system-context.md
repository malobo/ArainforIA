---
spec: system-context
version: 1.0.0
last_updated: 2026-01-08
updated_by: AI
status: current
---

# Contexto del Sistema - Arainfor

## Descripción General

Sistema de gestión empresarial desarrollado en Delphi (Object Pascal) que incluye:

- Facturación electrónica (Verifactu)
- Gestión de clientes y proveedores
- Control de inventario
- Terminal Punto de Venta (TPV)

## Proyectos Principales

| Proyecto | Descripción | Estado |
|----------|-------------|--------|
| **ARAFAC/Aracostes** | Facturación con Verifactu | Activo |
| **ERPW** | ERP Central | Activo |
| **TPVARA** | Terminal Punto de Venta | Activo |

## Arquitectura

```
┌─────────────────────────────────────────────────────────┐
│                    CAPA DE PRESENTACIÓN                 │
│  ┌─────────┐  ┌─────────┐  ┌─────────┐  ┌─────────┐   │
│  │ Clientes│  │Facturas │  │ Informes│  │  TPV    │   │
│  └────┬────┘  └────┬────┘  └────┬────┘  └────┬────┘   │
└───────┼────────────┼────────────┼────────────┼─────────┘
        │            │            │            │
┌───────┴────────────┴────────────┴────────────┴─────────┐
│                    CAPA DE NEGOCIO                      │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │ uVerifactu  │  │ uFacturas   │  │ uClientes   │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
└────────────────────────────┬────────────────────────────┘
                             │
┌────────────────────────────┴────────────────────────────┐
│                    CAPA DE DATOS                        │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐     │
│  │ Bases.pas   │  │ BDE/Paradox │  │   FireDAC   │     │
│  │ (DataModule)│  │  (Legacy)   │  │  (Futuro)   │     │
│  └─────────────┘  └─────────────┘  └─────────────┘     │
└─────────────────────────────────────────────────────────┘
```

## Convenciones de Nomenclatura

| Prefijo | Tipo | Ejemplo |
|---------|------|---------|
| `frm` | Formulario | `frmClientes` |
| `dm` | DataModule | `dmDatos`, `Bases` |
| `u` | Unidad de lógica | `uFacturas`, `uVerifactu` |
| `qry` | Query | `qryConsulta` |
| `tbl` | Tabla | `tblClientes` |

## Flujos Críticos

### 1. Creación de Factura (Verifactu)

```
Usuario crea factura → Calcular totales → Generar Hash → 
Encadenar con anterior → Firmar → Generar XML → Enviar AEAT
```

### 2. Venta TPV

```
Escanear productos → Calcular total → Forma de pago → 
Generar ticket → Actualizar stock → Registrar en caja
```

## Integraciones Externas

- **AEAT**: Envío de facturas Verifactu
- **ZKTeco**: Lectores biométricos
- **Impresoras**: Tickets y facturas (GmPrintSuite)

---

**Última revisión**: 2026-01-08
