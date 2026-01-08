# 📋 Propuesta de Mejoras al Sistema de Skills

**Fecha**: 2026-01-08  
**Versión actual**: 1.6.0  
**Objetivo**: Optimizar el desarrollo híbrido con Delphi

---

## 🚀 Skills de Alta Prioridad (Impacto Inmediato)

### 1. `convert-sql-paradox` (domain/database)

Convierte consultas SQL estándar a sintaxis compatible con Paradox/BDE y viceversa.

| Campo | Valor |
|-------|-------|
| **Triggers** | `convertir sql`, `paradox query`, `sql a paradox` |
| **Uso** | Migración de consultas, optimización de queries |
| **Complejidad** | 4/10 |

### 2. `generate-api-client` (domain/delphi)

Genera código cliente REST para consumir APIs (AEAT, bancos, servicios externos).

| Campo | Valor |
|-------|-------|
| **Triggers** | `consumir api`, `cliente rest`, `llamada http` |
| **Uso** | Integraciones con AEAT, servicios de facturación electrónica |
| **Complejidad** | 6/10 |

### 3. `migrate-bde-firedac` (workflows/migration)

Workflow paso a paso para migrar de BDE/Paradox a FireDAC/SQL.

| Campo | Valor |
|-------|-------|
| **Triggers** | `migrar bde`, `eliminar paradox`, `modernizar datos` |
| **Uso** | Modernización gradual del acceso a datos |
| **Complejidad** | 8/10 |

### 4. `optimize-vcl-form` (domain/delphi)

Analiza formularios VCL para detectar problemas de rendimiento y UX.

| Campo | Valor |
|-------|-------|
| **Triggers** | `formulario lento`, `optimizar form`, `mejorar ui` |
| **Uso** | Mejora de formularios legacy |
| **Complejidad** | 5/10 |

---

## 🔧 Skills de Prioridad Media (Productividad)

### 5. `generate-report-template` (domain/database)

Genera plantillas de informes para GmPrintSuite.

| Campo | Valor |
|-------|-------|
| **Triggers** | `crear informe`, `plantilla impresion`, `reporte nuevo` |
| **Uso** | Creación rápida de facturas, tickets, listados |
| **Complejidad** | 5/10 |

### 6. `validate-dfm-integrity` (core/analysis)

Valida archivos .dfm para detectar referencias rotas, componentes faltantes.

| Campo | Valor |
|-------|-------|
| **Triggers** | `dfm roto`, `componente no encontrado`, `error form` |
| **Uso** | Debugging de formularios corruptos |
| **Complejidad** | 4/10 |

### 7. `generate-data-export` (domain/database)

Genera código para exportar datos a Excel, CSV, JSON, XML.

| Campo | Valor |
|-------|-------|
| **Triggers** | `exportar excel`, `generar csv`, `sacar datos` |
| **Uso** | Funcionalidades de exportación estándar |
| **Complejidad** | 4/10 |

### 8. `implement-audit-trail` (domain/database)

Implementa sistema de auditoría de cambios en tablas.

| Campo | Valor |
|-------|-------|
| **Triggers** | `auditar cambios`, `historial registros`, `log modificaciones` |
| **Uso** | Trazabilidad para cumplimiento normativo |
| **Complejidad** | 6/10 |

---

## 🎯 Skills Avanzadas (Arquitectura)

### 9. `refactor-to-mvp` (core/refactoring)

Guía para separar lógica de negocio de UI (Model-View-Presenter).

| Campo | Valor |
|-------|-------|
| **Triggers** | `separar logica`, `mvp pattern`, `desacoplar form` |
| **Uso** | Modernización de arquitectura |
| **Complejidad** | 7/10 |

### 10. `create-rest-endpoint` (domain/delphi)

Crea endpoints REST con mORMot2 para exponer funcionalidad.

| Campo | Valor |
|-------|-------|
| **Triggers** | `crear api`, `endpoint rest`, `servicio web` |
| **Uso** | Exposición de servicios para apps móviles/web |
| **Complejidad** | 7/10 |

### 11. `debug-connection-issue` (domain/database)

Diagnóstico de problemas de conexión a BD (BDE, Paradox, SQL Server).

| Campo | Valor |
|-------|-------|
| **Triggers** | `no conecta`, `error conexion`, `bd bloqueada` |
| **Uso** | Troubleshooting de conectividad |
| **Complejidad** | 5/10 |

---

## 📱 Skills para Desarrollo Híbrido

### 12. `sync-mobile-data` (workflows/development)

Workflow para sincronización de datos entre app móvil y desktop.

| Campo | Valor |
|-------|-------|
| **Triggers** | `sincronizar movil`, `datos offline`, `sync app` |
| **Uso** | Integración con apps React Native/Flutter |
| **Complejidad** | 8/10 |

### 13. `generate-json-dto` (core/generation)

Genera DTOs (Data Transfer Objects) para comunicación JSON.

| Campo | Valor |
|-------|-------|
| **Triggers** | `crear dto`, `json structure`, `serializar objeto` |
| **Uso** | Comunicación entre Delphi y front-ends modernos |
| **Complejidad** | 4/10 |

---

## 📊 Resumen

| Prioridad | Cantidad | Complejidad Promedio |
|-----------|----------|---------------------|
| Alta | 4 | 5.75 |
| Media | 4 | 4.75 |
| Avanzada | 3 | 6.33 |
| Híbrido | 2 | 6.0 |
| **Total** | **13** | **5.5** |

---

## 🔗 Próximos Pasos

1. Revisar priorización con el usuario
2. Seleccionar 3-4 skills para implementar primero
3. Crear plan de implementación detallado
4. Implementar skills seleccionadas
5. Actualizar INDEX.md y registry
