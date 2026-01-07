---
name: sync-project-docs
version: 1.0.0
category: core/integration
tags: [notion, documentation, sync, delphi, auto-doc]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 4
estimated_tokens: 600-900
type: automation
---

# Sincronizar Documentación de Proyecto

## Descripción

Sincroniza automáticamente la documentación de código con Notion,
creando y actualizando páginas con información de unidades, clases y métodos.

## Invocación

```
@skill:core/integration/sync-project-docs
proyecto: "FACARAVF"
ruta: "D:/ARAINFORIA/FACARAVF/Fuente"
archivos: ["uVerifactu.pas", "uFactura.pas"]
modo: "update"  // create, update, full
```

---

## Inputs

| Nombre | Tipo | Requerido | Descripción |
|--------|------|-----------|-------------|
| `proyecto` | string | ✅ | Nombre del proyecto |
| `ruta` | string | ✅ | Ruta al código fuente |
| `archivos` | array | Opcional | Archivos específicos (o todos) |
| `modo` | string | Opcional | create, update, full |
| `incluir_privados` | boolean | Opcional | Documentar métodos privados |
| `generar_diagramas` | boolean | Opcional | Crear diagramas de dependencias |

## Outputs

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `success` | boolean | Sincronización exitosa |
| `paginas_creadas` | number | Páginas nuevas |
| `paginas_actualizadas` | number | Páginas modificadas |
| `url_indice` | string | URL del índice en Notion |

---

## Procedimiento

### Paso 1: Analizar Código

```
1. Escanear archivos .pas en la ruta
2. Para cada archivo:
   - Extraer nombre de unidad
   - Identificar clases y records
   - Listar métodos públicos/privados
   - Detectar dependencias (uses)
   - Calcular métricas (líneas, complejidad)
```

### Paso 2: Sincronizar con Notion

```
1. Buscar/crear página de proyecto en Notion
2. Para cada unidad:
   - Buscar página existente
   - Si existe y modo=update: actualizar
   - Si no existe: crear nueva página
3. Actualizar índice del proyecto
```

### Paso 3: Generar Contenido

```
Para cada página de unidad:
- Cabecera con metadata
- Lista de clases
- Tabla de métodos con firmas
- Dependencias
- Métricas
- Notas y TODOs encontrados
```

---

## Estructura en Notion

```
📁 Documentación de Código
├── 📁 FACARAVF
│   ├── 📄 Índice de Unidades
│   ├── 📄 uVerifactu
│   │   ├── Clase: TVerifactuManager
│   │   ├── Métodos
│   │   └── Dependencias
│   ├── 📄 uFactura
│   └── 📄 uCliente
├── 📁 Aracostes
│   └── ...
└── 📁 Gesfac
    └── ...
```

---

## Plantilla de Unidad

```markdown
# 📦 uVerifactu.pas

**Proyecto**: FACARAVF
**Última sincronización**: 2026-01-07
**Líneas de código**: 1,245
**Complejidad**: Media

## 🏛️ Clases

### TVerifactuManager
Gestiona la generación de registros Verifactu.

| Método | Visibilidad | Descripción |
|--------|-------------|-------------|
| GenerarHash | public | Genera hash SHA-256 |
| ValidarCadena | public | Valida encadenamiento |
| EnviarAEAT | public | Envía a la AEAT |

### TRegistroVerifactu
Record con datos de factura Verifactu.

## 📦 Dependencias
- System.Hash
- System.SysUtils
- uFactura
- uCliente

## 📊 Métricas
- Clases: 2
- Métodos públicos: 15
- Métodos privados: 8
- TODOs encontrados: 3
```

---

## Notas

- Usa analyze-delphi-unit internamente
- Detecta cambios para sincronización incremental
- Puede programarse para ejecución automática

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
