---
name: update-specs
version: 1.0.0
category: core/analysis
tags: [specs, arquitectura, documentacion, sincronizar]
author: ARAINFORIA
created: 2026-01-08
complexity: 6
triggers:
  - "actualizar specs"
  - "sincronizar arquitectura"
  - "documentar sistema"
  - "mapear proyecto"
---

# Update Specs (Arquitecto)

## Descripción

Analiza el código fuente y actualiza automáticamente los archivos en `specs/`.

## AI Context

> **SYSTEM_INSTRUCTION**: Analyze source code and update spec files to reflect current state.
> **OUTPUT_FORMAT**: Updated spec files + summary of changes.
> **TOKEN_STRATEGY**: Focus on structural changes, not implementation details.

## Procedimiento

### 1. Analizar Proyecto

```pascal
// Leer archivo .dpr para identificar unidades
// Leer DataModules para identificar tablas
// Leer uses para identificar dependencias
```

### 2. Actualizar system-context.md

- Diagrama de arquitectura
- Flujos principales
- Convenciones detectadas

### 3. Actualizar data-schema.yaml

- Nuevas tablas detectadas
- Campos añadidos/modificados
- Relaciones (FK)

### 4. Actualizar dependencies.md

- Nuevos componentes en uses
- Versiones actualizadas
- Dependencias obsoletas

## Spec Requirements

- Leer: `specs/system-context.md` (estado actual)
- Leer: `specs/data-schema.yaml` (esquema actual)
- Leer: `specs/dependencies.md` (dependencias actuales)

## Spec Updates

- Actualizar: Todos los specs analizados
- Crear: Nuevo spec si se detecta área no documentada

## Ejemplo de Uso

```
Usuario: "Actualiza los specs del proyecto Aracostes"

IA:
1. Escanea FACARAVF.dpr → Identifica 45 unidades
2. Lee Bases.pas → Detecta 12 tablas
3. Compara con data-schema.yaml → 2 tablas nuevas
4. Actualiza specs con cambios detectados
5. Reporta: "Añadidas tablas CONTRATOS, TARIFAS al esquema"
```

## Checklist de Análisis

- [ ] Archivo .dpr leído
- [ ] DataModules identificados
- [ ] Tablas mapeadas
- [ ] Dependencias listadas
- [ ] Specs actualizados
- [ ] Cambios documentados

---

**Estado**: stable
