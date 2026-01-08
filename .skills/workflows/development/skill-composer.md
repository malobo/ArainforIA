---
name: skill-composer
version: 1.0.0
category: workflows/development
tags: [workflow, composicion, pipeline, automatizacion]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 6
estimated_tokens: 500-700
triggers:
  - "combinar skills"
  - "pipeline desarrollo"
  - "workflow completo"
  - "ejecutar secuencia"
---

# Skill Composer

## Descripción

Permite combinar múltiples skills en secuencias (pipelines) para automatizar flujos de trabajo complejos.

## AI Context

> **SYSTEM_INSTRUCTION**: Compose multiple skills into automated workflows. Execute sequentially.
> **OUTPUT_FORMAT**: Workflow definition + execution results.
> **TOKEN_STRATEGY**: Load skills on-demand, summarize outputs.

## Definición de Pipeline

```yaml
name: desarrollo-feature-completo
description: Workflow para desarrollar una feature nueva
version: 1.0.0

steps:
  - skill: analyze-project-structure
    input: ${PROJECT_PATH}
    output: $estructura
    
  - skill: generate-boilerplate
    input:
      type: unit
      name: ${FEATURE_NAME}
    output: $codigo_base
    
  - skill: generate-unit-tests
    input: $codigo_base
    output: $tests
    
  - skill: validate-skill-format
    input: $codigo_base
    condition: ${RUN_VALIDATION}
```

## Pipelines Predefinidos

### 1. Nueva Funcionalidad

```yaml
name: nueva-funcionalidad
steps:
  1. analyze-project-structure  # Entender contexto
  2. generate-boilerplate       # Crear estructura
  3. implement-design-pattern   # Aplicar patrones
  4. generate-unit-tests        # Crear tests
```

**Invocación**:

```text
"Ejecuta el pipeline nueva-funcionalidad para crear un módulo de Clientes"
```

### 2. Integración API

```yaml
name: integracion-api
steps:
  1. generate-json-dto          # Crear DTOs
  2. generate-api-client        # Crear cliente REST
  3. validate-dfm-integrity     # Verificar forms
```

### 3. Migración Verifactu

```yaml
name: migracion-verifactu
steps:
  1. analyze-delphi-unit        # Analizar código actual
  2. validate-verifactu-implementation  # Verificar cumplimiento
  3. generate-verifactu-xml     # Generar templates XML
  4. generate-gmprint-invoice   # Actualizar impresión
```

### 4. Mantenimiento BD

```yaml
name: mantenimiento-bd
steps:
  1. validate-paradox-table     # Verificar integridad
  2. create-database-migration  # Crear migración
  3. convert-sql-paradox        # Optimizar queries
```

## Ejecución de Pipeline

### Sintaxis de Invocación

```text
"Ejecuta pipeline [NOMBRE] con parámetros:
- PARAM1: valor1
- PARAM2: valor2"
```

### Ejemplo Completo

```text
Usuario: "Ejecuta pipeline nueva-funcionalidad para crear módulo Proveedores"

IA: Ejecutando pipeline nueva-funcionalidad...

Paso 1/4: analyze-project-structure
  ✓ Proyecto analizado: 45 unidades, 12 formularios
  ✓ Dependencias mapeadas

Paso 2/4: generate-boilerplate
  ✓ Creado: uProveedores.pas
  ✓ Creado: frmProveedores.pas/dfm

Paso 3/4: implement-design-pattern
  ✓ Aplicado patrón Repository
  ✓ Separada lógica de UI

Paso 4/4: generate-unit-tests
  ✓ Creado: TestProveedores.pas
  ✓ 5 tests generados

Pipeline completado ✓
```

## Control de Flujo

### Condiciones

```yaml
- skill: validate-paradox-table
  condition: ${HAY_ERRORES}
  skip_on_failure: true
```

### Bucles

```yaml
- skill: analyze-delphi-unit
  foreach: ${LISTA_ARCHIVOS}
  output: $resultados[]
```

### Rollback

```yaml
on_failure:
  - notify: "Error en paso ${STEP}"
  - rollback: true
```

## Crear Pipeline Personalizado

```yaml
# Guardar en: .skills/workflows/custom/mi-pipeline.yaml
name: mi-pipeline-custom
description: Descripción del workflow
author: Tu nombre

steps:
  - skill: skill-1
    input: ...
  - skill: skill-2
    input: ...
```

---

**Estado**: stable  
**Última revisión**: 2026-01-08
