---
name: full-feature-development
version: 1.0.0
category: workflows/development
complexity: 9
tokens_estimate: 2000-3500
duration: 60-120 min
tags: [workflow, development, agile, full-cycle, feature]
requires: []
dependencies:
  - analyze-delphi-unit
  - generate-boilerplate
  - generate-unit-tests
  - implement-design-pattern
---

# 🚀 Full Feature Development Workflow

## Descripción

Workflow completo para desarrollar una nueva funcionalidad de principio a fin, desde el análisis de requisitos hasta la documentación final. Integra múltiples skills para garantizar calidad y consistencia.

## Cuándo Usar

- Al iniciar el desarrollo de una nueva funcionalidad
- Para estandarizar el proceso de desarrollo
- Cuando se quiere asegurar que no se omitan pasos críticos
- Para onboarding de nuevos desarrolladores

## Inputs

| Parámetro | Tipo | Requerido | Descripción |
| --------- | ---- | --------- | ----------- |
| `feature_name` | string | ✅ | Nombre de la funcionalidad |
| `description` | string | ✅ | Descripción detallada |
| `requirements` | array | ✅ | Lista de requisitos funcionales |
| `acceptance_criteria` | array | ✅ | Criterios de aceptación |
| `affected_modules` | array | ❌ | Módulos afectados |
| `database_changes` | boolean | ❌ | Requiere cambios en BD |
| `priority` | string | ❌ | `low`, `medium`, `high`, `critical` |

## Outputs

| Output | Tipo | Descripción |
| ------ | ---- | ----------- |
| `implementation_plan` | object | Plan detallado de implementación |
| `code_files` | array | Archivos de código generados |
| `test_files` | array | Archivos de test generados |
| `documentation` | string | Documentación de la funcionalidad |
| `review_checklist` | array | Checklist de revisión |

## Fases del Workflow

### 📋 FASE 1: Análisis y Diseño (15-30 min)

#### 1.1 Analizar Requisitos

```text
ENTRADA: requirements + acceptance_criteria

TAREAS:
├── Desglosar requisitos en tareas atómicas
├── Identificar dependencias entre tareas
├── Estimar complejidad de cada tarea
├── Detectar riesgos potenciales
└── Validar viabilidad técnica

SALIDA: Lista de tareas con estimaciones
```

#### 1.2 Diseñar Arquitectura

```text
ENTRADA: Lista de tareas + affected_modules

TAREAS:
├── Identificar clases/unidades necesarias
├── Definir interfaces públicas
├── Mapear integraciones con código existente
├── Seleccionar patrones de diseño apropiados
└── Crear diagrama de componentes

SALIDA: Diagrama de arquitectura + lista de clases
```

#### 1.3 Planificar Base de Datos (si aplica)

```text
SI database_changes = true:
├── Identificar tablas afectadas
├── Diseñar nuevos campos/tablas
├── Crear script de migración
├── Crear script de rollback
└── Documentar cambios de esquema
```

### 🔧 FASE 2: Preparación del Entorno (10-15 min)

#### 2.1 Crear Estructura de Archivos

```pascal
// Estructura típica para nueva funcionalidad
📁 {Proyecto}
├── 📁 Source
│   ├── u{Feature}.pas           // Lógica de negocio
│   ├── uFrm{Feature}.pas        // Formulario (si aplica)
│   ├── uFrm{Feature}.dfm        // Diseño del form
│   └── uDm{Feature}.pas         // DataModule (si aplica)
├── 📁 Tests
│   └── uTest{Feature}.pas       // Tests unitarios
├── 📁 Docs
│   └── {Feature}.md             // Documentación
└── 📁 SQL
    └── migration_{Feature}.sql  // Script BD (si aplica)
```

#### 2.2 Generar Boilerplate

```yaml
@skill:core/generation/generate-boilerplate
component_type: "unit"
name: "{Feature}"
options:
  include_interface: true
  include_implementation: true
  use_strict_types: true
```

### 💻 FASE 3: Implementación (30-60 min)

#### 3.1 Implementar Capa de Datos (si aplica)

```text
TAREAS:
├── Ejecutar migraciones de BD
├── Crear/actualizar DataModule
├── Implementar métodos CRUD
├── Añadir validaciones de datos
└── Probar conexión y operaciones básicas
```

#### 3.2 Implementar Lógica de Negocio

```pascal
// Estructura recomendada
unit u{Feature};

interface

uses
  System.SysUtils, System.Classes;

type
  // Interfaz para facilitar testing
  I{Feature}Service = interface
    ['{GUID}']
    function Ejecutar(const AParams: T{Feature}Params): T{Feature}Result;
  end;

  T{Feature}Service = class(TInterfacedObject, I{Feature}Service)
  private
    // Dependencias inyectadas
    FRepository: I{Feature}Repository;
    // Métodos privados
    procedure ValidarParametros(const AParams: T{Feature}Params);
  public
    constructor Create(ARepository: I{Feature}Repository);
    function Ejecutar(const AParams: T{Feature}Params): T{Feature}Result;
  end;

implementation

// Implementación...

end.
```

#### 3.3 Implementar Interfaz de Usuario (si aplica)

```text
TAREAS:
├── Diseñar layout del formulario
├── Conectar controles a datos
├── Implementar validaciones visuales
├── Añadir feedback al usuario (mensajes, estados)
├── Implementar atajos de teclado
└── Asegurar accesibilidad (Tab order, hints)
```

### 🧪 FASE 4: Testing (15-30 min)

#### 4.1 Generar Tests Unitarios

```yaml
@skill:core/generation/generate-unit-tests
source_code: "{código implementado}"
target_unit: "u{Feature}"
framework: "dunitx"
include_edge_cases: true
```

#### 4.2 Ejecutar Tests

```text
VERIFICAR:
├── Todos los tests pasan ✅
├── Cobertura > 80%
├── Sin warnings del compilador
└── Sin hints del compilador
```

#### 4.3 Testing Manual

```text
CHECKLIST:
├── [ ] Funcionalidad principal funciona
├── [ ] Casos límite manejados
├── [ ] Errores mostrados correctamente
├── [ ] Rendimiento aceptable
├── [ ] Sin memory leaks (ReportMemoryLeaksOnShutdown)
└── [ ] Integración con módulos existentes OK
```

### 📝 FASE 5: Documentación (10-15 min)

#### 5.1 Documentar Código

```pascal
/// <summary>
/// Procesa la funcionalidad {Feature}.
/// </summary>
/// <param name="AParams">Parámetros de entrada</param>
/// <returns>Resultado del procesamiento</returns>
/// <exception cref="E{Feature}Exception">
/// Lanzada cuando los parámetros son inválidos
/// </exception>
function T{Feature}Service.Ejecutar(
  const AParams: T{Feature}Params): T{Feature}Result;
```

#### 5.2 Crear Documentación de Usuario

```markdown
# {Feature}

## Descripción
{Descripción de la funcionalidad}

## Cómo Usar
1. Paso 1...
2. Paso 2...

## Requisitos
- Requisito 1
- Requisito 2

## Limitaciones Conocidas
- Limitación 1

## FAQ
### ¿Cómo hago X?
Respuesta...
```

### ✅ FASE 6: Revisión y Entrega (10-15 min)

#### 6.1 Code Review Checklist

```text
CÓDIGO:
├── [ ] Nomenclatura consistente
├── [ ] Sin código duplicado
├── [ ] Métodos <= 30 líneas
├── [ ] Clases con responsabilidad única
├── [ ] Sin magic numbers
├── [ ] Try-finally para recursos
└── [ ] Sin warnings/hints

ARQUITECTURA:
├── [ ] Separación de capas correcta
├── [ ] Dependencias inyectadas
├── [ ] Interfaces donde corresponde
└── [ ] Patrones aplicados correctamente

TESTING:
├── [ ] Cobertura adecuada
├── [ ] Tests independientes
├── [ ] Nombres descriptivos
└── [ ] Sin leaks de memoria

DOCUMENTACIÓN:
├── [ ] Código documentado
├── [ ] README actualizado
└── [ ] Changelog actualizado
```

#### 6.2 Preparar Commit

```bash
# Formato de mensaje de commit
feat({módulo}): {descripción breve}

{Descripción detallada}

- Implementado: {lista de funcionalidades}
- Tests: {cobertura}%
- Docs: Actualizada

Closes #{número-issue}
```

## Ejemplo de Uso

```yaml
@skill:workflows/development/full-feature-development
feature_name: "Exportar Facturas a XML Verifactu"
description: "Permitir exportar facturas al formato XML requerido por la AEAT para Verifactu"
requirements:
  - "Generar XML según esquema XSD de la AEAT"
  - "Incluir firma digital"
  - "Validar XML antes de exportar"
  - "Permitir exportación individual y masiva"
acceptance_criteria:
  - "El XML generado pasa validación contra XSD"
  - "La firma digital es válida"
  - "Se puede exportar una factura en menos de 2 segundos"
  - "Log de exportaciones guardado"
affected_modules:
  - "uFacturas"
  - "uVerifactu"
database_changes: true
priority: "high"
```

## Timeline Visual

```text
┌─────────────────────────────────────────────────────────────────┐
│ FULL FEATURE DEVELOPMENT WORKFLOW                               │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│ FASE 1: Análisis        ████████░░░░░░░░░░░░░░░░  15-30 min    │
│ FASE 2: Preparación     ██████░░░░░░░░░░░░░░░░░░  10-15 min    │
│ FASE 3: Implementación  ████████████████░░░░░░░░  30-60 min    │
│ FASE 4: Testing         ████████░░░░░░░░░░░░░░░░  15-30 min    │
│ FASE 5: Documentación   ██████░░░░░░░░░░░░░░░░░░  10-15 min    │
│ FASE 6: Revisión        ██████░░░░░░░░░░░░░░░░░░  10-15 min    │
│                                                                 │
│ TOTAL ESTIMADO: 90-165 minutos                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Historial de Cambios

| Versión | Fecha | Cambios |
| ------- | ----- | ------- |
| 1.0.0 | 2026-01-07 | Versión inicial |
