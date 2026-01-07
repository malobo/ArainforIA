---
name: generate-boilerplate
version: 1.0.0
category: core/generation
tags: [generation, boilerplate, code, templates]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 4
estimated_tokens: 500-800
---

# Generar Boilerplate de Código

## Descripción

Genera código boilerplate (plantilla base) para diferentes tipos de componentes según el lenguaje y framework especificado.

## Objetivo

Acelerar el desarrollo proporcionando estructuras de código base bien formadas y siguiendo las mejores prácticas del proyecto.

## Inputs

- **component_type** (string): Tipo de componente [class|unit|form|procedure|interface|record]
- **name** (string): Nombre del componente a generar
- **language** (string, opcional): Lenguaje [delphi|pascal] (default: delphi)
- **template** (string, opcional): Plantilla específica a usar
- **options** (object, opcional): Opciones adicionales de generación

## Outputs

- **code** (string): Código generado
- **filename** (string): Nombre de archivo sugerido
- **dependencies** (array): Unidades/librerías necesarias
- **instructions** (string): Instrucciones de uso

## Precondiciones

- Nombre de componente debe ser válido (sin espacios, caracteres especiales)
- Tipo de componente debe ser soportado

## Postcondiciones

- Código generado es sintácticamente válido
- Sigue convenciones del proyecto
- Incluye comentarios de documentación

## Procedimiento

### Paso 1: Validar Inputs

Verificar:

- Nombre es identificador válido (PascalCase para clases, etc.)
- Tipo de componente es soportado
- Lenguaje es soportado

**Validación**: Inputs válidos

### Paso 2: Seleccionar Plantilla

Basado en component_type, seleccionar plantilla:

**Delphi/Pascal**:

- `class` → Clase con constructor/destructor
- `unit` → Unidad completa con interface/implementation
- `form` → Formulario con eventos básicos
- `procedure` → Procedimiento con documentación
- `interface` → Interface con métodos
- `record` → Record con campos

**Validación**: Plantilla seleccionada

### Paso 3: Aplicar Variables

Reemplazar placeholders con valores:

- `{{NAME}}` → Nombre del componente
- `{{DATE}}` → Fecha actual
- `{{AUTHOR}}` → Autor (de configuración)
- `{{UNIT_NAME}}` → Nombre de la unidad

**Validación**: Variables aplicadas

### Paso 4: Aplicar Opciones

Si options incluye:

- `add_logging: true` → Añadir código de logging
- `add_error_handling: true` → Añadir try/except
- `add_comments: true` → Añadir comentarios XML
- `implement_interface: "IMyInterface"` → Implementar interface

**Validación**: Opciones aplicadas

### Paso 5: Generar Código Final

Ensamblar código final con:

- Header con copyright/autor
- Uses clause con dependencias
- Código del componente
- Comentarios de documentación

**Validación**: Código sintácticamente válido

### Paso 6: Generar Metadatos

Crear objeto de respuesta con:

- Código generado
- Nombre de archivo sugerido
- Lista de dependencias
- Instrucciones de uso

**Validación**: Respuesta completa

## Ejemplos de Uso

### Ejemplo 1: Generar Clase Básica

**Contexto**: Nueva clase de negocio

**Input**:

```
component_type: "class"
name: "TFacturaService"
language: "delphi"
options: {
  "add_error_handling": true,
  "add_comments": true
}
```

**Output Esperado**:

```pascal
{*******************************************************}
{                                                       }
{       TFacturaService                                 }
{       Generado: 2026-01-07                           }
{                                                       }
{*******************************************************}

unit FacturaService;

interface

uses
  System.SysUtils, System.Classes;

type
  /// <summary>
  /// Servicio para gestión de facturas
  /// </summary>
  TFacturaService = class
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create;
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

{ TFacturaService }

constructor TFacturaService.Create;
begin
  inherited Create;
  try
    // Inicialización
  except
    on E: Exception do
      raise Exception.Create('Error al crear TFacturaService: ' + E.Message);
  end;
end;

destructor TFacturaService.Destroy;
begin
  // Limpieza
  inherited Destroy;
end;

end.
```

### Ejemplo 2: Generar Unidad con Interface

**Contexto**: Nueva unidad con interface para inyección de dependencias

**Input**:

```
component_type: "interface"
name: "IVerifactuService"
language: "delphi"
```

**Output Esperado**:

```pascal
unit uVerifactuService;

interface

type
  /// <summary>
  /// Interface para servicio de Verifactu
  /// </summary>
  IVerifactuService = interface
    ['{GUID-GENERADO}']
    function GenerarHash(const AFactura: string): string;
    function ValidarCadena: Boolean;
    procedure EnviarAEAT;
  end;

implementation

end.
```

## Manejo de Errores

### Error 1: Nombre inválido

**Síntoma**: Error de validación de nombre
**Causa**: Nombre contiene caracteres no válidos o palabras reservadas
**Solución**: Usar PascalCase sin caracteres especiales

### Error 2: Tipo no soportado

**Síntoma**: component_type no reconocido
**Causa**: Tipo de componente no implementado
**Solución**: Usar uno de los tipos soportados: class, unit, form, procedure, interface, record

### Error 3: Conflicto de opciones

**Síntoma**: Opciones incompatibles especificadas
**Causa**: Combinación de opciones no válida
**Solución**: Revisar documentación de opciones compatibles

## Optimizaciones

### Optimización de Tokens

- Usar plantillas compactas
- Solo incluir opciones solicitadas
- Omitir comentarios si no se requieren

### Optimización de Calidad

- Validar código generado
- Usar convenciones del proyecto
- Incluir TODO para partes a completar

## Dependencias

### Skills Requeridas

- Ninguna

### Herramientas Externas

- Ninguna

## Variantes

### Variante 1: Generar Test Unit

Generar unidad de tests para una clase existente

### Variante 2: Generar CRUD Completo

Generar clase con métodos CRUD básicos (Create, Read, Update, Delete)

### Variante 3: Generar desde Interface

Generar clase que implementa una interface existente

## Métricas de Éxito

- [ ] Código generado compila sin errores
- [ ] Sigue convenciones del proyecto
- [ ] Incluye documentación básica
- [ ] Dependencias correctamente listadas

## Notas

- Los GUIDs para interfaces se generan automáticamente
- El código generado es punto de partida, requiere personalización
- Revisar y adaptar según necesidades específicas

## Referencias

- [Delphi Coding Standards](https://docwiki.embarcadero.com/RADStudio/en/Delphi_Coding_Standards)
- [Object Pascal Style Guide](https://edn.embarcadero.com/article/10280)

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
- Soporte para class, unit, form, procedure, interface, record
- Opciones de logging, error handling y comentarios

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
