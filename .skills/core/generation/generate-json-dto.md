---
name: generate-json-dto
version: 1.0.0
category: core/generation
tags: [json, dto, serialization, mormot, api, hybrid]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 4
estimated_tokens: 400-600
triggers:
  - "crear dto"
  - "json structure"
  - "serializar objeto"
  - "deserializar json"
  - "record json"
---

# Generar DTO para JSON

## Descripción

Genera DTOs (Data Transfer Objects) en Delphi para serialización/deserialización JSON usando mORMot2 o System.JSON.

## AI Context

> **SYSTEM_INSTRUCTION**: Generate Delphi DTOs for JSON serialization. Use records for simple data, classes for complex.
> **OUTPUT_FORMAT**: Complete Delphi code with type definitions and usage.
> **TOKEN_STRATEGY**: Minimal code, practical examples.

## Inputs

- **json_sample** (string): JSON de ejemplo
- **dto_name** (string): Nombre del tipo a generar
- **framework** (enum): mormot | system_json
- **style** (enum): record | class

## Template: Record con mORMot2

```pascal
type
  /// DTO para [descripción]
  TClienteDTO = packed record
    Id: Integer;
    Nombre: RawUtf8;
    Email: RawUtf8;
    FechaAlta: TDateTime;
    Activo: Boolean;
  end;
  PClienteDTO = ^TClienteDTO;

  /// Array de clientes
  TClienteDTOArray = array of TClienteDTO;

// Serialización
function ClienteToJson(const Cliente: TClienteDTO): RawUtf8;
begin
  Result := RecordSaveJson(Cliente, TypeInfo(TClienteDTO));
end;

// Deserialización
function JsonToCliente(const Json: RawUtf8): TClienteDTO;
begin
  RecordLoadJson(Result, pointer(Json), TypeInfo(TClienteDTO));
end;

// Array
function JsonToClientes(const Json: RawUtf8): TClienteDTOArray;
begin
  DynArrayLoadJson(Result, pointer(Json), TypeInfo(TClienteDTOArray));
end;
```

## Template: Clase con mORMot2

```pascal
type
  TFacturaDTO = class(TSynPersistent)
  private
    FNumero: RawUtf8;
    FCliente: TClienteDTO;
    FLineas: TLineaDTOArray;
    FTotal: Currency;
  published
    property Numero: RawUtf8 read FNumero write FNumero;
    property Cliente: TClienteDTO read FCliente write FCliente;
    property Lineas: TLineaDTOArray read FLineas write FLineas;
    property Total: Currency read FTotal write FTotal;
  end;

// Uso
var
  Factura: TFacturaDTO;
  Json: RawUtf8;
begin
  Factura := TFacturaDTO.Create;
  try
    ObjectLoadJson(Factura, Json);
    // ... trabajar con Factura
    Json := ObjectToJson(Factura);
  finally
    Factura.Free;
  end;
end;
```

## Template: System.JSON (sin mORMot)

```pascal
uses
  System.JSON, REST.Json;

type
  [JsonReflect]
  TProductoDTO = class
  private
    FCodigo: string;
    FDescripcion: string;
    FPrecio: Double;
  public
    property Codigo: string read FCodigo write FCodigo;
    property Descripcion: string read FDescripcion write FDescripcion;
    property Precio: Double read FPrecio write FPrecio;
  end;

// Serialización
function ProductoToJson(Producto: TProductoDTO): string;
begin
  Result := TJson.ObjectToJsonString(Producto);
end;

// Deserialización
function JsonToProducto(const Json: string): TProductoDTO;
begin
  Result := TJson.JsonToObject<TProductoDTO>(Json);
end;
```

## Mapeo de Tipos JSON → Delphi

| JSON | Delphi (mORMot) | Delphi (System.JSON) |
|------|-----------------|----------------------|
| `string` | `RawUtf8` | `string` |
| `number` (int) | `Integer` / `Int64` | `Integer` / `Int64` |
| `number` (float) | `Double` / `Currency` | `Double` |
| `boolean` | `Boolean` | `Boolean` |
| `null` | Valor por defecto | Nullable o nil |
| `array` | `array of T` | `TArray<T>` |
| `object` | `record` o `class` | `class` |
| `date` (ISO) | `TDateTime` | `TDateTime` |

## Ejemplo Práctico: API de Facturas

**JSON de entrada:**

```json
{
  "numero": "F2026-001",
  "fecha": "2026-01-08",
  "cliente": {
    "nif": "12345678A",
    "nombre": "Empresa S.L."
  },
  "lineas": [
    {"concepto": "Servicio", "cantidad": 1, "precio": 100.00}
  ],
  "total": 121.00
}
```

**DTO generado:**

```pascal
type
  TClienteFacturaDTO = packed record
    Nif: RawUtf8;
    Nombre: RawUtf8;
  end;
  
  TLineaFacturaDTO = packed record
    Concepto: RawUtf8;
    Cantidad: Integer;
    Precio: Currency;
  end;
  TLineaFacturaDTOArray = array of TLineaFacturaDTO;
  
  TFacturaDTO = packed record
    Numero: RawUtf8;
    Fecha: TDateTime;
    Cliente: TClienteFacturaDTO;
    Lineas: TLineaFacturaDTOArray;
    Total: Currency;
  end;
```

## Validación de DTOs

```pascal
function ValidarFacturaDTO(const F: TFacturaDTO): Boolean;
begin
  Result := (F.Numero <> '') and
            (F.Cliente.Nif <> '') and
            (Length(F.Lineas) > 0) and
            (F.Total > 0);
end;
```

## Buenas Prácticas

1. **Usar records para datos simples** (menos memoria, sin destructor)
2. **Usar clases para datos complejos** (herencia, métodos)
3. **Nombres claros**: Sufijo `DTO` para distinguir de entidades
4. **Documentar con XML-doc** cada campo
5. **Validar después de deserializar**

---

**Estado**: stable  
**Última revisión**: 2026-01-08
