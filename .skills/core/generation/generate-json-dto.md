---
id: skill-generate-json-dto
name: Generar JSON DTOs (Data Transfer Objects)
version: 1.0.0
category: core/generation
priority: high
last_updated: 2026-01-08
triggers:
  - "crear dto"
  - "json structure"
  - "serializar objeto"
  - "clase para json"
---

# 📦 Generar JSON DTOs

<context>
Esta skill genera clases Delphi (DTOs) diseñadas específicamente para ser serializadas/deserializadas a JSON automáticamente. Se prioriza el uso de frameworks modernos como `mORMot 2` o las capacidades nativas de `System.JSON` / `REST.Json`.
</context>

<instruction>
Al generar un DTO:

1. **Estructura de Clase**:
    * Definir campos `published` o `public` (dependiendo del serializador).
    * Usar atributos para mapear nombres de propiedades si difieren del JSON (ej: `[JSONName('api_key')]`).
2. **Tipos de Datos**:
    * `TDateTime` -> ISO 8601 String (generalmente).
    * Listas -> `TList<T>` o `TObjectList<T>`.
3. **Métodos Helper**:
    * Incluir `ToJson()` y `FromJson()` estáticos para facilitar el uso.

</instruction>

<examples>
User: "Genera un DTO para un Cliente con ID, Nombre y Lista de Pedidos"
Agent:
```delphi
uses System.Generics.Collections, REST.Json.Types;

type
  TPedidoDTO = class
  public
    [JSONName('id')]
    Id: Integer;
    [JSONName('total')]
    Total: Double;
  end;

  TClienteDTO = class
  public
    [JSONName('id')]
    Id: Integer;
    [JSONName('nombre')]
    Nombre: string;
    [JSONName('pedidos')]
    Pedidos: TList<TPedidoDTO>;

    constructor Create;
    destructor Destroy; override;
    function ToJson: string;
    class function FromJson(const AJson: string): TClienteDTO;
  end;

```
</examples>
