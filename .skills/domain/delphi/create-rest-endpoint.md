---
id: skill-create-rest-endpoint
name: Crear Endpoint REST (mORMot 2)
version: 1.0.0
category: domain/delphi
priority: high
last_updated: 2026-01-08
triggers:
  - "crear api"
  - "endpoint rest"
  - "servicio web"
  - "exponer funcion"
---

# 🌐 Crear Endpoint REST (mORMot 2)

<context>
Esta skill guía en la creación de servicios RESTful utilizando el framework `mORMot 2`. Se enfoca en la definición de interfaces de servicio (SOA) que se exponen automáticamente como endpoints JSON.
</context>

<instruction>
Pasos para exponer una función como REST:

1. **Definir la Interfaz (SOA)**:
    * Debe heredar de `IInvokable` (o `IServiceAbstract` en mORMot).
    * Los métodos deben usar tipos simples o DTOs serializables.
    * Añadir `const` a parámetros de entrada y `out` o `result` para salida.
2. **Implementar la Clase**:
    * Clase que implementa la interfaz.
3. **Registrar en el Servidor**:
    * `RestServer.ServiceDefine(TMiServicio, [IMiServicio], sicShared);`

</instruction>

<examples>
User: "Crea un endpoint que sume dos números"
Agent:
```delphi
uses mormot.core.base, mormot.core.service, mormot.core.data;

// 1. Definición Interfaz
type
  ICalculadora = interface(IInvokable)
    ['{GUID-GENERATE-HERE}']
    function Sumar(const A, B: Double): Double;
  end;

// 2. Implementación
type
  TCalculadoraService = class(TInterfacedObject, ICalculadora)
  public
    function Sumar(const A, B: Double): Double;
  end;

function TCalculadoraService.Sumar(const A, B: Double): Double;
begin
  Result := A + B;
end;

// 3. Registro (En el Server Start)
// El endpoint será accesible en: GET /api/Calculadora/Sumar?A=10&B=20
MyRestServer.ServiceDefine(TCalculadoraService, [ICalculadora], sicShared);

```
</examples>
