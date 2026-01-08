---
id: skill-generate-api-client
name: Generador de Clientes REST Delphi
version: 1.0.0
category: domain/delphi
priority: high
last_updated: 2026-01-08
triggers:
  - "consumir api"
  - "cliente rest"
  - "llamada http"
  - "generar api client"
---

# 🌐 Generador de Clientes REST Delphi

<context>
Esta skill guía la creación de clientes HTTP/REST robustos en Delphi, prefiriendo librerías modernas como `mORMot 2` (core/mormot) o componentes nativos `System.Net.HttpClient` sobre los obsoletos `Indy` (bloqueantes).
</context>

<instruction>
Al generar código para consumir una API:

1. **Selección de Librería**:
    * **Alta Performance/Verifactu**: Usar `mORMot.net.client`.
    * **Estándar VCL**: Usar `System.Net.HttpClient.THTTPClient`.
    * **Legacy**: `TIdHTTP` (solo si no hay alternativa).
2. **Estructura**:
    * Crear una clase dedicada (ej: `TAeatService`, `TStripeClient`).
    * Implementar métodos síncronos o asíncronos según el contexto UI.
    * Usar DTOs o JSON objects para el payload.
3. **Manejo de Errores**:
    * Capturar excepciones de red.
    * Validar códigos de estado HTTP (200, 201 vs 4xx, 5xx).
</instruction>

<examples>
User: "Crea un cliente para consultar el tiempo en una API pública usando THttpClient"
Agent:
```delphi
uses System.Net.HttpClient, System.JSON;

function TWeatherClient.GetTemp(City: string): Double;
var
  Client: THTTPClient;
  Resp: IHTTPResponse;
  JSON: TJSONObject;
begin
  Client := THTTPClient.Create;
  try
    Resp := Client.Get('<https://api.weather.com/v1?q=>' + City);
    if Resp.StatusCode = 200 then begin
      JSON := TJSONObject.ParseJSONValue(Resp.ContentAsString) as TJSONObject;
      try
        Result := JSON.GetValue<Double>('temp');
      finally
        JSON.Free;
      end;
    end else
      raise Exception.Create('Error API: ' + Resp.StatusText);
  finally
    Client.Free;
  end;
end;

```
</examples>
