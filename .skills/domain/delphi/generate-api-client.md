---
name: generate-api-client
version: 1.0.0
category: domain/delphi
tags: [rest, api, http, mormot, aeat, integration]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 6
estimated_tokens: 600-900
triggers:
  - "consumir api"
  - "cliente rest"
  - "llamada http"
  - "integrar aeat"
  - "web service"
---

# Generar Cliente API REST

## Descripción

Genera código Delphi para consumir APIs REST usando mORMot2, incluyendo autenticación, manejo de errores y reintentos.

## AI Context

> **SYSTEM_INSTRUCTION**: Generate production-ready Delphi REST client code using mORMot2 framework.
> **OUTPUT_FORMAT**: Complete Delphi unit with interface, implementation and usage example.
> **TOKEN_STRATEGY**: Focus on reusable patterns, include error handling.

## Inputs

- **base_url** (string): URL base de la API
- **endpoints** (list): Lista de endpoints a implementar
- **auth_type** (enum): none | api_key | oauth2 | certificate
- **response_format** (enum): json | xml

## Template Base

```pascal
unit uAPIClient;

interface

uses
  SysUtils, Classes,
  mormot.core.base,
  mormot.core.json,
  mormot.core.text,
  mormot.core.unicode,
  mormot.net.client;

type
  /// Cliente base para APIs REST
  TAPIClient = class
  private
    FBaseURL: RawUtf8;
    FApiKey: RawUtf8;
    FTimeout: Integer;
    FHttpClient: THttpClientSocket;
    function DoRequest(const Method, Endpoint: RawUtf8; 
      const Body: RawUtf8 = ''): RawUtf8;
  public
    constructor Create(const ABaseURL: RawUtf8; const AApiKey: RawUtf8 = '');
    destructor Destroy; override;
    
    property BaseURL: RawUtf8 read FBaseURL;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

implementation

{ TAPIClient }

constructor TAPIClient.Create(const ABaseURL: RawUtf8; const AApiKey: RawUtf8);
begin
  inherited Create;
  FBaseURL := ABaseURL;
  FApiKey := AApiKey;
  FTimeout := 30000; // 30 segundos
  FHttpClient := THttpClientSocket.Open(FBaseURL);
end;

destructor TAPIClient.Destroy;
begin
  FHttpClient.Free;
  inherited;
end;

function TAPIClient.DoRequest(const Method, Endpoint: RawUtf8;
  const Body: RawUtf8): RawUtf8;
var
  Headers: RawUtf8;
  Status: Integer;
begin
  Headers := 'Content-Type: application/json'#13#10;
  if FApiKey <> '' then
    Headers := Headers + 'Authorization: Bearer ' + FApiKey + #13#10;
    
  Status := FHttpClient.Request(
    Endpoint,
    Method,
    30000,      // KeepAlive
    Headers,
    Body,
    '',         // ContentType (ya en headers)
    Result
  );
  
  if Status >= 400 then
    raise Exception.CreateFmt('API Error %d: %s', [Status, Result]);
end;

end.
```

## Patrones de Autenticación

### API Key (Header)

```pascal
Headers := 'X-API-Key: ' + FApiKey + #13#10;
```

### API Key (Query Parameter)

```pascal
Endpoint := Endpoint + '?api_key=' + FApiKey;
```

### OAuth2 Bearer Token

```pascal
Headers := 'Authorization: Bearer ' + FAccessToken + #13#10;
```

### Certificado Digital (AEAT)

```pascal
// Usar THttpClientSocket con certificado
FHttpClient := THttpClientSocket.Open(FBaseURL);
FHttpClient.SslOptions.CertFile := 'certificado.pfx';
FHttpClient.SslOptions.Password := 'password';
```

## Ejemplo: Cliente AEAT Verifactu

```pascal
type
  TAEATClient = class(TAPIClient)
  public
    function EnviarFactura(const XML: RawUtf8): RawUtf8;
    function ConsultarEstado(const IdFactura: RawUtf8): RawUtf8;
  end;

function TAEATClient.EnviarFactura(const XML: RawUtf8): RawUtf8;
begin
  Result := DoRequest('POST', '/verifactu/facturas', XML);
end;

function TAEATClient.ConsultarEstado(const IdFactura: RawUtf8): RawUtf8;
begin
  Result := DoRequest('GET', '/verifactu/facturas/' + IdFactura);
end;
```

## Manejo de Errores

```pascal
type
  EAPIError = class(Exception)
  public
    StatusCode: Integer;
    ResponseBody: RawUtf8;
  end;

function TAPIClient.DoRequest(...): RawUtf8;
begin
  // ...
  if Status >= 400 then
  begin
    var E := EAPIError.CreateFmt('API Error %d', [Status]);
    E.StatusCode := Status;
    E.ResponseBody := Result;
    raise E;
  end;
end;
```

## Reintentos con Backoff

```pascal
function TAPIClient.DoRequestWithRetry(const Method, Endpoint: RawUtf8;
  MaxRetries: Integer = 3): RawUtf8;
var
  I: Integer;
  WaitMs: Integer;
begin
  for I := 1 to MaxRetries do
  begin
    try
      Result := DoRequest(Method, Endpoint);
      Exit;
    except
      on E: EAPIError do
      begin
        if (E.StatusCode >= 500) and (I < MaxRetries) then
        begin
          WaitMs := 1000 * I; // Exponential backoff
          Sleep(WaitMs);
        end
        else
          raise;
      end;
    end;
  end;
end;
```

## Dependencias

- **mORMot2**: `mormot.net.client`, `mormot.core.json`
- Opcional: OpenSSL para HTTPS/certificados

---

**Estado**: stable  
**Última revisión**: 2026-01-08
