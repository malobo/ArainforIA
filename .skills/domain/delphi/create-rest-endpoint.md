---
name: create-rest-endpoint
version: 1.0.0
category: domain/delphi
tags: [rest, api, mormot, servidor, endpoint]
author: ARAINFORIA
created: 2026-01-08
complexity: 7
triggers:
  - "crear api"
  - "endpoint rest"
  - "servicio web"
  - "exponer datos"
---

# Crear Endpoint REST

## Descripción

Crea endpoints REST con mORMot2 para exponer funcionalidad como API.

## Estructura Básica

```pascal
uses
  mormot.core.base,
  mormot.core.json,
  mormot.rest.http.server,
  mormot.rest.server;

type
  TApiServer = class(TRestServerFullMemory)
  published
    function GetClientes(Ctxt: TRestServerUriContext): Integer;
    function GetCliente(Ctxt: TRestServerUriContext): Integer;
    function PostCliente(Ctxt: TRestServerUriContext): Integer;
  end;
```

## Implementar Endpoints

```pascal
function TApiServer.GetClientes(Ctxt: TRestServerUriContext): Integer;
var
  Clientes: TClienteDTOArray;
begin
  Clientes := CargarTodosClientes;
  Ctxt.Returns(DynArraySaveJson(Clientes, TypeInfo(TClienteDTOArray)));
  Result := HTTP_SUCCESS;
end;

function TApiServer.GetCliente(Ctxt: TRestServerUriContext): Integer;
var
  Id: Integer;
  Cliente: TClienteDTO;
begin
  Id := Ctxt.InputInt['id'];
  if Id > 0 then
  begin
    Cliente := CargarCliente(Id);
    Ctxt.Returns(RecordSaveJson(Cliente, TypeInfo(TClienteDTO)));
    Result := HTTP_SUCCESS;
  end
  else
    Result := HTTP_BADREQUEST;
end;

function TApiServer.PostCliente(Ctxt: TRestServerUriContext): Integer;
var
  Cliente: TClienteDTO;
begin
  RecordLoadJson(Cliente, Ctxt.InputUtf8, TypeInfo(TClienteDTO));
  GuardarCliente(Cliente);
  Ctxt.Returns('{"success": true}');
  Result := HTTP_CREATED;
end;
```

## Iniciar Servidor

```pascal
var
  Server: TApiServer;
  HttpServer: TRestHttpServer;
begin
  Server := TApiServer.Create(nil, 'api');
  HttpServer := TRestHttpServer.Create('8080', Server);
  try
    WriteLn('API escuchando en http://localhost:8080');
    ReadLn;
  finally
    HttpServer.Free;
    Server.Free;
  end;
end;
```

## Endpoints Resultantes

| Método | URL | Acción |
| ------ | --- | ------ |
| GET | `/api/clientes` | Listar todos |
| GET | `/api/cliente?id=1` | Obtener uno |
| POST | `/api/cliente` | Crear nuevo |

---

**Estado**: stable
