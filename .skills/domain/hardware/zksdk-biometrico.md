---
name: zksdk-biometrico
version: 1.0.0
category: domain/hardware
tags: [zksdk, biometria, huella, lector, zkteco]
author: ARAINFORIA
created: 2026-01-08
complexity: 6
triggers:
  - "huella dactilar"
  - "zksdk"
  - "biometria"
  - "lector huella"
  - "zkteco"
---

# Integración ZKSDK Biométrico

## Descripción

Integración con lectores de huella dactilar ZKTeco usando ZKSDK.

## Inicialización

```pascal
uses
  zkemkeeper_TLB;

var
  ZK: TZKEM;
  Conectado: Boolean;

procedure Conectar(const IP: string; Puerto: Integer);
begin
  ZK := TZKEM.Create(nil);
  Conectado := ZK.Connect_Net(IP, Puerto);
  if not Conectado then
    raise Exception.Create('No se pudo conectar al lector');
end;
```

## Registrar Huella

```pascal
procedure RegistrarHuella(IdUsuario: Integer; NumHuella: Integer);
var
  Resultado: Boolean;
begin
  if not Conectado then Exit;
  
  // Poner dispositivo en modo de registro
  ZK.CancelOperation;
  ZK.EnableDevice(1, False);
  
  try
    ShowMessage('Coloque el dedo en el lector (3 veces)');
    Resultado := ZK.StartEnrollEx(IntToStr(IdUsuario), NumHuella, 0);
    
    if Resultado then
      ShowMessage('Huella registrada correctamente')
    else
      ShowMessage('Error al registrar huella');
  finally
    ZK.EnableDevice(1, True);
  end;
end;
```

## Verificar Huella

```pascal
function VerificarHuella(IdUsuario: Integer): Boolean;
var
  Template: WideString;
begin
  Result := False;
  if not Conectado then Exit;
  
  // Obtener huella capturada
  if ZK.OnFinger then
  begin
    ZK.GetUserTmpExStr(1, IntToStr(IdUsuario), 0, Template, Longitud);
    Result := ZK.VerifyUserTmpExStr(Template);
  end;
end;
```

## Obtener Eventos de Marcaje

```pascal
procedure LeerMarcajes;
var
  IdUsuario, Modo, Ano, Mes, Dia, Hora, Min, Seg: Integer;
begin
  ZK.EnableDevice(1, False);
  try
    if ZK.ReadGeneralLogData(1) then
    begin
      while ZK.SSR_GetGeneralLogData(1, IdUsuario, Ano, Mes, Dia, 
        Hora, Min, Seg, Modo) do
      begin
        // Procesar marcaje
        InsertarMarcaje(IdUsuario, 
          EncodeDate(Ano, Mes, Dia) + EncodeTime(Hora, Min, Seg, 0));
      end;
    end;
  finally
    ZK.EnableDevice(1, True);
  end;
end;
```

## Sincronizar Usuarios

```pascal
procedure SincronizarUsuarios;
var
  Query: TQuery;
begin
  Query := TQuery.Create(nil);
  try
    Query.SQL.Text := 'SELECT ID, NOMBRE FROM EMPLEADOS';
    Query.Open;
    
    while not Query.Eof do
    begin
      ZK.SSR_SetUserInfo(1, 
        Query.FieldByName('ID').AsString,
        Query.FieldByName('NOMBRE').AsString,
        '', 0, True);
      Query.Next;
    end;
  finally
    Query.Free;
  end;
end;
```

---

**Estado**: stable
