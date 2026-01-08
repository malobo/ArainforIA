---
name: generate-singleton
version: 1.0.0
category: domain/delphi
tags: [singleton, pattern, global, instancia]
author: ARAINFORIA
created: 2026-01-08
complexity: 4
triggers:
  - "singleton delphi"
  - "instancia unica"
  - "patron singleton"
  - "clase global"
---

# Generar Singleton

## Descripción

Implementar el patrón Singleton en Delphi para clases con instancia única.

## Singleton Thread-Safe

```pascal
unit uConfiguracion;

interface

type
  TConfiguracion = class
  private
    class var FInstance: TConfiguracion;
    class var FLock: TObject;
    
    FRutaBD: string;
    FUsuario: string;
    
    constructor CreatePrivate;
  public
    class function GetInstance: TConfiguracion;
    class procedure ReleaseInstance;
    
    property RutaBD: string read FRutaBD write FRutaBD;
    property Usuario: string read FUsuario write FUsuario;
    
    procedure Cargar;
    procedure Guardar;
  end;

implementation

uses
  SysUtils, IniFiles;

constructor TConfiguracion.CreatePrivate;
begin
  inherited Create;
  Cargar;
end;

class function TConfiguracion.GetInstance: TConfiguracion;
begin
  if not Assigned(FInstance) then
  begin
    TMonitor.Enter(FLock);
    try
      if not Assigned(FInstance) then
        FInstance := TConfiguracion.CreatePrivate;
    finally
      TMonitor.Exit(FLock);
    end;
  end;
  Result := FInstance;
end;

class procedure TConfiguracion.ReleaseInstance;
begin
  FreeAndNil(FInstance);
end;

procedure TConfiguracion.Cargar;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('config.ini');
  try
    FRutaBD := Ini.ReadString('Database', 'Path', '');
    FUsuario := Ini.ReadString('User', 'Name', '');
  finally
    Ini.Free;
  end;
end;

procedure TConfiguracion.Guardar;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create('config.ini');
  try
    Ini.WriteString('Database', 'Path', FRutaBD);
    Ini.WriteString('User', 'Name', FUsuario);
  finally
    Ini.Free;
  end;
end;

initialization
  TConfiguracion.FLock := TObject.Create;
  
finalization
  TConfiguracion.ReleaseInstance;
  TConfiguracion.FLock.Free;

end.
```

## Uso

```pascal
// En cualquier parte del código
TConfiguracion.GetInstance.RutaBD := 'C:\Datos';
ShowMessage(TConfiguracion.GetInstance.Usuario);

// Alias para comodidad
function Config: TConfiguracion;
begin
  Result := TConfiguracion.GetInstance;
end;

// Uso simplificado
Config.RutaBD := 'C:\Datos';
```

## Singleton Simple (Sin Thread-Safety)

```pascal
type
  TMiSingleton = class
  private
    class var FInstance: TMiSingleton;
  public
    class function Instance: TMiSingleton;
  end;

class function TMiSingleton.Instance: TMiSingleton;
begin
  if not Assigned(FInstance) then
    FInstance := TMiSingleton.Create;
  Result := FInstance;
end;
```

---

**Estado**: stable
