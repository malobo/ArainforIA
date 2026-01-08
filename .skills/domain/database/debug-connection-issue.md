---
name: debug-connection-issue
version: 1.0.0
category: domain/database
tags: [conexion, debug, bde, firedac, red]
author: ARAINFORIA
created: 2026-01-08
complexity: 5
triggers:
  - "no conecta"
  - "error conexion"
  - "bd bloqueada"
  - "timeout conexion"
---

# Diagnóstico Problemas de Conexión

## Descripción

Diagnóstico y solución de problemas de conexión a base de datos.

## Árbol de Diagnóstico

```text
¿Error de conexión?
├── ¿El archivo/servidor existe?
│   ├── SÍ → ¿Tienes permisos?
│   │   ├── SÍ → ¿Archivo bloqueado?
│   │   │   ├── SÍ → Eliminar .LCK
│   │   │   └── NO → ¿Driver correcto?
│   │   └── NO → Revisar permisos NTFS
│   └── NO → Verificar ruta/servidor
└── ¿Timeout?
    ├── SÍ → Aumentar timeout
    └── NO → Ver logs específicos
```

## Problemas Comunes

### 1. Archivo/Tabla Bloqueada

```pascal
// Verificar si hay bloqueo
function TablaEnUso(const Tabla: string): Boolean;
begin
  Result := FileExists(ChangeFileExt(Tabla, '.lck'));
end;

// Forzar limpieza
procedure LimpiarBloqueos(const Dir: string);
var
  SR: TSearchRec;
begin
  if FindFirst(Dir + '\*.lck', faAnyFile, SR) = 0 then
  repeat
    DeleteFile(Dir + '\' + SR.Name);
  until FindNext(SR) <> 0;
  FindClose(SR);
  
  // PDOXUSRS.NET
  if FileExists(Dir + '\PDOXUSRS.NET') then
    DeleteFile(Dir + '\PDOXUSRS.NET');
end;
```

### 2. Timeout de Conexión

```pascal
// BDE
Session.NetFileDir := 'C:\Temp';
Query.RequestLive := False; // Más rápido

// FireDAC
FDConnection.Params.Values['LoginTimeout'] := '30';
FDConnection.Params.Values['CommandTimeout'] := '60';
```

### 3. Ruta Incorrecta

```pascal
function VerificarRutaBD(const Ruta: string): Boolean;
begin
  if not DirectoryExists(Ruta) then
  begin
    ShowMessage('Directorio no existe: ' + Ruta);
    Result := False;
    Exit;
  end;
  
  // Verificar al menos una tabla
  if not FileExists(Ruta + '\CLIENTES.DB') then
  begin
    ShowMessage('Tabla CLIENTES.DB no encontrada');
    Result := False;
    Exit;
  end;
  
  Result := True;
end;
```

### 4. Permisos de Red

```text
Verificar:
1. Usuario tiene acceso a \\servidor\share
2. Firewall permite tráfico
3. No hay restricciones de grupo
```

### 5. Error de Driver

```pascal
// Verificar driver BDE instalado
function BDEInstalado: Boolean;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := Reg.KeyExists('SOFTWARE\Borland\Database Engine');
  finally
    Reg.Free;
  end;
end;
```

## Códigos de Error Comunes

| Código | Significado | Solución |
| ------ | ----------- | -------- |
| 9729 | Table busy | Cerrar otras apps |
| 10024 | Record locked | Esperar o forzar |
| 10048 | Directory busy | Limpiar bloqueos |
| 8449 | Insufficient rights | Revisar permisos |

## Logging de Diagnóstico

```pascal
procedure LogConexion(const Msg: string);
var
  F: TextFile;
begin
  AssignFile(F, 'conexion.log');
  if FileExists('conexion.log') then
    Append(F)
  else
    Rewrite(F);
  WriteLn(F, FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + ' - ' + Msg);
  CloseFile(F);
end;

// Uso
try
  Table.Open;
  LogConexion('Conexión exitosa a ' + Table.TableName);
except
  on E: Exception do
    LogConexion('ERROR: ' + E.Message);
end;
```

---

**Estado**: stable
