---
name: validate-paradox-table
version: 1.0.0
category: domain/database
tags: [paradox, bde, integridad, reparar, db]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 5
estimated_tokens: 400-600
triggers:
  - "tabla corrupta"
  - "paradox roto"
  - "reparar db"
  - "verificar tabla"
  - "indice dañado"
---

# Validar Integridad Tabla Paradox

## Descripción

Verifica y repara tablas Paradox (.DB) detectando corrupción, índices dañados y bloqueos.

## AI Context

> **SYSTEM_INSTRUCTION**: Diagnose and repair Paradox table issues. Provide step-by-step solutions.
> **OUTPUT_FORMAT**: Diagnostic report + repair commands/code.
> **TOKEN_STRATEGY**: Focus on actionable solutions.

## Problemas Comunes y Soluciones

### 1. Tabla Bloqueada (.LCK)

**Síntomas**: "Table is busy" o "Record locked by another user"

**Solución**:

```text
1. Cerrar todas las aplicaciones que usan la tabla
2. Eliminar archivos de bloqueo:
   - NombreTabla.lck
   - PDOXUSRS.NET (en directorio de red)
3. Reiniciar la aplicación
```

**Código Delphi**:

```pascal
procedure EliminarBloqueos(const DirectorioTabla: string);
var
  SR: TSearchRec;
begin
  if FindFirst(DirectorioTabla + '\*.lck', faAnyFile, SR) = 0 then
  begin
    repeat
      DeleteFile(DirectorioTabla + '\' + SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  
  // Eliminar PDOXUSRS.NET si existe
  if FileExists(DirectorioTabla + '\PDOXUSRS.NET') then
    DeleteFile(DirectorioTabla + '\PDOXUSRS.NET');
end;
```

### 2. Índice Corrupto (.PX, .Xnn, .Ynn)

**Síntomas**: "Index is out of date" o resultados incorrectos

**Solución con Database Desktop**:

```text
1. Abrir Database Desktop (DBD32.EXE)
2. Tools > Utilities > Restructure
3. Seleccionar la tabla
4. Sin hacer cambios, click "Restructure"
5. Esto reconstruye todos los índices
```

**Solución con código**:

```pascal
procedure ReconstruirIndices(const NombreTabla: string);
var
  Table: TTable;
begin
  Table := TTable.Create(nil);
  try
    Table.DatabaseName := ExtractFilePath(NombreTabla);
    Table.TableName := ExtractFileName(NombreTabla);
    Table.Exclusive := True;
    Table.Open;
    
    // Forzar reconstrucción
    Table.EmptyTable;  // ¡CUIDADO! Solo si tienes backup
    // Alternativa: copiar datos a tabla temporal y reimportar
    
    Table.Close;
  finally
    Table.Free;
  end;
end;
```

### 3. Registros Corruptos

**Síntomas**: Datos ilegibles, caracteres extraños, errores al leer

**Diagnóstico**:

```pascal
procedure DiagnosticarTabla(const NombreTabla: string);
var
  Table: TTable;
  ErrorCount: Integer;
begin
  Table := TTable.Create(nil);
  ErrorCount := 0;
  try
    Table.DatabaseName := ExtractFilePath(NombreTabla);
    Table.TableName := ExtractFileName(NombreTabla);
    Table.Open;
    
    while not Table.Eof do
    begin
      try
        // Intentar leer todos los campos
        for var I := 0 to Table.FieldCount - 1 do
          Table.Fields[I].AsString;  // Fuerza lectura
      except
        on E: Exception do
        begin
          Inc(ErrorCount);
          WriteLn('Error en registro ', Table.RecNo, ': ', E.Message);
        end;
      end;
      Table.Next;
    end;
    
    WriteLn('Total errores: ', ErrorCount);
  finally
    Table.Free;
  end;
end;
```

### 4. Archivo .MB Corrupto (Memos/BLOBs)

**Síntomas**: Campos memo vacíos o con basura

**Solución**:

```text
1. Exportar tabla a CSV (sin memos)
2. Recrear estructura de tabla
3. Importar datos desde CSV
4. Reingresar memos manualmente
```

## Checklist de Verificación

- [ ] Verificar tamaño de archivo .DB (no debe ser 0 bytes)
- [ ] Comprobar que existe .PX si hay clave primaria
- [ ] Verificar permisos de lectura/escritura
- [ ] Comprobar espacio en disco
- [ ] Verificar que no hay archivos .LCK huérfanos
- [ ] Comparar número de registros con respaldo

## Herramientas Útiles

| Herramienta | Propósito |
| ----------- | --------- |
| Database Desktop | Restructure, reparar |
| Paradox for Windows | Edición avanzada |
| PXTOOLS | Utilidades de reparación |
| Data Pump | Migración de datos |

## Prevención

```pascal
// Siempre cerrar tablas correctamente
try
  Table.Post;  // Guardar cambios pendientes
finally
  Table.Close;
end;

// Usar transacciones cuando sea posible
// Hacer backups regulares
```

---

**Estado**: stable  
**Última revisión**: 2026-01-08
