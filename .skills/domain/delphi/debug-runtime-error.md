---
name: debug-runtime-error
version: 1.0.0
category: domain/delphi
tags: [debug, error, runtime, exception, crash]
author: ARAINFORIA
created: 2026-01-08
complexity: 6
triggers:
  - "error runtime"
  - "access violation"
  - "exception"
  - "programa crashea"
  - "invalid pointer"
---

# Debug Runtime Error

## Descripción

Diagnóstico y solución de errores en tiempo de ejecución en Delphi.

## Errores Comunes y Soluciones

### Access Violation (EAccessViolation)

**Causas**:

- Acceso a objeto no inicializado (nil)
- Acceso a objeto ya liberado
- Índice fuera de rango en array

**Diagnóstico**:

```pascal
procedure DiagnosticarAV;
begin
  // 1. Verificar nil antes de acceder
  if Assigned(MiObjeto) then
    MiObjeto.Metodo
  else
    raise Exception.Create('Objeto no inicializado');
    
  // 2. Usar try-except para capturar
  try
    Operacion;
  except
    on E: EAccessViolation do
      LogError('AV en: ' + E.Message);
  end;
end;
```

### Invalid Pointer Operation

**Causas**:

- Double Free (liberar objeto dos veces)
- Liberar memoria no asignada

**Solución**:

```pascal
// ANTES (problemático)
MiObjeto.Free;
MiObjeto.Free; // ¡Double free!

// DESPUÉS (correcto)
FreeAndNil(MiObjeto); // Pone a nil después de liberar
if Assigned(MiObjeto) then
  FreeAndNil(MiObjeto);
```

### List Index Out of Bounds

```pascal
// ANTES
Item := Lista[5]; // Puede fallar si Count < 6

// DESPUÉS
if (Index >= 0) and (Index < Lista.Count) then
  Item := Lista[Index]
else
  raise ERangeError.CreateFmt('Índice %d fuera de rango', [Index]);
```

### Stack Overflow

**Causas**: Recursión infinita

```pascal
// Detectar recursión
procedure MetodoRecursivo(Nivel: Integer);
begin
  if Nivel > 100 then
    raise Exception.Create('Recursión demasiado profunda');
  MetodoRecursivo(Nivel + 1);
end;
```

## Herramientas de Debug

### Activar Excepciones del Depurador

```text
Project > Options > Debugger > Delphi Debugger:
✓ Notify on Language Exceptions
✓ Integrated Debugging
```

### Usar MadExcept

```pascal
uses
  madExcept;

// Configurar manejo global
RegisterExceptionHandler(MiManejador, stTrySyncCallOnMainThread);

procedure MiManejador(const ExcHandlerInfo: TExcHandlerInfo);
begin
  LogError(ExcHandlerInfo.ExcMessage);
  LogError(ExcHandlerInfo.StackTrace);
end;
```

### Log de Excepciones

```pascal
procedure LogException(E: Exception);
var
  F: TextFile;
begin
  AssignFile(F, 'errors.log');
  if FileExists('errors.log') then Append(F) else Rewrite(F);
  WriteLn(F, Format('%s | %s: %s', 
    [DateTimeToStr(Now), E.ClassName, E.Message]));
  CloseFile(F);
end;
```

## Checklist de Debug

- [ ] ¿El objeto está inicializado (Assigned)?
- [ ] ¿El objeto ya fue liberado (FreeAndNil)?
- [ ] ¿Los índices están en rango?
- [ ] ¿Hay recursión infinita?
- [ ] ¿Los threads acceden a recursos compartidos sin sincronización?

---

**Estado**: stable
