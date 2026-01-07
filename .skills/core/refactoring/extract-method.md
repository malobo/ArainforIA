---
name: extract-method
version: 1.0.0
category: core/refactoring
tags: [refactoring, methods, clean-code, solid]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 5
estimated_tokens: 600-900
---

# Extraer Método

## Descripción

Refactoriza código existente extrayendo un bloque de código a un nuevo método/procedimiento/función, mejorando la legibilidad y reutilización.

## Objetivo

Aplicar el principio de responsabilidad única (SRP) y mejorar la mantenibilidad del código mediante la extracción de lógica a métodos independientes.

## Inputs

- **source_code** (string): Código fuente a refactorizar
- **selection_start** (integer): Línea de inicio del bloque a extraer
- **selection_end** (integer): Línea de fin del bloque a extraer
- **new_method_name** (string): Nombre para el nuevo método
- **target_visibility** (string, opcional): Visibilidad [private|protected|public] (default: private)
- **add_documentation** (boolean, opcional): Añadir comentarios XML (default: true)

## Outputs

- **refactored_code** (string): Código refactorizado completo
- **new_method** (string): Código del nuevo método extraído
- **parameters** (array): Parámetros detectados automáticamente
- **return_type** (string): Tipo de retorno si aplica
- **warnings** (array): Advertencias sobre la extracción

## Precondiciones

- El bloque seleccionado debe ser sintácticamente completo
- El código debe estar dentro de un método existente
- No debe romper el flujo de control (sin returns parciales)

## Postcondiciones

- El nuevo método es sintácticamente válido
- El código original llama al nuevo método
- Los parámetros son correctamente pasados
- El comportamiento es equivalente al original

## Procedimiento

### Paso 1: Analizar Bloque Seleccionado

Identificar:

- Variables usadas pero definidas fuera del bloque (→ parámetros)
- Variables definidas dentro que se usan fuera (→ valor de retorno)
- Variables locales al bloque (→ variables locales del nuevo método)

**Validación**: Bloque es extraíble (no tiene múltiples puntos de salida)

### Paso 2: Determinar Parámetros

Para cada variable usada en el bloque:

- Si está definida fuera → añadir como parámetro
- Si es modificada y usada después → parámetro var
- Si solo se lee → parámetro const

**Validación**: Lista de parámetros completa

### Paso 3: Determinar Tipo de Retorno

Analizar:

- Si una variable local se usa después del bloque → function con ese tipo
- Si el bloque es una expresión → function con tipo de expresión
- Si no hay valor de retorno → procedure

**Validación**: Tipo de retorno determinado

### Paso 4: Generar Nuevo Método

Crear el método con:

- Documentación XML si add_documentation=true
- Visibilidad según target_visibility
- Parámetros ordenados (const primero, luego normales, luego var)
- Cuerpo con el código extraído
- Variables locales si las hay

```pascal
/// <summary>
/// [Descripción del método]
/// </summary>
/// <param name="Param1">Descripción</param>
/// <returns>Descripción del retorno</returns>
function TMyClass.NewMethodName(const AParam1: TType; var AParam2: TType): TReturnType;
var
  LocalVar: TType;
begin
  // Código extraído
  Result := ...;
end;
```

**Validación**: Método generado es válido

### Paso 5: Reemplazar Bloque Original

Reemplazar el bloque seleccionado con:

- Llamada al nuevo método
- Asignación de resultado si hay valor de retorno
- Variables apropiadas como argumentos

```pascal
// Antes
begin
  // Código largo aquí
  X := CalculoComplejo;
  // Más código
end;

// Después
begin
  X := CalcularValorX(A, B, C);
  // Más código
end;
```

**Validación**: Llamada correctamente generada

### Paso 6: Validar Refactorización

Verificar:

- El nuevo método compila
- La llamada tiene los tipos correctos
- El comportamiento es equivalente

**Validación**: Refactorización es correcta

## Ejemplos de Uso

### Ejemplo 1: Extracción Simple de Procedimiento

**Contexto**: Código de validación repetido

**Input**:

```
source_code: "
procedure TForm1.Button1Click(Sender: TObject);
begin
  // Línea 1
  if Edit1.Text = '' then
  begin
    ShowMessage('Campo vacío');
    Edit1.SetFocus;
    Exit;
  end;
  if Length(Edit1.Text) < 3 then
  begin
    ShowMessage('Mínimo 3 caracteres');
    Edit1.SetFocus;
    Exit;
  end;
  // Continuar procesamiento
  ProcessData(Edit1.Text);
end;
"
selection_start: 3
selection_end: 14
new_method_name: "ValidarEdit1"
```

**Output Esperado**:

```pascal
// Nuevo método
procedure TForm1.ValidarEdit1(out AValid: Boolean);
begin
  AValid := True;
  if Edit1.Text = '' then
  begin
    ShowMessage('Campo vacío');
    Edit1.SetFocus;
    AValid := False;
    Exit;
  end;
  if Length(Edit1.Text) < 3 then
  begin
    ShowMessage('Mínimo 3 caracteres');
    Edit1.SetFocus;
    AValid := False;
    Exit;
  end;
end;

// Código refactorizado
procedure TForm1.Button1Click(Sender: TObject);
var
  IsValid: Boolean;
begin
  ValidarEdit1(IsValid);
  if not IsValid then
    Exit;
  ProcessData(Edit1.Text);
end;
```

### Ejemplo 2: Extracción de Función con Retorno

**Contexto**: Cálculo complejo inline

**Input**:

```
source_code: "
procedure TForm1.CalcularTotal;
var
  Base, IVA, Total: Currency;
begin
  Base := StrToCurr(EditBase.Text);
  IVA := Base * 0.21;
  Total := Base + IVA;
  LabelTotal.Caption := CurrToStr(Total);
end;
"
selection_start: 5
selection_end: 7
new_method_name: "CalcularTotalConIVA"
```

**Output Esperado**:

```pascal
// Nuevo método
function TForm1.CalcularTotalConIVA(const ABase: Currency): Currency;
var
  IVA: Currency;
begin
  IVA := ABase * 0.21;
  Result := ABase + IVA;
end;

// Código refactorizado
procedure TForm1.CalcularTotal;
var
  Base, Total: Currency;
begin
  Base := StrToCurr(EditBase.Text);
  Total := CalcularTotalConIVA(Base);
  LabelTotal.Caption := CurrToStr(Total);
end;
```

## Manejo de Errores

### Error 1: Bloque incompleto

**Síntoma**: El bloque seleccionado no es sintácticamente válido
**Causa**: Selección corta un statement a la mitad
**Solución**: Ajustar selección para incluir statements completos

### Error 2: Múltiples puntos de salida

**Síntoma**: El bloque tiene varios Exit o Break
**Causa**: Flujo de control complejo
**Solución**: Reestructurar el código antes de extraer o extraer secciones más pequeñas

### Error 3: Nombre de método duplicado

**Síntoma**: Ya existe un método con ese nombre
**Causa**: Conflicto de nombres
**Solución**: Elegir un nombre diferente para el nuevo método

### Error 4: Variables globales modificadas

**Síntoma**: El bloque modifica estado global
**Causa**: Side effects no controlados
**Solución**: Considerar pasar el estado como parámetro var

## Optimizaciones

### Optimización de Tokens

- Analizar solo el bloque seleccionado y su contexto inmediato
- Generar código mínimo necesario

### Optimización de Calidad

- Ordenar parámetros de forma lógica
- Usar nombres descriptivos para parámetros
- Añadir documentación útil

## Dependencias

### Skills Requeridas

- Ninguna

### Herramientas Externas

- Ninguna (análisis estático)

## Variantes

### Variante 1: Inline Method

Operación inversa: tomar un método simple y expandir su código en los puntos de llamada

### Variante 2: Extract to New Unit

Extraer el método a una nueva unidad para reutilización

### Variante 3: Extract Interface

Extraer el método y crear una interface que lo defina

## Métricas de Éxito

- [ ] Nuevo método compila sin errores
- [ ] Comportamiento es idéntico al original
- [ ] Parámetros correctamente detectados
- [ ] Tipo de retorno correcto
- [ ] Documentación incluida

## Notas

- Esta refactorización es segura si se hace correctamente
- Siempre probar después de refactorizar
- Considerar el impacto en tests existentes
- Evitar extraer bloques demasiado pequeños (< 3 líneas)

## Referencias

- [Refactoring: Improving the Design of Existing Code - Martin Fowler](https://refactoring.com/)
- [Clean Code - Robert C. Martin](https://www.oreilly.com/library/view/clean-code-a/9780136083238/)

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
- Soporte para procedures y functions
- Detección automática de parámetros
- Generación de documentación XML

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
