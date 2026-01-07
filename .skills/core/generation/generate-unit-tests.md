---
name: generate-unit-tests
version: 1.0.0
category: core/generation
complexity: 5
tokens_estimate: 800-1200
tags: [testing, dunit, dunitx, quality, automation]
requires: []
dependencies: []
---

# 🧪 Generate Unit Tests

## Descripción

Genera tests unitarios automáticos para métodos y clases Delphi, utilizando frameworks DUnit o DUnitX.

## Cuándo Usar

- Al crear nuevos métodos que requieren testing
- Para aumentar cobertura de tests en código existente
- Antes de refactorizar código crítico
- Para validar comportamiento esperado

## Inputs

| Parámetro | Tipo | Requerido | Descripción |
| --------- | ---- | --------- | ----------- |
| `source_code` | string | ✅ | Código fuente del método/clase a testear |
| `target_unit` | string | ✅ | Nombre de la unidad objetivo |
| `framework` | string | ❌ | `dunit` o `dunitx` (default: `dunitx`) |
| `test_type` | string | ❌ | `unit`, `integration`, `both` (default: `unit`) |
| `include_edge_cases` | boolean | ❌ | Incluir casos límite (default: true) |
| `mock_dependencies` | boolean | ❌ | Generar mocks para dependencias (default: true) |

## Outputs

| Output | Tipo | Descripción |
| ------ | ---- | ----------- |
| `test_unit` | string | Código fuente de la unidad de tests |
| `test_cases` | array | Lista de casos de prueba generados |
| `coverage_estimate` | number | Estimación de cobertura (%) |
| `setup_instructions` | string | Instrucciones de configuración |

## Proceso de Ejecución

### Paso 1: Análisis del Código Fuente

```text
ANALIZAR código fuente:
├── Identificar clase/método objetivo
├── Extraer firma de métodos públicos
├── Identificar parámetros y tipos de retorno
├── Detectar dependencias externas
└── Mapear excepciones posibles
```

### Paso 2: Generación de Casos de Prueba

Para cada método público, generar:

1. **Test de Camino Feliz (Happy Path)**
   - Entrada válida → Resultado esperado

2. **Tests de Casos Límite (Edge Cases)**
   - Valores nulos/vacíos
   - Valores máximos/mínimos
   - Tipos incorrectos

3. **Tests de Excepciones**
   - Verificar que se lanzan las excepciones correctas

4. **Tests de Estado**
   - Verificar estado antes/después de operación

### Paso 3: Estructura de Test Unit

```pascal
unit Test{NombreUnidad};

interface

uses
  {$IFDEF DUNITX}
  DUnitX.TestFramework,
  {$ELSE}
  TestFramework,
  {$ENDIF}
  {UnidadObjetivo};

type
  {$IFDEF DUNITX}
  [TestFixture]
  {$ENDIF}
  TTest{NombreClase} = class({$IFDEF DUNITX}TObject{$ELSE}TTestCase{$ENDIF})
  private
    FSUT: T{NombreClase}; // System Under Test
  public
    {$IFDEF DUNITX}
    [Setup]
    {$ENDIF}
    procedure SetUp; {$IFNDEF DUNITX}override;{$ENDIF}
    
    {$IFDEF DUNITX}
    [TearDown]
    {$ENDIF}
    procedure TearDown; {$IFNDEF DUNITX}override;{$ENDIF}
  published
    // Tests generados aquí
  end;

implementation

{ TTest{NombreClase} }

procedure TTest{NombreClase}.SetUp;
begin
  FSUT := T{NombreClase}.Create;
end;

procedure TTest{NombreClase}.TearDown;
begin
  FSUT.Free;
end;

// Tests implementados aquí

{$IFNDEF DUNITX}
initialization
  RegisterTest(TTest{NombreClase}.Suite);
{$ENDIF}

end.
```

### Paso 4: Patrones de Test

#### Test de Método con Retorno

```pascal
{$IFDEF DUNITX}[Test]{$ENDIF}
procedure TTest{Clase}.Test{Metodo}_ConEntradaValida_RetornaResultadoEsperado;
var
  Resultado: TipoResultado;
begin
  // Arrange
  var Entrada := ValorEntrada;
  
  // Act
  Resultado := FSUT.{Metodo}(Entrada);
  
  // Assert
  {$IFDEF DUNITX}
  Assert.AreEqual(ValorEsperado, Resultado);
  {$ELSE}
  CheckEquals(ValorEsperado, Resultado);
  {$ENDIF}
end;
```

#### Test de Excepción

```pascal
{$IFDEF DUNITX}[Test]{$ENDIF}
procedure TTest{Clase}.Test{Metodo}_ConEntradaInvalida_LanzaExcepcion;
begin
  {$IFDEF DUNITX}
  Assert.WillRaise(
    procedure
    begin
      FSUT.{Metodo}(EntradaInvalida);
    end,
    E{TipoExcepcion});
  {$ELSE}
  try
    FSUT.{Metodo}(EntradaInvalida);
    Fail('Se esperaba excepción E{TipoExcepcion}');
  except
    on E: E{TipoExcepcion} do
      ; // OK, excepción esperada
  end;
  {$ENDIF}
end;
```

#### Test de Valor Nulo

```pascal
{$IFDEF DUNITX}[Test]{$ENDIF}
procedure TTest{Clase}.Test{Metodo}_ConParametroNil_LanzaEAccessViolation;
begin
  {$IFDEF DUNITX}
  Assert.WillRaise(
    procedure
    begin
      FSUT.{Metodo}(nil);
    end,
    EAccessViolation);
  {$ELSE}
  try
    FSUT.{Metodo}(nil);
    Fail('Se esperaba EAccessViolation');
  except
    on E: EAccessViolation do
      ; // OK
  end;
  {$ENDIF}
end;
```

## Ejemplo de Uso

### Input

```yaml
@skill:core/generation/generate-unit-tests
source_code: |
  function TCalculadora.Sumar(A, B: Integer): Integer;
  begin
    Result := A + B;
  end;
  
  function TCalculadora.Dividir(A, B: Integer): Double;
  begin
    if B = 0 then
      raise EDivByZero.Create('División por cero');
    Result := A / B;
  end;
target_unit: "uCalculadora"
framework: "dunitx"
include_edge_cases: true
```

### Output

```pascal
unit TestuCalculadora;

interface

uses
  DUnitX.TestFramework,
  uCalculadora;

type
  [TestFixture]
  TTestCalculadora = class
  private
    FSUT: TCalculadora;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;
  published
    // Tests para Sumar
    [Test]
    procedure TestSumar_NumerosPositivos_RetornaSuma;
    [Test]
    procedure TestSumar_NumerosNegativos_RetornaSuma;
    [Test]
    procedure TestSumar_Ceros_RetornaCero;
    [Test]
    procedure TestSumar_MaxInt_NoOverflow;
    
    // Tests para Dividir
    [Test]
    procedure TestDividir_NumerosPositivos_RetornaCociente;
    [Test]
    procedure TestDividir_DivisorCero_LanzaEDivByZero;
    [Test]
    procedure TestDividir_DividendoCero_RetornaCero;
  end;

implementation

procedure TTestCalculadora.SetUp;
begin
  FSUT := TCalculadora.Create;
end;

procedure TTestCalculadora.TearDown;
begin
  FSUT.Free;
end;

procedure TTestCalculadora.TestSumar_NumerosPositivos_RetornaSuma;
begin
  Assert.AreEqual(5, FSUT.Sumar(2, 3));
end;

procedure TTestCalculadora.TestSumar_NumerosNegativos_RetornaSuma;
begin
  Assert.AreEqual(-5, FSUT.Sumar(-2, -3));
end;

procedure TTestCalculadora.TestSumar_Ceros_RetornaCero;
begin
  Assert.AreEqual(0, FSUT.Sumar(0, 0));
end;

procedure TTestCalculadora.TestSumar_MaxInt_NoOverflow;
begin
  Assert.AreEqual(MaxInt, FSUT.Sumar(MaxInt - 1, 1));
end;

procedure TTestCalculadora.TestDividir_NumerosPositivos_RetornaCociente;
begin
  Assert.AreEqual(2.5, FSUT.Dividir(5, 2), 0.001);
end;

procedure TTestCalculadora.TestDividir_DivisorCero_LanzaEDivByZero;
begin
  Assert.WillRaise(
    procedure
    begin
      FSUT.Dividir(10, 0);
    end,
    EDivByZero);
end;

procedure TTestCalculadora.TestDividir_DividendoCero_RetornaCero;
begin
  Assert.AreEqual(0.0, FSUT.Dividir(0, 5), 0.001);
end;

initialization
  TDUnitX.RegisterTestFixture(TTestCalculadora);

end.
```

## Convenciones de Nombrado

```text
Test{Metodo}_{Escenario}_{ResultadoEsperado}

Ejemplos:
- TestSumar_NumerosPositivos_RetornaSuma
- TestDividir_DivisorCero_LanzaExcepcion
- TestGuardar_DatosValidos_RetornaTrue
- TestBuscar_ClienteNoExiste_RetornaNil
```

## Mejores Prácticas

1. **AAA Pattern**: Arrange, Act, Assert
2. **Un assert por test** (cuando sea posible)
3. **Tests independientes**: No depender unos de otros
4. **Nombres descriptivos**: Que expliquen el escenario
5. **Setup/TearDown**: Limpiar estado entre tests

## Historial de Cambios

| Versión | Fecha | Cambios |
| ------- | ----- | ------- |
| 1.0.0 | 2026-01-07 | Versión inicial |
