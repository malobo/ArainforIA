---
name: generate-unit-test
version: 1.0.0
category: workflows/testing
tags: [test, dunit, unit, pruebas, tdd]
author: ARAINFORIA
created: 2026-01-08
complexity: 5
triggers:
  - "crear test"
  - "unit test"
  - "dunit"
  - "probar codigo"
---

# Generar Unit Test

## Descripción

Templates para crear tests unitarios en Delphi usando DUnit.

## Estructura Básica (DUnit)

```pascal
unit TestClientes;

interface

uses
  TestFramework, uClientes;

type
  TTestClientes = class(TTestCase)
  private
    FCliente: TCliente;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCrearCliente;
    procedure TestValidarNIF;
    procedure TestCalcularDescuento;
  end;

implementation

procedure TTestClientes.SetUp;
begin
  FCliente := TCliente.Create;
end;

procedure TTestClientes.TearDown;
begin
  FreeAndNil(FCliente);
end;

procedure TTestClientes.TestCrearCliente;
begin
  FCliente.Nombre := 'Test';
  FCliente.NIF := '12345678Z';
  CheckEquals('Test', FCliente.Nombre);
  CheckEquals('12345678Z', FCliente.NIF);
end;

procedure TTestClientes.TestValidarNIF;
begin
  CheckTrue(FCliente.ValidarNIF('12345678Z'), 'NIF válido');
  CheckFalse(FCliente.ValidarNIF('00000000A'), 'NIF inválido');
end;

procedure TTestClientes.TestCalcularDescuento;
begin
  FCliente.TipoCliente := tcVIP;
  CheckEquals(10.0, FCliente.DescuentoPorDefecto, 0.01);
end;

initialization
  RegisterTest(TTestClientes.Suite);

end.
```

## Métodos Assert Disponibles

```pascal
Check(Condition, 'Mensaje');           // Verificar condición
CheckEquals(Expected, Actual);         // Igualdad
CheckNotEquals(A, B);                  // Desigualdad
CheckTrue(Condition);                  // Verdadero
CheckFalse(Condition);                 // Falso
CheckNull(Obj);                        // Es nil
CheckNotNull(Obj);                     // No es nil
CheckIs(Obj, TClase);                  // Es instancia de
CheckException(Proc, EExceptionClass); // Lanza excepción
Fail('Mensaje');                       // Forzar fallo
```

## Test con Base de Datos Mock

```pascal
type
  TMockDatabase = class(TInterfacedObject, IDatabase)
  private
    FDatos: TDictionary<Integer, TCliente>;
  public
    constructor Create;
    destructor Destroy; override;
    function GetCliente(Id: Integer): TCliente;
    procedure SaveCliente(C: TCliente);
  end;

procedure TTestClienteService.SetUp;
begin
  FMockDB := TMockDatabase.Create;
  FService := TClienteService.Create(FMockDB);
end;
```

## Ejecutar Tests

```pascal
// En proyecto de consola
program TestRunner;

uses
  TestFramework,
  GUITestRunner,  // Para GUI
  TextTestRunner, // Para consola
  TestClientes in 'TestClientes.pas';

begin
  // GUI
  GUITestRunner.RunRegisteredTests;
  
  // O consola
  // TextTestRunner.RunRegisteredTests;
end.
```

---

**Estado**: stable
