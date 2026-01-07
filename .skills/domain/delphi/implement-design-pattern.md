---
name: implement-design-pattern
version: 1.0.0
category: domain/delphi
complexity: 5
tokens_estimate: 900-1400
tags: [patterns, design, architecture, solid, clean-code]
requires: []
dependencies: []
---

# 🏗️ Implement Design Pattern

## Descripción

Implementa patrones de diseño clásicos (GoF) en Delphi, adaptados a las características del lenguaje y mejores prácticas modernas.

## Cuándo Usar

- Al diseñar arquitectura de nuevos módulos
- Para resolver problemas de diseño recurrentes
- Al refactorizar código con alto acoplamiento
- Para mejorar testabilidad y mantenibilidad

## Patrones Soportados

### Creacionales

| Patrón | Uso Principal |
| ------ | ------------- |
| Singleton | Instancia única global |
| Factory Method | Creación delegada a subclases |
| Abstract Factory | Familias de objetos relacionados |
| Builder | Construcción paso a paso |
| Prototype | Clonación de objetos |

### Estructurales

| Patrón | Uso Principal |
| ------ | ------------- |
| Adapter | Compatibilidad entre interfaces |
| Decorator | Añadir funcionalidad dinámicamente |
| Facade | Simplificar interfaz compleja |
| Proxy | Control de acceso a objeto |
| Composite | Estructuras jerárquicas |

### De Comportamiento

| Patrón | Uso Principal |
| ------ | ------------- |
| Observer | Notificación de cambios |
| Strategy | Algoritmos intercambiables |
| Command | Encapsular operaciones |
| State | Comportamiento según estado |
| Template Method | Esqueleto de algoritmo |

## Inputs

| Parámetro | Tipo | Requerido | Descripción |
| --------- | ---- | --------- | ----------- |
| `pattern` | string | ✅ | Nombre del patrón a implementar |
| `context` | string | ✅ | Descripción del problema a resolver |
| `class_names` | object | ❌ | Nombres personalizados para las clases |
| `include_interface` | boolean | ❌ | Incluir interfaces (default: true) |
| `thread_safe` | boolean | ❌ | Hacer thread-safe si aplica (default: false) |

## Outputs

| Output | Tipo | Descripción |
| ------ | ---- | ----------- |
| `implementation` | string | Código fuente completo |
| `usage_example` | string | Ejemplo de uso |
| `unit_test` | string | Test unitario sugerido |
| `diagram` | string | Diagrama UML en formato Mermaid |

## Implementaciones de Patrones

### 1. Singleton (Thread-Safe)

```pascal
unit uSingleton;

interface

type
  TSingleton = class
  private
    class var FInstance: TSingleton;
    class var FLock: TObject;
    constructor CreatePrivate;
  public
    class function GetInstance: TSingleton;
    class procedure ReleaseInstance;
    class constructor Create;
    class destructor Destroy;
    
    // Métodos de negocio aquí
    procedure DoSomething;
  end;

implementation

uses
  System.SyncObjs;

class constructor TSingleton.Create;
begin
  FLock := TObject.Create;
  FInstance := nil;
end;

class destructor TSingleton.Destroy;
begin
  FInstance.Free;
  FLock.Free;
end;

constructor TSingleton.CreatePrivate;
begin
  inherited Create;
  // Inicialización aquí
end;

class function TSingleton.GetInstance: TSingleton;
begin
  if not Assigned(FInstance) then
  begin
    TMonitor.Enter(FLock);
    try
      if not Assigned(FInstance) then
        FInstance := TSingleton.CreatePrivate;
    finally
      TMonitor.Exit(FLock);
    end;
  end;
  Result := FInstance;
end;

class procedure TSingleton.ReleaseInstance;
begin
  TMonitor.Enter(FLock);
  try
    FreeAndNil(FInstance);
  finally
    TMonitor.Exit(FLock);
  end;
end;

procedure TSingleton.DoSomething;
begin
  // Implementación
end;

end.
```

### 2. Factory Method

```pascal
unit uFactory;

interface

type
  // Producto abstracto
  IProducto = interface
    ['{GUID}']
    procedure Operar;
  end;

  // Productos concretos
  TProductoA = class(TInterfacedObject, IProducto)
  public
    procedure Operar;
  end;

  TProductoB = class(TInterfacedObject, IProducto)
  public
    procedure Operar;
  end;

  // Creador abstracto
  TCreador = class abstract
  public
    function FactoryMethod: IProducto; virtual; abstract;
    procedure AlgunaOperacion;
  end;

  // Creadores concretos
  TCreadorA = class(TCreador)
  public
    function FactoryMethod: IProducto; override;
  end;

  TCreadorB = class(TCreador)
  public
    function FactoryMethod: IProducto; override;
  end;

implementation

{ TProductoA }
procedure TProductoA.Operar;
begin
  // Operación específica de A
end;

{ TProductoB }
procedure TProductoB.Operar;
begin
  // Operación específica de B
end;

{ TCreador }
procedure TCreador.AlgunaOperacion;
var
  Producto: IProducto;
begin
  Producto := FactoryMethod;
  Producto.Operar;
end;

{ TCreadorA }
function TCreadorA.FactoryMethod: IProducto;
begin
  Result := TProductoA.Create;
end;

{ TCreadorB }
function TCreadorB.FactoryMethod: IProducto;
begin
  Result := TProductoB.Create;
end;

end.
```

### 3. Observer

```pascal
unit uObserver;

interface

uses
  System.Generics.Collections;

type
  IObserver = interface
    ['{GUID}']
    procedure Update(const AData: string);
  end;

  ISubject = interface
    ['{GUID}']
    procedure Attach(AObserver: IObserver);
    procedure Detach(AObserver: IObserver);
    procedure Notify;
  end;

  TSubject = class(TInterfacedObject, ISubject)
  private
    FObservers: TList<IObserver>;
    FState: string;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Attach(AObserver: IObserver);
    procedure Detach(AObserver: IObserver);
    procedure Notify;
    
    property State: string read FState write FState;
  end;

  TConcreteObserver = class(TInterfacedObject, IObserver)
  private
    FName: string;
  public
    constructor Create(const AName: string);
    procedure Update(const AData: string);
  end;

implementation

{ TSubject }
constructor TSubject.Create;
begin
  inherited;
  FObservers := TList<IObserver>.Create;
end;

destructor TSubject.Destroy;
begin
  FObservers.Free;
  inherited;
end;

procedure TSubject.Attach(AObserver: IObserver);
begin
  if not FObservers.Contains(AObserver) then
    FObservers.Add(AObserver);
end;

procedure TSubject.Detach(AObserver: IObserver);
begin
  FObservers.Remove(AObserver);
end;

procedure TSubject.Notify;
var
  Observer: IObserver;
begin
  for Observer in FObservers do
    Observer.Update(FState);
end;

{ TConcreteObserver }
constructor TConcreteObserver.Create(const AName: string);
begin
  inherited Create;
  FName := AName;
end;

procedure TConcreteObserver.Update(const AData: string);
begin
  WriteLn(Format('%s recibió: %s', [FName, AData]));
end;

end.
```

### 4. Strategy

```pascal
unit uStrategy;

interface

type
  // Estrategia abstracta
  IEstrategia = interface
    ['{GUID}']
    function Ejecutar(A, B: Integer): Integer;
  end;

  // Estrategias concretas
  TEstrategiaSuma = class(TInterfacedObject, IEstrategia)
  public
    function Ejecutar(A, B: Integer): Integer;
  end;

  TEstrategiaResta = class(TInterfacedObject, IEstrategia)
  public
    function Ejecutar(A, B: Integer): Integer;
  end;

  TEstrategiaMultiplica = class(TInterfacedObject, IEstrategia)
  public
    function Ejecutar(A, B: Integer): Integer;
  end;

  // Contexto
  TContexto = class
  private
    FEstrategia: IEstrategia;
  public
    procedure SetEstrategia(AEstrategia: IEstrategia);
    function EjecutarEstrategia(A, B: Integer): Integer;
  end;

implementation

{ TEstrategiaSuma }
function TEstrategiaSuma.Ejecutar(A, B: Integer): Integer;
begin
  Result := A + B;
end;

{ TEstrategiaResta }
function TEstrategiaResta.Ejecutar(A, B: Integer): Integer;
begin
  Result := A - B;
end;

{ TEstrategiaMultiplica }
function TEstrategiaMultiplica.Ejecutar(A, B: Integer): Integer;
begin
  Result := A * B;
end;

{ TContexto }
procedure TContexto.SetEstrategia(AEstrategia: IEstrategia);
begin
  FEstrategia := AEstrategia;
end;

function TContexto.EjecutarEstrategia(A, B: Integer): Integer;
begin
  if Assigned(FEstrategia) then
    Result := FEstrategia.Ejecutar(A, B)
  else
    raise Exception.Create('No hay estrategia definida');
end;

end.
```

### 5. Builder

```pascal
unit uBuilder;

interface

type
  TProducto = class
  private
    FParteA: string;
    FParteB: string;
    FParteC: string;
  public
    property ParteA: string read FParteA write FParteA;
    property ParteB: string read FParteB write FParteB;
    property ParteC: string read FParteC write FParteC;
    function ToString: string; override;
  end;

  IBuilder = interface
    ['{GUID}']
    function Reset: IBuilder;
    function BuildParteA: IBuilder;
    function BuildParteB: IBuilder;
    function BuildParteC: IBuilder;
    function GetProducto: TProducto;
  end;

  TConcreteBuilder = class(TInterfacedObject, IBuilder)
  private
    FProducto: TProducto;
  public
    constructor Create;
    function Reset: IBuilder;
    function BuildParteA: IBuilder;
    function BuildParteB: IBuilder;
    function BuildParteC: IBuilder;
    function GetProducto: TProducto;
  end;

  // Director opcional
  TDirector = class
  public
    class function ConstruirMinimo(ABuilder: IBuilder): TProducto;
    class function ConstruirCompleto(ABuilder: IBuilder): TProducto;
  end;

implementation

{ TProducto }
function TProducto.ToString: string;
begin
  Result := Format('A: %s, B: %s, C: %s', [FParteA, FParteB, FParteC]);
end;

{ TConcreteBuilder }
constructor TConcreteBuilder.Create;
begin
  inherited;
  Reset;
end;

function TConcreteBuilder.Reset: IBuilder;
begin
  FProducto := TProducto.Create;
  Result := Self;
end;

function TConcreteBuilder.BuildParteA: IBuilder;
begin
  FProducto.ParteA := 'Parte A construida';
  Result := Self;
end;

function TConcreteBuilder.BuildParteB: IBuilder;
begin
  FProducto.ParteB := 'Parte B construida';
  Result := Self;
end;

function TConcreteBuilder.BuildParteC: IBuilder;
begin
  FProducto.ParteC := 'Parte C construida';
  Result := Self;
end;

function TConcreteBuilder.GetProducto: TProducto;
begin
  Result := FProducto;
  FProducto := nil; // Transferir ownership
end;

{ TDirector }
class function TDirector.ConstruirMinimo(ABuilder: IBuilder): TProducto;
begin
  Result := ABuilder.Reset.BuildParteA.GetProducto;
end;

class function TDirector.ConstruirCompleto(ABuilder: IBuilder): TProducto;
begin
  Result := ABuilder.Reset.BuildParteA.BuildParteB.BuildParteC.GetProducto;
end;

end.
```

## Ejemplo de Uso

```yaml
@skill:domain/delphi/implement-design-pattern
pattern: "observer"
context: "Necesito notificar cambios en el stock de productos a múltiples pantallas"
class_names:
  subject: "TInventario"
  observer: "IPantallaStock"
thread_safe: true
```

## Diagrama de Selección

```text
¿Qué problema tienes?
│
├─ Necesito UNA sola instancia
│  └─ SINGLETON
│
├─ Necesito crear objetos sin especificar clase exacta
│  ├─ Una familia de productos → ABSTRACT FACTORY
│  ├─ Un producto, delegando a subclases → FACTORY METHOD
│  └─ Objeto complejo paso a paso → BUILDER
│
├─ Necesito adaptar/extender estructura
│  ├─ Convertir interfaz incompatible → ADAPTER
│  ├─ Añadir comportamiento dinámico → DECORATOR
│  └─ Simplificar subsistema complejo → FACADE
│
└─ Necesito variar comportamiento
   ├─ Algoritmos intercambiables → STRATEGY
   ├─ Notificar cambios a varios objetos → OBSERVER
   ├─ Encapsular solicitudes como objetos → COMMAND
   └─ Comportamiento según estado → STATE
```

## Historial de Cambios

| Versión | Fecha | Cambios |
| ------- | ----- | ------- |
| 1.0.0 | 2026-01-07 | Versión inicial con 10 patrones |
