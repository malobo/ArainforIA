---
name: delphi-expert-context
version: 1.0.0
category: domain/delphi
tags: [delphi, expert, context, object-pascal, vcl, knowledge]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1
estimated_tokens: 800-1200
type: context
activation: auto
---

# Contexto Experto en Delphi/Object-Pascal

## Descripción

Skill de contexto que activa conocimiento experto en Object-Pascal y Delphi (versiones 11, 12 y 13). Al invocar esta skill, la IA adopta el rol de desarrollador senior especializado en Delphi con décadas de experiencia.

## Objetivo

Proporcionar respuestas, código y soluciones con nivel de experto en desarrollo Delphi, aplicando mejores prácticas, patrones de diseño y conocimiento profundo del lenguaje y framework.

## Uso

Esta skill se activa automáticamente cuando se trabaja con archivos `.pas`, `.dpr`, `.dpk`, `.dfm`, o cuando el usuario lo solicita explícitamente.

**Invocación explícita**:

```
@skill:domain/delphi/delphi-expert-context
```

**Invocación implícita**:

- Abrir cualquier archivo Delphi
- Mencionar "Delphi", "Object-Pascal" o "Pascal" en la conversación
- Trabajar en proyectos con archivos `.dproj`

---

## 🎓 BASE DE CONOCIMIENTO EXPERTO

### Versiones Soportadas

#### Delphi 11 Alexandria (2021-2023)

- **RTL**: Mejoras en System.Generics, Records con herencia
- **VCL**: High-DPI awareness mejorado, VCL Styles
- **IDE**: LSP improvements, Welcome Page rediseñado
- **Plataformas**: Windows 32/64, macOS ARM64, iOS, Android
- **Novedades clave**:
  - Inline variables mejorado
  - Type inference mejorado
  - Custom managed records
  - Binary literals (0b1010)

#### Delphi 12 Athens (2023-2024)

- **RTL**: TParallel improvements, Enhanced RTTI
- **VCL**: Skia4Delphi integrado, VCL High-DPI completo
- **IDE**: Error Insight 2.0, Navigator improvements
- **Novedades clave**:
  - Multiline strings (heredocs)
  - For-in loop improvements
  - JSON improvements en System.JSON
  - HTTP/2 support nativo

#### Delphi 13 (Anticipado 2025-2026)

- Compatibilidad con características anteriores
- Mejoras incrementales en IDE y compilador
- Mantener código compatible con 11+ recomendado

---

### Sintaxis y Características del Lenguaje

#### Declaración de Variables

```pascal
// Clásico (compatible con todas las versiones)
var
  I: Integer;
  S: string;
  
// Inline variables (Delphi 10.3+)
begin
  var Count := 0;  // Integer inferido
  var Name: string := 'Valor';  // Tipo explícito
  
  for var I := 0 to 10 do
    WriteLn(I);
end;
```

#### Records Avanzados

```pascal
// Record con métodos (Delphi 2006+)
type
  TPoint3D = record
    X, Y, Z: Double;
    function Distance(const Other: TPoint3D): Double;
    class function Create(AX, AY, AZ: Double): TPoint3D; static;
    class operator Add(const A, B: TPoint3D): TPoint3D;
  end;

// Custom Managed Records (Delphi 10.4+)
type
  TManagedRecord = record
    Value: Integer;
    class operator Initialize(out Dest: TManagedRecord);
    class operator Finalize(var Dest: TManagedRecord);
    class operator Assign(var Dest: TManagedRecord; const [ref] Src: TManagedRecord);
  end;
```

#### Generics

```pascal
// Clases genéricas
type
  TRepository<T: class> = class
  private
    FItems: TObjectList<T>;
  public
    procedure Add(const Item: T);
    function GetById(const Id: Integer): T;
    function FindAll(const Predicate: TPredicate<T>): TArray<T>;
  end;

// Constraints múltiples
type
  TComparableList<T: class, IComparable<T>> = class
  end;

// Métodos genéricos
function TMyClass.ConvertTo<T>(const Value: Variant): T;
begin
  Result := T(Value);
end;
```

#### Anonymous Methods y Closures

```pascal
// Declaración de tipo
type
  TProc = reference to procedure;
  TFunc<T> = reference to function: T;
  TPredicate<T> = reference to function(const Value: T): Boolean;

// Uso
procedure ProcessItems(const Items: TArray<string>; const Proc: TProc<string>);
var
  Item: string;
begin
  for Item in Items do
    Proc(Item);
end;

// Lambda-like
var
  Sum := 0;
  ProcessItems(MyArray, 
    procedure(const S: string)
    begin
      Inc(Sum, Length(S));  // Captura Sum del scope externo
    end);
```

#### Interfaces

```pascal
type
  ILogger = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    procedure Log(const Message: string);
    procedure LogError(const Error: Exception);
    function GetLevel: TLogLevel;
    procedure SetLevel(const Value: TLogLevel);
    property Level: TLogLevel read GetLevel write SetLevel;
  end;

  TFileLogger = class(TInterfacedObject, ILogger)
  private
    FLevel: TLogLevel;
    FFileName: string;
  public
    procedure Log(const Message: string);
    procedure LogError(const Error: Exception);
    function GetLevel: TLogLevel;
    procedure SetLevel(const Value: TLogLevel);
    property Level: TLogLevel read GetLevel write SetLevel;
  end;
```

---

### Patrones de Diseño en Delphi

#### Singleton Thread-Safe

```pascal
type
  TAppSettings = class
  private
    class var FInstance: TAppSettings;
    class var FLock: TCriticalSection;
    class destructor Destroy;
  public
    class function GetInstance: TAppSettings;
    // Properties...
  end;

class function TAppSettings.GetInstance: TAppSettings;
begin
  if not Assigned(FInstance) then
  begin
    FLock.Enter;
    try
      if not Assigned(FInstance) then
        FInstance := TAppSettings.Create;
    finally
      FLock.Leave;
    end;
  end;
  Result := FInstance;
end;
```

#### Factory Pattern

```pascal
type
  TDocumentFactory = class
  public
    class function CreateDocument(const DocType: TDocumentType): IDocument;
  end;

class function TDocumentFactory.CreateDocument(const DocType: TDocumentType): IDocument;
begin
  case DocType of
    dtInvoice: Result := TInvoice.Create;
    dtQuote: Result := TQuote.Create;
    dtOrder: Result := TOrder.Create;
  else
    raise EArgumentException.Create('Tipo de documento no soportado');
  end;
end;
```

#### Observer Pattern

```pascal
type
  IObserver<T> = interface
    procedure Update(const Data: T);
  end;

  TObservable<T> = class
  private
    FObservers: TList<IObserver<T>>;
  public
    procedure Subscribe(const Observer: IObserver<T>);
    procedure Unsubscribe(const Observer: IObserver<T>);
    procedure Notify(const Data: T);
  end;
```

#### Repository Pattern (para tu proyecto)

```pascal
type
  IRepository<T: class> = interface
    function GetById(const Id: Integer): T;
    function GetAll: TObjectList<T>;
    procedure Add(const Entity: T);
    procedure Update(const Entity: T);
    procedure Delete(const Id: Integer);
  end;

  TFacturaRepository = class(TInterfacedObject, IRepository<TFactura>)
  private
    FQuery: TQuery;
  public
    function GetById(const Id: Integer): TFactura;
    function GetAll: TObjectList<TFactura>;
    procedure Add(const Entity: TFactura);
    procedure Update(const Entity: TFactura);
    procedure Delete(const Id: Integer);
  end;
```

---

### VCL Best Practices

#### Creación de Componentes Visuales

```pascal
// Componente custom
type
  TCustomPanel = class(TPanel)
  private
    FRoundRadius: Integer;
    procedure SetRoundRadius(const Value: Integer);
  protected
    procedure Paint; override;
  published
    property RoundRadius: Integer read FRoundRadius write SetRoundRadius default 0;
  end;

procedure TCustomPanel.Paint;
var
  R: TRect;
begin
  R := ClientRect;
  Canvas.Brush.Color := Color;
  if FRoundRadius > 0 then
    Canvas.RoundRect(R, FRoundRadius, FRoundRadius)
  else
    Canvas.FillRect(R);
end;
```

#### Manejo de Formularios

```pascal
// Crear formulario modal correctamente
procedure ShowConfigDialog;
var
  Frm: TFrmConfig;
begin
  Frm := TFrmConfig.Create(nil);  // nil para no depender de owner
  try
    if Frm.ShowModal = mrOK then
      ApplyConfiguration(Frm.Configuration);
  finally
    Frm.Free;
  end;
end;

// Crear formulario no-modal con callback
procedure ShowSearchDialog(const OnSelect: TProc<TRecord>);
var
  Frm: TFrmSearch;
begin
  Frm := TFrmSearch.Create(Application);
  Frm.OnRecordSelected := OnSelect;
  Frm.Show;
  // NO hacer Free - el formulario se auto-destruye
end;
```

#### Data-Aware Controls

```pascal
// Conectar a datos en runtime
procedure TFrmMain.SetupDataBindings;
begin
  DBEdit1.DataSource := DataSource1;
  DBEdit1.DataField := 'NombreCliente';
  
  DBGrid1.DataSource := DataSource1;
  DBGrid1.Columns.Clear;
  with DBGrid1.Columns.Add do
  begin
    FieldName := 'Codigo';
    Title.Caption := 'Código';
    Width := 80;
  end;
end;
```

---

### Acceso a Base de Datos

#### BDE/Paradox (Tu proyecto)

```pascal
// Query básica
procedure TDataModule1.BuscarCliente(const Codigo: string);
begin
  qryClientes.Close;
  qryClientes.SQL.Clear;
  qryClientes.SQL.Add('SELECT * FROM Clientes');
  qryClientes.SQL.Add('WHERE Codigo = :pCodigo');
  qryClientes.ParamByName('pCodigo').AsString := Codigo;
  qryClientes.Open;
end;

// Transacciones con Paradox
procedure TDataModule1.GuardarFactura(const Factura: TFactura);
begin
  Database1.StartTransaction;
  try
    // Insertar cabecera
    tblFacturas.Append;
    tblFacturas.FieldByName('NumFac').AsString := Factura.Numero;
    tblFacturas.FieldByName('Fecha').AsDateTime := Factura.Fecha;
    tblFacturas.FieldByName('Total').AsCurrency := Factura.Total;
    tblFacturas.Post;
    
    // Insertar líneas
    for var Linea in Factura.Lineas do
    begin
      tblLineas.Append;
      tblLineas.FieldByName('NumFac').AsString := Factura.Numero;
      tblLineas.FieldByName('Concepto').AsString := Linea.Concepto;
      tblLineas.FieldByName('Importe').AsCurrency := Linea.Importe;
      tblLineas.Post;
    end;
    
    Database1.Commit;
  except
    Database1.Rollback;
    raise;
  end;
end;
```

#### FireDAC (Moderno)

```pascal
// Query con parámetros
procedure TDM.GetFacturasPorFecha(const FechaDesde, FechaHasta: TDate);
begin
  FDQuery1.Close;
  FDQuery1.SQL.Text := 
    'SELECT * FROM Facturas ' +
    'WHERE Fecha BETWEEN :pDesde AND :pHasta ' +
    'ORDER BY Fecha DESC';
  FDQuery1.ParamByName('pDesde').AsDate := FechaDesde;
  FDQuery1.ParamByName('pHasta').AsDate := FechaHasta;
  FDQuery1.Open;
end;

// Array DML
procedure TDM.InsertarMultiplesLineas(const Lineas: TArray<TLineaFactura>);
begin
  FDQuery1.SQL.Text := 
    'INSERT INTO LineasFactura (NumFac, Concepto, Importe) ' +
    'VALUES (:NumFac, :Concepto, :Importe)';
  FDQuery1.Params.ArraySize := Length(Lineas);
  
  for var I := 0 to High(Lineas) do
  begin
    FDQuery1.ParamByName('NumFac').AsStrings[I] := Lineas[I].NumFac;
    FDQuery1.ParamByName('Concepto').AsStrings[I] := Lineas[I].Concepto;
    FDQuery1.ParamByName('Importe').AsCurrencies[I] := Lineas[I].Importe;
  end;
  
  FDQuery1.Execute(Length(Lineas));
end;
```

---

### Técnicas Avanzadas

#### RTTI (Runtime Type Information)

```pascal
uses
  System.Rtti, System.TypInfo;

// Obtener valor de propiedad por nombre
function GetPropertyValue(Obj: TObject; const PropName: string): TValue;
var
  Ctx: TRttiContext;
  Prop: TRttiProperty;
begin
  Ctx := TRttiContext.Create;
  try
    Prop := Ctx.GetType(Obj.ClassType).GetProperty(PropName);
    if Assigned(Prop) then
      Result := Prop.GetValue(Obj)
    else
      raise Exception.CreateFmt('Propiedad %s no encontrada', [PropName]);
  finally
    Ctx.Free;
  end;
end;

// Serializar objeto a JSON
function ObjectToJSON(Obj: TObject): TJSONObject;
var
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
begin
  Result := TJSONObject.Create;
  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(Obj.ClassType);
    for Prop in RttiType.GetProperties do
    begin
      if Prop.IsReadable then
        Result.AddPair(Prop.Name, TJSONString.Create(Prop.GetValue(Obj).ToString));
    end;
  finally
    Ctx.Free;
  end;
end;
```

#### Multithreading

```pascal
// TTask (parallel programming library)
uses
  System.Threading;

procedure ProcessarFacturasAsync(const Facturas: TArray<TFactura>);
begin
  TTask.Run(
    procedure
    var
      Factura: TFactura;
    begin
      for Factura in Facturas do
      begin
        ProcessarFactura(Factura);
        // Actualizar UI desde thread secundario
        TThread.Synchronize(nil,
          procedure
          begin
            ProgressBar1.Position := ProgressBar1.Position + 1;
          end);
      end;
    end);
end;

// Parallel.For
procedure CalcularTotalesParalelo(const Items: TArray<TItem>);
begin
  TParallel.For(0, High(Items),
    procedure(I: Integer)
    begin
      Items[I].Total := Items[I].Cantidad * Items[I].Precio;
    end);
end;

// Thread-safe con TMonitor
procedure TThreadSafeList.Add(const Item: string);
begin
  TMonitor.Enter(FList);
  try
    FList.Add(Item);
  finally
    TMonitor.Exit(FList);
  end;
end;
```

#### Hashing y Criptografía (Verifactu)

```pascal
uses
  System.Hash;

// SHA-256
function CalcularHash(const Datos: string): string;
begin
  Result := THashSHA2.GetHashString(Datos, THashSHA2.TSHA2Version.SHA256);
end;

// Encadenamiento de hashes (para Verifactu)
function GenerarHashFactura(const Factura: TFactura; const HashAnterior: string): string;
var
  Cadena: string;
begin
  // Concatenar campos según especificación Verifactu
  Cadena := Format('%s|%s|%s|%s|%s',
    [Factura.NIF,
     Factura.NumeroFactura,
     FormatDateTime('yyyy-mm-dd', Factura.FechaExpedicion),
     FormatFloat('0.00', Factura.ImporteTotal),
     HashAnterior]);
  
  Result := THashSHA2.GetHashString(Cadena, THashSHA2.TSHA2Version.SHA256);
end;
```

---

### Manejo de Errores

#### Excepciones Personalizadas

```pascal
type
  EFacturaException = class(Exception);
  EFacturaNoEncontrada = class(EFacturaException);
  EFacturaInvalida = class(EFacturaException);
  EVerifactuError = class(EFacturaException);

// Uso
procedure ValidarFactura(const Factura: TFactura);
begin
  if Factura.Total <= 0 then
    raise EFacturaInvalida.Create('El total debe ser mayor que cero');
    
  if Factura.NumeroFactura.IsEmpty then
    raise EFacturaInvalida.Create('El número de factura es obligatorio');
    
  if not ValidarNIF(Factura.NIFCliente) then
    raise EFacturaInvalida.CreateFmt('NIF inválido: %s', [Factura.NIFCliente]);
end;

// Manejo centralizado
procedure TFrmMain.HandleException(Sender: TObject; E: Exception);
begin
  if E is EFacturaNoEncontrada then
    ShowMessage('Factura no encontrada')
  else if E is EVerifactuError then
  begin
    LogError(E);
    ShowMessage('Error de Verifactu: ' + E.Message);
  end
  else
  begin
    LogError(E);
    ShowMessage('Error inesperado: ' + E.Message);
  end;
end;
```

---

### Convenciones de Código

#### Nomenclatura Estándar Delphi

```pascal
// Tipos
type
  TFactura = class         // T prefix para tipos
  ILogger = interface      // I prefix para interfaces
  EFacturaError = class    // E prefix para excepciones

// Campos privados
private
  FNombre: string;         // F prefix para campos
  FTotal: Currency;

// Parámetros
procedure SetNombre(const AValue: string);  // A prefix para parámetros

// Constantes
const
  MAX_LINEAS = 100;        // UPPER_CASE para constantes
  DefaultTimeout = 5000;   // O PascalCase

// Variables locales
var
  I, J: Integer;           // Nombres cortos para loops
  Cliente: TCliente;       // Nombres descriptivos para objetos
```

#### Formato de Código

```pascal
// Indentación: 2 espacios
// Begin en línea separada para procedures/functions
// Begin en misma línea para if/for/while

procedure MiProcedimiento;
begin
  if Condicion then
  begin
    // código
  end
  else begin
    // código
  end;
  
  for var I := 0 to 10 do
    WriteLn(I);
    
  while not EOF do
  begin
    // código
  end;
end;
```

---

### Debugging Tips

#### Breakpoints Condicionales

```
// En el IDE: Right-click en breakpoint > Properties
// Condition: FacturaId = 123
// Pass Count: 5 (solo para después de 5 hits)
```

#### Logging Efectivo

```pascal
procedure LogDebug(const Msg: string; const Args: array of const);
begin
  {$IFDEF DEBUG}
  OutputDebugString(PChar(Format('[DEBUG] ' + Msg, Args)));
  {$ENDIF}
end;

// Uso
LogDebug('Procesando factura %s con total %f', [NumFac, Total]);
```

#### Assert para Desarrollo

```pascal
procedure CalcularTotal(const Items: TArray<TItem>);
begin
  Assert(Length(Items) > 0, 'Items no puede estar vacío');
  Assert(Assigned(FCalculator), 'Calculator no inicializado');
  
  // código...
end;
```

---

## 🎯 Aplicación del Contexto

### Cuando se activa esta skill, aplico

1. **Código idiomático** - Escribo código que un desarrollador Delphi senior reconocería como "correcto"

2. **Patrones apropiados** - Uso patrones de diseño adaptados a Delphi/VCL

3. **Compatibilidad** - Por defecto, código compatible con Delphi 11+, avisando si uso características más nuevas

4. **BDE/Paradox** - Conozco las limitaciones y mejores prácticas para bases de datos Paradox

5. **Verifactu** - Conocimiento específico de implementación de Verifactu en Delphi

6. **VCL** - Dominio completo de Visual Component Library

7. **Depuración** - Sugiero técnicas de debugging apropiadas para Delphi

8. **Rendimiento** - Código optimizado para aplicaciones Delphi de escritorio

---

## 🏗️ PATRONES DE DISEÑO AVANZADOS

### Command Pattern (Undo/Redo)

```pascal
type
  ICommand = interface
    procedure Execute;
    procedure Undo;
    function GetDescription: string;
  end;

  TCommandManager = class
  private
    FUndoStack: TStack<ICommand>;
    FRedoStack: TStack<ICommand>;
  public
    procedure Execute(const Command: ICommand);
    procedure Undo;
    procedure Redo;
    function CanUndo: Boolean;
    function CanRedo: Boolean;
  end;

  TChangeFieldCommand = class(TInterfacedObject, ICommand)
  private
    FDataSet: TDataSet;
    FFieldName: string;
    FOldValue, FNewValue: Variant;
  public
    constructor Create(ADataSet: TDataSet; const AFieldName: string; const ANewValue: Variant);
    procedure Execute;
    procedure Undo;
    function GetDescription: string;
  end;

procedure TChangeFieldCommand.Execute;
begin
  FOldValue := FDataSet.FieldByName(FFieldName).Value;
  FDataSet.Edit;
  FDataSet.FieldByName(FFieldName).Value := FNewValue;
  FDataSet.Post;
end;

procedure TChangeFieldCommand.Undo;
begin
  FDataSet.Edit;
  FDataSet.FieldByName(FFieldName).Value := FOldValue;
  FDataSet.Post;
end;
```

### Strategy Pattern

```pascal
type
  ICalculoIVA = interface
    function Calcular(const Base: Currency): Currency;
    function GetPorcentaje: Double;
    function GetDescripcion: string;
  end;

  TIVAGeneral = class(TInterfacedObject, ICalculoIVA)
  public
    function Calcular(const Base: Currency): Currency;
    function GetPorcentaje: Double;
    function GetDescripcion: string;
  end;

  TIVAReducido = class(TInterfacedObject, ICalculoIVA)
  public
    function Calcular(const Base: Currency): Currency;
    function GetPorcentaje: Double;
    function GetDescripcion: string;
  end;

  TIVASuperReducido = class(TInterfacedObject, ICalculoIVA)
  public
    function Calcular(const Base: Currency): Currency;
    function GetPorcentaje: Double;
    function GetDescripcion: string;
  end;

  TFacturaCalculator = class
  private
    FEstrategiaIVA: ICalculoIVA;
  public
    procedure SetEstrategiaIVA(const Estrategia: ICalculoIVA);
    function CalcularTotal(const Base: Currency): Currency;
  end;

// Uso
var
  Calculator: TFacturaCalculator;
begin
  Calculator := TFacturaCalculator.Create;
  try
    Calculator.SetEstrategiaIVA(TIVAGeneral.Create);  // 21%
    Total21 := Calculator.CalcularTotal(100);  // 121
    
    Calculator.SetEstrategiaIVA(TIVAReducido.Create);  // 10%
    Total10 := Calculator.CalcularTotal(100);  // 110
  finally
    Calculator.Free;
  end;
end;
```

### Builder Pattern

```pascal
type
  TFacturaBuilder = class
  private
    FFactura: TFactura;
  public
    constructor Create;
    function WithCliente(const NIF, Nombre: string): TFacturaBuilder;
    function WithFecha(const Fecha: TDateTime): TFacturaBuilder;
    function WithLinea(const Concepto: string; Cantidad: Integer; Precio: Currency): TFacturaBuilder;
    function WithDescuento(const Porcentaje: Double): TFacturaBuilder;
    function WithVerifactu(const HashAnterior: string): TFacturaBuilder;
    function Build: TFactura;
  end;

function TFacturaBuilder.WithCliente(const NIF, Nombre: string): TFacturaBuilder;
begin
  FFactura.NIFCliente := NIF;
  FFactura.NombreCliente := Nombre;
  Result := Self;  // Fluent interface
end;

function TFacturaBuilder.Build: TFactura;
begin
  FFactura.Validar;  // Validación antes de construir
  FFactura.CalcularTotales;
  Result := FFactura;
  FFactura := nil;  // Transferir ownership
end;

// Uso fluido
var
  Factura: TFactura;
begin
  Factura := TFacturaBuilder.Create
    .WithCliente('12345678Z', 'Cliente Ejemplo')
    .WithFecha(Now)
    .WithLinea('Servicio A', 1, 100)
    .WithLinea('Servicio B', 2, 50)
    .WithDescuento(5)
    .WithVerifactu(UltimoHash)
    .Build;
end;
```

### Decorator Pattern

```pascal
type
  IReporte = interface
    procedure Generar;
    function GetContenido: string;
  end;

  TReporteBase = class(TInterfacedObject, IReporte)
  public
    procedure Generar; virtual;
    function GetContenido: string; virtual;
  end;

  TReporteDecorator = class(TInterfacedObject, IReporte)
  protected
    FReporte: IReporte;
  public
    constructor Create(const AReporte: IReporte);
    procedure Generar; virtual;
    function GetContenido: string; virtual;
  end;

  TReporteConLogo = class(TReporteDecorator)
  public
    procedure Generar; override;
  end;

  TReporteConFirma = class(TReporteDecorator)
  public
    procedure Generar; override;
  end;

  TReporteConQR = class(TReporteDecorator)
  private
    FQRData: string;
  public
    constructor Create(const AReporte: IReporte; const AQRData: string);
    procedure Generar; override;
  end;

// Uso composicional
var
  Reporte: IReporte;
begin
  Reporte := TReporteBase.Create;
  Reporte := TReporteConLogo.Create(Reporte);
  Reporte := TReporteConQR.Create(Reporte, QRContent);
  Reporte := TReporteConFirma.Create(Reporte);
  Reporte.Generar;
end;
```

### State Pattern (Máquina de Estados)

```pascal
type
  TEstadoFactura = (efBorrador, efEmitida, efEnviada, efPagada, efAnulada);

  IEstadoFactura = interface
    function PuedeEditar: Boolean;
    function PuedeEmitir: Boolean;
    function PuedeAnular: Boolean;
    function PuedePagar: Boolean;
    procedure Emitir(Factura: TFactura);
    procedure Anular(Factura: TFactura);
    procedure Pagar(Factura: TFactura);
  end;

  TEstadoBorrador = class(TInterfacedObject, IEstadoFactura)
  public
    function PuedeEditar: Boolean;  // True
    function PuedeEmitir: Boolean;  // True
    function PuedeAnular: Boolean;  // False
    function PuedePagar: Boolean;   // False
    procedure Emitir(Factura: TFactura);
    procedure Anular(Factura: TFactura);
    procedure Pagar(Factura: TFactura);
  end;

  TEstadoEmitida = class(TInterfacedObject, IEstadoFactura)
  public
    function PuedeEditar: Boolean;  // False
    function PuedeEmitir: Boolean;  // False
    function PuedeAnular: Boolean;  // True
    function PuedePagar: Boolean;   // True
    procedure Emitir(Factura: TFactura);
    procedure Anular(Factura: TFactura);
    procedure Pagar(Factura: TFactura);
  end;

  TFactura = class
  private
    FEstado: IEstadoFactura;
    FEstadoActual: TEstadoFactura;
    procedure SetEstado(const Value: TEstadoFactura);
  public
    property Estado: TEstadoFactura read FEstadoActual write SetEstado;
    procedure Emitir;
    procedure Anular;
    procedure Pagar;
    function PuedeEditar: Boolean;
  end;

procedure TFactura.Emitir;
begin
  FEstado.Emitir(Self);
end;

function TFactura.PuedeEditar: Boolean;
begin
  Result := FEstado.PuedeEditar;
end;
```

### Unit of Work Pattern (para transacciones)

```pascal
type
  IUnitOfWork = interface
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function GetFacturaRepository: IFacturaRepository;
    function GetLineaRepository: ILineaRepository;
    function GetClienteRepository: IClienteRepository;
  end;

  TBDEUnitOfWork = class(TInterfacedObject, IUnitOfWork)
  private
    FDatabase: TDatabase;
    FFacturaRepo: IFacturaRepository;
    FLineaRepo: ILineaRepository;
    FClienteRepo: IClienteRepository;
  public
    constructor Create(ADatabase: TDatabase);
    procedure BeginTransaction;
    procedure Commit;
    procedure Rollback;
    function GetFacturaRepository: IFacturaRepository;
    function GetLineaRepository: ILineaRepository;
    function GetClienteRepository: IClienteRepository;
  end;

// Uso
procedure TFacturaService.GuardarFacturaCompleta(const Factura: TFactura);
var
  UoW: IUnitOfWork;
begin
  UoW := TBDEUnitOfWork.Create(Database1);
  UoW.BeginTransaction;
  try
    UoW.GetFacturaRepository.Add(Factura);
    for var Linea in Factura.Lineas do
      UoW.GetLineaRepository.Add(Linea);
    UoW.Commit;
  except
    UoW.Rollback;
    raise;
  end;
end;
```

---

## 🧩 COMPONENTES DE TERCEROS COMUNES

### DevExpress (cxGrid, ExpressBars)

```pascal
// Configurar cxGrid programáticamente
procedure ConfigurarGrid(Grid: TcxGrid);
var
  View: TcxGridDBTableView;
  Column: TcxGridDBColumn;
begin
  View := Grid.CreateView(TcxGridDBTableView) as TcxGridDBTableView;
  View.DataController.DataSource := DataSource1;
  
  // Columna con formato monetario
  Column := View.CreateColumn;
  Column.DataBinding.FieldName := 'Total';
  Column.PropertiesClass := TcxCurrencyEditProperties;
  Column.Caption := 'Total';
  Column.Width := 100;
  
  // Columna con lookup
  Column := View.CreateColumn;
  Column.DataBinding.FieldName := 'ClienteID';
  Column.PropertiesClass := TcxLookupComboBoxProperties;
  with TcxLookupComboBoxProperties(Column.Properties) do
  begin
    ListSource := dsClientes;
    KeyFieldNames := 'ID';
    ListFieldNames := 'Nombre';
  end;
  
  Grid.Levels[0].GridView := View;
end;

// ExpressBars - Crear menú dinámico
procedure CrearMenuDinamico(BarManager: TdxBarManager);
var
  Item: TdxBarButton;
begin
  Item := TdxBarButton.Create(BarManager);
  Item.Caption := 'Nueva Factura';
  Item.ImageIndex := 0;
  Item.OnClick := NuevaFacturaClick;
  BarManager.Bars[0].ItemLinks.Add(Item);
end;
```

### FastReport

```pascal
// Generar reporte de factura
procedure GenerarFacturaFR(const NumFactura: string);
var
  Report: TfrxReport;
  DataSet: TfrxDBDataset;
begin
  Report := TfrxReport.Create(nil);
  DataSet := TfrxDBDataset.Create(nil);
  try
    DataSet.DataSource := dsFacturas;
    DataSet.UserName := 'Factura';
    
    Report.DataSets.Add(DataSet);
    Report.LoadFromFile('Factura.fr3');
    
    // Pasar parámetros
    Report.Script.Variables['NumFactura'] := NumFactura;
    Report.Script.Variables['FechaEmision'] := Now;
    
    // Vista previa o imprimir directo
    if MostrarPreview then
      Report.ShowReport
    else
      Report.Print;
  finally
    DataSet.Free;
    Report.Free;
  end;
end;

// Exportar a PDF
procedure ExportarFacturaPDF(const NumFactura, RutaPDF: string);
var
  Report: TfrxReport;
  PDFExport: TfrxPDFExport;
begin
  Report := TfrxReport.Create(nil);
  PDFExport := TfrxPDFExport.Create(nil);
  try
    Report.LoadFromFile('Factura.fr3');
    Report.PrepareReport;
    
    PDFExport.FileName := RutaPDF;
    PDFExport.ShowDialog := False;
    PDFExport.ShowProgress := False;
    Report.Export(PDFExport);
  finally
    PDFExport.Free;
    Report.Free;
  end;
end;
```

### Indy (TIdHTTP, TIdSMTP)

```pascal
// Enviar petición HTTP a AEAT
function EnviarAEAT(const XML: string): string;
var
  HTTP: TIdHTTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  Request, Response: TStringStream;
begin
  HTTP := TIdHTTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Request := TStringStream.Create(XML, TEncoding.UTF8);
  Response := TStringStream.Create('', TEncoding.UTF8);
  try
    SSL.SSLOptions.Method := sslvTLSv1_2;
    SSL.SSLOptions.SSLVersions := [sslvTLSv1_2];
    HTTP.IOHandler := SSL;
    
    HTTP.Request.ContentType := 'application/xml';
    HTTP.Request.Charset := 'UTF-8';
    HTTP.Request.CustomHeaders.AddValue('Authorization', 'Bearer ' + Token);
    
    HTTP.Post(AEAT_URL, Request, Response);
    Result := Response.DataString;
  finally
    Response.Free;
    Request.Free;
    SSL.Free;
    HTTP.Free;
  end;
end;

// Enviar correo con factura adjunta
procedure EnviarFacturaPorEmail(const Email, Asunto, Cuerpo, RutaPDF: string);
var
  SMTP: TIdSMTP;
  SSL: TIdSSLIOHandlerSocketOpenSSL;
  Msg: TIdMessage;
  Attachment: TIdAttachmentFile;
begin
  SMTP := TIdSMTP.Create(nil);
  SSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  Msg := TIdMessage.Create(nil);
  try
    SSL.SSLOptions.Method := sslvTLSv1_2;
    SMTP.IOHandler := SSL;
    SMTP.Host := 'smtp.ejemplo.com';
    SMTP.Port := 587;
    SMTP.Username := 'usuario@ejemplo.com';
    SMTP.Password := 'password';
    SMTP.UseTLS := utUseExplicitTLS;
    
    Msg.From.Address := 'facturacion@empresa.com';
    Msg.Recipients.Add.Address := Email;
    Msg.Subject := Asunto;
    Msg.Body.Text := Cuerpo;
    
    Attachment := TIdAttachmentFile.Create(Msg.MessageParts, RutaPDF);
    
    SMTP.Connect;
    try
      SMTP.Send(Msg);
    finally
      SMTP.Disconnect;
    end;
  finally
    Msg.Free;
    SSL.Free;
    SMTP.Free;
  end;
end;
```

### DelphiZXingQRCode (QR para Verifactu)

```pascal
uses
  DelphiZXIngQRCode;

// Generar QR para Verifactu
function GenerarQRVerifactu(const Factura: TFactura): TBitmap;
var
  QRCode: TDelphiZXingQRCode;
  Row, Col: Integer;
  Scale: Integer;
  QRContent: string;
begin
  // Construir contenido según especificación Verifactu
  QRContent := Format('https://www2.agenciatributaria.gob.es/wlpl/TIKE-CONT/ValidarQR?' +
    'nif=%s&numserie=%s&fecha=%s&importe=%s',
    [Factura.NIFEmisor,
     Factura.NumeroFactura,
     FormatDateTime('dd-mm-yyyy', Factura.FechaExpedicion),
     FormatFloat('0.00', Factura.ImporteTotal)]);

  QRCode := TDelphiZXingQRCode.Create;
  Result := TBitmap.Create;
  try
    QRCode.Data := QRContent;
    QRCode.Encoding := qrUTF8NoBOM;
    QRCode.QuietZone := 4;
    
    Scale := 4;  // Píxeles por módulo
    Result.Width := QRCode.Columns * Scale;
    Result.Height := QRCode.Rows * Scale;
    Result.Canvas.Brush.Color := clWhite;
    Result.Canvas.FillRect(Rect(0, 0, Result.Width, Result.Height));
    
    Result.Canvas.Brush.Color := clBlack;
    for Row := 0 to QRCode.Rows - 1 do
      for Col := 0 to QRCode.Columns - 1 do
        if QRCode.IsBlack[Row, Col] then
          Result.Canvas.FillRect(Rect(
            Col * Scale, Row * Scale,
            (Col + 1) * Scale, (Row + 1) * Scale));
  finally
    QRCode.Free;
  end;
end;

// Dibujar QR en Canvas de impresión
procedure DibujarQREnReporte(Canvas: TCanvas; X, Y, Size: Integer; const QRData: string);
var
  QRBitmap: TBitmap;
  DestRect: TRect;
begin
  QRBitmap := GenerarQRVerifactu(QRData);
  try
    DestRect := Rect(X, Y, X + Size, Y + Size);
    Canvas.StretchDraw(DestRect, QRBitmap);
  finally
    QRBitmap.Free;
  end;
end;
```

### TMS Components

```pascal
// TMS AdvStringGrid con colores y estilos
procedure ConfigurarAdvGrid(Grid: TAdvStringGrid);
begin
  Grid.FixedRows := 1;
  Grid.FixedCols := 0;
  Grid.ColCount := 5;
  Grid.RowCount := 100;
  
  // Cabeceras
  Grid.Cells[0, 0] := 'Código';
  Grid.Cells[1, 0] := 'Descripción';
  Grid.Cells[2, 0] := 'Cantidad';
  Grid.Cells[3, 0] := 'Precio';
  Grid.Cells[4, 0] := 'Total';
  
  // Formato de columnas
  Grid.ColWidths[0] := 80;
  Grid.ColWidths[1] := 250;
  Grid.FloatFormat[2] := '0';
  Grid.FloatFormat[3] := '#,##0.00 €';
  Grid.FloatFormat[4] := '#,##0.00 €';
  
  // Colores alternados
  Grid.Bands.Active := True;
  Grid.Bands.PrimaryColor := clWhite;
  Grid.Bands.SecondaryColor := $00F0F0F0;
end;

// TAdvEdit con validación
procedure ConfigurarEditNIF(Edit: TAdvEdit);
begin
  Edit.EditType := etString;
  Edit.MaxLength := 9;
  Edit.CharCase := ecUpperCase;
  Edit.OnValidate := ValidarNIF;
end;
```

---

## 📋 IMPLEMENTACIÓN VERIFACTU COMPLETA

### Estructura de Datos Verifactu

```pascal
type
  TTipoFactura = (tfFactura, tfFacturaSimplificada, tfFacturaRectificativa);
  TEstadoVerifactu = (evPendiente, evGenerado, evEnviado, evAceptado, evRechazado);

  TRegistroVerifactu = record
    // Identificación
    IDFactura: string;
    NumeroFactura: string;
    SerieFactura: string;
    FechaExpedicion: TDateTime;
    HoraExpedicion: TTime;
    
    // Emisor
    NIFEmisor: string;
    NombreEmisor: string;
    
    // Destinatario
    NIFDestinatario: string;
    NombreDestinatario: string;
    
    // Importes
    BaseImponible: Currency;
    TipoIVA: Double;
    CuotaIVA: Currency;
    ImporteTotal: Currency;
    
    // Verifactu específico
    TipoFactura: TTipoFactura;
    HashAnterior: string;
    HashActual: string;
    HuellaRegistro: string;
    FirmaDigital: string;
    CodigoQR: string;
    
    // Estado
    Estado: TEstadoVerifactu;
    FechaGeneracion: TDateTime;
    FechaEnvio: TDateTime;
    RespuestaAEAT: string;
  end;
```

### Generación de Huella (Hash Encadenado)

```pascal
uses
  System.Hash, System.NetEncoding;

type
  TVerifactuHasher = class
  private
    class function ConstruirCadenaHash(const Reg: TRegistroVerifactu): string;
  public
    class function GenerarHash(const Reg: TRegistroVerifactu; const HashAnterior: string): string;
    class function ValidarCadena(const Registros: TArray<TRegistroVerifactu>): Boolean;
    class function ObtenerPrimerHash: string;  // Hash inicial de la cadena
  end;

class function TVerifactuHasher.ConstruirCadenaHash(const Reg: TRegistroVerifactu): string;
begin
  // Según especificación técnica AEAT - Orden de campos obligatorios
  Result := Format('%s|%s|%s|%s|%s|%s|%s|%s',
    [Reg.NIFEmisor,
     Reg.NumeroFactura + Reg.SerieFactura,
     FormatDateTime('dd-mm-yyyy', Reg.FechaExpedicion),
     FormatDateTime('hh:nn:ss', Reg.HoraExpedicion),
     FormatFloat('0.00', Reg.BaseImponible),
     FormatFloat('0.00', Reg.CuotaIVA),
     FormatFloat('0.00', Reg.ImporteTotal),
     Reg.HashAnterior]);
end;

class function TVerifactuHasher.GenerarHash(const Reg: TRegistroVerifactu; 
  const HashAnterior: string): string;
var
  Cadena: string;
  Bytes: TBytes;
begin
  // Incluir hash anterior para encadenamiento
  var RegConHash := Reg;
  RegConHash.HashAnterior := HashAnterior;
  
  Cadena := ConstruirCadenaHash(RegConHash);
  
  // SHA-256 según normativa
  Bytes := TEncoding.UTF8.GetBytes(Cadena);
  Result := THashSHA2.GetHashString(Cadena, THashSHA2.TSHA2Version.SHA256);
end;

class function TVerifactuHasher.ValidarCadena(const Registros: TArray<TRegistroVerifactu>): Boolean;
var
  I: Integer;
  HashCalculado: string;
begin
  Result := True;
  
  for I := 0 to High(Registros) do
  begin
    if I = 0 then
      HashCalculado := GenerarHash(Registros[I], ObtenerPrimerHash)
    else
      HashCalculado := GenerarHash(Registros[I], Registros[I-1].HashActual);
    
    if HashCalculado <> Registros[I].HashActual then
    begin
      Result := False;
      LogError('Cadena rota en registro: ' + Registros[I].NumeroFactura);
      Exit;
    end;
  end;
end;

class function TVerifactuHasher.ObtenerPrimerHash: string;
begin
  // Hash inicial definido por normativa (vacío o valor específico)
  Result := THashSHA2.GetHashString('INICIO_CADENA', THashSHA2.TSHA2Version.SHA256);
end;
```

### Generación de XML AEAT

```pascal
uses
  Xml.XMLDoc, Xml.XMLIntf;

type
  TVerifactuXMLGenerator = class
  public
    class function GenerarAltaFactura(const Reg: TRegistroVerifactu): string;
    class function GenerarAnulacionFactura(const Reg: TRegistroVerifactu): string;
    class function GenerarLote(const Registros: TArray<TRegistroVerifactu>): string;
  end;

class function TVerifactuXMLGenerator.GenerarAltaFactura(const Reg: TRegistroVerifactu): string;
var
  XML: IXMLDocument;
  Root, Cabecera, Factura, Emisor, Destinatario, Desglose: IXMLNode;
begin
  XML := TXMLDocument.Create(nil);
  XML.Active := True;
  XML.Version := '1.0';
  XML.Encoding := 'UTF-8';
  
  // Namespace según esquema AEAT
  Root := XML.AddChild('sii:SuministroLRFacturasEmitidas');
  Root.DeclareNamespace('sii', 'https://www2.agenciatributaria.gob.es/static_files/common/internet/dep/aplicaciones/es/aeat/tike/cont/ws/SuministroLR.xsd');
  Root.DeclareNamespace('siiLR', 'https://www2.agenciatributaria.gob.es/static_files/common/internet/dep/aplicaciones/es/aeat/tike/cont/ws/SuministroInformacion.xsd');
  
  // Cabecera
  Cabecera := Root.AddChild('sii:Cabecera');
  Cabecera.AddChild('sii:IDVersionSii').Text := '1.1';
  Cabecera.AddChild('sii:Titular').AddChild('sii:NIF').Text := Reg.NIFEmisor;
  Cabecera.ChildNodes['sii:Titular'].AddChild('sii:NombreRazon').Text := Reg.NombreEmisor;
  
  // Registro factura
  Factura := Root.AddChild('sii:RegistroLRFacturasEmitidas');
  
  // Periodo
  Factura.AddChild('sii:PeriodoLiquidacion').AddChild('sii:Ejercicio').Text := 
    FormatDateTime('yyyy', Reg.FechaExpedicion);
  Factura.ChildNodes['sii:PeriodoLiquidacion'].AddChild('sii:Periodo').Text := 
    FormatDateTime('mm', Reg.FechaExpedicion);
  
  // ID Factura
  var IDFactura := Factura.AddChild('sii:IDFactura');
  IDFactura.AddChild('sii:IDEmisorFactura').AddChild('sii:NIF').Text := Reg.NIFEmisor;
  IDFactura.AddChild('sii:NumSerieFacturaEmisor').Text := Reg.NumeroFactura;
  IDFactura.AddChild('sii:FechaExpedicionFacturaEmisor').Text := 
    FormatDateTime('dd-mm-yyyy', Reg.FechaExpedicion);
  
  // Factura expedida
  var FactExpedida := Factura.AddChild('sii:FacturaExpedida');
  FactExpedida.AddChild('sii:TipoFactura').Text := 'F1';  // Factura normal
  FactExpedida.AddChild('sii:ClaveRegimenEspecialOTrascendencia').Text := '01';
  FactExpedida.AddChild('sii:ImporteTotal').Text := FormatFloat('0.00', Reg.ImporteTotal);
  FactExpedida.AddChild('sii:DescripcionOperacion').Text := 'Factura de venta';
  
  // Contraparte (destinatario)
  var Contraparte := FactExpedida.AddChild('sii:Contraparte');
  Contraparte.AddChild('sii:NombreRazon').Text := Reg.NombreDestinatario;
  Contraparte.AddChild('sii:NIF').Text := Reg.NIFDestinatario;
  
  // Desglose factura
  Desglose := FactExpedida.AddChild('sii:TipoDesglose').AddChild('sii:DesgloseFactura');
  var Sujeta := Desglose.AddChild('sii:Sujeta').AddChild('sii:NoExenta');
  var TipoNoExenta := Sujeta.AddChild('sii:TipoNoExenta');
  TipoNoExenta.AddChild('sii:DesgloseIVA').AddChild('sii:DetalleIVA');
  var DetalleIVA := TipoNoExenta.ChildNodes['sii:DesgloseIVA'].ChildNodes['sii:DetalleIVA'];
  DetalleIVA.AddChild('sii:TipoImpositivo').Text := FormatFloat('0.00', Reg.TipoIVA);
  DetalleIVA.AddChild('sii:BaseImponible').Text := FormatFloat('0.00', Reg.BaseImponible);
  DetalleIVA.AddChild('sii:CuotaRepercutida').Text := FormatFloat('0.00', Reg.CuotaIVA);
  
  // Huella Verifactu
  var Huella := FactExpedida.AddChild('sii:Huella');
  Huella.AddChild('sii:Hash').Text := Reg.HashActual;
  Huella.AddChild('sii:HashAnterior').Text := Reg.HashAnterior;
  
  Result := XML.XML.Text;
end;
```

### Validación de NIF/CIF

```pascal
type
  TTipoDocumento = (tdNIF, tdNIE, tdCIF, tdDesconocido);

  TValidadorNIF = class
  public
    class function Validar(const Documento: string): Boolean;
    class function ObtenerTipo(const Documento: string): TTipoDocumento;
    class function CalcularLetra(const Numero: string): Char;
    class function ValidarConAEAT(const NIF: string): Boolean;
  end;

class function TValidadorNIF.Validar(const Documento: string): Boolean;
var
  Doc: string;
  Tipo: TTipoDocumento;
begin
  Doc := UpperCase(Trim(Documento));
  if Length(Doc) <> 9 then
    Exit(False);
    
  Tipo := ObtenerTipo(Doc);
  
  case Tipo of
    tdNIF:
      Result := ValidarNIF(Doc);
    tdNIE:
      Result := ValidarNIE(Doc);
    tdCIF:
      Result := ValidarCIF(Doc);
  else
    Result := False;
  end;
end;

class function TValidadorNIF.ObtenerTipo(const Documento: string): TTipoDocumento;
var
  Primera: Char;
begin
  if Length(Documento) = 0 then
    Exit(tdDesconocido);
    
  Primera := Documento[1];
  
  if CharInSet(Primera, ['0'..'9']) then
    Result := tdNIF
  else if CharInSet(Primera, ['X', 'Y', 'Z']) then
    Result := tdNIE
  else if CharInSet(Primera, ['A'..'H', 'J', 'U', 'V', 'N', 'P', 'Q', 'R', 'S', 'W']) then
    Result := tdCIF
  else
    Result := tdDesconocido;
end;

class function TValidadorNIF.CalcularLetra(const Numero: string): Char;
const
  LETRAS = 'TRWAGMYFPDXBNJZSQVHLCKE';
var
  Num: Integer;
begin
  if not TryStrToInt(Numero, Num) then
    raise Exception.Create('Número inválido para cálculo de letra');
  Result := LETRAS[(Num mod 23) + 1];
end;

class function TValidadorNIF.ValidarConAEAT(const NIF: string): Boolean;
var
  HTTP: TIdHTTP;
  Response: string;
begin
  // Llamada al servicio de validación de la AEAT
  HTTP := TIdHTTP.Create(nil);
  try
    try
      Response := HTTP.Get(Format(
        'https://www1.agenciatributaria.gob.es/wlpl/BURT-JDIT/IdentificadorWS?nif=%s',
        [NIF]));
      Result := Pos('"valido":true', Response) > 0;
    except
      Result := False;  // En caso de error, asumir no válido
    end;
  finally
    HTTP.Free;
  end;
end;
```

### Log de Eventos Verifactu

```pascal
type
  TTipoEventoVerifactu = (
    tevGeneracionFactura,
    tevEmisionFactura,
    tevAnulacionFactura,
    tevEnvioAEAT,
    tevRespuestaAEAT,
    tevErrorSistema,
    tevModificacionDatos,
    tevAccesoUsuario
  );

  TLogVerifactu = class
  private
    FDatabase: TDatabase;
  public
    procedure Registrar(const TipoEvento: TTipoEventoVerifactu;
      const NumFactura, Descripcion, Usuario: string;
      const DatosAdicionales: string = '');
    function ObtenerLog(const FechaDesde, FechaHasta: TDateTime): TDataSet;
    function ExportarLog(const Ruta: string): Boolean;
  end;

procedure TLogVerifactu.Registrar(const TipoEvento: TTipoEventoVerifactu;
  const NumFactura, Descripcion, Usuario, DatosAdicionales: string);
const
  SQL_INSERT = 
    'INSERT INTO LogVerifactu (Fecha, Hora, TipoEvento, NumFactura, ' +
    'Descripcion, Usuario, DatosAdicionales, HashRegistro) ' +
    'VALUES (:Fecha, :Hora, :TipoEvento, :NumFactura, :Descripcion, ' +
    ':Usuario, :DatosAdicionales, :HashRegistro)';
var
  Query: TQuery;
  DatosHash: string;
begin
  Query := TQuery.Create(nil);
  try
    Query.DatabaseName := FDatabase.DatabaseName;
    Query.SQL.Text := SQL_INSERT;
    
    Query.ParamByName('Fecha').AsDate := Date;
    Query.ParamByName('Hora').AsTime := Time;
    Query.ParamByName('TipoEvento').AsInteger := Ord(TipoEvento);
    Query.ParamByName('NumFactura').AsString := NumFactura;
    Query.ParamByName('Descripcion').AsString := Descripcion;
    Query.ParamByName('Usuario').AsString := Usuario;
    Query.ParamByName('DatosAdicionales').AsString := DatosAdicionales;
    
    // Hash del registro para integridad
    DatosHash := Format('%s|%s|%d|%s|%s',
      [DateToStr(Date), TimeToStr(Time), Ord(TipoEvento), NumFactura, Descripcion]);
    Query.ParamByName('HashRegistro').AsString := 
      THashSHA2.GetHashString(DatosHash, THashSHA2.TSHA2Version.SHA256);
    
    Query.ExecSQL;
  finally
    Query.Free;
  end;
end;
```

---

## 🔄 MIGRACIÓN ENTRE VERSIONES DE DELPHI

### De Delphi 10.x a Delphi 11/12/13

#### Cambios de Sintaxis Recomendados

```pascal
// ANTES (Delphi 10.x y anteriores)
var
  I: Integer;
  S: string;
  List: TStringList;
begin
  List := TStringList.Create;
  try
    for I := 0 to 10 do
    begin
      S := IntToStr(I);
      List.Add(S);
    end;
  finally
    List.Free;
  end;
end;

// DESPUÉS (Delphi 11+) - Más moderno
begin
  var List := TStringList.Create;
  try
    for var I := 0 to 10 do
      List.Add(I.ToString);
  finally
    List.Free;
  end;
end;
```

#### Nuevas Características por Versión

```pascal
// Delphi 10.3 Rio - Inline Variables
begin
  var X := 10;  // Tipo inferido
  var Y: Double := 3.14;  // Tipo explícito
end;

// Delphi 10.4 Sydney - Custom Managed Records
type
  TMyRecord = record
    Value: string;
    class operator Initialize(out Dest: TMyRecord);
    class operator Finalize(var Dest: TMyRecord);
  end;

// Delphi 11 Alexandria - Binary Literals
const
  FLAGS = 0b10101010;  // Binary literal
  
// Delphi 12 Athens - Multiline Strings
const
  SQL_QUERY = '''
    SELECT *
    FROM Facturas
    WHERE Fecha >= :FechaDesde
      AND Fecha <= :FechaHasta
    ORDER BY Fecha DESC
  ''';
```

#### Migrar de ANSI a Unicode

```pascal
// ANTES (Delphi 7 / 2007)
var
  S: AnsiString;
  P: PAnsiChar;
begin
  S := 'Texto ANSI';
  P := PAnsiChar(S);
end;

// DESPUÉS (Delphi 2009+)
var
  S: string;  // Ya es Unicode (UnicodeString)
  P: PChar;   // Ya es PWideChar
begin
  S := 'Texto Unicode';
  P := PChar(S);
end;

// Si necesitas ANSI para compatibilidad
var
  AnsiS: AnsiString;
  UnicodeS: string;
begin
  UnicodeS := 'Texto';
  AnsiS := AnsiString(UnicodeS);  // Conversión explícita
end;
```

#### Migrar de BDE a FireDAC

```pascal
// ANTES (BDE)
uses
  DBTables;

var
  Table: TTable;
  Query: TQuery;
begin
  Table := TTable.Create(nil);
  Table.DatabaseName := 'MiDB';
  Table.TableName := 'Clientes';
  Table.Open;
  
  Query := TQuery.Create(nil);
  Query.DatabaseName := 'MiDB';
  Query.SQL.Text := 'SELECT * FROM Clientes WHERE ID = :ID';
  Query.ParamByName('ID').AsInteger := 1;
  Query.Open;
end;

// DESPUÉS (FireDAC)
uses
  FireDAC.Comp.Client;

var
  Table: TFDTable;
  Query: TFDQuery;
begin
  Table := TFDTable.Create(nil);
  Table.Connection := FDConnection1;
  Table.TableName := 'Clientes';
  Table.Open;
  
  Query := TFDQuery.Create(nil);
  Query.Connection := FDConnection1;
  Query.SQL.Text := 'SELECT * FROM Clientes WHERE ID = :ID';
  Query.ParamByName('ID').AsInteger := 1;
  Query.Open;
end;

// Wrapper para migración gradual
type
  TDBWrapper = class
  private
    FUseBDE: Boolean;
    FBDEQuery: TQuery;
    FFireDACQuery: TFDQuery;
  public
    procedure SetSQL(const SQL: string);
    procedure SetParam(const Name: string; Value: Variant);
    procedure Open;
    procedure ExecSQL;
    function FieldByName(const Name: string): TField;
  end;
```

#### Formulario de Compatibilidad de Componentes

```pascal
// Verificar versión de Delphi en tiempo de compilación
{$IF CompilerVersion >= 35.0}  // Delphi 11+
  // Código específico para Delphi 11+
  var InlineVar := 'Soportado';
{$ELSE}
  // Código compatible con versiones anteriores
  var
    InlineVar: string;
  begin
    InlineVar := 'Modo compatibilidad';
  end;
{$ENDIF}

// Constantes útiles de versión
{$IF CompilerVersion >= 36.0}
  {$DEFINE DELPHI12_UP}
{$ENDIF}
{$IF CompilerVersion >= 35.0}
  {$DEFINE DELPHI11_UP}
{$ENDIF}
{$IF CompilerVersion >= 34.0}
  {$DEFINE DELPHI10_4_UP}
{$ENDIF}

// Uso
{$IFDEF DELPHI12_UP}
procedure UsarMultilineStrings;
const
  SQL = '''
    SELECT * FROM table
    WHERE condition
  ''';
begin
end;
{$ELSE}
procedure UsarMultilineStrings;
const
  SQL = 'SELECT * FROM table ' +
        'WHERE condition';
begin
end;
{$ENDIF}
```

#### Script de Migración Automática

```pascal
// Herramienta para detectar código obsoleto
type
  TCodeMigrator = class
  public
    class procedure AnalizarUnidad(const RutaPas: string);
    class function DetectarBDE(const Codigo: string): TArray<string>;
    class function DetectarAnsiString(const Codigo: string): TArray<string>;
    class function SugerirMejoras(const Codigo: string): TArray<string>;
  end;

class function TCodeMigrator.DetectarBDE(const Codigo: string): TArray<string>;
var
  Lineas: TStringList;
  I: Integer;
  Problemas: TList<string>;
begin
  Lineas := TStringList.Create;
  Problemas := TList<string>.Create;
  try
    Lineas.Text := Codigo;
    
    for I := 0 to Lineas.Count - 1 do
    begin
      if Pos('TTable', Lineas[I]) > 0 then
        Problemas.Add(Format('Línea %d: TTable (BDE) - Migrar a TFDTable', [I + 1]));
      if Pos('TQuery', Lineas[I]) > 0 then
        Problemas.Add(Format('Línea %d: TQuery (BDE) - Migrar a TFDQuery', [I + 1]));
      if Pos('TDatabase', Lineas[I]) > 0 then
        Problemas.Add(Format('Línea %d: TDatabase (BDE) - Migrar a TFDConnection', [I + 1]));
      if Pos('DBTables', Lineas[I]) > 0 then
        Problemas.Add(Format('Línea %d: uses DBTables - Cambiar a FireDAC.Comp.Client', [I + 1]));
    end;
    
    Result := Problemas.ToArray;
  finally
    Problemas.Free;
    Lineas.Free;
  end;
end;
```

---

## 🛠️ HERRAMIENTAS Y UTILIDADES

### Clase de Utilidades Generales

```pascal
type
  TDelphiUtils = class
  public
    // Strings
    class function IsEmpty(const S: string): Boolean; inline;
    class function IfThen(Condition: Boolean; const TrueValue, FalseValue: string): string; overload;
    class function IfThen<T>(Condition: Boolean; const TrueValue, FalseValue: T): T; overload;
    class function Coalesce(const Values: array of string): string;
    
    // Fechas
    class function StartOfDay(const ADate: TDateTime): TDateTime;
    class function EndOfDay(const ADate: TDateTime): TDateTime;
    class function IsWeekend(const ADate: TDateTime): Boolean;
    class function AddWorkDays(const ADate: TDateTime; Days: Integer): TDateTime;
    
    // Números
    class function TruncateCurrency(const Value: Currency; Decimals: Integer): Currency;
    class function RoundCurrency(const Value: Currency; Decimals: Integer): Currency;
    
    // Archivos
    class function GetTempFileName(const Prefix: string = 'tmp'): string;
    class function EnsureDirectoryExists(const Path: string): Boolean;
    class function GetAppDataPath: string;
  end;

class function TDelphiUtils.IsEmpty(const S: string): Boolean;
begin
  Result := S.Trim.IsEmpty;
end;

class function TDelphiUtils.Coalesce(const Values: array of string): string;
begin
  for var S in Values do
    if not IsEmpty(S) then
      Exit(S);
  Result := '';
end;

class function TDelphiUtils.EndOfDay(const ADate: TDateTime): TDateTime;
begin
  Result := Trunc(ADate) + EncodeTime(23, 59, 59, 999);
end;

class function TDelphiUtils.TruncateCurrency(const Value: Currency; Decimals: Integer): Currency;
var
  Factor: Currency;
begin
  Factor := Power(10, Decimals);
  Result := Trunc(Value * Factor) / Factor;
end;
```

### Gestor de Configuración

```pascal
type
  TConfigManager = class
  private
    FIniFile: TIniFile;
    FCache: TDictionary<string, Variant>;
    class var FInstance: TConfigManager;
  public
    constructor Create(const ConfigPath: string);
    destructor Destroy; override;
    
    class function GetInstance: TConfigManager;
    
    function GetString(const Section, Key: string; const Default: string = ''): string;
    function GetInteger(const Section, Key: string; Default: Integer = 0): Integer;
    function GetBoolean(const Section, Key: string; Default: Boolean = False): Boolean;
    function GetCurrency(const Section, Key: string; Default: Currency = 0): Currency;
    
    procedure SetValue(const Section, Key: string; const Value: Variant);
    procedure Save;
    
    // Propiedades comunes del proyecto
    property NombreEmpresa: string index 'Empresa|Nombre' read GetConfigString;
    property NIFEmpresa: string index 'Empresa|NIF' read GetConfigString;
    property RutaDatos: string index 'Rutas|Datos' read GetConfigString;
    property ModoVerifactu: Boolean index 'Verifactu|Activo' read GetConfigBool;
  end;

// Uso
var
  NIF: string;
begin
  NIF := TConfigManager.GetInstance.NIFEmpresa;
  // o
  NIF := TConfigManager.GetInstance.GetString('Empresa', 'NIF', '');
end;
```

---

## Métricas de Éxito

- [ ] Código compila sin errores en Delphi 11+
- [ ] Sigue convenciones estándar de Delphi
- [ ] Usa patrones apropiados para el contexto
- [ ] Maneja errores correctamente
- [ ] Es eficiente y mantenible
- [ ] Compatible con proyecto Verifactu existente

## Notas

- Esta skill se activa automáticamente al trabajar con proyectos Delphi
- Se puede desactivar con: `@skill:delphi-expert-context:off`
- Combina bien con: analyze-delphi-unit, generate-boilerplate, extract-method
- Conocimiento específico para ARAINFORIA/FACARAVF incluido

## Changelog

### v2.0.0 (2026-01-07)

- Ampliación masiva del conocimiento experto
- Añadidos patrones: Command, Strategy, Builder, Decorator, State, Unit of Work
- Componentes de terceros: DevExpress, FastReport, Indy, DelphiZXingQRCode, TMS
- Implementación completa de Verifactu: hash encadenado, XML AEAT, validación NIF
- Guía de migración Delphi 10.x a 11/12/13
- Migración BDE a FireDAC
- Utilidades y herramientas comunes

### v1.0.0 (2026-01-07)

- Creación inicial
- Base de conocimiento Delphi 11/12/13
- Patrones de diseño básicos
- VCL best practices
- BDE/Paradox y FireDAC
- Técnicas avanzadas

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
