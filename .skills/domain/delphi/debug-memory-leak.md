---
name: debug-memory-leak
version: 1.0.0
category: domain/delphi
complexity: 7
tokens_estimate: 1000-1500
tags: [debugging, memory, leak, performance, optimization]
requires: []
dependencies: []
---

# 🔍 Debug Memory Leak

## Descripción

Detecta, analiza y sugiere correcciones para memory leaks en código Delphi. Identifica patrones comunes de fugas de memoria y proporciona soluciones específicas.

## Cuándo Usar

- Cuando la aplicación consume cada vez más memoria
- Al recibir errores de "Out of memory"
- Durante revisión de código crítico
- Antes de liberar a producción
- Al analizar código legacy

## Inputs

| Parámetro | Tipo | Requerido | Descripción |
| --------- | ---- | --------- | ----------- |
| `source_code` | string | ✅ | Código fuente a analizar |
| `unit_path` | string | ❌ | Ruta al archivo .pas |
| `analysis_level` | string | ❌ | `quick`, `standard`, `deep` (default: `standard`) |
| `include_suggestions` | boolean | ❌ | Incluir sugerencias de fix (default: true) |

## Outputs

| Output | Tipo | Descripción |
| ------ | ---- | ----------- |
| `leaks_found` | array | Lista de memory leaks detectados |
| `severity` | string | Severidad general: `low`, `medium`, `high`, `critical` |
| `fixes` | array | Correcciones sugeridas |
| `best_practices` | array | Mejores prácticas recomendadas |

## Patrones de Memory Leak Detectados

### 1. Create sin Free

```pascal
// ❌ LEAK: Objeto creado pero nunca liberado
procedure TForm1.Button1Click(Sender: TObject);
var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  Lista.Add('Item 1');
  // Falta: Lista.Free
end;

// ✅ CORRECTO: Con try-finally
procedure TForm1.Button1Click(Sender: TObject);
var
  Lista: TStringList;
begin
  Lista := TStringList.Create;
  try
    Lista.Add('Item 1');
  finally
    Lista.Free;
  end;
end;
```

### 2. Exception antes del Free

```pascal
// ❌ LEAK: Si hay excepción, no se libera
procedure ProcesarDatos;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  HacerAlgoQuePuedeFallar; // Si falla, Stream queda en memoria
  Stream.Free;
end;

// ✅ CORRECTO
procedure ProcesarDatos;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    HacerAlgoQuePuedeFallar;
  finally
    Stream.Free;
  end;
end;
```

### 3. Objetos en Listas sin OwnsObjects

```pascal
// ❌ LEAK: Lista no libera los objetos
procedure CrearItems;
var
  Lista: TObjectList<TCliente>;
begin
  Lista := TObjectList<TCliente>.Create(False); // OwnsObjects = False
  Lista.Add(TCliente.Create);
  Lista.Add(TCliente.Create);
  Lista.Free; // Los TCliente quedan huérfanos
end;

// ✅ CORRECTO
procedure CrearItems;
var
  Lista: TObjectList<TCliente>;
begin
  Lista := TObjectList<TCliente>.Create(True); // OwnsObjects = True
  Lista.Add(TCliente.Create);
  Lista.Add(TCliente.Create);
  Lista.Free; // TCliente son liberados automáticamente
end;
```

### 4. Referencias Circulares con Interfaces

```pascal
// ❌ LEAK: Referencias circulares impiden liberación
type
  IParent = interface;
  IChild = interface;

  IParent = interface
    procedure SetChild(AChild: IChild);
  end;

  IChild = interface
    procedure SetParent(AParent: IParent); // Referencia fuerte al padre
  end;

// ✅ CORRECTO: Usar referencia débil [weak]
type
  TChild = class(TInterfacedObject, IChild)
  private
    [weak] FParent: IParent; // Referencia débil
  public
    procedure SetParent(AParent: IParent);
  end;
```

### 5. Event Handlers no Desconectados

```pascal
// ❌ LEAK: Handler conectado mantiene referencia
procedure TForm1.FormCreate(Sender: TObject);
begin
  ObjetoGlobal.OnChange := Self.HandleChange;
end;
// Si Form1 se destruye pero ObjetoGlobal sigue vivo,
// la referencia al handler mantiene memoria

// ✅ CORRECTO: Desconectar en destrucción
procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(ObjetoGlobal) then
    ObjetoGlobal.OnChange := nil;
end;
```

### 6. Strings y Arrays Dinámicos en Records

```pascal
// ❌ POTENCIAL LEAK: Records con strings no finalizados
type
  TDatos = record
    Nombre: string;
    Valores: TArray<Integer>;
  end;

procedure Procesar;
var
  Datos: ^TDatos;
begin
  New(Datos);
  Datos.Nombre := 'Test';
  SetLength(Datos.Valores, 100);
  Dispose(Datos); // OK en Delphi moderno, pero cuidado con FreeMem
end;

// ⚠️ NUNCA usar FreeMem con records que tienen strings
// FreeMem(Datos); // ¡LEAK! Strings no se liberan
```

### 7. Componentes sin Owner

```pascal
// ❌ LEAK: Componente sin owner debe liberarse manualmente
procedure TForm1.CrearPanelDinamico;
begin
  FPanel := TPanel.Create(nil); // Sin owner
  FPanel.Parent := Self;
  // Si no hay Free explícito, hay leak
end;

// ✅ CORRECTO: Con owner
procedure TForm1.CrearPanelDinamico;
begin
  FPanel := TPanel.Create(Self); // Self es el owner
  FPanel.Parent := Self;
  // Se libera automáticamente con el Form
end;
```

### 8. TThread sin FreeOnTerminate

```pascal
// ❌ LEAK: Thread no se libera
procedure TForm1.IniciarProceso;
var
  Thread: TWorkerThread;
begin
  Thread := TWorkerThread.Create(True);
  Thread.Start;
  // Thread queda en memoria después de terminar
end;

// ✅ CORRECTO
procedure TForm1.IniciarProceso;
var
  Thread: TWorkerThread;
begin
  Thread := TWorkerThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;
```

## Proceso de Análisis

```text
1. ESCANEAR código fuente
   ├── Buscar: TXxx.Create
   ├── Buscar: New(), GetMem(), AllocMem()
   └── Buscar: SetLength() para arrays

2. RASTREAR cada creación
   ├── Verificar existencia de Free/Dispose/FreeMem
   ├── Verificar try-finally envolvente
   ├── Verificar ownership (componentes)
   └── Verificar OwnsObjects (listas)

3. DETECTAR patrones peligrosos
   ├── Referencias circulares
   ├── Event handlers sin desconectar
   ├── Variables globales con objetos
   └── Threads sin FreeOnTerminate

4. GENERAR reporte
   ├── Ubicación exacta (línea, método)
   ├── Tipo de leak
   ├── Severidad
   └── Sugerencia de corrección
```

## Ejemplo de Uso

```yaml
@skill:domain/delphi/debug-memory-leak
source_code: |
  procedure TForm1.ProcessData;
  var
    Data: TStringList;
    Stream: TMemoryStream;
  begin
    Data := TStringList.Create;
    Stream := TMemoryStream.Create;
    
    Data.LoadFromFile('input.txt');
    ProcessItems(Data);
    
    Stream.SaveToFile('output.dat');
  end;
analysis_level: "standard"
include_suggestions: true
```

### Output Esperado

```json
{
  "leaks_found": [
    {
      "line": 4,
      "object": "TStringList (Data)",
      "issue": "Create sin Free correspondiente",
      "severity": "high"
    },
    {
      "line": 5,
      "object": "TMemoryStream (Stream)",
      "issue": "Create sin Free correspondiente",
      "severity": "high"
    }
  ],
  "severity": "high",
  "fixes": [
    {
      "description": "Envolver en try-finally con Free",
      "code": "procedure TForm1.ProcessData;\nvar\n  Data: TStringList;\n  Stream: TMemoryStream;\nbegin\n  Data := TStringList.Create;\n  try\n    Stream := TMemoryStream.Create;\n    try\n      Data.LoadFromFile('input.txt');\n      ProcessItems(Data);\n      Stream.SaveToFile('output.dat');\n    finally\n      Stream.Free;\n    end;\n  finally\n    Data.Free;\n  end;\nend;"
    }
  ]
}
```

## Herramientas Complementarias

1. **ReportMemoryLeaksOnShutdown**

   ```pascal
   // En el archivo .dpr
   begin
     ReportMemoryLeaksOnShutdown := True;
     Application.Initialize;
     // ...
   end.
   ```

2. **FastMM4** - Detector avanzado de leaks
3. **madExcept** - Reportes detallados
4. **AQTime** - Profiler de memoria

## Historial de Cambios

| Versión | Fecha | Cambios |
| ------- | ----- | ------- |
| 1.0.0 | 2026-01-07 | Versión inicial |
