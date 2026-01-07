---
name: generate-crud-forms
version: 1.0.0
category: domain/database
complexity: 6
tokens_estimate: 1000-1500
tags: [crud, forms, database, automation, paradox, ui]
requires: []
dependencies: []
---

# 📋 Generate CRUD Forms

## Descripción

Genera formularios CRUD (Create, Read, Update, Delete) completos para tablas de base de datos, incluyendo el formulario visual, DataModule y lógica de negocio.

## Cuándo Usar

- Al crear mantenimientos de tablas maestras
- Para generar ABM rápidamente
- Al estandarizar formularios de edición
- Para prototipar interfaces de datos

## Inputs

| Parámetro | Tipo | Requerido | Descripción |
| --------- | ---- | --------- | ----------- |
| `table_name` | string | ✅ | Nombre de la tabla |
| `table_structure` | object | ✅ | Estructura de campos de la tabla |
| `form_type` | string | ❌ | `grid`, `detail`, `master-detail` (default: `grid`) |
| `include_search` | boolean | ❌ | Incluir búsqueda (default: true) |
| `include_validation` | boolean | ❌ | Validaciones automáticas (default: true) |
| `include_print` | boolean | ❌ | Botón de impresión (default: false) |
| `database_type` | string | ❌ | `paradox`, `firedac`, `ado` (default: `paradox`) |

## Outputs

| Output | Tipo | Descripción |
| ------ | ---- | ----------- |
| `form_unit` | string | Código del formulario (.pas) |
| `form_dfm` | string | Diseño del formulario (.dfm) |
| `datamodule_unit` | string | Código del DataModule |
| `datamodule_dfm` | string | Diseño del DataModule |
| `usage_instructions` | string | Instrucciones de uso |

## Estructura Generada

```text
📁 Salida
├── uFrm{Tabla}.pas          # Formulario principal
├── uFrm{Tabla}.dfm          # Diseño visual
├── uDm{Tabla}.pas           # DataModule
├── uDm{Tabla}.dfm           # Conexiones de datos
└── uBO{Tabla}.pas           # Business Object (opcional)
```

## Proceso de Generación

### Paso 1: Análisis de Estructura

```text
ANALIZAR table_structure:
├── Identificar campo clave (PK)
├── Mapear tipos de datos a controles
│   ├── String → TDBEdit
│   ├── Integer → TDBEdit con máscara
│   ├── Float → TDBEdit con formato
│   ├── Date → TDBDateTimePicker
│   ├── Boolean → TDBCheckBox
│   ├── Memo → TDBMemo
│   └── Lookup → TDBLookupComboBox
├── Detectar campos requeridos (NOT NULL)
└── Identificar relaciones (FK)
```

### Paso 2: Generación del DataModule

```pascal
unit uDm{Tabla};

interface

uses
  System.SysUtils, System.Classes, Data.DB, 
  {$IFDEF PARADOX}
  Bde.DBTables;
  {$ELSE}
  FireDAC.Comp.Client;
  {$ENDIF}

type
  TDm{Tabla} = class(TDataModule)
    {$IFDEF PARADOX}
    tbl{Tabla}: TTable;
    {$ELSE}
    qry{Tabla}: TFDQuery;
    {$ENDIF}
    ds{Tabla}: TDataSource;
    
    // Campos
    {CamposGenerados}
    
    procedure DataModuleCreate(Sender: TObject);
    procedure tbl{Tabla}BeforePost(DataSet: TDataSet);
    procedure tbl{Tabla}BeforeDelete(DataSet: TDataSet);
  private
    procedure ValidarDatos;
  public
    procedure Abrir;
    procedure Cerrar;
    procedure Nuevo;
    procedure Guardar;
    procedure Cancelar;
    procedure Eliminar;
    procedure Buscar(const ACampo, AValor: string);
    procedure Refrescar;
    
    function EstaEditando: Boolean;
  end;

var
  Dm{Tabla}: TDm{Tabla};

implementation

{$R *.dfm}

uses
  Vcl.Dialogs;

procedure TDm{Tabla}.DataModuleCreate(Sender: TObject);
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.DatabaseName := 'DATOS';
  tbl{Tabla}.TableName := '{TABLA}';
  {$ENDIF}
end;

procedure TDm{Tabla}.Abrir;
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.Open;
  {$ELSE}
  qry{Tabla}.Open;
  {$ENDIF}
end;

procedure TDm{Tabla}.Cerrar;
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.Close;
  {$ELSE}
  qry{Tabla}.Close;
  {$ENDIF}
end;

procedure TDm{Tabla}.Nuevo;
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.Append;
  {$ELSE}
  qry{Tabla}.Append;
  {$ENDIF}
end;

procedure TDm{Tabla}.Guardar;
begin
  ValidarDatos;
  {$IFDEF PARADOX}
  tbl{Tabla}.Post;
  {$ELSE}
  qry{Tabla}.Post;
  {$ENDIF}
end;

procedure TDm{Tabla}.Cancelar;
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.Cancel;
  {$ELSE}
  qry{Tabla}.Cancel;
  {$ENDIF}
end;

procedure TDm{Tabla}.Eliminar;
begin
  if MessageDlg('¿Está seguro de eliminar este registro?', 
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    {$IFDEF PARADOX}
    tbl{Tabla}.Delete;
    {$ELSE}
    qry{Tabla}.Delete;
    {$ENDIF}
  end;
end;

procedure TDm{Tabla}.Buscar(const ACampo, AValor: string);
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.Filtered := False;
  if AValor <> '' then
  begin
    tbl{Tabla}.Filter := Format('%s = ''%s*''', [ACampo, AValor]);
    tbl{Tabla}.Filtered := True;
  end;
  {$ENDIF}
end;

procedure TDm{Tabla}.Refrescar;
begin
  {$IFDEF PARADOX}
  tbl{Tabla}.Refresh;
  {$ELSE}
  qry{Tabla}.Refresh;
  {$ENDIF}
end;

function TDm{Tabla}.EstaEditando: Boolean;
begin
  {$IFDEF PARADOX}
  Result := tbl{Tabla}.State in [dsEdit, dsInsert];
  {$ELSE}
  Result := qry{Tabla}.State in [dsEdit, dsInsert];
  {$ENDIF}
end;

procedure TDm{Tabla}.ValidarDatos;
begin
  // Validaciones generadas aquí
  {ValidacionesGeneradas}
end;

procedure TDm{Tabla}.tbl{Tabla}BeforePost(DataSet: TDataSet);
begin
  ValidarDatos;
end;

procedure TDm{Tabla}.tbl{Tabla}BeforeDelete(DataSet: TDataSet);
begin
  // Validar si se puede eliminar (integridad referencial)
end;

end.
```

### Paso 3: Generación del Formulario (Grid)

```pascal
unit uFrm{Tabla};

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, 
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.Grids, Vcl.DBGrids,
  Data.DB, Vcl.DBCtrls, uDm{Tabla};

type
  TFrm{Tabla} = class(TForm)
    pnlTop: TPanel;
    pnlBottom: TPanel;
    pnlClient: TPanel;
    
    // Búsqueda
    lblBuscar: TLabel;
    edtBuscar: TEdit;
    cmbCampoBusqueda: TComboBox;
    btnBuscar: TSpeedButton;
    btnLimpiar: TSpeedButton;
    
    // Grid
    DBGrid1: TDBGrid;
    
    // Navegación
    DBNavigator1: TDBNavigator;
    
    // Acciones
    btnNuevo: TBitBtn;
    btnEditar: TBitBtn;
    btnEliminar: TBitBtn;
    btnGuardar: TBitBtn;
    btnCancelar: TBitBtn;
    btnRefrescar: TBitBtn;
    btnCerrar: TBitBtn;
    
    // Barra de estado
    StatusBar1: TStatusBar;
    
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    
    procedure btnNuevoClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnEliminarClick(Sender: TObject);
    procedure btnGuardarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure btnRefrescarClick(Sender: TObject);
    procedure btnCerrarClick(Sender: TObject);
    
    procedure btnBuscarClick(Sender: TObject);
    procedure btnLimpiarClick(Sender: TObject);
    procedure edtBuscarKeyPress(Sender: TObject; var Key: Char);
    
    procedure DBGrid1DblClick(Sender: TObject);
  private
    FDataModule: TDm{Tabla};
    procedure ActualizarBotones;
    procedure ActualizarStatusBar;
    procedure ConfigurarGrid;
  public
    class procedure Mostrar;
  end;

var
  Frm{Tabla}: TFrm{Tabla};

implementation

{$R *.dfm}

class procedure TFrm{Tabla}.Mostrar;
begin
  if not Assigned(Frm{Tabla}) then
    Frm{Tabla} := TFrm{Tabla}.Create(Application);
  Frm{Tabla}.Show;
end;

procedure TFrm{Tabla}.FormCreate(Sender: TObject);
begin
  FDataModule := TDm{Tabla}.Create(Self);
  
  // Configurar campos de búsqueda
  cmbCampoBusqueda.Items.Clear;
  {CamposBusquedaGenerados}
  cmbCampoBusqueda.ItemIndex := 0;
  
  ConfigurarGrid;
  FDataModule.Abrir;
  ActualizarBotones;
  ActualizarStatusBar;
end;

procedure TFrm{Tabla}.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FDataModule.EstaEditando then
  begin
    case MessageDlg('Hay cambios sin guardar. ¿Qué desea hacer?',
      mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes: FDataModule.Guardar;
      mrNo: FDataModule.Cancelar;
      mrCancel: Action := caNone;
    end;
  end;
  
  if Action <> caNone then
  begin
    FDataModule.Cerrar;
    Action := caFree;
    Frm{Tabla} := nil;
  end;
end;

procedure TFrm{Tabla}.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: btnCerrarClick(nil);
    VK_F2: btnNuevoClick(nil);
    VK_F3: btnEditarClick(nil);
    VK_F4: btnEliminarClick(nil);
    VK_F5: btnRefrescarClick(nil);
    VK_F6: btnBuscarClick(nil);
    VK_RETURN:
      if ssCtrl in Shift then
        btnGuardarClick(nil);
  end;
end;

procedure TFrm{Tabla}.ConfigurarGrid;
begin
  DBGrid1.DataSource := FDataModule.ds{Tabla};
  
  // Configurar columnas
  DBGrid1.Columns.Clear;
  {ColumnasGridGeneradas}
end;

procedure TFrm{Tabla}.ActualizarBotones;
var
  Editando: Boolean;
begin
  Editando := FDataModule.EstaEditando;
  
  btnNuevo.Enabled := not Editando;
  btnEditar.Enabled := not Editando;
  btnEliminar.Enabled := not Editando;
  btnRefrescar.Enabled := not Editando;
  
  btnGuardar.Enabled := Editando;
  btnCancelar.Enabled := Editando;
  
  DBGrid1.ReadOnly := not Editando;
end;

procedure TFrm{Tabla}.ActualizarStatusBar;
begin
  StatusBar1.Panels[0].Text := Format('Registros: %d', 
    [FDataModule.tbl{Tabla}.RecordCount]);
end;

procedure TFrm{Tabla}.btnNuevoClick(Sender: TObject);
begin
  FDataModule.Nuevo;
  ActualizarBotones;
end;

procedure TFrm{Tabla}.btnEditarClick(Sender: TObject);
begin
  FDataModule.tbl{Tabla}.Edit;
  ActualizarBotones;
end;

procedure TFrm{Tabla}.btnEliminarClick(Sender: TObject);
begin
  FDataModule.Eliminar;
  ActualizarStatusBar;
end;

procedure TFrm{Tabla}.btnGuardarClick(Sender: TObject);
begin
  try
    FDataModule.Guardar;
    ActualizarBotones;
    ActualizarStatusBar;
  except
    on E: Exception do
      MessageDlg('Error al guardar: ' + E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TFrm{Tabla}.btnCancelarClick(Sender: TObject);
begin
  FDataModule.Cancelar;
  ActualizarBotones;
end;

procedure TFrm{Tabla}.btnRefrescarClick(Sender: TObject);
begin
  FDataModule.Refrescar;
  ActualizarStatusBar;
end;

procedure TFrm{Tabla}.btnCerrarClick(Sender: TObject);
begin
  Close;
end;

procedure TFrm{Tabla}.btnBuscarClick(Sender: TObject);
begin
  FDataModule.Buscar(cmbCampoBusqueda.Text, edtBuscar.Text);
  ActualizarStatusBar;
end;

procedure TFrm{Tabla}.btnLimpiarClick(Sender: TObject);
begin
  edtBuscar.Clear;
  FDataModule.Buscar('', '');
  ActualizarStatusBar;
end;

procedure TFrm{Tabla}.edtBuscarKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
  begin
    Key := #0;
    btnBuscarClick(nil);
  end;
end;

procedure TFrm{Tabla}.DBGrid1DblClick(Sender: TObject);
begin
  btnEditarClick(nil);
end;

end.
```

## Ejemplo de Uso

```yaml
@skill:domain/database/generate-crud-forms
table_name: "Clientes"
table_structure:
  - name: "IdCliente"
    type: "integer"
    primary_key: true
    auto_increment: true
  - name: "Nombre"
    type: "string"
    size: 100
    required: true
  - name: "NIF"
    type: "string"
    size: 15
    required: true
  - name: "Email"
    type: "string"
    size: 100
  - name: "Telefono"
    type: "string"
    size: 20
  - name: "FechaAlta"
    type: "date"
    default: "now"
  - name: "Activo"
    type: "boolean"
    default: true
form_type: "grid"
include_search: true
include_validation: true
database_type: "paradox"
```

## Historial de Cambios

| Versión | Fecha | Cambios |
| ------- | ----- | ------- |
| 1.0.0 | 2026-01-07 | Versión inicial |
