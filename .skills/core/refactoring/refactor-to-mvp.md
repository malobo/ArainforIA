---
name: refactor-to-mvp
version: 1.0.0
category: core/refactoring
tags: [mvp, arquitectura, separar, desacoplar, patron]
author: ARAINFORIA
created: 2026-01-08
complexity: 7
triggers:
  - "separar logica"
  - "mvp pattern"
  - "desacoplar form"
  - "arquitectura limpia"
---

# Refactorizar a MVP

## Descripción

Guía para separar lógica de negocio de UI usando patrón Model-View-Presenter.

## Arquitectura MVP

```text
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│    VIEW     │────▶│  PRESENTER  │────▶│    MODEL    │
│  (Form)     │◀────│  (Logic)    │◀────│   (Data)    │
└─────────────┘     └─────────────┘     └─────────────┘
```

## Paso 1: Definir Interfaces

```pascal
// Vista (Form)
type
  IClienteView = interface
    procedure MostrarCliente(const C: TClienteDTO);
    procedure MostrarError(const Msg: string);
    procedure MostrarExito(const Msg: string);
    function ObtenerDatosFormulario: TClienteDTO;
  end;

// Presenter
type
  IClientePresenter = interface
    procedure CargarCliente(Id: Integer);
    procedure GuardarCliente;
    procedure EliminarCliente;
  end;
```

## Paso 2: Implementar Model

```pascal
type
  TClienteModel = class
  public
    function ObtenerPorId(Id: Integer): TClienteDTO;
    procedure Guardar(const C: TClienteDTO);
    procedure Eliminar(Id: Integer);
    function Validar(const C: TClienteDTO): TStringList;
  end;
```

## Paso 3: Implementar Presenter

```pascal
type
  TClientePresenter = class(TInterfacedObject, IClientePresenter)
  private
    FView: IClienteView;
    FModel: TClienteModel;
  public
    constructor Create(AView: IClienteView);
    destructor Destroy; override;
    
    procedure CargarCliente(Id: Integer);
    procedure GuardarCliente;
  end;

procedure TClientePresenter.GuardarCliente;
var
  Cliente: TClienteDTO;
  Errores: TStringList;
begin
  Cliente := FView.ObtenerDatosFormulario;
  Errores := FModel.Validar(Cliente);
  try
    if Errores.Count > 0 then
      FView.MostrarError(Errores.Text)
    else
    begin
      FModel.Guardar(Cliente);
      FView.MostrarExito('Cliente guardado');
    end;
  finally
    Errores.Free;
  end;
end;
```

## Paso 4: Adaptar Form

```pascal
type
  TfrmCliente = class(TForm, IClienteView)
  private
    FPresenter: IClientePresenter;
    // IClienteView
    procedure MostrarCliente(const C: TClienteDTO);
    procedure MostrarError(const Msg: string);
    function ObtenerDatosFormulario: TClienteDTO;
  public
    procedure AfterConstruction; override;
  end;

procedure TfrmCliente.AfterConstruction;
begin
  inherited;
  FPresenter := TClientePresenter.Create(Self);
end;

procedure TfrmCliente.btnGuardarClick(Sender: TObject);
begin
  FPresenter.GuardarCliente; // Delegar al Presenter
end;
```

## Beneficios

- Lógica testeable sin UI
- Forms más simples
- Reutilización de código
- Mantenimiento más fácil

---

**Estado**: stable
