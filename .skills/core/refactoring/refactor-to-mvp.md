---
id: skill-refactor-to-mvp
name: Refactorizar a MVP (Model-View-Presenter)
version: 1.0.0
category: core/refactoring
priority: high
last_updated: 2026-01-08
triggers:
  - "separar logica"
  - "mvp pattern"
  - "desacoplar form"
  - "limpiar formulario"
---

# 🏗️ Refactorizar a MVP (Model-View-Presenter)

<context>
El patrón MVP es ideal para modernizar aplicaciones Delphi VCL heredadas. Permite sacar la lógica de negocio del "Code-Behind" del Form (`.pas`) y moverla a una clase `Presenter`, dejando el Form (`View`) solo para entrada/salida.
</context>

<instruction>
Pasos para refactorizar un Form a MVP:

1. **Definir la Interfaz de la Vista (`IView`)**:
    * Crear una `interface` que declare lo que el Presenter necesita del Form (Getters/Setters de datos, métodos para mostrar/ocultar controles).
    * Hacer que el Form implemente esta interfaz.
2. **Crear el Presenter**:
    * Clase pura (sin dependencia de `Vcl.Forms`).
    * Constructor recibe `IView`.
    * Métodos públicos para acciones (`Guardar`, `Buscar`).
3. **Vincular**:
    * En `FormCreate`, instanciar el Presenter.
    * En eventos de botones (`OnClick`), llamar al métdo del Presenter.

</instruction>

<examples>
User: "Saca la lógica de guardar del FormFactura a un Presenter"
Agent:
```delphi
// 1. Interfaz (Contrato)
type
  IFacturaView = interface
    function GetClienteID: Integer;
    function GetTotal: Double;
    procedure ShowError(const Msg: string);
  end;

// 2. Presenter (Lógica)
type
  TFacturaPresenter = class
  private
    FView: IFacturaView;
  public
    constructor Create(AView: IFacturaView);
    procedure GuardarFactura;
  end;

procedure TFacturaPresenter.GuardarFactura;
begin
  if FView.GetTotal <= 0 then
  begin
    FView.ShowError('El total debe ser mayor a 0'); // Lógica de UI abstracta
    Exit;
  end;
  // ... lógica de guardado en BD ...
end;

// 3. Form (Implementación)
type
  TFormFactura = class(TForm, IFacturaView)
    procedure BtnGuardarClick(Sender: TObject);
  // ...
  end;

procedure TFormFactura.BtnGuardarClick(Sender: TObject);
begin
  FPresenter.GuardarFactura;
end;

```
</examples>
