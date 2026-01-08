---
name: optimize-vcl-form
version: 1.0.0
category: domain/delphi
tags: [vcl, formulario, rendimiento, optimizacion, ui]
author: ARAINFORIA
created: 2026-01-08
complexity: 5
triggers:
  - "formulario lento"
  - "optimizar form"
  - "mejorar ui"
  - "form pesado"
---

# Optimizar Formulario VCL

## Descripción

Analiza y optimiza formularios VCL para mejorar rendimiento y UX.

## Checklist de Optimización

### Carga Inicial

- [ ] Usar `BeginUpdate`/`EndUpdate` en grids y listas
- [ ] Cargar datos bajo demanda (lazy loading)
- [ ] Evitar `AutoSize` en componentes complejos
- [ ] Diferir carga de imágenes/recursos

### Memoria

- [ ] Liberar objetos en `OnDestroy`
- [ ] No crear componentes en bucles
- [ ] Usar `FreeAndNil` en lugar de `Free`

### UI/UX

- [ ] Usar `Application.ProcessMessages` con moderación
- [ ] Implementar indicadores de progreso
- [ ] Evitar más de 50 controles visibles

## Patrones de Optimización

### Lazy Loading de Datos

```pascal
procedure TForm1.FormShow(Sender: TObject);
begin
  // NO cargar todo al inicio
  // Solo cargar cuando sea necesario
end;

procedure TForm1.TabSheet2Show(Sender: TObject);
begin
  if not FDatosTab2Cargados then
  begin
    CargarDatosTab2;
    FDatosTab2Cargados := True;
  end;
end;
```

### BeginUpdate/EndUpdate

```pascal
procedure TForm1.LlenarGrid;
begin
  DBGrid1.BeginUpdate;
  try
    Query1.DisableControls;
    try
      Query1.Open;
    finally
      Query1.EnableControls;
    end;
  finally
    DBGrid1.EndUpdate;
  end;
end;
```

### Crear Controles Dinámicamente

```pascal
// ANTES: 100 labels en diseño
// DESPUÉS: Crear bajo demanda
procedure TForm1.CrearEtiquetas(Count: Integer);
var
  I: Integer;
  Lbl: TLabel;
begin
  for I := 0 to Count - 1 do
  begin
    Lbl := TLabel.Create(Self);
    Lbl.Parent := ScrollBox1;
    Lbl.Top := I * 20;
    Lbl.Caption := 'Item ' + IntToStr(I);
  end;
end;
```

## Métricas de Rendimiento

```pascal
var
  T: TStopwatch;
begin
  T := TStopwatch.StartNew;
  // Código a medir
  T.Stop;
  ShowMessage('Tiempo: ' + IntToStr(T.ElapsedMilliseconds) + 'ms');
end;
```

---

**Estado**: stable
