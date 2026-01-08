---
id: skill-optimize-vcl-form
name: Optimización de Formularios VCL
version: 1.0.0
category: domain/delphi
priority: high
last_updated: 2026-01-08
triggers:
  - "formulario lento"
  - "optimizar form"
  - "mejorar ui"
  - "flickering"
---

# ⚡ Optimización de Formularios VCL

<context>
Esta skill se enfoca en detectar cuellos de botella de rendimiento y problemas de UX en formularios VCL Delphi, aplicando técnicas como DoubleBuffering, carga diferida (Lazy Loading) y reducción de repintado.
</context>

<instruction>
Al analizar un formulario lento (`.pas` + `.dfm`):

1. **Rendering & Flickering**:
    * Activar `DoubleBuffered := True` en el Form o componentes contenedores (Panels, Grids) para evitar parpadeos.
    * Usar `BeginUpdate` y `EndUpdate` al llenar listas (TStrings, TreeViews, Grids).

2. **Eventos Críticos**:
    * Revisar `OnDrawCell` o `OnPaint`: Deben ser extremadamente rápidos. No hacer consultas SQL dentro de estos eventos.
    * Revisar `OnResize`: Evitar cálculos pesados aquí.

3. **Gestión de Recursos**:
    * Destruir objetos creados dinámicamente (`FreeAndNil`).
    * Si el form es muy pesado, considerar crearlo solo cuando se necesita (`Application.CreateForm` vs `Auto-create forms`).

4. **Acceso a Datos**:
    * Abrir Datasets (`Open`) solo cuando sean visibles.
    * Usar `DisableControls` / `EnableControls` en el Dataset durante procesos batch.
</instruction>

<examples>
User: "El formulario de Lista de Facturas tarda mucho en abrirse y parpadea"
Agent: "Para el parpadeo (flickering), añade `DBGrid1.DoubleBuffered := True;` en el `FormCreate`. Para la lentitud, veo que haces un `Query.Open` el `FormShow`. ¿Podrías moverlo o usar paginación? Además, asegúrate de usar `DisableControls` si recorres el grid."
</examples>
