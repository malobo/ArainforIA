---
id: skill-validate-dfm-integrity
name: Validar Integridad DFM
version: 1.0.0
category: core/analysis
priority: medium
last_updated: 2026-01-08
triggers:
  - "dfm roto"
  - "componente no encontrado"
  - "error form"
  - "validar dfm"
---

# 🕵️ Validar Integridad DFM

<context>
Los archivos `.dfm` (Delphi Form) pueden corromperse o desincronizarse del `.pas` (ej: borrar declaración en PAS pero dejar objeto en DFM). Esta skill guía en la detección y reparación manual de estos problemas.
</context>

<instruction>
Pasos para validar/reparar un DFM:

1. **Verificación de Sintaxis**:
    * Asegurar que inicia con `object` y termina con `end`.
    * Buscar propiedades binarias corruptas (bloques ilegibles).
2. **Cruce con PAS**:
    * Para cada objeto en DFM (`object Button1: TButton`), verificar que exista `Button1: TButton;` en la clase del formulario en el `.pas`.
    * Si falta en el `.pas`, Delphi dará error "Field not found". Solución: Borrar del DFM o re-declarar en PAS.
3. **Referencias a Componentes**:
    * Verificar que `Library Path` incluya los paquetes de terceros usados.
    * Si es un error "Class TFakeComp not found", abrir DFM en editor de texto y cambiar `TFakeComp` por el correcto o borrar el bloque.

</instruction>

<examples>
User: "Tengo un error 'Property Icon does not exist' al abrir un form antiguo"
Agent: "El DFM contiene una propiedad 'Icon' que ya no existe en la versión actual del componente o VCL.
Solución:
1. Abre el archivo `.dfm` con un editor de texto (Notepad++ o VS Code).
2. Busca la línea `Icon = { ... }`.
3. Borra esa línea completa.
4. Guarda e intenta abrir el form en Delphi de nuevo."
</examples>
