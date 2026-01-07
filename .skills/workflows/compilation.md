---
id: skill-workflow-compilation
name: Workflow de Compilación
version: 2.0
category: workflow
priority: medium
last_updated: 2026-01-06
triggers:
  - "compila esto"
  - "build project"
  - "error dcc32"
  - "make"
---

# Workflow de Compilación

<context>
Proceso estándar de compilación para proyectos Delphi con `dcc32.exe`.
Uso de flag `-B` para Build completo y `-Q` para Quiet.
</context>

<instruction>
1. **Localiza DPR**: Siempre encuentra el archivo `.dpr` antes de intentar compilar.
2. **Limpia DCUs**: Ante errores de "Internal Error" o corrupción, borra `*.dcu`.
3. **Verifica Salida**: No asumas éxito. Lee el output buscando "Fatal:" o "Error:".
</instruction>

<examples>
User: "Compila TPVARA"
Assistant:
```shell
dcc32.exe -B C:\Arainfor\TPVARA\TPVARA.dpr -Q
```
</examples>
