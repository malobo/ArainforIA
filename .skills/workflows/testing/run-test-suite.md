---
name: run-test-suite
version: 0.1.0
category: workflows/testing
complexity: 5
tokens_estimate: 800-1200
tags: [testing, dunit, dunitx, automation, quality]
status: planning
---

# 🔨 Run Test Suite (WIP)

## Descripción

Este workflow orquestará la ejecución de la suite de pruebas unitarias del proyecto Delphi. Se encargará de compilar el proyecto de tests, ejecutarlo y parsear los resultados.

> [!NOTE]
> Esta skill está actualmente en fase de **Planificación**. La implementación completa llegará en futuras versiones (v1.7.0+).

## Entradas Planificadas

- `test_project_path` (string): Ruta al archivo .dpr del proyecto de tests.
- `framework` (string): Framework utilizado (DUnit / DUnitX).
- `output_format` (string): Formato de resultados (XML, Text, Console).

## Flujo de Trabajo Propuesto

1. **Validación**: Verificar que el proyecto de tests existe.
2. **Compilación**: Usar MSBuild/DCC32 para compilar el ejecutable de tests.
3. **Ejecución**: Correr el ejecutable en modo consola.
4. **Análisis**: Capturar el código de salida y el reporte XML (si aplica).
5. **Reporte**: Mostrar resumen de Tests Pasados / Fallados / Ignorados.

## Ejemplo de Invocación Futura

```yaml
@skill:workflows/testing/run-test-suite
test_project_path: "Tests/AracostesTests.dpr"
framework: "DUnitX"
```
