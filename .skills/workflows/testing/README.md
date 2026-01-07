# Workflows de Testing

Este directorio contiene los flujos de trabajo relacionados con la ejecución de pruebas automatizadas, validación de calidad y aseguramiento del funcionamiento correcto del sistema.

## Workflows Disponibles

### [run-test-suite](./run-test-suite.md) 🔨 (v0.1.0)

Ejecuta la suite de pruebas unitarias del proyecto. Actualmente en fase de planificación y desarrollo inicial.

## Objetivos

- Estandarizar la ejecución de pruebas (DUnit / DUnitX).
- Integrar la generación de reportes de cobertura.
- Validar builds antes del despliegue.

## Estructura Recomendada

Para que estos workflows funcionen, se recomienda la siguiente estructura en el proyecto Delphi:

- `/Tests`: Directorio raíz de pruebas.
- `/Tests/Lib`: Librerías de testing (DUnit/DUnitX).
- `TestProject.dpr`: Proyecto de consola que ejecuta todos los tests.
