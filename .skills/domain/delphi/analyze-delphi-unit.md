---
name: analyze-delphi-unit
version: 1.1.0
category: domain/delphi
tags: [delphi, analysis, code-review]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 4
estimated_tokens: 800-1200
---

# Analizar Unidad Delphi

## Descripción

Analiza una unidad (.pas) de Delphi para identificar estructura, dependencias, complejidad y posibles mejoras.

## Objetivo

Proporcionar un análisis completo de una unidad Delphi incluyendo su arquitectura, dependencias, métricas de calidad y recomendaciones de mejora.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as Senior Delphi Architect. Parse .pas file. Extract classes, methods, uses. Identify circular deps and complexity > 10.
> **OUTPUT_FORMAT**: Markdown Report with sections: Metrics, Structure, Dependencies, Risks.
> **TOKEN_STRATEGY**: Summarize implementations. Focus on Interface.

## Inputs

- **unit_path** (string): Ruta absoluta al archivo .pas a analizar
- **depth** (string, opcional): Nivel de análisis [basic|detailed|deep] (default: detailed)
- **focus** (array, opcional): Aspectos específicos a analizar [structure|dependencies|quality|security]

## Outputs

- **analysis_report** (markdown): Reporte completo del análisis
- **metrics** (json): Métricas cuantitativas del código
- **recommendations** (list): Lista priorizada de recomendaciones

## Tool Mapping

- **Lectura**: `read_file` (target: `{{unit_path}}`)
- **Búsqueda (Referencias)**: `search_file_content` (pattern: class name)
- **Validación Sintaxis**: `run_shell_command` (optional: dcc32 -syntax-check)

## Precondiciones

- El archivo .pas debe existir y ser accesible
- El archivo debe ser sintácticamente válido (compilable)

## Postcondiciones

- Se genera un reporte de análisis en markdown
- Se identifican áreas de mejora priorizadas
- Se documentan dependencias y acoplamiento

## Procedimiento

### Paso 1: Lectura y Parsing Inicial

Leer el archivo y extraer:

- Cláusula `unit`
- Sección `interface`
- Sección `implementation`
- Cláusulas `uses`

**Validación**: Verificar que la estructura básica de la unidad es válida

### Paso 2: Análisis de Estructura

Identificar:

- Clases y sus jerarquías
- Procedimientos y funciones
- Tipos definidos
- Constantes y variables globales

**Validación**: Todas las declaraciones están categorizadas

### Paso 3: Análisis de Dependencias

Extraer de las cláusulas `uses`:

- Dependencias de interface (públicas)
- Dependencias de implementation (privadas)
- Dependencias circulares potenciales
- Dependencias de terceros vs. RTL/VCL

**Validación**: Todas las dependencias están mapeadas

### Paso 4: Métricas de Calidad

Calcular:

- Líneas de código (LOC)
- Complejidad ciclomática aproximada
- Ratio comentarios/código
- Número de clases y métodos
- Profundidad de herencia

**Validación**: Todas las métricas están calculadas

### Paso 5: Identificación de Patrones

Detectar:

- Patrones de diseño utilizados
- Anti-patrones comunes
- Código duplicado potencial
- Violaciones de principios SOLID

**Validación**: Patrones documentados con ejemplos

### Paso 6: Generación de Reporte

Compilar toda la información en un reporte estructurado con:

- Resumen ejecutivo
- Métricas detalladas
- Dependencias visualizadas
- Recomendaciones priorizadas

**Validación**: Reporte completo y legible

## Ejemplos de Uso

### Ejemplo 1: Análisis Básico

**Contexto**: Revisión rápida de una unidad

**Input**:

```
unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/uVerifactu.pas"
depth: "basic"
```

**Output Esperado**:

```markdown
# Análisis de uVerifactu.pas

## Resumen
- **LOC**: 450
- **Clases**: 2
- **Funciones/Procedimientos**: 15
- **Dependencias**: 8

## Dependencias Principales
- System.SysUtils
- System.Hash
- Vcl.Dialogs

## Recomendaciones Top 3
1. Considerar extraer lógica de hashing a clase separada
2. Reducir dependencias de VCL en lógica de negocio
3. Añadir más comentarios de documentación
```

### Ejemplo 2: Análisis Profundo

**Contexto**: Refactorización mayor planificada

**Input**:

```
unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/Facturas.pas"
depth: "deep"
focus: ["structure", "quality", "dependencies"]
```

**Output Esperado**:

```markdown
# Análisis Profundo de Facturas.pas

## Métricas Detalladas
- **LOC Total**: 1250
- **LOC Código**: 980
- **LOC Comentarios**: 120
- **LOC Vacías**: 150
- **Complejidad Ciclomática Promedio**: 8.5
- **Clases**: 5
- **Métodos Públicos**: 45
- **Métodos Privados**: 30

## Estructura de Clases
[Diagrama de jerarquía]

## Análisis de Dependencias
[Grafo de dependencias]

## Problemas Detectados
1. **Alta complejidad** en método `CalcularTotales` (CC: 25)
2. **Acoplamiento fuerte** con módulo de base de datos
3. **Código duplicado** en validaciones de campos

## Recomendaciones Detalladas
[Lista priorizada con ejemplos de código]
```

## Manejo de Errores

### Error 1: Archivo no encontrado

**Síntoma**: No se puede leer el archivo .pas
**Causa**: Ruta incorrecta o archivo no existe
**Solución**: Verificar la ruta y existencia del archivo

### Error 2: Sintaxis inválida

**Síntoma**: No se puede parsear la estructura
**Causa**: Archivo con errores de compilación
**Solución**: Compilar primero el archivo para identificar errores de sintaxis

### Error 3: Archivo muy grande

**Síntoma**: Análisis excede límite de tokens
**Causa**: Archivo con más de 5000 LOC
**Solución**: Usar depth="basic" o dividir el análisis en secciones

## Optimizaciones

### Optimización de Tokens

- Para archivos grandes, analizar solo secciones específicas
- Usar sampling para métricas en lugar de análisis completo
- Cachear resultados de análisis de dependencias comunes

### Optimización de Rendimiento

- Parsear solo las secciones necesarias según el focus
- Reutilizar análisis previos si el archivo no ha cambiado
- Paralelizar análisis de múltiples archivos

## Dependencias

### Skills Requeridas

- `@skill:core/analysis/parse-code` - Para parsing básico

### Herramientas Externas

- Ninguna (análisis estático puro)

## Variantes

### Variante 1: Análisis de Formulario (.dfm)

Adaptar para analizar también el archivo .dfm asociado y correlacionar componentes visuales con código

### Variante 2: Análisis Comparativo

Comparar dos versiones de la misma unidad para identificar cambios y su impacto

## Métricas de Éxito

- [ ] Todas las secciones de la unidad están identificadas
- [ ] Dependencias completas mapeadas
- [ ] Al menos 3 recomendaciones concretas generadas
- [ ] Métricas de calidad calculadas
- [ ] Reporte legible y accionable

## Notas

- El análisis es estático, no ejecuta el código
- La complejidad ciclomática es aproximada (análisis heurístico)
- Para análisis de rendimiento real, usar profiling tools

## Referencias

- [Delphi Coding Standards](https://docwiki.embarcadero.com/RADStudio/en/Delphi_Coding_Standards)
- [Object Pascal Style Guide](https://edn.embarcadero.com/article/10280)

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial de la skill
- Soporte para análisis básico, detallado y profundo
- Métricas de calidad integradas

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
