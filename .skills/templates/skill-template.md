---
name: [nombre-de-la-skill]
version: 1.0.0
category: [core|domain|workflows]/[subcategoria]
tags: [tag1, tag2, tag3]
author: [Nombre del autor]
created: [YYYY-MM-DD]
updated: [YYYY-MM-DD]
complexity: [1-10]
estimated_tokens: [rango estimado]
---

# [Nombre de la Skill]

## Descripción

[Descripción clara y concisa de qué hace la skill en 1-2 líneas]

## Objetivo

[Explicación detallada del problema que resuelve o la capacidad que proporciona]

## AI Context

> **SYSTEM_INSTRUCTION**: [Instrucción concisa y técnica para el modelo de IA. Ej: Act as a Delphi Expert.]
> **OUTPUT_FORMAT**: [Formato de salida esperado. Ej: Markdown Report + JSON Metrics]
> **TOKEN_STRATEGY**: [Estrategia de tokens. Ej: High compression, concise output]

## Inputs

- **input1** (tipo): Descripción del input requerido
- **input2** (tipo, opcional): Descripción del input opcional con valor por defecto

## Outputs

- **output1** (tipo): Descripción del output principal
- **output2** (tipo): Descripción del output secundario

## Tool Mapping

- **[Acción Abstracta]**: `[nombre_herramienta_real]` (Ej: **Lectura**: `read_file`)
- **[Acción Abstracta]**: `[nombre_herramienta_real]` (Ej: **Comandos**: `run_shell_command`)

## Precondiciones

- [Condición que debe cumplirse antes de ejecutar la skill]
- [Archivos, permisos o estado requerido]

## Postcondiciones

- [Estado esperado del sistema después de la ejecución]
- [Archivos creados, modificados o eliminados]

## Procedimiento

### Paso 1: [Nombre descriptivo del paso]

[Descripción detallada de qué hacer en este paso]

```[lenguaje]
// Código de ejemplo o pseudocódigo si aplica
```

**Validación**: [Cómo verificar que este paso se completó correctamente]

### Paso 2: [Nombre descriptivo del paso]

[Descripción detallada de qué hacer en este paso]

**Validación**: [Cómo verificar que este paso se completó correctamente]

### Paso N: [Finalización]

[Descripción del paso final y verificación de resultados]

## Ejemplos de Uso

### Ejemplo 1: [Caso de uso común]

**Contexto**: [Situación típica donde se usa esta skill]

**Input**:

```
[ejemplo concreto de input]
```

**Ejecución**:

```
[comandos o pasos específicos]
```

**Output Esperado**:

```
[resultado esperado]
```

### Ejemplo 2: [Caso de uso avanzado]

**Contexto**: [Situación más compleja]

**Input**:

```
[ejemplo concreto de input]
```

**Output Esperado**:

```
[resultado esperado]
```

## Manejo de Errores

### Error 1: [Nombre del error]

**Síntoma**: [Cómo se manifiesta el error]
**Causa**: [Por qué ocurre]
**Solución**: [Cómo resolverlo]

### Error 2: [Nombre del error]

**Síntoma**: [Cómo se manifiesta el error]
**Causa**: [Por qué ocurre]
**Solución**: [Cómo resolverlo]

## Optimizaciones

### Optimización de Tokens

- [Técnica 1 para reducir uso de tokens]
- [Técnica 2 para reducir uso de tokens]

### Optimización de Rendimiento

- [Mejora 1 de rendimiento]
- [Mejora 2 de rendimiento]

### Optimización de Calidad

- [Mejora 1 de calidad del output]
- [Mejora 2 de calidad del output]

## Dependencias

### Skills Requeridas

- `@skill:categoria/skill-requerida` - Descripción de por qué se necesita

### Herramientas Externas

- **Herramienta 1** (versión): Descripción de su uso
- **Herramienta 2** (versión): Descripción de su uso

### Archivos de Configuración

- `path/to/config.file` - Descripción de la configuración necesaria

## Variantes

### Variante 1: [Nombre de la variante]

[Descripción de cómo adaptar la skill para este caso específico]

### Variante 2: [Nombre de la variante]

[Descripción de cómo adaptar la skill para este caso específico]

## Métricas de Éxito

- [ ] [Criterio de éxito 1]
- [ ] [Criterio de éxito 2]
- [ ] [Criterio de éxito 3]

## Notas

- [Nota importante 1]
- [Consideración especial 2]
- [Limitación conocida 3]

## Referencias

- [Enlace a documentación relevante]
- [Enlace a especificación técnica]
- [Enlace a ejemplos adicionales]

## Changelog

### v1.0.0 ([YYYY-MM-DD])

- Creación inicial de la skill
- [Característica inicial 1]
- [Característica inicial 2]

---

**Última revisión**: [YYYY-MM-DD]  
**Próxima revisión**: [YYYY-MM-DD]  
**Estado**: [draft|stable|deprecated]
