---
name: [nombre-del-workflow]
version: 1.0.0
category: workflows/[subcategoria]
tags: [tag1, tag2, tag3]
author: [Nombre del autor]
created: [YYYY-MM-DD]
updated: [YYYY-MM-DD]
complexity: [1-10]
estimated_duration: [tiempo estimado]
estimated_tokens: [rango estimado]
---

# [Nombre del Workflow]

## Descripción

[Descripción clara del workflow completo en 2-3 líneas]

## Objetivo

[Explicación detallada del resultado final que se busca alcanzar]

## Alcance

**Incluye**:

- [Qué está incluido en este workflow]
- [Otra cosa incluida]

**Excluye**:

- [Qué NO está incluido]
- [Otra cosa excluida]

## Inputs Globales

- **input1** (tipo): Descripción del input global
- **input2** (tipo, opcional): Descripción del input opcional

## Outputs Globales

- **output1** (tipo): Descripción del output final
- **output2** (tipo): Descripción de artefactos generados

## Precondiciones Globales

- [Condición general 1]
- [Condición general 2]
- [Estado del sistema requerido]

## Skills Utilizadas

1. `@skill:categoria/skill-1` - [Propósito en este workflow]
2. `@skill:categoria/skill-2` - [Propósito en este workflow]
3. `@skill:categoria/skill-3` - [Propósito en este workflow]

## Diagrama de Flujo

```
[Inicio]
   ↓
[Fase 1: Nombre]
   ↓
[Fase 2: Nombre]
   ↓
[Decisión?] → [Sí] → [Acción A]
   ↓                      ↓
  [No]                [Continuar]
   ↓                      ↓
[Fase 3: Nombre] ←────────┘
   ↓
[Fin]
```

## Fases del Workflow

### Fase 1: [Nombre de la Fase]

**Objetivo**: [Qué se logra en esta fase]

**Skills**:

- `@skill:categoria/skill-nombre`

**Pasos**:

1. [Paso detallado 1]
2. [Paso detallado 2]
3. [Paso detallado 3]

**Inputs de Fase**:

- [Input específico de esta fase]

**Outputs de Fase**:

- [Output específico de esta fase]

**Criterios de Éxito**:

- [ ] [Criterio 1]
- [ ] [Criterio 2]

**Punto de Control**: [Qué verificar antes de continuar]

---

### Fase 2: [Nombre de la Fase]

**Objetivo**: [Qué se logra en esta fase]

**Skills**:

- `@skill:categoria/skill-nombre`

**Pasos**:

1. [Paso detallado 1]
2. [Paso detallado 2]

**Inputs de Fase**:

- [Input específico de esta fase]

**Outputs de Fase**:

- [Output específico de esta fase]

**Criterios de Éxito**:

- [ ] [Criterio 1]
- [ ] [Criterio 2]

**Punto de Control**: [Qué verificar antes de continuar]

---

### Fase N: [Finalización]

**Objetivo**: [Verificación y cierre]

**Pasos**:

1. [Verificación final 1]
2. [Verificación final 2]
3. [Documentación de resultados]

**Criterios de Éxito**:

- [ ] [Criterio final 1]
- [ ] [Criterio final 2]

## Ejemplo Completo de Ejecución

### Contexto

[Descripción del escenario de ejemplo]

### Inputs Iniciales

```
[Inputs concretos para el ejemplo]
```

### Ejecución Paso a Paso

#### Fase 1

```
[Comandos o acciones específicas]
```

**Resultado**: [Output de esta fase]

#### Fase 2

```
[Comandos o acciones específicas]
```

**Resultado**: [Output de esta fase]

#### Fase N

```
[Comandos o acciones específicas]
```

**Resultado Final**: [Output completo del workflow]

## Manejo de Errores y Rollback

### Error en Fase 1

**Síntoma**: [Cómo se manifiesta]
**Acción**: [Qué hacer]
**Rollback**: [Cómo revertir cambios si es necesario]

### Error en Fase 2

**Síntoma**: [Cómo se manifiesta]
**Acción**: [Qué hacer]
**Rollback**: [Cómo revertir cambios si es necesario]

### Rollback Completo

**Procedimiento**:

1. [Paso de rollback 1]
2. [Paso de rollback 2]
3. [Verificación de estado limpio]

## Puntos de Decisión

### Decisión 1: [Nombre de la decisión]

**Condición**: [Qué evaluar]
**Si Verdadero**: [Qué hacer]
**Si Falso**: [Qué hacer]

### Decisión 2: [Nombre de la decisión]

**Condición**: [Qué evaluar]
**Si Verdadero**: [Qué hacer]
**Si Falso**: [Qué hacer]

## Optimizaciones

### Paralelización

- [Qué fases pueden ejecutarse en paralelo]
- [Consideraciones de dependencias]

### Caching

- [Qué resultados intermedios pueden cachearse]
- [Cuándo invalidar el cache]

### Reducción de Tokens

- [Estrategia 1 para optimizar tokens]
- [Estrategia 2 para optimizar tokens]

## Variantes del Workflow

### Variante 1: [Nombre]

**Cuándo usar**: [Situación específica]
**Diferencias**: [Qué cambia respecto al workflow base]

### Variante 2: [Nombre]

**Cuándo usar**: [Situación específica]
**Diferencias**: [Qué cambia respecto al workflow base]

## Métricas y KPIs

### Métricas de Proceso

- **Duración total**: [Tiempo esperado]
- **Tokens consumidos**: [Estimación]
- **Pasos manuales**: [Número]

### Métricas de Calidad

- **Tasa de éxito**: [Porcentaje esperado]
- **Errores comunes**: [Frecuencia]
- **Necesidad de rollback**: [Frecuencia]

## Checklist de Ejecución

### Pre-ejecución

- [ ] [Verificación 1]
- [ ] [Verificación 2]
- [ ] [Verificación 3]

### Durante Ejecución

- [ ] [Checkpoint 1]
- [ ] [Checkpoint 2]
- [ ] [Checkpoint 3]

### Post-ejecución

- [ ] [Verificación final 1]
- [ ] [Verificación final 2]
- [ ] [Documentación completada]

## Dependencias

### Skills Requeridas

- `@skill:categoria/skill-1` - [Por qué se necesita]
- `@skill:categoria/skill-2` - [Por qué se necesita]

### Herramientas Externas

- **Herramienta 1** (versión): [Para qué se usa]
- **Herramienta 2** (versión): [Para qué se usa]

### Permisos Requeridos

- [Permiso 1]
- [Permiso 2]

## Notas Importantes

- [Nota crítica 1]
- [Consideración especial 2]
- [Limitación conocida 3]

## Referencias

- [Documentación relacionada]
- [Especificaciones técnicas]
- [Ejemplos adicionales]

## Changelog

### v1.0.0 ([YYYY-MM-DD])

- Creación inicial del workflow
- [Fase inicial 1]
- [Fase inicial 2]

---

**Última ejecución exitosa**: [YYYY-MM-DD]  
**Próxima revisión**: [YYYY-MM-DD]  
**Estado**: [draft|stable|deprecated]  
**Mantenedor**: [Nombre]
