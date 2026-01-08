# Guías de Creación de Skills

## 🎯 Principios Fundamentales

### 1. **Atomicidad**

Cada skill debe hacer **una cosa** y hacerla bien. Si una skill hace múltiples cosas, divídela en varias skills más pequeñas.

### 2. **Idempotencia**

Ejecutar una skill múltiples veces con los mismos inputs debe producir el mismo resultado.

### 3. **Claridad**

La documentación debe ser tan clara que cualquier IA (o humano) pueda entender y ejecutar la skill sin ambigüedad.

### 4. **Eficiencia de Tokens**

Minimizar el uso de tokens innecesarios. Ser conciso pero completo.

## 📝 Estructura de una Skill

Cada skill debe contener:

```markdown
---
name: nombre-de-la-skill
version: 1.1.0
category: core/analysis
tags: [tag1, tag2, tag3]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 1-10
estimated_tokens: 500-1000
---

# Nombre de la Skill

## Descripción
[Descripción clara y concisa de qué hace la skill]

## Objetivo
[Qué problema resuelve o qué capacidad proporciona]

## AI Context
> **SYSTEM_INSTRUCTION**: Instrucción "System Prompt" de alta densidad.
> **OUTPUT_FORMAT**: Formato estricto de salida.

## Inputs
- **input1** (tipo): Descripción del input
- **input2** (tipo, opcional): Descripción del input opcional

## Outputs
- **output1** (tipo): Descripción del output
- **output2** (tipo): Descripción del output

## Tool Mapping
- **Acción Conceptual**: `herramienta_real`
- **Análisis**: `read_file`

## Precondiciones
- [Condición que debe cumplirse antes de ejecutar]

- [Otra condición]

## Postcondiciones
- [Estado esperado después de la ejecución]
- [Otro estado]

## Procedimiento

### Paso 1: [Nombre del paso]
[Descripción detallada del paso]

```[lenguaje]
// Código de ejemplo si aplica
```

### Paso 2: [Nombre del paso]

[Descripción detallada del paso]

## Ejemplos de Uso

### Ejemplo 1: [Caso de uso común]

```
Input: [ejemplo de input]
Output: [ejemplo de output esperado]
```

### Ejemplo 2: [Caso de uso avanzado]

```
Input: [ejemplo de input]
Output: [ejemplo de output esperado]
```

## Manejo de Errores

- **Error 1**: Descripción y cómo manejarlo
- **Error 2**: Descripción y cómo manejarlo

## Optimizaciones

- [Optimización 1]
- [Optimización 2]

## Dependencias

- [Skill o herramienta requerida 1]
- [Skill o herramienta requerida 2]

## Notas

- [Nota importante 1]
- [Nota importante 2]

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial de la skill

```

## 🏷️ Sistema de Categorías

### **core/** - Skills Fundamentales
- `analysis/` - Análisis de código, arquitectura, dependencias
- `generation/` - Generación de código, boilerplate
- `refactoring/` - Refactorización, optimización
- `documentation/` - Generación de documentación

### **domain/** - Skills Específicas del Dominio
- `delphi/` - Específicas de Delphi/Pascal
- `database/` - Gestión de bases de datos
- `verifactu/` - Normativa Verifactu
- `[custom]/` - Otras específicas del proyecto

### **workflows/** - Flujos de Trabajo
- `deployment/` - Despliegue y distribución
- `testing/` - Testing y QA
- `migration/` - Migraciones de datos/código

## 🎨 Mejores Prácticas

### ✅ DO (Hacer)
- Usar nombres descriptivos y concisos
- Documentar todos los inputs y outputs
- Incluir ejemplos de uso reales
- Versionar cambios significativos
- Mantener skills pequeñas y enfocadas
- Usar markdown para formateo claro
- Incluir estimación de complejidad y tokens

### ❌ DON'T (No Hacer)
- Crear skills monolíticas
- Asumir contexto implícito
- Omitir manejo de errores
- Duplicar funcionalidad existente
- Usar nombres ambiguos
- Dejar documentación incompleta

## 📊 Niveles de Complejidad

- **1-3**: Skill simple, ejecución directa
- **4-6**: Skill moderada, requiere análisis
- **7-8**: Skill compleja, múltiples pasos
- **9-10**: Skill crítica, requiere revisión experta

## 🔄 Proceso de Creación

1. **Identificar la necesidad**: ¿Qué problema resuelve?
2. **Verificar duplicados**: ¿Ya existe una skill similar?
3. **Diseñar la interfaz**: Definir inputs/outputs
4. **Documentar el procedimiento**: Pasos claros y concisos
5. **Crear ejemplos**: Al menos 2 casos de uso
6. **Probar**: Validar con casos reales
7. **Registrar**: Añadir al índice de skills
8. **Versionar**: Commit con mensaje descriptivo

## 🧪 Testing de Skills

Cada skill debe ser probada con:
- Caso de uso típico
- Caso de uso límite (edge case)
- Caso de error esperado

## 📈 Métricas de Calidad

Una skill de calidad debe tener:
- ✅ Documentación completa (100%)
- ✅ Al menos 2 ejemplos de uso
- ✅ Manejo de errores documentado
- ✅ Estimación de tokens precisa
- ✅ Versionamiento semántico
- ✅ Changelog actualizado

## 🔗 Composición de Skills

Las skills pueden componerse para crear workflows:

```markdown
# Workflow: Deploy Application

## Skills Utilizadas
1. @skill:core/analysis/validate-code
2. @skill:core/generation/build-release
3. @skill:workflows/testing/run-tests
4. @skill:workflows/deployment/deploy-production

## Flujo
[Descripción del flujo de ejecución]
```

## 📝 Plantillas Disponibles

- `skill-template-v2.md` - Plantilla recomendada para nuevas skills (OpenSpec compliant)
- `skill-template.md` - Plantilla legacy (verbose)
- `workflow-template.md` - Plantilla para workflows complejos

## 🆘 Ayuda

Para más información sobre la creación de skills, consulta:

- [README principal](./README.md)
- [Plantillas](./templates/)
- [Ejemplos en registry](./registry/)
