---
name: validate-skill-format
version: 1.1.0
category: core/analysis
tags: [validation, skills, quality, meta]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 3
estimated_tokens: 400-600
---

# Validar Formato de Skill

## Descripción

Valida que un archivo de skill sigue el formato estándar del sistema (v1.1), verificando todas las secciones requeridas y la calidad de la documentación.

## Objetivo

Garantizar que todas las skills del sistema mantienen un formato consistente y cumplen con los estándares de calidad definidos en GUIDELINES.md.

## AI Context

> **SYSTEM_INSTRUCTION**: Check file against strict v1.1 structure. Focus on 'AI Context' and 'Tool Mapping' existence.
> **OUTPUT_FORMAT**: JSON Report with 'status' and 'issues'.
> **TOKEN_STRATEGY**: Stop reading after 'Changelog'.

## Inputs

- **skill_path** (string): Ruta al archivo .md de la skill a validar
- **strict_mode** (boolean, opcional): Si true, requiere todas las secciones opcionales (default: false)
- **auto_fix** (boolean, opcional): Si true, sugiere correcciones automáticas (default: false)

## Outputs

- **validation_result** (object): Resultado con score y detalles
- **issues** (array): Lista de problemas encontrados
- **suggestions** (array): Sugerencias de mejora
- **fixed_content** (string, opcional): Contenido corregido si auto_fix=true

## Tool Mapping

- **Lectura**: `read_file`
- **Parsing**: (Internal Logic)
- **Reporte**: (Internal Logic)

## Precondiciones

- El archivo .md debe existir
- El archivo debe ser legible

## Postcondiciones

- Se genera un reporte de validación
- Se identifican todos los problemas de formato
- Se proporciona score de calidad

## Procedimiento

### Paso 1: Verificar Frontmatter YAML

Comprobar que existe y contiene:

- `name` (requerido)
- `version` (requerido, formato X.Y.Z)
- `category` (requerido)
- `tags` (requerido, array)
- `author` (requerido)
- `created` (requerido, formato YYYY-MM-DD)
- `updated` (requerido)
- `complexity` (requerido, 1-10)
- `estimated_tokens` (requerido)

**Validación**: Frontmatter completo y válido

### Paso 2: Verificar Secciones Requeridas

Comprobar presencia de:

- `# [Título]` (H1)
- `## Descripción`
- `## Objetivo`
- `## AI Context` (Nueva en v1.1.0)
- `## Inputs`
- `## Outputs`
- `## Tool Mapping` (Nueva en v1.1.0)
- `## Precondiciones`
- `## Postcondiciones`
- `## Procedimiento`
- `## Ejemplos de Uso`
- `## Manejo de Errores`
- `## Changelog`

**Validación**: Todas las secciones requeridas presentes

### Paso 3: Verificar Calidad del Contenido

Evaluar:

- Descripción > 50 caracteres
- Al menos 2 inputs documentados
- Al menos 1 output documentado
- Al menos 2 pasos en procedimiento
- Al menos 2 ejemplos de uso
- Al menos 1 error documentado
- Changelog con al menos 1 entrada

**Validación**: Contenido cumple mínimos de calidad

### Paso 4: Verificar Secciones Opcionales

Si strict_mode=true, verificar:

- `## Optimizaciones`
- `## Dependencias`
- `## Variantes`
- `## Métricas de Éxito`
- `## Notas`
- `## Referencias`

**Validación**: Secciones opcionales presentes (solo en modo estricto)

### Paso 5: Calcular Score de Calidad

Puntuación basada en:

- Frontmatter completo: 20 puntos
- Secciones requeridas: 40 puntos
- Calidad de contenido: 30 puntos
- Secciones opcionales: 10 puntos

**Validación**: Score calculado correctamente

### Paso 6: Generar Reporte

Crear reporte con:

- Score total (0-100)
- Estado (PASS >= 80, WARN 60-79, FAIL < 60)
- Lista de issues por severidad
- Sugerencias de mejora
- Contenido corregido si aplica

**Validación**: Reporte generado

## Ejemplos de Uso

### Ejemplo 1: Validación Básica

**Contexto**: Verificar skill recién creada

**Input**:

```
skill_path: ".skills/domain/delphi/analyze-delphi-unit.md"
strict_mode: false
```

**Output Esperado**:

```json
{
  "validation_result": {
    "score": 95,
    "status": "PASS",
    "frontmatter": "VALID",
    "required_sections": "COMPLETE",
    "content_quality": "HIGH"
  },
  "issues": [],
  "suggestions": [
    "Considerar añadir sección 'Referencias' con enlaces a documentación oficial"
  ]
}
```

### Ejemplo 2: Validación Estricta con Problemas

**Contexto**: Skill con formato incompleto

**Input**:

```
skill_path: ".skills/draft/nueva-skill.md"
strict_mode: true
auto_fix: true
```

**Output Esperado**:

```json
{
  "validation_result": {
    "score": 65,
    "status": "WARN",
    "frontmatter": "INCOMPLETE",
    "required_sections": "MISSING: 2",
    "content_quality": "MEDIUM"
  },
  "issues": [
    {"severity": "ERROR", "section": "frontmatter", "message": "Falta campo 'complexity'"},
    {"severity": "ERROR", "section": "content", "message": "Falta sección 'Manejo de Errores'"},
    {"severity": "WARN", "section": "examples", "message": "Solo hay 1 ejemplo, mínimo recomendado: 2"}
  ],
  "suggestions": [
    "Añadir campo complexity (1-10) al frontmatter",
    "Crear sección '## Manejo de Errores' con al menos 1 error documentado",
    "Añadir al menos 1 ejemplo de uso adicional"
  ],
  "fixed_content": "---\nname: nueva-skill\n..."
}
```

## Manejo de Errores

### Error 1: Archivo no encontrado

**Síntoma**: No se puede leer el archivo
**Causa**: Ruta incorrecta
**Solución**: Verificar que la ruta es correcta y el archivo existe

### Error 2: YAML inválido

**Síntoma**: No se puede parsear el frontmatter
**Causa**: Sintaxis YAML incorrecta
**Solución**: Corregir la sintaxis del frontmatter (comillas, indentación)

### Error 3: Encoding incorrecto

**Síntoma**: Caracteres extraños en el contenido
**Causa**: Archivo no está en UTF-8
**Solución**: Convertir archivo a UTF-8

## Optimizaciones

### Optimización de Tokens

- Solo leer las primeras líneas para validación rápida de frontmatter
- Usar regex para detectar secciones en lugar de parsing completo
- Cachear reglas de validación

### Optimización de Rendimiento

- Validar múltiples skills en lote
- Paralelizar validación de secciones independientes

## Dependencias

### Skills Requeridas

- Ninguna (skill fundamental)

### Herramientas Externas

- Ninguna

## Variantes

### Variante 1: Validación de Workflow

Adaptar para validar workflows que tienen secciones adicionales como "Fases" y "Rollback"

### Variante 2: Validación de Lote

Validar todas las skills del sistema en una sola ejecución

## Métricas de Éxito

- [ ] Frontmatter parseado correctamente
- [ ] Todas las secciones requeridas verificadas
- [ ] Score calculado correctamente
- [ ] Issues categorizados por severidad
- [ ] Sugerencias útiles generadas

## Notas

- Esta skill es "meta" - valida otras skills
- Usar antes de commit de nuevas skills
- Integrar con workflow de CI/CD si existe

## Referencias

- [GUIDELINES.md](../../GUIDELINES.md) - Estándares de formato
- [skill-template.md](../../templates/skill-template.md) - Plantilla oficial

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
- Validación de frontmatter, secciones y contenido
- Sistema de scoring
- Modo estricto y auto-fix

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
