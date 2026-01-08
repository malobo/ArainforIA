# Guía para Asistentes de IA - Sistema de Skills

> **Audiencia**: Modelos de IA (Gemini, Claude, GPT-4, etc.)  
> **Propósito**: Instrucciones para usar efectivamente el sistema de skills  
> **Versión**: 1.0.0

## 🤖 Introducción para IAs

Este sistema de **skills** está diseñado específicamente para que asistentes de IA puedan ejecutar tareas complejas de forma consistente, eficiente y documentada. Cada skill es una capacidad modular que puedes invocar para ayudar al usuario.

## 📖 Cómo Interpretar una Skill

Cada skill tiene la siguiente estructura:

```yaml
---
name: nombre-de-la-skill          # Identificador único
version: 1.0.0                    # Versión semántica
category: core/analysis           # Categoría jerárquica
tags: [tag1, tag2]                # Tags para búsqueda
complexity: 1-10                  # Nivel de complejidad
estimated_tokens: 500-1000        # Tokens estimados
---
```

Seguido de:

- **Descripción**: Qué hace la skill
- **Objetivo**: Por qué existe
- **Inputs**: Qué necesita (con tipos y opcionalidad)
- **Outputs**: Qué produce
- **Precondiciones**: Qué debe existir antes
- **Postcondiciones**: Qué estado resulta después
- **Procedimiento**: Pasos detallados a seguir
- **Ejemplos**: Casos de uso concretos
- **Manejo de errores**: Qué hacer si algo falla

## 🎯 Cómo Ejecutar una Skill

### Método 1: Invocación Explícita del Usuario

Cuando el usuario dice:

```
Ejecuta la skill: @skill:domain/delphi/analyze-delphi-unit
con unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/uVerifactu.pas"
```

**Tu proceso**:

1. Localizar el archivo de la skill: `.skills/domain/delphi/analyze-delphi-unit.md`
2. Leer y parsear la documentación completa
3. Validar que los inputs proporcionados cumplen los requisitos
4. Verificar las precondiciones
5. Ejecutar cada paso del procedimiento en orden
6. Generar los outputs especificados
7. Verificar las postcondiciones
8. Reportar resultados al usuario

### Método 2: Inferencia Automática

Cuando el usuario dice:

```
Analiza el archivo uVerifactu.pas para ver si tiene problemas
```

**Tu proceso**:

1. Identificar la intención: análisis de código Delphi
2. Buscar en INDEX.md skills relacionadas con "analysis" y "delphi"
3. Encontrar `domain/delphi/analyze-delphi-unit`
4. Proponer al usuario: "Voy a usar la skill analyze-delphi-unit para esto"
5. Ejecutar la skill con parámetros inferidos
6. Reportar resultados

### Método 3: Composición de Skills (Workflows)

Cuando el usuario dice:

```
Despliega la nueva versión a producción
```

**Tu proceso**:

1. Identificar que es un workflow complejo
2. Buscar en `workflows/deployment/`
3. Encontrar `deploy-verifactu-update.md`
4. Leer las fases del workflow
5. Identificar las skills dependientes
6. Ejecutar cada fase en orden, usando las skills necesarias
7. Manejar puntos de decisión según resultados
8. Ejecutar rollback si es necesario
9. Generar reporte final

## 🔍 Búsqueda de Skills

### Por Categoría

```
Usuario: "Necesito algo para analizar código"
Tú: Buscar en INDEX.md → Categoría "Core - Analysis"
```

### Por Tag

```
Usuario: "Algo relacionado con Verifactu"
Tú: Buscar en INDEX.md → Tag "verifactu"
Resultado: validate-verifactu-implementation, deploy-verifactu-update
```

### Por Nombre

```
Usuario: "Validar implementación"
Tú: Buscar en INDEX.md → "validate"
Resultado: validate-verifactu-implementation
```

## 💡 Optimización de Tokens

### Estrategia 1: Lectura Selectiva

No leas toda la skill de una vez. Lee:

1. **Primero**: Frontmatter (metadata)
2. **Segundo**: Descripción y Objetivo
3. **Tercero**: Solo si es la skill correcta, lee el resto

### Estrategia 2: Cacheo de Skills Comunes

Si una skill se usa frecuentemente en la conversación, mantén su contenido en contexto.

### Estrategia 3: Resumen de Procedimientos

Para workflows largos, resume los pasos en lugar de repetir todo el contenido.

### Estrategia 4: Referencias en Lugar de Duplicación

En lugar de copiar el contenido de una skill, referencia su ubicación:

```
"Voy a ejecutar la skill domain/delphi/analyze-delphi-unit (ver .skills/domain/delphi/analyze-delphi-unit.md para detalles)"
```

## 🎨 Formato de Respuestas

### Al Ejecutar una Skill

```markdown
## Ejecutando Skill: analyze-delphi-unit

**Versión**: 1.0.0  
**Complejidad**: 4/10  
**Tokens estimados**: 800-1200

### Inputs Recibidos
- unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/uVerifactu.pas"
- depth: "detailed" (default)

### Validación de Precondiciones
✅ Archivo existe y es accesible
✅ Archivo es sintácticamente válido

### Ejecución

#### Paso 1: Lectura y Parsing Inicial
[Resultado del paso]

#### Paso 2: Análisis de Estructura
[Resultado del paso]

[... más pasos ...]

### Outputs Generados
- **analysis_report**: [Ver abajo]
- **metrics**: {...}
- **recommendations**: [...]

### Postcondiciones
✅ Reporte generado
✅ Métricas calculadas
✅ Recomendaciones priorizadas

---

## Reporte de Análisis
[Contenido del reporte]
```

### Al Reportar Errores

```markdown
## ❌ Error en Skill: create-database-migration

**Fase**: Paso 3 - Crear Script de Migración  
**Error**: Tipo de dato incompatible  
**Causa**: BLOB no soportado en Paradox para este contexto

### Solución Sugerida
Según la documentación de la skill, usar MEMO en lugar de BLOB para campos de texto largo.

### ¿Deseas que intente de nuevo con MEMO?
```

## 🔄 Manejo de Workflows

### Puntos de Control

Los workflows tienen **puntos de control** donde debes:

1. Validar que el paso se completó correctamente
2. Preguntar al usuario si desea continuar (si es crítico)
3. Ofrecer rollback si algo falló

Ejemplo:

```markdown
### ✅ Fase 2 Completada: Backup

**Resultado**: Backup creado exitosamente (245MB)
**Ubicación**: backup_20260107_110316.zip
**Checksum**: a3f5b8c9d2e1...

**Punto de Control**: ¿Deseas continuar con la Fase 3 (Detener Aplicación)?
```

### Decisiones Automáticas vs. Manuales

**Decisiones Automáticas** (no preguntar):

- Validaciones técnicas (score >= 90)
- Verificaciones de existencia de archivos
- Cálculos y métricas

**Decisiones Manuales** (preguntar al usuario):

- Continuar después de un error no crítico
- Ejecutar rollback
- Saltar validaciones (nunca en producción)
- Modificar parámetros críticos

## 🚨 Manejo de Errores

### Errores Documentados

Si el error está en la sección "Manejo de Errores" de la skill:

1. Identificar el error por síntoma
2. Explicar la causa al usuario
3. Aplicar la solución documentada
4. Continuar o abortar según corresponda

### Errores No Documentados

Si el error no está documentado:

1. Explicar el error al usuario
2. Proponer soluciones basadas en tu conocimiento
3. Ofrecer rollback si aplica
4. Sugerir documentar este error en la skill

## 📊 Reportes y Documentación

### Generar Reportes

Muchas skills generan reportes. Usa este formato:

```markdown
# [Título del Reporte]

**Generado por**: @skill:[ruta/a/skill]  
**Fecha**: YYYY-MM-DD HH:MM:SS  
**Versión de Skill**: X.Y.Z

## Resumen Ejecutivo
[Resumen de 2-3 líneas]

## Detalles
[Contenido detallado según la skill]

## Recomendaciones
1. [Recomendación priorizada 1]
2. [Recomendación priorizada 2]

## Próximos Pasos
[Acciones sugeridas]
```

### Actualizar Documentación

Si descubres mejoras o errores en una skill:

1. Completar la tarea del usuario primero
2. Sugerir actualización de la skill
3. Si el usuario aprueba, actualizar el archivo .md
4. Actualizar CHANGELOG.md
5. Incrementar versión si es necesario

## 🎓 Mejores Prácticas

### ✅ DO (Hacer)

1. **Leer la skill completa** antes de ejecutarla
2. **Validar precondiciones** siempre
3. **Seguir el procedimiento** paso a paso
4. **Reportar progreso** en workflows largos
5. **Manejar errores** según documentación
6. **Verificar postcondiciones** al finalizar
7. **Generar outputs** en el formato especificado
8. **Optimizar tokens** usando referencias
9. **Sugerir skills** cuando sean relevantes
10. **Documentar problemas** encontrados

### ❌ DON'T (No Hacer)

1. **No improvises** el procedimiento
2. **No saltes pasos** sin justificación
3. **No ignores precondiciones**
4. **No omitas validaciones** en producción
5. **No asumas inputs** no proporcionados
6. **No mezcles skills** sin usar un workflow
7. **No modifiques skills** sin permiso del usuario
8. **No uses skills deprecated**
9. **No excedas** la complejidad estimada sin avisar
10. **No olvides** generar todos los outputs

## 🔗 Composición de Skills

### Skills Atómicas

Úsalas individualmente para tareas específicas:

```
@skill:domain/delphi/analyze-delphi-unit
```

### Workflows

Úsalos para tareas complejas que requieren múltiples skills:

```
@skill:workflows/deployment/deploy-verifactu-update
  (internamente usa: backup-database, create-database-migration, validate-verifactu-implementation)
```

### Composición Manual

Si no existe un workflow, puedes componer skills manualmente:

```markdown
Voy a ejecutar una secuencia de skills:

1. @skill:domain/database/backup-database
   → Crear backup de seguridad

2. @skill:domain/database/create-database-migration
   → Aplicar cambios de esquema

3. @skill:domain/verifactu/validate-verifactu-implementation
   → Validar que todo funciona

¿Deseas que proceda?
```

## 📈 Métricas y Feedback

### Tracking de Ejecución

Mantén registro mental de:

- Skills ejecutadas en esta conversación
- Tokens consumidos aproximados
- Errores encontrados
- Tiempo estimado vs. real

### Sugerencias de Mejora

Si detectas:

- Pasos innecesarios
- Documentación confusa
- Estimaciones incorrectas
- Errores no documentados

Sugiere al usuario actualizar la skill.

## 🎯 Casos de Uso Comunes

### Caso 1: Análisis de Código

```
Usuario: "Revisa el archivo Facturas.pas"
Tú: 
1. Identificar skill: analyze-delphi-unit
2. Ejecutar con depth: "detailed"
3. Generar reporte con métricas
4. Priorizar recomendaciones
5. Presentar al usuario
```

### Caso 2: Modificación de BD

```
Usuario: "Añade un campo Email a la tabla Clientes"
Tú:
1. Identificar skill: create-database-migration
2. Proponer nombre: "add_cliente_email"
3. Generar scripts de migración y rollback
4. Presentar para revisión
5. Si aprueba, guardar scripts
```

### Caso 3: Validación Verifactu

```
Usuario: "Verifica que cumplimos con Verifactu"
Tú:
1. Identificar skill: validate-verifactu-implementation
2. Ejecutar validación exhaustiva
3. Calcular score de cumplimiento
4. Listar gaps encontrados
5. Priorizar acciones correctivas
6. Generar certificado si score >= 95
```

### Caso 4: Despliegue a Producción

```
Usuario: "Despliega la versión 2.1.0 a producción"
Tú:
1. Identificar workflow: deploy-verifactu-update
2. Validar inputs (versión, ejecutable, scripts)
3. Ejecutar Fase 1: Pre-Despliegue
4. Reportar resultados y pedir confirmación
5. Ejecutar Fase 2: Backup
6. [... continuar con todas las fases ...]
7. Si algo falla, ejecutar rollback
8. Generar reporte final
```

## 🆘 Troubleshooting

### Problema: Skill no encontrada

**Solución**: Buscar en INDEX.md por categoría o tag

### Problema: Inputs insuficientes

**Solución**: Preguntar al usuario por los inputs faltantes

### Problema: Precondiciones no cumplidas

**Solución**: Informar al usuario y sugerir cómo cumplirlas

### Problema: Error durante ejecución

**Solución**: Consultar sección "Manejo de Errores" de la skill

### Problema: Workflow muy largo

**Solución**: Reportar progreso cada fase, permitir pausas

## 📚 Recursos

- **Índice completo**: [INDEX.md](./INDEX.md)
- **Registro OpenSpec**: [registry/tools.yaml](./registry/tools.yaml)

---

## 🎓 Entrenamiento Recomendado

Para familiarizarte con el sistema:

1. **Lee**: README.md, GUIDELINES.md, QUICKSTART.md
2. **Estudia**: Las 4 skills existentes en detalle
3. **Practica**: Ejecuta cada skill con los ejemplos proporcionados
4. **Experimenta**: Compón skills manualmente
5. **Optimiza**: Aprende a minimizar tokens

---

**Versión**: 1.0.0  
**Última actualización**: 2026-01-07  
**Audiencia**: Asistentes de IA  
**Mantenedor**: Sistema de Skills ARAINFORIA
