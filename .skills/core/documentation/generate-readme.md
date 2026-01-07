---
name: generate-readme
version: 1.0.0
category: core/documentation
tags: [documentation, readme, markdown, auto-doc]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 3
estimated_tokens: 400-600
---

# Generar README

## Descripción

Genera automáticamente un archivo README.md profesional para un proyecto o carpeta, analizando su estructura y contenido.

## Objetivo

Crear documentación de proyecto consistente y de alta calidad que siga las mejores prácticas de documentación de software.

## Inputs

- **project_path** (string): Ruta al proyecto o carpeta a documentar
- **template** (string, opcional): Plantilla a usar [minimal|standard|detailed|github] (default: standard)
- **include_sections** (array, opcional): Secciones específicas a incluir
- **language** (string, opcional): Idioma del README [es|en] (default: es)
- **badges** (boolean, opcional): Incluir badges/insignias (default: true)

## Outputs

- **readme_content** (string): Contenido del README generado
- **detected_info** (object): Información detectada del proyecto
- **suggestions** (array): Sugerencias para mejorar el README

## Precondiciones

- El proyecto debe existir y ser accesible
- Debe haber al menos algunos archivos para analizar

## Postcondiciones

- README generado es válido Markdown
- Incluye información relevante del proyecto
- Es coherente con la estructura del proyecto

## Procedimiento

### Paso 1: Analizar Estructura del Proyecto

Detectar:

- Tipo de proyecto (Delphi, Web, Script, etc.)
- Archivos principales (.dpr, .dproj, package.json, etc.)
- Estructura de directorios
- Licencia si existe
- Archivos de configuración

**Validación**: Tipo de proyecto identificado

### Paso 2: Detectar Información del Proyecto

Extraer de archivos de configuración:

- Nombre del proyecto
- Versión
- Descripción
- Autor
- Dependencias

**Validación**: Información básica detectada

### Paso 3: Seleccionar Plantilla

Según template seleccionado:

**minimal**:

- Título, Descripción, Instalación

**standard**:

- Título, Badges, Descripción, Requisitos, Instalación, Uso, Licencia

**detailed**:

- Todo lo anterior + Contribución, Changelog, Roadmap, FAQ

**github**:

- Optimizado para GitHub con badges, Actions, etc.

**Validación**: Plantilla seleccionada

### Paso 4: Generar Contenido

Para cada sección:

- Generar contenido basado en información detectada
- Añadir placeholders para información no disponible
- Incluir ejemplos de código si es relevante

**Validación**: Contenido generado para cada sección

### Paso 5: Añadir Elementos Visuales

Si badges=true:

- Badge de versión
- Badge de estado
- Badge de licencia
- Badges específicos del proyecto

**Validación**: Elementos visuales añadidos

### Paso 6: Generar Sugerencias

Identificar:

- Secciones que necesitan completarse manualmente
- Información faltante recomendada
- Mejoras potenciales

**Validación**: Sugerencias útiles generadas

## Ejemplos de Uso

### Ejemplo 1: README Standard para Proyecto Delphi

**Contexto**: Nuevo proyecto Delphi

**Input**:

```
project_path: "D:/ARAINFORIA/FACARAVF"
template: "standard"
language: "es"
```

**Output Esperado**:

```markdown
# FACARAVF

![Version](https://img.shields.io/badge/version-1.0.0-blue)
![Delphi](https://img.shields.io/badge/Delphi-11+-red)
![License](https://img.shields.io/badge/license-Private-gray)

## Descripción

Sistema de facturación con soporte para Verifactu.

## Requisitos

- Delphi 11 o superior
- BDE (Borland Database Engine)
- Windows 10/11

## Instalación

1. Clonar el repositorio
2. Abrir `FACARAVF.dproj` en Delphi
3. Compilar el proyecto (Ctrl+F9)
4. Ejecutar (F9)

## Uso

Para iniciar la aplicación:

```bash
FACARAVF.exe
```

## Estructura del Proyecto

```
FACARAVF/
├── Fuente/          # Código fuente
├── DATOS/           # Base de datos Paradox
├── Documentacion/   # Documentación
└── ...
```

## Licencia

Proyecto privado. Todos los derechos reservados.

---

*Generado automáticamente por @skill:core/documentation/generate-readme*

```

### Ejemplo 2: README Minimal
**Contexto**: Script simple

**Input**:
```

project_path: "D:/Scripts/backup"
template: "minimal"
language: "es"

```

**Output Esperado**:
```markdown
# Backup Script

Script de backup automático.

## Instalación

```bash
copy backup.bat C:\Scripts\
```

## Uso

```bash
backup.bat [origen] [destino]
```

```

## Manejo de Errores

### Error 1: Proyecto vacío
**Síntoma**: No hay archivos para analizar
**Causa**: Directorio vacío o sin permisos
**Solución**: Verificar que el directorio contiene archivos del proyecto

### Error 2: Tipo de proyecto no detectado
**Síntoma**: No se puede determinar el tipo de proyecto
**Causa**: Estructura no estándar
**Solución**: Especificar tipo manualmente o usar template minimal

### Error 3: Información insuficiente
**Síntoma**: Muchos placeholders en el README
**Causa**: Proyecto sin archivos de configuración
**Solución**: Completar manualmente o añadir archivos de configuración

## Optimizaciones

### Optimización de Tokens
- Solo analizar archivos relevantes (ignorar binarios, node_modules, etc.)
- Limitar profundidad de análisis de directorios
- Cachear detección de tipo de proyecto

### Optimización de Calidad
- Detectar y adaptar al estilo existente del proyecto
- Incluir ejemplos relevantes al tipo de proyecto
- Generar TOC para READMEs largos

## Dependencias

### Skills Requeridas
- Ninguna

### Herramientas Externas
- Ninguna

## Variantes

### Variante 1: README para Skill
Generar README específico para documentar una skill del sistema

### Variante 2: CONTRIBUTING.md
Generar archivo de contribución para proyectos open source

### Variante 3: API Documentation
Generar documentación de API basada en código fuente

## Métricas de Éxito
- [ ] Tipo de proyecto correctamente detectado
- [ ] Información relevante incluida
- [ ] Markdown válido generado
- [ ] Secciones apropiadas para el proyecto
- [ ] Sugerencias útiles proporcionadas

## Notas
- El README generado es punto de partida, revisar y personalizar
- Los placeholders [TODO] indican información a completar
- Regenerar si la estructura del proyecto cambia significativamente

## Referencias
- [Make a README](https://www.makeareadme.com/)
- [Awesome README](https://github.com/matiassingers/awesome-readme)
- [README Template](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2)

## Changelog

### v1.0.0 (2026-01-07)
- Creación inicial
- Soporte para templates: minimal, standard, detailed, github
- Detección automática de tipo de proyecto
- Soporte para español e inglés

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
