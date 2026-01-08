# ArainforIA Workspace & AI Skills System v2.0

> **Next-Gen Context Engineering for Delphi & Hybrid Development**

Este repositorio centraliza el **Sistema de Habilidades (Skills System)** y la documentación de desarrollo para el ecosistema de proyectos de ARAINFORIA.

Actúa como el "cerebro" y la base de conocimientos viva para los asistentes de IA (Gemini 2.0, Claude 3.5 Sonnet, etc.) que colaboradores en los proyectos de facturación y gestión (FACARAVF, GESFAC, ERPW).

## 🧠 Sistema de Skills (v2.0 Next-Gen)

El núcleo de este repositorio es el directorio `.skills/`, un framework modular de **Context as Code** diseñado para eliminar alucinaciones y estandarizar el desarrollo.

### Novedades v2.0

- ✨ **Hyper-Context XML**: Uso de tags semánticos (`<context>`, `<instruction>`, `<examples>`) para segmentar la información y guiar a la IA con precisión quirúrgica.
- ⚡ **Auto-Triggers**: Las skills definen sus propias palabras clave (`triggers`) para ser cargadas automáticamente solo cuando se necesitan.
- 🏗️ **Arquitectura Híbrida**: Soporte nativo para desarrollo dual **Delphi (Escritorio)** + **PHP (Nube/Sincronización)**.
- 🛡️ **Validación Automática**: Scripts Python (`generate_index.py`) que garantizan la integridad del sistema.

### Estructura del Sistema

```text
.skills/
├── core/           # Capacidades fundamentales
│   ├── analysis/   # Validación de sistema y código
│   ├── generation/ # Boilerplate y Tests
│   └── integration/# Conectores RAG y Notion MCP
├── domain/         # Conocimiento experto
│   ├── delphi/     # Clean Code & Best Practices
│   ├── verifactu/  # Normativa Antifraude Española
│   ├── hybrid/     # Sincronización Nube-Escritorio
│   └── projects/   # Contexto específico (ARAFAC, ERPW)
└── workflows/      # Procedimientos secuenciales (Compilación, Deploy)
```

## 🛠️ Capacidades Destacadas

1. **Experto VERIFACTU & mORMot 2**:
    - Implementación criptográfica normativa (Huella de factura, Trazabilidad).
    - Uso de `mORMot 2` para firma digital (PKCS#11/X.509) y generación de PDF/A-1.

2. **Integración MCP (Model Context Protocol)**:
    - **Delphi RAG**: Consultas directas a la documentación oficial y ayudas CHM/PDF.
    - **Notion Sync**: Sincronización bidireccional con la gestión de proyectos y roadmap.

3. **Desarrollo Híbrido**:
    - Patrones de sincronización offline-first entre Delphi y APIs REST PHP.

## 🚀 Cómo usar este repositorio

### Para Humanos

Este repositorio es la **Fuente de Verdad**.

- Consulta `AGENTES.md` para ver las reglas de "Constitución" de la IA.
- Consulta `LOG_SISTEMA_SKILLS.md` para ver la evolución del sistema.

### Para Agentes de IA

Si has sido conectado a este workspace:

1. **NO inventes**: Busca siempre primero en `.skills/index.md`.
2. **Sé preciso**: Observa los `triggers` de las skills para cargar el contexto adecuado.
3. **Obedece la Constitución**: Sigue estrictamente `AGENTES.md`.

## 📦 Instalación y Réplica

Para dotar a otro proyecto de esta inteligencia:

```bash
git clone https://github.com/malobo/ArainforIA.git
cp -r ArainforIA/.skills /ruta/a/tu/nuevo/proyecto/
```

---

## Guía Práctica: Cómo Usar el Sistema de Skills

### 1. Introducción: ¿Qué es un "Skill" y Por Qué es Tan Útil?

En el contexto de la interacción con una Inteligencia Artificial (IA), un **"skill"** es un bloque de conocimiento o un conjunto de instrucciones autocontenido y reutilizable que se almacena en un archivo.

**El Problema Principal que Resuelve:**

Las IAs tienen una "memoria a corto plazo" (ventana de contexto) limitada. No puedes pegar miles de líneas de documentación en cada pregunta. Hacerlo es ineficiente, costoso (en tokens) y propenso a errores.

**La Solución del Sistema de Skills:**

En lugar de repetir el contexto, lo almacenamos en archivos bien estructurados. Luego, simplemente le decimos a la IA: *"Para esta tarea, usa el conocimiento del archivo `mi-skill.md`"*.

**Beneficios Clave:**

- **Ahorro de Contexto:** Reduce drásticamente la cantidad de texto que necesitas enviar en cada instrucción.
- **Consistencia:** La IA siempre recibe la misma información base para una tarea, lo que produce resultados más predecibles.
- **Mantenibilidad:** Si una norma o un proyecto cambia, solo actualizas un archivo (el skill), no cientos de instrucciones pasadas.
- **Reutilización:** El mismo skill puede ser usado por diferentes IAs o en diferentes conversaciones.

---

### 2. La Arquitectura: Una Estructura de Carpetas Inteligente

La organización es fundamental. Una buena estructura permite a la IA (y a ti) encontrar rápidamente el contexto correcto. La que hemos implementado es un excelente punto de partida:

```text
.skills/
├── core/            # Conocimiento fundamental y transversal
├── projects/        # Contexto específico de cada proyecto
├── workflows/       # Guías paso a paso para tareas comunes
├── _template.md     # Plantilla para crear nuevos skills
├── generate_index.py# Script para automatizar el índice
└── index.md         # El índice de todos los skills (generado automáticamente)
```

- **`/core`**: Para el conocimiento que no cambia a menudo pero es crucial. Ejemplos: guías de estilo de programación, normativas legales (Verifactu), configuración de herramientas.
- **`/projects`**: Un archivo por cada proyecto en el que trabajas. Contiene el resumen, la arquitectura, la ubicación de los archivos, etc.
- **`/workflows`**: Para tareas repetitivas. ¿Cómo se compila un proyecto? ¿Cómo se despliega a producción? Cada uno de estos flujos de trabajo es un skill.

---

### 3. La Anatomía de un Skill: El Archivo Markdown

Cada skill es un archivo `.md` que sigue una estructura predecible. Esto ayuda a la IA a analizarlo eficientemente.

```yaml
---
# Metadatos para la automatización (Frontmatter YAML)
id: skill-unico-id # Identificador único
name: Nombre Legible del Skill # Nombre para mostrar
version: 1.2 # Versión del skill
category: core | project | workflow # Categoría a la que pertenece
priority: critical | high | medium | low # Importancia del skill
last_updated: YYYY-MM-DD # Fecha de la última actualización
---

# Título Principal del Skill

## Descripción
¿Qué es este skill y qué problema resuelve?

## Contexto Clave
Una lista de 3-5 puntos críticos que la IA debe entender inmediatamente.

## Instrucciones / Reglas
Un conjunto de reglas numeradas y obligatorias sobre cómo usar la información. ¡Sé directo! "HAZ esto", "NUNCA hagas aquello".

## Recursos Relacionados
Enlaces a otros skills o archivos importantes para crear una red de conocimiento.

## Historial de Revisiones
Un registro de cambios para entender la evolución del skill.
```

---

### 4. El Flujo de Trabajo en la Práctica

#### Paso 1: Crear o Actualizar un Skill

1. Copia `_template.md` a un nuevo archivo (ej. `projects/nuevo_proyecto.md`).
2. Rellena los metadatos YAML.
3. Completa las secciones de Markdown con información clara y concisa.

#### Paso 2: Mantener el Índice Actualizado

Después de crear, modificar o eliminar uno o más skills, ejecuta el script de Python:

```shell
python .skills/generate_index.py
```

Esto regenerará `index.md` para que refleje el estado actual de tu base de conocimiento.

#### Paso 3: Usar el Skill con la IA

Esta es la parte más simple. Tu instrucción a la IA ahora es mucho más corta y precisa.

**Ejemplo MALO (sin skills):**
> "Hola, necesito añadir una función al proyecto Aracostes. Recuerda que está en C:\Arainfor\ARAFAC, el dpr es Aracostes.dpr, la base de datos es SQLite en Costes.s3db, usa FireDAC y tienes que seguir la normativa Verifactu para el hash, que implica SHA-256..."

**Ejemplo BUENO (con skills):**
> "Vamos a trabajar en el proyecto ARAFAC. Por favor, carga el contexto del skill `projects/arafac.md`."

La IA, si es capaz de leer archivos, cargará todo el contexto relevante y estará lista para trabajar de manera consistente y alineada con tus estándares.

---

### Conclusión: Trata tu Contexto como Código

Este sistema te anima a tratar tu conocimiento y contexto **como si fuera código fuente**:

- **Es versionable** (puedes usar Git).
- **Es modular**.
- **Es mantenible**.

Al adoptar esta mentalidad, potencias enormemente tu colaboración con cualquier asistente de IA.

---
**Maintainer**: Manuel José López & Gemini Agent
**Version**: 2.0.0 (January 2026)
**License**: Proprietary / ARAINFORIA Internal Use
