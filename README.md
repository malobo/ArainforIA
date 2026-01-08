# ArainforIA Workspace & AI Skills System v2.0

> **Next-Gen Context Engineering for Delphi & Hybrid Development**

Este repositorio centraliza el **Sistema de Habilidades (Skills System)** y la documentación de desarrollo para el ecosistema de proyectos de ARAINFORIA.

Actúa como el "cerebro" y la base de conocimientos viva para los asistentes de IA (Gemini 2.0, Claude 3.5 Sonnet, etc.) que colaboradores en los proyectos de facturación y gestión (FACARAVF, GESFAC, ERPW).

## 🧠 Sistema de Skills (v2.0 Next-Gen)

El núcleo de este repositorio es el directorio `.skills/`, un framework modular de **Context as Code** diseñado para eliminar alucinaciones y estandarizar el desarrollo.

### Novedades v2.1 (Enero 2026 - Ciclo Completado)

Hemos cerrado el ciclo de implementación estratégico con **73 Skills** activas, cubriendo 3 áreas clave:

1. ☁️ **Integración Híbrida**: Arquitectura PHP/MySQL para sincronización móvil y nube.
2. ⚡ **Productividad**: Generadores de reportes, validadores DFM y exportación de datos.
3. 🏗️ **Arquitectura Avanzada**: Refactorización MVP, Servicios REST mORMot 2 y Debugging de Conexiones.

- ✨ **Hyper-Context XML**: Uso de tags semánticos (`<context>`, `<instruction>`, `<examples>`) para segmentar la información y guiar a la IA con precisión quirúrgica.
- ⚡ **Auto-Triggers**: Las skills definen sus propias palabras clave (`triggers`) para ser cargadas automáticamente solo cuando se necesitan.
- 🛡️ **Validación Automática**: Scripts Python (`generate_index.py`) que garantizan la integridad del sistema.

### Estructura del Sistema

```text
.skills/
├── core/           # Capacidades fundamentales (Refactoring, Analysis)
├── domain/         # Conocimiento experto (Delphi, Database, Verifactu)
├── workflows/      # Procedimientos secuenciales (Sync, Build, Deploy)
└── registry/       # OpenSpec Tools Registry (73 skills indexed)
```

## 📚 Recursos Clave

- **[MANUAL_SKILLS.docx](MANUAL_SKILLS.docx)**: Referencia completa imprimible con todas las skills, descripciones y ejemplos.
- **[LOG_DESARROLLO.md](LOG_DESARROLLO.md)**: Bitácora detallada de la evolución del sistema y decisiones de diseño.
- **[AGENTES.md](AGENTES.md)**: Reglas de "Constitución" para los agentes de IA.

## 🛠️ Capacidades Destacadas

1. **Experto VERIFACTU & mORMot 2**:
    - Implementación criptográfica normativa (Huella de factura, Trazabilidad).
    - Uso de `mORMot 2` para firma digital (PKCS#11/X.509) y generación de PDF/A-1.

2. **Integración MCP (Model Context Protocol)**:
    - **Delphi RAG**: Consultas directas a la documentación oficial.
    - **Notion Sync**: Sincronización bidireccional con la gestión de proyectos.

3. **Arquitectura Híbrida & MVP**:
    - Sincronización offline-first.
    - Desacoplamiento de UI y Lógica de Negocio (Model-View-Presenter).

## 🚀 Cómo usar este repositorio

### Para Humanos

Este repositorio es la **Fuente de Verdad** del proyecto.
Descarga el **[Manual de Skills](MANUAL_SKILLS.docx)** para tener una referencia rápida de qué puede hacer tu asistente IA por ti.

### Para Agentes de IA

Si has sido conectado a este workspace:

1. **NO inventes**: Busca siempre primero en `.skills/INDEX.md`.
2. **Sé preciso**: Observa los `triggers` de las skills para cargar el contexto adecuado.
3. **Obedece la Constitución**: Sigue estrictamente `AGENTES.md`.

## 📦 Instalación y Réplica

Para dotar a otro proyecto de esta inteligencia:

```bash
git clone https://github.com/malobo/ArainforIA.git
cp -r ArainforIA/.skills /ruta/a/tu/nuevo/proyecto/
```

---

## 🧠 Filosofía del Sistema: "Context as Code"

### 1. Introducción Conceptual

 En ARAINFORIA, tratamos el contexto de la IA como código fuente. En lugar de instrucciones repetitivas, utilizamos **Skills**: módulos de conocimiento versionados y estructurados.

- **Problema**: La "memoria" limitada y las alucinaciones de los LLMs.
- **Solución**: Inyección precisa de documentación técnica solo cuando se necesita.

### 2. Referencia Técnica

 Toda la documentación técnica, scripts de mantenimiento y guías de desarrollo se encuentran en el directorio `.skills/`.

 > [!IMPORTANT]
 > **Para desarrolladores y mantenedores**:
 > Consulta el [README Técnico del Sistema de Skills](.skills/README.md) para ver detalles de implementación y OpenSpec.

### 3. Flujo de Trabajo Simplificado

 1. **Consulta**: La IA busca en el índice (`.skills/INDEX.md`).
 2. **Carga**: Si detecta una tarea conocida (ej: "Analizar unidad Delphi"), carga la skill correspondiente.
 3. **Ejecución**: Sigue los pasos estrictos definidos en el archivo `.md`.

 Para regenerar el índice después de cambios:

 ```powershell
 python .skills/scripts/generate_index.py
 ```

 ---
 **Maintainer**: Manuel José López & Gemini Agent
 **Version**: 2.1.0 (Enero 2026) - *Fully Synced*
 **License**: Proprietary / ARAINFORIA Internal Use
