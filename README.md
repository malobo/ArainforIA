# ArainforIA Workspace & AI Skills System

Este repositorio centraliza el **Sistema de Habilidades (Skills System)** y la documentación de desarrollo para el ecosistema de proyectos Delphi de ARAINFORIA.

Actúa como el "cerebro" y la base de conocimientos para los asistentes de IA que trabajan en los proyectos de facturación y gestión (FACARAVF, GESFAC, etc.).

## 🧠 Sistema de Skills (v1.1)

El núcleo de este repositorio es el directorio `.skills/`, un framework modular diseñado para estandarizar y potenciar las capacidades de los agentes de IA (como Gemini, ChatGPT, Claude).

### Características Principales

*   **Portabilidad Total**: El sistema puede copiarse a cualquier otro proyecto y funcionar inmediatamente.
*   **Contexto de Alta Densidad**: Instrucciones optimizadas (`SYSTEM_INSTRUCTION`) para reducir el consumo de tokens y aumentar la precisión.
*   **Mapeo de Herramientas**: Vinculación explícita entre acciones abstractas (ej: "Analizar") y herramientas concretas del CLI.
*   **Automatización**: Workflows parametrizados para tareas complejas como despliegues o migraciones.

### Estructura del Sistema

```text
.skills/
├── core/           # Capacidades fundamentales (Análisis, Generación, Refactorización)
├── domain/         # Conocimiento específico (Delphi, Verifactu, Bases de Datos)
├── workflows/      # Procesos complejos paso a paso (Despliegues, Migraciones)
└── registry/       # Índice y metadatos para descubrimiento automático
```

### Skills Destacadas

1.  **`domain/delphi/analyze-delphi-unit`**: Análisis estático profundo de código Delphi, detectando dependencias y complejidad.
2.  **`workflows/deployment/deploy-verifactu-update`**: Flujo seguro para desplegar actualizaciones críticas de normativa Verifactu con rollback automático.
3.  **`core/analysis/validate-skill-format`**: Meta-skill que asegura la calidad y consistencia del propio sistema.

## 📂 Proyectos Relacionados

Este sistema da soporte al desarrollo de aplicaciones de gestión en Delphi

## 🚀 Cómo usar este repositorio

### Para Humanos

Este repositorio sirve como documentación viva de las metodologías y estándares de desarrollo de ARAINFORIA. Consulta `log_gemini.md` para ver el historial de decisiones técnicas.

### Para Agentes de IA

Si eres una IA y has sido "conectada" a este repositorio:

1.  Lee `.skills/README.md` para entender tus capacidades.
2.  Consulta `.skills/registry/index.json` para descubrir qué herramientas tienes disponibles.
3.  Usa los workflows definidos en `.skills/workflows/` para ejecutar tareas complejas sin errores.

## 🛠️ Instalación en otros entornos

Para utilizar este sistema de skills en otro proyecto:

```bash
# Copia la carpeta .skills a la raíz de tu proyecto
cp -r ARAINFORIA/.skills /ruta/a/tu/proyecto/
```

Consulta [INSTALL.md](.skills/INSTALL.md) para más detalles.

---
**Maintainer**: Manuel José López & Gemini Agent
**License**: Proprietary / ARAINFORIA Internal Use
