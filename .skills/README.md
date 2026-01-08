# Skills - Sistema de Habilidades para IA

## 📋 Descripción General

Este directorio contiene un sistema estructurado de **skills** (habilidades) diseñado para optimizar la interacción con asistentes de IA. Cada skill es un bloque de conocimiento modular que permite a la IA ejecutar tareas complejas (análisis, generación de código, despliegues) de forma consistente y eficiente.

> **Para la IA**: Si eres un asistente, lee primero la [Guía para IAs](./AI_GUIDE.md).

## 🧩 Estándar OpenSpec

Este sistema implementa la filosofía **OpenSpec** ("Context as Code"), que busca estandarizar cómo los agentes de IA consumen y ejecutan herramientas.

- **Definición Formal**: Cada skill se registra automáticamente en un catálogo YAML compatible con OpenSpec (`registry/tools.yaml`).
- **Híbrido Humano/Máquina**: Mantenemos documentación legible para humanos (`.md`) que se compila a definiciones estrictas para máquinas.
- **Interoperabilidad**: Al seguir este estándar, las skills son portables entre diferentes modelos y entornos (Gemini, ChatGPT, Claude, Local LLMs).

## 🚀 Inicio Rápido

### Invocar una Skill

Puedes invocar skills por su referencia directa o describiendo la tarea:

```markdown
@skill:domain/delphi/analyze-delphi-unit
Input: unit_path="Source/Main.pas"
```

O en lenguaje natural:
> "Analiza la unidad Main.pas usando tus skills de Delphi."

### Listar Skills

Consulta el [Índice Maestro](./INDEX.md) o ejecuta:

```markdown
@skill:registry/list
```

### Crear Nueva Skill

Copia la plantilla y sigue las [Guías de Creación](./GUIDELINES.md):

```powershell
Copy-Item ".skills/templates/skill-template.md" ".skills/domain/custom/nueva-skill.md"
```

## 📁 Estructura del Sistema

```text
.skills/
├── READMe.md               # Este archivo
├── AI_GUIDE.md             # Instrucciones críticas para la IA
├── GUIDELINES.md           # Normas de desarrollo de skills
├── INDEX.md                # Catálogo generado automáticamente
├── CHANGELOG.md            # Historial de versiones
│
├── core/                   # Capacidades transversales
│   ├── analysis/           # Validación y QA
│   ├── generation/         # Boilerplate y Tests
│   └── integration/        # Notion y herramientas externas
│
├── domain/                 # Conocimiento experto vertical
│   ├── delphi/             # Buenas prácticas Delphi/Object Pascal
│   ├── verifactu/          # Normativa Fiscal Española
│   └── database/           # Migraciones y SQL estándar
│
├── workflows/              # Procesos secuenciales
│   └── deployment/         # CI/CD y Releases
│
└── registry/               # Definiciones OpenSpec (YAML) y esquemas
```

## 📦 Instalación y Portabilidad

Este sistema es **100% portable**. Para usarlo en otro proyecto:

1. **Copiar**: Mueve toda la carpeta `.skills/` a la raíz del nuevo proyecto.
2. **Verificar**: Pide a la IA: *"He instalado el sistema de skills. Indexa las capacidades."*

No requiere dependencias externas más allá de un agente capaz de leer archivos Markdown.

## 📊 Beneficios Clave

| Métrica | Impacto |
| :--- | :--- |
| **Ahorro de Tokens** | **~60%** (Carga selectiva de contexto vs. Dump total) |
| **Precisión** | **+40%** (Reducción de alucinaciones mediante instrucciones estrictas) |
| **Velocidad** | **x2** (Ejecución guiada sin necesidad de re-explicar reglas) |

## 🔗 Recursos

- [Guía para Asistentes de IA](./AI_GUIDE.md) - **CRÍTICO**
- [Guías de Estilo y Creación](./GUIDELINES.md)
- [Índice de Skills](./INDEX.md)
- [Historial de Cambios](./CHANGELOG.md)

---
**Versión del Sistema**: 1.6.0
**Mantenedor**: ARAINFORIA Team
