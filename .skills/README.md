# Skills - Sistema de Habilidades para IA

## 📋 Descripción General

Este directorio contiene un sistema estructurado de **skills** (habilidades) diseñado para optimizar la interacción con asistentes de IA. Cada skill representa una capacidad específica que puede ser invocada, reutilizada y mejorada iterativamente.

## 🎯 Objetivos

- **Modularidad**: Cada skill es independiente y reutilizable
- **Claridad**: Documentación explícita de entradas, salidas y comportamiento
- **Eficiencia**: Optimización de tokens y contexto
- **Escalabilidad**: Fácil adición de nuevas skills
- **Versionamiento**: Control de cambios y mejoras

## 📁 Estructura de Directorios

```
.skills/
├── README.md                    # Este archivo
├── GUIDELINES.md                # Guías de creación de skills
├── core/                        # Skills fundamentales
│   ├── analysis/               # Análisis de código y proyectos
│   ├── generation/             # Generación de código
│   ├── refactoring/            # Refactorización
│   └── documentation/          # Documentación automática
├── domain/                      # Skills específicas del dominio
│   ├── delphi/                 # Específicas de Delphi
│   ├── database/               # Gestión de bases de datos
│   └── verifactu/              # Normativa Verifactu
├── workflows/                   # Flujos de trabajo complejos
│   ├── deployment/             # Despliegue
│   ├── testing/                # Testing
│   └── migration/              # Migraciones
├── templates/                   # Plantillas reutilizables
│   ├── skill-template.md       # Plantilla para nuevas skills
│   └── workflow-template.md    # Plantilla para workflows
└── registry/                    # Registro de skills disponibles
    ├── index.json              # Índice de todas las skills
    └── metadata/               # Metadatos de cada skill
```

## 🚀 Uso Rápido

### Instalación en Nuevo Proyecto

Ver [INSTALL.md](./INSTALL.md) para instrucciones detalladas de portabilidad.

### Invocar una Skill

```markdown
@skill:core/analysis/code-review
```

### Listar Skills Disponibles

```markdown
@skill:registry/list
```

### Crear una Nueva Skill

```markdown
@skill:templates/create --name="mi-nueva-skill" --category="domain/custom"
```

## 📖 Convenciones

1. **Nombres**: kebab-case (ejemplo: `validate-nif`)
2. **Categorías**: Usar la estructura de directorios existente
3. **Versionamiento**: Seguir Semantic Versioning (MAJOR.MINOR.PATCH)
4. **Documentación**: Cada skill debe tener su README.md

## 🔗 Enlaces Útiles

- [Guías de Creación](./GUIDELINES.md)
- [Plantilla de Skill](./templates/skill-template.md)
- [Registro de Skills](./registry/index.json)

## 📝 Notas

- Las skills están diseñadas para ser agnósticas del modelo de IA
- Se recomienda usar skills atómicas y componerlas en workflows
- Mantener las skills actualizadas con las mejores prácticas del proyecto
