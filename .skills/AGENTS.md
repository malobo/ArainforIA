# Skills System - Guía para Asistentes de IA

> Este archivo es leído automáticamente por asistentes de IA para entender el sistema de skills del proyecto.

## 🎯 Objetivo

Este proyecto usa un **sistema de skills** para estandarizar y optimizar la interacción con asistentes de IA. Las skills son documentos Markdown que definen capacidades reutilizables.

## 📁 Estructura

```
.skills/
├── INDEX.md          # Catálogo de todas las skills
├── AGENTS.md         # Este archivo (instrucciones para IAs)
├── core/             # Skills fundamentales
│   ├── analysis/     # Análisis de código
│   ├── generation/   # Generación de código
│   └── refactoring/  # Refactorización
├── domain/           # Skills específicas del dominio
│   ├── delphi/       # Desarrollo Delphi
│   ├── database/     # Bases de datos (Paradox, SQL)
│   └── verifactu/    # Normativa Verifactu España
├── workflows/        # Flujos de trabajo complejos
└── registry/         # Metadatos y aliases
```

## 🚀 Cómo Usar Skills

### Activación Automática (Triggers)

Las skills se activan cuando el usuario menciona palabras clave. Ejemplos:

| Trigger | Skill Activada |
|---------|----------------|
| "convertir sql", "paradox query" | `convert-sql-paradox` |
| "xml verifactu", "factura aeat" | `generate-verifactu-xml` |
| "imprimir factura", "ticket" | `generate-gmprint-invoice` |
| "tabla corrupta", "paradox roto" | `validate-paradox-table` |

### Invocación Explícita

El usuario puede invocar skills directamente:

```
"Usa la skill generate-api-client para crear un cliente REST"
"Ejecuta validate-dfm-integrity en mi formulario"
```

### Comandos Slash

```
/skill-list          # Listar skills disponibles
/skill-run <nombre>  # Ejecutar skill específica
/skill-help <nombre> # Ver documentación de skill
```

## 📖 Formato de Skills

Cada skill tiene:

1. **Frontmatter YAML**: Metadatos (nombre, versión, triggers)
2. **Descripción**: Qué hace la skill
3. **AI Context**: Instrucciones para la IA
4. **Procedimiento**: Pasos a seguir
5. **Ejemplos**: Casos de uso

## 🔍 Buscar Skills

1. Consultar `INDEX.md` para el catálogo completo
2. Usar `registry/aliases.md` para nombres alternativos
3. Buscar por categoría en las carpetas

## ⚡ Prioridades

Las skills marcadas con 🔴 **CRÍTICO** o **ALTA** deben cargarse siempre que el contexto sea relevante:

- `core/delphi.md` - Convenciones Delphi
- `core/verifactu.md` - Normativa Verifactu
- `core/mormot.md` - Framework mORMot2

## 🔗 Composición de Skills

Usar `skill-composer` para combinar múltiples skills en pipelines:

```
"Ejecuta pipeline nueva-funcionalidad para crear módulo Clientes"
```

## 📝 Contexto del Proyecto

- **Lenguaje principal**: Delphi (Object Pascal)
- **Base de datos**: Paradox (legacy), migrando a SQL
- **Framework**: mORMot2 para REST/JSON/Crypto
- **Impresión**: GmPrintSuite
- **Normativa**: Verifactu (facturación electrónica España)

## 🤖 Instrucciones para la IA

1. **Al recibir una solicitud**, buscar skills relevantes por triggers
2. **Cargar la skill** y seguir su procedimiento
3. **Usar el formato de salida** especificado en AI Context
4. **Aplicar convenciones Delphi** del proyecto
5. **Documentar cambios** en formato estructurado

---

**Versión**: 1.8.0  
**Última actualización**: 2026-01-08
