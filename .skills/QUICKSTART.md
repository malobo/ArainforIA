# Quick Start - Sistema de Skills

## 🚀 Inicio Rápido

### ¿Qué son las Skills?

Las **skills** son capacidades modulares y reutilizables que las IAs pueden ejecutar para realizar tareas específicas. Cada skill está documentada de forma exhaustiva para garantizar ejecución consistente.

### Estructura Básica

```
.skills/
├── core/           # Skills fundamentales
├── domain/         # Skills específicas del proyecto
├── workflows/      # Flujos de trabajo complejos
└── templates/      # Plantillas para crear nuevas skills
```

## 📚 Skills Disponibles

### Domain - Delphi

- **analyze-delphi-unit**: Analiza unidades .pas para identificar estructura, dependencias y mejoras

### Domain - Verifactu

- **validate-verifactu-implementation**: Valida cumplimiento completo de normativa Verifactu

### Domain - Database

- **create-database-migration**: Genera scripts de migración para Paradox con rollback

### Workflows - Deployment

- **deploy-verifactu-update**: Workflow completo de despliegue con validaciones

## 🎯 Cómo Usar una Skill

### Método 1: Referencia Directa

```markdown
Ejecuta la skill: @skill:domain/delphi/analyze-delphi-unit

Inputs:
- unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/uVerifactu.pas"
- depth: "detailed"
```

### Método 2: Descripción Natural

```markdown
Analiza la unidad uVerifactu.pas para identificar posibles mejoras
```

La IA identificará automáticamente la skill apropiada.

## 🛠️ Crear una Nueva Skill

### Paso 1: Copiar Plantilla

```powershell
Copy-Item ".skills/templates/skill-template.md" ".skills/domain/custom/mi-skill.md"
```

### Paso 2: Completar Información

Edita el archivo y completa:

- Nombre y descripción
- Inputs y outputs
- Procedimiento paso a paso
- Ejemplos de uso

### Paso 3: Registrar

Añade la skill al registro en `.skills/registry/index.json`

### Paso 4: Probar

Ejecuta la skill con casos de prueba

## 📖 Ejemplos Prácticos

### Ejemplo 1: Análisis de Código

```markdown
@skill:domain/delphi/analyze-delphi-unit

Inputs:
- unit_path: "D:/ARAINFORIA/FACARAVF/Fuente/Facturas.pas"
- depth: "deep"
- focus: ["quality", "dependencies"]

Genera un análisis profundo con recomendaciones de refactorización.
```

### Ejemplo 2: Validación Verifactu

```markdown
@skill:domain/verifactu/validate-verifactu-implementation

Inputs:
- project_path: "D:/ARAINFORIA/FACARAVF"
- validation_level: "exhaustive"
- generate_report: true

Valida cumplimiento completo y genera certificado si score >= 95.
```

### Ejemplo 3: Migración de BD

```markdown
@skill:domain/database/create-database-migration

Inputs:
- migration_name: "add_cliente_email"
- target_table: "Clientes"
- changes: [
    {type: "add_column", name: "Email", datatype: "CHAR(100)"}
  ]
- generate_rollback: true

Genera scripts de migración y rollback.
```

### Ejemplo 4: Despliegue Completo

```markdown
@skill:workflows/deployment/deploy-verifactu-update

Inputs:
- version: "2.1.0"
- environment: "production"
- executable_path: "D:/Build/FACARAVF_v2.1.0.exe"
- migration_scripts: ["migrations/20260107_*.sql"]

Ejecuta despliegue completo con validaciones y rollback disponible.
```

## 🔍 Buscar Skills

### Por Categoría

```markdown
Lista todas las skills de la categoría "domain/verifactu"
```

### Por Tag

```markdown
Lista todas las skills con tag "validation"
```

### Por Nombre

```markdown
Busca skills relacionadas con "hash" o "encadenamiento"
```

## 💡 Mejores Prácticas

### ✅ DO

- Leer la documentación completa de la skill antes de usarla
- Proporcionar todos los inputs requeridos
- Verificar precondiciones antes de ejecutar
- Revisar outputs y validar resultados

### ❌ DON'T

- Modificar skills sin actualizar documentación
- Ejecutar skills sin entender su propósito
- Ignorar errores o warnings
- Saltarse validaciones en producción

## 🆘 Solución de Problemas

### Problema: Skill no encontrada

**Solución**: Verifica que la ruta es correcta y que la skill existe en el registro

### Problema: Inputs inválidos

**Solución**: Revisa la documentación de la skill para ver inputs requeridos y su formato

### Problema: Precondiciones no cumplidas

**Solución**: Lee la sección de precondiciones y asegúrate de cumplirlas antes de ejecutar

### Problema: Resultado inesperado

**Solución**: Revisa los ejemplos de uso y compara con tu caso

## 📚 Recursos Adicionales

- [README Principal](.skills/README.md) - Visión general del sistema
- [Guías de Creación](.skills/GUIDELINES.md) - Cómo crear skills de calidad
- [Plantillas](.skills/templates/) - Plantillas para nuevas skills
- [Registro](.skills/registry/index.json) - Índice completo de skills

## 🎓 Próximos Pasos

1. **Explora** las skills disponibles en cada categoría
2. **Prueba** las skills de ejemplo con tus propios datos
3. **Crea** tus propias skills para tareas repetitivas
4. **Comparte** skills útiles con el equipo

---

**Versión**: 1.0.0  
**Última actualización**: 2026-01-07  
**Mantenedor**: Sistema de Skills
