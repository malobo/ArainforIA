# Sistema de Propuestas de Cambio

Este directorio contiene **propuestas de cambio** antes de ser implementadas.

## 🎯 Propósito

El flujo de cambios estructurado garantiza:

1. **Revisión** antes de implementar
2. **Trazabilidad** de decisiones
3. **Documentación** automática

## 📁 Estructura de una Propuesta

```
changes/
└── nombre-cambio/
    ├── proposal.md    # Descripción del cambio
    ├── tasks.md       # Tareas técnicas
    └── specs/         # Deltas a specs afectados
        └── data-schema.yaml  # Cambios al esquema
```

## 📝 Formato de Proposal.md

```markdown
# Propuesta: [Nombre del Cambio]

## Resumen
Breve descripción del cambio propuesto.

## Justificación
Por qué es necesario este cambio.

## Impacto
- Archivos afectados
- Specs a actualizar
- Riesgos identificados

## Aprobación
- [ ] Usuario aprueba
- [ ] Specs actualizados
- [ ] Código implementado
```

## 🔄 Flujo de Trabajo

```
1. Crear propuesta     →  changes/mi-cambio/proposal.md
2. Revisar con usuario →  Aprobar o rechazar
3. Si aprobado        →  Actualizar specs/
4. Implementar código →  Seguir tasks.md
5. Archivar          →  Mover a changes/archive/
```

## 📋 Formato de Deltas

Los deltas usan convención OpenSpec:

```markdown
## ADDED
### Campo: email
Se añade campo email a tabla Clientes

## MODIFIED  
### Campo: telefono
Cambia de VARCHAR(15) a VARCHAR(20)

## REMOVED
### Campo: fax
Campo obsoleto, eliminado
```

---

**Versión**: 1.0.0
