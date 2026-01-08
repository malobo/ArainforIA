# Sistema de Especificaciones (Specs)

Este directorio contiene la **"Verdad del Sistema"** - documentación estructurada que describe el estado actual del proyecto.

## 🎯 Propósito

Las specs permiten a la IA:

1. **Entender** el contexto antes de generar código
2. **Validar** que los cambios son coherentes con la arquitectura
3. **Mantener** documentación sincronizada con el código

## 📁 Estructura

```
specs/
├── README.md           # Este archivo
├── system-context.md   # Arquitectura general
├── data-schema.yaml    # Esquema de datos (tablas/campos)
├── dependencies.md     # Componentes y librerías
└── api-contracts/      # Contratos de API (si aplica)
```

## 📝 Formato de Specs

### Metadatos (Frontmatter)

```yaml
---
spec: nombre-del-spec
version: 1.0.0
last_updated: 2026-01-08
updated_by: AI/Usuario
status: current | draft | deprecated
---
```

### Cambios de Specs

Los cambios a specs **nunca se hacen directamente**. El flujo es:

1. Crear propuesta en `../changes/nombre-cambio/`
2. Usuario aprueba la propuesta
3. AI aplica cambios a specs
4. Se archiva la propuesta

## 🔗 Relación con Skills

Las skills pueden:

- **Leer** specs para obtener contexto
- **Requerir** specs específicos antes de ejecutar
- **Actualizar** specs después de cambios significativos

Ejemplo en skill:

```markdown
## Spec Requirements
- Leer: `specs/data-schema.yaml`
- Requiere: Lista de tablas de Clientes

## Spec Updates
- Actualizar: `specs/system-context.md` si se añade nueva funcionalidad
```

## ⚡ Comandos

```
/spec-list          # Listar specs disponibles
/spec-view <nombre> # Ver un spec
/spec-update        # Actualizar specs desde código
```

---

**Versión**: 1.0.0
