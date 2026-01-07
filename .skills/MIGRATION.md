---
name: migration-guide
version: 1.0.0
category: documentation
tags: [migration, export, import, portability]
---

# 📦 Guía de Migración y Portabilidad

## ¿Es el sistema portable?

**SÍ.** El sistema de skills ha sido diseñado para ser completamente portable.

- ✅ **Rutas Relativas**: Todas las referencias internas en `registry/index.json` y archivos Markdown son relativas.
- 📂 **Auto-contenido**: Todo lo necesario está dentro de la carpeta `.skills`.
- 🔄 **Independiente**: No hay dependencias hardcodeadas a `D:/ARAINFORIA`.

## Cómo Exportar/Mover el Sistema

### Opción A: Copiar la carpeta (Recomendado)

Simplemente copia toda la carpeta `.skills` a la raíz de tu nuevo proyecto o computador.

```text
Origen:  D:/ARAINFORIA/.skills
Destino: C:/NuevoProyecto/.skills
```

### Opción B: Git Submodule

Si usas Git, puedes convertir el sistema de skills en un repositorio independiente y añadirlo como submódulo.

```bash
git submodule add https://github.com/tu-usuario/ai-skills-system.git .skills
```

### Opción C: Archivo ZIP

Comprime la carpeta `.skills` y descomprímela donde quieras.

## Requisitos en el Nuevo Entorno

Para que el sistema funcione al 100% en el nuevo ordenador, asegúrate de:

1. **Agente AI**: El agente debe tener acceso de lectura/escritura a la carpeta `.skills`.
2. **Notion (Opcional)**:
    - Si usas las skills de integración (`core/integration/*`), el nuevo entorno debe tener el servidor MCP de Notion configurado.
    - Si no lo tiene, esas skills específicas fallarán, pero el resto (Delphi, Verifactu, etc.) funcionarán perfectamente.
3. **Herramientas Externas**:
    - Las skills de Delphi asumen que tienes Delphi instalado si piden compilar.
    - Las skills de Base de Datos asumen que tienes acceso a los archivos de datos.

## Verificación Post-Migración

Una vez copiada la carpeta, pide a tu IA que ejecute un chequeo de salud:

```text
"Ejecuta la skill validate-system-health"
```

Si el resultado es 🟢 SANO, la migración ha sido un éxito total.
