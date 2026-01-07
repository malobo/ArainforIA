# Instalación del Sistema de Skills

El Sistema de Skills está diseñado para ser **completamente portátil**. Puedes llevar estas capacidades a cualquier otro proyecto simplemente copiando el directorio `.skills`.

## Pasos para Exportar/Instalar

1.  **Copiar Directorio**:
    Copia la carpeta completa `.skills` a la raíz de tu nuevo proyecto.

    ```bash
    cp -r /ruta/origen/.skills /ruta/destino/nuevo-proyecto/
    ```

2.  **Ignorar en Control de Versiones (Opcional)**:
    Si no deseas versionar las skills con tu código, añade `.skills/` a tu `.gitignore`.
    *Recomendación*: Versionarlas permite compartir capacidades con el equipo.

    ```gitignore
    # .gitignore
    .skills/
    ```

3.  **Verificar Instalación**:
    En tu nuevo chat con la IA, indícale que has instalado el sistema:
    > "He instalado el sistema de skills en la carpeta .skills. Por favor, indexa las capacidades disponibles."

    O ejecuta manualmente la validación si tienes acceso a CLI:
    ```yaml
    @skill:core/analysis/validate-skill-format
    skill_path: ".skills/README.md"
    ```

## Adaptación al Nuevo Proyecto

Al mover el sistema, ten en cuenta:

*   **Rutas**: Las skills usan rutas relativas dentro de la carpeta `.skills`, pero los *inputs* que proporcionas (como `unit_path`) deben ser válidos para el nuevo proyecto.
*   **Dominio**: Skills específicas como `domain/delphi` o `domain/verifactu` pueden no tener sentido en un proyecto de Python o Node.js. Puedes eliminar las carpetas de dominio que no apliquen.
*   **Configuración**: Revisa `registry/index.json` si deseas cambiar el nombre del proyecto en los metadatos, aunque no afecta la funcionalidad.

## Requisitos del Agente

Para que el sistema funcione, el agente de IA debe tener acceso a:
*   Herramientas básicas de lectura de archivos (`read_file`).
*   Capacidad de interpretar instrucciones en Markdown/YAML.

---
**Estado de Portabilidad**: ✅ 100% Portable (Rutas Relativas)
