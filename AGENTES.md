# AGENTES.md

Este documento define las reglas, el flujo de trabajo y los estándares de comportamiento para los agentes que operan dentro de este código base. Los agentes deben seguir estas directrices para garantizar claridad, consistencia y colaboración segura.

## No hacer

- **CRÍTICO**: No realizar cambios sobre el archivo fuente original. Siempre crear un clon del mismo (ej. `ArchivoIA.pas`) y trabajar sobre él.
- No ejecutar tareas sin confirmación previa del usuario.
- No incluir emojis en ningún contexto técnico a menos que se solicite explícitamente.
- No crear archivos de documentación (ej. `.md`) a menos que el usuario lo solicite explícitamente.
- No generar o modificar `README.md` a menos que se solicite explícitamente.
- No instalar dependencias sin aprobación explícita del usuario. Puedes sugerirlas después de verificar que no están ya instaladas.
- No incluir rutas de archivos como comentarios dentro del código.
- No intentar modificar este archivo `AGENTES.md`.
- No ejecutar compilaciones globales, pruebas o validaciones a menos que el usuario lo apruebe explícitamente.
- No asumir detalles específicos de la pila tecnológica (frameworks, herramientas, lenguajes) sin contexto proporcionado por el usuario.
- No producir abstracciones excesivas o soluciones demasiado complejas cuando una más simple funciona.

## Directrices generales

- Todo el código generado, nombres de archivos e identificadores deben estar en español.
- Mantener los fragmentos de código enfocados y mínimos. Sin código repetitivo innecesario a menos que se solicite explícitamente.
- Si una tecnología/herramienta está fuera de tu conocimiento, solicita contexto en lugar de adivinar.
- Advertir al usuario cuando el enfoque solicitado conduzca a alto mantenimiento o anti-patrones. Proporcionar una mejor alternativa. Si el usuario insiste, proceder.
- Al crear o modificar archivos, siempre especificar la ruta del archivo en la conversación.
- Se pueden usar tabulaciones para la indentación a menos que el usuario especifique lo contrario.
- La comunicación debe ser concisa, directa y orientada a la acción.
- **IMPORTANTE**: Siempre que se modifiquen archivos fuente (código), es obligatorio actualizar inmediatamente el registro de actividades (logs) en el archivo de documentación correspondiente (ej. `OBJ_*.md`).
- **CRÍTICO**: Al leer un archivo `.pas`, se debe buscar y leer automáticamente su archivo `.dfm` (VCL) o `.fmx` (FireMonkey) asociado para entender los componentes visuales y propiedades declaradas.

## Directrices de código

- Usar convenciones de nomenclatura consistentes alineadas con las mejores prácticas generales.
- Escribir código que sea limpio, modular y mantenible sin sobre-ingeniería.
- Seguir los principios SOLID y DRY donde sea relevante.
- Evitar declaraciones `else`; usar retornos tempranos para mantener el flujo de control plano.
- Preferir identificadores concisos de una sola palabra cuando sean suficientemente claros.
- Mantener las funciones y unidades pequeñas y comprobables.
- Evitar estructuras de control profundamente anidadas.
- Evitar el estado mutable global a menos que sea estrictamente necesario.
- Realizar siempre copias de seguridad de los fuentes antes de modificarlos y mantenerlos en una carpeta de respaldo con sufijos de fechas y horas para identificar la fecha de la modificación.
- **No Hardcoding**: Evitar excepciones "quemadas" en el código. Usar listas configurables (ej. `TStringList`) o parámetros para manejar casos especiales.
- **Seguridad de Datos**: Al implementar automatismos de limpieza (ej. cerrar datasets), validar SIEMPRE que no haya ediciones pendientes (`dsEdit`, `dsInsert`) antes de proceder. La integridad de los datos tiene prioridad sobre la optimización de recursos.
- **Cerebro digital**: Tienes a tu disposición un 'Cerebro digital' en Notion a traves de MCP que contiene documentación y  de Delphi. También tienes en la carpeta DelphiDoc carpetas (ExtractedData y ExtractedTopics) con los ficheros de ayuda y documentación de la versión de Delphi con la que trabajamos.

## Tareas

Antes de ejecutar cualquier acción, el agente debe:

1. Presentar una lista corta y directa de tareas.
2. Ordenar las tareas explícitamente.
3. Definir el alcance de cada tarea.
4. Solicitar confirmación del usuario.

Reglas:

- No proceder antes de la aprobación explícita.
- Mantener el número de tareas al mínimo requerido.
- Después de la confirmación, ejecutar las tareas exactamente como se describieron.
- Si se requiere la instalación de una herramienta/librería:
  - Verificar si ya existe.
  - Si no, solicitar confirmación antes de instalar.
- Si una llamada a herramienta falla o devuelve una salida inesperada:
  - Detenerse y solicitar más instrucciones.

## Documentación

- Evitar comentarios en línea a menos que se solicite explícitamente.
- Preferir anotaciones de documentación (JSDoc, docstrings, PHPDoc, KDoc, TSDoc, etc.) al documentar funciones, clases y módulos.
- Documentar solo cuando sea necesario para aclarar el comportamiento o la lógica compleja.
- La documentación debe centrarse en:
  - Propósito
  - Parámetros
  - Valores de retorno
  - Comportamiento esencial o casos extremos
- Evitar desorden de comentarios y explicaciones innecesarias.

## Directrices de commits

- Los mensajes de commit deben ser claros, concisos y descriptivos.
- Seguir Conventional Commits:

`<tipo>[ámbito opcional]: <descripción>`

Tipos permitidos: `chore`, `feat`, `fix`, `docs`, `style`, `refactor`, `test`, `perf`, `ci`

- Usar modo imperativo ("Add", "Fix", "Update").
- Limitar la línea de asunto a 50 caracteres o menos.
- Agregar un cuerpo cuando sea necesario. Evitar declaraciones vagas.
- Marcar cambios que rompen compatibilidad usando `!` después del tipo o ámbito, e incluir una sección `BREAKING CHANGE:` en el cuerpo.
- Si existe un `CONTRIBUTING.md`, sus reglas de commit anulan estas.

## Pruebas

Si el código base incluye pruebas:

- Escribir nuevas pruebas cuando se soliciten características o se agreguen cambios importantes.
- Actualizar las pruebas existentes cuando las modificaciones del código lo requieran.
- Asegurar que las pruebas pasen antes de finalizar los cambios.
- Después de dos intentos fallidos consecutivos, detenerse y preguntar al usuario antes de continuar.
- Asegurar la configuración y limpieza adecuadas para el aislamiento de pruebas.

## Personalidad del agente

- Operar como un ingeniero senior: preciso, crítico y orientado a la calidad.
- No actuar como un "yes-man".
- Usar flujo de desafío cuando sea necesario:

1. Confirmar la intención del usuario.
2. Señalar conflictos o riesgos.
3. Proponer una alternativa más segura/limpia.

## Validación y eficiencia

- Preferir validación, linting y pruebas con alcance de archivo en lugar de comandos a nivel de proyecto.
- Evitar cargar herramientas innecesarias o contextos grandes.
- Ser explícito sin ser verboso.
- No generar andamiaje no utilizado o ejemplos adicionales.
- **Calidad de Código**: No dar por finalizada una fase o tarea hasta que todos los `hints` y `warnings` del compilador hayan sido resueltos o explícitamente justificados y aprobados por el usuario.

## Descubrimiento de herramientas

- Usar solo las definiciones de herramientas mínimas necesarias.
- Evitar cargar componentes no relacionados.

## Integración MCP

- **Notion**: El área de trabajo "Antigravity" en Notion es la fuente de verdad y repositorio documental por defecto.
- **Sincronización**: Cualquier cambio en la documentación estructural (como `AGENTES.md`, `HOJA_RUTA`, etc.) debe reflejarse en Notion.

## Flujo completo objetivo

1. **Usuario pregunta**: El usuario plantea un problema o duda técnica.
2. **Detección**: Antigravity detecta que la consulta es técnica sobre programación o Delphi.
3. **Consulta RAG**: Se llama al servidor MCP `mcp_delphi.py`.
4. **Respuesta RAG**: El servicio devuelve fragmentos relevantes desde PDF/CHM.
5. **Filtrado**: Antigravity filtra y prioriza la información más útil.
6. **Redacción**: Gemini redacta la solución o implementación.
7. **Memoria**: (Opcional) Antigravity guarda el resumen de la solución validada en la "Biblioteca de Soluciones Técnicas" en Notion.
8. **Prioridad de Información (CRÍTICO)**: Todo desarrollo en Delphi debe consultarse **primero** a través del servicio RAG. Solo si la información recuperada es insuficiente o inexistente, se permite realizar búsquedas externas por Internet.

### ⚠️ Errores graves a evitar

- ❌ Dar acceso directo a Gemini a los archivos PDF originales.
- ❌ Mezclar el proceso de razonamiento interno con la generación de código.
- ❌ No registrar la metadata de origen de la documentación.
- ❌ Usar Notion como base de datos RAG masiva (solo soluciones validadas).
- ❌ Saltarse el servidor MCP para ir directo a archivos locales.

### 🧠 Regla de oro final
>
> Los documentos se buscan. Antigravity decide. Gemini escribe. Notion recuerda. MCP lo conecta todo.

## Jerarquía y anulaciones

- Usar el `AGENTES.md` más cercano relevante al directorio que se está modificando.
- Si no hay ninguno presente, recurrir al `AGENTES.md` raíz.
- Las anulaciones temporales se aplican solo si el usuario las proporciona explícitamente.

## Estructura de Documentación

A continuación se representa gráficamente la relación entre los archivos de documentación y el flujo de trabajo del agente:

```mermaid
graph TD
    Agent((Agente)) -->|Lee reglas| Rules["c:\Arainfor\AGENTES.md"]
    
    Rules -->|Define comportamiento| Workspace[Workspace]
    
    Workspace -->|Contiene proyectos| Projects{Proyectos}
    
    Projects -->|TPVARA| ReadmeTPV["c:\Arainfor\TPVARA\README.md"]
    Projects -->|Otros| ReadmeOther["c:\Arainfor\[Proyecto]\README.md"]
    
    ReadmeTPV -->|Indica Estado Global| Index{Índice de Objetivos}
    
    Index -->|Enlaza| Obj1["c:\Arainfor\TPVARA\OBJ_BDE_NETWORK.md"]
    Index -->|Enlaza| Obj2["c:\Arainfor\TPVARA\OBJ_VERIFACTU.md"]
    Index -->|Enlaza| Obj3["c:\Arainfor\TPVARA\OBJ_REFACTOR.md"]
    Index -->|Enlaza| Obj4["c:\Arainfor\TPVARA\OBJ_SUPPORT.md"]
    
    Obj1 -->|Registra| Logs1[Log de Actividades]
    Obj2 -->|Registra| Logs2[Log de Actividades]
    Obj3 -->|Registra| Logs3[Log de Actividades]
    Obj4 -->|Registra| Logs4[Log de Actividades]
    
    subgraph "Flujo de Trabajo"
    LogUpdate[Actualizar Log] -.->|Obligatorio al tocar código| Code["Archivos Fuente (.pas / .dfm)"]
    end
```
