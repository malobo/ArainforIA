---
name: update-specs
version: 2.0.0
category: core/analysis
tags: [specs, arquitectura, documentacion, sincronizar, automatico]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 6
triggers:
  - "actualizar specs"
  - "sincronizar arquitectura"
  - "documentar sistema"
  - "mapear proyecto"
  - "spec update"
---

# Update Specs (Arquitecto) v2.0

## Descripción

Analiza el código fuente y actualiza automáticamente los archivos en `specs/`. Incluye detección de cambios y modos de operación.

## AI Context

> **SYSTEM_INSTRUCTION**: Analyze source code and update spec files to reflect current state.
> **OUTPUT_FORMAT**: Updated spec files + delta summary.
> **TOKEN_STRATEGY**: Focus on structural changes, not implementation details.
> **AUTO_TRIGGER**: Check specs after significant code changes.

## Modos de Operación

### 1. Modo Completo (`/spec-update full`)

Analiza todo el proyecto y regenera specs desde cero.

```text
/spec-update full --project=FACARAVF
```

**Acciones:**

- Escanear .dpr → Listar todas las unidades
- Analizar DataModules → Extraer tablas y campos
- Mapear uses → Inventariar dependencias
- Sobrescribir specs existentes

### 2. Modo Incremental (`/spec-update`)

Detecta cambios desde última actualización.

```text
/spec-update
```

**Acciones:**

- Comparar fecha de specs vs archivos fuente
- Identificar archivos modificados
- Actualizar solo secciones afectadas
- Generar delta de cambios

### 3. Modo Validación (`/spec-validate`)

Verifica coherencia specs vs código sin modificar.

```text
/spec-validate
```

**Salida:**

```markdown
## Validación de Specs

✅ system-context.md - Sincronizado
⚠️ data-schema.yaml - 2 tablas no documentadas
❌ dependencies.md - Falta mormot.net.async
```

## Algoritmo de Detección

```pascal
procedure DetectarCambios;
begin
  // 1. Leer última fecha de actualización de specs
  UltimaActualizacion := LeerMetadataSpec('last_updated');
  
  // 2. Buscar archivos modificados después de esa fecha
  for Archivo in ProyectoArchivos do
    if Archivo.FechaModificacion > UltimaActualizacion then
      ArchivosModificados.Add(Archivo);
  
  // 3. Clasificar cambios
  for Archivo in ArchivosModificados do
  begin
    if Archivo.Extension = '.pas' then
      AnalizarUnidad(Archivo);
    if Archivo.Extension = '.dfm' then
      AnalizarFormulario(Archivo);
  end;
  
  // 4. Generar delta
  GenerarDeltaSpecs(Cambios);
end;
```

## Hooks Automáticos

### Post-Implementación

Después de implementar código significativo, recordar:

```markdown
> [!IMPORTANT]
> Se han modificado archivos que podrían afectar los specs.
> Ejecuta `/spec-update` para sincronizar documentación.
```

### Pre-Skill

Antes de ejecutar skills que modifican código:

```markdown
## Spec Requirements
- Verificar: `specs/data-schema.yaml` está actualizado
- Si no: Ejecutar `/spec-update` primero
```

## Integración con Git

### Recordatorio Pre-Commit

Añadir a `.git/hooks/pre-commit` (opcional):

```bash
#!/bin/bash
echo "⚠️ Recuerda ejecutar /spec-update si modificaste arquitectura"
```

## Formato de Delta

```yaml
# delta-2026-01-08.yaml
timestamp: 2026-01-08T13:53:00
affected_specs:
  - data-schema.yaml
  - dependencies.md

changes:
  added:
    - tabla: CONTRATOS
      campos: [ID, ID_CLIENTE, FECHA_INICIO, FECHA_FIN]
    - dependencia: mormot.net.async
  
  modified:
    - tabla: CLIENTES
      campo: EMAIL → VARCHAR(150) (antes: 100)
  
  removed:
    - dependencia: RxLib.deprecated
```

## Procedimiento Detallado

### 1. Analizar Proyecto

```pascal
// Leer archivo .dpr  
ArchivoProyecto := BuscarArchivo('*.dpr');
Unidades := ExtraerUses(ArchivoProyecto);

// Identificar DataModules
for Unidad in Unidades do
  if EsDataModule(Unidad) then
    DataModules.Add(Unidad);
```

### 2. Extraer Tablas

```pascal
for DM in DataModules do
begin
  Componentes := ExtraerComponentes(DM.ArchivoDFM);
  for Comp in Componentes do
    if (Comp is TTable) or (Comp is TQuery) then
      Tablas.Add(Comp.TableName);
end;
```

### 3. Actualizar Specs

```pascal
// Cargar spec actual
SchemaActual := CargarYAML('specs/data-schema.yaml');

// Comparar y generar delta
Delta := CompararEsquemas(SchemaActual, TablasDetectadas);

// Aplicar cambios
if Delta.HayCambios then
begin
  AplicarDelta(SchemaActual, Delta);
  GuardarYAML('specs/data-schema.yaml', SchemaActual);
  RegistrarDelta(Delta);
end;
```

## Spec Requirements

- Leer: `specs/system-context.md` (arquitectura actual)
- Leer: `specs/data-schema.yaml` (esquema actual)
- Leer: `specs/dependencies.md` (dependencias actuales)

## Spec Updates

- Actualizar: Todos los specs afectados por cambios
- Crear: `specs/deltas/` para histórico de cambios
- Notificar: Usuario de cambios significativos

## Ejemplo de Uso

```text
Usuario: "/spec-update"

IA:
1. Detecta: Bases.pas modificado hace 2 horas
2. Analiza: Nueva tabla TARIFAS con 5 campos
3. Compara: No existe en data-schema.yaml
4. Actualiza: Añade TARIFAS al esquema
5. Reporta:

   ## Specs Actualizados
   
   ### data-schema.yaml
   - ✅ Añadida tabla TARIFAS (ID, DESCRIPCION, PRECIO, IVA, ACTIVO)
   
   ### dependencies.md
   - Sin cambios
   
   Última actualización: 2026-01-08 13:53
```

## Checklist de Análisis

- [ ] Archivo .dpr leído
- [ ] DataModules identificados
- [ ] Tablas mapeadas
- [ ] Dependencias listadas
- [ ] Delta generado
- [ ] Specs actualizados
- [ ] Usuario notificado

---

**Estado**: stable  
**Versión**: 2.0.0
