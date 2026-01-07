---
name: create-database-migration
version: 1.0.0
category: domain/database
tags: [database, migration, paradox, bde]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 6
estimated_tokens: 600-1000
---

# Crear Migración de Base de Datos

## Descripción

Genera scripts de migración para bases de datos Paradox (BDE) con versionamiento y rollback.

## Objetivo

Facilitar la evolución del esquema de base de datos de forma controlada, versionada y reversible, específicamente para tablas Paradox utilizadas en proyectos Delphi.

## Inputs

- **migration_name** (string): Nombre descriptivo de la migración (ej: "add_verifactu_fields")
- **target_table** (string): Nombre de la tabla a modificar
- **changes** (array): Lista de cambios a aplicar
- **generate_rollback** (boolean, opcional): Generar script de rollback (default: true)

## Outputs

- **migration_script** (sql): Script SQL de migración
- **rollback_script** (sql, opcional): Script de rollback
- **documentation** (markdown): Documentación de la migración
- **version_number** (string): Número de versión asignado

## Precondiciones

- Base de datos Paradox accesible
- BDE configurado correctamente
- Backup de la tabla objetivo realizado

## Postcondiciones

- Script de migración generado y documentado
- Script de rollback disponible
- Migración registrada en tabla de control de versiones
- Documentación actualizada

## Procedimiento

### Paso 1: Validar Inputs

Verificar que:

- El nombre de migración es único
- La tabla objetivo existe
- Los cambios son válidos para Paradox

**Validación**: Todos los inputs son válidos

### Paso 2: Generar Número de Versión

Formato: `YYYYMMDD_HHMMSS_migration_name`

Ejemplo: `20260107_110316_add_verifactu_fields`

**Validación**: Número de versión único

### Paso 3: Crear Script de Migración

Generar SQL compatible con Paradox/BDE:

```sql
-- Migration: add_verifactu_fields
-- Version: 20260107_110316
-- Date: 2026-01-07 11:03:16
-- Description: Añade campos requeridos por Verifactu

-- Añadir campos
ALTER TABLE Facturas
  ADD HashAnterior CHAR(64),
  ADD HashActual CHAR(64),
  ADD FirmaDigital CHAR(512),
  ADD QRCode BLOB,
  ADD EstadoVerifactu CHAR(20);

-- Crear índices
CREATE INDEX idx_hash_actual ON Facturas (HashActual);

-- Valores por defecto para registros existentes
UPDATE Facturas
  SET EstadoVerifactu = 'PENDIENTE'
  WHERE EstadoVerifactu IS NULL;

-- Registrar migración
INSERT INTO MigracionesDB (Version, Nombre, FechaAplicacion, Estado)
VALUES ('20260107_110316', 'add_verifactu_fields', NOW(), 'APLICADA');
```

**Validación**: Script sintácticamente correcto

### Paso 4: Crear Script de Rollback

Generar script inverso:

```sql
-- Rollback: add_verifactu_fields
-- Version: 20260107_110316

-- Eliminar índices
DROP INDEX idx_hash_actual ON Facturas;

-- Eliminar campos
ALTER TABLE Facturas
  DROP COLUMN HashAnterior,
  DROP COLUMN HashActual,
  DROP COLUMN FirmaDigital,
  DROP COLUMN QRCode,
  DROP COLUMN EstadoVerifactu;

-- Actualizar registro de migración
UPDATE MigracionesDB
  SET Estado = 'REVERTIDA', FechaReversion = NOW()
  WHERE Version = '20260107_110316';
```

**Validación**: Rollback revierte todos los cambios

### Paso 5: Generar Documentación

Crear archivo markdown con:

```markdown
# Migración: add_verifactu_fields

## Información
- **Versión**: 20260107_110316
- **Fecha**: 2026-01-07
- **Autor**: Sistema
- **Tipo**: Schema Change

## Descripción
Añade los campos necesarios para el cumplimiento de Verifactu en la tabla de facturas.

## Cambios Aplicados
1. **Nuevos Campos**:
   - `HashAnterior` (CHAR 64): Hash de la factura anterior en la cadena
   - `HashActual` (CHAR 64): Hash de esta factura
   - `FirmaDigital` (CHAR 512): Firma electrónica de la factura
   - `QRCode` (BLOB): Imagen del código QR
   - `EstadoVerifactu` (CHAR 20): Estado del registro Verifactu

2. **Índices**:
   - `idx_hash_actual`: Índice en campo HashActual para búsquedas rápidas

3. **Datos**:
   - Inicialización de EstadoVerifactu a 'PENDIENTE' para registros existentes

## Impacto
- **Tablas afectadas**: Facturas
- **Registros afectados**: Todos los existentes
- **Downtime estimado**: < 1 minuto
- **Tamaño adicional**: ~150 bytes por registro

## Dependencias
- Ninguna

## Rollback
Disponible en: `migrations/rollback/20260107_110316_add_verifactu_fields_rollback.sql`

## Testing
- [ ] Migración aplicada en entorno de desarrollo
- [ ] Datos existentes preservados
- [ ] Nuevos campos accesibles
- [ ] Rollback probado exitosamente

## Notas
- Realizar backup antes de aplicar
- Verificar espacio en disco disponible
- Actualizar código de aplicación para usar nuevos campos
```

**Validación**: Documentación completa

### Paso 6: Registrar en Control de Versiones

Actualizar tabla de control:

```sql
CREATE TABLE IF NOT EXISTS MigracionesDB (
  Version CHAR(50) PRIMARY KEY,
  Nombre CHAR(100),
  FechaAplicacion DATETIME,
  FechaReversion DATETIME,
  Estado CHAR(20),
  Descripcion MEMO
);
```

**Validación**: Migración registrada

## Ejemplos de Uso

### Ejemplo 1: Añadir Campos

**Contexto**: Necesidad de añadir campos Verifactu

**Input**:

```
migration_name: "add_verifactu_fields"
target_table: "Facturas"
changes: [
  {type: "add_column", name: "HashActual", datatype: "CHAR(64)"},
  {type: "add_column", name: "HashAnterior", datatype: "CHAR(64)"}
]
generate_rollback: true
```

**Output**:

- `migrations/20260107_110316_add_verifactu_fields.sql`
- `migrations/rollback/20260107_110316_add_verifactu_fields_rollback.sql`
- `migrations/docs/20260107_110316_add_verifactu_fields.md`

### Ejemplo 2: Modificar Tipo de Dato

**Contexto**: Ampliar tamaño de campo

**Input**:

```
migration_name: "expand_nif_field"
target_table: "Clientes"
changes: [
  {type: "modify_column", name: "NIF", old_type: "CHAR(9)", new_type: "CHAR(20)"}
]
```

**Output**:
Scripts de migración y rollback generados

## Manejo de Errores

### Error 1: Tabla no existe

**Síntoma**: Error al generar script
**Causa**: Nombre de tabla incorrecto
**Solución**: Verificar nombre exacto de la tabla

### Error 2: Tipo de dato incompatible

**Síntoma**: Script no se puede ejecutar
**Causa**: Tipo de dato no soportado por Paradox
**Solución**: Usar tipos compatibles (CHAR, INTEGER, FLOAT, DATE, BLOB, MEMO)

### Error 3: Conflicto de versión

**Síntoma**: Número de versión duplicado
**Causa**: Migración con mismo timestamp
**Solución**: Esperar 1 segundo y regenerar

## Optimizaciones

### Optimización de Tokens

- Generar solo documentación esencial en modo básico
- Reutilizar plantillas de migración comunes

### Optimización de Rendimiento

- Aplicar migraciones en lotes durante mantenimiento
- Usar transacciones cuando sea posible
- Indexar después de insertar datos masivos

## Dependencias

### Skills Requeridas

- `@skill:domain/database/backup-table` - Backup antes de migrar

### Herramientas Externas

- **Database Desktop** (Delphi): Para ejecutar scripts manualmente
- **BDE Administrator**: Para configuración

## Métricas de Éxito

- [ ] Script ejecuta sin errores
- [ ] Todos los cambios aplicados correctamente
- [ ] Rollback funciona correctamente
- [ ] Datos existentes preservados
- [ ] Documentación completa

## Notas

- Siempre hacer backup antes de aplicar migraciones
- Probar en entorno de desarrollo primero
- Paradox tiene limitaciones en ALTER TABLE (no todas las operaciones soportadas)
- Considerar recrear tabla si ALTER TABLE no es posible

## Referencias

- [Paradox Table Specifications](https://docwiki.embarcadero.com/RADStudio/en/Paradox_Table_Specifications)
- [BDE SQL Reference](https://docwiki.embarcadero.com/RADStudio/en/BDE_SQL_Reference)

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial de la skill
- Soporte para ADD COLUMN, DROP COLUMN, MODIFY COLUMN
- Generación automática de rollback
- Sistema de versionamiento integrado

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
