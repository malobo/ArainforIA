---
name: deploy-verifactu-update
version: 1.1.0
category: workflows/deployment
tags: [deployment, verifactu, production]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 8
estimated_duration: 30-45 minutos
estimated_tokens: 2000-3000
---

# Desplegar Actualización Verifactu

## Descripción

Workflow completo para desplegar actualizaciones relacionadas con Verifactu en producción de forma segura y controlada.

## Objetivo

Realizar un despliegue sin errores de actualizaciones de Verifactu, minimizando downtime y asegurando rollback disponible en caso de problemas.

## AI Context

> **SYSTEM_INSTRUCTION**: Execute sequential deployment phases. STOP on any failure. Use parameterized paths.
> **OUTPUT_FORMAT**: Log logging every step + Final Report.
> **CRITICAL**: Confirm user approval before modifying DB or overwriting binaries.

## Alcance

**Incluye**:

- Backup completo de base de datos
- Migración de esquema de BD
- Despliegue de ejecutable actualizado
- Validación post-despliegue
- Documentación de cambios

**Excluye**:

- Configuración inicial de Verifactu (usar otro workflow)
- Migración de datos históricos (requiere workflow específico)
- Actualización de certificados digitales (proceso manual)

## Inputs Globales

- **version** (string): Versión a desplegar (ej: "2.1.0")
- **environment** (string): Entorno destino [development|staging|production]
- **executable_path** (string): Ruta al ejecutable compilado
- **migration_scripts** (array): Lista de scripts de migración a aplicar
- **skip_backup** (boolean, opcional): Saltar backup (solo para dev, default: false)

## Outputs Globales

- **deployment_report** (markdown): Reporte completo del despliegue
- **rollback_package** (zip): Paquete completo para rollback
- **validation_results** (json): Resultados de validación post-despliegue
- **deployment_log** (txt): Log detallado de todas las operaciones

## Tool Mapping

- **Backup**: `@skill:domain/database/backup-database`
- **Migración**: `@skill:domain/database/create-database-migration`
- **Verificación**: `run_shell_command` (powershell Checksum/Test-Path)
- **Log**: `write_file` (deployment.log)

## Precondiciones Globales

- Ejecutable compilado y probado en staging
- Scripts de migración validados
- Certificados digitales vigentes
- Backup storage disponible (mínimo 2GB libre)
- Acceso administrativo al servidor/estación
- Usuarios notificados del mantenimiento

## Skills Utilizadas

1. `@skill:domain/database/backup-database` - Backup completo de BD
2. `@skill:domain/database/create-database-migration` - Aplicar migraciones
3. `@skill:domain/verifactu/validate-verifactu-implementation` - Validar cumplimiento
4. `@skill:core/documentation/generate-changelog` - Generar changelog

## Diagrama de Flujo

```
[Inicio]
   ↓
[Fase 1: Pre-Despliegue]
   ↓
[Fase 2: Backup]
   ↓
[Fase 3: Detener Aplicación]
   ↓
[Fase 4: Migración BD]
   ↓
[Fase 5: Despliegue Ejecutable]
   ↓
[Fase 6: Validación]
   ↓
[¿Validación OK?] → [NO] → [Rollback] → [Fin con Error]
   ↓
  [SÍ]
   ↓
[Fase 7: Iniciar Aplicación]
   ↓
[Fase 8: Validación Post-Inicio]
   ↓
[¿Todo OK?] → [NO] → [Rollback] → [Fin con Error]
   ↓
  [SÍ]
   ↓
[Fase 9: Documentación]
   ↓
[Fin Exitoso]
```

## Fases del Workflow

### Fase 1: Pre-Despliegue

**Objetivo**: Verificar que todo está listo para el despliegue

**Skills**: Ninguna (verificaciones manuales)

**Pasos**:

1. Verificar que la versión a desplegar está compilada y en la ruta correcta
2. Confirmar que todos los scripts de migración están disponibles
3. Verificar espacio en disco disponible (mínimo 2GB)
4. Confirmar que no hay usuarios conectados (o notificarlos)
5. Crear carpeta de despliegue con timestamp: `Deployments/YYYYMMDD_HHMMSS_vX.X.X/`

**Inputs de Fase**:

- Todos los inputs globales

**Outputs de Fase**:

- `deployment_folder`: Ruta a carpeta de despliegue creada
- `pre_check_passed`: Boolean indicando si pasó las verificaciones

**Criterios de Éxito**:

- [ ] Ejecutable existe y es accesible
- [ ] Scripts de migración validados
- [ ] Espacio en disco suficiente
- [ ] Carpeta de despliegue creada

**Punto de Control**: Si alguna verificación falla, DETENER el workflow

---

### Fase 2: Backup Completo

**Objetivo**: Crear backup completo de BD y ejecutable actual

**Skills**:

- `@skill:domain/database/backup-database`

**Pasos**:

1. Crear backup de todas las tablas Paradox en carpeta DATOS
2. Copiar ejecutable actual a carpeta de backup
3. Copiar archivos de configuración (.ini, .cfg)
4. Generar checksum MD5 de todos los archivos respaldados
5. Comprimir backup en archivo ZIP con timestamp

```powershell
# Ejemplo de comandos
$backupPath = "Deployments/{{timestamp}}_{{version}}/backup/"
Copy-Item "DATOS\*.db" -Destination $backupPath
Copy-Item "FACARAVF.exe" -Destination $backupPath
Copy-Item "*.ini" -Destination $backupPath
Compress-Archive -Path $backupPath -DestinationPath "backup_{{timestamp}}.zip"
```

**Inputs de Fase**:

- `deployment_folder` (de Fase 1)

**Outputs de Fase**:

- `backup_path`: Ruta al archivo ZIP de backup
- `backup_checksum`: MD5 del backup
- `backup_size`: Tamaño del backup en MB

**Criterios de Éxito**:

- [ ] Todas las tablas respaldadas
- [ ] Ejecutable actual respaldado
- [ ] Archivo ZIP creado correctamente
- [ ] Checksum generado

**Punto de Control**: Verificar que el backup es válido y restaurable

---

### Fase 3: Detener Aplicación

**Objetivo**: Cerrar la aplicación de forma ordenada

**Skills**: Ninguna

**Pasos**:

1. Si es aplicación multi-usuario, notificar a usuarios activos
2. Esperar 30 segundos para que usuarios guarden trabajo
3. Cerrar aplicación (kill process si es necesario)
4. Verificar que no hay procesos de la aplicación ejecutándose
5. Verificar que no hay archivos de BD bloqueados

```powershell
# Ejemplo
Get-Process "FACARAVF" -ErrorAction SilentlyContinue | Stop-Process -Force
Start-Sleep -Seconds 5
# Verificar
if (Get-Process "FACARAVF" -ErrorAction SilentlyContinue) {
    Write-Error "No se pudo detener la aplicación"
    exit 1
}
```

**Inputs de Fase**:

- Ninguno

**Outputs de Fase**:

- `app_stopped`: Boolean confirmando que la app está detenida

**Criterios de Éxito**:

- [ ] Aplicación completamente cerrada
- [ ] No hay procesos residuales
- [ ] Archivos de BD no están bloqueados

**Punto de Control**: Si la aplicación no se puede detener, ABORTAR

---

### Fase 4: Migración de Base de Datos

**Objetivo**: Aplicar scripts de migración al esquema de BD

**Skills**:

- `@skill:domain/database/create-database-migration`

**Pasos**:

1. Para cada script de migración en orden:
   a. Leer script SQL
   b. Ejecutar usando Database Desktop o BDE
   c. Verificar que se ejecutó correctamente
   d. Registrar en tabla MigracionesDB
   e. Log de resultado

2. Verificar integridad de datos post-migración
3. Actualizar índices si es necesario
4. Compactar tablas si es necesario

```sql
-- Ejemplo de ejecución
-- Script 1: 20260107_110316_add_verifactu_fields.sql
ALTER TABLE Facturas ADD HashActual CHAR(64);
-- Verificar
SELECT COUNT(*) FROM Facturas; -- Debe retornar mismo número que antes

-- Script 2: 20260107_110320_create_log_table.sql
CREATE TABLE LogVerifactu (
  ID AUTOINC,
  Fecha DATETIME,
  Evento CHAR(100),
  Detalle MEMO
);
```

**Inputs de Fase**:

- `migration_scripts` (global)

**Outputs de Fase**:

- `migrations_applied`: Lista de migraciones aplicadas exitosamente
- `migration_errors`: Lista de errores si los hubo

**Criterios de Éxito**:

- [ ] Todos los scripts ejecutados sin errores
- [ ] Tabla MigracionesDB actualizada
- [ ] Integridad de datos verificada
- [ ] Número de registros preservado

**Punto de Control**: Si alguna migración falla, ejecutar ROLLBACK de BD

---

### Fase 5: Despliegue de Ejecutable

**Objetivo**: Reemplazar ejecutable antiguo con la nueva versión

**Skills**: Ninguna

**Pasos**:

1. Renombrar ejecutable actual a `FACARAVF_old.exe`
2. Copiar nuevo ejecutable a carpeta de producción
3. Copiar archivos auxiliares si los hay (.dll, .bpl)
4. Verificar que el ejecutable es válido (checksum, firma digital)
5. Actualizar archivo de versión

```powershell
# Ejemplo
Rename-Item "FACARAVF.exe" "FACARAVF_old.exe"
Copy-Item "D:\Build\FACARAVF_v2.1.0.exe" "FACARAVF.exe"
# Verificar
if (!(Test-Path "FACARAVF.exe")) {
    Write-Error "Fallo al copiar ejecutable"
    # Restaurar
    Rename-Item "FACARAVF_old.exe" "FACARAVF.exe"
    exit 1
}
```

**Inputs de Fase**:

- `executable_path` (global)

**Outputs de Fase**:

- `executable_deployed`: Boolean confirmando despliegue
- `old_executable_path`: Ruta al ejecutable antiguo

**Criterios de Éxito**:

- [ ] Nuevo ejecutable copiado correctamente
- [ ] Ejecutable antiguo preservado
- [ ] Archivos auxiliares actualizados
- [ ] Permisos de ejecución correctos

**Punto de Control**: Verificar que el nuevo ejecutable es válido

---

### Fase 6: Validación Pre-Inicio

**Objetivo**: Validar que todo está correcto antes de iniciar la aplicación

**Skills**:

- `@skill:domain/verifactu/validate-verifactu-implementation`

**Pasos**:

1. Validar esquema de BD (campos requeridos presentes)
2. Validar integridad de cadena de hashes
3. Verificar que archivos de configuración son correctos
4. Validar certificados digitales (vigencia)
5. Ejecutar validación Verifactu en modo "basic"

**Inputs de Fase**:

- Ninguno

**Outputs de Fase**:

- `validation_score`: Puntuación de validación (0-100)
- `validation_issues`: Lista de problemas encontrados

**Criterios de Éxito**:

- [ ] Esquema de BD correcto
- [ ] Cadena de hashes íntegra
- [ ] Configuración válida
- [ ] Puntuación validación >= 90

**Punto de Control**: Si validación < 90, ejecutar ROLLBACK completo

---

### Fase 7: Iniciar Aplicación

**Objetivo**: Iniciar la aplicación actualizada

**Skills**: Ninguna

**Pasos**:

1. Iniciar aplicación en modo normal
2. Esperar 10 segundos para que cargue completamente
3. Verificar que el proceso está ejecutándose
4. Verificar que no hay errores en logs de inicio
5. Verificar conexión a BD

```powershell
# Ejemplo
Start-Process "FACARAVF.exe"
Start-Sleep -Seconds 10
if (!(Get-Process "FACARAVF" -ErrorAction SilentlyContinue)) {
    Write-Error "La aplicación no inició correctamente"
    # Ejecutar rollback
    exit 1
}
```

**Inputs de Fase**:

- Ninguno

**Outputs de Fase**:

- `app_started`: Boolean confirmando inicio
- `startup_time`: Tiempo que tardó en iniciar (segundos)

**Criterios de Éxito**:

- [ ] Aplicación inició sin errores
- [ ] Proceso ejecutándose
- [ ] Conexión a BD establecida
- [ ] No hay errores críticos en logs

**Punto de Control**: Si la aplicación no inicia, ejecutar ROLLBACK

---

### Fase 8: Validación Post-Inicio

**Objetivo**: Verificar que la aplicación funciona correctamente

**Skills**:

- `@skill:domain/verifactu/validate-verifactu-implementation`

**Pasos**:

1. Ejecutar validación Verifactu completa
2. Crear una factura de prueba
3. Verificar generación de hash
4. Verificar generación de QR
5. Verificar impresión de factura
6. Anular factura de prueba
7. Verificar logs de eventos

**Inputs de Fase**:

- Ninguno

**Outputs de Fase**:

- `post_validation_score`: Puntuación de validación (0-100)
- `functional_tests_passed`: Boolean de pruebas funcionales

**Criterios de Éxito**:

- [ ] Validación Verifactu >= 95
- [ ] Factura de prueba creada correctamente
- [ ] Hash y QR generados correctamente
- [ ] Impresión funcional
- [ ] Logs registrando eventos

**Punto de Control**: Si validación < 95 o pruebas fallan, ejecutar ROLLBACK

---

### Fase 9: Documentación y Cierre

**Objetivo**: Documentar el despliegue y notificar

**Skills**:

- `@skill:core/documentation/generate-changelog`

**Pasos**:

1. Generar reporte de despliegue
2. Actualizar changelog del proyecto
3. Registrar despliegue en sistema de control
4. Notificar a usuarios que el sistema está disponible
5. Archivar logs y reportes
6. Eliminar ejecutable antiguo (después de 7 días de estabilidad)

**Inputs de Fase**:

- Todos los outputs de fases anteriores

**Outputs de Fase**:

- `deployment_report` (global)
- `deployment_log` (global)

**Criterios de Éxito**:

- [ ] Reporte generado
- [ ] Changelog actualizado
- [ ] Usuarios notificados
- [ ] Logs archivados

**Punto de Control**: Documentación completa

---

## Ejemplo Completo de Ejecución

### Contexto

Despliegue de versión 2.1.0 que añade funcionalidad completa de Verifactu en producción.

### Inputs Iniciales

```json
{
  "version": "2.1.0",
  "environment": "production",
  "executable_path": "D:/Build/FACARAVF_v2.1.0.exe",
  "migration_scripts": [
    "migrations/20260107_110316_add_verifactu_fields.sql",
    "migrations/20260107_110320_create_log_table.sql"
  ],
  "skip_backup": false
}
```

### Ejecución Paso a Paso

#### Fase 1: Pre-Despliegue

```powershell
# Verificaciones
Test-Path "D:/Build/FACARAVF_v2.1.0.exe" # True
Get-PSDrive C | Select-Object Free # 15GB disponible
New-Item -ItemType Directory -Path "Deployments/20260107_110316_v2.1.0"
```

**Resultado**: ✓ Pre-checks pasados

#### Fase 2: Backup

```powershell
# Backup completo
Copy-Item "DATOS\*.db" "Deployments/20260107_110316_v2.1.0/backup/"
Copy-Item "FACARAVF.exe" "Deployments/20260107_110316_v2.1.0/backup/"
Compress-Archive -Path "Deployments/20260107_110316_v2.1.0/backup/" -DestinationPath "backup_20260107_110316.zip"
```

**Resultado**: ✓ Backup creado (245MB)

#### Fase 3: Detener Aplicación

```powershell
Stop-Process -Name "FACARAVF" -Force
```

**Resultado**: ✓ Aplicación detenida

#### Fase 4: Migración BD

```sql
-- Ejecutar scripts
ALTER TABLE Facturas ADD HashActual CHAR(64);
CREATE TABLE LogVerifactu (...);
```

**Resultado**: ✓ 2 migraciones aplicadas exitosamente

#### Fase 5: Despliegue Ejecutable

```powershell
Rename-Item "FACARAVF.exe" "FACARAVF_old.exe"
Copy-Item "D:/Build/FACARAVF_v2.1.0.exe" "FACARAVF.exe"
```

**Resultado**: ✓ Ejecutable desplegado

#### Fase 6: Validación Pre-Inicio

```
@skill:domain/verifactu/validate-verifactu-implementation
```

**Resultado**: ✓ Puntuación: 92/100

#### Fase 7: Iniciar Aplicación

```powershell
Start-Process "FACARAVF.exe"
```

**Resultado**: ✓ Aplicación iniciada en 8 segundos

#### Fase 8: Validación Post-Inicio

```
@skill:domain/verifactu/validate-verifactu-implementation (exhaustive)
```

**Resultado**: ✓ Puntuación: 98/100, Pruebas funcionales: PASS

#### Fase 9: Documentación

```markdown
# Despliegue v2.1.0 - EXITOSO
Fecha: 2026-01-07 11:30:00
Duración: 35 minutos
Estado: COMPLETADO SIN ERRORES
```

**Resultado Final**: ✓ DESPLIEGUE EXITOSO

## Manejo de Errores y Rollback

### Error en Fase 4: Migración BD Falla

**Síntoma**: Script SQL retorna error
**Acción**:

1. Detener ejecución de migraciones
2. Ejecutar scripts de rollback en orden inverso
3. Restaurar backup de BD
4. Verificar integridad de datos
**Rollback**: Restaurar desde backup_20260107_110316.zip

### Error en Fase 6: Validación < 90

**Síntoma**: Puntuación de validación insuficiente
**Acción**:

1. No iniciar aplicación
2. Restaurar ejecutable antiguo
3. Ejecutar rollback de migraciones BD
4. Restaurar desde backup
5. Iniciar aplicación antigua
**Rollback**: Completo (BD + Ejecutable)

### Error en Fase 8: Pruebas Funcionales Fallan

**Síntoma**: No se puede crear factura o generar QR
**Acción**:

1. Detener aplicación nueva
2. Restaurar ejecutable antiguo
3. Ejecutar rollback de migraciones BD
4. Restaurar desde backup
5. Iniciar aplicación antigua
6. Notificar a usuarios
**Rollback**: Completo con notificación

### Rollback Completo

**Procedimiento**:

1. Detener aplicación actual

```powershell
Stop-Process -Name "FACARAVF" -Force
```

1. Restaurar ejecutable antiguo

```powershell
Remove-Item "FACARAVF.exe"
Rename-Item "FACARAVF_old.exe" "FACARAVF.exe"
```

1. Restaurar BD desde backup

```powershell
Expand-Archive "backup_20260107_110316.zip" -DestinationPath "temp_restore/"
Copy-Item "temp_restore/DATOS/*.db" "DATOS/" -Force
```

1. Ejecutar scripts de rollback de migraciones

```sql
-- En orden inverso
DROP TABLE LogVerifactu;
ALTER TABLE Facturas DROP COLUMN HashActual;
```

1. Verificar integridad

```
@skill:domain/verifactu/validate-verifactu-implementation (basic)
```

1. Iniciar aplicación antigua

```powershell
Start-Process "FACARAVF.exe"
```

1. Verificar funcionamiento

```
# Crear factura de prueba
# Verificar que todo funciona
```

1. Documentar rollback

```markdown
# ROLLBACK EJECUTADO
Fecha: 2026-01-07 11:45:00
Razón: [Descripción del problema]
Estado: Sistema restaurado a versión anterior
```

## Puntos de Decisión

### Decisión 1: ¿Continuar con despliegue?

**Condición**: Pre-checks de Fase 1 pasados
**Si Verdadero**: Continuar a Fase 2
**Si Falso**: ABORTAR despliegue, notificar problemas

### Decisión 2: ¿Validación Pre-Inicio suficiente?

**Condición**: Puntuación >= 90
**Si Verdadero**: Continuar a Fase 7
**Si Falso**: Ejecutar ROLLBACK completo

### Decisión 3: ¿Validación Post-Inicio exitosa?

**Condición**: Puntuación >= 95 Y pruebas funcionales PASS
**Si Verdadero**: Continuar a Fase 9
**Si Falso**: Ejecutar ROLLBACK completo

## Optimizaciones

### Paralelización

- Backup de BD y copia de ejecutable pueden hacerse en paralelo
- Validaciones de esquema y cadena de hashes pueden paralelizarse

### Caching

- Cachear resultados de validación de scripts de migración
- Reutilizar checksums de archivos no modificados

### Reducción de Tokens

- En despliegues repetidos, usar plantillas pre-generadas
- Documentación incremental en lugar de completa

## Variantes del Workflow

### Variante 1: Despliegue en Desarrollo

**Cuándo usar**: Entorno de desarrollo
**Diferencias**:

- Puede omitir backup (skip_backup: true)
- Validación básica suficiente
- No requiere notificación a usuarios

### Variante 2: Hotfix Urgente

**Cuándo usar**: Corrección crítica en producción
**Diferencias**:

- Proceso acelerado (validaciones mínimas)
- Backup obligatorio pero sin compresión
- Documentación post-despliegue

## Métricas y KPIs

### Métricas de Proceso

- **Duración total**: 30-45 minutos
- **Downtime**: 5-10 minutos
- **Tokens consumidos**: ~2500

### Métricas de Calidad

- **Tasa de éxito**: >= 95%
- **Necesidad de rollback**: < 5%
- **Puntuación validación final**: >= 95

## Checklist de Ejecución

### Pre-ejecución

- [ ] Ejecutable compilado y probado en staging
- [ ] Scripts de migración validados
- [ ] Espacio en disco verificado (>2GB)
- [ ] Usuarios notificados del mantenimiento
- [ ] Backup storage disponible
- [ ] Certificados digitales vigentes

### Durante Ejecución

- [ ] Fase 1: Pre-checks PASS
- [ ] Fase 2: Backup creado y verificado
- [ ] Fase 3: Aplicación detenida
- [ ] Fase 4: Migraciones aplicadas sin errores
- [ ] Fase 5: Ejecutable desplegado
- [ ] Fase 6: Validación pre-inicio >= 90
- [ ] Fase 7: Aplicación iniciada
- [ ] Fase 8: Validación post-inicio >= 95

### Post-ejecución

- [ ] Reporte de despliegue generado
- [ ] Changelog actualizado
- [ ] Usuarios notificados de disponibilidad
- [ ] Logs archivados
- [ ] Monitoreo activo por 24 horas
- [ ] Backup antiguo eliminado (después de 7 días)

## Dependencias

### Skills Requeridas

- `@skill:domain/database/backup-database`
- `@skill:domain/database/create-database-migration`
- `@skill:domain/verifactu/validate-verifactu-implementation`
- `@skill:core/documentation/generate-changelog`

### Herramientas Externas

- **PowerShell** (5.1+): Para automatización
- **Database Desktop** (Delphi): Para ejecutar scripts SQL
- **7-Zip** o similar: Para compresión de backups

### Permisos Requeridos

- Acceso administrativo al servidor/estación
- Permisos de escritura en carpeta de aplicación
- Permisos de escritura en carpeta de BD
- Permisos para detener/iniciar procesos

## Notas Importantes

- **SIEMPRE** hacer backup antes de cualquier despliegue en producción
- **NUNCA** saltar validaciones en producción
- Probar el workflow completo en staging primero
- Mantener ejecutable antiguo por al menos 7 días
- Documentar cualquier desviación del workflow estándar
- En caso de duda, ejecutar ROLLBACK

## Referencias

- [Workflow de Backup](./backup-workflow.md)
- [Guía de Rollback](../docs/rollback-guide.md)
- [Checklist de Despliegue](../docs/deployment-checklist.md)

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial del workflow
- 9 fases definidas con puntos de control
- Procedimiento de rollback completo
- Validaciones Verifactu integradas

---

**Última ejecución exitosa**: Pendiente  
**Próxima revisión**: 2026-04-07  
**Estado**: stable  
**Mantenedor**: Sistema de Skills
