---
name: validate-verifactu-implementation
version: 1.0.0
category: domain/verifactu
tags: [verifactu, validation, compliance]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 7
estimated_tokens: 1000-1500
---

# Validar Implementación Verifactu

## Descripción

Valida que la implementación de Verifactu cumple con todos los requisitos del Real Decreto 1007/2023.

## Objetivo

Verificar de forma exhaustiva que el sistema de facturación cumple con todos los requisitos técnicos y legales de Verifactu, identificando gaps y no conformidades.

## Inputs

- **project_path** (string): Ruta al proyecto Delphi
- **validation_level** (string, opcional): [basic|standard|exhaustive] (default: standard)
- **generate_report** (boolean, opcional): Generar reporte PDF (default: true)

## Outputs

- **validation_report** (markdown): Reporte detallado de validación
- **compliance_score** (number): Puntuación de cumplimiento (0-100)
- **action_items** (list): Lista de acciones correctivas necesarias
- **certificate** (pdf, opcional): Certificado de cumplimiento si score >= 95

## Precondiciones

- Proyecto Delphi compilable
- Base de datos accesible
- Archivos de configuración Verifactu presentes

## Postcondiciones

- Reporte de validación generado
- Gaps identificados y documentados
- Plan de acción para correcciones si es necesario

## Procedimiento

### Paso 1: Validación de Estructura de Datos

Verificar que las tablas de base de datos contienen todos los campos requeridos:

**Campos Obligatorios en Tabla de Facturas**:

- IDFactura (único)
- NumeroFactura
- FechaExpedicion
- HoraExpedicion
- TipoFactura
- ImporteTotal
- BaseImponible
- CuotaIVA
- HashAnterior
- HashActual
- FirmaDigital
- QRCode
- EstadoVerifactu

**Validación**: Ejecutar query para verificar existencia de campos

### Paso 2: Validación de Encadenamiento

Verificar la integridad del encadenamiento de hashes:

```pascal
// Pseudocódigo de validación
for each factura in orden_cronologico do
begin
  hash_calculado := CalcularHash(factura);
  if hash_calculado <> factura.HashActual then
    ReportarError('Hash no coincide en factura ' + factura.NumeroFactura);
  
  if factura_anterior exists then
    if factura.HashAnterior <> factura_anterior.HashActual then
      ReportarError('Cadena rota entre ' + factura_anterior.NumeroFactura + ' y ' + factura.NumeroFactura);
end;
```

**Validación**: Cadena completa sin roturas

### Paso 3: Validación de Algoritmo de Hash

Verificar que se usa SHA-256 correctamente:

- Comprobar uso de `System.Hash.THashSHA2.GetHashString`
- Validar formato de salida (64 caracteres hexadecimales)
- Verificar que se incluyen todos los campos requeridos en el cálculo

**Validación**: Hash generado cumple especificación

### Paso 4: Validación de Código QR

Verificar generación correcta del código QR:

**Contenido Requerido**:

```
https://proveedor.com/verifactu?
  nif=[NIF_EMISOR]&
  num=[NUMERO_FACTURA]&
  fecha=[FECHA_EXPEDICION]&
  importe=[IMPORTE_TOTAL]&
  hash=[HASH_FACTURA]
```

**Validación**:

- QR generado es legible
- Contiene todos los campos obligatorios
- URL es válida y accesible

### Paso 5: Validación de Firma Digital

Verificar implementación de firma electrónica:

- Certificado digital válido y no expirado
- Algoritmo de firma conforme (RSA-SHA256 o superior)
- Timestamp incluido en la firma
- Cadena de certificación verificable

**Validación**: Firma verificable con certificado raíz

### Paso 6: Validación de Formato de Impresión

Verificar que las facturas impresas incluyen:

- [ ] Código QR visible y legible
- [ ] Hash de la factura
- [ ] Número de factura completo
- [ ] Fecha y hora de expedición
- [ ] Leyenda "Factura Verificable - Verifactu"
- [ ] Datos del emisor (NIF, razón social)

**Validación**: Factura impresa cumple requisitos visuales

### Paso 7: Validación de Generación XML

Verificar que el XML generado cumple el esquema XSD de AEAT:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<RegistroFacturacion xmlns="https://www2.agenciatributaria.gob.es/static_files/common/internet/dep/aplicaciones/es/aeat/tike/cont/ws/SuministroInformacion.xsd">
  <Cabecera>
    <IDVersion>1.0</IDVersion>
    <Titular>
      <NIF>[NIF]</NIF>
      <NombreRazon>[NOMBRE]</NombreRazon>
    </Titular>
  </Cabecera>
  <RegistroFactura>
    <!-- Campos de la factura -->
  </RegistroFactura>
</RegistroFacturacion>
```

**Validación**: XML válido contra XSD oficial

### Paso 8: Validación de Log de Eventos

Verificar que se registran todos los eventos críticos:

**Eventos Obligatorios**:

- Creación de factura
- Modificación de factura
- Anulación de factura
- Generación de factura rectificativa
- Envío a AEAT
- Errores de sistema

**Validación**: Log completo y auditable

### Paso 9: Validación de Seguridad

Verificar medidas de seguridad implementadas:

- [ ] Datos de facturación en tablas inmutables o con triggers de auditoría
- [ ] Backup automático de registros Verifactu
- [ ] Control de acceso a funciones críticas
- [ ] Encriptación de datos sensibles
- [ ] Protección contra manipulación de fechas del sistema

**Validación**: Controles de seguridad activos

### Paso 10: Generación de Reporte Final

Compilar todos los resultados en un reporte estructurado:

```markdown
# Reporte de Validación Verifactu

## Resumen Ejecutivo
- **Puntuación Global**: XX/100
- **Estado**: [CONFORME | NO CONFORME | CONFORME CON OBSERVACIONES]
- **Fecha de Validación**: YYYY-MM-DD

## Resultados por Área

### 1. Estructura de Datos: ✓ PASS
[Detalles]

### 2. Encadenamiento: ✗ FAIL
[Detalles del problema]

### 3. Algoritmo de Hash: ✓ PASS
[Detalles]

[... más áreas ...]

## Gaps Identificados
1. [Gap crítico 1]
2. [Gap importante 2]
3. [Gap menor 3]

## Plan de Acción
1. [Acción correctiva 1] - Prioridad: ALTA
2. [Acción correctiva 2] - Prioridad: MEDIA
3. [Acción correctiva 3] - Prioridad: BAJA

## Próximos Pasos
[Recomendaciones]
```

**Validación**: Reporte completo y accionable

## Ejemplos de Uso

### Ejemplo 1: Validación Estándar

**Contexto**: Verificación periódica de cumplimiento

**Input**:

```
project_path: "D:/ARAINFORIA/FACARAVF"
validation_level: "standard"
generate_report: true
```

**Output Esperado**:

```markdown
# Reporte de Validación Verifactu - FACARAVF

## Puntuación: 92/100 - CONFORME CON OBSERVACIONES

### Áreas Validadas
✓ Estructura de Datos (100%)
✓ Encadenamiento (100%)
✓ Algoritmo Hash (100%)
✓ Código QR (95%)
⚠ Firma Digital (80%) - Certificado próximo a expirar
✓ Formato Impresión (100%)
✓ Generación XML (90%)
✓ Log de Eventos (85%)
✓ Seguridad (90%)

### Observaciones
1. Certificado digital expira en 45 días - Renovar
2. Log de eventos no registra todos los intentos fallidos
3. XML generado válido pero podría optimizarse

### Recomendaciones
1. [ALTA] Renovar certificado digital
2. [MEDIA] Mejorar logging de eventos
3. [BAJA] Optimizar generación XML
```

### Ejemplo 2: Validación Exhaustiva Pre-Producción

**Contexto**: Antes de desplegar a producción

**Input**:

```
project_path: "D:/ARAINFORIA/FACARAVF"
validation_level: "exhaustive"
generate_report: true
```

**Output Esperado**:

```markdown
# Reporte Exhaustivo de Validación Verifactu

## Puntuación: 98/100 - CONFORME

[Reporte detallado con todas las pruebas]

## Certificado de Cumplimiento
✓ El sistema cumple con todos los requisitos obligatorios
✓ Apto para producción
✓ Próxima revisión: 2026-04-07
```

## Manejo de Errores

### Error 1: Base de datos no accesible

**Síntoma**: No se puede conectar a la base de datos
**Causa**: Conexión incorrecta o BD no disponible
**Solución**: Verificar cadena de conexión y disponibilidad de BD

### Error 2: Tablas incompletas

**Síntoma**: Faltan campos obligatorios en tablas
**Causa**: Migración de BD incompleta
**Solución**: Ejecutar scripts de actualización de esquema

### Error 3: Cadena de hashes rota

**Síntoma**: Hashes no coinciden entre facturas consecutivas
**Causa**: Manipulación de datos o error en cálculo
**Solución**: Identificar punto de ruptura y regenerar cadena desde ahí

## Optimizaciones

### Optimización de Tokens

- Validar solo facturas del último mes en modo "basic"
- Usar sampling estadístico para validación de cadena en BDs grandes
- Cachear resultados de validación de esquema

### Optimización de Rendimiento

- Paralelizar validaciones independientes
- Usar índices en consultas de validación
- Limitar profundidad de validación de cadena según nivel

## Dependencias

### Skills Requeridas

- `@skill:domain/database/validate-schema` - Validación de esquema BD
- `@skill:core/analysis/xml-validator` - Validación de XML contra XSD

### Herramientas Externas

- **XMLSpy** o similar (opcional): Para validación exhaustiva de XML
- **QR Code Reader**: Para verificar legibilidad de QR

## Métricas de Éxito

- [ ] Puntuación >= 95 para certificación
- [ ] Cero gaps críticos identificados
- [ ] Cadena de hashes 100% íntegra
- [ ] XML válido contra XSD oficial
- [ ] QR codes 100% legibles

## Notas

- La validación no modifica datos, solo lee
- Requiere acceso de solo lectura a la BD
- El certificado de cumplimiento es informativo, no legal
- Validación exhaustiva puede tomar varios minutos en BDs grandes

## Referencias

- [Real Decreto 1007/2023](https://www.boe.es/eli/es/rd/2023/12/05/1007)
- [Especificación Técnica Verifactu](https://www.agenciatributaria.es/AEAT.internet/Inicio/_Segmentos_/Empresas_y_profesionales/Obligaciones_fiscales/Verifactu/Verifactu.shtml)
- [Esquema XSD AEAT](https://www2.agenciatributaria.gob.es/static_files/common/internet/dep/aplicaciones/es/aeat/tike/cont/ws/SuministroInformacion.xsd)

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial de la skill
- Validación de 9 áreas críticas
- Sistema de puntuación implementado
- Generación de certificado de cumplimiento

---

**Última revisión**: 2026-01-07  
**Próxima revisión**: 2026-04-07  
**Estado**: stable
