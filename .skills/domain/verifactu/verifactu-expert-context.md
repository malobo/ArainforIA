---
name: verifactu-expert-context
version: 1.0.0
category: domain/verifactu
complexity: 8
tokens_estimate: 1500-2500
type: context
activation: auto
tags: [verifactu, aeat, facturacion, legal, compliance, hash, qr]
requires: []
dependencies: []
---

# ⚖️ Contexto Experto en Verifactu (RD 1007/2023)

## Descripción

Skill de contexto que activa el conocimiento experto sobre la normativa española de sistemas informáticos de facturación (Verifactu/SIF). Proporciona detalles técnicos precisos sobre la estructura de datos, algoritmos de huella y requisitos de seguridad.

## Objetivo

Asegurar que cualquier código generado relacionado con facturación cumpla estrictamente con los requisitos técnicos y legales de Verifactu, evitando errores normativos graves.

## Uso

Se activa automáticamente al trabajar con unidades relacionadas con facturación (`uVerifactu`, `Facturas`, `Previsualiza`) o al mencionar términos clave como "huella", "QR", "AEAT", "alta", "anulación".

---

## 📚 BASE DE CONOCIMIENTO NORMATIVO

### Principios Fundamentales

1. **Integridad**: Los registros una vez generados no pueden ser alterados sin dejar rastro.
2. **Conservación**: Los datos deben conservarse legibles y accesibles.
3. **Accesibilidad**: La AEAT debe poder acceder a los registros.
4. **Legibilidad**: Formatos estándar.
5. **Trazabilidad**: Encadenamiento de registros.
6. **Inalterabilidad**: Garantizada por el hash encadenado.

### Estructura del Registro de Facturación

Todo registro de facturación (Alta o Anulación) debe contener XML con:

1. **Cabecera**:
   - ID Emisor (NIF, Nombre)
   - ID SIF (Nombre sistema, Versión, ID Developer)
   - Tipo de Registro (Alta o Anulación)

2. **Datos de Factura** (Solo Alta):
   - Número y Serie
   - Fecha de Expedición
   - Tipo de Factura (F1: Completa, F2: Simplificada, etc.)
   - Cuotas e Importes desglosados
   - Regímenes especiales (si aplican)

3. **Huella (Hash)**:
   - Hash del registro actual
   - Hash del registro anterior (Encadenamiento)

4. **Firma Digital**:
   - Requerida si NO es sistema VERI*FACTU (envío voluntario).
   - Opcional si es sistema VERI*FACTU (envío inmediato).

---

## 🔐 Algoritmos Críticos

### 1. Cálculo de la Huella (Hash)

El Hash SHA-256 se calcula sobre una cadena concatenada específica. **El orden es CRÍTICO**.

**Formato de la cadena (Registro de Alta):**

```text
IDEmisor + NumSerieFactura + FechaExpedicion + TipoFactura + CuotaTotal + ImporteTotal + HuellaAnterior + FechaHoraGeneracion
```

**Formato de campos:**

- **IDEmisor**: NIF sin espacios ni guiones.
- **NumSerieFactura**: Tal cual aparece en la factura.
- **FechaExpedicion**: `dd-mm-yyyy`
- **TipoFactura**: `F1`, `F2`, `R1`, `R2`, etc.
- **CuotaTotal**: String con 2 decimales (`0.00`), separador decimal punto. Si no hay cuota, `0.00`.
- **ImporteTotal**: String con 2 decimales (`0.00`), separador decimal punto.
- **HuellaAnterior**: Hash SHA-256 del registro anterior (64 caracteres hex). Si es el primero, string vacío (o 64 ceros según implementación específica, revisar XSD). *Nota: Generalmente es vacío para el primero.*
- **FechaHoraGeneracion**: ISO 8601 (`yyyy-mm-ddThh:mm:ss`)

**Ejemplo Delphi:**

```pascal
function CalcularHuellaAlta(const Datos: TDatosHuella): string;
var
  Cadena: string;
begin
  // Nota: Asegurar cultura invariante para decimales (punto)
  Cadena := 
    Datos.IDEmisor +
    Datos.NumSerie +
    FormatDateTime('dd-mm-yyyy', Datos.FechaExp) +
    Datos.TipoFactura +
    FormatFloat('0.00', Datos.CuotaTotal) + // Usar punto decimal
    FormatFloat('0.00', Datos.ImporteTotal) + // Usar punto decimal
    Datos.HuellaAnterior +
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Datos.FechaHoraGen);
    
  Result := THashSHA2.GetHashString(Cadena, THashSHA2.TSHA2Version.SHA256);
end;
```

### 2. Generación del Código QR

El QR debe contener una URL con parámetros específicos.

**URL Base (Producción - Ejemplo):**
`https://www1.agenciatributaria.gob.es/wlpl/TIKE-CONT/ValidarQR?`

**Parámetros:**

- `nif`: NIF del emisor
- `num`: Número serie y número factura
- `fecha`: Fecha expedición (`dd-mm-yyyy`)
- `importe`: Importe total (`0.00`)
- `huella`: Parte de la huella (primeros caracteres, ver especificación técnica, usualmente 64 chars completa o truncada)

**Texto para el QR:**
El contenido del QR es la URL completa concatenada con los parámetros.

**Ejemplo Delphi (usando DelphiZXIngQRCode o similar):**

```pascal
function GenerarTextoQR(const Datos: TDatosQR): string;
begin
  Result := Format('https://.../ValidarQR?nif=%s&num=%s&fecha=%s&importe=%s',
    [Datos.NIF, Datos.NumFac, Datos.Fecha, Datos.Importe]);
end;
```

---

## 📋 Tipos de Factura (Claves)

| Clave | Descripción |
| :---: | :---------- |
| **F1** | Factura Completa |
| **F2** | Factura Simplificada (Tickets) |
| **F3** | Factura sustitutiva de simplificada |
| **R1** | Rectificativa: Error fundado en derecho |
| **R2** | Rectificativa: Artículo 80 LIVA (Concurso acreedores, impago) |
| **R3** | Rectificativa: Resto |
| **R4** | Rectificativa: Resto (Simplificada) |
| **R5** | Rectificativa: Factura simplificada (Art 80 LIVA) |

---

## 🛠️ Estructura de Tablas Recomendada (Paradox/SQL)

Para cumplir con Verifactu, la tabla de facturas necesita campos adicionales críticos.

| Campo | Tipo | Notas |
| :---: | :---: | :--- |
| `Veri_Huella` | String(64) | SHA-256 del registro actual |
| `Veri_HuellaAnt` | String(64) | SHA-256 del registro anterior |
| `Veri_FechaGen` | DateTime | Fecha/Hora exacta generación registro |
| `Veri_Estado` | String(10) | 'PENDIENTE', 'ENVIADO', 'ANULADO' |
| `Veri_CSV` | String(50) | Código Seguro Verificación (si aplica) |
| `Veri_Tipo` | String(2) | F1, F2, R1... |

---

## ⚠️ Errores Comunes a Evitar

1. **Recalcular Hash**: NUNCA recalcular el hash de una factura ya generada. Rompe la cadena.
2. **Modificar Facturas**: Las facturas enviadas/generadas son INMUTABLES. Para corregir, se debe hacer una Rectificativa o una Anulación.
3. **Hora UTC vs Local**: Asegurar consistencia en la zona horaria para `FechaHoraGeneracion`.
4. **Formato Numérico**: El separador decimal en la cadena de hash debe ser SIEMPRE punto (.), independientemente de la configuración regional del PC.
5. **Codificación**: Todo texto debe estar en UTF-8.

## 🔍 Referencias Técnicas

- **Esquema XSD**: `SuministroFacturacion.xsd` (AEAT)
- **Firma**: XAdES-BES (si aplica)
- **Encoding**: UTF-8

## Historial de Cambios

| Versión | Fecha | Cambios |
| ------- | ----- | ------- |
| 1.0.0 | 2026-01-07 | Creación inicial del contexto |
