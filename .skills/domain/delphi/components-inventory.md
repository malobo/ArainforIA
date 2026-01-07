---
id: skill-domain-components
name: Inventario de Componentes (Third-Party)
version: 1.0.0
category: domain/delphi
priority: medium
last_updated: 2026-01-07
triggers:
  - "que componentes tengo"
  - "usar grid"
  - "tms"
  - "imprimir"
  - "codigo barras"
  - "acceso datos"
  - "biometria"
---

# Inventario de Componentes (`C:\Arainfor\Comps`)

<context>
Catálogo de bibliotecas de terceros disponibles en el entorno de desarrollo.
Antes de sugerir una librería externa, verifica si ya existe una solución aquí.
</context>

<instruction>
1. **Core & Data**:
   - `mORMot2` (Recomendado): JSON, REST, Criptografía, PDF/A.
   - `ZeosDBO` (v8.0): Acceso a datos (alternativa a FireDAC).
   - `Tdbf` / `VkDnf`: Soporte Legacy para DBF.
   - `RxLib`: Componentes VCL clásicos y utilidades.

1. **UI & Reporting**:
   - `TMS`: Suite completa de controles UI modernos (Grids avanzados, Planners, etc.).
   - `GmPrintSuite` (`GmGridPrint`): Motor de impresión ligero usado en Aracostes/ARAFAC.

2. **Hardware & Periféricos**:
   - `FBarcode`: Generación de códigos de barras 1D.
   - `QRCode`: Generación de códigos QR (Vital para Verifactu).
   - `DelphiTwain` / `MHTwain`: Escaneo de documentos.
   - `ZKSDK`: Lectores biométricos (ZKSoftware).

3. **Utilidades**:
   - `RegExpr`: Motor de Expresiones Regulares.
   - `Parser`: Evaluador de expresiones.
</instruction>

<examples>
User: "¿Cómo muestro una tabla bonita?"
Assistant: "Tienes la suite **TMS** disponible. Usa un `TAdvStringGrid` o `TAdvDBGrid` en lugar del grid estándar."

User: "¿Cómo imprimo el QR de Verifactu?"
Assistant: "Utiliza la librería **QRCode** (`comps\qrcode`) para generar la imagen y **GmPrintSuite** para plasmarla en el informe."
</examples>
