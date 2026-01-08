---
name: generate-verifactu-xml
version: 1.0.0
category: domain/verifactu
tags: [verifactu, xml, factura, aeat, firma]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 7
estimated_tokens: 600-800
triggers:
  - "xml verifactu"
  - "generar factura xml"
  - "estructura xml aeat"
  - "enviar factura"
---

# Generar XML Verifactu

## Descripción

Genera XML conforme a la especificación Verifactu para envío de facturas a la AEAT.

## AI Context

> **SYSTEM_INSTRUCTION**: Generate Verifactu-compliant XML for invoices following AEAT specifications.
> **OUTPUT_FORMAT**: Complete XML with comments + Delphi code to generate it.
> **TOKEN_STRATEGY**: Focus on required fields, provide optional fields as reference.

## Estructura XML Verifactu

```xml
<?xml version="1.0" encoding="UTF-8"?>
<RegistroFactura xmlns="https://www2.agenciatributaria.gob.es/static_files/common/internet/dep/aplicaciones/es/aeat/ssii/verifactu/1_0/Verifactu.xsd">
  <Cabecera>
    <ObligadoEmision>
      <NIF>[NIF_EMISOR]</NIF>
      <NombreRazon>[RAZON_SOCIAL]</NombreRazon>
    </ObligadoEmision>
  </Cabecera>
  <RegistroAlta>
    <IDFactura>
      <NumSerieFacturaEmisor>[SERIE]-[NUMERO]</NumSerieFacturaEmisor>
      <FechaExpedicionFacturaEmisor>[YYYY-MM-DD]</FechaExpedicionFacturaEmisor>
    </IDFactura>
    <NombreRazonEmisor>[NOMBRE_EMISOR]</NombreRazonEmisor>
    <TipoFactura>F1</TipoFactura>
    <DescripcionOperacion>[DESCRIPCION]</DescripcionOperacion>
    <Destinatarios>
      <IDDestinatario>
        <NIF>[NIF_CLIENTE]</NIF>
        <NombreRazon>[NOMBRE_CLIENTE]</NombreRazon>
      </IDDestinatario>
    </Destinatarios>
    <Desglose>
      <DetalleDesglose>
        <BaseImponibleACoste>[BASE]</BaseImponibleACoste>
        <TipoImpositivo>[21.00]</TipoImpositivo>
        <CuotaRepercutida>[CUOTA]</CuotaRepercutida>
      </DetalleDesglose>
    </Desglose>
    <CuotaTotal>[TOTAL_CUOTA]</CuotaTotal>
    <ImporteTotal>[TOTAL]</ImporteTotal>
    <Huella>
      <Encadenamiento>
        <RegistroAnterior>
          <Huella>[HASH_ANTERIOR]</Huella>
        </RegistroAnterior>
      </Encadenamiento>
      <Software>
        <NombreRazon>[NOMBRE_SOFTWARE]</NombreRazon>
        <NIF>[NIF_DESARROLLADOR]</NIF>
        <IdSistemaInformatico>[ID_SISTEMA]</IdSistemaInformatico>
        <Version>[VERSION]</Version>
      </Software>
      <Hash>[HASH_SHA256]</Hash>
    </Huella>
  </RegistroAlta>
</RegistroFactura>
```

## Tipos de Factura

| Código | Descripción |
| ------ | ----------- |
| F1 | Factura normal |
| F2 | Factura simplificada (ticket) |
| R1 | Factura rectificativa (error fundado en derecho) |
| R2 | Factura rectificativa (artículo 80.3) |
| R3 | Factura rectificativa (artículo 80.4) |
| R4 | Factura rectificativa (resto) |
| R5 | Factura rectificativa simplificada |

## Código Delphi: Generación XML

```pascal
uses
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  SysUtils;

function GenerarXMLVerifactu(const Factura: TFacturaVerifactu): RawUtf8;
var
  XML: TTextWriter;
  Temp: TTextWriterStackBuffer;
begin
  XML := TTextWriter.CreateOwnedStream(Temp);
  try
    XML.AddString('<?xml version="1.0" encoding="UTF-8"?>');
    XML.AddCR;
    XML.AddString('<RegistroFactura xmlns="https://www2.agenciatributaria.gob.es/...');
    
    // Cabecera
    XML.AddString('<Cabecera>');
    XML.AddString('<ObligadoEmision>');
    XML.AddString('<NIF>'); XML.AddString(Factura.NifEmisor); XML.AddString('</NIF>');
    XML.AddString('<NombreRazon>'); XML.AddXmlEscape(Factura.NombreEmisor); XML.AddString('</NombreRazon>');
    XML.AddString('</ObligadoEmision>');
    XML.AddString('</Cabecera>');
    
    // RegistroAlta
    XML.AddString('<RegistroAlta>');
    XML.AddString('<IDFactura>');
    XML.AddString('<NumSerieFacturaEmisor>');
    XML.AddString(Factura.Serie + '-' + Factura.Numero);
    XML.AddString('</NumSerieFacturaEmisor>');
    // ... resto de campos
    XML.AddString('</RegistroAlta>');
    
    XML.AddString('</RegistroFactura>');
    
    Result := XML.Text;
  finally
    XML.Free;
  end;
end;
```

## Cálculo de Hash (Huella)

```pascal
uses
  mormot.crypt.core;

function CalcularHuellaVerifactu(const DatosFactura: RawUtf8; 
  const HashAnterior: RawUtf8): RawUtf8;
var
  CadenaHash: RawUtf8;
begin
  // Concatenar: NIF + NumFactura + Fecha + Cuota + Total + HashAnterior
  CadenaHash := DatosFactura + HashAnterior;
  Result := Sha256(CadenaHash);
end;
```

## Validación Pre-Envío

```pascal
function ValidarFacturaVerifactu(const F: TFacturaVerifactu): TStringList;
begin
  Result := TStringList.Create;
  
  if Length(F.NifEmisor) <> 9 then
    Result.Add('NIF emisor inválido');
  if F.Total <= 0 then
    Result.Add('Total debe ser mayor que 0');
  if F.FechaExpedicion > Date then
    Result.Add('Fecha no puede ser futura');
  if F.HashAnterior = '' then
    Result.Add('Falta hash de encadenamiento');
end;
```

## Referencias

- [Especificación Verifactu AEAT](https://www.agenciatributaria.es/)
- [Skill: core/verifactu.md](../../../core/verifactu.md)

---

**Estado**: stable  
**Última revisión**: 2026-01-08
