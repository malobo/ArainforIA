---
id: skill-core-mormot
name: mORMot 2 Framework
version: 2.1
category: core
priority: high
last_updated: 2026-01-06
triggers:
  - "mormot"
  - "firmar factura"
  - "verifactu signature"
  - "certificado digital"
  - "pkcs11"
  - "json rapido"
  - "generar pdf"
  - "log error"
  - "trazabilidad"
  - "plantilla email"
---

# mORMot 2 Framework

<context>
Biblioteca "Navaja Suiza" ubicada en `C:\Arainfor\Comps\mormot2`.
Pieza fundamental para **VERIFACTU** (Criptografía) y estandarización técnica de **ARAFAC/ERPW**.
Cubre: Criptografía, Logging, PDF, JSON y Templating.
</context>

<instruction>
1. **Firma Digital & Criptografía (Verifactu)**:
   - Usa `mormot.crypt.pkcs11` para tokens físicos y `mormot.crypt.x509` para certificados soft.
   - **Hashing**: `mormot.crypt.core` (SHA-256) es mandatorio para la Huella de Factura.

1. **Observabilidad (Logging)**:
   - Implementa `mormot.core.log` en todos los proyectos críticos.
   - Usa `TSynLog.Enter` al inicio de métodos complejos para trazabilidad automática y stack trace en excepciones.

2. **Generación de Documentos (PDF/A)**:
   - Usa `mormot.ui.pdf` para generar facturas PDF. Configúralo para **PDF/A-1** (Larga duración) como exige la normativa digital.
   - Es nativo (sin DLLs) y mucho más rápido que report managers visuales.

3. **Templating & Comms**:
   - Usa `mormot.core.mustache` para separar lógica Delphi de presentación (Emails, HTMLs).
   - Genera el contenido dinámico inyectando un JSON/Object al renderizador Mustache.

4. **Cliente HTTP & REST (Sincronización Híbrida)**:
   - ⚡ **IMPORTANTE**: Para conectar con la Nube (PHP), prefiere `mormot.net.client` o `mormot.rest.client` sobre los componentes nativos de Delphi. Son thread-safe, más rápidos y manejan mejor los timeouts/errores.
   - Soporta HTTPS nativo y validación de certificados.

5. **JSON & Core**:
   - Para serialización masiva, `mormot.core.json` sustituye a `System.JSON`. Es 10x-100x más rápido.
   - Usa `TDocVariant` para manipular JSON dinámicos sin crear clases.
</instruction>

<examples>
User: "¿Cómo calculo el Hash de la factura?"
Assistant:
```delphi
uses mormot.crypt.core, mormot.core.base;
function CalcularHash(const Datos: RawByteString): RawByteString;
begin
  Result := Algo(HashDescSHA256).Hash(Datos);
end;
```

User: "¿Cómo genero un PDF simple?"
Assistant:

```delphi
uses mormot.ui.pdf;
var Pdf: TPdfDocument;
begin
  Pdf := TPdfDocument.Create;
  try
    Pdf.AddPage;
    Pdf.Canvas.TextOut(10, 10, 'Factura Verifactu');
    Pdf.SaveToFile('Factura.pdf');
  finally
    Pdf.Free;
  end;
end;
```

User: "¿Cómo logueo un error?"
Assistant:

```delphi
uses mormot.core.log;
begin
  TSynLog.Add.LogError('Error crítico en base de datos: %', [ErrorMsg]);
end;
```

User: "¿Cómo envío un JSON a la API PHP?"
Assistant:

```delphi
uses mormot.net.client, mormot.core.json;
var
  Http: TWinHttp;
  Res: TWinHttpResult;
begin
  Http := TWinHttp.Create;
  try
    Res := Http.Post('https://mi-api.com/pedidos', '{"id": 1, "cliente": "Juan"}', 'application/json');
    if Res.Status = 200 then
      ShowMessage('Enviado OK: ' + Res.Body);
  finally
    Http.Free;
  end;
end;
```

</examples>

<resources>
- `C:\Arainfor\Comps\mormot2\src\crypt` (Cripto)
- `C:\Arainfor\Comps\mormot2\src\core` (Log, JSON, Mustache)
- `C:\Arainfor\Comps\mormot2\src\ui` (PDF)
- [Documentación Oficial](https://synopse.info/files/doc/mORMot2.html)
</resources>
