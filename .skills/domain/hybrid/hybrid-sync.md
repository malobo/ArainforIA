---
id: skill-domain-hybrid-sync
name: Desarrollo Híbrido (Desktop + Nube)
version: 1.0.0
category: domain/hybrid
priority: medium
last_updated: 2026-01-07
triggers:
  - "sincronizar nube"
  - "desarrollo hibrido"
  - "api rest delphi"
  - "subir datos web"
  - "bajar pedidos"
  - "php api"
---

# Desarrollo Híbrido: Escritorio + Nube

<context>
Estrategia para integrar aplicaciones de escritorio ARAINFORIA (Delphi) con servicios en la nube (PHP/MySQL) para sincronización bidireccional de negocio.
Basado en `CLOUD_INTEGRATION.md`.
Stack: Delphi (`System.Net.HttpClient` o `mORMot`) <-> PHP (REST/JSON) <-> MySQL.
</context>

<instruction>
1. **Arquitectura**:
   - Delphi actúa como cliente maestro que sincroniza el estado local con la nube.
   - Nube actúa como concentrador de pedidos/partes móviles.
   - Usar JWT para autenticación.

1. **Sincronización de Entidades**:
   - Soportar CRUD completo para: Clientes, Artículos, Pedidos, Partes de Trabajo.
   - Usar JSON para el intercambio de datos.
   - Implementar control de versiones o timestamps (`updated_at`) para detectar cambios.

2. **Tecnología**:
   - Lado Delphi: Usar `System.Net.HttpClient` nativo para llamadas REST simples o `mORMot` para altas prestaciones.
   - Lado PHP: API REST estructurada (MVC, Controllers por entidad).

3. **Seguridad**:
   - Validar siempre los datos entrantes en el servidor PHP.
   - Usar HTTPS obligatorio en producción.
</instruction>

<examples>
User: "¿Cómo subo los artículos a la web?"
Assistant: "Se debe implementar un servicio en Delphi que serialice el DataSet de artículos a JSON y lo envíe vía POST al endpoint `/api/articulos`. Se recomienda hacerlo en lotes (batch)."

User: "¿Qué endpoint recibe los pedidos?"
Assistant: "En la arquitectura híbrida, Delphi hace polling (GET) a `/api/pedidos/pendientes` para descargar los nuevos pedidos creados en la web/móvil."
</examples>

<resources>
- [`CLOUD_INTEGRATION.md`](c:/Arainfor/CLOUD_INTEGRATION.md) (Documento Maestro)
- [`mORMot Framework`](core/mormot.md) (Útil para JSON/REST rápidos)
</resources>
