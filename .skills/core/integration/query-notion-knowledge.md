---
name: query-notion-knowledge
version: 1.0.0
category: core/integration
tags: [notion, search, knowledge, rag, documentation]
author: Sistema
created: 2026-01-07
updated: 2026-01-07
complexity: 2
estimated_tokens: 300-500
type: query
---

# Consultar Base de Conocimiento en Notion

## Descripción

Busca información en tu base de conocimiento de Notion antes de recurrir
a internet, aprovechando la documentación y soluciones previamente guardadas.

## Invocación

```
@skill:core/integration/query-notion-knowledge
consulta: "cómo implementar hash SHA-256 en Delphi"
contexto: "verifactu"
limite: 5
```

---

## Inputs

| Nombre | Tipo | Requerido | Descripción |
|--------|------|-----------|-------------|
| `consulta` | string | ✅ | Pregunta o términos de búsqueda |
| `contexto` | string | Opcional | Filtrar por proyecto/área |
| `limite` | number | Opcional | Máximo de resultados (default: 5) |
| `tipo` | string | Opcional | page, database, all |
| `incluir_contenido` | boolean | Opcional | Traer contenido completo |

## Outputs

| Campo | Tipo | Descripción |
|-------|------|-------------|
| `encontrado` | boolean | Si hay resultados |
| `resultados` | array | Lista de páginas relevantes |
| `mejor_match` | object | Resultado más relevante |
| `sugerencia` | string | Qué hacer si no hay resultados |

---

## Procedimiento

### Paso 1: Buscar en Notion

```
1. Ejecutar búsqueda con términos de la consulta
2. Filtrar por contexto si se especifica
3. Ordenar por relevancia y fecha
```

### Paso 2: Procesar Resultados

```
1. Para cada resultado:
   - Extraer título y URL
   - Obtener extracto relevante
   - Calcular puntuación de relevancia
2. Limitar a número especificado
```

### Paso 3: Presentar

```
1. Mostrar resultados ordenados
2. Si incluir_contenido=true, expandir mejor match
3. Si no hay resultados, sugerir alternativas
```

---

## Ejemplo de Uso

**Consulta**: "cómo validar NIF en Delphi"

**Resultado**:

```
✅ Encontrado en tu base de conocimiento:

1. 📄 Validación de NIF/CIF (FACARAVF)
   URL: notion.so/...
   Extracto: "La función ValidarNIF utiliza el algoritmo 
   estándar con la cadena de letras TRWAGMYFPDXBNJZSQVHLCKE..."
   Relevancia: ⭐⭐⭐⭐⭐

2. 📄 uVerifactu - Documentación
   URL: notion.so/...
   Extracto: "Incluye validación de NIF del emisor..."
   Relevancia: ⭐⭐⭐⭐

3. 📄 Implementación Verifactu
   URL: notion.so/...
   Relevancia: ⭐⭐⭐

💡 El primer resultado parece responder tu pregunta.
   ¿Quieres ver el contenido completo?
```

---

## Flujo de Decisión

```
┌─────────────────┐
│ Consulta del    │
│ usuario         │
└────────┬────────┘
         │
    ┌────▼────┐
    │ Buscar  │
    │ Notion  │
    └────┬────┘
         │
    ┌────▼────┐     Si
    │¿Encontrado?├──────► Mostrar resultados
    └────┬────┘
         │ No
    ┌────▼────┐
    │ Buscar  │
    │ Internet│
    └────┬────┘
         │
    ┌────▼────┐
    │ Guardar │
    │en Notion│
    └─────────┘
```

---

## Notas

- Prioriza conocimiento propio sobre búsquedas externas
- Aprende de consultas frecuentes
- Sugiere guardar información útil encontrada externamente

## Changelog

### v1.0.0 (2026-01-07)

- Creación inicial
