---
name: convert-sql-paradox
version: 1.0.0
category: domain/database
tags: [sql, paradox, bde, conversion, migration]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 4
estimated_tokens: 400-600
triggers:
  - "convertir sql"
  - "paradox query"
  - "sql a paradox"
  - "bde query"
  - "sql estandar"
---

# Convertir SQL ↔ Paradox

## Descripción

Convierte consultas SQL estándar a sintaxis compatible con Paradox/BDE y viceversa.

## AI Context

> **SYSTEM_INSTRUCTION**: Act as a database expert. Convert SQL queries between standard SQL and Paradox/BDE syntax.
> **OUTPUT_FORMAT**: SQL code with comments explaining differences.
> **TOKEN_STRATEGY**: Concise output, focus on practical conversion.

## Tabla de Conversiones

### Funciones de Fecha

| SQL Estándar | Paradox/BDE | Notas |
|--------------|-------------|-------|
| `CURRENT_DATE` | `DATE()` | Fecha actual |
| `CURRENT_TIMESTAMP` | `DATETIME()` | No soportado directamente |
| `DATEADD(day, N, fecha)` | No soportado | Usar lógica Delphi |
| `DATEDIFF(day, f1, f2)` | No soportado | Calcular en Delphi |
| `YEAR(fecha)` | `EXTRACT(YEAR FROM fecha)` | |
| `MONTH(fecha)` | `EXTRACT(MONTH FROM fecha)` | |

### JOINs

| SQL Estándar | Paradox/BDE | Notas |
|--------------|-------------|-------|
| `INNER JOIN` | ✅ Soportado | Usar sintaxis explícita |
| `LEFT JOIN` | ❌ No soportado | Usar subconsultas |
| `RIGHT JOIN` | ❌ No soportado | Usar subconsultas |
| `CROSS JOIN` | Producto cartesiano | `FROM tabla1, tabla2` |

### Funciones de Cadena

| SQL Estándar | Paradox/BDE | Notas |
|--------------|-------------|-------|
| `CONCAT(a, b)` | `a + b` | Concatenación con + |
| `SUBSTRING(s, i, n)` | `SUBSTRING(s FROM i FOR n)` | |
| `UPPER(s)` | `UPPER(s)` | ✅ Compatible |
| `LOWER(s)` | `LOWER(s)` | ✅ Compatible |
| `TRIM(s)` | `TRIM(s)` | ✅ Compatible |
| `LEN(s)` / `LENGTH(s)` | No soportado | Calcular en Delphi |

### Agregados

| SQL Estándar | Paradox/BDE | Notas |
|--------------|-------------|-------|
| `COUNT(*)` | ✅ Compatible | |
| `SUM(campo)` | ✅ Compatible | |
| `AVG(campo)` | ✅ Compatible | |
| `MIN(campo)` | ✅ Compatible | |
| `MAX(campo)` | ✅ Compatible | |
| `GROUP BY` | ✅ Compatible | |
| `HAVING` | ⚠️ Limitado | Algunas restricciones |

### Operadores

| SQL Estándar | Paradox/BDE | Notas |
|--------------|-------------|-------|
| `LIKE '%texto%'` | ✅ Compatible | |
| `IN (lista)` | ✅ Compatible | |
| `BETWEEN a AND b` | ✅ Compatible | |
| `IS NULL` | ✅ Compatible | |
| `COALESCE(a, b)` | No soportado | Usar lógica Delphi |
| `CASE WHEN` | No soportado | Usar lógica Delphi |

## Limitaciones Conocidas de Paradox

1. **Sin LEFT/RIGHT JOIN**: Usar subconsultas o múltiples queries
2. **Sin CASE WHEN**: Implementar lógica en Delphi
3. **Sin subconsultas en SELECT**: Solo en WHERE
4. **Límite de tablas**: Máximo ~40 tablas simultáneas
5. **Sin transacciones**: No hay COMMIT/ROLLBACK
6. **Bloqueos**: Blocking pesimista en red

## Ejemplos

### Ejemplo 1: Conversión de fecha

**SQL Estándar:**

```sql
SELECT * FROM Facturas 
WHERE FechaEmision >= DATEADD(month, -1, CURRENT_DATE)
```

**Paradox/BDE + Delphi:**

```sql
SELECT * FROM Facturas 
WHERE FechaEmision >= :FechaInicio
```

```pascal
// En Delphi
Query.ParamByName('FechaInicio').AsDate := IncMonth(Date, -1);
```

### Ejemplo 2: LEFT JOIN alternativo

**SQL Estándar:**

```sql
SELECT c.Nombre, f.Total
FROM Clientes c
LEFT JOIN Facturas f ON c.IdCliente = f.IdCliente
```

**Paradox/BDE:**

```sql
-- Query 1: Obtener clientes
SELECT * FROM Clientes

-- Query 2: Por cada cliente, buscar facturas
SELECT Total FROM Facturas WHERE IdCliente = :IdCliente
```

### Ejemplo 3: COALESCE alternativo

**SQL Estándar:**

```sql
SELECT COALESCE(Descuento, 0) AS Descuento FROM Productos
```

**Paradox + Delphi:**

```pascal
if Query.FieldByName('Descuento').IsNull then
  Descuento := 0
else
  Descuento := Query.FieldByName('Descuento').AsFloat;
```

## Procedimiento

1. **Identificar funciones no soportadas** en la query original
2. **Buscar en la tabla de conversiones** la alternativa
3. **Si no hay alternativa SQL**, mover lógica a Delphi
4. **Usar parámetros** para valores calculados
5. **Probar con datos reales** antes de desplegar

---

**Estado**: stable  
**Última revisión**: 2026-01-08
