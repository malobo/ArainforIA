---
id: skill-convert-sql-paradox
name: Convertir SQL a Paradox/BDE
version: 1.0.0
category: domain/database
priority: high
last_updated: 2026-01-08
triggers:
  - "convertir sql"
  - "paradox query"
  - "sql a paradox"
  - "consulta local sql"
---

# 🔄 Convertir SQL a Paradox/BDE

<context>
Esta skill especializa al agente en la conversión de consultas SQL estándar (ANSI/T-SQL) a la sintaxis limitada y específica soportada por el BDE (Borland Database Engine) y tablas Paradox. Es crucial para el mantenimiento de sistemas legacy en Delphi.
</context>

<instruction>
El agente debe transformar la consulta SQL siguiendo estas reglas de compatibilidad BDE/Paradox:

1. **Fechas**: Usar formato americano MM/DD/YYYY entre comillas o parámetros.
    * *Incorrecto*: `'2024-01-31'`
    * *Correcto*: `'01/31/2024'`
2. **Concatenación**: Usar `||` en lugar de `+` o `CONCAT()`.
3. **Joins**: Preferir sintaxis explícita `INNER JOIN` si el LocalSQL lo soporta, o sintaxis implícita `WHERE t1.id = t2.id` para máxima compatibilidad con versiones antiguas.
4. **Funciones de Cadena**:
    * `SUBSTRING` -> `SUBSTR`
5. **Extract**: Uso específico `EXTRACT(YEAR FROM fecha)`.
6. **Wildcards**: `LIKE` usa `%`, pero en algunos contextos antiguos de Paradox se usaba `*` (asumimos LocalSQL nivel 7, usar `%`).
7. **Palabras Reservadas**: Evitar palabras como `DATE`, `USER`, `POSITION` como nombres de campo sin comillas.
</instruction>

<examples>
User: "Convierte este SQL para Paradox: SELECT * FROM ventas WHERE fecha = '2024-05-20' y fullname = nombre + ' ' + apellido"
Agent:
```sql
SELECT *
FROM "ventas.db"
WHERE fecha = '05/20/2024'
  AND fullname = nombre || ' ' || apellido
```
</examples>
