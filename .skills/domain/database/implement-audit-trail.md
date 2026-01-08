---
id: skill-implement-audit-trail
name: Implementar Auditoría de Datos
version: 1.0.0
category: domain/database
priority: medium
last_updated: 2026-01-08
triggers:
  - "auditar cambios"
  - "historial registros"
  - "log modificaciones"
  - "trigger auditoria"
---

# 🕵️ Implementar Auditoría de Datos

<context>
Guiar en la implementación de un sistema de "Audit Trail" para registrar quién y cuándo modificó registros. Se puede hacer vía Triggers de Base de Datos (recomendado) o en la capa de Aplicación (Eventos Delphi).
</context>

<instruction>
Estrategias de Auditoría:

1. **Tabla de Auditoría Única**:
    * `AUDIT_LOG (ID, TableName, RecordID, Action, User, Timestamp, OldValue, NewValue)`.
2. **Columnas de Auditoría**:
    * Añadir `CreatedBy`, `CreatedAt`, `UpdatedBy`, `UpdatedAt` a cada tabla importante.
3. **Implementación Delphi (`BeforePost`)**:
    * En el `DataModule`, asignar valores a los campos de auditoría antes de guardar.
    * Ventaja: Conoce al usuario de la aplicación.
    * Desventaja: No captura cambios hechos fuera de la app (SQL directo).

</instruction>

<examples>
User: "Quiero guardar quién modificó una factura"
Agent: "Lo más sencillo en Delphi es usar el evento `BeforePost` del TTable/TQuery:

```delphi
procedure TDataModule1.TableFacturasBeforePost(DataSet: TDataSet);
begin
  if DataSet.State = dsInsert then
  begin
    DataSet.FieldByName('CREATED_AT').AsDateTime := Now;
    DataSet.FieldByName('CREATED_BY').AsString := GlobalUser;
  end
  else if DataSet.State = dsEdit then
  begin
    DataSet.FieldByName('UPDATED_AT').AsDateTime := Now;
    DataSet.FieldByName('UPDATED_BY').AsString := GlobalUser;
  end;
end;
```

Asegúrate de crear esos campos en la tabla Paradox/SQL primero."
</examples>
