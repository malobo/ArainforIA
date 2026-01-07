---
id: skill-project-erpw
name: Proyecto ERPW
version: 2.0
category: project
priority: high
last_updated: 2026-01-06
triggers:
  - "erpw"
  - "erp central"
  - "erpw.new"
  - "gestion comercial"
---

# Proyecto ERPW

<context>
Sistema ERP central legacy/modernizado en `C:\Arainfor\ERPW.NEW\`.
Gestión Comercial, Almacén y Contabilidad. Multi-empresa.
</context>

<instruction>
1. **Cuidado con Dependencias**: El proyecto tiene dependencias circulares históricas. Evita añadir `uses` innecesarios en `interface`.
2. **Integración Verifactu**: El objetivo es integrar los módulos compartidos de Verifactu, no reescribir todo el ERP.
</instruction>

<examples>
User: "Compila el ERP"
Assistant: (Call tool `run_command` with `dcc32.exe ... ERPW.dpr`)
</examples>
