---
id: skill-debug-connection-issue
name: Depurar Conexión Base de Datos
version: 1.0.0
category: domain/database
priority: high
last_updated: 2026-01-08
triggers:
  - "no conecta"
  - "error conexion"
  - "bd bloqueada"
  - "bde error"
  - "firedac error"
---

# 🔌 Depurar Conexión Base de Datos

<context>
Ayuda a diagnosticar y resolver problemas comunes de conectividad con bases de datos en Delphi, cubriendo tanto BDE (Paradox) como FireDAC/mORMot (SQL).
</context>

<instruction>
Flujo de Diagnóstico:

1. **Verificar Rutas/Alias**:
    * ¿Existe la carpeta física?
    * ¿Está el `PDOXUSRS.NET` bloqueado? (Error `$210D` o `Directory is busy`).
    * **Solución BDE**: Borrar `*.LCK` y `PDOXUSRS.NET` (con todos los usuarios fuera).
2. **FireDAC Connection Def**:
    * Revisar parámetros en tiempo de ejecución: `FDConnection.Params.Text`.
    * Probar conexión mínima: `FDConnection.Connected := True` dentro de un bloque `try..except`.
3. **Firewall/Red**:
    * ¿Puerto 3306 (MySQL) o 1433 (SQL Server) abierto?
    * Usar `Telnet` o `Ping` desde la máquina cliente.
4. **Error Específico**:
    * `Vendor Lib not found`: Falta DLL cliente (`libmysql.dll`, `fbclient.dll`).

</instruction>

<examples>
User: "Me sale 'Directory is busy' al intentar abrir la aplicación"
Agent: "Es un error clásico de BDE/Paradox co bloqueo de archivos de control.
**Pasos para solucionar:**
1. Asegúrate de que **NADIE** tenga el programa abierto.
2. Ve a la raíz de la unidad de red (`Z:\` o ruta de datos).
3. Busca y borra el archivo `PDOXUSRS.NET`.
4. Busca y borra cualquier archivo `.LCK` en la carpeta de datos.
5. Intenta entrar de nuevo."
</examples>
