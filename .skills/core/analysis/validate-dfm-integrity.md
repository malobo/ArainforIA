---
name: validate-dfm-integrity
version: 1.0.0
category: core/analysis
tags: [dfm, form, debugging, validation, vcl]
author: ARAINFORIA
created: 2026-01-08
updated: 2026-01-08
complexity: 4
estimated_tokens: 400-500
triggers:
  - "dfm roto"
  - "componente no encontrado"
  - "error form"
  - "validar dfm"
  - "formulario corrupto"
---

# Validar Integridad de DFM

## Descripción

Valida archivos .dfm para detectar referencias rotas, componentes faltantes y propiedades huérfanas.

## AI Context

> **SYSTEM_INSTRUCTION**: Analyze Delphi .dfm files for integrity issues. Report problems clearly.
> **OUTPUT_FORMAT**: Checklist with findings and recommendations.
> **TOKEN_STRATEGY**: Focus on actionable issues, skip verbose descriptions.

## Checklist de Validación

### 1. Estructura Básica

- [ ] El archivo comienza con `object` o `inherited`
- [ ] Todos los bloques `object` tienen `end` correspondiente
- [ ] La indentación es consistente
- [ ] No hay caracteres corruptos

### 2. Referencias de Componentes

- [ ] Todos los componentes referenciados en `.pas` existen en `.dfm`
- [ ] No hay componentes en `.dfm` sin declaración en `.pas`
- [ ] Las propiedades `DataSource`, `DataField` apuntan a componentes existentes
- [ ] Los eventos (`OnClick`, etc.) tienen métodos correspondientes en `.pas`

### 3. Propiedades Comunes con Problemas

| Propiedad | Problema Común | Solución |
|-----------|----------------|----------|
| `DataSource` | Componente eliminado | Limpiar referencia |
| `PopupMenu` | Menú eliminado | Limpiar referencia |
| `ImageList` | Lista eliminada | Limpiar referencia |
| `Action` | Acción eliminada | Limpiar referencia |
| `Font.Name` | Fuente no instalada | Cambiar a fuente estándar |

### 4. Componentes de Terceros

- [ ] Verificar que los paquetes de componentes están instalados
- [ ] Comprobar que las versiones son compatibles
- [ ] Revisar componentes legacy sin soporte

## Errores Comunes y Soluciones

### Error: "Class TXxxxx not found"

```
Síntoma: Al abrir el formulario aparece este error
Causa: El componente no está instalado en el IDE
Solución:
  1. Instalar el paquete del componente
  2. O eliminar el componente del .dfm manualmente
```

**Eliminación manual:**

```
Buscar y eliminar el bloque:
  object NombreComponente: TComponenteFaltante
    ...
  end
```

### Error: "Property Xxxx does not exist"

```
Síntoma: Error al cargar el formulario
Causa: Propiedad de versión anterior o componente actualizado
Solución:
  1. Abrir .dfm como texto
  2. Eliminar la línea de la propiedad
  3. Guardar y reabrir
```

### Error: "Invalid property value"

```
Síntoma: Valor de propiedad no reconocido
Causa: Enumeración cambiada en nueva versión
Solución:
  1. Identificar la propiedad problemática
  2. Cambiar al valor válido equivalente
```

## Procedimiento de Validación

### Paso 1: Verificar sintaxis

```bash
# Abrir como texto y buscar problemas obvios
1. Verificar que cada 'object' tiene su 'end'
2. Buscar caracteres extraños: ÿ, þ, etc.
3. Verificar codificación UTF-8 o ANSI consistente
```

### Paso 2: Comparar con .pas

```pascal
// Buscar en .pas los componentes declarados
type
  TFormXxx = class(TForm)
    // Todos estos deben existir en .dfm
    Button1: TButton;
    Edit1: TEdit;
```

### Paso 3: Verificar referencias cruzadas

```
En .dfm buscar propiedades que referencian otros componentes:
  DataSource = dsClientes    <- ¿Existe dsClientes?
  PopupMenu = pmContexto     <- ¿Existe pmContexto?
  ImageList = ilIconos       <- ¿Existe ilIconos?
```

### Paso 4: Limpiar referencias huérfanas

```
# En el .dfm, eliminar líneas como:
  DataSource = dsEliminado   <- Si dsEliminado no existe
  
# Dejar la propiedad vacía o eliminar la línea
```

## Script de Limpieza Automática

```pascal
procedure LimpiarDFM(const FileName: string);
var
  Lines: TStringList;
  I: Integer;
  Line: string;
begin
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    
    for I := Lines.Count - 1 downto 0 do
    begin
      Line := Trim(Lines[I]);
      
      // Eliminar propiedades problemáticas conocidas
      if StartsText('DataSource = ds', Line) or
         StartsText('DataField = ', Line) then
      begin
        // Verificar si el componente existe...
        // Si no existe, eliminar la línea
      end;
    end;
    
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;
```

## Herramientas Útiles

1. **GExperts**: Tiene utilidad para limpiar DFMs
2. **DFMCheck**: Herramienta standalone para validación
3. **Beyond Compare**: Para comparar versiones del DFM

---

**Estado**: stable  
**Última revisión**: 2026-01-08
