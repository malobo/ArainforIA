---
name: validate-nif-cif
version: 1.0.0
category: domain/verifactu
tags: [nif, cif, validacion, aeat, dni]
author: ARAINFORIA
created: 2026-01-08
complexity: 5
triggers:
  - "validar nif"
  - "comprobar cif"
  - "dni valido"
  - "nif aeat"
---

# Validar NIF/CIF

## Descripción

Valida NIFs y CIFs tanto algorítmicamente como contra la AEAT.

## Validación Local (Algorítmica)

```pascal
function ValidarNIF(const NIF: string): Boolean;
var
  Letras: string;
  Numero: Integer;
  Letra: Char;
begin
  Result := False;
  if Length(NIF) <> 9 then Exit;
  
  Letras := 'TRWAGMYFPDXBNJZSQVHLCKE';
  
  // DNI (8 números + letra)
  if TryStrToInt(Copy(NIF, 1, 8), Numero) then
  begin
    Letra := Letras[(Numero mod 23) + 1];
    Result := UpCase(NIF[9]) = Letra;
  end
  // NIE (X, Y, Z + 7 números + letra)
  else if CharInSet(UpCase(NIF[1]), ['X', 'Y', 'Z']) then
  begin
    case UpCase(NIF[1]) of
      'X': Numero := StrToIntDef(Copy(NIF, 2, 7), -1);
      'Y': Numero := StrToIntDef('1' + Copy(NIF, 2, 7), -1);
      'Z': Numero := StrToIntDef('2' + Copy(NIF, 2, 7), -1);
    end;
    if Numero >= 0 then
    begin
      Letra := Letras[(Numero mod 23) + 1];
      Result := UpCase(NIF[9]) = Letra;
    end;
  end;
end;

function ValidarCIF(const CIF: string): Boolean;
var
  SumaPar, SumaImpar, Total, Digito, I: Integer;
  LetraControl: Char;
  ControlLetras: string;
begin
  Result := False;
  if Length(CIF) <> 9 then Exit;
  if not CharInSet(UpCase(CIF[1]), ['A'..'H', 'J', 'K', 'L', 'M', 'N', 'P', 'Q', 'R', 'S', 'U', 'V', 'W']) then Exit;
  
  ControlLetras := 'JABCDEFGHI';
  SumaPar := 0;
  SumaImpar := 0;
  
  // Posiciones pares (2,4,6)
  for I := 2 to 6 do
    if I mod 2 = 0 then
      SumaPar := SumaPar + StrToIntDef(CIF[I], 0);
  
  // Posiciones impares (1,3,5,7) - multiplicar por 2
  for I := 2 to 8 do
    if I mod 2 = 1 then
    begin
      Digito := StrToIntDef(CIF[I], 0) * 2;
      SumaImpar := SumaImpar + (Digito div 10) + (Digito mod 10);
    end;
  
  Total := SumaPar + SumaImpar;
  Digito := (10 - (Total mod 10)) mod 10;
  LetraControl := ControlLetras[Digito + 1];
  
  // Según tipo, control es número o letra
  if CharInSet(UpCase(CIF[1]), ['K', 'P', 'Q', 'S', 'W']) then
    Result := UpCase(CIF[9]) = LetraControl
  else if CharInSet(UpCase(CIF[1]), ['A', 'B', 'E', 'H']) then
    Result := CIF[9] = Chr(Ord('0') + Digito)
  else
    Result := (CIF[9] = Chr(Ord('0') + Digito)) or (UpCase(CIF[9]) = LetraControl);
end;
```

## Validación contra AEAT

```pascal
uses
  mormot.core.base,
  mormot.net.client;

function ValidarNIFAEAT(const NIF, Nombre: string): Boolean;
var
  Client: THttpClientSocket;
  Response: RawUtf8;
  XML: string;
begin
  Result := False;
  
  XML := Format(
    '<?xml version="1.0"?>' +
    '<soapenv:Envelope xmlns:soapenv="...">' +
    '<VNifV2Ent><Nif>%s</Nif><Nombre>%s</Nombre></VNifV2Ent>' +
    '</soapenv:Envelope>',
    [NIF, Nombre]);
  
  Client := THttpClientSocket.Open('www1.agenciatributaria.gob.es', '443', nlTls);
  try
    Client.Post('/wlpl/BURT-JDIT/ws/VNifV2SOAP', XML, 
      'text/xml; charset=utf-8', Response);
    Result := Pos('<Resultado>IDENTIFICADO</Resultado>', Response) > 0;
  finally
    Client.Free;
  end;
end;
```

## Función Combinada

```pascal
function ValidarNIFCompleto(const NIF, Nombre: string; 
  ValidarOnline: Boolean = False): TResultadoValidacion;
begin
  Result.NIF := NIF;
  Result.ValidoLocal := ValidarNIF(NIF) or ValidarCIF(NIF);
  
  if ValidarOnline and Result.ValidoLocal then
    Result.ValidoAEAT := ValidarNIFAEAT(NIF, Nombre)
  else
    Result.ValidoAEAT := False;
    
  Result.Valido := Result.ValidoLocal and 
    (not ValidarOnline or Result.ValidoAEAT);
end;
```

---

**Estado**: stable
