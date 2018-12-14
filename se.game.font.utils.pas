{******************************************************************************}
{                                                                              }
{       SE Network Development Framework                                       }
{                                                                              }
{       Copyright (c) 2018 looper(2031056602@qq.com)                           }
{                                                                              }
{       Source: https://github.com/looper/se-framework                         }
{       Homepage: http://www.asphyre.cn                                        }
{                                                                              }
{******************************************************************************}

unit se.game.font.utils;

interface

uses
  System.Classes, System.SysUtils, System.Math, PXL.Types;

type
  TFontUtils = class
    class function CharValue(const AChar: Char): Cardinal; static;
    class function CharToString(const AChar: Char): string; static;
    class procedure AppendChar(var AString: string; const AChar: Char); static;
    class function IsPunctuation(const AChar: Char): Boolean; static;
    class function IsAlphaNumeric(const AChar: Char): Boolean; static;
    class function Upper(const AChar: Char): Char; static;
    class function Inc(out AValue: Single; const AIncrement, AInitValue: Single): Single; static;
    class function HexToColor(const AHexColor: string): TIntColor; static;
    class function ColorToHex(const AColor: TIntColor): string; static;
  end;

implementation

{ TFontUtils }

class procedure TFontUtils.AppendChar(var AString: string;
  const AChar: Char);
begin
  AString:= AString + CharToString(AChar);
end;

class function TFontUtils.IsPunctuation(const AChar: Char): Boolean;
begin
  Result:= Cardinal(AChar) in [33,44,46,63]; //! , . ?
end;

class function TFontUtils.CharToString(const AChar: Char): string;
var
  N: Cardinal;
  A, B, C: Byte;
begin
  N:= CharValue(AChar);

  if N < $80 then
  begin
    Result:= string('' + AChar);
    Exit;
  end;

  if N <= $7FF then
  begin
    A:= $C0 Or (N Shr 6);
    B:= $80 Or (N and $3F);
    Result:= '' + Char(A) + Char(B);
    Exit;
  end;

  A:= $E0 Or (N Shr 12);
  B:= $80 Or ((N Shr 6) And $3F);
  C:= $80 Or (N And $3F);
  Result:= '' + Char(A) + Char(B) + Char(C);
end;

class function TFontUtils.CharValue(const AChar: Char): Cardinal;
begin
  Result:= Cardinal(AChar);
end;

class function TFontUtils.ColorToHex(const AColor: TIntColor): string;
begin
  FmtStr(Result, '%0.8x', [AColor]);
end;

class function TFontUtils.HexToColor(const AHexColor: string): TIntColor;
const
  cZeroV = '00000000';
var
  LHexColor: string;
begin
  LHexColor:= AHexColor.Replace('#','').Replace('$','');
  LHexColor:= '$' + Copy(cZeroV, 1, 8-Length(LHexColor)) + LHexColor;
  Result:= StrToIntDef(LHexColor, 0);
end;

class function TFontUtils.Inc(out AValue: Single; const AIncrement,
  AInitValue: Single): Single;
begin
  AValue:= AInitValue + AIncrement;
  Result:= AValue;
end;

class function TFontUtils.IsAlphaNumeric(const AChar: Char): Boolean;
var
  LCode: Cardinal;
begin
  LCode:= Cardinal(AChar);
  Result:= ((LCode>=48) and (LCode<=57)) or   //0-9
           ((LCode>=65) and (LCode<=90)) or   //A-Z
           ((LCode>=97) and (LCode<=122))     //a-z
        or ((LCode>=128) and (LCode<=256))    //ascii)?
        or IsPunctuation(AChar);
end;

class function TFontUtils.Upper(const AChar: Char): Char;
begin
  if (AChar>='a')   and (AChar<='z')   then
    Result:= Char(Cardinal(AChar) - 32)
  else
  if (AChar>=#1072) and (AChar<=#1103) then // cyrillic
    Result:= Char(Cardinal(AChar) - 32)
  else
  if (AChar>=#1104) and (AChar<=#1119) then // cyrillic2
    Result:= Char(Cardinal(AChar) - 80)
  else
    Result:= AChar;
end;

end.
