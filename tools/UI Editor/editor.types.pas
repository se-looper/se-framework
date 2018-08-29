unit editor.types;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Rtti, System.Types,
  System.Generics.Collections, System.TypInfo, PXL.Types, se.game.exlib.ui;

type
  TEditorColor = type TIntColor;

  TStringsHelper = class helper for TStrings
    procedure AddFromArray(const AValue: TArray<string>);
  end;

  TImageRecord = record
    Name: string;
    Left, Top, Right, Bottom: Integer;
  private
    procedure Reset;
  public
    procedure Parse(const AString: string);
  end;

  TEditorControlType = (ectForm, ectButton, ectLabel, ectEditBox, ectImage,
    ectListBox, ectCheckBox, ectRadioButton, ectProgressBar,
    ectTrackBar, ectMemo);

  TPropertyType = (ptString, ptInteger, ptFloat, ptBoolean, ptColor,
    ptImage, ptList, ptEnumSet);

  TProperty = record
    Name: string;
    Value: string;
    &Type: TPropertyType;
    Items: TArray<string>;
  end;

  TPropertyList = record
    Properties: TArray<TProperty>;
  private
    function GetCount: Integer;
    function ValueFromName(const AName: string): string;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
  public
    function Add(const AName, AValue: string;
      const AType: TPropertyType): Integer; overload;
    function Add(const AName: string; const AValue: Boolean): Integer; overload;
    function Add(const AName: string; const AValue: Integer): Integer; overload;
    function Add(const AName: string; const AValue: Single): Integer; overload;
    function Add<T{: enum}>(const AName: string; AValue: T): Integer; overload;
    function Add(const AName, AValue: string; const AType: TPropertyType;
      const AItems: TArray<string>): Integer; overload;

    function AddFontName(const AValue: string): Integer;
    function AddFontSize(const AValue: Word): Integer;
    function AddFontStyle(const AValue: string): Integer;
    function AddFontColor(const AValue: string): Integer;

    property Count: Integer read GetCount;
    property Value[const Name: string]: string read GetValue write SetValue;
  end;

implementation

{ TStringsHelper }

procedure TStringsHelper.AddFromArray(const AValue: TArray<string>);
var
  I: Integer;
begin
  for I:= 0 to Length(AValue) -1 do
    Self.Add(AValue[I]);
end;

{ TImageRecord }

procedure TImageRecord.Reset;
begin
  Self.Name   := '';
  Self.Left   := 0;
  Self.Top    := 0;
  Self.Right  := 0;
  Self.Bottom := 0;
end;

procedure TImageRecord.Parse(const AString: string);
var
  LPos: Integer;
  LRectArray: TArray<string>;
begin
  Self.Reset;
  if AString.Trim.IsEmpty then Exit;  
  LPos:= Pos('(', AString) - 1;
  Self.Name:= AString.Substring(0, LPos);
  LRectArray:= AString
                 .Substring(LPos, Length(AString))
                 .Trim(['(', ')'])
                 .Split([',']);
  Self.Left   := StrToIntDef(LRectArray[0], 0);
  Self.Top    := StrToIntDef(LRectArray[1], 0);
  Self.Right  := StrToIntDef(LRectArray[2], 0);
  Self.Bottom := StrToIntDef(LRectArray[3], 0);
end;

{ TPropertyList }

function TPropertyList.Add(const AName, AValue: string;
  const AType: TPropertyType): Integer;
begin
  Result:= Self.Count;
  SetLength(Properties, Result + 1);
  Properties[Result].Name := AName;
  Properties[Result].Value:= AValue;
  Properties[Result].&Type:= AType;
end;

function TPropertyList.Add(const AName, AValue: string;
  const AType: TPropertyType; const AItems: TArray<string>): Integer;
begin
  Result:= Self.Add(AName, AValue, AType);
  Properties[Result].Items:= AItems;
end;

function TPropertyList.Add<T>(const AName: string; AValue: T): Integer;
var
  RTX: TRttiContext;
  RT: TRttiType;
  ROT: TRttiOrdinalType;
  I: Integer;
  LNames: TArray<string>;
  LValue: string;
begin
  RT:= RTX.GetType(TypeInfo(T));
  if RT.IsOrdinal then
  begin
    LValue:= TRttiEnumerationType.GetName<T>(AValue);
    //
    ROT:= RT.AsOrdinal;
    SetLength(LNames, ROT.MaxValue + 1);
    for I:= ROT.MinValue to ROT.MaxValue do
      LNames[I]:= System.TypInfo.GetEnumName(TypeInfo(T), I);
    Result:= Self.Add(AName, LValue, TPropertyType.ptList, LNames);
  end;
end;

function TPropertyList.AddFontName(const AValue: string): Integer;
begin
  Result:= Self.Add('FontName', AValue, TPropertyType.ptList, ['simsun', 'number_style01']);
end;

function TPropertyList.AddFontSize(const AValue: Word): Integer;
begin
  Result:= Self.Add('FontSize', AValue.ToString, TPropertyType.ptInteger);
end;

function TPropertyList.AddFontStyle(const AValue: string): Integer;
begin
  Result:= Self.Add('FontStyle', AValue, TPropertyType.ptEnumSet,
    ['fsBold','fsItalic','fsUnderline','fsStrikeOut']);
end;

function TPropertyList.Add(const AName: string; const AValue: Boolean): Integer;
begin
  Result:= Self.Add(AName, AValue.ToString(TUseBoolStrs.True), TPropertyType.ptBoolean);
end;

function TPropertyList.Add(const AName: string; const AValue: Integer): Integer;
begin
  Result:= Self.Add(AName, AValue.ToString, TPropertyType.ptInteger);
end;

function TPropertyList.Add(const AName: string; const AValue: Single): Integer;
begin
  Result:= Self.Add(AName, AValue.ToString, TPropertyType.ptFloat);
end;

function TPropertyList.AddFontColor(const AValue: string): Integer;
begin
  Result:= Self.Add('FontColor', AValue, TPropertyType.ptColor);
end;

function TPropertyList.GetCount: Integer;
begin
  Result:= Length(Properties);
end;

function TPropertyList.ValueFromName(const AName: string): string;
var
  I: Integer;
begin
  for I:= 0 to Self.Count -1 do
    if Properties[I].Name.Equals(AName) then
    begin
      Result:= Properties[I].Value;
      Break;
    end;
end;

function TPropertyList.GetValue(const Name: string): string;
begin
  Result:= ValueFromName(Name);
end;

procedure TPropertyList.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  for I:= 0 to Self.Count -1 do
    if Properties[I].Name.Equals(Name) then
    begin
      Properties[I].Value:= Value;
      Break;
    end;
end;

end.
