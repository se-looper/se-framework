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

unit se.game.font.classes;

interface

uses
  System.Classes, System.SysUtils, System.Math,
  PXL.Types, se.game.types, se.game.font.types, se.game.font.utils;

type
  TStBttVertex = record
    x, y, cx, cy: Smallint;
    vertexType, padding: Byte;
  end;

  TStBttVertexArray = record
    list: array Of TStBttVertex;
    count: Integer;
  end;

  TStBttPoint = record
    x, y: Single;
  end;

  TStBttPointArray = record
    list: array Of TStBttPoint;
    count: Integer;
  end;

  TStBttActiveEdge = class
    x, dx: Integer;
    ey: Single;
    valid: Integer;
    next: TStBttActiveEdge;
  end;

  TContourArray = record
    list: array Of Integer;
    count: Integer;
  end;

  TStBttEdge = record
    x0,y0, x1,y1: Single;
    invert: Integer;
  end;

  PFontKerning = ^TFontKerning;
  TFontKerning = record
    NextCharID: Cardinal;
    Amount: SmallInt;
  end;

  TFontCharStyle = record
    WavyText     : Boolean;   //波浪
    Bold         : Boolean;   //粗体
    Italics      : Boolean;   //斜体
    Underline    : Boolean;   //下划线
    StrikeThrough: Boolean;   //删除线
    Link: string;             //链接(如URL等)
    Color: TIntColor;         //颜色
    ImageName: string;        //图片名称(如表情图标等)
  public
    class function Default(const AColor: TIntColor = IntColorBlack): TFontCharStyle; static;
  end;

  TEdgeList = class
  private
    FList: array Of TStBttEdge;
    FCount: Integer;
    procedure QuickSort(L, R: Integer);
  public
    constructor Create();
    destructor Destroy; override;

    function Add(const Item: TStBttEdge): Integer;
    function Get(Index: Integer): TStBttEdge;
    procedure Fix(Index:Integer; Y0:Single);
    procedure Sort();

    property Count: Integer read FCount;
  end;

  TStringIterator = record
  private
    FChars: TArray<Char>;
    FIndex, FCount: Integer;

    function Next: Char;
    function GetCurrChar: Char;
    function GetCurrCharID: Cardinal;
    function GetNextChar: Char;
    function GetNextCharID: Cardinal;
  public
    procedure Create(const AString: string; const AAutoCase: Boolean = False);

    function Eof: Boolean;

    property CurrChar: Char read GetCurrChar;
    property CurrCharID: Cardinal read GetCurrCharID;
    property NextChar: Char read GetNextChar;
    property NextCharID: Cardinal read GetNextCharID;
  end;

Implementation

{ TFontCharStyle }

class function TFontCharStyle.Default(const AColor: TIntColor): TFontCharStyle;
begin
  Result.Italics:= False;
  Result.Underline:= False;
  Result.StrikeThrough:= False;
  Result.WavyText:= False;
  Result.Link:= '';
  Result.Color:= AColor;
  Result.ImageName:= '';
end;

{ TEdgeList }

constructor TEdgeList.Create();
begin
  SetLength(FList, 16);
  FCount:= 0;
end;

destructor TEdgeList.Destroy;
var
  I:Integer;
begin
  for I:=0 to Pred(FCount) Do
  begin
//    FreeMem(FList[i]);
  end;
  SetLength(FList, 0);
  FCount:= 0;
end;

function TEdgeList.Add(Const Item: TStBttEdge): Integer;
begin
  Result:= FCount;
  while Result >= Length(FList) do
    SetLength(FList, Length(FList) * 2);

  FList[Result]:= Item;
  Inc(FCount);
end;

function TEdgeList.Get(Index: Integer): TStBttEdge;
begin
  if (Index < 0) or (Index >= FCount) then
  begin
  	//Result:= nil;
  	Exit;
  end;
  Result:= FList[Index];
end;

function EdgeCompare(Const pa, pb: TStBttEdge): Integer;
begin
   if pa.y0 < pb.y0 then
      Result:= -1
   else if pa.y0 > pb.y0 then
      Result:= 1
   else
      Result:= 0;
end;

procedure TEdgeList.QuickSort(L, R: Integer);
var
  I, J: Integer;
  P, T: TStBttEdge;
begin
  repeat
    I:= L;
    J:= R;
    P:= FList[(L + R) shr 1];
    repeat
      while EdgeCompare(FList[I], P) < 0 do
        Inc(I);

      while EdgeCompare(FList[J], P) > 0 do
        Dec(J);

      if I <= J then
      begin
        T:= FList[I];
        FList[I]:= FList[J];
        FList[J]:= T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort( L, J);
    L:= I;
  until I >= R;
end;

procedure TEdgeList.Sort();
begin
  if (Count > 0) then
    QuickSort(0, FCount - 1);
end;

procedure TEdgeList.Fix(Index: Integer; Y0: Single);
begin
  FList[Index].y0:= Y0;
end;

{ TStringIterator }

procedure TStringIterator.Create(const AString: string;
  const AAutoCase: Boolean);
begin
  FChars:= AString.ToCharArray;
  FIndex:= -1;
  FCount:= Length(FChars);
end;

function TStringIterator.GetCurrChar: Char;
begin
  if FIndex < FCount then
    Result:= FChars[FIndex]
  else
    Result:= TFontTypes.NullChar;
end;

function TStringIterator.GetCurrCharID: Cardinal;
begin
  Result:= TFontUtils.CharValue(Self.CurrChar);
end;

function TStringIterator.GetNextChar: Char;
begin
  if FIndex < FCount-1 then
    Result:= FChars[FIndex+1]
  else
    Result:= TFontTypes.NullChar;
end;

function TStringIterator.GetNextCharID: Cardinal;
begin
  Result:= TFontUtils.CharValue(Self.NextChar);
end;

function TStringIterator.Eof: Boolean;
begin
  Result:= FIndex >= FCount - 1;
  if not Result then
    Self.Next;
end;

function TStringIterator.Next: Char;
begin
  Inc(FIndex);
  if FIndex < FCount then
    Result:= FChars[FIndex]
  else
    Result:= TFontTypes.NullChar;
end;

end.
