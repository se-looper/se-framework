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

unit se.game.types;

interface

uses
  System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
  System.TypInfo, XSuperObject,
  PXL.Types, PXL.Canvas, PXL.Images, PXL.Textures, PXL.Surfaces, PXL.ImageFormats,
  PXL.Providers, PXL.Formats, PXL.Archives, PXL.Lists,
  se.game.consts;

const
  IntColorRed    = $FFFF0000;
  IntColorOrange = $FFFF8020;
  IntColorYellow = $FFFFFF00;
  IntColorGreen  = $FF008000;
  IntColorCyan   = $FF00FFFF;
  IntColorBlue   = $FF0000FF;
  IntColorPurple = $FF800080;

type
  //碰撞模式
  TCollideMode   = (cmCircle,           // 圆
                    cmRect,             // 矩形
                    cmQuadrangle,       // 四边形
                    cmPolygon           // 多边形
                   );
  //动画播放模式
  TAnimPlayMode  = (pmForward,          // 12345 12345 12345
                    pmBackward,         // 54321 54321 54321
                    pmPingPong          // 12345432123454321
                   );
  //渲染模式
  TDrawMode      = (dmColor,
                    dmRotate,
                    dmTransform
                   );

  //对齐方式
  TAlignMode     = (amNone,             // 0
                    amClient,           // 1
                    amLeftTop,          // 2
                    amRightTop,         // 3
                    amLeftBottom,       // 4
                    amRightBottom,      // 5
                    amCenter,           // 6
                    amCenterTop,        // 7
                    amCenterBottom,     // 8
                    amCenterLeft,       // 9
                    amCenterRight       // 10
                   );

  //文本对齐方式
  TTextAlignMode = (tamLeft,
                    tamRight,
                    tamCenter,
                    tamJustify
                   );


type
  TNotifyInfoEvent = procedure (AMsg: string) of object;

type
  TEngineCanvas = TCustomCanvas;
  TEnginePixelSurface = TPixelSurface;
  TEnginePixels = TLockedPixels;
  TEngineBlendingEffect = TBlendingEffect;
  TEngineTexture = TCustomLockableTexture;
  TEngineImage = TAtlasImage;
  TEngineArchive = TArchive;
  TEngineIntRectList = TIntRectList;

type
  TPolygon = record
    Values: array of TPoint2f;
  public
    function Overlaps(const APolygon: TPolygon): Boolean;
    function Contains(const APoint: TPoint2f): Boolean;
  end;

  TTextureChunk = record
    Name: string;
    Rect: TIntRect;
    function Formatting: string;
    class function Empty: TTextureChunk; static;
    class function Make(const AImage: TEngineImage): TTextureChunk; static;
  end;

  TSceneData = class
  private type
    TFontProp = record
      Size: Word;
      Color: TIntColor;
    public
      procedure SetColor(const AColor: string);
    end;

    TTextProp = record
      Name: string;
      Text: string;
      Font: TFontProp;
      TextAlign: TTextAlignMode;
    public
      procedure SetTextAlign(const ATextAlign: string);
      function IsNull: Boolean;
    end;
  public type
    TLayoutItem = class
    private
      FName, FImageName: string;
      FWidth, FHeight: Integer;
      FAlign: TAlignMode;
      FHitTest: Boolean;
      FZOrder: Integer;
      FText: TTextProp;
      FMargins: TIntRect;
      FChilds: TObjectList<TLayoutItem>;
    public
      constructor Create;
      destructor Destroy; override;

      procedure SetAlign(const AAlign: string);

      property Name: string read FName;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property ImageName: string read FImageName;
      property Align: TAlignMode read FAlign;
      property HitTest: Boolean read FHitTest;
      property ZOrder: Integer read FZOrder;
      property Margins: TIntRect read FMargins;
      property Text: TTextProp read FText;
      property Childs: TObjectList<TLayoutItem> read FChilds;
    end;
  private
    FName, FMapFile: string;
    FLayoutItems: TObjectList<TLayoutItem>;
    function GetCount: Integer;
    function GetItem(const Index: Integer): TLayoutItem;
    function Parse(const AFile: string): string; overload;
    procedure Parse(const AData: ISuperObject;
      const AItems: TObjectList<TLayoutItem>); overload;
  public
    constructor Create(const AFile: string);
    destructor Destroy; override;

    property Name: string read FName;
    property Items[const Index: Integer]: TLayoutItem read GetItem;
    property Count: Integer read GetCount;
  end;

implementation

{ TPolygon }

function TPolygon.Contains(const APoint: TPoint2f): Boolean;
var
  N, Counter , I : Integer;
  XInters : Real;
  P1, P2 : TPoint2f;
begin
  N := High(Self.Values);
  Counter := 0;
  P1 := Self.Values[0];
  for I := 1 to N do
  begin
    P2 := Self.Values[I mod N];
    if APoint.y > Min(P1.y, P2.y) then
      if APoint.y <= Max(P1.y, P2.y) then
        if APoint.x <= Max(P1.x, P2.x) then
          if P1.y <> P2.y then
          begin
            XInters := (APoint.y - P1.y) * (P2.x - P1.x) / (P2.y - P1.y) + P1.x;
            if (P1.x = P2.x) or (APoint.x <= XInters) then Inc(Counter);
          end;
    P1 := P2;
  end;
  Result := (Counter mod 2 <> 0);
end;

function TPolygon.Overlaps(const APolygon: TPolygon): Boolean;
var
  Poly1, Poly2 : TPolygon;
  I, J : Integer;
  xx , yy : Single;
  StartP, EndP : Integer;
  Found : Boolean;
begin
  Found := False;
  { Find polygon with fewer points }
  if High(Self.Values) < High(APolygon.Values) then
  begin
    Poly1 := Self;
    Poly2 := APolygon;
  end
  else
  begin
    Poly1 := APolygon;
    Poly2 := Self;
  end;

  for I := 0 to High(Poly1.Values) - 1 do
  begin
    { Trace new line }
    StartP := Round(Min(Poly1.Values[I].x, Poly1.Values[I+1].x));
    EndP   := Round(Max(Poly1.Values[I].x, Poly1.Values[I+1].x));


    if StartP = EndP then
    { A vertical line (ramp = inf) }
    begin
      xx := StartP;
      StartP := Round(Min(Poly1.Values[I].y, Poly1.Values[I+1].y));
      EndP   := Round(Max(Poly1.Values[I].y, Poly1.Values[I+1].y));
      { Follow a vertical line }
      for J := StartP to EndP do
      begin
        { line equation }
        if Poly2.Contains(Point2f(xx,J)) then
        begin
          Found := True;
          Break;
        end;
      end;
    end
    else
    { Follow a usual line (ramp <> inf) }
    begin
      { A Line which X is its variable i.e. Y = f(X) }
      if Abs(Poly1.Values[I].x -  Poly1.Values[I+1].x) >= Abs(Poly1.Values[I].y -  Poly1.Values[I+1].y) then
      begin
        StartP := Round(Min(Poly1.Values[I].x, Poly1.Values[I+1].x));
        EndP   := Round(Max(Poly1.Values[I].x, Poly1.Values[I+1].x));
        for J := StartP to EndP do
        begin
          xx := J;
          { line equation }
          yy := (Poly1.Values[I+1].y - Poly1.Values[I].y) /
                (Poly1.Values[I+1].x - Poly1.Values[I].x) *
                (xx - Poly1.Values[I].x) + Poly1.Values[I].y;
          if Poly2.Contains(Point2f(xx,yy)) then
          begin
            Found := True;
            Break;
          end;
        end;
      end
      { A Line which Y is its variable i.e. X = f(Y) }
      else
      begin
        StartP := Round(Min(Poly1.Values[I].y, Poly1.Values[I+1].y));
        EndP   := Round(Max(Poly1.Values[I].y, Poly1.Values[I+1].y));
        for J := StartP to EndP do
        begin
          yy := J;
          { line equation }
          xx := (Poly1.Values[I+1].x - Poly1.Values[I].x) /
                (Poly1.Values[I+1].y - Poly1.Values[I].y) *
                (yy - Poly1.Values[I].y) + Poly1.Values[I].x;
          if Poly2.Contains(Point2f(xx,yy)) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
    end;
    if Found then Break;
  end;

  { Maybe one polygon is completely inside another }
  if not Found then
    Found := Poly2.Contains(Poly1.Values[0]) or Poly1.Contains(Poly2.Values[0]);

  Result := Found;
end;

{ TTextureChunk }

class function TTextureChunk.Empty: TTextureChunk;
begin
  Result.Name:= '';
  Result.Rect:= ZeroIntRect;
end;

function TTextureChunk.Formatting: string;
begin
  Result:= Format(cChunkRectFormat,[Self.Name, Self.Rect.Left,
    Self.Rect.Top, Self.Rect.Right, Self.Rect.Bottom]);
end;

class function TTextureChunk.Make(const AImage: TEngineImage): TTextureChunk;
begin
  Result.Name:= AImage.Name;
  if AImage.TextureCount = 0 then
    Result.Rect:= ZeroIntRect
  else
    Result.Rect:= IntRect(0, 0, AImage.Texture[0].Width, AImage.Texture[0].Height);
end;

{ TSceneData.TFontProp }

procedure TSceneData.TFontProp.SetColor(const AColor: string);
const
  cZeroV = '00000000';
var
  LHexColor: string;
begin
  LHexColor:= AColor.Replace('#','').Replace('$','');
  LHexColor:= '$' + Copy(cZeroV, 1, 8-Length(LHexColor)) + LHexColor;
  Self.Color:= StrToIntDef(LHexColor, 0);
end;

{ TSceneData.TTextProp }

function TSceneData.TTextProp.IsNull: Boolean;
begin
  Result:= Self.Name.IsEmpty or Self.Text.IsEmpty;
end;

procedure TSceneData.TTextProp.SetTextAlign(const ATextAlign: string);
begin
  Self.TextAlign:= TTextAlignMode(GetEnumValue(TypeInfo(TTextAlignMode), ATextAlign));
end;

{ TSceneData.TLayoutItem }

constructor TSceneData.TLayoutItem.Create;
begin
  inherited;
  FChilds:= TObjectList<TLayoutItem>.Create;
end;

destructor TSceneData.TLayoutItem.Destroy;
begin
  FreeAndNil(FChilds);
  inherited;
end;

procedure TSceneData.TLayoutItem.SetAlign(const AAlign: string);
begin
  FAlign:= TAlignMode(GetEnumValue(TypeInfo(TAlignMode), AAlign));
end;

{ TSceneData }

constructor TSceneData.Create(const AFile: string);
var
  jo: ISuperObject;
  I: Integer;
begin
  inherited Create;
  FLayoutItems:= TObjectList<TLayoutItem>.Create;
  if not FileExists(AFile) then
    Exit;
  jo:= SO(Self.Parse(AFile));
  try
    FName:= ExtractFileName(AFile).Replace('.scene', '');
    FMapFile:= jo.S['map'];
    for I:= 0 to jo.A['gui'].Length -1 do
      Self.Parse(jo.A['gui'].O[I], FLayoutItems);
  finally
    jo:= nil;
  end;
end;

destructor TSceneData.Destroy;
begin
  FreeAndNil(FLayoutItems);
  inherited;
end;

function TSceneData.GetCount: Integer;
begin
  Result:= FLayoutItems.Count;
end;

function TSceneData.GetItem(const Index: Integer): TLayoutItem;
begin
  if Index < FLayoutItems.Count then
    Result:= FLayoutItems[Index]
  else
    Result:= nil;
end;

function TSceneData.Parse(const AFile: string): string;
var
  LStrings: TStrings;
begin
  LStrings:= TStringList.Create;
  try
    LStrings.LoadFromFile(AFile, TEncoding.ANSI);
    Result:= LStrings.Text;
  finally
    FreeAndNil(LStrings);
  end;
end;

procedure TSceneData.Parse(const AData: ISuperObject;
  const AItems: TObjectList<TLayoutItem>);
var
  I: Integer;
  LItem: TLayoutItem;
begin
  LItem:= TLayoutItem.Create;
  LItem.FName:= AData.S['name'];
  LItem.FWidth:= AData.I['width'];
  LItem.FHeight:= AData.I['height'];
  LItem.FImageName:= AData.S['image'];
  LItem.SetAlign(AData.S['align']);
  LItem.FHitTest:= AData.B['hittest'];
  LItem.FZOrder:= AData.I['zorder'];
  LItem.FMargins.Left  := AData.O['margins'].I['left'];
  LItem.FMargins.Top   := AData.O['margins'].I['top'];
  LItem.FMargins.Right := AData.O['margins'].I['right'];
  LItem.FMargins.Bottom:= AData.O['margins'].I['bottom'];
  if AData.Contains('text') then
  begin
    LItem.FText.Name:= AData.O['text'].S['name'];
    LItem.FText.Text:= AData.O['text'].S['text'];
    LItem.FText.Font.Size := AData.O['text'].O['font'].I['size'];
    LItem.FText.Font.SetColor(AData.O['text'].O['font'].S['color']);
    LItem.FText.SetTextAlign(AData.O['text'].S['textalign']);
  end;
  AItems.Add(LItem);
  //
  for I:= 0 to AData.A['childs'].Length -1 do
    Parse(AData.A['childs'].O[I], LItem.Childs);
end;

end.
