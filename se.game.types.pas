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
  System.Classes, System.SysUtils, System.Math,
  PXL.Types, PXL.Canvas, PXL.Images, PXL.Textures, PXL.Surfaces, PXL.ImageFormats,
  PXL.Providers, PXL.Formats, PXL.Archives,
  se.game.consts;

type
  TNotifyInfoEvent = procedure (const AMsg: string) of object;

type
  //ÎÄ±¾¶ÔÆë
  TTextAlignment = (taCenter, taLeading, taTrailing);

type
  TEngineCanvas = TCustomCanvas;
  TEnginePixelSurface = TPixelSurface;
  TEngineLockedPixels = TLockedPixels;
  TEngineBlendingEffect = TBlendingEffect;
  TEngineTexture = TCustomLockableTexture;
  TEngineImage = TAtlasImage;
  TEngineArchive = TArchive;

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

end.
