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

unit se.game.helper;

interface

uses
  System.Classes, System.SysUtils, System.Math,
  PXL.Types, PXL.Classes, PXL.Canvas, PXL.Textures, PXL.Surfaces, PXL.Images,
  PXL.ImageFormats, PXL.Providers, PXL.Formats, PXL.Archives,
  se.game.consts, se.game.types, se.utils.client;

type
  TQuadHelper = record helper for TQuad
    function Overlaps(const AQuad: TQuad): Boolean;
    procedure Trunc(const ATrunc: Boolean = True);
  end;

  TEngineImageHelper = class helper for TAtlasImage
  private
    function GetDefaultTexture: TEngineTexture;
    function GetPatternRect(const Index: Integer): TIntRect;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    /// <summary>
    ///   获取特定格式的图片描述字符串
    /// </summary>
    function Formatting: string;
    /// <summary>
    ///   获取图片矩形区域
    /// </summary>
    function Rect: TIntRect;
    /// <summary>
    ///   通过16进制数据生成图片
    /// </summary>
    function LoadFromHexString(const AHexData: string;
      const AExtension: string = '.png'): Boolean;
    /// <summary>
    ///   Return Texture[0]
    /// </summary>
    property DefaultTexture: TEngineTexture read GetDefaultTexture;
    /// <summary>
    ///   Return a pattern rect
    /// </summary>
    property PatternRect[const Index: Integer]: TIntRect read GetPatternRect;
    /// <summary>
    ///   Return Texture[0].Width
    /// </summary>
    property Width: Integer read GetWidth;
    /// <summary>
    ///   Return Texture[0].Height
    /// </summary>
    property Height: Integer read GetHeight;
  end;

  TEngineCanvasHelper = class helper for TEngineCanvas
    procedure DrawImage(const AImage: TEngineImage; const AQuad: TQuad); overload;
    procedure DrawImagePx(const AImage: TEngineImage;
      const ASourceQuad, ATargetQuad: TQuad; const AColor: TColorRect;
      const ABlendingEffect: TBlendingEffect = TBlendingEffect.Normal);

    procedure DrawImageRegion(const AImage: TEngineImage;
      const ARegionIndex: Integer;
      const ATargetQuad: TQuad;
      const AColor: TIntColor;
      const AMirror, AFlip: Boolean;
      const AAngle: Single;
      const AScale: TPoint2f;
      const ABlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;
    procedure DrawImageRegion(const AImage: TEngineImage;
      const ARegionIndex: Integer;
      const APosX, APosY: Single;
      const AColor: TIntColor;
      const AMirror, AFlip: Boolean;
      const ABlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;
    procedure DrawImageRegion(const AImage: TEngineImage;
      const ARegionIndex: Integer;
      const APosX, APosY: Single;
      const AColor: TIntColor;
      const AMirror, AFlip: Boolean;
      const AAngle: Single;
      const AScale: TPoint2f;
      const ABlendingEffect: TBlendingEffect = TBlendingEffect.Normal); overload;
  end;

  TArchiveHelper = class helper for TArchive
    function LoadImage(const ACanvas: TEngineCanvas; const AKey: string;
      const AMipMapping: Boolean = False;
      const APixelFormat: TPixelFormat = TPixelFormat.Unknown): TEngineImage;
  end;

implementation

{ TQuadHelper }

function TQuadHelper.Overlaps(const AQuad: TQuad): Boolean;
var
  d1, d2, d3, d4: Single;
begin
  d1 := (Self.Values[2].X  - Self.Values[1].X) * (AQuad.Values[0].X - Self.Values[0].X) +
        (Self.Values[2].Y  - Self.Values[1].Y) * (AQuad.Values[0].Y - Self.Values[0].Y);
  d2 := (Self.Values[3].X  - Self.Values[2].X) * (AQuad.Values[0].X - Self.Values[1].X) +
        (Self.Values[3].Y  - Self.Values[2].Y) * (AQuad.Values[0].Y - Self.Values[1].Y);
  d3 := (Self.Values[0].X  - Self.Values[3].X) * (AQuad.Values[0].X - Self.Values[2].X) +
        (Self.Values[0].Y  - Self.Values[3].Y) * (AQuad.Values[0].Y - Self.Values[2].Y);
  d4 := (Self.Values[1].X  - Self.Values[0].X) * (AQuad.Values[0].X - Self.Values[3].X) +
        (Self.Values[1].Y  - Self.Values[0].Y) * (AQuad.Values[0].Y - Self.Values[3].Y);
  if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
    Exit(True);
  //
  d1 := (Self.Values[2].X - Self.Values[1].X) * (AQuad.Values[1].X - Self.Values[0].X) +
        (Self.Values[2].Y - Self.Values[1].Y) * (AQuad.Values[1].Y - Self.Values[0].Y);
  d2 := (Self.Values[3].X - Self.Values[2].X) * (AQuad.Values[1].X - Self.Values[1].X) +
        (Self.Values[3].Y - Self.Values[2].Y) * (AQuad.Values[1].Y - Self.Values[1].Y);
  d3 := (Self.Values[0].X - Self.Values[3].X) * (AQuad.Values[1].X - Self.Values[2].X) +
        (Self.Values[0].Y - Self.Values[3].Y) * (AQuad.Values[1].Y - Self.Values[2].Y);
  d4 := (Self.Values[1].X - Self.Values[0].X) * (AQuad.Values[1].X - Self.Values[3].X) +
        (Self.Values[1].Y - Self.Values[0].Y) * (AQuad.Values[1].Y - Self.Values[3].Y);
  if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
    Exit(True);
  //
  d1 := (Self.Values[2].X - Self.Values[1].X) * (AQuad.Values[2].X - Self.Values[0].X) +
        (Self.Values[2].Y - Self.Values[1].Y) * (AQuad.Values[2].Y - Self.Values[0].Y);
  d2 := (Self.Values[3].X - Self.Values[2].X) * (AQuad.Values[2].X - Self.Values[1].X) +
        (Self.Values[3].Y - Self.Values[2].Y) * (AQuad.Values[2].Y - Self.Values[1].Y);
  d3 := (Self.Values[0].X - Self.Values[3].X) * (AQuad.Values[2].X - Self.Values[2].X) +
        (Self.Values[0].Y - Self.Values[3].Y) * (AQuad.Values[2].Y - Self.Values[2].Y);
  d4 := (Self.Values[1].X - Self.Values[0].X) * (AQuad.Values[2].X - Self.Values[3].X) +
        (Self.Values[1].Y - Self.Values[0].Y) * (AQuad.Values[2].Y - Self.Values[3].Y);
  if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
    Exit(True);
  //
  d1 := (Self.Values[2].X - Self.Values[1].X) * (AQuad.Values[3].X - Self.Values[0].X) +
        (Self.Values[2].Y - Self.Values[1].Y) * (AQuad.Values[3].Y - Self.Values[0].Y);
  d2 := (Self.Values[3].X - Self.Values[2].X) * (AQuad.Values[3].X - Self.Values[1].X) +
        (Self.Values[3].Y - Self.Values[2].Y) * (AQuad.Values[3].Y - Self.Values[1].Y);
  d3 := (Self.Values[0].X - Self.Values[3].X) * (AQuad.Values[3].X - Self.Values[2].X) +
        (Self.Values[0].Y - Self.Values[3].Y) * (AQuad.Values[3].Y - Self.Values[2].Y);
  d4 := (Self.Values[1].X - Self.Values[0].X) * (AQuad.Values[3].X - Self.Values[3].X) +
        (Self.Values[1].Y - Self.Values[0].Y) * (AQuad.Values[3].Y - Self.Values[3].Y);
  if (d1 >= 0) and (d2 >= 0) and (d3 >= 0) and (d4 >= 0) then
    Exit(True);
  //
  Result := False;
end;

procedure TQuadHelper.Trunc(const ATrunc: Boolean);
begin
  if ATrunc then
  begin
    Self.Values[0].X:= System.Trunc(Self.Values[0].X);
    Self.Values[0].Y:= System.Trunc(Self.Values[0].Y);
    Self.Values[1].X:= System.Trunc(Self.Values[1].X);
    Self.Values[1].Y:= System.Trunc(Self.Values[1].Y);
    Self.Values[2].X:= System.Trunc(Self.Values[2].X);
    Self.Values[2].Y:= System.Trunc(Self.Values[2].Y);
    Self.Values[3].X:= System.Trunc(Self.Values[3].X);
    Self.Values[3].Y:= System.Trunc(Self.Values[3].Y);
  end;
end;

{ TEngineImageHelper }

function TEngineImageHelper.Formatting: string;
var
  LRect: TIntRect;
begin
  LRect:= Self.Rect;
  Result:= Format(cChunkRectFormat,[Self.Name, LRect.Left, LRect.Top, LRect.Width,
    LRect.Height]);
end;

function TEngineImageHelper.Rect: TIntRect;
begin
  if not Assigned(Self.DefaultTexture) then
    Exit(ZeroIntRect);
  //
  Result:= IntRect(0, 0, Self.DefaultTexture.Width, Self.DefaultTexture.Height);
end;

function TEngineImageHelper.LoadFromHexString(const AHexData, AExtension: string): Boolean;
var
{$IFDEF NEXTGEN}
  LBuff: TBytes;
{$ENDIF}
  LStream: TMemoryStream;
begin
  LStream := TMemoryStream.Create;
  try
    {$IFDEF NEXTGEN}
    SetLength(LBuff, Length(AHexData) div 2);
    System.Classes.HexToBin(BytesOf(AHexData),0, LBuff, 0, Length(LBuff));
    LStream.Size:= 0;
    LStream.Write(LBuff, Length(LBuff));
    {$ELSE}
    LStream.SetSize(Length(AHexData) div 2);
    LStream.Position:= 0;
    System.Classes.HexToBin(PAnsiChar(AHexData), LStream.Memory, LStream.Size);
    {$ENDIF}
    Self.LoadFromStream(AExtension, LStream); //android下有bug, 会崩溃
  finally
    LStream.Free;
  end;
end;

function TEngineImageHelper.GetDefaultTexture: TEngineTexture;
begin
  if Self.TextureCount = 0 then
    Exit(nil);
  //
  Result:= Self.Texture[0];
end;

function TEngineImageHelper.GetPatternRect(const Index: Integer): TIntRect;
begin
  if (Self.Regions.Count - 1 < Index) or (Index < 0) then
    Exit(Self.Rect);
  //
  Result:= Self.Regions.Items[Index].Rect;
end;

function TEngineImageHelper.GetWidth: Integer;
begin
  if not Assigned(Self.DefaultTexture) then
    Exit(0);
  //
  Result:= Self.DefaultTexture.Width;
end;

function TEngineImageHelper.GetHeight: Integer;
begin
  if not Assigned(Self.DefaultTexture) then
    Exit(0);
  //
  Result:= Self.DefaultTexture.Height;
end;

{ TEngineCanvasHelper }

procedure TEngineCanvasHelper.DrawImage(const AImage: TEngineImage;
  const AQuad: TQuad);
begin
  Self.UseTexturePx(AImage.DefaultTexture, Quad(AImage.Rect));
  Self.TexQuad(AQuad, ColorRectWhite);
end;

procedure TEngineCanvasHelper.DrawImagePx(const AImage: TEngineImage;
  const ASourceQuad, ATargetQuad: TQuad; const AColor: TColorRect;
  const ABlendingEffect: TBlendingEffect);
begin
  Self.UseTexturePx(AImage.DefaultTexture, ASourceQuad);
  Self.TexQuad(ATargetQuad, AColor, ABlendingEffect);
end;

procedure TEngineCanvasHelper.DrawImageRegion(const AImage: TEngineImage;
  const ARegionIndex: Integer; const ATargetQuad: TQuad;
  const AColor: TIntColor; const AMirror, AFlip: Boolean; const AAngle: Single;
  const AScale: TPoint2f; const ABlendingEffect: TBlendingEffect);
var
  LSize: TPoint2i;
  LTargetQuad: TQuad;
begin
  LSize:= AImage.PatternRect[0].Size;
  if AAngle <> 0 then
    LTargetQuad:= ATargetQuad.Rotated(LTargetQuad.TopLeft,
                                      Point2f(LSize.X, LSize.Y),
                                      AAngle, AScale.X)
  else
    LTargetQuad:= ATargetQuad.Scale(AScale.X);
  Self.UseImageRegion(AImage, ARegionIndex, AMirror, AFlip);
  Self.TexQuad(LTargetQuad, AColor, ABlendingEffect);
end;

procedure TEngineCanvasHelper.DrawImageRegion(const AImage: TEngineImage;
  const ARegionIndex: Integer; const APosX, APosY: Single;
  const AColor: TIntColor; const AMirror, AFlip: Boolean;
  const ABlendingEffect: TBlendingEffect);
var
  LSize: TPoint2f;
begin
  LSize.X:= AImage.PatternRect[0].Size.X;
  LSize.Y:= AImage.PatternRect[0].Size.Y;
  LSize:= LSize * TClientUtils.ScreenScale;
  Self.UseImageRegion(AImage, ARegionIndex, AMirror, AFlip);
  Self.TexQuad(Quad(APosX, APosY, LSize.X, LSize.Y), AColor, ABlendingEffect);
end;

procedure TEngineCanvasHelper.DrawImageRegion(const AImage: TEngineImage;
  const ARegionIndex: Integer; const APosX, APosY: Single;
  const AColor: TIntColor; const AMirror, AFlip: Boolean; const AAngle: Single;
  const AScale: TPoint2f; const ABlendingEffect: TBlendingEffect);
var
  LSize: TPoint2f;
  LTargetQuad: TQuad;
begin
  LSize.X:= AImage.PatternRect[0].Size.X;
  LSize.Y:= AImage.PatternRect[0].Size.Y;
  LSize:= LSize * TClientUtils.ScreenScale;
  LTargetQuad:= Quad(APosX, APosY, LSize.X, LSize.Y);
  if AAngle <> 0 then
    LTargetQuad:= LTargetQuad.Rotated(LTargetQuad.TopLeft,
                                      Point2f(LSize.X, LSize.Y),
                                      AAngle, AScale.X)
  else
    LTargetQuad:= LTargetQuad.Scale(AScale.X);
  //
  Self.UseImageRegion(AImage, ARegionIndex, AMirror, AFlip);
  Self.TexQuad(LTargetQuad, AColor, ABlendingEffect);
end;

{ TArchiveHelper }

function ReadPixelsRaw(const AStream: TStream; const ATexture: TCustomLockableTexture): Boolean;
var
  LockedPixels: TLockedPixels;
  ScanlineBytes, I: Integer;
begin
  if not ATexture.Lock(LockedPixels) then
    Exit(False);
  try
    ScanlineBytes := ATexture.Width * SizeOf(TIntColor);

    for I := 0 to ATexture.Height - 1 do
    begin
      Result := AStream.Read(LockedPixels.Scanline[I]^, ScanlineBytes) = ScanlineBytes;
      if not Result then
        Break;
    end;
  finally
    ATexture.Unlock;
  end;
end;

function ReadPixels32toX(const AStream: TStream; const ATexture: TCustomLockableTexture): Boolean;
var
  LockedPixels: TLockedPixels;
  SourcePixels: Pointer;
  SourceSize, I: Integer;
begin
  if not ATexture.Lock(LockedPixels) then
    Exit(False);
  try
    SourceSize := ATexture.Width * SizeOf(TIntColor);
    GetMem(SourcePixels, SourceSize);
    try
      for I := 0 to ATexture.Height - 1 do
      begin
        Result := AStream.Read(SourcePixels^, SourceSize) = SourceSize;
        if not Result then
          Break;

        Pixel32toXArray(SourcePixels, LockedPixels.Scanline[I], ATexture.PixelFormat, ATexture.Width);
      end;
    finally
      FreeMem(SourcePixels);
    end;
  finally
    ATexture.Unlock;
  end;
end;

function ReadPixelsXToX(const AStream: TStream; const ATexture: TCustomLockableTexture;
  const ASourceFormat: TPixelFormat): Boolean;
var
  LockedPixels: TLockedPixels;
  SourcePixels, RawPixels: Pointer;
  SourceSize, RawSize, I: Integer;
begin
  if not ATexture.Lock(LockedPixels) then
    Exit(False);
  try
    SourceSize := ATexture.Width * ASourceFormat.Bytes;
    GetMem(SourcePixels, SourceSize);
    try
      RawSize := ATexture.Width * SizeOf(TIntColor);
      GetMem(RawPixels, RawSize);
      try
        for I := 0 to ATexture.Height - 1 do
        begin
          Result := AStream.Read(SourcePixels^, SourceSize) = SourceSize;
          if not Result then
            Break;

          PixelXto32Array(SourcePixels, RawPixels, ASourceFormat, ATexture.Width);
          Pixel32toXArray(RawPixels, LockedPixels.Scanline[I], ATexture.PixelFormat, ATexture.Width);
        end;
      finally
        FreeMem(RawPixels);
      end;
    finally
      FreeMem(SourcePixels);
    end;
  finally
    ATexture.Unlock;
  end;
end;

function ASDbToPixelFormat(const AFormat: Integer): TPixelFormat;
begin
  if AFormat and $80 > 0 then
    case AFormat and $7F of
      1: Result := TPixelFormat.R8G8B8;
      2: Result := TPixelFormat.A8R8G8B8;
      3: Result := TPixelFormat.X8R8G8B8;
      4: Result := TPixelFormat.R5G6B5;
      5: Result := TPixelFormat.X1R5G5B5;
      6: Result := TPixelFormat.A1R5G5B5;
      7: Result := TPixelFormat.A4R4G4B4;
      8: Result := TPixelFormat.R3G3B2;
      9: Result := TPixelFormat.A8;
      10: Result := TPixelFormat.A8R3G3B2;
      11: Result := TPixelFormat.X4R4G4B4;
      12: Result := TPixelFormat.A2B10G10R10;
      13: Result := TPixelFormat.G16R16;
      14: Result := TPixelFormat.A2R10G10B10;
      15: Result := TPixelFormat.A16B16G16R16;
      16: Result := TPixelFormat.L8;
      17: Result := TPixelFormat.A8L8;
      18: Result := TPixelFormat.A4L4;
      34: Result := TPixelFormat.L16;
      43: Result := TPixelFormat.A8B8G8R8;
      44: Result := TPixelFormat.X8B8G8R8;
    else
      Result := TPixelFormat.Unknown;
    end
  else
    case AFormat of
      0: Result := TPixelFormat.R3G3B2;
      1: Result := TPixelFormat.R5G6B5;
      2: Result := TPixelFormat.X8R8G8B8;
      3: Result := TPixelFormat.X1R5G5B5;
      4: Result := TPixelFormat.X4R4G4B4;
      5: Result := TPixelFormat.A8R8G8B8;
      6: Result := TPixelFormat.A1R5G5B5;
      7: Result := TPixelFormat.A4R4G4B4;
      8: Result := TPixelFormat.A8R3G3B2;
      9: Result := TPixelFormat.A2R2G2B2;
    else
      Result := TPixelFormat.Unknown;
    end;
end;

function LoadImageASDb(const AImage: TEngineImage; const AStream: TStream): Boolean;
var
  PixelFormat: TPixelFormat;
  TextureSize, PatternSize, VisibleSize: TPoint2i;
  TextureCount, PatternCount, I: Integer;
  Texture: TCustomLockableTexture;
begin
  // --> Pixel Format
  PixelFormat := ASDbToPixelFormat(AStream.GetByte);
  // --> Pattern Size
  PatternSize.X := AStream.GetLongInt;
  PatternSize.Y := AStream.GetLongInt;
  // --> Visible Size
  VisibleSize.X := AStream.GetLongInt;
  VisibleSize.Y := AStream.GetLongInt;
  // --> Pattern Count
  PatternCount := AStream.GetLongInt;
  // --> Texture Size
  TextureSize.X := AStream.GetLongInt;
  TextureSize.Y := AStream.GetLongInt;
  // --> Texture Count
  TextureCount := AStream.GetLongInt;

  if AImage.PixelFormat = TPixelFormat.Unknown then
    AImage.PixelFormat := PixelFormat;

  Result := False;

  for I := 0 to TextureCount - 1 do
  begin
    Texture := AImage.InsertTexture(TextureSize.X, TextureSize.Y);
    if Texture = nil then
      Exit(False);

    if Texture.PixelFormat.CanBulkCopyTo(PixelFormat) then
      // Direct copy of pixels (very fast).
      Result := ReadPixelsRaw(AStream, Texture)
    else if PixelFormat = TPixelFormat.A8R8G8B8 then
      // Conversion from 32-bit RGBA to custom pixel format (moderately slow).
      Result := ReadPixels32toX(AStream, Texture)
    else
      // Conversion from one pixel format to another (quite slow).
      Result := ReadPixelsXToX(AStream, Texture, PixelFormat);

    if not Result then
      Exit;
  end;

  AImage.SetupRegionPatterns(PatternSize, VisibleSize, PatternCount);
end;

function PXLAToPixelFormat(const AFormat: Integer): TPixelFormat;
begin
  if AFormat and $80 > 0 then
    Result := TPixelFormat(AFormat and $7F)
  else
    case AFormat of
      1: Result := TPixelFormat.R8G8B8;
      2: Result := TPixelFormat.A8R8G8B8;
      3: Result := TPixelFormat.X8R8G8B8;
      4: Result := TPixelFormat.R5G6B5;
      5: Result := TPixelFormat.X1R5G5B5;
      6: Result := TPixelFormat.A1R5G5B5;
      7: Result := TPixelFormat.A4R4G4B4;
      8: Result := TPixelFormat.R3G3B2;
      9: Result := TPixelFormat.A8;
      10: Result := TPixelFormat.A8R3G3B2;
      11: Result := TPixelFormat.X4R4G4B4;
      12: Result := TPixelFormat.A2B10G10R10;
      13: Result := TPixelFormat.G16R16;
      14: Result := TPixelFormat.A2R10G10B10;
      15: Result := TPixelFormat.A16B16G16R16;
      16: Result := TPixelFormat.L8;
      17: Result := TPixelFormat.A8L8;
      18: Result := TPixelFormat.A4L4;
      19: Result := TPixelFormat.L16;
      20: Result := TPixelFormat.R16F;
      21: Result := TPixelFormat.G16R16F;
      22: Result := TPixelFormat.A16B16G16R16F;
      23: Result := TPixelFormat.R32F;
      24: Result := TPixelFormat.G32R32F;
      25: Result := TPixelFormat.A32B32G32R32F;
      26: Result := TPixelFormat.A8B8G8R8;
      27: Result := TPixelFormat.X8B8G8R8;
      29: Result := TPixelFormat.A2R2G2B2;
    else
      Result := TPixelFormat.Unknown;
    end;
end;

function LoadImagePXLA(const AImage: TEngineImage; const AStream: TStream): Boolean;
var
  PixelFormat: TPixelFormat;
  TextureSize, PatternSize, VisibleSize: TPoint2i;
  TextureCount, PatternCount, I: Integer;
  Texture: TCustomLockableTexture;
begin
  // --> Pixel Format
  PixelFormat := PXLAToPixelFormat(AStream.GetByte);
  // --> Pattern Size
  PatternSize.X := AStream.GetWord;
  PatternSize.Y := AStream.GetWord;
  // --> Pattern Count
  PatternCount := AStream.GetLongInt;
  // --> Visible Size
  VisibleSize.X := AStream.GetWord;
  VisibleSize.Y := AStream.GetWord;
  // --> Texture Size
  TextureSize.X := AStream.GetWord;
  TextureSize.Y := AStream.GetWord;
  // --> Texture Count
  TextureCount := AStream.GetWord;

  if AImage.PixelFormat = TPixelFormat.Unknown then
    AImage.PixelFormat := PixelFormat;

  Result := False;

  for I := 0 to TextureCount - 1 do
  begin
    Texture := AImage.InsertTexture(TextureSize.X, TextureSize.Y);
    if Texture = nil then
      Exit(False);

    if Texture.PixelFormat.CanBulkCopyTo(PixelFormat) then
      // Direct copy of pixels (very fast).
      Result := ReadPixelsRaw(AStream, Texture)
    else if PixelFormat = TPixelFormat.A8R8G8B8 then
      // Conversion from 32-bit RGBA to custom pixel format (moderately slow).
      Result := ReadPixels32toX(AStream, Texture)
    else
      // Conversion from one pixel format to another (quite slow).
      Result := ReadPixelsXToX(AStream, Texture, PixelFormat);

    if not Result then
      Exit;
  end;

  AImage.SetupRegionPatterns(PatternSize, VisibleSize, PatternCount);
end;

function TArchiveHelper.LoadImage(const ACanvas: TEngineCanvas; const AKey: string;
  const AMipMapping: Boolean; const APixelFormat: TPixelFormat): TEngineImage;
var
  LSuccess: Boolean;
  LEntryIndex: Integer;
  LStream: TMemoryStream;
begin
  LSuccess:= False;
  LEntryIndex:= Self.IndexOf(AKey);
  if LEntryIndex = -1 then Exit(nil);
  LStream:= TMemoryStream.Create;
  try
    if not Self.ReadStream(AKey, LStream) then Exit(nil);
    try
      LStream.Position := 0;

      Result:= TEngineImage.Create(ACanvas.Device, False);
      Result.MipMapping:= AMipMapping;
      Result.PixelFormat:= APixelFormat;
      if Self.Entries[LEntryIndex].EntryType = TArchive.TEntryType.Image then
        // The entry is an image in optimal RAW format.
        case Self.Format of
          TArchive.TFormat.VTDb,
          TArchive.TFormat.ASDb: LSuccess:= LoadImageASDb(Result, LStream);
          TArchive.TFormat.ASVF: LSuccess:= LoadImagePXLA(Result, LStream);
        end
      else
        // The entry is just an embedded file.
        LSuccess:= Result.LoadFromStream(ExtractFileExt(AKey), LStream);
      //
      if not LSuccess then
      begin
        FreeAndNil(Result);
        Exit(nil);
      end;
      Result.Name:= AKey;
    except
      FreeAndNil(Result);
      Exit(nil);
    end;
  finally
    LStream.Free;
  end;
end;

end.
