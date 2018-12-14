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

unit se.game.font;

interface

uses
  System.Classes, System.SysUtils, System.Math, System.Generics.Collections,
  PXL.Types, se.utils.client,
  se.game.helper, se.game.types,
  se.game.font.types, se.game.font.classes, se.game.font.utils;

type
  TFontInstance = class;
  TFontGlyphFactory = class;

  TFontGlyph = class
  private
    FCharID: Cardinal;
    FXOffset, FYOffset, FXAdvance: SmallInt;
    FWidth, FHeight: Word;
    FPageIndex, FRegionIndex: Integer;
  protected
    FFont: TFontInstance;
    FKerningCount: Integer;
    FKerningList: array of TFontKerning;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAdvance(const ANextCharID: Cardinal): Integer;

    property CharID: Cardinal read FCharID write FCharID;
    property XOffset: SmallInt read FXOffset write FXOffset;
    property YOffset: SmallInt read FYOffset write FYOffset;
    property XAdvance: SmallInt read FXAdvance write FXAdvance;
    property Width: Word read FWidth write FWidth;
    property Height: Word read FHeight write FHeight;
    property PageIndex: Integer read FPageIndex;
    property RegionIndex: Integer read FRegionIndex;
  end;

  TFontGlyphFactory = class abstract
  protected
    FCanvas: TEngineCanvas;
    FReady: Boolean;
    FNext: TFontGlyphFactory;
    FLocalScale: Single;
  public
    constructor Create(const AScale: Single); virtual; abstract;

    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const ASource: TFileStream); virtual; abstract;

    function InitGlyph(const AFont: TFontInstance; const ACharID: Cardinal;
      const AFontSize: Integer): TFontGlyph; virtual; abstract;
    function GetKerning(const ACurrent, ANext: Cardinal): Integer; virtual; abstract;

    property Canvas: TEngineCanvas read FCanvas write FCanvas;
    property Ready: Boolean read FReady;
  end;

  TFontInstance = class
  public type
    TFontChar = record
      Value: Char;
      X: Single;
      Y: Single;
      Width: Single;
      Height: Single;
      Glyph: TFontGlyph;
      StyleIndex: Cardinal;
      RowIndex: Cardinal;
    public
      function IsNull: Boolean;
      class function Null: TFontChar; static;
    end;

    TFontCharList = record
      List: TArray<TFontChar>;
    private
      function GetCount: Integer;
    public
      procedure Clear;
      function Add(const AValue: Char; var APosX: Single;
        const APosY, AWidth, AHeight: Single; const AGlyph: TFontGlyph;
        const AStyleIndex, ARowCount: Integer; const AMaxPoint: TPoint2f): TPoint2f;
      function Last: TFontChar;
      property Count: Integer read GetCount;
    end;

    TFontCharStyleList = record
      List: TArray<TFontCharStyle>;
    private
      function GetCount: Integer;
    public
      procedure Clear;
      procedure Add(const AStyle: TFontCharStyle);
      function Last: TFontCharStyle;
      function HasBoldStyle: Boolean;
      function PopColor(const ADefault: TIntColor): TIntColor;

      property Count: Integer read GetCount;
    end;

    TFontRow = record
      Width: Single;
      Spaces: Integer;
    public
      class function Null: TFontRow; static;
    end;

    TFontRowList = record
      List: TArray<TFontRow>;
    private
      function GetCount: Integer;
    public
      procedure Clear;
      procedure Add;
      function Last: TFontRow;

      property Count: Integer read GetCount;
    end;

    TFontRenderer = record
      Chars: TFontCharList;
      CharStyles: TFontCharStyleList;
      Rows: TFontRowList;
      MaxPoint: TPoint2f;
    public
      procedure Reset;
    end;
  private
    FGlyphMap: TObjectDictionary<Cardinal, TFontGlyph>;
    FFactory: TFontGlyphFactory;
    FAvgHeight: Single;
    FCache: TEngineImage;
    function GetGlyph(const CharID: Cardinal): TFontGlyph;
    function GetTempTexture: TEngineTexture;
  private
    procedure MakeFontEffect(const ARenderer: TFontRenderer;
      const AIter: TStringIterator; const AStyle: TFontCharStyle;
      var AStyleChanged, AInsidedImage: Boolean; var APosX, APosY: Single);
    procedure CachedGlyphTexture(const ATexture: TEngineTexture;
      var APageIndex, ARegionIndex: Integer);
  public
    constructor Create(const ACanvas: TEngineCanvas);
    destructor Destroy; override;

    function Compile(const APoint: TPoint2f; const AText: string;
      const AFontSize: Word; const AStyle: TFontCharStyle;
      const ARect: TIntRect;
      const AAutoWrap: Boolean = False;
      const AMaxCount: Integer = -1;
      const AHorzAlign: TTextAlignMode = TTextAlignMode.tamLeft;
      const AVertAlign: TTextAlignMode = TTextAlignMode.tamCenter): TFontRenderer;

    function AddGlyphFromImage(const ACharID: Cardinal;
                               const ASource: TEngineTexture;
                               const AXOffset, AYOffset: SmallInt;
                               const AXAdvance: SmallInt = -1): TFontGlyph;

    procedure DrawText(const APoint: TPoint2f; const AText: string;
      const AFontSize: Word; const AStyle: TFontCharStyle;
      const ARect: TIntRect;
      const AAutoWrap: Boolean = False;
      const AMaxCount: Integer = -1;
      const AHorzAlign: TTextAlignMode = TTextAlignMode.tamLeft;
      const AVertAlign: TTextAlignMode = TTextAlignMode.tamCenter); overload;
    procedure DrawText(const ARenderer: TFontRenderer; const AScale: Single); overload;
    //
    property Factory: TFontGlyphFactory read FFactory write FFactory;
    property Glyph[const CharID: Cardinal]: TFontGlyph read GetGlyph;
    property AvgHeight: Single read FAvgHeight write FAvgHeight;
    property Cache: TEngineImage read FCache;
    property TempTexture: TEngineTexture read GetTempTexture;
  end;

implementation

{ TFontGlyph }

constructor TFontGlyph.Create;
begin
  inherited;
  FPageIndex:= 1;
  FRegionIndex:= -1;
end;

destructor TFontGlyph.Destroy;
begin
  inherited;
end;

function TFontGlyph.GetAdvance(const ANextCharID: Cardinal): Integer;
var
  I: Integer;
begin
  Result:= FXAdvance - FXOffset;
  for I:= 0 To Pred(FKerningCount) do
    if (FKerningList[I].NextCharID = ANextCharID) then
    begin
      Inc(Result, FKerningList[I].Amount);
      Exit;
    end;

  if Assigned(FFont.FFactory) then
    Inc(Result, FFont.FFactory.GetKerning(FCharID, ANextCharID));
end;

{ TFontGlyphFactory }

procedure TFontGlyphFactory.LoadFromFile(const AFileName: string);
var
  LSource: TFileStream;
begin
  LSource:= TFileStream.Create(AFileName, 0);
  try
    Self.LoadFromStream(LSource);
  finally
    FreeAndNil(LSource);
  end;
end;

{ TFontInstance.TFontChar }

function TFontInstance.TFontChar.IsNull: Boolean;
begin
  Result:= Self.Value = TFontTypes.NullChar;
end;

class function TFontInstance.TFontChar.Null: TFontChar;
begin
  Result.Value:= TFontTypes.NullChar;
end;

{ TFontInstance.TFontCharList }

function TFontInstance.TFontCharList.Add(const AValue: Char;
  var APosX: Single; const APosY, AWidth, AHeight: Single;
  const AGlyph: TFontGlyph; const AStyleIndex, ARowCount: Integer;
  const AMaxPoint: TPoint2f): TPoint2f;
var
  LCount: Integer;
  LHeight: Single;
begin
  LCount:= Length(Self.List);
  SetLength(Self.List, Succ(LCount));
  //
  Self.List[LCount].Value      := AValue;
  Self.List[LCount].X          := APosX;
  Self.List[LCount].Y          := APosY;
  Self.List[LCount].Width      := AWidth;
  Self.List[LCount].Height     := AHeight;
  Self.List[LCount].Glyph      := AGlyph;
  Self.List[LCount].StyleIndex := AStyleIndex;
  Self.List[LCount].RowIndex   := Pred(ARowCount);
  // 更新X坐标
  APosX:= APosX + AWidth;
  // 更新最大的X和最大的Y
  if APosX > AMaxPoint.X then Result.X:= APosX;
  if TFontUtils.Inc(LHeight, APosY, AHeight) > AMaxPoint.Y then Result.Y:= LHeight;
end;

procedure TFontInstance.TFontCharList.Clear;
begin
  SetLength(Self.List, 0);
end;

function TFontInstance.TFontCharList.GetCount: Integer;
begin
  Result:= Length(Self.List);
end;

function TFontInstance.TFontCharList.Last: TFontChar;
begin
  if Length(Self.List) > 0 then
    Result:= Self.List[High(Self.List)]
  else
    Result:= TFontChar.Null;
end;

{ TFontInstance.TFontCharStyleList }

procedure TFontInstance.TFontCharStyleList.Add(const AStyle: TFontCharStyle);
var
  LCount: Integer;
begin
  LCount:= Length(Self.List);
  SetLength(Self.List, Succ(LCount));
  Self.List[LCount]:= AStyle;
end;

procedure TFontInstance.TFontCharStyleList.Clear;
begin
  SetLength(Self.List, 0);
end;

function TFontInstance.TFontCharStyleList.GetCount: Integer;
begin
  Result:= Length(Self.List);
end;

function TFontInstance.TFontCharStyleList.HasBoldStyle: Boolean;
var
  I: Integer;
begin
  Result:= False;
  for I:= 0 to High(List) do
    if List[I].Bold then
      Exit(True);
end;

function TFontInstance.TFontCharStyleList.Last: TFontCharStyle;
begin
  if Length(Self.List) > 0 then
    Result:= Self.List[High(Self.List)]
  else
    Result:= TFontCharStyle.Default;
end;

function TFontInstance.TFontCharStyleList.PopColor(
  const ADefault: TIntColor): TIntColor;
begin

end;

{ TFontInstance.TFontRow }

class function TFontInstance.TFontRow.Null: TFontRow;
begin
  Result.Width := -1;
  Result.Spaces:= -1;
end;

{ TFontInstance.TFontRowList }

procedure TFontInstance.TFontRowList.Add;
var
  LCount: Integer;
begin
  LCount:= Length(Self.List);
  SetLength(Self.List, Succ(LCount));
  Self.List[LCount].Width := 0;
  Self.List[LCount].Spaces:= 0;
end;

procedure TFontInstance.TFontRowList.Clear;
begin
  SetLength(Self.List, 0);
end;

function TFontInstance.TFontRowList.GetCount: Integer;
begin
  Result:= Length(Self.List);
end;

function TFontInstance.TFontRowList.Last: TFontRow;
begin
  if Length(Self.List) > 0 then
    Result:= Self.List[High(Self.List)]
  else
    Result:= TFontRow.Null;
end;

{ TFontInstance.TFontRenderer }

procedure TFontInstance.TFontRenderer.Reset;
begin
  Self.Chars.Clear;
  Self.CharStyles.Clear;
  Self.Rows.Clear;
  Self.MaxPoint:= ZeroPoint2f;
end;

{ TFontInstance }

constructor TFontInstance.Create(const ACanvas: TEngineCanvas);
begin
  inherited Create;
  FGlyphMap:= TObjectDictionary<Cardinal, TFontGlyph>.Create([doOwnsValues]);
  FCache:= TEngineImage.Create(ACanvas.Device);
  FCache.InsertTexture(TFontTypes.CacheRegionWidth, TFontTypes.CacheRegionHeight);
  FCache.InsertTexture(TFontTypes.CachePageWidth, TFontTypes.CachePageHeight);
end;

destructor TFontInstance.Destroy;
begin
  FreeAndNil(FCache);
  FreeAndNil(FGlyphMap);
  inherited;
end;

function TFontInstance.GetGlyph(const CharID: Cardinal): TFontGlyph;
begin
  if not FGlyphMap.TryGetValue(CharID, Result) then
  begin
    Result:= FFactory.InitGlyph(Self, CharID,
      Trunc(TFontTypes.OptimumFontSize * FFactory.FLocalScale * TFontTypes.FontRescale));
  end;
end;

function TFontInstance.GetTempTexture: TEngineTexture;
begin
  Result:= FCache.DefaultTexture;
  Result.Clear;
  Result.Finalize;
end;

function TFontInstance.AddGlyphFromImage(const ACharID: Cardinal;
  const ASource: TEngineTexture; const AXOffset, AYOffset, AXAdvance: SmallInt): TFontGlyph;
begin
  if not FGlyphMap.TryGetValue(ACharID, Result) then
  begin
    Result:= TFontGlyph.Create;
    Result.CharID  := ACharID;
    Result.FFont   := Self;
    Result.XOffset := AXOffset;
    Result.YOffset := AYOffset;
    Result.Width   := ASource.Width;
    Result.Height  := ASource.Height;
    Result.XAdvance:= IfThen(AXAdvance<=0, ASource.Width, AXAdvance);
    Self.CachedGlyphTexture(ASource, Result.FPageIndex, Result.FRegionIndex);
    FGlyphMap.Add(ACharID, Result);
    Exit;
  end;
  Result.XOffset := AXOffset;
  Result.YOffset := AYOffset;
  Result.XAdvance:= AXAdvance;
end;

procedure TFontInstance.CachedGlyphTexture(const ATexture: TEngineTexture;
  var APageIndex, ARegionIndex: Integer);
var
  LDestPoint: TPoint2i;
  LPageRegionCount, LRowIndex, LColIndex: Integer;
  LSourcePixels, LDestPixels: TEnginePixels;
  LDestBytesPerPixel, I: Integer;
  LDestRect: TIntRect;
begin
  // 计算缓存页索引
  APageIndex:= (FCache.Regions.Count div TFontTypes.CachePageRegions) + 1;

  // 如果页面已满(创建新的页面)
  if FCache.TextureCount < APageIndex + 1 then
  begin
    FCache.InsertTexture(TFontTypes.CachePageWidth, TFontTypes.CachePageHeight);
    LDestPoint:= ZeroPoint2i;
    ARegionIndex:= 0;
    ARegionIndex:= FCache.Regions.Add(0, 0, ATexture.Width, ATexture.Height);
  end else
  begin
    // 计算起点
    LPageRegionCount:= (FCache.Regions.Count + 1) mod TFontTypes.CachePageRegions;
    LRowIndex:= (LPageRegionCount -1) div TFontTypes.CacheRegionRows;
    LColIndex:= (LPageRegionCount -1) mod TFontTypes.CacheRegionCols;
    LDestPoint.X:= LColIndex * TFontTypes.CacheRegionWidth;
    LDestPoint.Y:= LRowIndex * TFontTypes.CacheRegionHeight;
    ARegionIndex:= FCache.Regions.Add(LDestPoint.X, LDestPoint.Y, ATexture.Width, ATexture.Height);
  end;

  // 拷贝像素数据到缓存页
  if not ATexture.Lock(LSourcePixels) then
    Exit;
  try
    LDestRect.Left  := LDestPoint.X;
    LDestRect.Top   := LDestPoint.Y;
    LDestRect.Width := ATexture.Width;
    LDestRect.Height:= ATexture.Height;
    if not FCache.Texture[APageIndex].Lock(LDestRect, LDestPixels) then
      Exit;
    try
      LDestBytesPerPixel:= ATexture.Width * ATexture.BytesPerPixel;
      for I:= 0 to ATexture.Height - 1 do
        Move(LSourcePixels.Scanline[I]^, LDestPixels.Scanline[I]^, LDestBytesPerPixel);
    finally
      FCache.Texture[APageIndex].Unlock;
    end;
  finally
    ATexture.Unlock;
  end;
end;

procedure TFontInstance.MakeFontEffect(const ARenderer: TFontRenderer;
  const AIter: TStringIterator; const AStyle: TFontCharStyle;
  var AStyleChanged, AInsidedImage: Boolean; var APosX, APosY: Single);
var
  LStyleTag, LStyleParam: string;
  LActiveStyle,            // 激活效果, 如<i>text</i> 读取到'/'说明要对text激活斜体效果了
  LIsParamMode: Boolean;   // 带参数, 如<img=64x64>logo.png</img> 指定了要显示logo.png且大小为64x64
  LImageName: string;
  LImageW, LImageH: Single;
  LArgs: TArray<string>;
begin
  if not AStyleChanged then
  begin
    ARenderer.CharStyles.Add(AStyle);
    AStyleChanged:= True;
  end;
  //
  LStyleTag:= '';
  LStyleParam:= '';
  LActiveStyle:= True;
  LIsParamMode:= False;
  //
  while not AIter.Eof do
  begin
    if AIter.CurrChar = TFontTypes.FontEffectEnd then
      Break
    else if (AIter.CurrChar = '/') and (LStyleTag = '') then
      LActiveStyle:= False
    else if (AIter.CurrChar = '=') then
      LIsParamMode:= True
    else if LIsParamMode then
      TFontUtils.AppendChar(LStyleParam, AIter.CurrChar)
    else
      TFontUtils.AppendChar(LStyleTag, AIter.CurrChar);
  end;
  //
  if LStyleTag.Equals('i') then
    ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Italics:= LActiveStyle
  else
  if LStyleTag.Equals('u') then
    ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Underline:= LActiveStyle
  else
  if LStyleTag.Equals('s') then
    ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].StrikeThrough:= LActiveStyle
  else
  if LStyleTag.Equals('b') then
    ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Bold:= LActiveStyle
  else
  if LStyleTag.Equals('w') then
    ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].WavyText:= LActiveStyle
  else
  if LStyleTag.Equals('color') then
  begin
    if LActiveStyle then
      ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Color:= TFontUtils.HexToColor(LStyleTag)
    else
      ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Color:= ARenderer.CharStyles.PopColor(AStyle.Color);
  end
  else
  if LStyleTag.Equals('img') then
  begin
    AInsidedImage:= LActiveStyle;
    if LActiveStyle then
    begin
      LImageName:= '';
      LArgs:= LStyleParam.Split(['x']);
      LImageW:= LArgs[0].ToSingle;
      LImageH:= LArgs[1].ToSingle;
      if LImageW <=0 then LImageW:= 32;
      If LImageH <=0 Then LImageH:= LImageW;
      ARenderer.Chars.Add('!', APosX, APosY, LImageW, LImageH, nil,
        ARenderer.CharStyles.Count-1, ARenderer.Rows.Count, ARenderer.MaxPoint);
    end else
    begin
      ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].ImageName:= LImageName; //?
      AStyleChanged:= False;
    end;
  end else
  if LStyleTag.Equals('url') then
  begin
    ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Link:= LStyleParam;
    if LActiveStyle then
      ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Color:= IntColorRGB(128, 128, 255)
    else
      ARenderer.CharStyles.List[ARenderer.CharStyles.Count-1].Color:= ARenderer.CharStyles.PopColor(AStyle.Color);
  end;
end;

function TFontInstance.Compile(const APoint: TPoint2f; const AText: string;
  const AFontSize: Word; const AStyle: TFontCharStyle; const ARect: TIntRect;
  const AAutoWrap: Boolean; const AMaxCount: Integer;
  const AHorzAlign, AVertAlign: TTextAlignMode): TFontRenderer;
var
  LIter: TStringIterator;
  LGlyph: TFontGlyph;
  LPosX, LPosY, LScale, LTargetWidth, LTargetHeight, LTargetX, LTargetY: Single;
  LStyleChanged, LInsidedImage: Boolean;
  I, J: Integer;
begin
  Result.Chars.Clear;
  Result.CharStyles.Clear;
  Result.Rows.Clear;
  //
  Result.MaxPoint:= ZeroPoint2f;
  LPosX:= APoint.X;
  LPosY:= APoint.Y;
  LScale:= AFontSize / TFontTypes.OptimumFontSize * TClientUtils.ScreenScale;
  LStyleChanged:= False;
  // 增加默认风格
  Result.CharStyles.Add(AStyle);
  // 新增一行
  Result.Rows.Add;
  //
  LIter.Create(AText);
  while not LIter.Eof do
  begin
    // 空字符
    if LIter.CurrChar = TFontTypes.NullChar then
      Continue;
    // 效果(粗体,斜体等以及富文本)
    if LIter.CurrChar = TFontTypes.FontEffectBegin then
    begin
      Self.MakeFontEffect(Result, LIter, Result.CharStyles.Last, LStyleChanged,
        LInsidedImage, LPosX, LPosY);
      Continue;
    end;
    // 换行符
    if LIter.CurrChar = TFontTypes.NewRowChar then
    begin
      LPosX:= APoint.X;
      LPosY:= LPosY + (Self.AvgHeight + TFontTypes.FontPadding * 2) * TFontTypes.FontInvScale *  LScale;
      Result.Rows.Add;
      Continue;
    end;
    LStyleChanged:= False;
    // 自动换行(最大宽度有限制且将超过最大宽度：标点符号不换行, 单词要整体换行, 汉字直接换行)
    if AAutoWrap and (Result.Chars.Count > 0) and (ARect.Width > 0) then
    begin

    end;
    // 常规处理
    LGlyph:= Self.GetGlyph(LIter.CurrCharID);
    if not Assigned(LGlyph) then
      Continue;
    //
    LTargetWidth := LGlyph.GetAdvance(LIter.NextCharID) * TFontTypes.FontInvScale * LScale;
    LTargetHeight:= TFontTypes.FontInvScale * LScale * (LGlyph.Height + TFontTypes.FontPadding);
    // 是否达到字符数量限制(AMaxCount=-1表示字符数不限)
    if (AMaxCount >= 0) and (Result.Chars.Count >= AMaxCount) then
      Break;
    //
    Result.MaxPoint:= Result.Chars.Add(LIter.CurrChar, LPosX, LPosY, LTargetWidth,
      LTargetHeight, LGlyph, Result.CharStyles.Count-1, Result.Rows.Count, Result.MaxPoint);
    //
    Result.Rows.List[Result.Rows.Count-1].Width:= LPosX - APoint.X;
    if Result.Chars.Last.Value = TFontTypes.SpaceChar then
      Inc(Result.Rows.List[Result.Rows.Count-1].Spaces);
  end;

  // 对齐方式处理
  if AHorzAlign <> TTextAlignMode.tamLeft then
  begin
    for I:= 0 to Result.Chars.Count -1 do
    begin
      LTargetWidth:= Result.Rows.List[Result.Chars.List[I].RowIndex].Width;
      case AHorzAlign of
        TTextAlignMode.tamRight:
          begin
            Result.Chars.List[I].X:= (ARect.Width - LTargetWidth) + Result.Chars.List[I].X;
          end;
        TTextAlignMode.tamCenter:
          begin
            Result.Chars.List[I].X:= (ARect.Width - LTargetWidth) * 0.5 + Result.Chars.List[I].X;
          end;
        TTextAlignMode.tamJustify:
          begin
            if Result.Chars.List[I].Value = TFontTypes.SpaceChar then
            begin
              LTargetWidth:= (ARect.Width - LTargetWidth) / Result.Rows.List[Result.Chars.List[I].RowIndex].Spaces;
              for J:= Succ(I) to Result.Chars.Count -1 do
              begin
                if Result.Chars.List[I].RowIndex = Result.Chars.List[J].RowIndex then
                  Result.Chars.List[J].X:= Result.Chars.List[J].X + LTargetWidth;
              end;
            end;
          end;
      end;
    end;
  end;
  //Self.FCache.SaveToFile('c:\ttttt.png', 1);
end;

procedure TFontInstance.DrawText(const APoint: TPoint2f; const AText: string;
  const AFontSize: Word; const AStyle: TFontCharStyle; const ARect: TIntRect;
  const AAutoWrap: Boolean; const AMaxCount: Integer; const AHorzAlign, AVertAlign: TTextAlignMode);
var
  LScale: Single;
  LRenderer: TFontRenderer;
begin
  LScale:= AFontSize / TFontTypes.OptimumFontSize * TClientUtils.ScreenScale;
  LRenderer:= Self.Compile(APoint, AText, AFontSize, AStyle, ARect, AAutoWrap,
    AMaxCount, AHorzAlign, AVertAlign);
  Self.DrawText(LRenderer, LScale);
end;

procedure TFontInstance.DrawText(const ARenderer: TFontRenderer;
  const AScale: Single);
var
  I: Integer;
  LSrc, LTgt: TQuad;
  LChar: TFontChar;
  LTargetX, LTargetY, LWidth, LHeight: Single;
  LTexture: TEngineTexture;
  LRegion: TEngineIntRectList.PItem;
begin
  for I:= 0 to ARenderer.Chars.Count -1 do
  begin
    LChar:= ARenderer.Chars.List[I];
    LTargetX:= LChar.X;
    LTargetY:= ARenderer.MaxPoint.Y + LChar.Glyph.YOffset * TFontTypes.FontInvScale * AScale;
    LWidth := Trunc((LChar.Glyph.Width  - TFontTypes.FontPadding) * AScale);
    LHeight:= Trunc((LChar.Glyph.Height - TFontTypes.FontPadding) * AScale);
    LTexture:= FCache.Texture[LChar.Glyph.FPageIndex];
    LRegion:= FCache.Regions.Items[LChar.Glyph.FRegionIndex];
    if LRegion = nil then Continue;
    //
    LSrc:= Quad(LRegion.Rect.Left,
                LRegion.Rect.Top,
                LChar.Glyph.Width,
                LChar.Glyph.Height);
    LTgt:= Quad(LTargetX,
                LTargetY,
                LWidth  * TFontTypes.FontInvScale,
                LHeight * TFontTypes.FontInvScale);
    FFactory.Canvas.UseTexturePx(LTexture, LSrc);
    FFactory.Canvas.TexQuad(LTgt, ARenderer.CharStyles.List[LChar.StyleIndex].Color);
    //粗体
//    if ARenderer.CharStyles.List[ARenderer.Chars.List[I].StyleIndex].Bold then
//    begin
//      ARenderer.Chars.List[I].X:= ARenderer.Chars.List[I].X - TFontTypes.BoldOffset;
//      ARenderer.Chars.List[I].Y:= ARenderer.Chars.List[I].Y - TFontTypes.BoldOffset * 0.5;
//      ARenderer.Chars.List[I].Width := ARenderer.Chars.List[I].Width  + TFontTypes.BoldOffset * 2;
//      ARenderer.Chars.List[I].Height:= ARenderer.Chars.List[I].Height + TFontTypes.BoldOffset * 2;
//    end;
    //斜体(Quad.Rotated,字符图片中心点旋转)
    //下划线(在字符下方画直线)
    //删除线(在字符垂直中心画直线)
    //表情图标(LTexture=nil,显示图片)
    //波浪效果(字符根据计时器上下浮动)
  end;
end;

end.
