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

unit se.game.font.ttf;

interface

uses
  System.Classes, System.SysUtils, System.Math, PXL.Types,
  se.game.helper, se.game.types,
  se.game.font.types, se.game.font.classes, se.game.font;

type
  TTTFFont = class(TFontGlyphFactory)
  private
    FData: TFontTypes.PByteArray;
    FDataSize: Cardinal;
    FScale: Single;
    FScanline: array Of Byte;
  private
    head: Cardinal;   //字体头: 字体的全局信息
    cmap: Cardinal;   //字符代码到图元的映射: 把字符代码映射为图元索引
    glyf: Cardinal;   //图元数据: 图元轮廓定义以及网格调整指令
    maxp: Cardinal;   //最大需求表: 字体中所需内存分配情况的汇总数据
    loca: Cardinal;   //位置表索引: 把元索引转换为图元的位置
    hhea: Cardinal;   //水平度量头信息
    hmtx: Cardinal;   //水平度量信息: 字体水平布局星系(上高,下高,行间距,最大前进宽度,最小左支撑,最小右支撑)
    kern: Cardinal;   //字距调整表: 字距调整对的数组
  private
    index_map: Integer;       // a cmap mapping for our chosen character encoding
    indexToLocFormat: Integer;// format needed to map from glyph index to glyph
    glyphCount:Integer;       // number of glyphs, needed for range checking
  private
    function ttBYTE(const offset: Cardinal): Byte;
    function ttUSHORT(const offset: Cardinal): Word;
    function ttSHORT(const offset: Cardinal): Smallint;
    function ttULONG(const offset: Cardinal): Cardinal;
    function ttLONG(const offset: Cardinal): Integer;
  private
    function stbtt__find_table(const ATableTag: TFontTypes.TFileHeader): Cardinal;
    function stbtt_tag(const offset: Cardinal; const ATableTag: TFontTypes.TFileHeader): Boolean;

    function new_active(const e: TStBttEdge; off_x: Integer;
      start_point: Single): TStBttActiveEdge;
  private
    function stbtt_FindGlyphIndex(const unicode_codepoint: Integer): Word;
    function stbtt_GetGlyphBitmap(scale_x, scale_y, shift_x, shift_y: Single;
      const glyph: Integer; const ATexture: TEngineTexture; var xoff, yoff: Integer): Boolean;
    procedure stbtt_GetGlyphShape(const glyph_index: Integer; out Result: TStBttVertexArray);
    procedure stbtt_GetGlyphBitmapBox(const glyph: Integer; scale_x, scale_y, shift_x, shift_y: Single;
      var ix0, iy0, ix1, iy1: Integer);
    procedure stbtt_Rasterize(const resultTexture: TEngineTexture;
      const FlatnessInPixels: Single; var vertices: TStBttVertexArray;
      const ScaleX, ScaleY, ShiftX, ShiftY: Single; xoff, yoff, Invert: Integer);
    function stbtt_GetGlyphBox(const glyph_index: Integer;
      var x0, y0, x1, y1: Integer): Integer;
    function stbtt__GetGlyfOffset(const glyph_index: Integer): Integer;
    procedure stbtt_GetCodepointHMetrics(const codepoint: Integer;
      var advanceWidth, leftSideBearing: Integer);
    procedure stbtt_GetGlyphHMetrics(const GlyphIndex: Integer;
      var advanceWidth, leftSideBearing: Integer);
    function stbtt_GetCodepointKernAdvance(const ch1, ch2: Integer): Integer;
    function stbtt_GetGlyphKernAdvance(const glyph1, glyph2: Integer): Integer;
    procedure stbtt_FlattenCurves(var vertices: TStBttVertexArray;
      ObjSpaceFlatness: Single; out Contours: TContourArray; out Windings: TStBttPointArray);
    procedure stbtt__rasterize(const resultTexture: TEngineTexture;
      var Pts: TStBttPointArray; var Windings: TContourArray;
      const ScaleX, ScaleY, ShiftX, ShiftY: Single; xoff, yoff, Invert: Integer);
    procedure stbtt_setvertex(var v: TStBttVertex; typeByte: Byte;
      x, y, cx, cy: Smallint);
    procedure stbtt__rasterize_sorted_edges(resultTexture: TEngineTexture;
      e: TEdgeList; vsubsample, off_x, off_y: Integer);
    procedure stbtt__fill_active_edges(scanline: PByteArray; len: Integer;
      e: TStBttActiveEdge; max_weight: Integer);
    procedure stbtt__add_point(var points: TStBttPointArray; n: Integer;
      x, y: Single);
    function stbtt__tesselate_curve(var points: TStBttPointArray;
      var num_points: Integer; x0, y0, x1, y1, x2, y2, objspace_flatness_squared
      : Single; n: Integer): Integer;
  private
    procedure AddLog(const AType: Integer; const AClassName, ALog: string);
  public
    constructor Create(const AScale: Single); override;
    destructor Destroy; override;
  public
    function HasGlyph(const AID: Cardinal): Boolean;
    function ScaleForPixelHeight(const AHeight: Single): Single;
    function GetCodepointBitmap(ScaleX, ScaleY: Single; const codepoint, glyphindex: Integer;
      const ATexture: TEngineTexture; var xoff, yoff: Integer): Boolean;
  public
    procedure LoadFromStream(const ASource: TFileStream); override;
    function InitGlyph(const AFont: TFontInstance; const ACharID: Cardinal;
      const AFontSize: Integer): TFontGlyph; override;
    function GetKerning(const ACurrent, ANext: Cardinal): Integer; override;
  end;

implementation

{ TTTFFont }

procedure TTTFFont.AddLog(const AType: Integer; const AClassName, ALog: string);
begin

end;

constructor TTTFFont.Create(const AScale: Single);
begin
  inherited;
  FData:= nil;
  FLocalScale:= AScale;
end;

destructor TTTFFont.Destroy;
begin
  if Assigned(FData) then
  begin
    FreeMem(FData);
    FData:= nil;
  end;
  inherited;
end;

function TTTFFont.ttBYTE(const offset: Cardinal): Byte;
begin
  if offset >= FDataSize then
    Result:= 0
  else
    Result:= FData[offset];
end;

function TTTFFont.ttLONG(const offset: Cardinal): Integer;
begin
  if offset >= FDataSize then
    Result:= 0
  else
    Result:= TFontTypes.PtrUInt(FData[offset] shl 24) +
             TFontTypes.PtrUInt(FData[offset+1] shl 16) +
             TFontTypes.PtrUInt(FData[offset+2] shl 8) +
             TFontTypes.PtrUInt(FData[offset+3]);
end;

function TTTFFont.ttSHORT(const offset: Cardinal): Smallint;
begin
  if offset >= FDataSize then
    Result:= 0
  else
    Result:= (SmallInt(FData[offset]) shl 8) + SmallInt(FData[offset+1]);
end;

function TTTFFont.ttULONG(const offset: Cardinal): Cardinal;
begin
  if offset >= FDataSize then
    Result:= 0
  else
    Result:= (TFontTypes.PtrUInt(FData[offset]) shl 24) +
             (TFontTypes.PtrUInt(FData[offset+1]) shl 16) +
             (TFontTypes.PtrUInt(FData[offset+2]) shl 8) + Cardinal(FData[offset+3]);
end;

function TTTFFont.ttUSHORT(const offset: Cardinal): Word;
begin
  if offset >= FDataSize then
    Result:= 0
  else
    Result:= (FData[offset] shl 8) + FData[offset+1];
end;

function TTTFFont.stbtt_tag(const offset: Cardinal;
  const ATableTag: TFontTypes.TFileHeader): Boolean;
begin
  if offset >= FDataSize then
    Result:= False
  else
    Result:= (FData[offset]   = Byte(ATableTag[1])) and
             (FData[offset+1] = Byte(ATableTag[2])) and
             (FData[offset+2] = Byte(ATableTag[3])) and
             (FData[offset+3] = Byte(ATableTag[4]));
end;

procedure TTTFFont.stbtt__add_point(var points: TStBttPointArray; n: Integer; x,
  y: Single);
begin
  if points.Count <= 0 then
    Exit; // during first pass, it's unallocated

  points.List[n].x := x;
  points.List[n].y := y;
end;

procedure TTTFFont.stbtt__fill_active_edges(scanline: PByteArray; len: Integer;
  e: TStBttActiveEdge; max_weight: Integer);
var
  x0, x1, w: Integer;
  i, j: Integer;
begin
  // non-zero winding fill
  x0 := 0;
  w := 0;

  while Assigned(e) do
  begin
    if w = 0 then
    begin
      // if we're currently at zero, we need to record the edge start point
      x0 := e.x;
      Inc(w, e.valid);
    end
    else
    begin
      x1 := e.x;
      Inc(w, e.valid);
      // if we went to zero, we need to draw
      if w = 0 then
      begin
        i := x0 shr TFontTypes.FIXSHIFT;
        j := x1 shr TFontTypes.FIXSHIFT;

        if (i < len) and (j >= 0) then
        begin
          if i = j then
          begin
            // x0,x1 are the same pixel, so compute combined coverage
            scanline[i] := scanline[i] +
              Byte(((x1 - x0) * max_weight) shr TFontTypes.FIXSHIFT);
          end
          else
          begin
            if i >= 0 then // add antialiasing for x0
              scanline[i] := scanline[i] +
                Byte(((TFontTypes.FIX - (x0 and TFontTypes.FIXMASK)) * max_weight) shr TFontTypes.FIXSHIFT)
            else
              i := -1; // clip

            if (j < len) then // add antialiasing for x1
              scanline[j] := scanline[j] +
                Byte(((x1 and TFontTypes.FIXMASK) * max_weight) shr TFontTypes.FIXSHIFT)
            else
              j := len; // clip

            Inc(i);
            while i < j do // fill pixels between x0 and x1
            begin
              scanline[i] := scanline[i] + Byte(max_weight);
              Inc(i);
            end;
          end;
        end;
      end;
    end;
    e:= e.next;
  end;
end;

function TTTFFont.stbtt__find_table(const ATableTag: TFontTypes.TFileHeader): Cardinal;
var
  num_tables: Integer;
  tabledir: Cardinal;
  i: Cardinal;
  loc: Cardinal;
begin
  num_tables:= ttUSHORT(4);

  if (num_tables<=0) Then
  begin
    Result:= 0;
    Exit;
  end;

  tabledir:= 12;

  for i:= 0 to num_tables -1 do
  begin
    loc:= tabledir + 16*i;
    if stbtt_tag(loc, ATableTag) then
    begin
      Result:= ttULONG(loc+8);
      Break;
    end;
  end;
end;

function TTTFFont.stbtt__GetGlyfOffset(const glyph_index: Integer): Integer;
var
  g1, g2: Integer;
begin
  if glyph_index >= glyphCount then
  begin
    Result := -1; // glyph index out of range
    Exit;
  end;

  if indexToLocFormat >= 2 then
  begin
    Result := -1; // unknown index->glyph map format
    Exit;
  end;

  if indexToLocFormat = 0 then
  begin
    g1 := glyf + ttUSHORT(loca + glyph_index * 2) * 2;
    g2 := glyf + ttUSHORT(loca + glyph_index * 2 + 2) * 2;
  end
  else
  begin
    g1 := glyf + ttULONG(loca + glyph_index * 4);
    g2 := glyf + ttULONG(loca + glyph_index * 4 + 4);
  end;

  if g1 = g2 then
    Result := -1
  else
    Result := g1; // if length is 0, return -1
end;

procedure TTTFFont.stbtt__rasterize(const resultTexture: TEngineTexture;
  var Pts: TStBttPointArray; var Windings: TContourArray; const ScaleX, ScaleY,
  ShiftX, ShiftY: Single; xoff, yoff, Invert: Integer);
var
  YScaleInv: Single;
  i, j, k, m, a, b, vsubsample: Integer;
  e: TEdgeList;
  en: TStBttEdge;
  ptOfs: Cardinal;
  function p(Index: Integer): TStBttPoint;
  begin
    Result:= Pts.List[ptOfs + index];
  end;
begin
  e:= TEdgeList.Create();
  try
    if Invert <> 0 then
      YScaleInv:= -ScaleY
    else
      YScaleInv:= ScaleY;

    if resultTexture.height < 8 then
      vsubsample:= 15
    else
      vsubsample:= 5;
    // vsubsample should divide 255 evenly; otherwise we won't reach full opacity

    m:= 0;
    for i:= 0 to Pred(Windings.Count) do
    begin
      ptOfs:= m;

      Inc(m, Windings.List[i]);
      j:= Windings.List[i] - 1;
      k:= 0;
      while k < Windings.List[i] do
      begin
        a:= k;
        b:= j;
        // skip the edge if horizontal
        if p(j).y <> p(k).y then
        begin
          // add edge from j to k to the list
          en.Invert := 0;

          if ((Invert <> 0) and (p(j).y > p(k).y)) or
            ((Invert = 0) and (p(j).y < p(k).y)) then
          begin
            en.Invert := 1;
            a := j;
            b := k;
          end;

          en.x0 := p(a).x * ScaleX + ShiftX;
          en.y0 := p(a).y * YScaleInv * vsubsample + ShiftY;
          en.x1 := p(b).x * ScaleX + ShiftX;
          en.y1 := p(b).y * YScaleInv * vsubsample + ShiftY;

          e.Add(en);
        end;

        j := k;
        Inc(k);
      end;
    end;

    SetLength(Pts.List, 0);
    Pts.Count := 0;

    SetLength(Windings.List, 0);
    Windings.Count := 0;

    // now sort the edges by their highest point (should snap to integer, and then by x)
    e.Sort();

    FillChar(en, SizeOf(TStBttEdge), 0);
    en.y0 := 10000000;
    e.Add(en);

    // now, traverse the scanlines and find the intersections on each scanline, use xor winding rule
    stbtt__rasterize_sorted_edges(resultTexture, e, vsubsample, xoff, yoff);
  finally
    FreeAndNil(e);
  end;
end;

procedure TTTFFont.stbtt__rasterize_sorted_edges(resultTexture: TEngineTexture;
  e: TEdgeList; vsubsample, off_x, off_y: Integer);
var
  active: TStBttActiveEdge;
  y, j, s, iii: Integer;
  max_weight: Integer;
  scan_y: Single;
  Temp, Prev: TStBttActiveEdge;
  p, z: TStBttActiveEdge;
  changed: Boolean;
  t, q: TStBttActiveEdge;
  eIndex: Integer;

  n, cnt, ofs: Integer;

  Color: TIntColor;

  pixels: TEnginePixels;
begin
  Color:= IntColorWhite;

  eIndex:= 0;
  n:= e.Count - 1;

  active:= nil;
  j:= 0;
  max_weight:= 255 div vsubsample; // weight per vertical scanline

  y:= off_y * vsubsample;

  e.FIX(n, (off_y + resultTexture.height) * vsubsample + 1);

  if (Length(FScanline) <= resultTexture.Width) then
    SetLength(FScanline, Succ(resultTexture.Width));
  //
  resultTexture.Lock(pixels);
  try
    while (j < resultTexture.height) do
    begin
      for iii := 0 to resultTexture.Width do
        FScanline[iii] := 0;

      for s := 0 to vsubsample - 1 do
      begin
        // find center of pixel for this scanline
        scan_y := y + 0.5;

        // update all active edges;
        // remove all active edges that terminate before the center of this scanline
        Temp := active;
        Prev := nil;
        while Assigned(Temp) do
        begin
          if (Temp.ey <= scan_y) then
          begin
            // delete from list
            if Assigned(Prev) then
              Prev.next := Temp.next
            else
              active := Temp.next;

            z := Temp;
            freeandnil(z);

            Temp := Temp.next;
          end
          else
          begin
            Inc(Temp.x, Temp.dx); // advance to position for current scanline

            Prev := Temp;
            Temp := Temp.next; // advance through list
          end;
        end;

        // resort the list if needed
        repeat
          changed := False;
          Temp := active;
          while (Assigned(Temp)) and (Assigned(Temp.next)) do
          begin
            if Temp.x > Temp.next.x then
            begin
              t := Temp;
              q := t.next;

              t.next := q.next;
              q.next := t;
              Temp := q;

              changed := True;
            end;

            Temp := Temp.next;
          end;

        until (not changed);

        // insert all edges that start before the center of this scanline -- omit ones that also end on this scanline
        while (e.Get(eIndex).y0 <= scan_y) do
        begin
          if (e.Get(eIndex).y1 > scan_y) then
          begin
            z := new_active(e.Get(eIndex), off_x, scan_y);
            // find insertion point
            if active = nil then
              active := z
            else if (z.x < active.x) then // insert at front
            begin
              z.next := active;
              active := z;
            end
            else
            begin
              // find thing to insert AFTER
              p := active;
              while (Assigned(p.next)) and (p.next.x < z.x) do
                p := p.next;

              // at this point, p->next->x is NOT < z->x
              z.next := p.next;
              p.next := z;
            end;
          end;

          Inc(eIndex);
        end;

        // now process all active edges in XOR fashion
        if Assigned(active) then
          stbtt__fill_active_edges(@FScanline[0], resultTexture.Width, active,
            max_weight);

        Inc(y);
      end;

      for iii:= 0 to Pred(resultTexture.Width) do
        if (FScanline[iii] > 0) then // OPTIMIZATION?
        begin
          TIntColorRec(Color).Alpha:= FScanline[iii];
          pixels.Pixels[iii, j]:= Color;
        end;

      Inc(j);
    end;
  finally
    resultTexture.Unlock;
  end;

  while Assigned(active) do
  begin
    z := active;
    active := active.next;
    freeandnil(z);
  end;
end;

function TTTFFont.stbtt__tesselate_curve(var points: TStBttPointArray;
  var num_points: Integer; x0, y0, x1, y1, x2, y2,
  objspace_flatness_squared: Single; n: Integer): Integer;
var
  mx, my, dx, dy: Single;
begin
  // midpoint
  mx := (x0 + 2 * x1 + x2) / 4;
  my := (y0 + 2 * y1 + y2) / 4;
  // versus directly drawn line
  dx := (x0 + x2) / 2 - mx;
  dy := (y0 + y2) / 2 - my;
  if n > 16 then // 65536 segments on one curve better be enough!
  begin
    Result := 1;
    Exit;
  end;

  if dx * dx + dy * dy > objspace_flatness_squared then
  // half-pixel error allowed... need to be smaller if AA
  begin
    stbtt__tesselate_curve(points, num_points, x0, y0, (x0 + x1) / 2.0,
      (y0 + y1) / 2.0, mx, my, objspace_flatness_squared, n + 1);
    stbtt__tesselate_curve(points, num_points, mx, my, (x1 + x2) / 2.0,
      (y1 + y2) / 2.0, x2, y2, objspace_flatness_squared, n + 1);
  end
  else
  begin
    stbtt__add_point(points, num_points, x2, y2);
    Inc(num_points);
  end;
  Result := 1;
end;

procedure TTTFFont.LoadFromStream(const ASource: TFileStream);
var
  encoding_record: Cardinal;
  i, numTables: Cardinal;
begin
  FData:= nil;
  FDataSize:= 0;
  FReady:= False;

  if (ASource = nil) then
  begin
    Self.AddLog(1, 'TTTFFont', 'Null stream!');
    Exit;
  end;

  // read file
  FDataSize:= ASource.Size;
  GetMem(FData, FDataSize);
  ASource.ReadData(@FData[0], FDataSize);

  cmap:= stbtt__find_table('cmap');
  loca:= stbtt__find_table('loca');
  head:= stbtt__find_table('head');
  glyf:= stbtt__find_table('glyf');
  hhea:= stbtt__find_table('hhea');
  hmtx:= stbtt__find_table('hmtx');
  kern:= stbtt__find_table('kern');

  if (cmap=0) or (loca=0) or (head=0) or (glyf=0) or (hhea=0) or (hmtx=0) then
  begin
    Self.AddLog(1, 'TTTFFont', 'Invalid font file: '+ ASource.FileName);
    Exit;
  end;

  maxp:= stbtt__find_table('maxp');
  if maxp <> 0 then
    glyphCount:= ttUSHORT(maxp+4)
  else
    glyphCount:= -1;

  // find a cmap encoding table we understand *now* to avoid searching
  // later. (todo: could make this installable)
  // the same regardless of glyph.
  numTables:= Integer(ttUSHORT( cmap + 2));
  index_map:= 0;

  for i:= 0 to numTables -1 do
  begin
    encoding_record:= cmap + 4 + 8 * i;
    // find an encoding we understand:
    case ttUSHORT(encoding_record) of
      TFontTypes.STBTT_PLATFORM_ID_MICROSOFT:
        case ttUSHORT(encoding_record+2) of
          TFontTypes.STBTT_MS_EID_UNICODE_BMP,
          TFontTypes.STBTT_MS_EID_UNICODE_FULL:
            index_map:= cmap + ttULONG(encoding_record+4);  // MS/Unicode
        end;
    end;
  end;

  if index_map = 0 then
  begin
    Self.AddLog(1, 'TTTFFont', 'Could not find font index map: '+ ASource.FileName);
    Exit;
  end;

  indexToLocFormat:= ttUSHORT(head + 50);
  FReady:= True;
end;

function TTTFFont.new_active(const e: TStBttEdge; off_x: Integer;
  start_point: Single): TStBttActiveEdge;
var
  z: TStBttActiveEdge;
  dxdy: Single;
begin
  z := TStBttActiveEdge.Create();

  dxdy := (e.x1 - e.x0) / (e.y1 - e.y0);
  // STBTT_assert(e->y0 <= start_point);

  // round dx down to avoid going too far
  if dxdy < 0 then
    z.dx := -Floor(TFontTypes.FIX * -dxdy)
  else
    z.dx := Floor(TFontTypes.FIX * dxdy);

  z.x := Floor(TFontTypes.FIX * (e.x0 + dxdy * (start_point - e.y0)));
  Dec(z.x, off_x * TFontTypes.FIX);
  z.ey := e.y1;
  z.next := nil;

  if e.Invert <> 0 then
    z.valid := 1
  else
    z.valid := -1;

  Result := z;
end;

function TTTFFont.stbtt_FindGlyphIndex(const unicode_codepoint: Integer): Word;
var
  formatType: Smallint;
  bytes: Integer;
  first, count: Cardinal;
  segcount, searchRange, entrySelector, rangeShift: Word;
  item, offset, startValue, endValue: Word;
  startValue2, endValue2: Word;
  endCount: Cardinal;
  search: Cardinal;
  ngroups: Word;
  low, high: Integer;
  g: Word;
  n, mid: Integer;
  start_char: Cardinal;
  end_char: Cardinal;
  start_glyph: Cardinal;
begin
  formatType:= ttUSHORT(index_map);
  case formatType of
    0: // apple byte encoding
      begin
        bytes:= Integer(ttUSHORT(index_map + 2));
        if unicode_codepoint < bytes - 6 then
        begin
          Result:= Integer(FData[index_map + 6 + unicode_codepoint]);
          Exit;
        end;
        Result:= 0;
      end;
    6:
      begin
        first:= Cardinal(ttUSHORT(index_map + 6));
        count:= Cardinal(ttUSHORT(index_map + 8));
        if (Cardinal(unicode_codepoint) >= first) and
          (unicode_codepoint < first + count) then
        begin
          Result:= Integer(ttUSHORT(TFontTypes.PtrUInt(index_map) + 10 +
            (unicode_codepoint - first) * 2));
          Exit;
        end;
        Result:= 0;
      end;
    2:
      begin
        // STBTT_assert(0); // TODO: high-byte mapping for japanese/chinese/korean
        Result:= 0;
        Exit;
      end;
    4: // standard mapping for windows fonts: binary search collection of ranges
      begin
        segcount:= ttUSHORT(index_map + 6) shr 1;
        searchRange:= ttUSHORT(index_map + 8) shr 1;
        entrySelector:= ttUSHORT(index_map + 10);
        rangeShift:= ttUSHORT(index_map + 12) shr 1;

        // do a binary search of the segments
        endCount:= index_map + 14;
        search:= endCount;

        if unicode_codepoint > $FFFF then
        begin
          Result:= 0;
          Exit;
        end;

        // they lie from endCount .. endCount + segCount
        // but searchRange is the nearest power of two, so...
        if unicode_codepoint >= Integer(ttUSHORT(search + rangeShift * 2)) then
          Inc(search, rangeShift * 2);

        // now decrement to bias correctly to find smallest
        Dec(search, 2);
        while entrySelector <> 0 do
        begin
          // stbtt_uint16 start, end;
          searchRange:= searchRange shr 1;
          startValue2:= ttUSHORT(search + 2 + segcount * 2 + 2);
          endValue2:= ttUSHORT(search + 2);
          startValue2:= ttUSHORT(search + searchRange * 2 + segcount * 2 + 2);
          endValue2:= ttUSHORT(search + searchRange * 2);

          if unicode_codepoint > endValue2 then
            Inc(search, searchRange * 2);
          Dec(entrySelector);
        end;
        Inc(search, 2);

        item:= Word((search - endCount) shr 1);

        // STBTT_assert(unicode_codepoint <= ttUSHORT(data + endCount + 2*item));
        startValue:= ttUSHORT(index_map + 14 + segcount * 2 + 2 + 2 * item);
        endValue:= ttUSHORT(index_map + 14 + 2 + 2 * item);
        if unicode_codepoint < startValue then
        begin
          // IntToString(unicode_codepoint); //BOO
          Result:= 0;
          Exit;
        end;

        offset:= Integer(ttUSHORT(index_map + 14 + segcount * 6 + 2 + 2 * item));
        if offset = 0 then
        begin
          n:= ttSHORT(index_map + 14 + segcount * 4 + 2 + 2 * item);
          Result:= unicode_codepoint + n;
          Exit;
        end;

        Result:= ttUSHORT(offset + (unicode_codepoint - startValue) * 2 +
          index_map + 14 + segcount * 6 + 2 + 2 * item);
      end;
    12:
      begin
        ngroups:= ttUSHORT(index_map + 6);
        g:= 0;
        low:= 0;
        high:= Integer(ngroups);
        // Binary search the right group.
        while low <= high do
        begin
          mid:= low + ((high - low) shr 1);
          // rounds down, so low <= mid < high
          start_char:= ttULONG(index_map + 16 + mid * 12);
          end_char:= ttULONG(index_map + 16 + mid * 12 + 4);
          if unicode_codepoint < start_char then
            high:= mid - 1
          else if unicode_codepoint > end_char then
            low:= mid + 1
          else
          begin
            start_glyph:= ttULONG(index_map + 16 + mid * 12 + 8);
            Result:= start_glyph + unicode_codepoint - start_char;
            Exit;
          end;
        end;
        Result:= 0; // not found
      end;
  else
    begin
      // TODO
      Result := 0;
    end;
  end;
end;

procedure TTTFFont.stbtt_FlattenCurves(var vertices: TStBttVertexArray;
  ObjSpaceFlatness: Single; out Contours: TContourArray;
  out Windings: TStBttPointArray);
var
  NumPoints: Integer;
  objspace_flatness_squared: Single;
  i, n, start, pass: Integer;
  x, y: Single;
begin
  Windings.Count := 0;
  Windings.List := nil;
  NumPoints := 0;

  objspace_flatness_squared := Sqr(ObjSpaceFlatness);
  n := 0;
  start := 0;

  // count how many "moves" there are to get the contour count
  for i := 0 to vertices.Count - 1 do
    If vertices.List[i].vertexType = TFontTypes.STBTT_vmove then
      Inc(n);

  Contours.List := nil;
  Contours.Count := n;
  if n = 0 then
    Exit;

  SetLength(Contours.List, n);

  // make two passes through the points so we don't need to realloc
  for pass := 0 to 1 do
  begin
    x := 0;
    y := 0;
    if (pass = 1) then
    begin
      Windings.Count := NumPoints * 2;
      SetLength(Windings.List, Windings.Count);
    end;

    NumPoints := 0;
    n := -1;

    for i := 0 to vertices.Count - 1 do
    begin
      case vertices.List[i].vertexType of
        TFontTypes.STBTT_vmove:
          begin
            // start the next contour
            if n >= 0 then
              Contours.List[n] := NumPoints - start;
            Inc(n);
            start := NumPoints;

            x := vertices.List[i].x;
            y := vertices.List[i].y;
            stbtt__add_point(Windings, NumPoints, x, y);
            Inc(NumPoints);
          end;

        TFontTypes.STBTT_vline:
          begin
            x := vertices.List[i].x;
            y := vertices.List[i].y;
            stbtt__add_point(Windings, NumPoints, x, y);
            Inc(NumPoints);
          end;

        TFontTypes.STBTT_vcurve:
          begin
            stbtt__tesselate_curve(Windings, NumPoints, x, y,
              vertices.List[i].cx, vertices.List[i].cy, vertices.List[i].x,
              vertices.List[i].y, objspace_flatness_squared, 0);
            x := vertices.List[i].x;
            y := vertices.List[i].y;
          end;
      end;
    end;
    Contours.List[n] := NumPoints - start;
  end;
end;

procedure TTTFFont.stbtt_GetCodepointHMetrics(const codepoint: Integer;
  var advanceWidth, leftSideBearing: Integer);
begin
  stbtt_GetGlyphHMetrics(stbtt_FindGlyphIndex(codepoint), advanceWidth,
    leftSideBearing);
end;

function TTTFFont.stbtt_GetCodepointKernAdvance(const ch1,ch2: Integer): Integer;
begin
  if (kern <= 0) Then
    Result:= 0
  else
    Result:= stbtt_GetGlyphKernAdvance(stbtt_FindGlyphIndex(ch1),
      stbtt_FindGlyphIndex(ch2));
end;

function TTTFFont.stbtt_GetGlyphBitmap(scale_x, scale_y, shift_x, shift_y: Single;
  const glyph: Integer; const ATexture: TEngineTexture; var xoff, yoff: Integer): Boolean;
var
  ix0, iy0, ix1, iy1: Integer;
  w, h: Integer;
  vertices: TStBttVertexArray;
begin
  stbtt_GetGlyphShape(glyph, vertices);
  if scale_x = 0 then scale_x:= scale_y;
  if scale_y = 0 then
  begin
    if scale_x = 0 then Exit(False);
    scale_y:= scale_x;
  end;
  //
  stbtt_GetGlyphBitmapBox(glyph, scale_x, scale_y, shift_x, shift_y, ix0, iy0, ix1, iy1);
  w:= ix1 - ix0;
  h:= iy1 - iy0;

  if (w <= 0) or (h <= 0) then Exit(False);

  // now we get the size
  ATexture.Width:= w;
  ATexture.Height:= h;
  ATexture.Initialize;
  xoff:= ix0;
  yoff:= iy0;
  stbtt_Rasterize(ATexture, 0.35, vertices, scale_x, scale_y, shift_x, shift_y, ix0, iy0, 1);
  Result:= True;
end;

procedure TTTFFont.stbtt_GetGlyphBitmapBox(const glyph: Integer; scale_x,
  scale_y, shift_x, shift_y: Single; var ix0, iy0, ix1, iy1: Integer);
var
  x0, y0, x1, y1: Integer;
begin
  if stbtt_GetGlyphBox(glyph, x0, y0, x1, y1) = 0 then
  begin
    x0 := 0;
    y0 := 0;
    x1 := 0;
    y1 := 0; // e.g. space character
  end;

  // now move to integral bboxes (treating pixels as little squares, what pixels get touched)?
  ix0 := Floor(x0 * scale_x + shift_x);
  iy0 := -Ceil(y1 * scale_y + shift_y);
  ix1 := Ceil(x1 * scale_x + shift_x);
  iy1 := -Floor(y0 * scale_y + shift_y);
end;

function TTTFFont.stbtt_GetGlyphBox(const glyph_index: Integer; var x0, y0, x1,
  y1: Integer): Integer;
var
  g: Integer;
begin
  g:= stbtt__GetGlyfOffset(glyph_index);
  if g < 0 then Exit(0);
  //
  x0:= ttSHORT(g + 2);
  y0:= ttSHORT(g + 4);
  x1:= ttSHORT(g + 6);
  y1:= ttSHORT(g + 8);
  Result:= 1;
end;

procedure TTTFFont.stbtt_GetGlyphHMetrics(const GlyphIndex: Integer;
  var advanceWidth, leftSideBearing: Integer);
var
  numOfLongHorMetrics: Cardinal;
begin
  numOfLongHorMetrics:= Integer(ttUSHORT(hhea + 34));
  if GlyphIndex < numOfLongHorMetrics then
  begin
    advanceWidth:= Integer(ttSHORT(hmtx + 4 * GlyphIndex));
    leftSideBearing:= Integer(ttSHORT(hmtx + 4 * GlyphIndex + 2));
  end
  else
  begin
    advanceWidth:= Integer(ttSHORT(hmtx + 4 * (numOfLongHorMetrics - 1)));
    leftSideBearing:=
      Integer(ttSHORT(hmtx + 4 * numOfLongHorMetrics + 2 * (GlyphIndex - numOfLongHorMetrics)));
  end;
end;

function TTTFFont.stbtt_GetGlyphKernAdvance(const glyph1,glyph2: Integer): Integer;
var
  needle, straw: Cardinal;
  l, r, m: Integer;
begin
  Result:= 0;

  if Self.kern <= 0 then
    Exit;

  // we only look at the first table. it must be 'horizontal' and format 0.
  if (ttUSHORT(Self.kern + 2) <  1) then  // number of tables
    Exit;

  if (ttUSHORT(Self.kern + 8) <> 1) then  // horizontal flag, format
    Exit;

  l:= 0;
  r:= ttUSHORT(Self.kern + 10) - 1;
  needle:= (glyph1 shl 16) or glyph2;
  while l <= r Do
  begin
    m:= (l + r) shr 1;
    straw:= ttULONG(Self.kern + 18 + (m * 6)); // note: unaligned read
    if needle < straw then
      r:= m - 1
    else if (needle > straw) then
      l:= m + 1
    else
    begin
      Result:= ttSHORT(Self.kern + 22 + (m * 6));
      Exit;
    end;
  end;
end;

procedure TTTFFont.stbtt_GetGlyphShape(const glyph_index: Integer;
  out Result: TStBttVertexArray);
var
  numberOfContours: Smallint;
  endPtsOfContours: Cardinal;
  g: Integer;

  flags, flagcount: Byte;
  ins, i, j, m, n, next_move, was_off, off: Integer;
  x, y, cx, cy, sx, sy, dx, dy: Smallint;
  more: Integer;
  comp2: Cardinal;
  points: PByteArray;

  gidx: Word;

  comp_verts: TStBttVertexArray;
  ms, ns: Single;
  mtx: array [0 .. 5] of Single;
  xx, yy: Smallint;
  PointIndex: Integer;

  TempSize: Integer;
begin
  // stbtt_uint8 *data = info->data;
  Result.list:= nil;
  Result.count:= 0;
  g:= stbtt__GetGlyfOffset(glyph_index);

  if g < 0 then
    Exit;

  numberOfContours := ttSHORT(g);

  if numberOfContours > 0 then
  begin
    flags := 0;
    j := 0;
    was_off := 0;
    endPtsOfContours := g + 10;
    ins := ttUSHORT(g + 10 + numberOfContours * 2);
    points := PByteArray(TFontTypes.PtrUInt(FData) +
                         TFontTypes.PtrUInt(g) + 10 +
                         TFontTypes.PtrUInt(numberOfContours) * 2 + 2 +
                         TFontTypes.PtrUInt(ins));

    n := 1 + ttUSHORT(endPtsOfContours + numberOfContours * 2 - 2);

    m := n + numberOfContours;
    // a loose bound on how many vertices we might need

    Result.Count := m;
    SetLength(Result.List, Result.Count);

    next_move := 0;
    flagcount := 0;

    // in first pass, we load uninterpreted data into the allocated array
    // above, shifted to the end of the array so we won't overwrite it when
    // we create our final data starting from the front
    off := m - n;

    // starting offset for uninterpreted data, regardless of how m ends up being calculated
    // first load flags
    PointIndex := 0;
    for i := 0 to n - 1 do
    begin
      if flagcount = 0 then
      begin
        flags := points[PointIndex];
        Inc(PointIndex);
        if (flags and 8) <> 0 then
        begin
          flagcount := points[PointIndex];
          Inc(PointIndex);
        end;
      end
      else
        Dec(flagcount);
      //
      Result.List[off + i].vertexType := flags;
    end;

    // now load x coordinates
    x := 0;
    for i := 0 to n - 1 do
    begin
      flags := Result.List[off + i].vertexType;
      if (flags and 2) <> 0 then
      begin
        dx := points[PointIndex];
        Inc(PointIndex);

        if (flags and 16) <> 0 then
          Inc(x, dx)
        else
          Dec(x, dx); // ???
      end
      else
      begin
        if (flags and 16) = 0 then
        begin
          x := x + (Smallint(points[PointIndex]) shl 8) +
            Smallint(points[PointIndex + 1]);
          Inc(PointIndex, 2);
        end;
      end;
      Result.List[off + i].x := x;
    end;

    // now load y coordinates
    y := 0;
    for i := 0 to n - 1 do
    begin
      flags := Result.List[off + i].vertexType;
      if (flags and 4) <> 0 then
      begin
        dy := points[PointIndex];
        Inc(PointIndex);

        if (flags and 32) <> 0 then
          Inc(y, dy)
        else
          Dec(y, dy); // ???
      end
      else
      begin
        if (flags and 32) = 0 then
        begin
          y := y + (Smallint(points[PointIndex]) shl 8) +
            Smallint(points[PointIndex + 1]);
          Inc(PointIndex, 2);
        end;
      end;
      Result.List[off + i].y := y;
    end;

    // now convert them to our format
    Result.Count := 0;
    sx := 0;
    sy := 0;
    cx := 0;
    cy := 0;
    i  := 0;
    while i < n do
    begin
      flags := Result.List[off + i].vertexType;
      x := Smallint(Result.List[off + i].x);
      y := Smallint(Result.List[off + i].y);
      if next_move = i then
      begin
        // when we get to the end, we have to close the shape explicitly
        if i <> 0 then
        begin
          if was_off <> 0 then
            stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vcurve, sx,
              sy, cx, cy)
          else
            stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vline, sx,
              sy, 0, 0);

          Inc(Result.Count);
        end;

        // now start the new one
        stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vmove, x, y, 0, 0);
        Inc(Result.Count);
        next_move := 1 + ttUSHORT(endPtsOfContours + j * 2);
        Inc(j);
        was_off := 0;
        sx := x;
        sy := y;
      end
      else
      begin
        if (flags and 1) = 0 then // if it's a curve
        begin
          if was_off <> 0 then
          // two off-curve control points in a row means interpolate an on-curve midpoint
          begin
            stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vcurve,
              (cx + x) shr 1, (cy + y) shr 1, cx, cy);
            Inc(Result.Count);
          end;
          cx := x;
          cy := y;
          was_off := 1;
        end
        else
        begin
          if was_off <> 0 then
            stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vcurve, x,
              y, cx, cy)
          else
            stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vline, x, y, 0, 0);
          Inc(Result.Count);
          was_off := 0;
        end;
      end;
      Inc(i);
    end;
    if i <> 0 then
    begin
      if was_off <> 0 then
        stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vcurve, sx, sy, cx, cy)
      else
        stbtt_setvertex(Result.List[Result.Count], TFontTypes.STBTT_vline, sx, sy, 0, 0);
      Inc(Result.Count);
    end;
  end
  else if numberOfContours = -1 then
  begin
    // Compound shapes.
    more := 1;
    comp2 := g + 10;
    Result.Count := 0;
    Result.List := nil;

    while more <> 0 do
    begin
      comp_verts.Count := 0;
      comp_verts.List := nil;

      mtx[0] := 1;
      mtx[1] := 0;
      mtx[2] := 0;
      mtx[3] := 1;
      mtx[4] := 0;
      mtx[5] := 0;

      flags := ttSHORT(comp2);
      Inc(comp2, 2);
      gidx := ttSHORT(comp2);
      Inc(comp2, 2);

      if (flags and 2) <> 0 then // XY values
      begin
        if (flags and 1) <> 0 then // shorts
        begin
          mtx[4] := ttSHORT(comp2);
          Inc(comp2, 2);
          mtx[5] := ttSHORT(comp2);
          Inc(comp2, 2);
        end
        else
        begin
          mtx[4] := ttBYTE(comp2);
          Inc(comp2);
          mtx[5] := ttBYTE(comp2);
          Inc(comp2);
        end;
      end
      else
      begin
        // TODO handle matching point
        // STBTT_assert(0);
      end;
      if (flags and (1 shl 3)) <> 0 then // WE_HAVE_A_SCALE
      begin
        mtx[0] := ttSHORT(comp2) / 16384.0;
        mtx[1] := 0;
        mtx[2] := 0;
        mtx[3] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
      end
      else if (flags and (1 shl 6)) <> 0 then // WE_HAVE_AN_X_AND_YSCALE
      begin
        mtx[0] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
        mtx[1] := 0;
        mtx[2] := 0;
        mtx[3] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
      end
      else if (flags and (1 shl 7)) <> 0 then // WE_HAVE_A_TWO_BY_TWO
      begin
        mtx[0] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
        mtx[1] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
        mtx[2] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
        mtx[3] := ttSHORT(comp2) / 16384.0;
        Inc(comp2, 2);
      end;

      // Find transformation scales.
      ms := Sqrt(mtx[0] * mtx[0] + mtx[1] * mtx[1]);
      ns := Sqrt(mtx[2] * mtx[2] + mtx[3] * mtx[3]);

      // Get indexed glyph.
      stbtt_GetGlyphShape(gidx, comp_verts);
      if comp_verts.Count > 0 then
      begin
        // Transform vertices.
        for i := 0 to comp_verts.Count - 1 do
        begin
          // comp_verts[i];
          xx := comp_verts.List[i].x;
          yy := comp_verts.List[i].y;

          comp_verts.List[i].x :=
            Smallint(Round(ms * (mtx[0] * xx + mtx[2] * yy + mtx[4])));
          comp_verts.List[i].y :=
            Smallint(Round(ns * (mtx[1] * xx + mtx[3] * yy + mtx[5])));

          xx := comp_verts.List[i].cx;
          yy := comp_verts.List[i].cy;

          comp_verts.List[i].cx :=
            Smallint(Round(ms * (mtx[0] * xx + mtx[2] * yy + mtx[4])));
          comp_verts.List[i].cy :=
            Smallint(Round(ns * (mtx[1] * xx + mtx[3] * yy + mtx[5])));
        end;

        // Append vertices.
        TempSize := Result.Count + comp_verts.Count;
        SetLength(Result.List, TempSize);

        for i := 0 To Pred(comp_verts.Count) do
          Result.List[Result.Count + i] := comp_verts.List[i];

        Result.Count := TempSize;
      end;

      // More components ?
      more := flags and (1 shl 5);
    end;
  end
  else If numberOfContours < 0 then
  begin
    // TODO other compound variations?
    // STBTT_assert(0);
  end
  else
  begin
    // numberOfCounters == 0, do nothing
  end;
end;

procedure TTTFFont.stbtt_Rasterize(const resultTexture: TEngineTexture;
  const FlatnessInPixels: Single; var vertices: TStBttVertexArray;
  const ScaleX, ScaleY, ShiftX, ShiftY: Single; xoff, yoff, Invert: Integer);
var
  Scale: Single;
  WindingLengths: TContourArray;
  Windings: TStBttPointArray;
begin
  Scale:= Min(ScaleX, ScaleY);
  stbtt_FlattenCurves(vertices, FlatnessInPixels / Scale, WindingLengths, Windings);
  if Windings.Count > 0 then
    stbtt__rasterize(resultTexture, Windings, WindingLengths, ScaleX, ScaleY,
      ShiftX, ShiftY, xoff, yoff, Invert);
end;

procedure TTTFFont.stbtt_setvertex(var v: TStBttVertex; typeByte: Byte; x, y,
  cx, cy: Smallint);
begin
  v.vertexType:= typeByte;
  v.x:= x;
  v.y:= y;
  v.cx:= cx;
  v.cy:= cy;
end;

function TTTFFont.HasGlyph(const AID: Cardinal): Boolean;
var
  p: Integer;
begin
  p:= stbtt_FindGlyphIndex(AID);
  Result:= p > 0;
end;

function TTTFFont.ScaleForPixelHeight(const AHeight: Single): Single;
var
  LHeight: Integer;
begin
  if FReady Then
  begin
    LHeight:= Integer(ttSHORT(hhea + 4) - ttSHORT(hhea + 6));
    Result:= AHeight / LHeight;
  end
  else
    Result:= 0.0;
end;

function TTTFFont.GetCodepointBitmap(ScaleX, ScaleY: Single;
  const codepoint, glyphindex: Integer; const ATexture: TEngineTexture;
  var xoff, yoff: Integer): Boolean;
begin
  if FReady then
    Result:= stbtt_GetGlyphBitmap(ScaleX, ScaleY, 0, 0, glyphindex, ATexture, xoff, yoff)
  else
    Result:= False;
end;

function TTTFFont.InitGlyph(const AFont: TFontInstance; const ACharID: Cardinal;
  const AFontSize: Integer): TFontGlyph;
var
  LGlyphIndex, LXOffset, LYOffset, LXAdvance, LLeftSideBearing: Integer;
  LCharID: Cardinal;
  LTexture: TEngineTexture;
begin
  LGlyphIndex:= stbtt_FindGlyphIndex(ACharID);
  if LGlyphIndex <= 0 then
    Exit(nil);
  LTexture:= AFont.TempTexture;
  //
  FScale:= Self.ScaleForPixelHeight(AFontSize);
  if ACharID = TFontTypes.SpaceCharID then
  begin
    LCharID:= Cardinal('_');
    LGlyphIndex:= stbtt_FindGlyphIndex(LCharID);
    Self.GetCodepointBitmap(FScale, FScale, LCharID, LGlyphIndex, LTexture, LXOffset, LYOffset);
    LTexture.Clear;
    LTexture.Width:= TFontTypes.SpaceWidth;
    LTexture.Height:= TFontTypes.SpaceHeight;
    Self.stbtt_GetCodepointHMetrics(LCharID, LXAdvance, LLeftSideBearing);
    Result:= AFont.AddGlyphFromImage(ACharID, LTexture, LXOffset, LYOffset, Trunc(LXAdvance * FScale));
    Exit;
  end;
  //
  if Self.GetCodepointBitmap(FScale, FScale, ACharID, LGlyphIndex, LTexture, LXOffset, LYOffset) then
  begin
    Self.stbtt_GetCodepointHMetrics(ACharID, LXAdvance, LLeftSideBearing);
    Result:= AFont.AddGlyphFromImage(ACharID, LTexture, LXOffset, LYOffset, Trunc(LXAdvance * FScale));
  end else
    Result:= nil;
end;

function TTTFFont.GetKerning(const ACurrent, ANext: Cardinal): Integer;
begin
  Result:= Trunc(Self.stbtt_GetCodepointKernAdvance(ACurrent,ANext) * FScale);
end;

end.
