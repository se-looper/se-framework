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

unit se.game.assetsmanager;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.UITypes, System.IOUtils,
  System.Generics.Collections, XSuperObject, PXL.Types,
  se.game.types, se.game.helper;

type
  TAssetsType = (atPackage, atPng, atJpg, atTTF, atOgg, atWav, atMap);
  TAssetsTypes = set of TAssetsType;

  TAssetsMode = (amNormal, amAtlas);
  TAssetsModes = set of TAssetsMode;

  TAssetsManager = class(TObject)
  private const
    cChunkFormatting = '%s[%s]';
  private
    class var FInstance: TAssetsManager;
    class constructor Create;
    class destructor Destroy;
  private
    FCanvas: TEngineCanvas;
    FRoot: string;
    FMissImage: TEngineImage;
    FOnPrint: TNotifyInfoEvent;
    //
    FFileMap : TDictionary<string, string>;
    FImageMap: TObjectDictionary<string, TEngineImage>;
    FChunkMap: TDictionary<string, TIntRect>;
    FSoundMap: TDictionary<string, string>;
    FPXLArchiveMap: TObjectDictionary<string, TEngineArchive>;
    procedure SetCanvas(const Value: TEngineCanvas);
    /// <summary>
    ///   解析图片集配置
    /// </summary>
    procedure ParseAtlas(const AAtlasFile: string);
    /// <summary>
    ///   加载PXL的打包格式数据
    /// </summary>
    procedure LoadPXLArchive(const AArchiveFile: string);
    /// <summary>
    ///   打印消息
    /// </summary>
    procedure Print(const AMsg: string);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   生成资源文件Map
    /// </summary>
    /// <param name="ARoot">
    ///   待扫描的根目录
    /// </param>
    /// <param name="AType">
    ///   支持的资源类型
    /// </param>
    /// <param name="AType">
    ///   支持的资源类型
    /// </param>
    /// <param name="AType">
    ///   支持的资源加载模式
    /// </param>
    procedure Mapping(const ARoot: string; const AType: TAssetsTypes;
      const AMode: TAssetsModes);
    /// <summary>
    ///   画布
    /// </summary>
    property Canvas: TEngineCanvas read FCanvas write SetCanvas;
    /// <summary>
    ///   资源根目录
    /// </summary>
    property Root: string read FRoot;
  public
    // example: Require('start_bg.png')
    function Require(const AKey: string;
      const AUseDefault: Boolean = True): TEngineImage; overload;
    // example: Require(obj, 'player_idle')
    function Require(const AArchive: TEngineArchive; const AKey: string;
      const AUseDefault: Boolean = True): TEngineImage; overload;
    // example: Require('data.asvf', 'player_idle')
    function Require(const APackageName, AKey: string;
      const AUseDefault: Boolean = True): TEngineImage; overload;
  end;

  function AssetsManager: TAssetsManager;

implementation

const
  cMissImage= '89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF'
             +'61000000017352474200AECE1CE90000000467414D410000B18F0BFC61050000'
             +'019849444154384F636460603803C4640326284D3618780330C2C0D25590F7E9'
             +'FDEF3F41EC0FEFFEFC05D1DAC6BC5C201A1980D43CBAF3E31733909D06129053'
             +'E1605B74D0448B859589C12D584C28B54241E6CF9FFF7F9F3FFCFEABA04D55D6'
             +'3D445CE4C7B7FF7FEDBC84057CA22445B58C78B976AC7AF51EEE85F46A25A92B'
             +'673E7D6ECFBFF5383FF8F2DDBD1BDFBC35B0E0E705D972E9C4A72FAF9EFFFC39'
             +'B9EEEEB3C33BDE7D88B53F734D418D1BEC2AB80B58D99818E28BE4A42D5D8478'
             +'D9399819564C7FF2FAEED56FDF5F3DFBF9DBDC518857498B8BEBCBC7BFBF53CA'
             +'E46516F63D7A01947F05D20777C1CED52F3F445A9CBAFCEAE9CF5F19D5F2B2EB'
             +'CE99E96A18F27242A519B8B9599841CEE7E66502590A07700366EF3254E31360'
             +'63A94EBCF6C045E1D8858D8B5EBE02190495660079012407F21A881F922A2D0C'
             +'0A70B801209BABA6A829E898F081FDC6C3CFCC7CFFD6F76F60492430B5F1DE33'
             +'F7507181C2766505504CC0A31164A2AA0E3797AE391FAF98041BDBC9FD1F3E2C'
             +'9FFAE4D5A70FBFFED44ED35400A94106200B412E1ACD0B0C0C006EC0AD688354'
             +'78B40000000049454E44AE426082';

function AssetsManager: TAssetsManager;
begin
  Exit(TAssetsManager.FInstance);
end;

{ TAssetsManager }

class constructor TAssetsManager.Create;
begin
  FInstance:= TAssetsManager.Create;
end;

constructor TAssetsManager.Create;
begin
  FMissImage:= nil;
  FFileMap := TDictionary<string, string>.Create;
  FImageMap:= TObjectDictionary<string, TEngineImage>.Create([doOwnsValues]);
  FChunkMap:= TDictionary<string, TIntRect>.Create;
  FSoundMap:= TDictionary<string, string>.Create;
  FPXLArchiveMap:= TObjectDictionary<string, TEngineArchive>.Create([doOwnsValues]);
end;

destructor TAssetsManager.Destroy;
begin
  if Assigned(FMissImage) then FreeAndNil(FMissImage);
  FreeAndNil(FFileMap);
  FreeAndNil(FImageMap);
  FreeAndNil(FChunkMap);
  FreeAndNil(FSoundMap);
  FreeAndNil(FPXLArchiveMap);
end;

class destructor TAssetsManager.Destroy;
begin
  FreeAndNil(FInstance);
end;

procedure TAssetsManager.LoadPXLArchive(const AArchiveFile: string);
var
  LArchive: TEngineArchive;
begin
  if FPXLArchiveMap.ContainsKey(AArchiveFile) then Exit;
  LArchive := TEngineArchive.Create;
  try
    LArchive.OpenMode := TEngineArchive.TOpenMode.ReadOnly;
    if not LArchive.OpenFile(AArchiveFile) then
      raise Exception.CreateFmt('Failed to open archive: %s', [AArchiveFile]);
    FPXLArchiveMap.AddOrSetValue(ExtractFileName(AArchiveFile), LArchive);
  except
    on e: exception do
    begin
      FreeAndNil(LArchive);
      Print(e.Message);
    end;
  end;
end;

procedure TAssetsManager.ParseAtlas(const AAtlasFile: string);
var
  jo: ISuperObject;
  I: Integer;
  LSourceName, LKey: string;
  LChunkRect: TIntRect;
begin
  try
    jo:= TSuperObject.ParseFile(AAtlasFile);
    try
      LSourceName:= jo.S['source'];
      for I:= 0 to jo.A['chunk'].Length -1 do
      begin
        LKey:= Format(cChunkFormatting,[LSourceName, jo.A['chunk'].O[I].S['name']]);
        LChunkRect.Left  := jo.A['chunk'].O[I].I['left'];
        LChunkRect.Top   := jo.A['chunk'].O[I].I['top'];
        LChunkRect.Right := jo.A['chunk'].O[I].I['right'];
        LChunkRect.Bottom:= jo.A['chunk'].O[I].I['bottom'];
        FChunkMap.AddOrSetValue(LKey, LChunkRect);
      end;
    finally
      jo:= nil;
    end;
  except
    on e: exception do
    begin
      FChunkMap.Clear;
      Print(e.Message);
    end;
  end;
end;

procedure TAssetsManager.Mapping(const ARoot: string; const AType: TAssetsTypes;
  const AMode: TAssetsModes);
var
  LFiles: TStringDynArray;
  I: Integer;
  LFileName, LFileExt: string;
begin
  if ARoot = '' then Exit;
{$IFDEF MSWINDOWS}
  FRoot:= IncludeTrailingPathDelimiter(ARoot);
{$ELSE}
  {$IFDEF ANDROID}
  FRoot:= TPath.GetDocumentsPath + '/';
  {$ELSE}
  FRoot:= ExtractFilePath(ParamStr(0)) + '/';
  {$ENDIF}
{$ENDIF}
  //
  FFileMap.Clear;
  FImageMap.Clear;
  FChunkMap.Clear;
  FSoundMap.Clear;
  FPXLArchiveMap.Clear;
  LFiles:= TDirectory.GetFiles(FRoot, '*.*', TSearchOption.soAllDirectories);
  for I:= 0 to Length(LFiles) -1 do
  begin
    LFileName:= ExtractFileName(LFiles[I]);
    LFileExt := ExtractFileExt(LFiles[I]);
    if amNormal in AMode then
    begin
      if LFileExt.Equals('.asvf') and (atPackage in AType) then
      begin
        FFileMap.AddOrSetValue(LFileName, LFiles[I]);
        LoadPXLArchive(LFiles[I]);
      end;
      if LFileExt.Equals('.png')  and (atPng in AType) then
        FFileMap.AddOrSetValue(LFileName, LFiles[I]);
      if LFileExt.Equals('.jpg')  and (atJpg in AType) then
        FFileMap.AddOrSetValue(LFileName, LFiles[I]);
      if LFileExt.Equals('.ogg')  and (atOgg in AType) then
        FFileMap.AddOrSetValue(LFileName, LFiles[I]);
      if LFileExt.Equals('.wav')  and (atWav in AType) then
        FFileMap.AddOrSetValue(LFileName, LFiles[I]);
      if LFileExt.Equals('.map')  and (atMap in AType) then
        FFileMap.AddOrSetValue(LFileName, LFiles[I]);
    end;
    //
    if LFileExt.Equals('.atlas') and (amAtlas in AMode) then
    begin
      FFileMap.AddOrSetValue(LFileName, LFiles[I]);
      ParseAtlas(LFiles[I]);
    end;
  end;
end;

procedure TAssetsManager.Print(const AMsg: string);
begin

end;

procedure TAssetsManager.SetCanvas(const Value: TEngineCanvas);
begin
  FCanvas:= Value;
  if not Assigned(FMissImage) then
  begin
    FMissImage:= TEngineImage.Create(FCanvas.Device);
    FMissImage.Name:= 'sys_miss.png';
    {$IFDEF MSWINDOWS}
    FMissImage.LoadFromHexString(cMissImage, '.png');
    {$ELSE}
    FMissImage.LoadFromFile(TPath.GetDocumentsPath + '/' + 'sys_miss.png');
    {$ENDIF}
  end;
end;

function TAssetsManager.Require(const AKey: string;
  const AUseDefault: Boolean): TEngineImage;
var
  LFile: string;
begin
  if not FImageMap.TryGetValue(AKey, Result) then
  begin
    if not Assigned(FCanvas) then
    begin
      if AUseDefault then
        Exit(FMissImage)
      else
        Exit(nil);
    end;
    if not FFileMap.TryGetValue(AKey, LFile) then
    begin
      if AUseDefault then
        Exit(FMissImage)
      else
        Exit(nil);
    end;
    Result:= TEngineImage.Create(FCanvas.Device);
    Result.LoadFromFile(LFile);
    Result.Name:= AKey;
    Result.SetupRegionPatterns(Point2i(Result.Width, Result.Height));
    FImageMap.Add(AKey, Result);
  end;
end;

function TAssetsManager.Require(const AArchive: TEngineArchive;
  const AKey: string; const AUseDefault: Boolean): TEngineImage;
begin
  Result:= AArchive.LoadImage(FCanvas, AKey);
  if not Assigned(Result) then
  begin
    if AUseDefault then
      Exit(FMissImage)
    else
      Exit(nil);
  end;
  FImageMap.Add(AKey+'.image', Result);
end;

function TAssetsManager.Require(const APackageName, AKey: string;
  const AUseDefault: Boolean): TEngineImage;
var
  LArchive: TEngineArchive;
begin
  if FImageMap.TryGetValue(AKey+'.image', Result) then
    Exit(Result);
  if not FPXLArchiveMap.TryGetValue(APackageName, LArchive) then
  begin
    if AUseDefault then
      Exit(FMissImage)
    else
      Exit(nil);
  end;
  Result:= Require(LArchive, AKey, AUseDefault);
end;

end.