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

unit se.game.main;

interface

uses
  System.Classes, System.SysUtils, System.UITypes,
  FMX.Forms, FMX.Dialogs,
  PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas, PXL.Providers, PXL.FMBridge,
  se.utils.client, se.game.assetsmanager, se.game.script, se.game.script.package,
  se.game.sprite;

type
  TGameMain = class
  private
    FForm: TForm;
    FBridge: TFMBridge;
    FDeviceProvider: TGraphicsDeviceProvider;
    FDevice: TCustomDevice;
    FCanvas: TCustomCanvas;
    FMultiTimer: TMultimediaTimer;
    FTicks: Integer;
    FDisplaySize: TPoint2i;
    FOnUpdate, FOnRender: TNotifyEvent;
    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);
    procedure RenderScene;
    function GetScreenScale: Single;
    function GetFullDeviceTechString: string;
    function GetFrameRate: Integer;
  private
    FAssetsRoot: string;
    FAssetsMode: TAssetsModes;
    FAssetsType: TAssetsTypes;
    procedure SetAssetsType(const Value: TAssetsTypes);
    procedure SetAssetsMode(const Value: TAssetsModes);
    procedure SetAssetsRoot(const Value: string);
  private
    FScriptSystem: TScriptSystem;
    FScriptRoot: string;
    procedure SetScriptRoot(const Value: string);
    procedure DoScriptPrint(AMsg: string);
    procedure DoScriptError(AMsg: string);
  private
    FSpriteManager: TSpriteManager;
  private
    FLogs: TStrings;
    FScriptLib: string;
    function GetLogs: TArray<string>;
    procedure SetScriptLib(const Value: string);
  public
    constructor Create(const AForm: TForm);
    destructor Destroy; override;

    /// <summary>
    ///   FMultiTimer.NotifyTick
    /// </summary>
    procedure NotifyTick;
    /// <summary>
    ///   渲染窗口大小发生改变时调用这个方法刷新屏幕相关数据
    /// </summary>
    procedure Resize;
    /// <summary>
    ///   注册脚本包
    /// </summary>
    function RegScriptPackage(const AClass: TScriptPackageClass;
      const AName: string): TScriptPackage;
    /// <summary>
    ///   从脚本驱动
    /// </summary>
    procedure DriveWithScript(const ALuaFile, AInitRunEnvironmentMethodName,
      AStartMethodName: string);
  public
    /// <summary>
    ///   数据更新事件
    /// </summary>
    property OnUpdate: TNotifyEvent read FOnUpdate write FOnUpdate;
    /// <summary>
    ///   渲染事件
    /// </summary>
    property OnRender: TNotifyEvent read FOnRender write FOnRender;
    /// <summary>
    ///   画布, PXL的画布
    /// </summary>
    property Canvas: TCustomCanvas read FCanvas;
    /// <summary>
    ///   显示大小, 即将要渲染的区域大小
    /// </summary>
    property DisplaySize: TPoint2i read FDisplaySize;
    /// <summary>
    ///   屏幕比例, win平台一般是1.0, 移动平台会根据分辨率进行计算
    /// </summary>
    property ScreenScale: Single read GetScreenScale;
    /// <summary>
    ///   技术标准(DX or OpenGL...)
    /// </summary>
    property FullDeviceTechString: string read GetFullDeviceTechString;
    /// <summary>
    ///   FPS
    /// </summary>
    property FrameRate: Integer read GetFrameRate;
  public
    /// <summary>
    ///   资源类型, 具体见TAssetsType定义
    /// </summary>
    property AssetsType: TAssetsTypes read FAssetsType write SetAssetsType;
    /// <summary>
    ///   资源加载模式, 具体见TAssetsMode定义
    /// </summary>
    property AssetsMode: TAssetsModes read FAssetsMode write SetAssetsMode;
    /// <summary>
    ///   资源文件的根目录
    /// </summary>
    property AssetsRoot: string read FAssetsRoot write SetAssetsRoot;
  public
    /// <summary>
    ///   脚本文件的根目录
    /// </summary>
    property ScriptRoot: string read FScriptRoot write SetScriptRoot;
  public
    /// <summary>
    ///   精灵管理器
    /// </summary>
    property SpriteManager: TSpriteManager read FSpriteManager;
  public
    /// <summary>
    ///   Log
    /// </summary>
    property Logs: TArray<string> read GetLogs;
  end;

implementation

{ TGameMain }

constructor TGameMain.Create(const AForm: TForm);
begin
  inherited Create;
  FLogs:= TStringList.Create;
  FTicks:= 0;
  FAssetsRoot:= '';
  FAssetsType:= [TAssetsType.atPng, TAssetsType.atJpg, TAssetsType.atOgg];
  FAssetsMode:= [TAssetsMode.amNormal];
  //
  if AForm = nil then
  begin
    MessageDlg('Owner form cannot be null.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;
  FForm:= AForm;
  //
  FBridge:= TFMBridge.Create;
  FDeviceProvider:= FBridge.CreateProvider;
  //
  FDevice:= FDeviceProvider.CreateDevice;
  if (FDevice is TCustomStateDevice) and (not TCustomStateDevice(FDevice).Initialize) then
  begin
    MessageDlg('Failed to initialize PXL Device.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;
  //
  FCanvas:= FDeviceProvider.CreateCanvas(FDevice);
  if not FCanvas.Initialize then
  begin
    MessageDlg('Failed to initialize PXL Canvas.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;
  //
  FMultiTimer:= TMultimediaTimer.Create;
  FMultiTimer.OnTimer  := EngineTiming;
  FMultiTimer.OnProcess:= EngineProcess;
  FMultiTimer.MaxFPS:= 4000;
  //
  FScriptSystem:= TScriptSystem.Create;
  FScriptSystem.OnPrint:= DoScriptPrint;
  FScriptSystem.OnError:= DoScriptError;
  //
  FSpriteManager:= TSpriteManager.Create(AForm);
  FSpriteManager.Canvas:= FCanvas;

  Self.Resize;
  AssetsManager.Canvas:= FCanvas;
end;

destructor TGameMain.Destroy;
begin
  FreeAndNil(FMultiTimer);
  FreeAndNil(FScriptSystem);
  FreeAndNil(FSpriteManager);
  FreeAndNil(FCanvas);
  FreeAndNil(FDevice);
  FreeAndNil(FDeviceProvider);
  FreeAndNil(FBridge);
  FreeAndNil(FLogs);
  inherited;
end;

procedure TGameMain.EngineProcess(const Sender: TObject);
begin
  Inc(FTicks);
  FSpriteManager.Move(1);
  FSpriteManager.Dead;
  if Assigned(FOnUpdate) then FOnUpdate(Sender);
end;

procedure TGameMain.EngineTiming(const Sender: TObject);
begin
  if FCanvas.BeginScene then
  try
    FCanvas.Device.Clear([TClearType.Color], IntColorWhite);
    RenderScene;
    FMultiTimer.Process;
  finally
    FCanvas.EndScene;
  end;
end;

procedure TGameMain.RenderScene;
begin
  FSpriteManager.Render;
  if Assigned(FOnRender) then FOnRender(nil);
  FCanvas.Flush;
end;

procedure TGameMain.NotifyTick;
begin
  FMultiTimer.NotifyTick;
end;

procedure TGameMain.Resize;
begin
  TClientUtils.SetMainForm(FForm);
{$IFDEF MSWINDOWS}
  FDisplaySize:= Point2i(Round(FForm.ClientWidth  * TClientUtils.ScreenScale),
                         Round(FForm.ClientHeight * TClientUtils.ScreenScale));
{$ELSE}
  FDisplaySize.X:= TClientUtils.PhysicalScreenSize.cx;
  FDisplaySize.Y:= TClientUtils.PhysicalScreenSize.cy;
{$ENDIF}
  FSpriteManager.Resize(FDisplaySize.X, FDisplaySize.Y);
end;

function TGameMain.GetScreenScale: Single;
begin
  Result:= TClientUtils.ScreenScale;
end;

function TGameMain.GetFullDeviceTechString: string;
begin
  Result:= PXL.Devices.GetFullDeviceTechString(FDevice);
end;

function TGameMain.GetFrameRate: Integer;
begin
  Result:= FMultiTimer.FrameRate;
end;

procedure TGameMain.SetAssetsMode(const Value: TAssetsModes);
begin
  if FAssetsMode <> Value then
  begin
    FAssetsMode:= Value;
    if FAssetsRoot <> '' then
      AssetsManager.Mapping(FAssetsRoot, FAssetsType, FAssetsMode);
  end;
end;

procedure TGameMain.SetAssetsType(const Value: TAssetsTypes);
begin
  if FAssetsType <> Value then
  begin
    FAssetsType:= Value;
    if FAssetsRoot <> '' then
      AssetsManager.Mapping(FAssetsRoot, FAssetsType, FAssetsMode);
  end;
end;

procedure TGameMain.SetAssetsRoot(const Value: string);
begin
  if FAssetsRoot <> Value then
  begin
    FAssetsRoot:= Value;
    AssetsManager.Mapping(FAssetsRoot, FAssetsType, FAssetsMode);
  end;
end;

procedure TGameMain.SetScriptLib(const Value: string);
begin
  FScriptLib := Value;
end;

procedure TGameMain.SetScriptRoot(const Value: string);
begin
  if FScriptRoot <> Value then
  begin
    FScriptRoot:= Value;
    FScriptSystem.FilePath:= IncludeTrailingPathDelimiter(FScriptRoot);
  end;
end;

function TGameMain.GetLogs: TArray<string>;
begin
  Result:= FLogs.ToStringArray;
end;

procedure TGameMain.DoScriptError(AMsg: string);
begin
  FLogs.Add('[ERROR] ' + AMsg);
end;

procedure TGameMain.DoScriptPrint(AMsg: string);
begin
  FLogs.Add('[INFO] ' + AMsg);
end;

function TGameMain.RegScriptPackage(const AClass: TScriptPackageClass;
  const AName: string): TScriptPackage;
begin
  Result:= FScriptSystem.RegPackage(AClass, AName);
end;

procedure TGameMain.DriveWithScript(const ALuaFile, AInitRunEnvironmentMethodName,
  AStartMethodName: string);
begin
  FScriptSystem.Run(ALuaFile, AInitRunEnvironmentMethodName, AStartMethodName);
end;

end.