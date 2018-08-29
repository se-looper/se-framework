unit frmPreview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  //
  PXL.TypeDef,
  PXL.Types,
  PXL.Timing,
  PXL.Devices,
  PXL.Canvas,
  PXL.Images,
  PXL.Fonts,
  PXL.Providers,
  PXL.FMBridge,
  PXL.ImageFormats,
  PXL.ImageFormats.Auto,
  //
  se.game.types, se.game.assets, se.game.exlib.freetype, se.game.exlib.ui;

type
  TPreviewFrm = class(TForm)
    SysTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SysTimerTimer(Sender: TObject);
  private
    FFMBridge: TFMBridge;
    FScreenScale: Single;
    FDeviceProvider: TGraphicsDeviceProvider;
    FEngineDevice: TCustomDevice;
    FEngineCanvas: TCustomCanvas;
    FEngineTimer: TMultimediaTimer;
    FDisplaySize: TPoint2i;
    FEngineTicks: Integer;
    //
    FControlManager: TUIManager;
    procedure EngineTiming(const Sender: TObject);
    procedure EngineProcess(const Sender: TObject);
    procedure RenderScene;
    procedure Initialize(const AForm: TUIForm);
  public
    class procedure ShowMe(AOwner: TForm; const AWidth, AHeight: Integer;
      const AForm: TUIForm);
  end;

var
  PreviewFrm: TPreviewFrm;

implementation

{$R *.fmx}

{ TPreviewFrm }

class procedure TPreviewFrm.ShowMe(AOwner: TForm; const AWidth, AHeight: Integer;
  const AForm: TUIForm);
begin
  PreviewFrm:= TPreviewFrm.Create(AOwner);
  try
    PreviewFrm.ClientWidth:= AWidth;
    PreviewFrm.ClientHeight:= AHeight;
    PreviewFrm.Initialize(AForm);
    PreviewFrm.ShowModal;
  finally
    FreeAndNil(PreviewFrm);
  end;
end;

procedure TPreviewFrm.SysTimerTimer(Sender: TObject);
begin
  PreviewFrm.Invalidate;
end;

procedure TPreviewFrm.FormCreate(Sender: TObject);
begin
  //pxl engine initialize
  FFMBridge:= TFMBridge.Create;
  FDeviceProvider:= FFMBridge.CreateProvider;
  FScreenScale:= FFMBridge.ScreenScale;
  FEngineDevice:= FDeviceProvider.CreateDevice;
  if (FEngineDevice is TCustomStateDevice) and (not TCustomStateDevice(FEngineDevice).Initialize) then
  begin
    MessageDlg('Failed to initialize Device.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Close;
    Exit;
  end;
  FEngineCanvas:= FDeviceProvider.CreateCanvas(FEngineDevice);
  if not FEngineCanvas.Initialize then
  begin
    MessageDlg('Failed to initialize Canvas.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Close;
    Exit;
  end;
  AssetManager.Canvas:= FEngineCanvas;
  // ui control manager
  FControlManager:= TUIManager.Create(Self, FEngineCanvas);
  //pxl timer
  FEngineTimer:= TMultimediaTimer.Create;
  FEngineTimer.OnTimer:= EngineTiming;
  FEngineTimer.OnProcess:= EngineProcess;
  FEngineTimer.MaxFPS:= 4000;
  //ticks
  FEngineTicks:= 0;
end;

procedure TPreviewFrm.FormDestroy(Sender: TObject);
begin
  FEngineTimer.Enabled:= False;
  SysTimer.Enabled:= False;
  FreeAndNil(FEngineTimer);
  FreeAndNil(FControlManager);
  FreeAndNil(FEngineCanvas);
  FreeAndNil(FEngineDevice);
  FreeAndNil(FDeviceProvider);
  FreeAndNil(FFMBridge);
end;

procedure TPreviewFrm.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  Self.Canvas.Flush;
  FEngineTimer.NotifyTick;
end;

procedure TPreviewFrm.Initialize(const AForm: TUIForm);
var
  LForm: TUIForm;
begin
  LForm:= TUIForm.Create(FControlManager.Root);
  LForm.Assign(AForm);
  LForm.Show(True);
end;

procedure TPreviewFrm.EngineProcess(const Sender: TObject);
begin
  Inc(FEngineTicks);
end;

procedure TPreviewFrm.EngineTiming(const Sender: TObject);
begin
  if FEngineCanvas.BeginScene then
  try
    RenderScene;
    FEngineTimer.Process;
  finally
    FEngineCanvas.EndScene;
  end;
end;

procedure TPreviewFrm.RenderScene;
begin
  FControlManager.Render;
end;

end.
