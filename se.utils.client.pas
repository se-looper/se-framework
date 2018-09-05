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

unit se.utils.client;

interface

uses
  System.UITypes, System.SysUtils, System.Types, FMX.Types, FMX.Forms;

type
  TClientUtils = class
  private class var
    FMainForm: TForm;
    FAppFile, FAppPath, FAppHome, FAppDocuments, FAppName: string;
    FStatusBarHeight, FScreenScale: Single;
    FPhysicalScreenSize, FRawScreenSize: TSize;
    FCosTable256: array[0..255] of Double;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class property AppFile: string read FAppFile;
    class property AppPath: string read FAppPath;
    class property AppHome: string read FAppHome;
    class property AppDocuments: string read FAppDocuments;
    class property AppName: string read FAppName;
  public
    //设置主窗口
    class procedure SetMainForm(const AForm: TForm); static;
    //状态栏高度
    class property StatusBarHeight: Single read FStatusBarHeight;
    //物理屏幕大小<移动设备不包含状态栏高度>
    class property PhysicalScreenSize: TSize read FPhysicalScreenSize;
    //原始屏幕大小<移动设备包含状态栏及导航栏高度>
    class property RawScreenSize: TSize read FRawScreenSize;
    //屏幕比例
    class property ScreenScale: Single read FScreenScale;
  public
    class function Cos256(const AIndex: Integer): Double;
    class function Sin256(const AIndex: Integer): Double;
  end;

implementation

uses FMX.Platform, FMX.BehaviorManager
{$IFDEF ANDROID}
  , Androidapi.JNIBridge, Androidapi.Helpers, Androidapi.JNI.OS, Androidapi.JNI.JavaTypes
{$ENDIF}
;

function GetStatusBarHeight: Single;
{$IFDEF ANDROID}
var
  resourceID: Integer;
  ScreenService: IFMXScreenService;
  sScale: Single;
  sAbis: string;
  arrAbis: TJavaObjectArray<JString>;
  I: Integer;
  needCheckStatusBarHeight: Boolean;
{$ENDIF}
begin
  Result := 0;
{$IFDEF ANDROID}
  if TOSVersion.Major >= 5 then
  begin
    sAbis := '';
    arrAbis := TJBuild.JavaClass.SUPPORTED_ABIS;
    for I := 0 to arrAbis.Length - 1 do
      sAbis := sAbis + ',' + JStringToString(arrAbis.Items[I]);
    sAbis := sAbis.trim([',']);
  end
  else
    sAbis := JStringToString(TJBuild.JavaClass.CPU_ABI) + ',' + JStringToString(TJBuild.JavaClass.CPU_ABI2);

  needCheckStatusBarHeight := (sAbis.Contains('x86') or JStringToString(TJBuild.JavaClass.FINGERPRINT).Contains('intel')
    or sAbis.Contains('arm64-v8a')) and (TOSVersion.Major >= 4);

  if (TOSVersion.Major >= 5) or (needCheckStatusBarHeight) then
  begin
    sScale := 1;
    if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, IInterface(ScreenService)) then
      sScale := ScreenService.GetScreenScale;

    resourceID := TAndroidHelper.Activity.getResources.getIdentifier(StringToJString('status_bar_height'),
      StringToJString('dimen'), StringToJString('android'));
    if resourceID > 0 then
      Result:= Trunc(TAndroidHelper.Activity.getResources.getDimensionPixelSize(resourceID)
        / sScale);
  end;
{$ENDIF}
end;

function GetScreenScale: Single;
var
  ScreenService: IFMXScreenService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService, ScreenService) then
    Result:= ScreenService.GetScreenScale
  else
    Result:= 1.0;
end;

{ TClientUtils }

class constructor TClientUtils.Create;
var
  I: Integer;
begin
  FStatusBarHeight:= 0;
  FScreenScale:= 1.0;
  FPhysicalScreenSize.Create(0, 0);
  FRawScreenSize.Create(0, 0);
  FMainForm:= nil;
  //
  for I:= 0 to 255 do
    FCosTable256[I]:= Cos((I/256)*2*PI);
end;

class destructor TClientUtils.Destroy;
begin
  FMainForm:= nil;
end;

class procedure TClientUtils.SetMainForm(const AForm: TForm);
var
  LDeviceBehavior: IDeviceBehavior;
  LDisplayMetrics: TDeviceDisplayMetrics;
begin
  if AForm = nil then Exit;
  FMainForm:= AForm;
  //
  FStatusBarHeight:= GetStatusBarHeight;
  FScreenScale:= GetScreenScale;
  FPhysicalScreenSize.cx:= FMainForm.ClientWidth;
  FPhysicalScreenSize.cy:= FMainForm.ClientHeight;
  FRawScreenSize.cx:= FMainForm.Width;
  FRawScreenSize.cy:= FMainForm.Height;
  //
  if TBehaviorServices.Current.SupportsBehaviorService(IDeviceBehavior,
    LDeviceBehavior, FMainForm) then
  begin
    LDisplayMetrics:= LDeviceBehavior.GetDisplayMetrics(FMainForm);
    FPhysicalScreenSize:= LDisplayMetrics.PhysicalScreenSize;
    FPhysicalScreenSize.cy:= Trunc(FPhysicalScreenSize.cy - FStatusBarHeight*FScreenScale);
    FRawScreenSize:= LDisplayMetrics.RawScreenSize;
  end;
end;

class function TClientUtils.Cos256(const AIndex: Integer): Double;
begin
  Result:= FCosTable256[AIndex and 255];
end;

class function TClientUtils.Sin256(const AIndex: Integer): Double;
begin
  Result:= FCosTable256[(AIndex+192) and 255];
end;

end.