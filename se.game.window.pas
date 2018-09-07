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

unit se.game.window;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.UITypes, System.UIConsts, System.Generics.Defaults,
  FMX.Types, FMX.Objects, FMX.Ani, FMX.Controls, FMX.Graphics, FMX.Forms,
  se.game.window.style;

type
  TWindow = class(TRectangle)
  private
    FOnControlClick: TNotifyEvent;
    procedure RegisterClickEvent(const AControl: string; const AMsgcode: Integer);
  protected
    FControlMap: TObjectDictionary<string, TControl>;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    /// <summary>
    ///   重置大小
    /// </summary>
    procedure Resize(const AScale: Single);
    /// <summary>
    ///   显示
    /// </summary>
    procedure ShowMe;  virtual;
    /// <summary>
    ///   隐藏
    /// </summary>
    procedure HideMe; virtual;
    /// <summary>
    ///   窗口中的控件点击事件
    /// </summary>
    property OnControlClick: TNotifyEvent read FOnControlClick write FOnControlClick;
  end;

  TWindowFactory = class
  private
    FOwnerForm: TForm;
    FMask: TRectangle;
    FWindowMap: TObjectDictionary<string, TWindow>;
    procedure SetOwnerForm(const Value: TForm);
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   重置大小
    /// </summary>
    procedure Resize(const AScale: Single);
    /// <summary>
    ///   注册一个窗口实例
    /// </summary>
    function RegWindow(const AName: string; AWindow: TWindow): Boolean;
    /// <summary>
    ///   所在窗口,一般传入主窗口Application.MainForm
    /// </summary>
    property OwnerForm: TForm read FOwnerForm write SetOwnerForm;
  public
    /// <summary>
    ///   注册窗口中指定控件的点击事件
    /// </summary>
    /// <param name="AWindowName">
    ///   窗口名称
    /// </param>
    /// <param name="AControlName">
    ///   控件名称
    /// </param>
    /// <param name="AMsgcode">
    ///   触发的消息编码
    /// </param>
    procedure RegisterClickEvent(const AWindowName, AControlName: string;
      const AMsgcode: Integer);
    /// <summary>
    ///   显示窗口
    /// </summary>
    procedure ShowWindow(const AWindowName: string);
    /// <summary>
    ///   关闭窗口
    /// </summary>
    procedure CloseWindow(const AWindowName: string);
  end;

implementation

{ TWindow }

constructor TWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlMap:= TObjectDictionary<string, TControl>.Create([doOwnsValues]);
  Self.Opacity:= 0;
  Self.Width:= 320;
  Self.Height:= 240;
  Self.Align:= TAlignLayout.Center;
  Self.Fill.Kind:= TBrushKind.Bitmap;
  Self.Stroke.Kind:= TBrushKind.None;
end;

destructor TWindow.Destroy;
begin
  FreeAndNil(FControlMap);
  inherited;
end;

procedure TWindow.RegisterClickEvent(const AControl: string;
  const AMsgcode: Integer);
var
  LControl: TControl;
begin
  if FControlMap.TryGetValue(AControl, LControl) then
  begin
    LControl.OnClick:= FOnControlClick;
    LControl.Tag:= AMsgcode;
  end;
end;

procedure TWindow.Resize(const AScale: Single);
begin
  Self.Scale.X:= AScale;
  Self.Scale.Y:= AScale;
end;

procedure TWindow.ShowMe;
begin
  Self.Visible:= True;
  Self.BringToFront;
  TAnimator.AnimateFloat(Self, 'Opacity', 1.0, 1.0);
end;

procedure TWindow.HideMe;
begin
  Self.Visible:= False;
end;

{ TWindowFactory }

constructor TWindowFactory.Create;
begin
  inherited Create;
  FOwnerForm:= nil;
  FWindowMap:= TObjectDictionary<string, TWindow>.Create([doOwnsValues]);
  FMask:= TRectangle.Create(nil);
  FMask.Align:= TAlignLayout.Contents;
  FMask.Fill.Color:= claBlack;
  FMask.Stroke.Kind:= TBrushKind.None;
  FMask.Opacity:= 0.5;
  FMask.Visible:= False;
end;

destructor TWindowFactory.Destroy;
begin
  FreeAndNil(FMask);
  FreeAndNil(FWindowMap);
  inherited;
end;

procedure TWindowFactory.Resize(const AScale: Single);
var
  LPair: TPair<string,TWindow>;
begin
  for LPair in FWindowMap do
    LPair.Value.Resize(AScale);
end;

procedure TWindowFactory.SetOwnerForm(const Value: TForm);
begin
  FOwnerForm:= Value;
  FMask.Parent:= FOwnerForm;
end;

function TWindowFactory.RegWindow(const AName: string; AWindow: TWindow): Boolean;
begin
  if AName.IsEmpty then
  begin
    if Assigned(AWindow) then
      FreeAndNil(AWindow);
    Exit(False);
  end;
  if not Assigned(AWindow) then
    Exit(False);
  //
  AWindow.Visible:= False;
  FWindowMap.AddOrSetValue(AName, AWindow);
  Result:= True;
end;

procedure TWindowFactory.RegisterClickEvent(const AWindowName,
  AControlName: string; const AMsgcode: Integer);
var
  LWindow: TWindow;
begin
  if FWindowMap.TryGetValue(AWindowName, LWindow) then
    LWindow.RegisterClickEvent(AControlName, AMsgcode);
end;

procedure TWindowFactory.ShowWindow(const AWindowName: string);
var
  LWindow: TWindow;
begin
  if FWindowMap.TryGetValue(AWindowName, LWindow) then
  begin
    FMask.Visible:= True;
    LWindow.ShowMe;
  end;
end;

procedure TWindowFactory.CloseWindow(const AWindowName: string);
var
  LWindow: TWindow;
begin
  if FWindowMap.TryGetValue(AWindowName, LWindow) then
  begin
    LWindow.HideMe;
    FMask.Visible:= False;
  end;
end;

end.
