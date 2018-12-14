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
  XSuperObject,
  se.common.helper, se.game.types, se.game.assetsmanager, se.game.window.style;

type
  TWindowFactory = class;
  TWindow = class
  private
    FFactory: TWindowFactory;
    FName: string;
    FParent: TFmxObject;
    FOnControlClick: TNotifyEvent;
    /// <summary>
    ///   注册指定控件的点击事件(由脚本接管)
    /// </summary>
    procedure RegisterClickEvent(const AControl: string; const AMsgcode: Integer);
    /// <summary>
    ///   通过lyt文件生成界面(生成控件和布局, 这个lyt相当于delphi的fmx或dfm)
    /// </summary>
    procedure MakeLayout(const AFile: string);
    /// <summary>
    ///   通过lytx文件填充界面(界面的Image属性及事件等)
    /// </summary>
    procedure MakeFill(const AFile: string);
    /// <summary>
    ///   设置父类
    /// </summary>
    procedure SetParent(const Value: TFmxObject);
  protected
    /// <summary>
    ///   窗口容器(所有控件都在这个容器中)
    /// </summary>
    FContainer: TRectangle;
  public
    constructor Create(AOwner: TComponent; const ALayoutFile: string = '');
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
    ///   窗口名称
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    ///   窗口父级控件
    /// </summary>
    property Parent: TFmxObject read FParent write SetParent;
    /// <summary>
    ///   窗口中控件的点击事件
    /// </summary>
    property OnControlClick: TNotifyEvent read FOnControlClick write FOnControlClick;
  end;

  TWindowFactory = class
  private
    FOwnerForm: TForm;
    FMask: TRectangle;
    FWindowMap: TObjectDictionary<string, TWindow>;
    FOnPrint: TNotifyInfoEvent;
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
    // 信息打印
    property OnPrint: TNotifyInfoEvent read FOnPrint write FOnPrint;
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
    procedure Show(const AWindowName: string);
    /// <summary>
    ///   关闭窗口
    /// </summary>
    procedure Close(const AWindowName: string);
  end;

implementation

uses FMX.Edit;

{ TWindow }

constructor TWindow.Create(AOwner: TComponent; const ALayoutFile: string);
begin
  inherited Create;
  if FileExists(ALayoutFile) then
    MakeLayout(ALayoutFile)
  else
  begin
    FContainer:= TRectangle.Create(AOwner);
    FContainer.Opacity:= 0;
    FContainer.Width:= 320;
    FContainer.Height:= 240;
    FContainer.Align:= TAlignLayout.Center;
    FContainer.Fill.Kind:= TBrushKind.Bitmap;
    FContainer.Stroke.Kind:= TBrushKind.None;
  end;
end;

procedure TWindow.MakeLayout(const AFile: string);
var
  LStrStream, LMemStream: TMemoryStream;
begin
  LStrStream:= TStringStream.Create;
  LMemStream:= TMemoryStream.Create;
  try
    LStrStream.LoadFromFile(AFile);
    ObjectTextToBinary(LStrStream, LMemStream);
    LMemStream.Position:= 0;
    FContainer:= TRectangle(LMemStream.ReadComponent(nil));
    FContainer.Opacity:= 0;
    FContainer.Stroke.Kind:= TBrushKind.None;
    FName:= FContainer.Name;
    if FileExists(AFile + 'x') then
      MakeFill(AFile + 'x');
  finally
    FreeAndNil(LStrStream);
    FreeAndNil(LMemStream);
  end;
end;

procedure TWindow.MakeFill(const AFile: string);
var
  ja: ISuperArray;
  I: Integer;
  LName: string;
  LComponent: TComponent;
begin
  ja:= TSuperArray.ParseFile(AFile);
  try
    for I:= 0 to ja.Length -1 do
    begin
      LName:= ja.O[I].S['name'];
      if LName = FContainer.Name then
      begin
        if ja.O[I].Contains('image') then
          FContainer.Fill.Bitmap.Bitmap.LoadFromFile(AssetsManager.RequireFile(ja.O[I].S['image']));
      end else
      begin
        LComponent:= FContainer.FindComponent(LName);
        if not Assigned(LComponent) then Continue;
        if LComponent is TImage then
        begin
          if ja.O[I].Contains('image') then
            TImage(LComponent).Bitmap.LoadFromFile(AssetsManager.RequireFile(ja.O[I].S['image']));
          if ja.O[I].Contains('mousedown') then
            TUIEvent.SetEvent(TImage(LComponent), 'FOnMouseDown', ja.O[I].S['mousedown']);
          if ja.O[I].Contains('mouseup') then
            TUIEvent.SetEvent(TImage(LComponent), 'FOnMouseUp', ja.O[I].S['mouseup']);
          if ja.O[I].Contains('mouseleave') then
            TUIEvent.SetEvent(TImage(LComponent), 'FOnMouseLeave', ja.O[I].S['mouseleave']);
        end;
      end;
    end;
  finally
    ja:= nil;
  end;
end;

destructor TWindow.Destroy;
begin
  FreeAndNil(FContainer);
  inherited;
end;

procedure TWindow.RegisterClickEvent(const AControl: string;
  const AMsgcode: Integer);
var
  LComponent: TComponent;
begin
  LComponent:= FContainer.FindComponent(AControl);
  if Assigned(LComponent) and (LComponent is TControl) then
  begin
    TControl(LComponent).OnClick:= FOnControlClick;
    LComponent.Tag:= AMsgcode;
  end;
end;

procedure TWindow.Resize(const AScale: Single);
begin
  FContainer.Scale.X:= AScale;
  FContainer.Scale.Y:= AScale;
end;

procedure TWindow.SetParent(const Value: TFmxObject);
begin
  FParent:= Value;
  FContainer.Parent:= FParent;
end;

procedure TWindow.ShowMe;
begin
  FContainer.Visible:= True;
  FContainer.BringToFront;
  TAnimator.AnimateFloat(FContainer, 'Opacity', 1.0, 1.0);
end;

procedure TWindow.HideMe;
begin
  FContainer.Visible:= False;
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
  LWindow: TWindow;
begin
  for LWindow in FWindowMap.Values do
    LWindow.Resize(AScale);
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
  AWindow.Name:= AName;
  AWindow.HideMe;
  AWindow.FFactory:= Self;
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

procedure TWindowFactory.Show(const AWindowName: string);
var
  LWindow: TWindow;
begin
  if FWindowMap.TryGetValue(AWindowName, LWindow) then
  begin
    FMask.Visible:= True;
    LWindow.ShowMe;
  end;
end;

procedure TWindowFactory.Close(const AWindowName: string);
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
