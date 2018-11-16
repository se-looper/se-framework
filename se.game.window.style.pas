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

unit se.game.window.style;

interface

uses
  System.Classes, System.SysUtils, System.TypInfo, System.Rtti, System.UITypes,
  System.Generics.Collections, System.UIConsts, System.Generics.Defaults,
  FMX.Types, FMX.Objects, FMX.Ani, FMX.Controls, FMX.Graphics, FMX.Edit,
  FMX.ScrollBox, FMX.BehaviorManager, FMX.Forms;

type
  TUIEvent = class
  private
    /// <summary>
    ///   获取TUIEvent中指定事件名称的事件地址(只能在published域)
    /// </summary>
    class function RequireMethod(const AEventName: string): Pointer; static;
  public
    /// <summary>
    ///   设置控件的属性(指向TUIEvent中的某个事件)
    /// </summary>
    class procedure SetEvent(const AControl: TControl;
      const AEventName, ATargetEventName: string); static;
  published
    class procedure ImageButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    class procedure ImageButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    class procedure ImageButtonMouseLeave(Sender: TObject);
  end;

  TViewListItem = class(TRectangle)
  private
    FDetail: string;
    FText: string;
    FImage: string;
    FTextView, FDetailView: TText;
    procedure SetImage(const Value: string);
      procedure SetDetail(const Value: string);
      procedure SetText(const Value: string);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property Text: string read FText write SetText;
    property Detail: string read FDetail write SetDetail;
    property Image: string read FImage write SetImage;
  end;

implementation

{ TUIEvent }

class procedure TUIEvent.SetEvent(const AControl: TControl;
  const AEventName, ATargetEventName: string);
var
  RTX: TRttiContext;
  RT: TRttiType;
  RF: TRttiField;
  RV: TValue;
begin
  RTX:= TRttiContext.Create;
  try
    RT:= RTX.GetType(AControl.ClassType);
    RF:= RT.GetField(AEventName);
    if Assigned(RF) then
    begin
      RV:= RF.GetValue(AControl);
      TValueData(RV).FAsMethod.Code:= TUIEvent.RequireMethod(ATargetEventName);
      RF.SetValue(AControl, RV);
    end;
  finally
    RTX.Free;
  end;
end;

class function TUIEvent.RequireMethod(const AEventName: string): Pointer;
var
  RTX: TRttiContext;
  RT: TRttiType;
  RM: TRttiMethod;
begin
  RTX:= TRttiContext.Create;
  try
    RT:= RTX.GetType(TUIEvent);
    RM:= RT.GetMethod(AEventName);
    if Assigned(RM) and (RM.Visibility = TMemberVisibility.mvPublished) then
      Result:= RM.CodeAddress
    else
      Exit(nil);
  finally
    RTX.Free;
  end;
end;

class procedure TUIEvent.ImageButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TImage(Sender).Opacity:= 0.8;
end;

class procedure TUIEvent.ImageButtonMouseLeave(Sender: TObject);
begin
  TImage(Sender).Opacity:= 1.0;
end;

class procedure TUIEvent.ImageButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  TImage(Sender).Opacity:= 1.0;
end;

{ TViewListItem }

constructor TViewListItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Fill.Kind:= TBrushKind.Bitmap;
  Stroke.Kind:= TBrushKind.None;
  Margins.Left:= 7;
  Margins.Right:= 3;
  Height:= 41;
  HitTest:= False;

  FTextView:= TText.Create(Self);
  FTextView.Parent:= Self;
  FTextView.Align:= TAlignLayout.Client;
  FTextView.Margins.Left:= 15;
  FTextView.TextSettings.HorzAlign:= TTextAlign.Leading;
  FTextView.Color:= TAlphaColorRec.White;
  FTextView.Font.Style:= [TFontStyle.fsBold];
  FTextView.HitTest:= False;

  FDetailView:= TText.Create(Self);
  FDetailView.Parent:= Self;
  FDetailView.Align:= TAlignLayout.Right;
  FDetailView.Margins.Right:= 15;
  FDetailView.Width:= 20;
  FDetailView.TextSettings.HorzAlign:= TTextAlign.Center;
  FDetailView.Color:= TAlphaColorRec.White;
  FDetailView.HitTest:= False;
end;

destructor TViewListItem.Destroy;
begin

  inherited;
end;

procedure TViewListItem.SetDetail(const Value: string);
begin
  FDetail:= Value;
  FDetailView.Text:= FDetail;
end;

procedure TViewListItem.SetImage(const Value: string);
begin
  FImage:= Value;
  Fill.Bitmap.Bitmap.LoadFromFile(FImage);
end;

procedure TViewListItem.SetText(const Value: string);
begin
  FText:= Value;
  FTextView.Text:= FText;
end;

end.
