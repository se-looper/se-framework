{ ------------------------------------------ }
{ }
{ (c) 2017 by Aone }
{ }
{ QQ: 1467948783 }
{ }
{ http://www.cnblogs.com/onechen }
{ }
{ ------------------------------------------ }
{ Start: 2017.07.07 }
{ ------------------------------------------ }

unit ONE.Toast;

interface

uses
  System.Types, System.Classes, System.SysUtils, System.UIConsts,
  System.UITypes, System.Messaging,

  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Objects, FMX.Layouts,
  FMX.Ani, FMX.Effects;

type
  TOneToast = class(TObject)
  private
    FixedWidth, FixedHeight: Integer;
    MainLayout: TLayout;
    ToastLayout: TLayout;
    ToastRectangle: TRectangle;
    ToastText: TText;
    OpacityAnimation: TFloatAnimation;
    procedure OnFinish(Sender: TObject);
  public
    constructor Create(const AFixedWidth, AFixedHeight: Integer); virtual;
    destructor Destroy; override;

    procedure Toast(const Text: string; const FontSize: Single;
      const TextAlign: TTextAlign; const BackgroundColor, FontColor: TAlphaColor);
  end;

procedure OneToast(const Text: string;
                   const FontSize: Single = 14;
                   const TextAlign: TTextAlign = TTextAlign.Trailing;
                   const BackgroundColor: TAlphaColor = claGray;
                   const FontColor: TAlphaColor = claWhite;
                   const FixedWidth: Integer = 0;
                   const FixedHeight: Integer = 0);

implementation

constructor TOneToast.Create(const AFixedWidth, AFixedHeight: Integer);
begin
  FixedWidth := AFixedWidth;
  FixedHeight := AFixedHeight;

  MainLayout := TLayout.Create(nil);
  MainLayout.Align := TAlignLayout.Client;

  ToastLayout := TLayout.Create(MainLayout);
  ToastLayout.Align := TAlignLayout.Center;
  ToastLayout.Opacity := 0;
  ToastLayout.Padding.Rect := RectF(5, 5, 5, 5);
  MainLayout.AddObject(ToastLayout);

  OpacityAnimation := TFloatAnimation.Create(ToastLayout);
  OpacityAnimation.Duration := 0.2;
  OpacityAnimation.PropertyName := 'Opacity';
  OpacityAnimation.StartValue := 1.0;
  OpacityAnimation.StopValue := 0.0;
  ToastLayout.AddObject(OpacityAnimation);

  ToastRectangle := TRectangle.Create(ToastLayout);
  ToastRectangle.HitTest := False;
  ToastRectangle.Align := TAlignLayout.Client;
  ToastRectangle.Stroke.Kind := TBrushKind.None;
  ToastLayout.AddObject(ToastRectangle);

  ToastText := TText.Create(ToastRectangle);
  ToastText.HitTest := False;
  ToastText.Align := TAlignLayout.Client;
  ToastText.TextSettings.WordWrap := False;
  ToastRectangle.AddObject(ToastText);
end;

destructor TOneToast.Destroy;
begin
  MainLayout.Free;
  inherited;
end;

procedure TOneToast.OnFinish(Sender: TObject);
begin
  Application.MainForm.RemoveObject(MainLayout);
  Self.Free;
end;

procedure TOneToast.Toast(const Text: String; const FontSize: Single;
  const TextAlign: TTextAlign; const BackgroundColor: TAlphaColor;
  const FontColor: TAlphaColor);
var
  R: TRectF;
begin
  Application.MainForm.AddObject(MainLayout);

  R := RectF(0, 0, 10000, 10000);
  ToastText.Canvas.Font.Size := FontSize;
  ToastText.Canvas.MeasureText(R, Text, False, [], TTextAlign.Leading,
    TTextAlign.Leading);
  if FixedHeight > 0 then
    ToastLayout.Height := FixedHeight
  else
    ToastLayout.Height := R.Height * 2.2;
  //
  if FixedWidth > 0 then
    ToastLayout.Width := FixedWidth
  else
    ToastLayout.Width := Application.MainForm.Width * 0.7;
  // R.Width + ToastLayout.Height + ExtendWidth;

  ToastRectangle.Fill.Color := BackgroundColor;
  ToastRectangle.XRadius := ToastLayout.Height * 0.15;
  ToastRectangle.YRadius := ToastRectangle.XRadius;

  ToastText.Color := FontColor;
  ToastText.Text := Text;
  ToastText.TextSettings.Font.Size := FontSize;

  case TextAlign of
    TTextAlign.Leading:
      ToastLayout.Margins.Bottom := Application.MainForm.Height * 0.8;
    TTextAlign.Trailing:
      ToastLayout.Margins.Top := Application.MainForm.Height * 0.8;
    TTextAlign.Center:
      ToastLayout.Margins.Rect := TRectF.Empty;
  end;

  ToastLayout.Opacity := 0;
  TAnimator.AnimateFloat(ToastLayout, 'Opacity', 1);

  OpacityAnimation.Delay := 2;
  OpacityAnimation.OnFinish := OnFinish;
  OpacityAnimation.Start;
end;

procedure OneToast(const Text: string; const FontSize: Single;
  const TextAlign: TTextAlign; const BackgroundColor, FontColor: TAlphaColor;
  const FixedWidth, FixedHeight: Integer);
var
  LToast: TOneToast;
begin
  LToast:= TOneToast.Create(FixedWidth, FixedHeight);
  LToast.Toast(Text, FontSize, TextAlign, BackgroundColor, FontColor);
end;

end.
