unit MainFm;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

uses
  { System }
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.UIConsts, System.Generics.Defaults,
  { FMX }
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Edit, FMX.Ani, FMX.BehaviorManager,
  FMX.StdCtrls, FMX.FontGlyphs, FMX.Surfaces, FMX.Platform, FMX.Objects,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PXL.TypeDef,
  { Very Lib }
  VerySimple.Lua, VerySimple.Lua.Lib,
  { PXL}
  PXL.Types, PXL.Fonts,
  { SE Framework }
  se.utils.client, se.game.main, se.game.assetsmanager, se.game.script.package,
  se.game.sprite;

type
  TBasicPackage = class(TScriptPackage)
  public
    procedure RegFunctions; override;
  published
    function ShowMsg(L: lua_State): Integer;
  end;

  TUIEvent = class
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

  TLoginForm = class(TRectangle)
  private
    FTitle: TImage;
    FEditUserName: TEdit;
    FButtonLogin: TImage;
    FOnLogin: TNotifyEvent;
    procedure SetOnLogin(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Resize(const AScale: Single);

    procedure ShowMe;
    property OnLogin: TNotifyEvent read FOnLogin write SetOnLogin;
  end;

  TRankForm = class(TRectangle)
  private
    FTitle: TImage;
    FRankListView: TPresentedScrollBox;
    FButtonClose: TImage;
    FOnClose: TNotifyEvent;
    FRankItems: TList<TViewListItem>;
    FCustomScale: Single;
    procedure SetOnClose(const Value: TNotifyEvent);
    procedure InitRankItems(const AMax: Integer);
    function AddRankItem: TViewListItem;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Resize(const AScale: Single);

    procedure ShowMe;
    property OnClose: TNotifyEvent read FOnClose write SetOnClose;
  end;

  TMainForm = class(TForm)
    SysTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SysTimerTimer(Sender: TObject);
    procedure btnTestClick(Sender: TObject);
  private
    FGameMain: TGameMain;
    EngineFonts: TBitmapFonts;
    FontTahoma: Integer;
    FStrings: TStrings;
    FBasicPackage: TBasicPackage;
    procedure DoRender(Sender: TObject);
  private
    FSpriteManager: TSpriteManager;
    procedure DoShowLogin(Sender: TObject);
    procedure DoShowRank(Sender: TObject);
  private
    FMask: TRectangle;
    FLoginForm: TLoginForm;
    FRankForm: TRankForm;
    procedure DoLogin(Sender: TObject);
    procedure DoCloseRank(Sender: TObject);

    procedure ShowLogin;
    procedure ShowRank;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.IOUtils, System.Math, ONE.Toast;

function GetMediaPath: StdString;
begin
{$IFDEF MSWINDOWS}
  Result := 'deploy\';
{$ELSE}
  {$IFDEF ANDROID}
    Result := TPath.GetDocumentsPath + '/';
  {$ELSE}
    Result := ExtractFilePath(ParamStr(0)) + '/';
  {$ENDIF}
{$ENDIF}
end;

{ TBasicPackage }

procedure TBasicPackage.RegFunctions;
begin
  inherited;
  RegFunction('ShowMsg','ShowMsg');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FGameMain:= TGameMain.Create(Self);
  FGameMain.OnRender:= DoRender;
  FGameMain.AssetsRoot:= GetMediaPath;
  FGameMain.ScriptRoot:= GetMediaPath + 'script';

  EngineFonts:= TBitmapFonts.Create(FGameMain.Canvas.Device);
  EngineFonts.Canvas:= FGameMain.Canvas;
  FontTahoma:= EngineFonts.AddFromXMLFile(GetMediaPath + 'Tahoma9b.png');
  if FontTahoma = -1 then
  begin
    MessageDlg('Could not load Tahoma font.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  FStrings:= TStringList.Create;

  FSpriteManager:= TSpriteManager.Create(Self);
  FSpriteManager.Canvas:= FGameMain.Canvas;
  FSpriteManager.ViewPort:= FGameMain.DisplaySize;

  //right
  with TGUISprite.Create(FSpriteManager) do
  begin
    Name:= 'btnShowLogin';
    Image:= AssetsManager.Require('head01.png');
    X:= FGameMain.DisplaySize.X  - 84* FGameMain.ScreenScale;
    Y:= 20;
    Width:= Round(84*FGameMain.ScreenScale);
    Height:= Round(84*FGameMain.ScreenScale);
    OnClick:= DoShowLogin;
  end;

  //bottom & center
  with TGUISprite.Create(FSpriteManager) do
  begin
    Name:= 'btnShowRank';
    Image:= AssetsManager.Require('rank_btn.png');
    X:= FGameMain.DisplaySize.X/2 - 81* FGameMain.ScreenScale/2;
    Y:= FGameMain.DisplaySize.Y   - 81 * FGameMain.ScreenScale;
    Width:= Round(81*FGameMain.ScreenScale);
    Height:= Round(81*FGameMain.ScreenScale);
    OnClick:= DoShowRank;
    //HitTest:= False;
  end;

  //ui
  FMask:= TRectangle.Create(nil);
  FMask.Parent:= Self;
  FMask.Align:= TAlignLayout.Contents;
  FMask.Fill.Color:= claBlack;
  FMask.Opacity:= 0.8;
  FMask.Visible:= False;

  FLoginForm:= TLoginForm.Create(nil);
  FLoginForm.Parent:= Self;
  FLoginForm.OnLogin:= DoLogin;
  FLoginForm.Visible:= False;

  FRankForm:= TRankForm.Create(nil);
  FRankForm.Parent:= Self;
  FRankForm.OnClose:= DoCloseRank;
  FRankForm.Visible:= False;

  FGameMain.RegScriptPackage(TBasicPackage, 'BasicPackage');
  FGameMain.DriveWithScript('app.lua', 'SetPackagePath', 'Start');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EngineFonts);
  FreeAndNil(FStrings);
  FreeAndNil(FGameMain);
  FreeAndNil(FSpriteManager);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  FGameMain.Resize;
  FLoginForm.Resize((Min(Self.ClientWidth/640, Self.ClientHeight/480)));
  FRankForm.Resize((Min(Self.ClientWidth/640, Self.ClientHeight/480)));
end;

procedure TMainForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  // Make sure there is nothing in FM canvas cache before using PXL.
  Canvas.Flush;

  // Invoke PXL's multimedia timer, which will call "EngineTiming" to continue drawing on this form with PXL.
  FGameMain.NotifyTick;
end;

procedure TMainForm.btnTestClick(Sender: TObject);
begin
  FStrings.Clear;
end;

procedure TMainForm.DoCloseRank(Sender: TObject);
begin
  FRankForm.Visible:= False;
  FMask.Visible:= False;
end;

procedure TMainForm.DoLogin(Sender: TObject);
begin
  if FLoginForm.FEditUserName.Text = '' then
    OneToast('请输入您的大名!', 14, TTextAlign.Center, claBlack, claWhite, 200, 35)
  else
  begin
    FLoginForm.Visible:= False;
    FMask.Visible:= False;
  end;
end;

procedure TMainForm.DoRender(Sender: TObject);
var
  J, I, LLeft: Integer;
begin
  // Draw gray background.
  for J := 0 to FGameMain.DisplaySize.Y div 40 do
    for I := 0 to FGameMain.DisplaySize.X div 40 do
      FGameMain.Canvas.FillQuad(
        Quad(I * 40, J * 40, 40, 40),
        ColorRect($FF585858, $FF505050, $FF484848, $FF404040));

  for I := 0 to FGameMain.DisplaySize.X div 40 do
    FGameMain.Canvas.Line(
      Point2f(I * 40.0, 0.0),
      Point2f(I * 40.0, FGameMain.DisplaySize.Y),
      $FF555555);

  for J := 0 to FGameMain.DisplaySize.Y div 40 do
    FGameMain.Canvas.Line(
      Point2f(0.0, J * 40.0),
      Point2f(FGameMain.DisplaySize.X, J * 40.0),
      $FF555555);

  FSpriteManager.Render;
  FSpriteManager.Move(1);
  FSpriteManager.Dead;

  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 4.0),
    'FPS: ' + IntToStr(FGameMain.FrameRate),
    ColorPair($FFFFE887, $FFFF0000));

  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 24.0),
    'Technology: ' + FGameMain.FullDeviceTechString,
    ColorPair($FFE8FFAA, $FF12C312));

  for I:= 0 to FStrings.Count -1 do
    EngineFonts[FontTahoma].DrawText(
      Point2f(200.0, 100.0 + (I+1)*20),
      FStrings[I],
      ColorPair($FFE8FFAA, $FF12C312));

  for I:= 0 to Length(FGameMain.Logs) -1 do
    EngineFonts[FontTahoma].DrawText(
      Point2f(200.0, 100.0 + (I+FStrings.Count+1)*20),
      FGameMain.Logs[I],
      ColorPair($FFE8FFAA, $FF12C312));
end;

procedure TMainForm.DoShowLogin(Sender: TObject);
begin
  FStrings.Add('login: '+FStrings.Count.ToString);
  //Self.ShowLogin;
end;

procedure TMainForm.DoShowRank(Sender: TObject);
begin
  //Self.ShowRank;
  FStrings.Add('rank: '+FStrings.Count.ToString);
end;

procedure TMainForm.ShowLogin;
begin
  FMask.Visible:= True;
  FLoginForm.ShowMe;
end;

procedure TMainForm.ShowRank;
begin
  FMask.Visible:= True;
  FRankForm.ShowMe;
end;

procedure TMainForm.SysTimerTimer(Sender: TObject);
begin
  MainForm.Invalidate;
end;

function TBasicPackage.ShowMsg(L: lua_State): Integer;
var
  LMsg: string;
begin
  LMsg:= lua_tostring(L, 2);
  MainForm.FStrings.Add(LMsg);
  Result:= 0;
end;

{ TUIEvent }

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

{ TLoginForm }

constructor TLoginForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Opacity:= 0;
  Self.Width:= 300;
  Self.Height:= 244;
  Self.Align:= TAlignLayout.Center;
  Self.Fill.Kind:= TBrushKind.Bitmap;
  Self.Fill.Bitmap.Bitmap.LoadFromFile(AssetsManager.RequireFile('login_bg.png'));
  Self.Stroke.Kind:= TBrushKind.None;
  //
  FTitle:= TImage.Create(Self);
  FTitle.Parent:= Self;
  FTitle.Align:= TAlignLayout.Top;
  FTitle.Margins.Left:= 25;
  FTitle.Margins.Right:= 25;
  FTitle.Margins.Top:= 40;
  FTitle.Height:= 41;
  FTitle.Bitmap.LoadFromFile(AssetsManager.RequireFile('login_title.png'));

  FEditUserName:= TEdit.Create(Self);
  FEditUserName.Parent:= Self;
  FEditUserName.Align:= TAlignLayout.VertCenter;
  FEditUserName.Margins.Left:= 30;
  FEditUserName.Margins.Right:= 30;
  FEditUserName.StyledSettings:= [];
  FEditUserName.TextSettings.Font.Size:= 14;
  FEditUserName.TextSettings.FontColor:= TAlphaColorRec.Green;
  FEditUserName.Text:= 'ddd';

  FButtonLogin:= TImage.Create(Self);
  FButtonLogin.Parent:= Self;
  FButtonLogin.Align:= TAlignLayout.Bottom;
  FButtonLogin.Margins.Left:= 76;
  FButtonLogin.Margins.Right:= 76;
  FButtonLogin.Margins.Top:= 10;
  FButtonLogin.Margins.Bottom:= 15;
  FButtonLogin.Height:= 59;
  FButtonLogin.Bitmap.LoadFromFile(AssetsManager.RequireFile('login.png'));
  FButtonLogin.OnMouseDown:= TUIEvent.ImageButtonMouseDown;
  FButtonLogin.OnMouseUp:= TUIEvent.ImageButtonMouseUp;
  FButtonLogin.OnMouseLeave:= TUIEvent.ImageButtonMouseLeave;
end;

destructor TLoginForm.Destroy;
begin

  inherited;
end;

procedure TLoginForm.Resize(const AScale: Single);
begin
  Self.Scale.X:= AScale;
  Self.Scale.Y:= AScale;
end;

procedure TLoginForm.SetOnLogin(const Value: TNotifyEvent);
begin
  FOnLogin:= Value;
  FButtonLogin.OnClick:= FOnLogin;
end;

procedure TLoginForm.ShowMe;
begin
  Self.Visible:= True;
  Self.BringToFront;
  TAnimator.AnimateFloat(Self, 'Opacity', 1.0, 1.0);
end;

{ TRankForm }

function TRankForm.AddRankItem: TViewListItem;
begin
  Result:= TViewListItem.Create(Self);
  Result.Parent:= FRankListView;
  Result.Align:= TAlignLayout.Top;
  Result.Image:= AssetsManager.RequireFile('item_bg.png');
  FRankItems.Add(Result);
end;

constructor TRankForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRankItems:= TList<TViewListItem>.Create;
  //
  Self.Opacity:= 0;
  Self.Width:= 300;
  Self.Height:= 423;
  Self.Align:= TAlignLayout.Center;
  Self.Fill.Kind:= TBrushKind.Bitmap;
  Self.Fill.Bitmap.Bitmap.LoadFromFile(AssetsManager.RequireFile('rank_bg.png'));
  Self.Stroke.Kind:= TBrushKind.None;
  //
  FTitle:= TImage.Create(Self);
  FTitle.Parent:= Self;
  FTitle.Align:= TAlignLayout.Top;
  FTitle.Margins.Left:= 25;
  FTitle.Margins.Right:= 25;
  FTitle.Margins.Top:= 30;
  FTitle.Height:= 41;
  FTitle.Bitmap.LoadFromFile(AssetsManager.RequireFile('rank_title.png'));

  FRankListView:= TPresentedScrollBox.Create(Self);
  FRankListView.Parent:= Self;
  FRankListView.Align:= TAlignLayout.Client;
  FRankListView.AutoHide:= TBehaviorBoolean.True;
  FRankListView.Bounces:= TBehaviorBoolean.True;
  FRankListView.ScrollAnimation:= TBehaviorBoolean.True;
  FRankListView.ScrollDirections:= TScrollDirections.Vertical;
  FRankListView.ShowScrollBars:= False;
  FRankListView.Size.PlatformDefault:= False;
  FRankListView.TouchTracking:= TBehaviorBoolean.True;

  FButtonClose:= TImage.Create(Self);
  FButtonClose.Parent:= Self;
  FButtonClose.Align:= TAlignLayout.Bottom;
  FButtonClose.Margins.Left:= 75;
  FButtonClose.Margins.Right:= 75;
  FButtonClose.Margins.Top:= 5;
  FButtonClose.Margins.Bottom:= 15;
  FButtonClose.Height:= 38;
  FButtonClose.Bitmap.LoadFromFile(AssetsManager.RequireFile('rank_close.png'));
  FButtonClose.OnMouseDown:= TUIEvent.ImageButtonMouseDown;
  FButtonClose.OnMouseUp:= TUIEvent.ImageButtonMouseUp;
  FButtonClose.OnMouseLeave:= TUIEvent.ImageButtonMouseLeave;

  InitRankItems(20);
end;

destructor TRankForm.Destroy;
begin
  FRankItems.Clear;
  FreeAndNil(FRankItems);
  inherited;
end;

procedure TRankForm.InitRankItems(const AMax: Integer);
var
  I: Integer;
begin
  for I:= 0 to AMax -1 do
    AddRankItem;

  FRankItems.Sort(
    TComparer<TViewListItem>.Construct(
      function (const L, R: TViewListItem): Integer
      begin
        Result:= Trunc(L.Position.Y - R.Position.Y);
      end
    )
  );

  for I:= 0 to FRankItems.Count -1 do
  begin
    FRankItems[I].Text:= ' -- ';
    FRankItems[I].Detail:= (I+1).ToString;
  end;
end;

procedure TRankForm.Resize(const AScale: Single);
begin
  Self.Scale.X:= AScale;
  Self.Scale.Y:= AScale;
  FCustomScale:= AScale;
end;

procedure TRankForm.SetOnClose(const Value: TNotifyEvent);
begin
  FOnClose:= Value;
  FButtonClose.OnClick:= FOnClose;
end;

procedure TRankForm.ShowMe;
var
  I: Integer;
  LRank: TArray<string>;
begin
  Self.Visible:= True;
  Self.BringToFront;
  TAnimator.AnimateFloat(Self, 'Opacity', 1.0, 1.0);

  //temp rank
  SetLength(LRank, 6);
  LRank[0]:= 'gm';
  LRank[1]:= 'doudou';
  LRank[2]:= 'meme';
  LRank[3]:= 'youyou';
  LRank[4]:= 'mario';
  LRank[5]:= FCustomScale.ToString;
  for I:= 0 to Length(LRank) -1 do
  begin
    FRankItems[I].Text:= LRank[I];
    case I of
      0: FRankItems[I].Image:= AssetsManager.RequireFile('item_bg1.png');
      1: FRankItems[I].Image:= AssetsManager.RequireFile('item_bg2.png');
      2: FRankItems[I].Image:= AssetsManager.RequireFile('item_bg3.png');
      else
         FRankItems[I].Image:= AssetsManager.RequireFile('item_bg4n.png');
    end;
  end;
end;

end.
