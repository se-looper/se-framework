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
  { PXL}
  PXL.Types, PXL.Fonts,
  { SE Framework }
  se.utils.client, se.game.helper, se.game.stage, se.game.assetsmanager,
  se.game.sprite,
  se.game.script.package, se.game.script.package.ui, se.game.script.package.sound,
  se.game.window, se.game.window.style;

type
  TRankWindow = class(TWindow)
  private
    FRankItems: TList<TViewListItem>;
    FRankListView: TPresentedScrollBox;
    procedure InitRankItems(const AMax: Integer);
    function AddRankItem: TViewListItem;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure ShowMe; override;
  end;

  TMainForm = class(TForm)
    SysTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SysTimerTimer(Sender: TObject);
  private
    FGameStage: TGameStage;
    EngineFonts: TBitmapFonts;
    FontTahoma: Integer;
    FStrings: TStrings;
    FUIPackage: TUIPackage;
    procedure DoRender(Sender: TObject);
    procedure DoPrint(const AMsg: string);
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

{ TRankWindow }

constructor TRankWindow.Create(AOwner: TComponent);
var
  LTitleImage, LCloseButton: TImage;
begin
  inherited Create(AOwner);
  FRankItems:= TList<TViewListItem>.Create;
  //
  FContainer.Width:= 300;
  FContainer.Height:= 423;
  FContainer.Fill.Bitmap.Bitmap.LoadFromFile(AssetsManager.RequireFile('rank_bg.png'));
  //
  LTitleImage:= TImage.Create(FContainer);
  LTitleImage.Parent:= FContainer;
  LTitleImage.Name:= 'imgTitle';
  LTitleImage.Align:= TAlignLayout.Top;
  LTitleImage.Margins.Left:= 25;
  LTitleImage.Margins.Right:= 25;
  LTitleImage.Margins.Top:= 30;
  LTitleImage.Height:= 41;
  LTitleImage.Bitmap.LoadFromFile(AssetsManager.RequireFile('rank_title.png'));

  FRankListView:= TPresentedScrollBox.Create(FContainer);
  FRankListView.Parent:= FContainer;
  FRankListView.Name:= 'lsvRank';
  FRankListView.Align:= TAlignLayout.Client;
  FRankListView.AutoHide:= TBehaviorBoolean.True;
  FRankListView.Bounces:= TBehaviorBoolean.True;
  FRankListView.ScrollAnimation:= TBehaviorBoolean.True;
  FRankListView.ScrollDirections:= TScrollDirections.Vertical;
  FRankListView.ShowScrollBars:= False;
  FRankListView.Size.PlatformDefault:= False;
  FRankListView.TouchTracking:= TBehaviorBoolean.True;

  LCloseButton:= TImage.Create(FContainer);
  LCloseButton.Parent:= FContainer;
  LCloseButton.Name:= 'btnClose';
  LCloseButton.Align:= TAlignLayout.Bottom;
  LCloseButton.Margins.Left:= 75;
  LCloseButton.Margins.Right:= 75;
  LCloseButton.Margins.Top:= 5;
  LCloseButton.Margins.Bottom:= 15;
  LCloseButton.Height:= 38;
  LCloseButton.Bitmap.LoadFromFile(AssetsManager.RequireFile('rank_close.png'));
  LCloseButton.OnMouseDown:= TUIEvent.ImageButtonMouseDown;
  LCloseButton.OnMouseUp:= TUIEvent.ImageButtonMouseUp;
  LCloseButton.OnMouseLeave:= TUIEvent.ImageButtonMouseLeave;

  InitRankItems(20);
end;

destructor TRankWindow.Destroy;
begin
  FreeAndNil(FRankItems);
  inherited;
end;

procedure TRankWindow.InitRankItems(const AMax: Integer);
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

function TRankWindow.AddRankItem: TViewListItem;
begin
  Result:= TViewListItem.Create(FContainer);
  Result.Parent:= FRankListView;
  Result.Align:= TAlignLayout.Top;
  Result.Image:= AssetsManager.RequireFile('item_bg.png');
  FRankItems.Add(Result);
end;

procedure TRankWindow.ShowMe;
var
  I: Integer;
  LRank: TArray<string>;
begin
  inherited;

  //temp rank
  SetLength(LRank, 5);
  LRank[0]:= 'gm';
  LRank[1]:= 'doudou';
  LRank[2]:= 'meme';
  LRank[3]:= 'youyou';
  LRank[4]:= 'mario';
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

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  LPackage: TScriptPackage;
  I: Integer;
begin
  FGameStage:= TGameStage.Create(Self);
  FGameStage.OnRender:= DoRender;
  FGameStage.AssetsRoot:= GetMediaPath;
  FGameStage.ScriptRoot:= GetMediaPath + 'script';

  EngineFonts:= TBitmapFonts.Create(FGameStage.Canvas.Device);
  EngineFonts.Canvas:= FGameStage.Canvas;
  FontTahoma:= EngineFonts.AddFromXMLFile(GetMediaPath + 'Tahoma9b.png');
  if FontTahoma = -1 then
  begin
    MessageDlg('Could not load Tahoma font.', TMsgDlgType.mtError, [TMsgDlgBtn.mbOk], 0);
    Application.Terminate;
    Exit;
  end;

  FStrings:= TStringList.Create;

  //right & top
  with TGUISprite.Create(FGameStage.SpriteManager) do
  begin
    Name:= 'btnShowLogin';
    Image:= AssetsManager.Require('head01.png');
    Width:= Image.Width;
    Height:= Image.Height;
    Align:= TAlignMode.amRightTop;
    Margins.Right:= 30;
  end;

  //bottom & center
  with TGUISprite.Create(FGameStage.SpriteManager) do
  begin
    Name:= 'btnShowRank';
    Image:= AssetsManager.Require('rank_btn.png');
    Width:= Image.Width;
    Height:= Image.Height;
    Align:= TAlignMode.amCenterBottom;
    //HitTest:= False;
  end;

  // ui package
  LPackage:= FGameStage.RegScriptPackage(TUIPackage, 'UIPackage');
  if Assigned(LPackage) then
  begin
    FUIPackage:= TUIPackage(LPackage);
    FUIPackage.OwnerForm:= Self;
    FUIPackage.SpriteManager:= FGameStage.SpriteManager;
    // test: make loginwindow by lyt file
    FUIPackage.RegWindow(GetMediaPath+'ui\login.lyt');
    // test: make rankwindow by native
    FUIPackage.RegWindow('frmRank', TRankWindow.Create(nil));
  end;
  // sound package
  LPackage:= FGameStage.RegScriptPackage(TSoundPackage, 'SoundPackage');
  // drive with lua
  FGameStage.DriveWithScript('app.lua', 'InitRunEnvironment', 'Start');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EngineFonts);
  FreeAndNil(FStrings);
  FreeAndNil(FGameStage);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  FGameStage.Resize;
end;

procedure TMainForm.FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
begin
  // Make sure there is nothing in FM canvas cache before using PXL.
  Canvas.Flush;

  // Invoke PXL's multimedia timer, which will call "EngineTiming" to continue drawing on this form with PXL.
  FGameStage.NotifyTick;
end;

procedure TMainForm.DoPrint(const AMsg: string);
begin
  FStrings.Add(AMsg);
end;

procedure TMainForm.DoRender(Sender: TObject);
var
  I: Integer;
begin
  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 4.0),
    'FPS: ' + IntToStr(FGameStage.FrameRate),
    ColorPair($FFFFE887, $FFFF0000));

  EngineFonts[FontTahoma].DrawText(
    Point2f(4.0, 24.0),
    'Technology: ' + FGameStage.FullDeviceTechString,
    ColorPair($FFE8FFAA, $FF12C312));

  for I:= 0 to FStrings.Count -1 do
    EngineFonts[FontTahoma].DrawText(
      Point2f(200.0, 100.0 + (I+1)*20),
      FStrings[I],
      ColorPair($FFE8FFAA, $FF12C312));

  for I:= 0 to Length(FGameStage.Logs) -1 do
    EngineFonts[FontTahoma].DrawText(
      Point2f(200.0, 100.0 + (I+FStrings.Count+1)*20),
      FGameStage.Logs[I],
      ColorPair($FFE8FFAA, $FF12C312));
end;

procedure TMainForm.SysTimerTimer(Sender: TObject);
begin
  MainForm.Invalidate;
end;

end.
