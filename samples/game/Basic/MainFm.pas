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
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, VerySimple.Lua, VerySimple.Lua.Lib,
  FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.FontGlyphs, FMX.Surfaces, FMX.Platform, FMX.Objects,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PXL.TypeDef,
  PXL.Types, PXL.Fonts,
  se.utils.client, se.game.main, se.game.assetsmanager, se.game.script.package;

type
  TBasicPackage = class(TScriptPackage)
  public
    procedure RegFunctions; override;
  published
    function ShowMsg(L: lua_State): Integer;
  end;

  TMainForm = class(TForm)
    SysTimer: TTimer;
    btnTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure SysTimerTimer(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnTestClick(Sender: TObject);
  private
    FGameMain: TGameMain;
    EngineFonts: TBitmapFonts;
    FontTahoma: Integer;
    ReturnButtonBounds: TRectF;
    FStrings: TStrings;
    FBasicPackage: TBasicPackage;
    procedure DoRender(Sender: TObject);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  System.IOUtils;

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

  //bottom & center
  ReturnButtonBounds.Left  := FGameMain.DisplaySize.X/2 - 150* FGameMain.ScreenScale/2;
  ReturnButtonBounds.Top   := FGameMain.DisplaySize.Y   - 38 * FGameMain.ScreenScale;
  ReturnButtonBounds.Width := 150*FGameMain.ScreenScale;
  ReturnButtonBounds.Height:= 38*FGameMain.ScreenScale;

  FGameMain.RegScriptPackage(TBasicPackage, 'BasicPackage');
  FGameMain.DriveWithScript('app.lua', 'SetPackagePath', 'Start');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(EngineFonts);
  FreeAndNil(FStrings);
  FreeAndNil(FGameMain);
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if PtInRect(ReturnButtonBounds, PointF(X*FGameMain.ScreenScale, Y*FGameMain.ScreenScale)) then
  begin
    btnTest.Visible:= not btnTest.Visible;
  end
  else
  begin
    FStrings.Add(Format('bounds: l=%d,t=%d,r=%d,b=%d',
      [Trunc(ReturnButtonBounds.Left), Trunc(ReturnButtonBounds.Top),
       Trunc(ReturnButtonBounds.Right),  Trunc(ReturnButtonBounds.Bottom)]));
    FStrings.Add(Format('md: x=%d,y=%d',[Trunc(X), Trunc(Y)]));
  end;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  FGameMain.Resize;
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

  FGameMain.Canvas.UseImage(AssetsManager.Require('return_button.png'));
  FGameMain.Canvas.TexQuad(Quad(ReturnButtonBounds.Left,
                            ReturnButtonBounds.Top,
                            ReturnButtonBounds.Width,
                            ReturnButtonBounds.Height),
                       ColorRectWhite);

  FGameMain.Canvas.FrameRect(Quad(ReturnButtonBounds.Left,
                            ReturnButtonBounds.Top,
                            ReturnButtonBounds.Width,
                            ReturnButtonBounds.Height),
                         $FFFF0000);

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

end.
