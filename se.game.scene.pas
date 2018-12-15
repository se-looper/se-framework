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

unit se.game.scene;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, FMX.Types,
  PXL.Types, se.game.types, se.game.font.types, se.game.assetsmanager,
  se.game.sprite;

type
  TSceneManager = class
  private
    FSpriteManager: TSpriteManager;
    FOnPrint: TNotifyInfoEvent;
    FActive: TSceneData;
    procedure MakeLayout(const ALayoutItem: TSceneData.TLayoutItem;
      const AParent: TCustomSprite; const AGroupName: string);
  public
    constructor Create(const ASpriteManager: TSpriteManager);
    destructor Destroy; override;

    function Switch(const ASceneName: string): Boolean; overload;
    function Switch(const AScene: TSceneData): Boolean; overload;

    // –≈œ¢¥Ú”°
    property OnPrint: TNotifyInfoEvent read FOnPrint write FOnPrint;
    //
    property Active: TSceneData read FActive;
  end;

implementation

{ TSceneManager }

constructor TSceneManager.Create(const ASpriteManager: TSpriteManager);
begin
  inherited Create;
  FSpriteManager:= ASpriteManager;
  FActive:= nil;
end;

destructor TSceneManager.Destroy;
begin
  FSpriteManager:= nil;
  FActive:= nil;
  inherited;
end;

procedure TSceneManager.MakeLayout(const ALayoutItem: TSceneData.TLayoutItem;
  const AParent: TCustomSprite; const AGroupName: string);
var
  I: Integer;
  LSprite: TGUISprite;
begin
  LSprite:= TGUISprite.Create(FSpriteManager);
  if Assigned(AParent) then
    LSprite.Parent:= AParent;
  with LSprite do
  begin
    GroupName:= AGroupName;
    Name:= ALayoutItem.Name;
    Width:= ALayoutItem.Width;
    Height:= ALayoutItem.Height;
    Image:= AssetsManager.RequireImage(ALayoutItem.ImageName);
    Align:= ALayoutItem.Align;
    HitTest:= ALayoutItem.HitTest;
    ZOrder:= ALayoutItem.ZOrder;
    Margins.Left:= ALayoutItem.Margins.Left;
    Margins.Top:= ALayoutItem.Margins.Top;
    Margins.Right:= ALayoutItem.Margins.Right;
    Margins.Bottom:= ALayoutItem.Margins.Bottom;
    if not ALayoutItem.Text.IsNull then
    begin
      Text:= TTextSprite.Create(FSpriteManager);
      Text.GroupName:= AGroupName;
      Text.Name:= ALayoutItem.Text.Name;
      Text.Text:= ALayoutItem.Text.Text;
      Text.FontSize:= ALayoutItem.Text.Font.Size;
      Text.FontColor:= ALayoutItem.Text.Font.Color;
      Text.TextAlign:= ALayoutItem.Text.TextAlign;
    end;
  end;
  //
  for I:= 0 to ALayoutItem.Childs.Count -1 do
    MakeLayout(ALayoutItem.Childs[I], LSprite, AGroupName);
end;

function TSceneManager.Switch(const ASceneName: string): Boolean;
var
  LScene: TSceneData;
begin
  LScene:= AssetsManager.RequireScene(ASceneName);
  Result:= Self.Switch(LScene);
end;

function TSceneManager.Switch(const AScene: TSceneData): Boolean;
var
  I: Integer;
begin
  if not Assigned(AScene) then
    Exit(False);
  //
  FSpriteManager.BeginUpdate;
  try
    if Assigned(FActive) then
      FSpriteManager.DeadGroup(FActive.Name + '.scene');
    //
    for I:= 0 to AScene.Count -1 do
      MakeLayout(AScene.Items[I], nil, AScene.Name + '.scene');
    FActive:= AScene;
    Result:= True;
  finally
    FSpriteManager.EndUpdate;
  end;
end;

end.
