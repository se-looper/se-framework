unit se.game.script.package.ui;

interface

uses
  System.Classes, System.SysUtils, FMX.Forms, FMX.Controls,
  VerySimple.Lua, VerySimple.Lua.Lib,
  se.game.script.package, se.game.sprite, se.game.formfactory;

type
  TUIPackage = class(TScriptPackage)
  private
    FWindowFactory: TWindowFactory;
    FOwnerForm: TForm;
    FSpriteManager: TSpriteManager;
    procedure DoSpriteClick(Sender: TObject);
    procedure DoControlClick(Sender: TObject);
    procedure SetOwnerForm(const Value: TForm);
    procedure RegisterSpriteClickEvent(const ASpriteName: string;
      const AMsgcode: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegFunctions; override;
    function RegWindow(const AName: string; AWindow: TWindow): Boolean;

    property OwnerForm: TForm read FOwnerForm write SetOwnerForm;
    property SpriteManager: TSpriteManager read FSpriteManager write FSpriteManager;
  published
    function ShowMsg(L: lua_State): Integer;

    function registerClickEvent(L: lua_State): Integer;
    function showWindow(L: lua_State): Integer;
    function closeWindow(L: lua_State): Integer;
  end;

implementation

{ TUIPackage }

constructor TUIPackage.Create;
begin
  inherited;
  FWindowFactory:= TWindowFactory.Create;
end;

destructor TUIPackage.Destroy;
begin
  FreeAndNil(FWindowFactory);
  inherited;
end;

procedure TUIPackage.DoControlClick(Sender: TObject);
begin
  if Sender is TControl then
    Self.InvokeLuaMethod('execClick', TControl(Sender).Tag);
end;

procedure TUIPackage.DoSpriteClick(Sender: TObject);
begin
  if Sender is TCustomSprite then
    Self.InvokeLuaMethod('execClick', TCustomSprite(Sender).Tag);
end;

procedure TUIPackage.RegFunctions;
begin
  inherited;
  RegFunction('ShowMsg','ShowMsg');
  RegFunction('registerClickEvent','registerClickEvent');
  RegFunction('showWindow','showWindow');
  RegFunction('closeWindow','closeWindow');
end;

function TUIPackage.RegWindow(const AName: string; AWindow: TWindow): Boolean;
begin
  AWindow.Parent:= FOwnerForm;
  AWindow.OnControlClick:= DoControlClick;
  Result:= FWindowFactory.RegWindow(AName, AWindow);
end;

procedure TUIPackage.SetOwnerForm(const Value: TForm);
begin
  FOwnerForm:= Value;
  FWindowFactory.OwnerForm:= FOwnerForm;
end;

function TUIPackage.registerClickEvent(L: lua_State): Integer;
var
  LFormName, LControlName: string;
  LMsgcode: Integer;
begin
  LFormName:= lua_tostring(L, 2);
  LControlName:= lua_tostring(L, 3);
  LMsgcode:= lua_tointeger(L, 4);
  //
  if Assigned(FOwnerForm) and Assigned(FSpriteManager) and LFormName.Equals(FOwnerForm.Name) then
    RegisterSpriteClickEvent(LControlName, LMsgcode)
  else
    FWindowFactory.RegisterClickEvent(LFormName, LControlName, LMsgcode);
  //
  Result:= 0;
end;

procedure TUIPackage.RegisterSpriteClickEvent(const ASpriteName: string;
  const AMsgcode: Integer);
var
  LSprite: TCustomSprite;
begin
  LSprite:= FSpriteManager.Sprite[ASpriteName];
  if Assigned(LSprite) then
  begin
    LSprite.Tag:= AMsgcode;
    LSprite.OnClick:= DoSpriteClick;
  end;
end;

function TUIPackage.showWindow(L: lua_State): Integer;
var
  LFormName: string;
begin
  LFormName:= lua_tostring(L, 2);
  FWindowFactory.ShowWindow(LFormName);
  Result:= 0;
end;

function TUIPackage.closeWindow(L: lua_State): Integer;
var
  LFormName: string;
begin
  LFormName:= lua_tostring(L, 2);
  FWindowFactory.CloseWindow(LFormName);
  Result:= 0;
end;

function TUIPackage.ShowMsg(L: lua_State): Integer;
var
  LMsg: string;
begin
  LMsg:= lua_tostring(L, 2);
//  MainForm.FStrings.Add(LMsg);
  Result:= 0;
end;

end.
