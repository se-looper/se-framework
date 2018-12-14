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

unit se.game.script.package.net;

interface

uses
  System.Classes, System.SysUtils, FMX.Forms, FMX.Controls,
  VerySimple.Lua, VerySimple.Lua.Lib,
  se.game.types, se.game.script.package;

type
  TNetPackage = class(TScriptPackage)
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegFunctions; override;
  published
    /// <summary>
    ///   «Î«Û
    /// </summary>
    function request(L: lua_State): Integer;
  end;

implementation

{ TNetPackage }

constructor TNetPackage.Create;
begin
  inherited;

end;

destructor TNetPackage.Destroy;
begin

  inherited;
end;

procedure TNetPackage.RegFunctions;
begin
  inherited;
  RegFunction('request','request');
end;

function TNetPackage.request(L: lua_State): Integer;
var
  LMsgcode: Integer;
  LBody: string;
begin
  LMsgcode:= lua_tointeger(L, 2);
  LBody:= lua_tostring(L, 3);
  // test
  Self.InvokeLuaMethod('OutputDebugString', 'request: ' + LBody);
  Self.InvokeLuaMethod('execNetMsg', LMsgcode+1, '{"result":true,"data":"test1121"}');
  //
  Result:= 0;
end;

end.
