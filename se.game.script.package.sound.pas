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

unit se.game.script.package.sound;

interface

uses
  System.Classes, System.SysUtils, System.IOUtils, FMX.Forms, FMX.Controls,
  VerySimple.Lua, VerySimple.Lua.Lib,
  se.game.types, se.game.script.package, se.game.sound, se.game.sound.bass;

type
  TSoundPackage = class(TScriptPackage)
  private
    FSoundPlayer: TSoundPlayer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegFunctions; override;
  published
    /// <summary>
    ///   ²¥·Å
    /// </summary>
    function play(L: lua_State): Integer;
    /// <summary>
    ///   ÔÝÍ£
    /// </summary>
    function pause(L: lua_State): Integer;
    /// <summary>
    ///   Í£Ö¹
    /// </summary>
    function stop(L: lua_State): Integer;
  end;

implementation

{ TSoundPackage }

constructor TSoundPackage.Create;
begin
  inherited;
  FSoundPlayer:= TBassSoundPlayer.Create(TPath.GetTempPath);
end;

destructor TSoundPackage.Destroy;
begin
  FreeAndNil(FSoundPlayer);
  inherited;
end;

procedure TSoundPackage.RegFunctions;
begin
  inherited;
  RegFunction('play','play');
  RegFunction('pause','pause');
  RegFunction('stop','stop');
end;

function TSoundPackage.play(L: lua_State): Integer;
var
  LSoundFileName: string;
begin
  LSoundFileName:= lua_tostring(L, 2);
  //test
  Self.InvokeLuaMethod('OutputDebugString', 'play: ' + LSoundFileName);
end;

function TSoundPackage.pause(L: lua_State): Integer;
var
  LSoundFileName: string;
begin
  LSoundFileName:= lua_tostring(L, 2);
  //test
  Self.InvokeLuaMethod('OutputDebugString', 'pause: ' + LSoundFileName);
end;

function TSoundPackage.stop(L: lua_State): Integer;
var
  LSoundFileName: string;
begin
  LSoundFileName:= lua_tostring(L, 2);
  //test
  Self.InvokeLuaMethod('OutputDebugString', 'stop: ' + LSoundFileName);
end;

end.
