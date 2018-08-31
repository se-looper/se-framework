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

unit se.game.script.package;

interface

uses
  VerySimple.Lua, VerySimple.Lua.Lib;

type
  TScriptPackage = class abstract
  private
    FName: string;
    FLuaState: Lua_State;
  protected
    /// <summary>
    ///   注册函数
    /// </summary>
    /// <param name="ANativeMethodName">
    ///   Delphi方法名
    /// </param>
    /// <param name="ALuaMethodName">
    ///   Lua方法名
    /// </param>
    procedure RegFunction(const ANativeMethodName, ALuaMethodName: string);
    /// <summary>
    ///   注册所有函数
    /// </summary>
    /// <remark>
    ///   这个抽象方法由子类实现
    /// </remark>
    procedure RegFunctions; virtual; abstract;
    /// <summary>
    ///   调用Lua中的公共方法
    /// </summary>
    /// <param name="ALuaMethodName">
    ///   Lua方法名
    /// </param>
    /// <param name="AMsgcode">
    ///   消息编码(Integer)
    /// </param>
    /// <param name="AData">
    ///   数据(string)
    /// </param>
    procedure InvokeLuaMethod(const ALuaMethodName: string;
      const AMsgcode: Integer; const AData: string); overload;
    /// <summary>
    ///   调用Lua中的公共方法
    /// </summary>
    /// <param name="ALuaMethodName">
    ///   Lua方法名
    /// </param>
    /// <param name="AData">
    ///   数据(string)
    /// </param>
    procedure InvokeLuaMethod(const ALuaMethodName: string;
      const AMsgcode: Integer); overload;
    procedure InvokeLuaMethod(const ALuaMethodName: string;
      const AData: string); overload;
  public
    /// <summary>
    ///   包加载方法
    /// </summary>
    /// <remark>
    ///   必须是Public或者Published
    /// </remark>
    function LoadPackage: Integer; virtual;
    /// <summary>
    ///   包名
    /// </summary>
    property Name: string read FName write FName;
    /// <summary>
    ///   Lua脚本上下文
    /// </summary>
    property LuaState: Lua_State read FLuaState write FLuaState;
  end;

  TScriptPackageClass = class of TScriptPackage;

implementation

{ TScriptPackage }

procedure TScriptPackage.RegFunction(const ANativeMethodName,
  ALuaMethodName: string);
begin
  TVerySimpleLua.PushFunction(FLuaState, Self, MethodAddress(ANativeMethodName), ALuaMethodName);
  lua_rawset(FLuaState, -3);
end;

function TScriptPackage.LoadPackage: Integer;
begin
  lua_newtable(FLuaState);
  RegFunctions;
  result:= 1;
end;

procedure TScriptPackage.InvokeLuaMethod(const ALuaMethodName: string;
  const AMsgcode: Integer; const AData: string);
var
  LError: Integer;
begin
  lua_getglobal(FLuaState, MarshaledAString(UTF8String(ALuaMethodName)));
  lua_pushinteger(FLuaState, AMsgcode);
  lua_pushstring(FLuaState, MarshaledAString(UTF8String(AData)));
  LError:= lua_pcall(FLuaState, 2, 0, 0);
end;

procedure TScriptPackage.InvokeLuaMethod(const ALuaMethodName: string;
  const AMsgcode: Integer);
var
  LError: Integer;
begin
  lua_getglobal(FLuaState, MarshaledAString(UTF8String(ALuaMethodName)));
  lua_pushinteger(FLuaState, AMsgcode);
  LError:= lua_pcall(FLuaState, 1, 0, 0);
end;

procedure TScriptPackage.InvokeLuaMethod(const ALuaMethodName, AData: string);
var
  LError: Integer;
begin
  lua_getglobal(FLuaState, MarshaledAString(UTF8String(ALuaMethodName)));
  lua_pushstring(FLuaState, MarshaledAString(UTF8String(AData)));
  LError:= lua_pcall(FLuaState, 1, 0, 0);
end;

end.
