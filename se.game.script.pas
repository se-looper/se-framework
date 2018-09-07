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

unit se.game.script;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  VerySimple.Lua, VerySimple.Lua.Lib, se.game.script.package;

type
  TScriptSystem = class(TVerySimpleLua)
  private
    /// <summary>
    ///   包列表
    /// </summary>
    FPackageList: TObjectList<TScriptPackage>;
    /// <summary>
    ///   根据包名获取包列表中的索引
    /// </summary>
    function IndexOfPackageName(const AName: string): Integer;
    /// <summary>
    ///   调用Lua中的方法, 两个参数
    /// </summary>
    procedure InvokeLuaMethod(const ALuaMethodName: string;
      const AData1, AData2: string); overload;
    /// <summary>
    ///   调用Lua中的方法, 一个参数
    /// </summary>
    procedure InvokeLuaMethod(const ALuaMethodName: string;
      const AData: string); overload;
    /// <summary>
    ///   调用Lua中的方法, 无参数
    /// </summary>
    procedure InvokeLuaMethod(const ALuaMethodName: string); overload;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    ///   在这个方法中注册包对象
    /// </summary>
    procedure Open; override;
    /// <summary>
    ///   对外开放的包注册方法, 实际上只是把包加入到FPackageList中
    ///   这个方法目前必须在Run之前调用才有效
    /// </summary>
    function RegPackage(const AClass: TScriptPackageClass;
      const AName: string): TScriptPackage;
    /// <summary>
    ///   执行某个lua脚本文件
    /// </summary>
    /// <param name="AInitRunEnvironmentMethodName">
    ///   1、在这里设置package.path
    ///   2、在这个lua方法中require各个模块
    ///   3、必须有这个方法而且必须是公共方法
    /// </param>
    /// <param name="AStartMethodName">
    ///   启动入口方法名, 必须有这个方法而且必须是公共方法
    /// </param>
    /// <remark>
    ///   这个只能在启动时调用一次
    /// </remark>
    procedure Run(const AFileName, AInitRunEnvironmentMethodName,
      AStartMethodName: string);
  end;

implementation

{ TScriptSystem }

procedure TScriptSystem.InvokeLuaMethod(const ALuaMethodName, AData1, AData2: string);
var
  LError: Integer;
begin
  lua_getglobal(Self.LuaState, MarshaledAString(UTF8String(ALuaMethodName)));
  lua_pushstring(Self.LuaState, MarshaledAString(UTF8String(AData1)));
  lua_pushstring(Self.LuaState, MarshaledAString(UTF8String(AData2)));
  LError:= lua_pcall(Self.LuaState, 2, 0, 0);
end;

procedure TScriptSystem.InvokeLuaMethod(const ALuaMethodName, AData: string);
var
  LError: Integer;
begin
  lua_getglobal(Self.LuaState, MarshaledAString(UTF8String(ALuaMethodName)));
  lua_pushstring(Self.LuaState, MarshaledAString(UTF8String(AData)));
  LError:= lua_pcall(Self.LuaState, 1, 0, 0);
end;

procedure TScriptSystem.InvokeLuaMethod(const ALuaMethodName: string);
var
  LError: Integer;
begin
  lua_getglobal(Self.LuaState, MarshaledAString(UTF8String(ALuaMethodName)));
  LError:= lua_pcall(Self.LuaState, 0, 0, 0);
end;

constructor TScriptSystem.Create;
begin
  inherited Create;
  FPackageList:= TObjectList<TScriptPackage>.Create;
end;

destructor TScriptSystem.Destroy;
begin
  FreeAndNil(FPackageList);
  inherited;
end;

function TScriptSystem.IndexOfPackageName(const AName: string): Integer;
var
  I: Integer;
begin
  Result:= -1;
  for I:= 0 to FPackageList.Count -1 do
    if FPackageList[I].Name = AName then
      Exit(I);
end;

procedure TScriptSystem.Open;
var
  I: Integer;
begin
  inherited;
  for I:= 0 to FPackageList.Count -1 do
  begin
    FPackageList[I].LuaState:= Self.LuaState;
    RegisterPackage(FPackageList[I].Name, FPackageList[I], 'LoadPackage');
  end;
end;

function TScriptSystem.RegPackage(const AClass: TScriptPackageClass;
  const AName: string): TScriptPackage;
var
  LIndex: Integer;
begin
  if AName = '' then Exit(nil);
  Result:= AClass.Create;
  Result.Name:= AName;
  LIndex:= IndexOfPackageName(AName);
  if LIndex >= 0 then FPackageList.Delete(LIndex);
  FPackageList.Add(Result);
end;

procedure TScriptSystem.Run(const AFileName, AInitRunEnvironmentMethodName,
  AStartMethodName: string);
begin
  Self.DoFile(AFileName);
  InvokeLuaMethod(AInitRunEnvironmentMethodName, Self.FilePath, Self.LibraryPath);
  InvokeLuaMethod(AStartMethodName);
end;

end.