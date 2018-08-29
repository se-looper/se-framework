unit editor.configs;

interface

uses
  System.Classes, System.SysUtils, System.IniFiles, System.Generics.Collections,
  System.Math;

type
  TConfigs = class
  private
    class var FInstance: TConfigs;
    class constructor Create;
    class destructor Destroy;
  private
    FIniFile: TMemIniFile;
    FAssetRoot, FSaveDir: string;
    function GetAssetRoot: string;
    procedure SetAssetRoot(const Value: string);
    function GetSaveDir: string;
    procedure SetSaveDir(const Value: string);
    function GetExtendFloat(const AKey: string): Single;
    function GetExtendInteger(const AKey: string): Integer;
    function GetExtendString(const AKey: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Apply;

    property AssetRoot: string read GetAssetRoot write SetAssetRoot;
    property SaveDir: string read GetSaveDir write SetSaveDir;

    property ExtendString[const AKey: string]: string read GetExtendString;
    property ExtendInteger[const AKey: string]: Integer read GetExtendInteger;
    property ExtendFloat[const AKey: string]: Single read GetExtendFloat;
  end;

  function Configs: TConfigs;

implementation

function Configs: TConfigs;
begin
  Exit(TConfigs.FInstance);
end;

{ TConfigs }

class constructor TConfigs.Create;
begin
  FInstance:= TConfigs.Create;
end;

class destructor TConfigs.Destroy;
begin
  FreeAndNil(FInstance);
end;

procedure TConfigs.Apply;
begin
  FIniFile.UpdateFile;
end;

constructor TConfigs.Create;
var
  LAppName, LAppPath: string;
begin
  LAppName:= ParamStr(0);
  LAppPath:= ExtractFilePath(LAppName);
  LAppPath:= IncludeTrailingPathDelimiter(LAppPath);
  FIniFile:= TMemIniFile.Create(ChangeFileExt(LAppName,'.cfg'));
  FAssetRoot:= FIniFile.ReadString('system', 'assetroot', LAppPath+'WorkPath\data');
  FSaveDir:= FIniFile.ReadString('system', 'savedir', LAppPath+'WorkPath\cofig\ui');
end;

destructor TConfigs.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

function TConfigs.GetExtendFloat(const AKey: string): Single;
begin
  Result:= FIniFile.ReadFloat('Extend', AKey, 0);
end;

function TConfigs.GetExtendInteger(const AKey: string): Integer;
begin
  Result:= FIniFile.ReadInteger('Extend', AKey, 0);
end;

function TConfigs.GetExtendString(const AKey: string): string;
begin
  Result:= FIniFile.ReadString('Extend', AKey, '');
end;

function TConfigs.GetAssetRoot: string;
begin
  Result:= IncludeTrailingPathDelimiter(FAssetRoot);
end;

procedure TConfigs.SetAssetRoot(const Value: string);
begin
  if FAssetRoot.Equals(Value) then Exit;
  FAssetRoot := Value;
  FIniFile.WriteString('system', 'assetroot', FAssetRoot);
end;

function TConfigs.GetSaveDir: string;
begin
  Result:= IncludeTrailingPathDelimiter(FSaveDir);
end;

procedure TConfigs.SetSaveDir(const Value: string);
begin
  if FSaveDir.Equals(Value) then Exit;
  FSaveDir := Value;
  FIniFile.WriteString('system', 'savedir', FSaveDir);
end;

end.
