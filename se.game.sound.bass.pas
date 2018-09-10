{******************************************************************************}
{                                                                              }
{       SE Network Development Framework                                       }
{                                                                              }
{       Copyright (c) 2018 looper(2031056602@qq.com)                           }
{                                                                              }
{       Refer to zSound (https://github.com/PassByYou888/zSound)               }
{                                                                              }
{       Source: https://github.com/looper/se-framework                         }
{       Homepage: http://www.asphyre.cn                                        }
{                                                                              }
{******************************************************************************}

unit se.game.sound.bass;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  Bass,
  se.game.sound;

type
  TBasSoundStyle = (ssUnknown, ssMusic, ssAmbient, ssSound);
  TBassSoundItem = class
  public
    FOwner: TSoundPlayer;
    FName: string;
    FHandle, FChannel: Cardinal;
    FStyle: TBasSoundStyle;
  public
    constructor Create(const AOwner: TSoundPlayer);
    destructor Destroy; override;

    property Name: string read FName write FName;
    property Handle: Cardinal read FHandle write FHandle;
    property Style: TBasSoundStyle read FStyle write FStyle;
    property Channel: Cardinal read FChannel write FChannel;
  end;

  TBassSoundPlayer = class(TSoundPlayer)
  protected
    FSoundList: TObjectDictionary<string, TBassSoundItem>;

    procedure DoPrepareMusic(const AFileName: string); override;
    procedure DoPlayMusic(const AFileName: string); override;
    procedure DoStopMusic; override;

    procedure DoPrepareAmbient(const AFileName: string); override;
    procedure DoPlayAmbient(const AFileName: string); override;
    procedure DoStopAmbient; override;

    procedure DoPrepareSound(const AFileName: string); override;
    procedure DoPlaySound(const AFileName: string); override;
    procedure DoStopSound(const AFileName: string); override;

    procedure DoStopAll; override;

    function Playing(const AFileName: string): Boolean; override;

    function SaveSoundAsLocalFile(const AFileName: string): string; override;
    function SoundReadyOk(const AFileName: string): Boolean; override;
  public
    constructor Create(const ATempPath: string); override;
    destructor Destroy; override;

    procedure Progress(const ADeltaTime: Double); override;
  end;

implementation

{ TBassSoundItem }

constructor TBassSoundItem.Create(const AOwner: TSoundPlayer);
begin

end;

destructor TBassSoundItem.Destroy;
begin

  inherited;
end;

{ TBassSoundPlayer }

constructor TBassSoundPlayer.Create(const ATempPath: string);
begin
  inherited;

end;

destructor TBassSoundPlayer.Destroy;
begin

  inherited;
end;

procedure TBassSoundPlayer.DoPlayAmbient(const AFileName: string);
begin
  inherited;

end;

procedure TBassSoundPlayer.DoPlayMusic(const AFileName: string);
begin
  inherited;

end;

procedure TBassSoundPlayer.DoPlaySound(const AFileName: string);
begin
  inherited;

end;

procedure TBassSoundPlayer.DoPrepareAmbient(const AFileName: string);
begin
  inherited;

end;

procedure TBassSoundPlayer.DoPrepareMusic(const AFileName: string);
begin
  inherited;

end;

procedure TBassSoundPlayer.DoPrepareSound(const AFileName: string);
begin
  inherited;

end;

procedure TBassSoundPlayer.DoStopAll;
begin
  inherited;

end;

procedure TBassSoundPlayer.DoStopAmbient;
begin
  inherited;

end;

procedure TBassSoundPlayer.DoStopMusic;
begin
  inherited;

end;

procedure TBassSoundPlayer.DoStopSound(const AFileName: string);
begin
  inherited;

end;

function TBassSoundPlayer.Playing(const AFileName: string): Boolean;
begin

end;

procedure TBassSoundPlayer.Progress(const ADeltaTime: Double);
begin
  inherited;

end;

function TBassSoundPlayer.SaveSoundAsLocalFile(const AFileName: string): string;
begin

end;

function TBassSoundPlayer.SoundReadyOk(const AFileName: string): Boolean;
begin

end;

end.
