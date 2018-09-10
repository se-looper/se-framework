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

unit se.game.sound;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TSoundPlayer = class abstract
  protected
    procedure DoPrepareMusic(const AFileName: string); virtual; abstract;
    procedure DoPlayMusic(const AFileName: string); virtual; abstract;
    procedure DoStopMusic; virtual; abstract;

    procedure DoPrepareAmbient(const AFileName: string); virtual; abstract;
    procedure DoPlayAmbient(const AFileName: string); virtual; abstract;
    procedure DoStopAmbient; virtual; abstract;

    procedure DoPrepareSound(const AFileName: string); virtual; abstract;
    procedure DoPlaySound(const AFileName: string); virtual; abstract;
    procedure DoStopSound(const AFileName: string); virtual; abstract;

    procedure DoStopAll; virtual; abstract;

    function Playing(const AFileName: string): Boolean; virtual; abstract;

    function SaveSoundAsLocalFile(const AFileName: string): string; virtual;
    function SoundReadyOk(const AFileName: string): Boolean; virtual;
  public
    constructor Create(const ATempPath: string); virtual;
    destructor Destroy; override;

    procedure PrepareMusic(const AFileName: string);
    procedure PlayMusic(const AFileName: string);
    procedure StopMusic;

    procedure PrepareAmbient(const AFileName: string);
    procedure PlayAmbient(const AFileName: string);
    procedure StopAmbient;

    procedure PrepareSound(const AFileName: string);
    procedure PlaySound(const AFileName: string);
    procedure StopSound(const AFileName: string);

    procedure StopAll;

    procedure Progress(const ADeltaTime: Double); virtual;
  end;

  TSoundPlayerClass = class of TSoundPlayer;

implementation

{ TSoundPlayer }

constructor TSoundPlayer.Create(const ATempPath: string);
begin

end;

destructor TSoundPlayer.Destroy;
begin

  inherited;
end;

procedure TSoundPlayer.PlayAmbient(const AFileName: string);
begin

end;

procedure TSoundPlayer.PlayMusic(const AFileName: string);
begin

end;

procedure TSoundPlayer.PlaySound(const AFileName: string);
begin

end;

procedure TSoundPlayer.PrepareAmbient(const AFileName: string);
begin

end;

procedure TSoundPlayer.PrepareMusic(const AFileName: string);
begin

end;

procedure TSoundPlayer.PrepareSound(const AFileName: string);
begin

end;

procedure TSoundPlayer.Progress(const ADeltaTime: Double);
begin

end;

function TSoundPlayer.SaveSoundAsLocalFile(const AFileName: string): string;
begin

end;

function TSoundPlayer.SoundReadyOk(const AFileName: string): Boolean;
begin

end;

procedure TSoundPlayer.StopAll;
begin

end;

procedure TSoundPlayer.StopAmbient;
begin

end;

procedure TSoundPlayer.StopMusic;
begin

end;

procedure TSoundPlayer.StopSound(const AFileName: string);
begin

end;

end.
