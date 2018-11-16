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

unit se.common.helper;

interface

uses
  System.Classes, System.SysUtils, XSuperObject;

type
  TSuperArrayHelper = class helper for TSuperArray
    class function ParseStream(Stream: TStream; CheckDate: Boolean = True): TSuperArray;
    class function ParseFile(FileName: String; CheckDate: Boolean = True): TSuperArray;
  end;

implementation

{ TSuperArrayHelper }

class function TSuperArrayHelper.ParseFile(FileName: String; CheckDate: Boolean): TSuperArray;
var
  Strm: TFileStream;
begin
  Strm := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  try
    Result := ParseStream(Strm, CheckDate);
  finally
    Strm.Free;
  end;
end;

class function TSuperArrayHelper.ParseStream(Stream: TStream; CheckDate: Boolean): TSuperArray;
var
  Strm: TStringStream;
begin
  Strm := TStringStream.Create;
  try
    Strm.LoadFromStream(Stream);
    Result := TSuperArray.Create(Strm.DataString, CheckDate);
  finally
    Strm.Free;
  end;
end;

end.
