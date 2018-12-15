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

unit se.game.font.types;

interface

uses
  System.Classes, System.SysUtils, System.Math;

type
  TFontTypes = record
  public const
    DefaultFontName       = 'droid';
    TabSize               = 100;
    FontPadding           = 10;
    FontSpread            = 14;
    FontRescale           = 3;
    FontInvScale          = 1.0 / FontRescale;
    OptimumFontSize       = 30; //最佳字体大小(大于这个大小有可能出现锯齿)
    DefaultFontPageWidth  = 256 * FontRescale;
    DefaultFontPageHeight = 512 * FontRescale;
    NullChar              = #0;
    NewRowChar            = #13;
    FontEffectBegin       = '<';
    FontEffectEnd         = '>';
    DefaultMaxChar        = 256;
    DefaultCharID         = 69; //'E'
    SpaceChar             = #32;
    SpaceCharID           = 32;
    Rad                   = 0.017453292519; // Pi/180
    Deg                   = 57.29577951308; // 180/Pi
    SpaceWidth            = 4;
    SpaceHeight           = 4;
    BoldOffset            = 4;
    PresetText            = '1234567890'
                          + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
                          + 'abcdefghijklmnopqrstuvwxyz'
                          + ':.!@,?][;"|~#$%^&)(`-+*/=';
  public const
    CacheRegionWidth      = 64;
    CacheRegionHeight     = 64;
    CacheRegionRows       = 32;
    CacheRegionCols       = 32;
    CachePageWidth        = CacheRegionWidth  * CacheRegionCols;
    CachePageHeight       = CacheRegionHeight * CacheRegionRows;
    CachePageRegions      = CacheRegionRows   * CacheRegionCols;
  public const
    STBTT_PLATFORM_ID_UNICODE   = 0;
    STBTT_PLATFORM_ID_MAC       = 1;
    STBTT_PLATFORM_ID_ISO       = 2;
    STBTT_PLATFORM_ID_MICROSOFT = 3;

    STBTT_MS_EID_SYMBOL         = 0;
    STBTT_MS_EID_UNICODE_BMP    = 1;
    STBTT_MS_EID_SHIFTJIS       = 2;
    STBTT_MS_EID_UNICODE_FULL   = 10;

    FIXSHIFT  = 10;
    FIX       = (1 shl FIXSHIFT);
    FIXMASK   = (FIX-1);

    STBTT_vmove  = 1;
    STBTT_vline  = 2;
    STBTT_vcurve = 3;
  public type
    TFileHeader = array[1..4] Of System.UTF8Char;

    PtrInt = Integer;
    PtrUInt = Cardinal;

    PByteArray= ^ByteArray;
    ByteArray = array[0..1024*64] Of Byte;

    PShortIntArray=^ShortIntArray;
    ShortIntArray = array[0..1024*64] Of ShortInt;

    PWordArray=^WordArray;
    WordArray = array[0..1024*64] Of Word;

    PSmallIntArray=^SmallIntArray;
    SmallIntArray = array[0..1024*64] Of SmallInt;

    PIntegerArray=^IntegerArray;
    IntegerArray = array[0..1024*64] Of Integer;

    PCardinalArray=^CardinalArray;
    CardinalArray = array[0..1024*64] Of Cardinal;

    PSingleArray=^SingleArray;
    SingleArray = array[0..1024*64] Of Single;

    PPointerArray=^ PointerArray;
    PointerArray = array[0..65534] Of Pointer;

    PBooleanArray=^BooleanArray;
    BooleanArray = array[0..1024*64] Of Boolean;
  end;

implementation

end.
