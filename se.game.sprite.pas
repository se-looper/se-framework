{******************************************************************************}
{                                                                              }
{       SE Network Development Framework                                       }
{                                                                              }
{       Copyright (c) 2018 looper(2031056602@qq.com)                           }
{                                                                              }
{       Source: https://github.com/looper/se-framework                         }
{       Homepage: http://www.asphyre.cn                                        }
{                                                                              }
{       Original author in Asphyre : DraculaLin (2014)                         }
{       Modified version in PXL : looper (2018)                                }
{                                                                              }
{******************************************************************************}

unit se.game.sprite;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Math,
  System.Types, System.UITypes, System.Generics.Defaults,
  FMX.Types, FMX.Forms,
  PXL.Types, se.utils.client,
  se.game.types, se.game.helper, se.game.assetsmanager,
  se.game.font.types, se.game.font, se.game.font.classes;

type
  TSpriteManager = class;
  TCustomSpriteClass = class of TCustomSprite;

  TCustomSprite = class
  private
    FParent: TCustomSprite;
    FChildList: TList<TCustomSprite>;
    FDeaded: Boolean;
    FMargins: TBounds;
    FAlign: TAlignMode;
    procedure DoMarginsChanged(Sender: TObject);
    procedure DoLayoutChanged;
  private
    FName: string;
    FImage: TEngineImage;
    FWidth, FHeight, FZOrder, FTag, FPatternIndex: Integer;
    FX, FY, FDrawX, FDrawY: Single;
    FBlendMode: TEngineBlendingEffect;
    FMoveable, FTruncDraw, FVisible: Boolean;
    FOffset: TPoint2f;
    FCollidePos: TPoint2f;
    FCollideRadius: Integer;
    FCollideRect: TFloatRect;
    FCollideQuadrangle: TQuad;
    FCollidePolygon: TPolygon;
    FCollideMode: TCollideMode;
    FCollisioned: Boolean;
    FCanFocus, FFocused, FHitTest: Boolean;
    FFixedLayout: Boolean;
    FGroupName: string;
    procedure SetParent(const Value: TCustomSprite);
    procedure SetImage(const Value: TEngineImage);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetZOrder(const Value: Integer);
    procedure SetAlign(const Value: TAlignMode);
    procedure SetGroupName(const Value: string);
    function GetWorldX: Single;
    function GetWorldY: Single;
    function GetPatternSize: TPoint2i;
    function GetPatternCount: Integer;
  private
    FZOrderAutoSort: Boolean;
    procedure Render;
    procedure Add(const ASprite: TCustomSprite);
    procedure Remove(const ASprite: TCustomSprite);
    procedure ResetDrawOrder;
    procedure SetFocus(const AFocused: Boolean);
    function SpriteAtPos(const X, Y: Integer; const ACheckVisible,
      ACheckHitTest: Boolean): TCustomSprite;
  private
    FOnClick, FOnDblClick, FOnMouseLeave: TNotifyEvent;
    FOnMouseDown, FOnMouseUp: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseWheel: TMouseWheelEvent;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
  protected
    FManager: TSpriteManager;
    procedure DoDraw; virtual;
    procedure DoMove(const AMoveCount: Single); virtual;
    procedure DoCollision(const ASprite: TCustomSprite); virtual;
    function  GetBoundsRect: TIntRect; virtual;
    procedure Resize; virtual;
    // 对齐方式
    property Align: TAlignMode read FAlign write SetAlign;
    // 边缘
    property Margins: TBounds read FMargins;
  public
    constructor Create(const AManager: TSpriteManager); virtual;
    destructor Destroy; override;

    // 批量操作前锁定<禁止子列表ZOrder排序>
    procedure BeginUpdate;
    // 批量操作前解锁<排序>
    procedure EndUpdate;

    // 死亡
    procedure Dead;
    // 移动
    procedure Move(const AMoveCount: Single);
    // 碰撞检测
    procedure Collision(const AOther: TCustomSprite); overload; virtual;
    procedure Collision; overload; virtual;
  public
    // 父级精灵
    property Parent: TCustomSprite read FParent write SetParent;
    // 名称
    property Name: string read FName write FName;
    // 图片
    property Image: TEngineImage read FImage write FImage;
    // 坐标
    property X: Single read FX write SetX;
    property Y: Single read FY write SetY;
    // 世界坐标
    property WorldX: Single read GetWorldX;
    property WorldY: Single read GetWorldY;
    // 渲染顺序
    property ZOrder: Integer read FZOrder write SetZOrder;
    // 宽度
    property Width: Integer read FWidth write SetWidth;
    // 高度
    property Height: Integer read FHeight write SetHeight;
    // 偏移
    property Offset: TPoint2f read FOffset write FOffset;
    // 混合模式
    property BlendMode: TEngineBlendingEffect read FBlendMode write FBlendMode;
    // 是否可移动
    property Moveable: Boolean read FMoveable write FMoveable;
    // 是否对坐标进行Trunc取整
    property TruncDraw: Boolean read FTruncDraw write FTruncDraw;
    // 是否显示
    property Visible: Boolean read FVisible write FVisible;
    // 图块索引
    property PatternIndex: Integer read FPatternIndex write FPatternIndex;
    // 图块大小
    property PatternSize: TPoint2i read GetPatternSize;
    // 图块数量
    property PatternCount: Integer read GetPatternCount;
    // Tag
    property Tag: Integer read FTag write FTag;
    // 碰撞相关属性
    property CollideMode: TCollideMode read FCollideMode write FCollideMode;
    property CollidePos: TPoint2f read FCollidePos write FCollidePos;
    property CollideRadius: Integer read FCollideRadius write FCollideRadius;
    property CollideRect: TFloatRect read FCollideRect write FCollideRect;
    property CollideQuadrangle: TQuad read FCollideQuadrangle write FCollideQuadrangle;
    property CollidePolygon: TPolygon read FCollidePolygon write FCollidePolygon;
    property Collisioned: Boolean read FCollisioned write FCollisioned;
    // 包围盒
    property BoundsRect: TIntRect read GetBoundsRect;
    // 是否可以获得焦点
    property CanFocus: Boolean read FCanFocus write FCanFocus;
    // 是否可以命中
    property HitTest: Boolean read FHitTest write FHitTest;
    // 是否固定位置(位置不根据世界坐标改变而改变, 通常用于GUI对象)
    property FixedLayout: Boolean read FFixedLayout write FFixedLayout;
    // 组名称
    property GroupName: string read FGroupName write SetGroupName;
  public
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
  end;

  TSprite = class(TCustomSprite)
    FColor: TFloatColor;
    FAngle: Single;
    FAngle360: Integer;
    FScale: TPoint2f;
    FDrawMode: TDrawMode;
    FTransformQuad: TQuad;
    FMirror, FFlip, FCentered, FSelected, FCollisionable: Boolean;
    procedure SetAngle360(Value: Integer);
    procedure SetSelected(const Value: Boolean);
  protected
    procedure DoDraw; override;
  public
    constructor Create(const AManager: TSpriteManager); override;
    destructor Destroy; override;

    // 颜色
    property Color: TFloatColor read FColor write FColor;
    // 角度
    property Angle: Single read FAngle write FAngle;
    // 弧度
    property Angle360: Integer read FAngle360 write SetAngle360;
    // 比例
    property Scale: TPoint2f read FScale write FScale;
    // 渲染模式
    property DrawMode: TDrawMode read FDrawMode write FDrawMode;
    // DrawMode=dmTransform时, 需要设置此属性
    property TransformQuad: TQuad read FTransformQuad write FTransformQuad;
    // 是否居中
    property Centered: Boolean read FCentered write FCentered;
    // 是否水平镜像
    property Mirror: Boolean read FMirror write FMirror;
    // 是否垂直翻转
    property Flip: Boolean read FFlip write FFlip;
    // 是否选中
    property Selected: Boolean read FSelected write SetSelected;
    // 是否可碰撞
    property Collisionable: Boolean read FCollisionable write FCollisionable;
  end;

  //动画精灵
  TAnimatedSprite = class(TSprite)
  private
    FFrameStart, FFrameAmount: Integer;
    FFrameIndex: Single;
    FSpeed: Single;
    FPlayMode: TAnimPlayMode;
    FLooped, FActive: Boolean;
    FPlayFlag1, FPlayFlag2: Boolean;
    procedure SetFrameStart(const Value: Integer);
    function GetEnded: Boolean;
  protected
    procedure SetActive(const Value: Boolean); virtual;
    procedure DoStart; virtual;
    procedure DoEnd; virtual;
    procedure DoMove(const AMoveCount: Single); override;
  public
    constructor Create(const AManager: TSpriteManager); override;

    procedure Play(const AImage: TEngineImage;
                   const AFrameStart, AFrameAmount: Integer;
                   const ASpeed: Single;
                   const ALooped, AMirror, AActive: Boolean;
                   const APlayMode: TAnimPlayMode = pmForward); overload; virtual;
    procedure Play(const AImage: TEngineImage;
                   const AFrameStart, AFrameAmount: Integer;
                   const ASpeed: Single;
                   const ALooped: Boolean;
                   const APlayMode: TAnimPlayMode = pmForward); overload; virtual;

    // 起始帧
    property FrameStart: Integer read FFrameStart write SetFrameStart;
    // 总帧数
    property FrameAmount: Integer read FFrameAmount write FFrameAmount;
    // 当前帧
    property FrameIndex: Single read FFrameIndex write FFrameIndex;
    // 播放速度
    property Speed: Single read FSpeed write FSpeed;
    // 播放模式
    property PlayMode: TAnimPlayMode read FPlayMode write FPlayMode;
    // 是否循环播放
    property Looped: Boolean read FLooped write FLooped;
    // 是否激活
    property Active: Boolean read FActive write SetActive;
    // 是否已结束
    property Ended: Boolean read GetEnded;
  end;

  //文本精灵
  TTextSprite = class(TSprite)
  private
    FFont: TFontInstance;
    FFontStyle: TFontCharStyle;
    FFontRenderer: TFontInstance.TFontRenderer;
    FFontSizeScale: Single;
  private
    FText: string;
    FFontSize: Word;
    FFontName: string;
    FFontColor: TIntColor;
    FTextAlign: TTextAlignMode;
    procedure SetText(const Value: string);
    procedure SetFontName(const Value: string);
    procedure SetFontColor(const Value: TIntColor);
    procedure SetFontSize(const Value: Word);
  protected
    procedure Resize; override;
    procedure DoDraw; override;
  public
    constructor Create(const AManager: TSpriteManager); override;

    property X: Single read FX;
    property Y: Single read FY;
  published
    property Text: string read FText write SetText;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontColor: TIntColor read FFontColor write SetFontColor;
    property TextAlign: TTextAlignMode read FTextAlign write FTextAlign;
    property Name;
    property Align;
    property Margins;
    property ZOrder;
  end;

  //GUI精灵
  TGUISprite = class(TAnimatedSprite)
  private
    FText: TTextSprite;
    procedure SetText(const Value: TTextSprite);
  protected
    procedure DoDraw; override;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(const AManager: TSpriteManager); override;

    property X: Single read FX;
    property Y: Single read FY;
  published
    property Name;
    property Image;
    property Align;
    property Margins;
    property HitTest;
    property ZOrder;
    property Text: TTextSprite read FText write SetText;
  end;

  //精灵管理器
  TSpriteManager = class
  private
    FCanvas: TEngineCanvas;
    FFontMap: TObjectDictionary<string, TFontInstance>;
    FViewPort: TPoint2i;
    FAllCount, FDrawCount: Integer;
    FSpriteList: TObjectList<TCustomSprite>;
    FDeadList: TList<TCustomSprite>;
    FSelectedList: TList<TCustomSprite>;
    FGroupMap: TObjectDictionary<string, TList<TCustomSprite>>;
    FWorldX, FWorldY: Single;
    FTouchPoint: TPoint2i;
    FZOrderAutoSort: Boolean;
    FOnPrint: TNotifyInfoEvent;
    FPadding: TIntRect;
    FDefaultFont: TFontInstance;
    function GetSprite(const Name: string): TCustomSprite;
    function GetScale: Single;
    function GetDefaultFont: TFontInstance;
    procedure SetPadding(const Value: TIntRect);
  private
    procedure Add(const ASprite: TCustomSprite);
    procedure Remove(const ASprite: TCustomSprite);
    function SpriteAtPos(const X, Y: Single; const ACheckVisible,
      ACheckHitTest: Boolean): TCustomSprite;
    // 加入分组
    procedure Grouping(const AGroupName: string; const ASprite: TCustomSprite);
    // 移出分组
    procedure OutGroup(const AGroupName: string; const ASprite: TCustomSprite);
    // 解散分组
    procedure BreakGroup(const AGroupName: string);
  private
    FOnFormMouseDown, FOnFormMouseUp: TMouseEvent;
    FOnFormMouseMove: TMouseMoveEvent;
    FOnFormMouseWheel: TMouseWheelEvent;
    FActiveSprite, FMouseEnterSprite: TCustomSprite;
    procedure AcquireEvents(const AForm: TForm);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure DoMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure DoMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  protected
    procedure ResetDrawOrder;
  public
    constructor Create(const AForm: TForm);
    destructor Destroy; override;

    // 批量更新(增加)前锁定
    procedure BeginUpdate;
    // 批量更新(增加)前解锁
    procedure EndUpdate;

    // 渲染
    procedure Render;
    // 移动
    procedure Move(const AMoveCount: Integer);
    // 清理死亡对象
    procedure Dead;
    // 计算两个Sprite之间的距离
    function SpriteDistance(const S1, S2: TCustomSprite): Real;
    // 重置大小
    procedure Resize(const ANewWidth, ANewHeight: Integer);
    // 释放整个分组中的所有Sprite
    procedure DeadGroup(const AGroupName: string);
  public
    // 世界坐标
    property WorldX: Single read FWorldX write FWorldX ;
    property WorldY: Single read FWorldY write FWorldY;
    // 画布
    property Canvas: TEngineCanvas read FCanvas write FCanvas;
    // 信息打印
    property OnPrint: TNotifyInfoEvent read FOnPrint write FOnPrint;
    // 可视区域大小
    property ViewPort: TPoint2i read FViewPort write FViewPort;
    // 点击屏幕的位置
    property TouchPoint: TPoint2i read FTouchPoint write FTouchPoint;
    // 根据名称获取Sprite
    property Sprite[const Name: string]: TCustomSprite read GetSprite;
    // 边缘区域
    property Padding: TIntRect read FPadding write SetPadding;
    // 默认字体对象
    property DefaultFont: TFontInstance read GetDefaultFont;
    // 屏幕比例
    property Scale: Single read GetScale;
  end;

implementation

function Angles(X, Y: Integer): Real;
begin
  Result := Abs(((Arctan2(X, Y) * 40.5)) - 128);
end;

{$REGION 'TCustomSprite'}

{ TCustomSprite }

constructor TCustomSprite.Create(const AManager: TSpriteManager);
begin
  inherited Create;
  FManager:= AManager;
  FChildList:= TList<TCustomSprite>.Create;
  FParent:= nil;
  FMargins:= TBounds.Create(RectF(0, 0, 0, 0));
  FMargins.OnChange:= DoMarginsChanged;
  FZOrderAutoSort:= True;
  //
  FName:= '';
  FImage:= nil;
  FX:= 0;
  FY:= 0;
  FDrawX:= 0;
  FDrawY:= 0;
  FZOrder:= 0;
  if Self.FZOrder = 0 then Self.FZOrder:= 1;
  FWidth:= 8;
  FHeight:= 8;
  FBlendMode:= TEngineBlendingEffect.Normal;
  FMoveable:= True;
  FTruncDraw:= True;
  FVisible:= True;
  FOffset:= ZeroPoint2f;
  FPatternIndex:= 0;
  FTag:= 0;
  FCollideMode:= TCollideMode.cmRect;
  FCollisioned:= False;
  FCanFocus:= False;
  FFocused:= False;
  FHitTest:= True;
  FAlign:= TAlignMode.amNone;
  FFixedLayout:= False;
  FGroupName:= '';
  //
  AManager.Add(Self);
end;

destructor TCustomSprite.Destroy;
begin
  FreeAndNil(FMargins);
  FreeAndNil(FChildList);
  inherited;
end;

procedure TCustomSprite.BeginUpdate;
begin
  FZOrderAutoSort:= False;
end;

procedure TCustomSprite.EndUpdate;
begin
  FZOrderAutoSort:= True;
  Self.ResetDrawOrder;
end;

procedure TCustomSprite.SetParent(const Value: TCustomSprite);
begin
  if FParent = Value then Exit;
  if Assigned(FParent) then
    FParent.Remove(Self);
  FParent:= Value;
  FParent.Add(Self);
  FParent.ResetDrawOrder;
  DoLayoutChanged;
end;

procedure TCustomSprite.SetWidth(const Value: Integer);
begin
  FWidth:= Round(Value * FManager.Scale);
end;

procedure TCustomSprite.SetAlign(const Value: TAlignMode);
begin
  if FAlign <> Value then  
  begin
    FAlign:= Value;
    DoLayoutChanged;
  end;
end;

procedure TCustomSprite.SetFocus(const AFocused: Boolean);
begin
  FFocused:= False;
  if FCanFocus then FFocused:= AFocused;
end;

procedure TCustomSprite.SetGroupName(const Value: string);
begin
  if FGroupName <> Value then
  begin
    if Value = '' then
      FManager.OutGroup(FGroupName, Self)
    else
    begin
      FGroupName:= Value;
      FManager.Grouping(FGroupName, Self);
    end;
  end;
end;

procedure TCustomSprite.SetHeight(const Value: Integer);
begin
  FHeight:= Round(Value * FManager.Scale);
end;

procedure TCustomSprite.SetImage(const Value: TEngineImage);
begin
  FImage:= Value;
end;

procedure TCustomSprite.SetX(const Value: Single);
begin
  FX:= Value * FManager.Scale;
  if Assigned(FParent) then
    FDrawX:= FParent.FDrawX + FX
  else
    FDrawX:= FX;
end;

procedure TCustomSprite.SetY(const Value: Single);
begin        
  FY:= Value * FManager.Scale;
  if Assigned(FParent) then
    FDrawY:= FParent.FDrawY + FY
  else
    FDrawY:= FY;
end;

procedure TCustomSprite.SetZOrder(const Value: Integer);
begin
  if FZOrder <> Value then
  begin
    FZOrder:= Value;
    if FParent <> nil then
      FParent.ResetDrawOrder
    else
      FManager.ResetDrawOrder
  end;
end;

function TCustomSprite.SpriteAtPos(const X, Y: Integer; const ACheckVisible,
  ACheckHitTest: Boolean): TCustomSprite;
var
  I: Integer;
begin
  Result:= nil;
  if ACheckVisible and not Self.Visible then Exit;
  if ACheckHitTest and not Self.HitTest then Exit;
  if Self.BoundsRect.Contains(Point2i(X, Y)) then
    Exit(Self)
  else
    for I:= FChildList.Count -1 downto 0 do
    begin
      Result:= FChildList[I].SpriteAtPos(X, Y, ACheckVisible, ACheckHitTest);
      if Result <> nil then
        Break;
    end;
end;

procedure TCustomSprite.ResetDrawOrder;
begin
  if not FZOrderAutoSort then Exit;
  FChildList.Sort(
    TComparer<TCustomSprite>.Construct(
      function (const L, R: TCustomSprite): Integer
      begin
        Result:= L.ZOrder - R.ZOrder;
      end
    )
  );
end;

procedure TCustomSprite.Resize;
var
  I: Integer;
begin
  DoLayoutChanged;
  for I:= 0 to FChildList.Count -1 do
    FChildList[I].Resize;
end;

procedure TCustomSprite.Render;
var
  I: Integer;
begin
  if FVisible then
  begin
    if (FDrawX + FOffset.X > ifthen(FFixedLayout,0,FManager.WorldX) - FWidth) and
       (FDrawY + FOffset.Y > ifthen(FFixedLayout,0,FManager.WorldY) - FHeight) and
       (FDrawX + FOffset.X < ifthen(FFixedLayout,0,FManager.WorldX) + FManager.ViewPort.X) and
       (FDrawY + FOffset.Y < ifthen(FFixedLayout,0,FManager.WorldY) + FManager.ViewPort.Y) then
    begin
      DoDraw;
      Inc(FManager.FDrawCount);
    end;
    //
    for I:= 0 to FChildList.Count - 1 do
      FChildList[I].Render;
  end;
end;

procedure TCustomSprite.DoDraw;
var
  LTargetQuad: TQuad;
begin
  if FVisible and Assigned(FImage) then
  begin
    LTargetQuad:= Quad(FDrawX - ifthen(FFixedLayout,0,FManager.WorldX),
                       FDrawY - ifthen(FFixedLayout,0,FManager.WorldY),
                       FWidth, FHeight);
    LTargetQuad.Trunc(FTruncDraw);
    FManager.Canvas.DrawImage(FImage, LTargetQuad);
  end;
end;

procedure TCustomSprite.DoMove(const AMoveCount: Single);
begin
end;

function TCustomSprite.GetPatternSize: TPoint2i;
begin
  if Assigned(FImage) then
    Result:= FImage.PatternRect[0].Size
  else
    Result:= Point2i(8,8);
end;

function TCustomSprite.GetBoundsRect: TIntRect;
begin
  Result:= IntRectBDS(Round(FDrawX), Round(FDrawY),
    Round(FDrawX + FWidth), Round(FDrawY + FHeight));
end;

function TCustomSprite.GetPatternCount: Integer;
begin
  if Assigned(FImage) then
    Result:= FImage.Regions.Count
  else
    Result:= 0;
end;

function TCustomSprite.GetWorldX: Single;
begin
  Result:= FManager.WorldX + FDrawX;
end;

function TCustomSprite.GetWorldY: Single;
begin
  Result:= FManager.WorldY + FDrawY;
end;

procedure TCustomSprite.DoCollision(const ASprite: TCustomSprite);
begin
end;

procedure TCustomSprite.Add(const ASprite: TCustomSprite);
begin
  if Assigned(ASprite) and not FChildList.Contains(ASprite) then
  begin
    ASprite.FDrawX:= Self.FDrawX + ASprite.X;
    ASprite.FDrawY:= Self.FDrawY + ASprite.Y;
    FChildList.Add(ASprite);
  end;
end;

procedure TCustomSprite.Remove(const ASprite: TCustomSprite);
begin
  if Assigned(ASprite) and FChildList.Contains(ASprite) then
  begin
    ASprite.FDrawX:= ASprite.X;
    ASprite.FDrawY:= ASprite.Y;
    ASprite.FParent:= nil;
    FChildList.Remove(ASprite);
  end;
end;

procedure TCustomSprite.Collision(const AOther: TCustomSprite);
var
  LDelta: Real;
  LCollided: Boolean;
begin
  LCollided := False;
  if FCollisioned and AOther.Collisioned and (not FDeaded) and (not AOther.FDeaded) then
  begin
    case FCollideMode of
      TCollideMode.cmCircle:
        begin
          LDelta:= Sqrt(Sqr(FCollidePos.X - AOther.CollidePos.X) +
                        Sqr(FCollidePos.Y - AOther.CollidePos.Y));
          LCollided:= LDelta < (FCollideRadius + AOther.CollideRadius);
        end;
      TCollideMode.cmRect:
          LCollided:= FCollideRect.Overlaps(AOther.CollideRect);
      TCollideMode.cmQuadrangle:
          LCollided:= FCollideQuadrangle.Overlaps(AOther.CollideQuadrangle);
      TCollideMode.cmPolygon:
          LCollided:= FCollidePolygon.Overlaps(AOther.CollidePolygon);
    end;
    //
    if LCollided then
    begin
      Self.DoCollision(AOther);
      AOther.DoCollision(Self);
    end;
  end;
end;

procedure TCustomSprite.Collision;
var
  I: Integer;
begin
  if FCollisioned and not FDeaded then
  begin
    for I:= 0 to FManager.FSpriteList.Count - 1 do
      Self.Collision(FManager.FSpriteList[I]);
  end;
end;

procedure TCustomSprite.Dead;
begin
  if not FDeaded then
  begin
    FDeaded:= True;
    FManager.FDeadList.Add(Self);
  end;
end;

procedure TCustomSprite.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomSprite.MouseEnter;
begin

end;

procedure TCustomSprite.MouseLeave;
begin

end;

procedure TCustomSprite.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCustomSprite.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  //
  if BoundsRect.Contains(Point2i(Round(X*FManager.Scale),
                                 Round(Y*FManager.Scale))) and
     Assigned(FOnClick) then FOnClick(Self);
end;

procedure TCustomSprite.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, Handled);
end;

procedure TCustomSprite.Move(const AMoveCount: Single);
var
  I: Integer;
begin
  if FMoveable then
  begin
    DoMove(AMoveCount);
    for I:= 0 to FChildList.Count - 1 do
      FChildList[I].Move(AMoveCount);
  end;
end;

procedure TCustomSprite.DoMarginsChanged(Sender: TObject);
begin
  DoLayoutChanged;
end;

procedure TCustomSprite.DoLayoutChanged;
var
  LMargins: TFloatRect;
  LContainerSize: TPoint2i;
begin
  LMargins.Left  := FMargins.Left   * FManager.Scale;
  LMargins.Top   := FMargins.Top    * FManager.Scale;
  LMargins.Right := FMargins.Right  * FManager.Scale;
  LMargins.Bottom:= FMargins.Bottom * FManager.Scale;
  if Assigned(FParent) then
    LContainerSize:= Point2i(FParent.Width, FParent.Height)
  else
    LContainerSize:= FManager.ViewPort;
  //
  if FAlign = amClient then
  begin
    FWidth := LContainerSize.X;
    FHeight:= LContainerSize.Y;
  end;
  //
  case FAlign of
    amRightTop:
      begin
        FDrawX:= LContainerSize.X - FWidth - LMargins.Right - FManager.Padding.Right;
        FDrawY:= 0 + LMargins.Top + FManager.Padding.Top;
      end;
    amLeftBottom:
      begin
        FDrawX:= 0 + LMargins.Left + FManager.Padding.Left;
        FDrawY:= LContainerSize.Y - FHeight - LMargins.Bottom - FManager.Padding.Bottom;
      end;
    amRightBottom:
      begin
        FDrawX:= LContainerSize.X - FWidth  - LMargins.Right - FManager.Padding.Right;
        FDrawY:= LContainerSize.Y - FHeight - LMargins.Bottom - FManager.Padding.Bottom;
      end;
    amCenter:
      begin
        FDrawX:= LContainerSize.X / 2 - FWidth / 2;
        FDrawY:= LContainerSize.Y / 2 - FHeight / 2;
      end;
    amCenterTop:
      begin
        FDrawX:= LContainerSize.X / 2 - FWidth / 2;
        FDrawY:= 0 + LMargins.Top + FManager.Padding.Top;
      end;
    amCenterBottom:
      begin
        FDrawX:= LContainerSize.X / 2 - FWidth / 2;
        FDrawY:= LContainerSize.Y - FHeight - LMargins.Bottom - FManager.Padding.Bottom;
      end;
    amCenterLeft:
      begin
        FDrawX:= 0 + LMargins.Left + FManager.Padding.Left;
        FDrawY:= LContainerSize.Y / 2 - FHeight / 2;
      end;
    amCenterRight:
      begin
        FDrawX:= LContainerSize.X - FWidth - LMargins.Right - FManager.Padding.Right;
        FDrawY:= LContainerSize.Y / 2 - FHeight / 2;
      end;
    else
      begin
        FDrawX:= 0 + LMargins.Left + FManager.Padding.Left;
        FDrawY:= 0 + LMargins.Top  + FManager.Padding.Top;
      end;
  end;
  //
  if Assigned(FParent) then
  begin
    FDrawX:= FParent.FDrawX + FDrawX;
    FDrawY:= FParent.FDrawY + FDrawY;
  end;
end;

{$ENDREGION}

{$REGION 'TSprite'}

{ TSprite }

constructor TSprite.Create(const AManager: TSpriteManager);
begin
  inherited;
  FColor:= FloatColorWhite;
  FAngle:= 0;
  FAngle360:= 0;
  FScale:= Point2f(1.0, 1.0);
  FCentered:= False;
  FDrawMode:= TDrawMode.dmColor;
end;

destructor TSprite.Destroy;
begin
  SetSelected(False);
  inherited Destroy;
end;

procedure TSprite.DoDraw;
var
  LX, LY: Single;
  LTargetQuad: TQuad;
begin
  if not Assigned(FImage) then Exit;
  if not Assigned(FManager) then Exit;
  //
  LX:= FDrawX + FOffset.X - ifthen(FFixedLayout,0,FManager.WorldX);
  LY:= FDrawY + FOffset.Y - ifthen(FFixedLayout,0,FManager.WorldY);
  case FDrawMode of
    TDrawMode.dmColor:
      begin
        FManager.Canvas.DrawImage(FImage, FPatternIndex,
          Quad(LX, LY, FWidth, FHeight), FColor.ToInt, FMirror, FFlip, 0, 1, FBlendMode);
      end;
    TDrawMode.dmRotate:
      begin
        FManager.Canvas.DrawImage(FImage, FPatternIndex,
          Quad(LX, LY, FWidth, FHeight), FColor.ToInt, FMirror, FFlip, FAngle, FScale.X, FBlendMode);
      end;
    TDrawMode.dmTransform:
      begin
        LTargetQuad.Values[0].X:= Trunc(FTransformQuad.Values[0].X + FOffset.X) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldX));
        LTargetQuad.Values[0].Y:= Trunc(FTransformQuad.Values[0].Y + FOffset.Y) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldY));
        LTargetQuad.Values[1].X:= Trunc(FTransformQuad.Values[1].X + FOffset.X) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldX));
        LTargetQuad.Values[1].Y:= Trunc(FTransformQuad.Values[1].Y + FOffset.Y) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldY));
        LTargetQuad.Values[2].X:= Trunc(FTransformQuad.Values[2].X + FOffset.X) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldX));
        LTargetQuad.Values[2].Y:= Trunc(FTransformQuad.Values[2].Y + FOffset.Y) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldY));
        LTargetQuad.Values[3].X:= Trunc(FTransformQuad.Values[3].X + FOffset.X) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldX));
        LTargetQuad.Values[3].Y:= Trunc(FTransformQuad.Values[3].Y + FOffset.Y) -
          Trunc(ifthen(FFixedLayout,0,FManager.WorldY));
        FManager.Canvas.DrawImage(FImage, FPatternIndex, LTargetQuad,
          FColor.ToInt, FMirror, FFlip, FAngle, FScale.X, FBlendMode);
      end;
  end;
end;

procedure TSprite.SetAngle360(Value: Integer);
begin
  if FAngle360 <> Value then
    FAngle:= DegToRad(Value);
end;

procedure TSprite.SetSelected(const Value: Boolean);
begin
  if Value <> FSelected then
  begin
    FSelected:= Value;
    if FSelected then
      FManager.FSelectedList.Add(Self)
    else
      FManager.FSelectedList.Remove(Self);
  end;
end;

{$ENDREGION}

{$REGION 'TAnimatedSprite'}

{ TAnimatedSprite }

constructor TAnimatedSprite.Create(const AManager: TSpriteManager);
begin
  inherited;
  FFrameStart:= 0;
  FFrameAmount:= 0;
  FFrameIndex:= 0;
  FSpeed:= 0;
  FPlayMode:= TAnimPlayMode.pmForward;
  FLooped:= False;
  FActive:= False;
  //
  FPlayFlag1:= False;
  FPlayFlag2:= False;
end;

procedure TAnimatedSprite.SetActive(const Value: Boolean);
begin
  FActive:= Value;
end;

procedure TAnimatedSprite.SetFrameStart(const Value: Integer);
begin
  if FFrameStart <> Value then
  begin
    FFrameStart:= Value;
    FFrameIndex:= Value;
  end;
end;

function TAnimatedSprite.GetEnded: Boolean;
begin
  if Trunc(FFrameIndex) = (FFrameStart + FFrameAmount - 1) then
    Result:= True
  else
    Result:= False;
end;

procedure TAnimatedSprite.DoEnd;
begin
end;

procedure TAnimatedSprite.DoStart;
begin
end;

procedure TAnimatedSprite.DoMove(const AMoveCount: Single);
begin
  if not FActive then Exit;
  //
  case FPlayMode of
    TAnimPlayMode.pmForward: // 12345 12345  12345
      begin
        FFrameIndex:= FFrameIndex + FSpeed * AMoveCount;
        if (FFrameIndex >= FFrameStart + FFrameAmount) then
        begin
          if Trunc(FFrameIndex) = FFrameStart then Self.DoStart;
          if Self.Ended then Self.DoEnd;
          if FLooped then
            FFrameIndex:= FFrameStart
          else
          begin
            FFrameIndex:= FFrameStart + FFrameAmount - 1;
            FActive:= False;
          end;
        end;
        FPatternIndex:= Trunc(FFrameIndex);
      end;
    TAnimPlayMode.pmBackward: // 54321 54321 54321
      begin
        FFrameIndex:= FFrameIndex - FSpeed * AMoveCount;
        if FFrameIndex < FFrameStart then
        begin
          if FLooped then
            FFrameIndex:= FFrameStart + FFrameAmount
          else
          begin
            FFrameIndex:= FFrameStart + FFrameAmount;
            FActive:= False;
          end;
        end;
        FPatternIndex:= Trunc(FFrameIndex);
      end;
    TAnimPlayMode.pmPingPong: // 12345432123454321
      begin
        FFrameIndex:= FFrameIndex + FSpeed * AMoveCount;
        if FLooped then
        begin
          if (FFrameIndex > FFrameStart + FFrameAmount - 1) or
             (FFrameIndex < FFrameStart) then
            FSpeed:= -FSpeed;
        end
        else
        begin
          if (FFrameIndex > FFrameStart + FFrameAmount) or
             (FFrameIndex < FFrameStart) then FSpeed:= -FSpeed;
          //
          if Trunc(FFrameIndex) = (FFrameStart + FFrameAmount) then
            FPlayFlag1:= True;
          if (Trunc(FFrameIndex) = FFrameStart) and FPlayFlag1 then
            FPlayFlag2:= True;
          if FPlayFlag1 and FPlayFlag2 then
          begin
            FActive:= False;
            FPlayFlag1:= False;
            FPlayFlag2:= False;
          end;
        end;
        FPatternIndex:= Round(FFrameIndex);
      end;
  end;
end;

procedure TAnimatedSprite.Play(const AImage: TEngineImage; const AFrameStart,
  AFrameAmount: Integer; const ASpeed: Single; const ALooped, AMirror,
  AActive: Boolean; const APlayMode: TAnimPlayMode);
begin
  FImage:= AImage;
  Self.FrameStart := AFrameStart;
  Self.FrameAmount:= AFrameAmount;
  Self.Speed      := ASpeed;
  Self.Looped     := ALooped;
  Self.Mirror     := AMirror;
  Self.PlayMode   := APlayMode;
  Self.Active     := AActive;
  //
  if (FPatternIndex < FFrameStart) or
     (FPatternIndex >= FFrameAmount + FFrameStart) then
  begin
    FPatternIndex:= FFrameStart mod FFrameAmount;
    FFrameIndex  := FFrameStart;
  end;
end;

procedure TAnimatedSprite.Play(const AImage: TEngineImage; const AFrameStart,
  AFrameAmount: Integer; const ASpeed: Single; const ALooped: Boolean;
  const APlayMode: TAnimPlayMode);
begin
  FImage:= AImage;
  Self.FrameStart := AFrameStart;
  Self.FrameAmount:= AFrameAmount;
  Self.Speed      := ASpeed;
  Self.Looped     := ALooped;
  Self.PlayMode   := APlayMode;
  //
  if (FPatternIndex < FFrameStart) or
     (FPatternIndex >= FFrameAmount + FFrameStart) then
  begin
    FPatternIndex:= FFrameStart mod FFrameAmount;
    FFrameIndex  := FFrameStart;
  end;
end;

{$ENDREGION}

{$REGION 'TTextSprite'}

{ TTextSprite }

constructor TTextSprite.Create(const AManager: TSpriteManager);
begin
  inherited;
  {$IFDEF DEBUG}
  FCanFocus:= True;
  {$ELSE}
  FCanFocus:= False;
  {$ENDIF}
  FFixedLayout:= True;
  FHitTest:= False;
  //
  FFont:= FManager.DefaultFont;
  FFontColor:= IntColorBlack;
  FFontSize := TFontTypes.OptimumFontSize;
  FFontSizeScale:= FFontSize / TFontTypes.OptimumFontSize * TClientUtils.ScreenScale;
  FFontStyle:= TFontCharStyle.Default(FFontColor);
  FTextAlign:= TTextAlignMode.tamLeft;
  FFontRenderer.Reset;
end;

procedure TTextSprite.DoDraw;
begin
  if not Assigned(FFont) then
    Exit;
  //
  if FFontRenderer.Chars.Count = 0 then
    FFontRenderer:= FFont.Compile(Point2f(Self.FDrawX, Self.FDrawY), FText,
      FFontSize, FFontStyle, IntRect(0, 0, FWidth, FHeight), FWidth <> 0, -1, FTextAlign);
  //
  FFont.DrawText(FFontRenderer, FFontSizeScale);
end;

procedure TTextSprite.Resize;
begin
  inherited;
  FFontRenderer.Reset;
end;

procedure TTextSprite.SetText(const Value: string);
begin
  if FText <> Value then
  begin
    FText:= Value;
    FFontRenderer.Reset;
  end;
end;

procedure TTextSprite.SetFontColor(const Value: TIntColor);
begin
  if FFontColor <> Value then
  begin
    FFontColor:= Value;
    FFontStyle.Color:= FFontColor;
    FFontRenderer.Reset;
  end;
end;

procedure TTextSprite.SetFontName(const Value: string);
begin
  if FFontName <> Value then
  begin
    FFontName:= Value;
    FFont:= AssetsManager.RequireFont(FFontName);
    FFontRenderer.Reset;
  end;
end;

procedure TTextSprite.SetFontSize(const Value: Word);
begin
  if FFontSize <> Value then
  begin
    FFontSize:= Value;
    FFontRenderer.Reset;
    FFontSizeScale:= FFontSize / TFontTypes.OptimumFontSize * TClientUtils.ScreenScale;
  end;
end;

{$ENDREGION}

{$REGION 'TGUISprite'}

{ TGUISprite }

constructor TGUISprite.Create(const AManager: TSpriteManager);
begin
  inherited;
  {$IFDEF DEBUG}
  FCanFocus:= True;
  {$ELSE}
  FCanFocus:= False;
  {$ENDIF}
  FFixedLayout:= True;
  FText:= nil;
end;

procedure TGUISprite.DoDraw;
begin
  inherited;
  if FCanFocus and FFocused then
    FManager.Canvas.FrameRect(FloatRect(Self.BoundsRect.Left,
      Self.BoundsRect.Top, Self.BoundsRect.Width, Self.BoundsRect.Height), $FFFF0000);
end;

procedure TGUISprite.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if Assigned(FImage) then
    FColor.Alpha:= 0.6;
  inherited;
end;

procedure TGUISprite.MouseEnter;
begin
  if Assigned(FImage) then
    FColor.Alpha:= 0.8;
  inherited;
end;

procedure TGUISprite.MouseLeave;
begin
  if Assigned(FImage) then
    FColor.Alpha:= 1.0;
  inherited;
end;

procedure TGUISprite.MouseMove(Shift: TShiftState; X, Y: Single);
begin

  inherited;
end;

procedure TGUISprite.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if Assigned(FImage) then
    FColor.Alpha:= 1.0;
  inherited;
end;

procedure TGUISprite.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
begin
  inherited;

end;

procedure TGUISprite.SetText(const Value: TTextSprite);
begin
  FText:= Value;
  FText.Width:= Self.Width;
  FText.Height:= Self.Height;
  FText.Parent:= Self;
  FText.Align:= TAlignMode.amClient;
end;

{$ENDREGION}

{$REGION 'TSpriteManager'}

{ TSpriteManager }

function TSpriteManager.SpriteAtPos(const X, Y: Single; const ACheckVisible,
  ACheckHitTest: Boolean): TCustomSprite;
var
  I: Integer;
begin
  Result:= nil;
  for I:= FSpriteList.Count -1 downto 0 do
  begin
    Result:= FSpriteList[I].SpriteAtPos(Round(X), Round(Y), ACheckHitTest, ACheckVisible);
    if Result <> nil then
      Break;
  end;
end;

function TSpriteManager.SpriteDistance(const S1, S2: TCustomSprite): Real;
begin
  Result := { @System.Math } Hypot(S1.X - S2.X, S1.Y - S2.Y);
end;

constructor TSpriteManager.Create(const AForm: TForm);
begin
  inherited Create;
  FFontMap:= TObjectDictionary<string, TFontInstance>.Create([doOwnsValues]);
  FSpriteList:= TObjectList<TCustomSprite>.Create;
  FDeadList:= TList<TCustomSprite>.Create;
  FSelectedList:= TList<TCustomSprite>.Create;
  FGroupMap:= TObjectDictionary<string, TList<TCustomSprite>>.Create([doOwnsValues]);
  //
  FViewPort:= Point2i(960, 480);
  FWorldX:= 0;
  FWorldY:= 0;
  FTouchPoint:= ZeroPoint2i;
  FZOrderAutoSort:= True;
  FActiveSprite:= nil;
  FMouseEnterSprite:= nil;
  FPadding:= IntRectBDS(0, 0, 0, 0);
  FDefaultFont:= nil;
  //
  AcquireEvents(AForm);
end;

destructor TSpriteManager.Destroy;
begin
  FreeAndNil(FFontMap);
  FreeAndNil(FSelectedList);
  FreeAndNil(FDeadList);
  FreeAndNil(FSpriteList);
  FreeAndNil(FGroupMap);
  inherited;
end;

procedure TSpriteManager.BeginUpdate;
begin
  FZOrderAutoSort:= False;
end;

procedure TSpriteManager.EndUpdate;
begin
  FZOrderAutoSort:= True;
  Self.ResetDrawOrder;
end;

function TSpriteManager.GetDefaultFont: TFontInstance;
begin
  if not Assigned(FDefaultFont) then
    FDefaultFont:= AssetsManager.RequireFont(TFontTypes.DefaultFontName);
  Result:= FDefaultFont;
end;

function TSpriteManager.GetScale: Single;
begin
  Result:= TClientUtils.ScreenScale;
end;

function TSpriteManager.GetSprite(const Name: string): TCustomSprite;
var
  I: Integer;
begin
  Result:= nil;
  for I:= 0 to FSpriteList.Count -1 do
    if FSpriteList.Items[I].Name = Name then
      Exit(FSpriteList.Items[I]);
end;

procedure TSpriteManager.SetPadding(const Value: TIntRect);
var
  I: Integer;
begin
  if FPadding <> Value then
  begin
    FPadding:= Value;
    for I:= 0 to FSpriteList.Count -1 do
      FSpriteList.Items[I].DoLayoutChanged;
  end;
end;

procedure TSpriteManager.Add(const ASprite: TCustomSprite);
begin
  FSpriteList.Add(ASprite);
end;

procedure TSpriteManager.Remove(const ASprite: TCustomSprite);
begin
  FSpriteList.Remove(ASprite);
end;

procedure TSpriteManager.Render;
var
  I: Integer;
begin
  FDrawCount:= 0;
  for I:= 0 to FSpriteList.Count -1 do
    FSpriteList.Items[I].Render;
end;

procedure TSpriteManager.ResetDrawOrder;
begin
  if not FZOrderAutoSort then Exit;
  FSpriteList.Sort(
    TComparer<TCustomSprite>.Construct(
      function (const L, R: TCustomSprite): Integer
      begin
        Result:= L.ZOrder - R.ZOrder;
      end
    )
  );
end;

procedure TSpriteManager.Resize(const ANewWidth, ANewHeight: Integer);
var
  I: Integer;
begin
  if (FViewPort.X = ANewWidth) and (FViewPort.Y = ANewHeight) then Exit;
  //  
  FViewPort.X:= ANewWidth;
  FViewPort.Y:= ANewHeight;  
  for I:= 0 to FSpriteList.Count -1 do
    FSpriteList[I].Resize;
end;

procedure TSpriteManager.AcquireEvents(const AForm: TForm);
begin
  if not Assigned(AForm) then Exit;

  FOnFormMouseDown  := AForm.OnMouseDown;
  FOnFormMouseMove  := AForm.OnMouseMove;
  FOnFormMouseUp    := AForm.OnMouseUp;
  FOnFormMouseWheel := AForm.OnMouseWheel;

  AForm.OnMouseDown  := DoMouseDown;
  AForm.OnMouseMove  := DoMouseMove;
  AForm.OnMouseUp    := DoMouseUp;
  AForm.OnMouseWheel := DoMouseWheel;
end;

procedure TSpriteManager.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  LSprite: TCustomSprite;
begin
  LSprite:= Self.SpriteAtPos(X*Self.Scale,
    Y*Self.Scale, True, True);
  if Assigned(LSprite) then
  begin
    if Assigned(FActiveSprite) then
      FActiveSprite.SetFocus(False);
    //
    LSprite.MouseDown(Button, Shift, X, Y);
    FActiveSprite:= LSprite;
    FActiveSprite.SetFocus(True);
  end else
  begin
    if Assigned(FActiveSprite) then
    begin
      FActiveSprite.SetFocus(False);
      FActiveSprite:= nil;
    end;
    if Assigned(FOnFormMouseDown) then
      FOnFormMouseDown(Sender, Button, Shift, X, Y);
  end;
end;

procedure TSpriteManager.DoMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  LSprite: TCustomSprite;
begin
  LSprite:= Self.SpriteAtPos(X*Self.Scale, Y*Self.Scale, True, True);
  if Assigned(LSprite) then
  begin
    LSprite.MouseMove(Shift, X, Y);
    if FMouseEnterSprite <> LSprite then
    begin
      LSprite.MouseEnter;
      if Assigned(FMouseEnterSprite) then      
        FMouseEnterSprite.MouseLeave;
      FMouseEnterSprite:= LSprite;
    end;
  end else
  begin
    if Assigned(FMouseEnterSprite) then
    begin
      FMouseEnterSprite.MouseLeave;
      FMouseEnterSprite:= nil;
    end;
    if Assigned(FOnFormMouseMove) then
      FOnFormMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TSpriteManager.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if Assigned(FActiveSprite) then
    FActiveSprite.MouseUp(Button, Shift, X, Y)
  else
    if Assigned(FOnFormMouseUp) then
      FOnFormMouseUp(Sender, Button, Shift, X, Y);
end;

procedure TSpriteManager.DoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin

end;

procedure TSpriteManager.Move(const AMoveCount: Integer);
var
  I: Integer;
begin
  for I:= 0 to FSpriteList.Count -1 do
    FSpriteList.Items[I].Move(AMoveCount);
end;

procedure TSpriteManager.Dead;
var
  I: Integer;
begin
  for I:= 0 to FDeadList.Count -1 do
    Self.Remove(FDeadList.Items[I]);
  FDeadList.Clear;
end;

procedure TSpriteManager.Grouping(const AGroupName: string;
  const ASprite: TCustomSprite);
var
  LGroup: TList<TCustomSprite>;
begin
  if AGroupName = '' then
    Exit;
  if not Assigned(ASprite) then
    Exit;
  //
  if not FGroupMap.TryGetValue(AGroupName, LGroup) then
  begin
    LGroup:= TList<TCustomSprite>.Create;
    FGroupMap.Add(AGroupName, LGroup);
  end;
  if not LGroup.Contains(ASprite) then
    LGroup.Add(ASprite);
end;

procedure TSpriteManager.OutGroup(const AGroupName: string;
  const ASprite: TCustomSprite);
var
  LGroup: TList<TCustomSprite>;
begin
  if AGroupName = '' then
    Exit;
  if not Assigned(ASprite) then
    Exit;
  if not FGroupMap.TryGetValue(AGroupName, LGroup) then
    Exit;
  //
  ASprite.FGroupName:= '';
  LGroup.Remove(ASprite);
  if LGroup.Count = 0 then
    FGroupMap.Remove(AGroupName);
end;

procedure TSpriteManager.BreakGroup(const AGroupName: string);
var
  LGroup: TList<TCustomSprite>;
  I: Integer;
begin
  if AGroupName = '' then
    Exit;
  if not FGroupMap.TryGetValue(AGroupName, LGroup) then
    Exit;
  //
  for I:= 0 to LGroup.Count -1 do
    LGroup.Items[I].FGroupName:= '';
  LGroup.Clear;
  FGroupMap.Remove(AGroupName);
end;

procedure TSpriteManager.DeadGroup(const AGroupName: string);
var
  LGroup: TList<TCustomSprite>;
  I: Integer;
begin
  if AGroupName = '' then
    Exit;
  if AGroupName = '' then
    Exit;
  if not FGroupMap.TryGetValue(AGroupName, LGroup) then
    Exit;
  //
  for I:= 0 to LGroup.Count -1 do
    LGroup.Items[I].Dead;
end;

{$ENDREGION}

end.
