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
  PXL.Types, se.game.types, se.game.helper, se.utils.client;

type
  //碰撞模式
  TCollideMode   = (cmCircle,           // 圆
                    cmRect,             // 矩形
                    cmQuadrangle,       // 四边形
                    cmPolygon           // 多边形
                   );
  //动画播放模式
  TAnimPlayMode  = (pmForward,          // 12345 12345 12345
                    pmBackward,         // 54321 54321 54321
                    pmPingPong          // 12345432123454321
                   );
  //渲染模式
  TDrawMode      = (dmColor,
                    dmRotate,
                    dmTransform
                   );

  TSpriteManager = class;
  TCustomSpriteClass = class of TCustomSprite;

  TCustomSprite = class
  private
    FParent: TCustomSprite;
    FChildList: TList<TCustomSprite>;
    FDeaded: Boolean;
  private
    FName: string;
    FImage: TEngineImage;
    FWidth, FHeight, FZOrder, FTag, FPatternIndex: Integer;
    FX, FY: Single;
    FBlendMode: TEngineBlendingEffect;
    FMoveable, FTruncDraw, FVisible: Boolean;
    FCollidePos: TPoint2f;
    FCollideRadius: Integer;
    FCollideRect: TFloatRect;
    FCollideQuadrangle: TQuad;
    FCollidePolygon: TPolygon;
    FCollideMode: TCollideMode;
    FCollisioned: Boolean;
    FCanFocus, FFocused, FHitTest: Boolean;
    procedure SetParent(const Value: TCustomSprite);
    procedure SetImage(const Value: TEngineImage);
    procedure SetX(const Value: Single);
    procedure SetY(const Value: Single);
    procedure SetZOrder(const Value: Integer);
    function GetWorldX: Single;
    function GetWorldY: Single;
    function GetPatternSize: TPoint2i;
    function GetPatternCount: Integer;
    function SpriteAtPos(const X, Y: Integer): TCustomSprite;
  private
    FZOrderAutoSort: Boolean;
    procedure Add(const ASprite: TCustomSprite);
    procedure Remove(const ASprite: TCustomSprite);
    procedure ResetDrawOrder;
    procedure SetFocus(const AFocused: Boolean);
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
    procedure Render; virtual;
    procedure DoDraw; virtual;
    procedure DoMove(const AMoveCount: Single); virtual;
    procedure DoCollision(const ASprite: TCustomSprite); virtual;
    function  GetBoundsRect: TIntRect; virtual;
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
    property Width: Integer read FWidth write FWidth;
    // 高度
    property Height: Integer read FHeight write FHeight;
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
    FOffset, FScale: TPoint2f;
    FDrawMode: TDrawMode;
    FTransformQuad: TQuad;
    FMirror, FFlip, FCentered, FSelected, FCollisionable: Boolean;
    procedure SetAngle360(Value: Integer);
    procedure SetSelected(const Value: Boolean);
  protected
    procedure Render; override;
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
    // 偏移
    property Offset: TPoint2f read FOffset write FOffset;
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

  //GUI精灵
  TGUISprite = class(TAnimatedSprite)
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
  end;

  //精灵管理器
  TSpriteManager = class
  private
    FCanvas: TEngineCanvas;
    FViewPort: TPoint2i;
    FAllCount, FDrawCount: Integer;
    FSpriteList: TObjectList<TCustomSprite>;
    FDeadList: TList<TCustomSprite>;
    FSelectedList: TList<TCustomSprite>;
    FWorldX, FWorldY: Single;
    FTouchPoint: TPoint2i;
    FZOrderAutoSort: Boolean;
    procedure Add(const ASprite: TCustomSprite);
    procedure Remove(const ASprite: TCustomSprite);
    function GetSprite(const Name: string): TCustomSprite;
    function SpriteAtPos(const X, Y: Single): TCustomSprite;
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
  public

  public
    // 世界坐标
    property WorldX: Single read FWorldX write FWorldX ;
    property WorldY: Single read FWorldY write FWorldY;
    // 画布
    property Canvas: TEngineCanvas read FCanvas write FCanvas;
    // 可视区域大小
    property ViewPort: TPoint2i read FViewPort write FViewPort;
    // 点击屏幕的位置
    property TouchPoint: TPoint2i read FTouchPoint write FTouchPoint;
    //
    property Sprite[const Name: string]: TCustomSprite read GetSprite;
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
  FZOrderAutoSort:= True;
  //
  FName:= '';
  FImage:= nil;
  FX:= 0;
  FY:= 0;
  FZOrder:= 0;
  if Self.FZOrder = 0 then Self.FZOrder:= 1;
  FWidth:= 8;
  FHeight:= 8;
  FBlendMode:= TEngineBlendingEffect.Normal;
  FMoveable:= True;
  FTruncDraw:= True;
  FVisible:= True;
  FPatternIndex:= 0;
  FTag:= 0;
  FCollideMode:= TCollideMode.cmRect;
  FCollisioned:= False;
  FCanFocus:= False;
  FFocused:= False;
  FHitTest:= True;
  //
  AManager.Add(Self);
end;

destructor TCustomSprite.Destroy;
begin
  FChildList.Free;
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
  if Assigned(FParent) then FParent.Remove(Self);
  FParent:= Value;
  FParent.Add(Self);
  FParent.ResetDrawOrder;
end;

procedure TCustomSprite.SetFocus(const AFocused: Boolean);
begin
  FFocused:= False;
  if FCanFocus then FFocused:= AFocused;
end;

procedure TCustomSprite.SetImage(const Value: TEngineImage);
begin
  FImage:= Value;
end;

procedure TCustomSprite.SetX(const Value: Single);
begin
  FX:= Value;
end;

procedure TCustomSprite.SetY(const Value: Single);
begin
  FY:= Value;
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

function TCustomSprite.SpriteAtPos(const X, Y: Integer): TCustomSprite;
var
  I: Integer;
begin
  Result:= nil;
  if Self.BoundsRect.Contains(Point2i(X, Y)) then
    Exit(Self)
  else
    for I:= 0 to FChildList.Count -1 do
      Result:= FChildList[I].SpriteAtPos(X, Y);
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

procedure TCustomSprite.Render;
var
  I: Integer;
begin
  if FVisible then
  begin
    if (FX > FManager.WorldX - FWidth) and
       (FY > FManager.WorldY - FHeight) and
       (FX < FManager.WorldX + FManager.ViewPort.X) and
       (FY < FManager.WorldY + FManager.ViewPort.Y) then
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
    LTargetQuad:= Quad(FX - FManager.WorldX,
                       FY - FManager.WorldY,
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
  Result:= IntRectBDS(Round(FX), Round(FY), Round(FX + FWidth), Round(FY + FHeight));
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
  if Assigned(FParent) then
    Result:= FParent.WorldX + X
  else
    Result:= FManager.WorldX + X;
end;

function TCustomSprite.GetWorldY: Single;
begin
  if Assigned(FParent) then
    Result:= FParent.WorldY + Y
  else
    Result:= FManager.WorldY + Y;
end;

procedure TCustomSprite.DoCollision(const ASprite: TCustomSprite);
begin
end;

procedure TCustomSprite.Add(const ASprite: TCustomSprite);
begin
  FChildList.Add(ASprite);
end;

procedure TCustomSprite.Remove(const ASprite: TCustomSprite);
begin
  FChildList.Remove(ASprite);
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
  if BoundsRect.Contains(Point2i(Round(X*TClientUtils.ScreenScale), 
                                 Round(Y*TClientUtils.ScreenScale))) and 
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
  FOffset:= Point2f(0, 0);
  FCentered:= False;
  FDrawMode:= TDrawMode.dmColor;
end;

destructor TSprite.Destroy;
begin
  SetSelected(False);
  inherited Destroy;
end;

procedure TSprite.Render;
var
  I: Integer;
begin
  if FVisible then
  begin
    if (FX + FOffset.X > FManager.WorldX - FWidth) and
       (FY + FOffset.Y > FManager.WorldY - FHeight) and
       (FX + FOffset.X < FManager.WorldX + FManager.ViewPort.X) and
       (FY + FOffset.Y < FManager.WorldY + FManager.ViewPort.Y) then
    begin
      DoDraw;
      Inc(FManager.FDrawCount);
    end;
    //
    for I:= 0 to FChildList.Count - 1 do
      TSprite(FChildList[I]).Render;
  end;
end;

procedure TSprite.DoDraw;
var
  LX, LY: Single;
  LTargetQuad: TQuad;
begin
  if not Assigned(FImage) then Exit;
  if not Assigned(FManager) then Exit;
  //
  LX:= FX + FOffset.X - FManager.WorldX;
  LY:= FY + FOffset.Y - FManager.WorldY;
  case FDrawMode of
    TDrawMode.dmColor:
      begin
        FManager.Canvas.DrawImageRegion(FImage, FPatternIndex, LX, LY,
          FColor.ToInt, FMirror, FFlip, FBlendMode);
      end;
    TDrawMode.dmRotate:
      begin
        FManager.Canvas.DrawImageRegion(FImage, FPatternIndex, LX, LY,
          FColor.ToInt, FMirror, FFlip, FAngle, FScale, FBlendMode);
      end;
    TDrawMode.dmTransform:
      begin
        LTargetQuad.Values[0].X:= Trunc(FTransformQuad.Values[0].X + FOffset.X) - Trunc(FManager.WorldX);
        LTargetQuad.Values[0].Y:= Trunc(FTransformQuad.Values[0].Y + FOffset.Y) - Trunc(FManager.WorldY);
        LTargetQuad.Values[1].X:= Trunc(FTransformQuad.Values[1].X + FOffset.X) - Trunc(FManager.WorldX);
        LTargetQuad.Values[1].Y:= Trunc(FTransformQuad.Values[1].Y + FOffset.Y) - Trunc(FManager.WorldY);
        LTargetQuad.Values[2].X:= Trunc(FTransformQuad.Values[2].X + FOffset.X) - Trunc(FManager.WorldX);
        LTargetQuad.Values[2].Y:= Trunc(FTransformQuad.Values[2].Y + FOffset.Y) - Trunc(FManager.WorldY);
        LTargetQuad.Values[3].X:= Trunc(FTransformQuad.Values[3].X + FOffset.X) - Trunc(FManager.WorldX);
        LTargetQuad.Values[3].Y:= Trunc(FTransformQuad.Values[3].Y + FOffset.Y) - Trunc(FManager.WorldY);
        FManager.Canvas.DrawImageRegion(FImage, FPatternIndex, LTargetQuad,
          FColor.ToInt, FMirror, FFlip, FAngle, FScale, FBlendMode);
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

{$REGION 'TGUISprite'}

{ TGUISprite }

constructor TGUISprite.Create(const AManager: TSpriteManager);
begin
  inherited;
  FCanFocus:= True;
end;

procedure TGUISprite.DoDraw;
begin
  inherited;
  if FFocused then
    FManager.Canvas.FrameRect(FloatRect(Self.BoundsRect.Left,
      Self.BoundsRect.Top, Self.BoundsRect.Width, Self.BoundsRect.Height), $FFFF0000);
end;

procedure TGUISprite.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
  if Assigned(FImage) then
    FColor.Alpha:= 0.1;
  inherited;
end;

procedure TGUISprite.MouseEnter;
begin
  if Assigned(FImage) then
    FColor.Red:= FColor.Red + 100;
  inherited;
end;

procedure TGUISprite.MouseLeave;
begin
  if Assigned(FImage) then
    FColor.Red:= FColor.Red - 100;
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

{$ENDREGION}

{$REGION 'TSpriteManager'}

{ TSpriteManager }

function TSpriteManager.SpriteAtPos(const X, Y: Single): TCustomSprite;
var
  I: Integer;
begin
  Result:= nil;
  for I:= 0 to FSpriteList.Count -1 do
  begin
    Result:= FSpriteList[I].SpriteAtPos(Round(X), Round(Y));
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
  FSpriteList:= TObjectList<TCustomSprite>.Create;
  FDeadList:= TList<TCustomSprite>.Create;
  FSelectedList:= TList<TCustomSprite>.Create;
  FViewPort:= Point2i(960, 480);
  FWorldX:= 0;
  FWorldY:= 0;
  FTouchPoint:= ZeroPoint2i;
  FZOrderAutoSort:= True;
  FActiveSprite:= nil;
  FMouseEnterSprite:= nil;
  //
  AcquireEvents(AForm);
end;

destructor TSpriteManager.Destroy;
begin
  FSelectedList.Free;
  FDeadList.Free;
  FSpriteList.Free;
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

function TSpriteManager.GetSprite(const Name: string): TCustomSprite;
var
  I: Integer;
begin
  Result:= nil;
  for I:= 0 to FSpriteList.Count -1 do
    if FSpriteList.Items[I].Name = Name then Exit(FSpriteList.Items[I]);
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
  LSprite:= Self.SpriteAtPos(X*TClientUtils.ScreenScale, Y*TClientUtils.ScreenScale);
  if Assigned(LSprite) and LSprite.Visible and LSprite.HitTest then
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
  LSprite:= Self.SpriteAtPos(X*TClientUtils.ScreenScale, Y*TClientUtils.ScreenScale);
  if Assigned(LSprite) and LSprite.Visible and LSprite.HitTest then
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

{$ENDREGION}

end.
