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

unit se.game.tiled.map;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  XSuperObject,
  PXL.Types;

type
  TTexture = record

  end;

  TTileTexSet = record

  end;

  TTileSet = record

  end;

  TSprite = record

  end;

  TMapLayer = record

  end;

  TJsonLoader = record

  end;

  TTextureLoader = record

  end;

  TTiledMap = class
  public const
    /// <summary>
    ///   地图支持的类型(目前支持四边形，菱形，六边形)
    /// </summary>
    /// <remark>
    ///   ORIENTATION_ORTHOGONAL   四边形地图
    ///   ORIENTATION_ISOMETRIC    菱形地图
    ///   ORIENTATION_STAGGERED    45度交错地图
    ///   ORIENTATION_HEXAGONAL    六边形地图
    /// </remark>
		ORIENTATION_ORTHOGONAL: string = 'orthogonal';
		ORIENTATION_ISOMETRIC : string = 'isometric';
		ORIENTATION_STAGGERED : string = 'staggered';
		ORIENTATION_HEXAGONAL : string = 'hexagonal';
    /// <summary>
    ///   地图格子(tile)的渲染顺序
    /// </summary>
    /// <remark>
    ///   RENDERORDER_RIGHTDOWN   从左上角开始渲染
    ///   RENDERORDER_RIGHTUP     从左下角开始渲染
    ///   RENDERORDER_LEFTDOWN    从右上角开始渲染
    ///   RENDERORDER_LEFTUP      从右下角开始渲染
    /// </remark>
		RENDERORDER_RIGHTDOWN : string = 'right-down';
		RENDERORDER_RIGHTUP   : string = 'right-up';
		RENDERORDER_LEFTDOWN  : string = 'left-down';
		RENDERORDER_LEFTUP    : string = 'left-up';
  private
		//json数据
		_jsonData: ISuperObject;

		//存放地图中用到的所有子纹理数据
		_tileTexSetArr: TStack<TTileTexSet>;

		//主纹理数据，主要在释放纹理资源时使用
		_texArray: TStack<TTexture>;

		//地图信息中的一些基本数据
		_x       : Single;  //地图的坐标X
		_y       : Single;  //地图的坐标Y
		_width   : Integer; //地图的宽度
		_height  : Integer; //地图的高度
		_mapW    : Integer; //地图的横向格子数
		_mapH    : Integer; //地图的竖向格子数
		_mapTileW: Integer; //tile的宽度
		_mapTileH: Integer; //tile的高度


		_rect: TIntRect; //用来存放地图的视口信息
		_paddingRect: TIntRect;  //用来存放地图的视口扩充区域
		_mapSprite: TSprite; //地图的显示对象
		_layerArray: TStack<TMapLayer>; //这里保存所有的MapLayer对象
		_renderLayerArray: TStack<TMapLayer>;//这里保存需要渲染的MapLayer对象
		_gridArray: array of array of Integer; //保存所有的块数据

		//地图块相关的
		_showGridKey   : Boolean; //是否显示块边界线（用来调试用）
		_totalGridNum  : Integer; //一层中的GridSprite的总数
		_gridW         : Integer; //地图的横向块数
		_gridH         : Integer; //地图的坚向块数
		_gridWidth     : Single;  //块的默认宽度
		_gridHeight    : Single;  //块的默认高度

		_jsonLoader: TJsonLoader; //用来加载JSON文件用的LOADER
		_loader: TTextureLoader;  //用来加载纹理数据用的LOADER
		_tileSetArray: TStack<TTileSet>; //用来存放还需要哪些儿纹理等待加载
		_currTileSet: TTileSet; //正在加载的纹理需要的数据源
		_completeHandler: TNotifyEvent; //地图创建完成的回调函数
		//用来裁剪块的区域（有当前视口和上次视口显示多少的块，就能哪些儿块需要显示或隐藏
		_mapRect:GRect = new GRect(); //当前视口显示的块范围
		_mapLogicRect:GRect = new GRect(); //当前视口显示的范围
		_mapLastRect:GRect = new GRect(); //上次视口显示的块范围
		_index:int = 0;
		_animationDic:Object = {}; //需要创建的动画数据
		_properties:*; //当前地图的自定义属性
		_tileProperties:Object = { }; //图块属性
		_tileProperties2:Object = { };
		//默认的地图类型（具体要看JSON文件）
		_orientation:String = "orthogonal";
		//默认的tile渲染顺序（具体要看JSON文件）
		_renderOrder:String = "right-down";
		//调试用的颜色组合
		_colorArray: array of string = ('FF', '00', '33', '66');
		//缩放相关的操作
		_scale: Single = 1;
		_pivotScaleX: Single = 0.5;
		_pivotScaleY: Single = 0.5;
		_centerX: Single = 0;
		_centerY: Single = 0;
  public
		_viewPortX: Single = 0;
		_viewPortY: Single = 0;
		_viewPortWidth: Single = 0;
		_viewPortHeight: Single = 0;
		//是否开启线性取样
		_enableLinear:Boolean = true;
		//资源的相对路径
		_resPath:String;
		_pathArray:Array;
		//把地图限制在显示区域
		_limitRange:Boolean = false;
		//快速更新模式是否不可用
		_fastDirty:Boolean = true;
		//是否自动缓存没有动画的地块
		autoCache:Boolean = true;
		//自动缓存类型,地图较大时建议使用normal
		autoCacheType: string = 'normal';
		//是否合并图层,开启合并图层时，图层属性内可添加layer属性，运行时将会将相邻的layer属性相同的图层进行合并以提高性能
		enableMergeLayer:Boolean = false;
		//是否移除被覆盖的格子,地块可添加type属性，type不为0时表示不透明，被不透明地块遮挡的地块将会被剔除以提高性能
		removeCoveredTile:Boolean = false;
		//是否显示大格子里显示的贴图数量
		showGridTextureCount:Boolean = false;
		//是否调整地块边缘消除缩放导致的缝隙
		antiCrack:Boolean = true;
		//是否在加载完成之后cache所有大格子
		cacheAllAfterInit:Boolean = false;
  public
    constructor Create;
    destructor Destroy; override;

		/// <summary>
		///   创建地图
    /// </summary>
    /// <params>
    ///   @param	mapName 		JSON文件名字
    ///   @param	viewRect 		视口区域
    ///   @param	completeHandler 地图创建完成的回调函数
    ///   @param	viewRectPadding 视口扩充区域，把视口区域上、下、左、右扩充一下，防止视口移动时的穿帮
    ///   @param	gridSize 		grid大小
    ///   @param	enableLinear 	是否开启线性取样（为false时，可以解决地图黑线的问题，但画质会锐化）
    ///   @param	limitRange		把地图限制在显示区域
    /// <params>
		function createMap(const mapName: string;
                       const viewRect: TIntRect;
                       const completeHandler: TNotifyEvent;
                       const viewRectPadding: TIntRect;
                       const gridSize: TPoint2i;
                       const enableLinear: Boolean = True;
                       const limitRange: Boolean = False): Boolean;
  end;

implementation

{ TTiledMap }

constructor TTiledMap.Create;
begin
  inherited;
  _x       := 0;
  _y       := 0;
  _width   := 0;
  _height  := 0;
  _mapW    := 0;
  _mapH    := 0;
  _mapTileW:= 0;
  _mapTileH:= 0;

  _showGridKey:= False;
  _totalGridNum:= 0;
  _gridW:= 0;
  _gridH:= 0;
  _gridWidth:= 450;
  _gridHeight:= 450;


end;

destructor TTiledMap.Destroy;
begin

  inherited;
end;

function TTiledMap.createMap(const mapName: string; const viewRect: TIntRect;
  const completeHandler: TNotifyEvent; const viewRectPadding: TIntRect;
  const gridSize: TPoint2i; const enableLinear, limitRange: Boolean): Boolean;
begin
  _enableLinear = enableLinear;
  _limitRange = limitRange;
  _rect.x = viewRect.x;
  _rect.y = viewRect.y;
  _rect.width = viewRect.width;
  _rect.height = viewRect.height;
  _viewPortWidth = viewRect.width / _scale;
  _viewPortHeight = viewRect.height / _scale;
  _completeHandler = completeHandler;
  if (viewRectPadding) {
    _paddingRect.copyFrom(viewRectPadding);
  }
  else {
    _paddingRect.setTo(0, 0, 0, 0);
  }
  if (gridSize) {
    _gridWidth = gridSize.x;
    _gridHeight = gridSize.y;
  }
  var tIndex:int = mapName.lastIndexOf("/");
  if (tIndex > -1) {
    _resPath = mapName.substr(0, tIndex);
    _pathArray = _resPath.split("/");
  }
  else {
    _resPath = "";
    _pathArray = [];
  }

  _jsonLoader = new Loader();
  _jsonLoader.once("complete", this, onJsonComplete);
  _jsonLoader.load(mapName, Loader.JSON, false);
end;

end.
