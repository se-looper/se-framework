unit editor.controls;

interface

uses
  System.UITypes, System.Classes, System.SysUtils, System.Generics.Collections,
  System.Rtti, System.Types,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.TreeView, FMX.StdCtrls, FMX.Graphics,
  PXL.Types, se.game.assets, se.game.exlib.ui,
  editor.types, editor.Selection;

type
  TEditorForm = class;
  TEditorControlClass = class of TEditorControl;
  TEditorControl = class(TComponent)
  private
    FChilds: TList<TComponent>;
    FSelection: TOneSelection;
    procedure AddChild(const AComponent: TComponent);
    procedure ResetImage(const ADest: TBitmap; const ASourceData: TFormatedImage);
  private
    FOwner: TControl;
    FParent: TEditorControl;
    FNode: TTreeViewItem;
    FID: Integer;
    FOnClick: TNotifyEvent;
    FSelected: Boolean;
    FUIControl: TUIControl;
    FPropertyList: TPropertyList;
    FControlType: TEditorControlType;
    FCutState: Boolean;
    procedure SetParent(const Value: TEditorControl);
    procedure SetID(const Value: Integer);
    procedure SetSelected(const Value: Boolean);
    procedure SetOnClick(const Value: TNotifyEvent);
    function GetProperty(const Index: Integer): TProperty;
    function GetPropertyCount: Integer;
  private
    FLeft, FTop, FWidth, FHeight: Single;
    FEnabled, FVisible, FLocked: Boolean;
    function GetName: string;
    procedure SetName(const Value: string);
    procedure SetLeft(const Value: Single);
    procedure SetTop(const Value: Single);
    procedure SetWidth(const Value: Single);
    procedure SetHeight(const Value: Single);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetLocked(const Value: Boolean);
    function GetOwnerForm: TEditorForm;
    procedure SetCutState(const Value: Boolean);
  protected
    procedure DoClick(Sender: TObject); virtual;
    procedure DoResize(Sender: TObject); virtual;
    procedure DoMove(Sender: TObject); virtual;
    procedure Assign(const AAControl: TUIControl); virtual;
  public
    constructor Create(AOwner: TControl); virtual;
    destructor Destroy; override;
    procedure AssignFrom(const AAControl: TUIControl); virtual;

    function Clone: TEditorControl; virtual;

    procedure Bind(const ANode: TTreeViewItem);
    procedure Select;

    property Parent: TEditorControl read FParent write SetParent;
    property ID: Integer read FID write SetID;

    property Selected: Boolean read FSelected write SetSelected;
    property UIControl: TUIControl read FUIControl;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;

    property &Property[const Index: Integer]: TProperty read GetProperty;
    property PropertyCount: Integer read GetPropertyCount;
    property ControlType: TEditorControlType read FControlType;
    property BindNode: TTreeViewItem read FNode;
    property Locked: Boolean read FLocked write SetLocked;
    property OwnerForm: TEditorForm read GetOwnerForm;
    property CutState: Boolean read FCutState write SetCutState;
  published
    property Name   : string read GetName write SetName;
    property Left   : Single read FLeft write SetLeft;
    property Top    : Single read FTop write SetTop;
    property Width  : Single read FWidth write SetWidth;
    property Height : Single read FHeight write SetHeight;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TEditorForm = class(TEditorControl)
  private
    FBGImage: TImage;
    function GetAForm: TUIForm;
  protected
    procedure Assign(const AAControl: TUIControl);
  private
    FCanMove: Boolean;
    FImage: string;
    procedure SetCanMove(const Value: Boolean);
    procedure SetImage(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateAForm(AOwner: TComponent): TUIForm;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AForm: TUIForm read GetAForm;
  published
    property Image: string read FImage write SetImage;
    property CanMove: Boolean read FCanMove write SetCanMove;
  end;

  TEditorButton = class(TEditorControl)
  private
    FBGImage: TImage;
    FCaptionLabel: TLabel;
    function GetAButton: TUIButton;
    procedure Assign(const AAControl: TUIControl);
  private
    FImage: string;
    FImageHover: string;
    FImagePressed: string;
    FImageDisabled: string;
    FCaption: string;
    FTransparent: Boolean;
    FFontName: string;
    FFontStyle: string;
    FFontColor: TEditorColor;
    FFontSize: Word;
    FTextHorzAlign: string;
    FTextVertAlign: string;
    procedure SetImage(const Value: string);
    procedure SetImageHover(const Value: string);
    procedure SetImagePressed(const Value: string);
    procedure SetImageDisabled(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetFontName(const Value: string);
    procedure SetTransparent(const Value: Boolean);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontStyle(const Value: string);
    procedure SetFontSize(const Value: Word);
    procedure SetTextHorzAlign(const Value: string);
    procedure SetTextVertAlign(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateAButton(AOwner: TComponent): TUIButton;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AButton: TUIButton read GetAButton;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: string read FImage write SetImage;
    property ImageHover: string read FImageHover write SetImageHover;
    property ImagePressed: string read FImagePressed write SetImagePressed;
    property ImageDisabled: string read FImageDisabled write SetImageDisabled;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property TextHorzAlign: string read FTextHorzAlign write SetTextHorzAlign;
    property TextVertAlign: string read FTextVertAlign write SetTextVertAlign;
  end;

  TEditorLabel = class(TEditorControl)
  private
    FBGImage: TImage;
    FCaptionLabel: TLabel;
    function GetALabel: TUILabel;
    procedure Assign(const AAControl: TUIControl);
  private
    FImage: string;
    FTransparent: Boolean;
    FFontStyle: string;
    FFontName: string;
    FCaption: string;
    FWordWrap: Boolean;
    FFontColor: TEditorColor;
    FFontSize: Word;
    FTextHorzAlign: string;
    FTextVertAlign: string;
    procedure SetImage(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontName(const Value: string);
    procedure SetFontStyle(const Value: string);
    procedure SetTransparent(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetFontSize(const Value: Word);
    procedure SetTextHorzAlign(const Value: string);
    procedure SetTextVertAlign(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateALabel(AOwner: TComponent): TUILabel;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property ALabel: TUILabel read GetALabel;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: string read FImage write SetImage;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property TextHorzAlign: string read FTextHorzAlign write SetTextHorzAlign;
    property TextVertAlign: string read FTextVertAlign write SetTextVertAlign;
  end;

  TEditorEditBox = class(TEditorControl)
  private
    FBGImage: TImage;
    FEditTextLabel: TLabel;
    function GetAEditBox: TUIEdit;
    procedure Assign(const AAControl: TUIControl);
  private
    FImage: string;
    FMaxLength: Integer;
    FAutoSelect: Boolean;
    FFontStyle: string;
    FFontName: string;
    FText: string;
    FReadOnly: Boolean;
    FFontColor: TEditorColor;
    FFontSize: Word;
    FTextHorzAlign: string;
    FTextVertAlign: string;
    procedure SetImage(const Value: string);
    procedure SetAutoSelect(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontName(const Value: string);
    procedure SetFontStyle(const Value: string);
    procedure SetMaxLength(const Value: Integer);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetFontSize(const Value: Word);
    procedure SetTextHorzAlign(const Value: string);
    procedure SetTextVertAlign(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateAEditBox(AOwner: TComponent): TUIEdit;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AEditBox: TUIEdit read GetAEditBox;
  published
    property Text: string read FText write SetText;
    property Image: string read FImage write SetImage;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property MaxLength: Integer read FMaxLength write SetMaxLength;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property AutoSelect: Boolean read FAutoSelect write SetAutoSelect;
    property TextHorzAlign: string read FTextHorzAlign write SetTextHorzAlign;
    property TextVertAlign: string read FTextVertAlign write SetTextVertAlign;
  end;

  TEditorImage = class(TEditorControl)
  private
    FBGImage: TImage;
    function GetAImage: TUIImage;
    procedure Assign(const AAControl: TUIControl);
  private
    FImage: string;
    procedure SetImage(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateAImage(AOwner: TComponent): TUIImage;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AImage: TUIImage read GetAImage;
  published
    property Image: string read FImage write SetImage;
  end;

  TEditorListBox = class(TEditorControl)
  private
    FBGImage, FBGScrollImage: TImage;
    FDownButtonImage, FUpButtonImage, FScrollButtonImage: TImage;
    FTransparent: Boolean;
    FItemIndex: Integer;
    FFontSize: Word;
    FFontStyle: string;
    FImage: string;
    FFontName: string;
    FLineHeight: Integer;
    FFontColor: TEditorColor;
    FImageDownButton: string;
    FImageUpButton: string;
    FImageScrollButton: string;
    function GetAListBox: TUIListBox;
    procedure Assign(const AAControl: TUIControl);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Word);
    procedure SetFontStyle(const Value: string);
    procedure SetImage(const Value: string);
    procedure SetItemIndex(const Value: Integer);
    procedure SetLineHeight(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetImageDownButton(const Value: string);
    procedure SetImageScrollButton(const Value: string);
    procedure SetImageUpButton(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateAListBox(AOwner: TComponent): TUIListBox;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AListBox: TUIListBox read GetAListBox;
  published
    property Image: string read FImage write SetImage;
    property ImageDownButton: string read FImageDownButton write SetImageDownButton;
    property ImageUpButton: string read FImageUpButton write SetImageUpButton;
    property ImageScrollButton: string read FImageScrollButton write SetImageScrollButton;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property LineHeight: Integer read FLineHeight write SetLineHeight;
    property ItemIndex: Integer read FItemIndex write SetItemIndex;
    property Transparent: Boolean read FTransparent write SetTransparent;
//    property ImageDownButtonHover: string read FImageDownButtonHover write SetImageDownButtonHover;
//    property ImageDownButtonPressed: string read FImageDownButtonPressed write SetImageDownButtonPressed;
//    property ImageDownButtonDisabled: string read FImageDownButtonDisabled write SetImageDownButtonDisabled;
//
//    property ImageUpButtonHover: string read FImageUpButtonHover write SetImageUpButtonHover;
//    property ImageUpButtonPressed: string read FImageUpButtonPressed write SetImageUpButtonPressed;
//    property ImageUpButtonDisabled: string read FImageUpButtonDisabled write SetImageUpButtonDisabled;
//
//    property ImageScrollButtonHover: string read FImageScrollButtonHover write SetImageScrollButtonHover;
//    property ImageScrollButtonPressed: string read FImageScrollButtonPressed write SetImageScrollButtonPressed;
//    property ImageScrollButtonDisabled: string read FImageScrollButtonDisabled write SetImageScrollButtonDisabled;
  end;

  TEditorCheckBox = class(TEditorControl)
  private
    FBGImage, FBoxImage: TImage;
    FCaptionLabel: TLabel;
    function GetACheckBox: TUICheckBox;
    procedure Assign(const AAControl: TUIControl);
    procedure ResetBoxImage;
  private
    FImage: string;
    FImageBox: string;
    FImageBoxChecked: string;
    FFontStyle: string;
    FFontName: string;
    FCaption: string;
    FFontColor: TEditorColor;
    FTransparent: Boolean;
    FReadOnly: Boolean;
    FChecked: Boolean;
    FFontSize: Word;
    procedure SetImage(const Value: string);
    procedure SetImageBoxChecked(const Value: string);
    procedure SetImageBox(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Word);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontStyle(const Value: string);
    procedure SetChecked(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure DoResize(Sender: TObject); override;
  public
    constructor Create(AOwner: TControl); override;
    function CreateACheckBox(AOwner: TComponent): TUICheckBox;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property ACheckBox: TUICheckBox read GetACheckBox;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: string read FImage write SetImage;
    property ImageBox: string read FImageBox write SetImageBox;
    property ImageBoxChecked: string read FImageBoxChecked write SetImageBoxChecked;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property Checked: Boolean read FChecked write SetChecked;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TEditorRadioButton = class(TEditorControl)
  private
    FBGImage, FBoxImage: TImage;
    FCaptionLabel: TLabel;
    function GetARadioButton: TUIRadioButton;
    procedure Assign(const AAControl: TUIControl);
    procedure ResetBoxImage;
  private
    FImage: string;
    FImageBox: string;
    FImageBoxChecked: string;
    FFontStyle: string;
    FFontName: string;
    FCaption: string;
    FFontColor: TEditorColor;
    FTransparent: Boolean;
    FReadOnly: Boolean;
    FChecked: Boolean;
    FFontSize: Word;
    procedure SetImage(const Value: string);
    procedure SetImageBoxChecked(const Value: string);
    procedure SetImageBox(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Word);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontStyle(const Value: string);
    procedure SetChecked(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure DoResize(Sender: TObject); override;
  public
    constructor Create(AOwner: TControl); override;
    function CreateARadioButton(AOwner: TComponent): TUIRadioButton;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property ARadioButton: TUIRadioButton read GetARadioButton;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: string read FImage write SetImage;
    property ImageBox: string read FImageBox write SetImageBox;
    property ImageBoxChecked: string read FImageBoxChecked write SetImageBoxChecked;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property Checked: Boolean read FChecked write SetChecked;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TEditorProgressBar = class(TEditorControl)
  private
    FBGImage, FPositionImage: TImage;
    FCaptionLabel: TLabel;
    function GetAProgressBar: TUIProgressBar;
    procedure Assign(const AAControl: TUIControl);
    procedure ResetProgress;
    procedure ResetProgressDisplay;
  private
    FImage: string;
    FImagePosition: string;
    FMax: Integer;
    FFontSize: Word;
    FFontStyle: string;
    FMin: Integer;
    FFontName: string;
    FCaption: string;
    FFontColor: TEditorColor;
    FPosition: Integer;
    FTransparent: Boolean;
    FDisplay: string;
    procedure SetImage(const Value: string);
    procedure SetImagePosition(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetFontColor(const Value: TEditorColor);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: Word);
    procedure SetFontStyle(const Value: string);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
    procedure SetTransparent(const Value: Boolean);
    procedure SetDisplay(const Value: string);
  public
    constructor Create(AOwner: TControl); override;
    function CreateAProgressBar(AOwner: TComponent): TUIProgressBar;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AProgressBar: TUIProgressBar read GetAProgressBar;
  published
    property Caption: string read FCaption write SetCaption;
    property Image: string read FImage write SetImage;
    property ImagePosition: string read FImagePosition write SetImagePosition;
    property FontName: string read FFontName write SetFontName;
    property FontSize: Word read FFontSize write SetFontSize;
    property FontStyle: string read FFontStyle write SetFontStyle;
    property FontColor: TEditorColor read FFontColor write SetFontColor;
    property Display: string read FDisplay write SetDisplay;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property Position: Integer read FPosition write SetPosition;
    property Transparent: Boolean read FTransparent write SetTransparent;
  end;

  TEditorTrackBar = class(TEditorControl)
  private
    FBGImage: TImage;
  private
    function GetATrackBar: TUITrackBar;
  public
    constructor Create(AOwner: TControl); override;
    function CreateATrackBar(AOwner: TComponent): TUITrackBar;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property TrackBar: TUITrackBar read GetATrackBar;
  end;

  TEditorMemo = class(TEditorControl)
  private
    FBGImage: TImage;
  private
    function GetAMemo: TUIMemo;
  public
    constructor Create(AOwner: TControl); override;
    function CreateAMemo(AOwner: TComponent): TUIMemo;
    procedure AssignFrom(const AAControl: TUIControl); override;

    function Clone: TEditorControl; override;

    property AMemo: TUIMemo read GetAMemo;
  end;

implementation

{$REGION 'TEditorControl'}

{ TEditorControl }

constructor TEditorControl.Create(AOwner: TControl);
begin
  inherited Create(nil);
  FOwner:= AOwner;
  FSelection:= TOneSelection.Create(AOwner);
  FSelection.Parent:= AOwner;
  FSelection.GripSize := 5;
  FSelection.Proportional := False;
  FSelection.OnClick:= DoClick;
  FSelection.OnResize:= DoResize;
  FSelection.OnTrack:= DoMove;
  FChilds:= TList<TComponent>.Create;
  FUIControl:= nil;
  //
  FPropertyList.Add('Name', '', TPropertyType.ptString);
  FPropertyList.Add('Left', 0);
  FPropertyList.Add('Top', 0);
  FPropertyList.Add('Width', 128);
  FPropertyList.Add('Height', 128);
  FPropertyList.Add('Enabled', True);
  FPropertyList.Add('Visible', True);
end;

destructor TEditorControl.Destroy;
begin
  FChilds.Free;
  FOwner.RemoveObject(FSelection);
  FNode:= nil;
  FUIControl:= nil;
  inherited;
end;

procedure TEditorControl.AddChild(const AComponent: TComponent);
begin
  FChilds.Add(AComponent);
end;

procedure TEditorControl.Assign(const AAControl: TUIControl);
begin
  Self.Name:= AAControl.Name;
  Self.Left:= AAControl.Left;
  Self.Top:= AAControl.Top;
  Self.Width:= AAControl.Width;
  Self.Height:= AAControl.Height;
  Self.Enabled:= AAControl.Enabled;
  Self.Visible:= AAControl.Visible;
  FUIControl:= AAControl;
end;

procedure TEditorControl.AssignFrom(const AAControl: TUIControl);
begin
  Self.Assign(AAControl);
end;

function TEditorControl.Clone: TEditorControl;
begin
  Result:= TEditorControlClass(Self.ClassType).Create(FOwner);
  Result.Left   := Self.Left;
  Result.Top    := Self.Top;
  Result.Width  := Self.Width;
  Result.Height := Self.Height;
  Result.Enabled:= Self.Enabled;
  Result.Visible:= Self.Visible;
end;

procedure TEditorControl.DoClick(Sender: TObject);
begin
  FNode.Select;
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TEditorControl.DoResize(Sender: TObject);
begin
  if Assigned(FUIControl) then
  begin
    Self.Width:= Round(FSelection.Width);
    Self.Height:= Round(FSelection.Height);
  end;
end;

function TEditorControl.GetProperty(const Index: Integer): TProperty;
begin
  Result:= FPropertyList.Properties[Index];
end;

function TEditorControl.GetPropertyCount: Integer;
begin
  Result:= FPropertyList.Count;
end;

procedure TEditorControl.ResetImage(const ADest: TBitmap;
  const ASourceData: TFormatedImage);
var
  LImageFile: string;
  LBitmap: TBitmap;
begin
  LImageFile:= AssetManager.RequireFile(ASourceData.Image);
  if FileExists(LImageFile) then  
  begin
    LBitmap:= TBitmap.Create;
    try
      LBitmap.LoadFromFile(LImageFile);
      ADest.Width:= ASourceData.Width;
      ADest.Height:= ASourceData.Height;
      ADest.CopyFromBitmap(LBitmap, ASourceData.DRect, 0, 0);
    finally
      LBitmap.Free;
    end;
  end;
end;

procedure TEditorControl.DoMove(Sender: TObject);
begin
  if Assigned(FUIControl) then
  begin
    Self.Left:= Round(FSelection.Position.X);
    Self.Top:= Round(FSelection.Position.Y);
  end;
end;

procedure TEditorControl.Bind(const ANode: TTreeViewItem);
begin
  FNode:= ANode;
end;

procedure TEditorControl.Select;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

function TEditorControl.GetOwnerForm: TEditorForm;
var
  LControl: TEditorControl;
begin
  LControl:= Self;
  while not (LControl is TEditorForm) do
    LControl:= LControl.Parent;
  Result:= TEditorForm(LControl);
end;

procedure TEditorControl.SetID(const Value: Integer);
begin
  FID := Value;
  FSelection.Tag:= FID;
end;

procedure TEditorControl.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TEditorControl.SetParent(const Value: TEditorControl);
begin
  FParent := Value;
  if Assigned(FParent) then
  begin
    FParent.AddChild(Self);
    Self.FSelection.Parent:= FParent.FSelection;
  end;
end;

procedure TEditorControl.SetSelected(const Value: Boolean);
var
  LOnTreeViewChange: TNotifyEvent;
begin
  FSelected := Value;
  FSelection.HideSelection:= not FSelected;
  FSelection.ShowHandles:= FSelected;
  //
  LOnTreeViewChange:= FNode.TreeView.OnChange;
  try
    FNode.TreeView.OnChange:= nil;
    FNode.IsSelected:= FSelected;
  finally
    FNode.TreeView.OnChange:= LOnTreeViewChange;
  end;
end;

procedure TEditorControl.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
  FSelection.CanEdit:= not FLocked;
end;

procedure TEditorControl.SetCutState(const Value: Boolean);
begin
  FCutState:= Value;
  if FCutState then
    FSelection.Opacity:= 0.5
  else
    FSelection.Opacity:= 1.0;
end;

function TEditorControl.GetName: string;
begin
  if Assigned(FUIControl) then
    Result:= FUIControl.Name;
end;

procedure TEditorControl.SetName(const Value: string);
begin
  FPropertyList.Value['Name']:= Value;
  if Assigned(FUIControl) then
    FUIControl.Name:= Value;
end;

procedure TEditorControl.SetLeft(const Value: Single);
begin
  FLeft := Value;
  FPropertyList.Value['Left']:= FLeft.ToString;
  FSelection.Position.X:= FLeft;
  if Assigned(FUIControl) then
    FUIControl.Left:= Trunc(FLeft);
end;

procedure TEditorControl.SetTop(const Value: Single);
begin
  FTop := Value;
  FPropertyList.Value['Top']:= FTop.ToString;
  FSelection.Position.Y:= FTop;
  if Assigned(FUIControl) then
    FUIControl.Top:= Trunc(FTop);
end;

procedure TEditorControl.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  FPropertyList.Value['Visible']:= FVisible.ToString(TUseBoolStrs.True);
  FSelection.Visible:= FVisible;
  if Assigned(FUIControl) then
    FUIControl.Visible:= FVisible;
end;

procedure TEditorControl.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  FPropertyList.Value['Enabled']:= FEnabled.ToString(TUseBoolStrs.True);
  FSelection.Enabled:= FEnabled;
  if Assigned(FUIControl) then
    FUIControl.Enabled:= FEnabled;
end;

procedure TEditorControl.SetHeight(const Value: Single);
begin
  FHeight := Value;
  FPropertyList.Value['Height']:= FHeight.ToString;
  FSelection.Height:= FHeight;
  if Assigned(FUIControl) then
    FUIControl.Height:= Trunc(FHeight);
end;

procedure TEditorControl.SetWidth(const Value: Single);
begin
  FWidth := Value;
  FPropertyList.Value['Width']:= FWidth.ToString;
  FSelection.Width:= FWidth;
  if Assigned(FUIControl) then
    FUIControl.Width:= Trunc(FWidth);
end;

{$ENDREGION}

{$REGION 'TEditorForm'}

{ TEditorForm }

constructor TEditorForm.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectForm;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
  FPropertyList.Add('CanMove', False);
end;

function TEditorForm.CreateAForm(AOwner: TComponent): TUIForm;
begin
  inherited Assign(TUIForm.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.AForm;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.Shadow:= False;
end;

procedure TEditorForm.Assign(const AAControl: TUIControl);
begin
  Self.Image:= TUIForm(AAControl).Image.Formatting;
  Self.CanMove:= TUIForm(AAControl).CanMove;
end;

procedure TEditorForm.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorForm.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorForm(Result).CreateAForm(Self.Parent.UIControl);
  TEditorForm(Result).Image:= FImage;
  TEditorForm(Result).CanMove:= FCanMove;
end;

function TEditorForm.GetAForm: TUIForm;
begin
  Result:= TUIForm(FUIControl);
end;

procedure TEditorForm.SetCanMove(const Value: Boolean);
begin
  FCanMove := Value;
  FPropertyList.Value['CanMove']:= FCanMove.ToString(TUseBoolStrs.True);
  Self.AForm.CanMove:= FCanMove;
end;

procedure TEditorForm.SetImage(const Value: string);
var
  LImageFile: string;
  LBitmap: TBitmap;
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.AForm.Image.ParseFrom(FImage);
  LImageFile:= AssetManager.RequireFile(Self.AForm.Image.Image);
  if FileExists(LImageFile) then
  begin
    LBitmap:= TBitmap.Create;
    try
      LBitmap.LoadFromFile(LImageFile);
      FBGImage.Bitmap.Width:= Self.AForm.Image.Width;
      FBGImage.Bitmap.Height:= Self.AForm.Image.Height;
      FBGImage.Bitmap.CopyFromBitmap(LBitmap, Self.AForm.Image.DRect, 0, 0);
    finally
      LBitmap.Free;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TEditorButton'}

{ TEditorButton }

constructor TEditorButton.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectButton;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FCaptionLabel:= TLabel.Create(FSelection);
  FCaptionLabel.Parent:= FSelection;
  FCaptionLabel.Align:= TAlignLayout.Client;
  FCaptionLabel.HitTest:= False;
  FCaptionLabel.StyledSettings:= [];
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign.Center;
  //
  FPropertyList.Add('Caption',  '', TPropertyType.ptString);
  FPropertyList.Add('Image',  '', TPropertyType.ptImage);
  FPropertyList.Add('ImageHover',  '', TPropertyType.ptImage);
  FPropertyList.Add('ImagePressed',  '', TPropertyType.ptImage);
  FPropertyList.Add('ImageDisabled',  '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('FFFFFFFF');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add<TTextAlign>('TextHorzAlign', TTextAlign.taCenter);
  FPropertyList.Add<TTextAlign>('TextVertAlign', TTextAlign.taCenter);
  FPropertyList.Add('Transparent', False);
end;

function TEditorButton.CreateAButton(AOwner: TComponent): TUIButton;
begin
  inherited Assign(TUIButton.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.AButton;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.Shadow:= False;
  Result.FocusRect:= TFocusRect.fNone;
  Result.Font.Name:= 'simsun';
end;

procedure TEditorButton.Assign(const AAControl: TUIControl);
begin
  Self.Caption:= TUIButton(AAControl).Caption;
  Self.Image:= TUIButton(AAControl).Image.Formatting;
  Self.ImageHover:= TUIButton(AAControl).ImageHover.Formatting;
  Self.ImagePressed:= TUIButton(AAControl).ImagePressed.Formatting;
  Self.ImageDisabled:= TUIButton(AAControl).ImageDisabled.Formatting;
  Self.FontName:= TUIButton(AAControl).Font.Name;
  Self.FontSize:= TUIButton(AAControl).Font.Size;
  Self.FontColor:= TUIButton(AAControl).Font.Color.First;
  Self.FontStyle:= TUIButton(AAControl).Font.StyleToString;
  Self.TextHorzAlign:= AAControl.EnumToString<TTextAlign>(TUIButton(AAControl).TextHorzAlign);
  Self.TextVertAlign:= AAControl.EnumToString<TTextAlign>(TUIButton(AAControl).TextVertAlign);
  Self.Transparent:= TUIButton(AAControl).Transparent;
end;

procedure TEditorButton.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorButton.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorButton(Result).CreateAButton(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorButton(Result).Caption:= Self.Caption;
  TEditorButton(Result).Image:= Self.Image;
  TEditorButton(Result).ImageHover:= Self.ImageHover;
  TEditorButton(Result).ImagePressed:= Self.ImagePressed;
  TEditorButton(Result).ImageDisabled:= Self.ImageDisabled;
  TEditorButton(Result).FontName:= Self.FontName;
  TEditorButton(Result).FontSize:= Self.FontSize;
  TEditorButton(Result).FontColor:= Self.FontColor;
  TEditorButton(Result).FontStyle:= Self.FontStyle;
  TEditorButton(Result).TextHorzAlign:=Self.TextHorzAlign;
  TEditorButton(Result).TextVertAlign:= Self.TextVertAlign;
  TEditorButton(Result).Transparent:= Self.Transparent;
end;

function TEditorButton.GetAButton: TUIButton;
begin
  Result:= TUIButton(FUIControl);
end;

procedure TEditorButton.SetCaption(const Value: string);
begin
  FCaption := Value;
  FPropertyList.Value['Caption']:= FCaption;
  Self.AButton.Caption:= FCaption;
  FCaptionLabel.Text:= FCaption;
end;

procedure TEditorButton.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.AButton.Font.Name:= FFontName;
  FCaptionLabel.Font.Family:= 'ÑÅºÚ';
end;

procedure TEditorButton.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.AButton.Font.Size:= FFontSize;
  FCaptionLabel.Font.Size:= FFontSize;
end;

procedure TEditorButton.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.AButton.Font.ChangeStyle(FFontStyle);
  FCaptionLabel.Font.Style:= Self.AButton.Font.Style;
end;

procedure TEditorButton.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.AButton.Font.Color:= ColorPair(FFontColor, FFontColor);
  FCaptionLabel.FontColor:= FFontColor;
end;

procedure TEditorButton.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.AButton.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.AButton.Image);
end;

procedure TEditorButton.SetImageHover(const Value: string);
begin
  FImageHover := Value;
  FPropertyList.Value['ImageHover']:= FImageHover;
  Self.AButton.ImageHover.ParseFrom(FImageHover);
end;

procedure TEditorButton.SetImagePressed(const Value: string);
begin
  FImagePressed := Value;
  FPropertyList.Value['ImagePressed']:= FImagePressed;
  Self.AButton.ImagePressed.ParseFrom(FImagePressed);
end;

procedure TEditorButton.SetImageDisabled(const Value: string);
begin
  FImageDisabled := Value;
  FPropertyList.Value['ImageDisabled']:= FImageDisabled;
  Self.AButton.ImageDisabled.ParseFrom(FImageDisabled);
end;

procedure TEditorButton.SetTextHorzAlign(const Value: string);
begin
  FTextHorzAlign := Value;
  FPropertyList.Value['TextHorzAlign']:= FTextHorzAlign;
  Self.AButton.ChangeTextHorzAlign(FTextHorzAlign);
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign(Self.AButton.TextHorzAlign);
end;

procedure TEditorButton.SetTextVertAlign(const Value: string);
begin
  FTextVertAlign := Value;
  FPropertyList.Value['TextVertAlign']:= FTextVertAlign;
  Self.AButton.ChangeTextVertAlign(FTextVertAlign);
  FCaptionLabel.TextSettings.VertAlign:= FMX.Types.TTextAlign(Self.AButton.TextVertAlign);
end;

procedure TEditorButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FPropertyList.Value['Transparent']:= FTransparent.ToString(TUseBoolStrs.True);
  Self.AButton.Transparent:= FTransparent;
end;

{$ENDREGION}

{$REGION 'TEditorLabel'}

{ TEditorLabel }

constructor TEditorLabel.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectLabel;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FCaptionLabel:= TLabel.Create(FSelection);
  FCaptionLabel.Parent:= FSelection;
  FCaptionLabel.Align:= TAlignLayout.Client;
  FCaptionLabel.HitTest:= False;
  FCaptionLabel.AutoSize:= True;
  FCaptionLabel.StyledSettings:= [];
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign.Center;
  FCaptionLabel.TextSettings.WordWrap:= False;
  //
  FPropertyList.Add('Caption', '', TPropertyType.ptString);
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add<TTextAlign>('TextHorzAlign', TTextAlign.taLeading);
  FPropertyList.Add<TTextAlign>('TextVertAlign', TTextAlign.taCenter);
  FPropertyList.Add('Transparent', True);
  FPropertyList.Add('WordWrap', False);
end;

function TEditorLabel.CreateALabel(AOwner: TComponent): TUILabel;
begin
  inherited Assign(TUILabel.Create(AOwner));
  Result:= Self.ALabel;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.Font.Name:= 'simsun';
  Result.CanMoveHandle:= False;
  Self.Assign(FUIControl);
end;

procedure TEditorLabel.Assign(const AAControl: TUIControl);
begin
  Self.Caption:= TUILabel(AAControl).Caption;
  Self.Image:= TUILabel(AAControl).Image.Formatting;
  Self.FontName:= TUILabel(AAControl).Font.Name;
  Self.FontSize:= TUILabel(AAControl).Font.Size;
  Self.FontColor:= TUILabel(AAControl).Font.Color.First;
  Self.FontStyle:= TUILabel(AAControl).Font.StyleToString;
  Self.TextHorzAlign:= AAControl.EnumToString<TTextAlign>(TUILabel(AAControl).TextHorzAlign);
  Self.TextVertAlign:= AAControl.EnumToString<TTextAlign>(TUILabel(AAControl).TextVertAlign);
  Self.Transparent:= TUILabel(AAControl).Transparent;
  Self.WordWrap:= TUILabel(AAControl).WordWrap;
end;

procedure TEditorLabel.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorLabel.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorLabel(Result).CreateALabel(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorLabel(Result).Caption:= Self.Caption;
  TEditorLabel(Result).FontName:= Self.FontName;
  TEditorLabel(Result).FontSize:= Self.FontSize;
  TEditorLabel(Result).FontColor:= Self.FontColor;
  TEditorLabel(Result).FontStyle:= Self.FontStyle;
  TEditorLabel(Result).TextHorzAlign:=Self.TextHorzAlign;
  TEditorLabel(Result).TextVertAlign:= Self.TextVertAlign;
  TEditorLabel(Result).Transparent:= Self.Transparent;
  TEditorLabel(Result).WordWrap:= Self.WordWrap;
end;

function TEditorLabel.GetALabel: TUILabel;
begin
  Result:= TUILabel(FUIControl);
end;

procedure TEditorLabel.SetCaption(const Value: string);
begin
  FCaption := Value;
  FPropertyList.Value['Caption']:= FCaption;
  Self.ALabel.Caption:= FCaption;
  FCaptionLabel.Text:= FCaption;
end;

procedure TEditorLabel.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.ALabel.Font.Color:= ColorPair(FFontColor, FFontColor);
  FCaptionLabel.FontColor:= FFontColor;
end;

procedure TEditorLabel.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.ALabel.Font.Name:= FFontName;
  FCaptionLabel.Font.Family:= 'ÑÅºÚ';
end;

procedure TEditorLabel.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.ALabel.Font.Size:= FFontSize;
  FCaptionLabel.Font.Size:= FFontSize;
end;

procedure TEditorLabel.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.ALabel.Font.ChangeStyle(FFontStyle);
  FCaptionLabel.Font.Style:= Self.ALabel.Font.Style;
end;

procedure TEditorLabel.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.ALabel.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.ALabel.Image);
end;

procedure TEditorLabel.SetTextHorzAlign(const Value: string);
begin
  FTextHorzAlign := Value;
  FPropertyList.Value['TextHorzAlign']:= FTextHorzAlign;
  Self.ALabel.ChangeTextHorzAlign(FTextHorzAlign);
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign(Self.ALabel.TextHorzAlign);
end;

procedure TEditorLabel.SetTextVertAlign(const Value: string);
begin
  FTextVertAlign := Value;
  FPropertyList.Value['TextVertAlign']:= FTextVertAlign;
  Self.ALabel.ChangeTextVertAlign(FTextVertAlign);
  FCaptionLabel.TextSettings.VertAlign:= FMX.Types.TTextAlign(Self.ALabel.TextVertAlign);
end;

procedure TEditorLabel.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FPropertyList.Value['Transparent']:= FTransparent.ToString(TUseBoolStrs.True);
  Self.ALabel.Transparent:= FTransparent;
end;

procedure TEditorLabel.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  FPropertyList.Value['WordWrap']:= FWordWrap.ToString(TUseBoolStrs.True);
  Self.ALabel.WordWrap:= FWordWrap;
end;

{$ENDREGION}

{$REGION 'TEditorEditBox'}

{ TEditorEditBox }

constructor TEditorEditBox.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectEditBox;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FEditTextLabel:= TLabel.Create(FSelection);
  FEditTextLabel.Parent:= FSelection;
  FEditTextLabel.Align:= TAlignLayout.Client;
  FEditTextLabel.HitTest:= False;
  FEditTextLabel.StyledSettings:= [];
  //
  FPropertyList.Add('Text', '', TPropertyType.ptString);
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add<TTextAlign>('TextHorzAlign', TTextAlign.taLeading);
  FPropertyList.Add<TTextAlign>('TextVertAlign', TTextAlign.taCenter);
  FPropertyList.Add('MaxLength',  255);
  FPropertyList.Add('ReadOnly', False);
  FPropertyList.Add('AutoSelect', False);
end;

function TEditorEditBox.CreateAEditBox(AOwner: TComponent): TUIEdit;
begin
  inherited Assign(TUIEdit.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.AEditBox;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.FocusRect:= TFocusRect.fNone;
  Result.AutoSelect:= False;
end;

procedure TEditorEditBox.Assign(const AAControl: TUIControl);
begin
  Self.Text:= TUIEdit(AAControl).Text;
  Self.Image:= TUIEdit(AAControl).Image.Formatting;
  Self.FontName:= TUIEdit(AAControl).Font.Name;
  Self.FontSize:= TUIEdit(AAControl).Font.Size;
  Self.FontColor:= TUIEdit(AAControl).Font.Color.First;
  Self.FontStyle:= TUIEdit(AAControl).Font.StyleToString;
  Self.TextHorzAlign:= AAControl.EnumToString<TTextAlign>(TUIEdit(AAControl).TextHorzAlign);
  Self.TextVertAlign:= AAControl.EnumToString<TTextAlign>(TUIEdit(AAControl).TextVertAlign);
  Self.MaxLength:= TUIEdit(AAControl).MaxLength;
  Self.ReadOnly:= TUIEdit(AAControl).ReadOnly;
  Self.AutoSelect:= TUIEdit(AAControl).AutoSelect;
end;

procedure TEditorEditBox.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorEditBox.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorEditBox(Result).CreateAEditBox(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorEditBox(Result).Text:= Self.Text;
  TEditorEditBox(Result).Image:= Self.Image;
  TEditorEditBox(Result).FontName:= Self.FontName;
  TEditorEditBox(Result).FontSize:= Self.FontSize;
  TEditorEditBox(Result).FontColor:= Self.FontColor;
  TEditorEditBox(Result).FontStyle:= Self.FontStyle;
  TEditorEditBox(Result).TextHorzAlign:=Self.TextHorzAlign;
  TEditorEditBox(Result).TextVertAlign:= Self.TextVertAlign;
  TEditorEditBox(Result).MaxLength:= Self.MaxLength;
  TEditorEditBox(Result).ReadOnly:= Self.ReadOnly;
  TEditorEditBox(Result).AutoSelect:= Self.AutoSelect;
end;

function TEditorEditBox.GetAEditBox: TUIEdit;
begin
  Result:= TUIEdit(FUIControl);
end;

procedure TEditorEditBox.SetAutoSelect(const Value: Boolean);
begin
  FAutoSelect := Value;
  FPropertyList.Value['AutoSelect']:= FAutoSelect.ToString(TUseBoolStrs.True);
  Self.AEditBox.AutoSelect:= FAutoSelect;
end;

procedure TEditorEditBox.SetText(const Value: string);
begin
  FText := Value;
  FPropertyList.Value['Text']:= FText;
  Self.AEditBox.Text:= FText;
  FEditTextLabel.Text:= FText;
end;

procedure TEditorEditBox.SetTextHorzAlign(const Value: string);
begin
  FTextHorzAlign := Value;
  FPropertyList.Value['TextHorzAlign']:= FTextHorzAlign;
  Self.AEditBox.ChangeTextHorzAlign(FTextHorzAlign);
  FEditTextLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign(Self.AEditBox.TextHorzAlign);
end;

procedure TEditorEditBox.SetTextVertAlign(const Value: string);
begin
  FTextVertAlign := Value;
  FPropertyList.Value['TextVertAlign']:= FTextVertAlign;
  Self.AEditBox.ChangeTextVertAlign(FTextVertAlign);
  FEditTextLabel.TextSettings.VertAlign:= FMX.Types.TTextAlign(Self.AEditBox.TextVertAlign);
end;

procedure TEditorEditBox.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.AEditBox.Font.Color:= ColorPair(FFontColor, FFontColor);
  FEditTextLabel.FontColor:= FFontColor;
end;

procedure TEditorEditBox.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.AEditBox.Font.Name:= FFontName;
  FEditTextLabel.Font.Family:= 'ÑÅºÚ';
end;

procedure TEditorEditBox.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.AEditBox.Font.Size:= FFontSize;
  FEditTextLabel.Font.Size:= FFontSize;
end;

procedure TEditorEditBox.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.AEditBox.Font.ChangeStyle(FFontStyle);
  FEditTextLabel.Font.Style:= Self.AEditBox.Font.Style;
end;

procedure TEditorEditBox.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.AEditBox.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.AEditBox.Image);
end;

procedure TEditorEditBox.SetMaxLength(const Value: Integer);
begin
  FMaxLength := Value;
  FPropertyList.Value['MaxLength']:= FMaxLength.ToString;
  Self.AEditBox.MaxLength:= FMaxLength;
end;

procedure TEditorEditBox.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  FPropertyList.Value['ReadOnly']:= FReadOnly.ToString(TUseBoolStrs.True);
  Self.AEditBox.ReadOnly:= FReadOnly;
end;

{$ENDREGION}

{$REGION 'TEditorImage'}

{ TEditorImage }

constructor TEditorImage.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectImage;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
end;

function TEditorImage.CreateAImage(AOwner: TComponent): TUIImage;
begin
  inherited Assign(TUIImage.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.AImage;
  Result.Border.Size:= 0;
  Result.CanMoveHandle:= False;
end;

procedure TEditorImage.Assign(const AAControl: TUIControl);
begin
  Self.Image:= TUIImage(AAControl).Image.Formatting;
end;

procedure TEditorImage.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorImage.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorImage(Result).CreateAImage(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorImage(Result).Image:= Self.Image;
end;

function TEditorImage.GetAImage: TUIImage;
begin
  Result:= TUIImage(FUIControl);
end;

procedure TEditorImage.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.AImage.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.AImage.Image);
end;

{$ENDREGION}

{$REGION 'TEditorListBox'}

{ TEditorListBox }

constructor TEditorListBox.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectListBox;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FBGScrollImage:= TImage.Create(FBGImage);
  FBGScrollImage.Parent:= FBGImage;
  FBGScrollImage.Align:= TAlignLayout.Right;
  FBGScrollImage.Width:= 21;
  FBGScrollImage.HitTest:= False;
  FBGScrollImage.WrapMode:= TImageWrapMode.Original;
  //
  FDownButtonImage:= TImage.Create(FBGScrollImage);
  FDownButtonImage.Parent:= FBGScrollImage;
  FDownButtonImage.Align:= TAlignLayout.Bottom;
  FDownButtonImage.Height:= 19;
  FDownButtonImage.HitTest:= False;
  FDownButtonImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FUpButtonImage:= TImage.Create(FBGScrollImage);
  FUpButtonImage.Parent:= FBGScrollImage;
  FUpButtonImage.Align:= TAlignLayout.Top;
  FUpButtonImage.Height:= 19;
  FUpButtonImage.HitTest:= False;
  FUpButtonImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FScrollButtonImage:= TImage.Create(FBGScrollImage);
  FScrollButtonImage.Parent:= FBGScrollImage;
  FScrollButtonImage.Align:= TAlignLayout.Top;
  FScrollButtonImage.Margins.Left:= 1;
  FScrollButtonImage.Margins.Right:= 1;
  FScrollButtonImage.Height:= 19;
  FScrollButtonImage.HitTest:= False;
  FScrollButtonImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FPropertyList.Add('Image',  '', TPropertyType.ptImage);
  FPropertyList.Add('ImageDownButton',  '', TPropertyType.ptImage);
  FPropertyList.Add('ImageUpButton',  '', TPropertyType.ptImage);
  FPropertyList.Add('ImageScrollButton',  '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add('LineHeight', 0);
  FPropertyList.Add('ItemIndex', -1);
  FPropertyList.Add('Transparent', False);
end;

function TEditorListBox.CreateAListBox(AOwner: TComponent): TUIListBox;
begin
  inherited Assign(TUIListBox.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.AListBox;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.FocusRect:= TFocusRect.fNone;
  Result.UpButton.Border.Size:= 0;
  Result.DownButton.Border.Size:= 0;
  Result.ScrollButton.Border.Size:= 0;
end;

procedure TEditorListBox.Assign(const AAControl: TUIControl);
begin
  Self.Image:= TUIListBox(AAControl).Image.Formatting;
  Self.ImageDownButton:= TUIListBox(AAControl).DownButton.Image.Formatting;
  Self.ImageUpButton:= TUIListBox(AAControl).UpButton.Image.Formatting;
  Self.ImageScrollButton:= TUIListBox(AAControl).ScrollButton.Image.Formatting;
  Self.FontName:= TUIListBox(AAControl).Font.Name;
  Self.FontSize:= TUIListBox(AAControl).Font.Size;
  Self.FontColor:= TUIListBox(AAControl).Font.Color.First;
  Self.FontStyle:= TUIListBox(AAControl).Font.StyleToString;
  Self.LineHeight:= TUIListBox(AAControl).LineHeight;
  Self.ItemIndex:= TUIListBox(AAControl).ItemIndex;
  Self.Transparent:= TUIListBox(AAControl).Transparent;
end;

procedure TEditorListBox.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorListBox.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorListBox(Result).CreateAListBox(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorListBox(Result).Image:= Self.Image;
  TEditorListBox(Result).ImageDownButton:= Self.ImageDownButton;
  TEditorListBox(Result).ImageUpButton:= Self.ImageUpButton;
  TEditorListBox(Result).ImageScrollButton:= Self.ImageScrollButton;
  TEditorListBox(Result).FontName:= Self.FontName;
  TEditorListBox(Result).FontSize:= Self.FontSize;
  TEditorListBox(Result).FontColor:= Self.FontColor;
  TEditorListBox(Result).FontStyle:= Self.FontStyle;
  TEditorListBox(Result).LineHeight:= Self.LineHeight;
  TEditorListBox(Result).ItemIndex:= Self.ItemIndex;
  TEditorListBox(Result).Transparent:= Self.Transparent;
end;

function TEditorListBox.GetAListBox: TUIListBox;
begin
  Result:= TUIListBox(FUIControl);
end;

procedure TEditorListBox.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.AListBox.Font.Color:= ColorPair(FFontColor, FFontColor);
end;

procedure TEditorListBox.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.AListBox.Font.Name:= FFontName;
end;

procedure TEditorListBox.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.AListBox.Font.Size:= FFontSize;
end;

procedure TEditorListBox.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.AListBox.Font.ChangeStyle(FFontStyle);
end;

procedure TEditorListBox.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.AListBox.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.AListBox.Image);
end;

procedure TEditorListBox.SetImageDownButton(const Value: string);
begin
  FImageDownButton := Value;
  FPropertyList.Value['ImageDownButton']:= FImageDownButton;
  Self.AListBox.DownButton.Image.ParseFrom(FImageDownButton);
  Self.ResetImage(FDownButtonImage.Bitmap, Self.AListBox.DownButton.Image);
end;

procedure TEditorListBox.SetImageUpButton(const Value: string);
begin
  FImageUpButton := Value;
  FPropertyList.Value['ImageUpButton']:= FImageUpButton;
  Self.AListBox.UpButton.Image.ParseFrom(FImageUpButton);
  Self.ResetImage(FUpButtonImage.Bitmap, Self.AListBox.UpButton.Image);
end;

procedure TEditorListBox.SetImageScrollButton(const Value: string);
begin
  FImageScrollButton := Value;
  FPropertyList.Value['ImageScrollButton']:= FImageScrollButton;
  Self.AListBox.ScrollButton.Image.ParseFrom(FImageScrollButton);
  Self.ResetImage(FScrollButtonImage.Bitmap, Self.AListBox.ScrollButton.Image);
end;

procedure TEditorListBox.SetItemIndex(const Value: Integer);
begin
  FItemIndex := Value;
  FPropertyList.Value['ItemIndex']:= FItemIndex.ToString;
  Self.AListBox.ItemIndex:= FItemIndex;
end;

procedure TEditorListBox.SetLineHeight(const Value: Integer);
begin
  FLineHeight := Value;
  FPropertyList.Value['LineHeight']:= FLineHeight.ToString;
  Self.AListBox.LineHeight:= FLineHeight;
end;

procedure TEditorListBox.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FPropertyList.Value['Transparent']:= FTransparent.ToString(TUseBoolStrs.True);
  Self.AListBox.Transparent:= FTransparent;
end;

{$ENDREGION}

{$REGION 'TEditorCheckBox'}

{ TEditorCheckBox }

constructor TEditorCheckBox.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectCheckBox;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FBoxImage:= TImage.Create(FSelection);
  FBoxImage.Parent:= FSelection;
  FBoxImage.Align:= TAlignLayout.Left;
  FBoxImage.HitTest:= False;
  FBoxImage.WrapMode:= TImageWrapMode.Stretch;
  FBoxImage.Visible:= True;
  //
  FCaptionLabel:= TLabel.Create(FSelection);
  FCaptionLabel.Parent:= FSelection;
  FCaptionLabel.Align:= TAlignLayout.Client;
  FCaptionLabel.HitTest:= False;
  FCaptionLabel.StyledSettings:= [];
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign.Center;
  //
  FPropertyList.Add('Caption', '', TPropertyType.ptString);
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
  FPropertyList.Add('ImageBox', '', TPropertyType.ptImage);
  FPropertyList.Add('ImageBoxChecked', '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add('Transparent', False);
  FPropertyList.Add('ReadOnly', False);
  FPropertyList.Add('Checked', False);
end;

function TEditorCheckBox.CreateACheckBox(AOwner: TComponent): TUICheckBox;
begin
  inherited Assign(TUICheckBox.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.ACheckBox;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.FocusRect:= TFocusRect.fNone;
  Result.Box.Border.Size:= 0;
end;

procedure TEditorCheckBox.DoResize(Sender: TObject);
begin
  inherited;
  FBoxImage.Width:= FSelection.Height;
  if Assigned(FUIControl) then
    Self.ACheckBox.Box.Size:= Round(FBoxImage.Width);
end;

procedure TEditorCheckBox.Assign(const AAControl: TUIControl);
begin
  Self.Caption:= TUICheckBox(AAControl).Caption;
  Self.Image:= TUICheckBox(AAControl).Image.Formatting;
  Self.ImageBox:= TUICheckBox(AAControl).Box.Image.Formatting;
  Self.ImageBoxChecked:= TUICheckBox(AAControl).Box.CheckedImage.Formatting;
  Self.FontName:= TUICheckBox(AAControl).Font.Name;
  Self.FontSize:= TUICheckBox(AAControl).Font.Size;
  Self.FontColor:= TUICheckBox(AAControl).Font.Color.First;
  Self.FontStyle:= TUICheckBox(AAControl).Font.StyleToString;
  Self.Transparent:= TUICheckBox(AAControl).Transparent;
  Self.ReadOnly:= TUICheckBox(AAControl).ReadOnly;
  Self.Checked:= TUICheckBox(AAControl).Checked;
end;

procedure TEditorCheckBox.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorCheckBox.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorCheckBox(Result).CreateACheckBox(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorCheckBox(Result).Caption:= Self.Caption;
  TEditorCheckBox(Result).Image:= Self.Image;
  TEditorCheckBox(Result).ImageBox:= Self.ImageBox;
  TEditorCheckBox(Result).ImageBoxChecked:= Self.ImageBoxChecked;
  TEditorCheckBox(Result).FontName:= Self.FontName;
  TEditorCheckBox(Result).FontSize:= Self.FontSize;
  TEditorCheckBox(Result).FontColor:= Self.FontColor;
  TEditorCheckBox(Result).FontStyle:= Self.FontStyle;
  TEditorCheckBox(Result).Transparent:= Self.Transparent;
  TEditorCheckBox(Result).ReadOnly:= Self.ReadOnly;
  TEditorCheckBox(Result).Checked:= Self.Checked;
end;

function TEditorCheckBox.GetACheckBox: TUICheckBox;
begin
  Result:= TUICheckBox(FUIControl);
end;

procedure TEditorCheckBox.ResetBoxImage;
var
  LImage: TFormatedImage;
begin
  if FChecked then
    LImage:= Self.ACheckBox.Box.CheckedImage
  else
    LImage:= Self.ACheckBox.Box.Image;
  //
  Self.ResetImage(FBoxImage.Bitmap, LImage);
end;

procedure TEditorCheckBox.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.ACheckBox.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.ACheckBox.Image);
end;

procedure TEditorCheckBox.SetImageBox(const Value: string);
begin
  FImageBox := Value;
  FPropertyList.Value['ImageBox']:= FImageBox;
  Self.ACheckBox.Box.Image.ParseFrom(FImageBox);
  ResetBoxImage;
end;

procedure TEditorCheckBox.SetImageBoxChecked(const Value: string);
begin
  FImageBoxChecked := Value;
  FPropertyList.Value['ImageBoxChecked']:= FImageBoxChecked;
  Self.ACheckBox.Box.CheckedImage.ParseFrom(FImageBoxChecked);
  ResetBoxImage;
end;

procedure TEditorCheckBox.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  FPropertyList.Value['ReadOnly']:= FReadOnly.ToString(TUseBoolStrs.True);
  Self.ACheckBox.ReadOnly:= FReadOnly;
end;

procedure TEditorCheckBox.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FPropertyList.Value['Transparent']:= FTransparent.ToString(TUseBoolStrs.True);
  Self.ACheckBox.Transparent:= FTransparent;
end;

procedure TEditorCheckBox.SetChecked(const Value: Boolean);
begin
  FChecked := Value;
  FPropertyList.Value['Checked']:= FChecked.ToString(TUseBoolStrs.True);
  Self.ACheckBox.Checked:= FChecked;
  ResetBoxImage;
end;

procedure TEditorCheckBox.SetCaption(const Value: string);
begin
  FCaption := Value;
  FPropertyList.Value['Caption']:= FCaption;
  Self.ACheckBox.Caption:= FCaption;
  FCaptionLabel.Text:= FCaption;
end;

procedure TEditorCheckBox.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.ACheckBox.Font.Color:= ColorPair(FFontColor, FFontColor);
  FCaptionLabel.FontColor:= FFontColor;
end;

procedure TEditorCheckBox.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.ACheckBox.Font.Name:= FFontName;
  FCaptionLabel.Font.Family:= 'ÑÅºÚ';
end;

procedure TEditorCheckBox.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.ACheckBox.Font.Size:= FFontSize;
  FCaptionLabel.Font.Size:= FFontSize;
end;

procedure TEditorCheckBox.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.ACheckBox.Font.ChangeStyle(FFontStyle);
  FCaptionLabel.Font.Style:= Self.ACheckBox.Font.Style;
end;

{$ENDREGION}

{$REGION 'TEditorRadioButton'}

{ TEditorRadioButton }

constructor TEditorRadioButton.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectRadioButton;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FBoxImage:= TImage.Create(FSelection);
  FBoxImage.Parent:= FSelection;
  FBoxImage.Align:= TAlignLayout.Left;
  FBoxImage.HitTest:= False;
  FBoxImage.WrapMode:= TImageWrapMode.Stretch;
  FBoxImage.Visible:= True;
  //
  FCaptionLabel:= TLabel.Create(FSelection);
  FCaptionLabel.Parent:= FSelection;
  FCaptionLabel.Align:= TAlignLayout.Client;
  FCaptionLabel.HitTest:= False;
  FCaptionLabel.StyledSettings:= [];
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign.Center;
  //
  FPropertyList.Add('Caption', '', TPropertyType.ptString);
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
  FPropertyList.Add('ImageBox', '', TPropertyType.ptImage);
  FPropertyList.Add('ImageBoxChecked', '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add('Transparent', False);
  FPropertyList.Add('ReadOnly', False);
  FPropertyList.Add('Checked', False);
end;

function TEditorRadioButton.CreateARadioButton(AOwner: TComponent): TUIRadioButton;
begin
  inherited Assign(TUIRadioButton.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.ARadioButton;
  Result.Border.Size:= 0;
  Result.Margin:= 0;
  Result.FocusRect:= TFocusRect.fNone;
  Result.Box.Border.Size:= 0;
end;

procedure TEditorRadioButton.DoResize(Sender: TObject);
begin
  inherited;
  FBoxImage.Width:= FSelection.Height;
  if Assigned(FUIControl) then
    Self.ARadioButton.Box.Size:= Round(FBoxImage.Width);
end;

procedure TEditorRadioButton.Assign(const AAControl: TUIControl);
begin
  Self.Caption:= TUIRadioButton(AAControl).Caption;
  Self.Image:= TUIRadioButton(AAControl).Image.Formatting;
  Self.ImageBox:= TUIRadioButton(AAControl).Box.Image.Formatting;
  Self.ImageBoxChecked:= TUIRadioButton(AAControl).Box.CheckedImage.Formatting;
  Self.FontName:= TUIRadioButton(AAControl).Font.Name;
  Self.FontSize:= TUIRadioButton(AAControl).Font.Size;
  Self.FontColor:= TUIRadioButton(AAControl).Font.Color.First;
  Self.FontStyle:= TUIRadioButton(AAControl).Font.StyleToString;
  Self.Transparent:= TUIRadioButton(AAControl).Transparent;
  Self.ReadOnly:= TUIRadioButton(AAControl).ReadOnly;
  Self.Checked:= TUIRadioButton(AAControl).Checked;
end;

procedure TEditorRadioButton.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorRadioButton.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorRadioButton(Result).CreateARadioButton(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorRadioButton(Result).Caption:= Self.Caption;
  TEditorRadioButton(Result).Image:= Self.Image;
  TEditorRadioButton(Result).ImageBox:= Self.ImageBox;
  TEditorRadioButton(Result).ImageBoxChecked:= Self.ImageBoxChecked;
  TEditorRadioButton(Result).FontName:= Self.FontName;
  TEditorRadioButton(Result).FontSize:= Self.FontSize;
  TEditorRadioButton(Result).FontColor:= Self.FontColor;
  TEditorRadioButton(Result).FontStyle:= Self.FontStyle;
  TEditorRadioButton(Result).Transparent:= Self.Transparent;
  TEditorRadioButton(Result).ReadOnly:= Self.ReadOnly;
  TEditorRadioButton(Result).Checked:= Self.Checked;
end;

function TEditorRadioButton.GetARadioButton: TUIRadioButton;
begin
  Result:= TUIRadioButton(FUIControl);
end;

procedure TEditorRadioButton.ResetBoxImage;
var
  LImage: TFormatedImage;
begin
  if FChecked then
    LImage:= Self.ARadioButton.Box.CheckedImage
  else
    LImage:= Self.ARadioButton.Box.Image;
  //
  Self.ResetImage(FBoxImage.Bitmap, LImage);
end;

procedure TEditorRadioButton.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.ARadioButton.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.ARadioButton.Image);
end;

procedure TEditorRadioButton.SetImageBox(const Value: string);
begin
  FImageBox := Value;
  FPropertyList.Value['ImageBox']:= FImageBox;
  Self.ARadioButton.Box.Image.ParseFrom(FImageBox);
  ResetBoxImage;
end;

procedure TEditorRadioButton.SetImageBoxChecked(const Value: string);
begin
  FImageBoxChecked := Value;
  FPropertyList.Value['ImageBoxChecked']:= FImageBoxChecked;
  Self.ARadioButton.Box.CheckedImage.ParseFrom(FImageBoxChecked);
  ResetBoxImage;
end;

procedure TEditorRadioButton.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  FPropertyList.Value['ReadOnly']:= FReadOnly.ToString(TUseBoolStrs.True);
  Self.ARadioButton.ReadOnly:= FReadOnly;
end;

procedure TEditorRadioButton.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FPropertyList.Value['Transparent']:= FTransparent.ToString(TUseBoolStrs.True);
  Self.ARadioButton.Transparent:= FTransparent;
end;

procedure TEditorRadioButton.SetChecked(const Value: Boolean);
var
  I: Integer;
  C: TComponent;
begin
  if (Value = True) and (FParent <> nil) and (FParent <> Self) then
  begin
    for I := FParent.FChilds.Count - 1 downto 0 do
    begin
      C := FParent.FChilds[I];

      if (C <> Self) and (C is TEditorRadioButton) then
      begin
        if (C as TEditorRadioButton).Checked then
          (C as TEditorRadioButton).Checked := False;
      end;
    end;
  end;
  //
  FChecked := Value;
  FPropertyList.Value['Checked']:= FChecked.ToString(TUseBoolStrs.True);
  Self.ARadioButton.Checked:= FChecked;
  ResetBoxImage;
end;

procedure TEditorRadioButton.SetCaption(const Value: string);
begin
  FCaption := Value;
  FPropertyList.Value['Caption']:= FCaption;
  Self.ARadioButton.Caption:= FCaption;
  FCaptionLabel.Text:= FCaption;
end;

procedure TEditorRadioButton.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.ARadioButton.Font.Color:= ColorPair(FFontColor, FFontColor);
  FCaptionLabel.FontColor:= FFontColor;
end;

procedure TEditorRadioButton.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.ARadioButton.Font.Name:= FFontName;
  FCaptionLabel.Font.Family:= 'ÑÅºÚ';
end;

procedure TEditorRadioButton.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.ARadioButton.Font.Size:= FFontSize;
  FCaptionLabel.Font.Size:= FFontSize;
end;

procedure TEditorRadioButton.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.ARadioButton.Font.ChangeStyle(FFontStyle);
  FCaptionLabel.Font.Style:= Self.ARadioButton.Font.Style;
end;

{$ENDREGION}

{$REGION 'TEditorProgressBar'}

{ TEditorProgressBar }

constructor TEditorProgressBar.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectProgressBar;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FPositionImage:= TImage.Create(FSelection);
  FPositionImage.Parent:= FBGImage;
  FPositionImage.Align:= TAlignLayout.Left;
  FPositionImage.HitTest:= False;
  FPositionImage.WrapMode:= TImageWrapMode.Stretch;
  //
  FCaptionLabel:= TLabel.Create(FSelection);
  FCaptionLabel.Parent:= FSelection;
  FCaptionLabel.Align:= TAlignLayout.Client;
  FCaptionLabel.HitTest:= False;
  FCaptionLabel.StyledSettings:= [];
  FCaptionLabel.TextSettings.HorzAlign:= FMX.Types.TTextAlign.Center;
  FCaptionLabel.BringToFront;
  //
  FPropertyList.Add('Caption', '', TPropertyType.ptString);
  FPropertyList.Add('Image', '', TPropertyType.ptImage);
  FPropertyList.Add('ImagePosition', '', TPropertyType.ptImage);
  FPropertyList.AddFontName('');
  FPropertyList.AddFontSize(0);
  FPropertyList.AddFontColor('');
  FPropertyList.AddFontStyle('');
  FPropertyList.Add<TProgressDisplay>('Display', TProgressDisplay.pdPercentage);
  FPropertyList.Add('Max', 100);
  FPropertyList.Add('Min', 0);
  FPropertyList.Add('Position', 0);
  FPropertyList.Add('Transparent', False);
end;

function TEditorProgressBar.CreateAProgressBar(
  AOwner: TComponent): TUIProgressBar;
begin
  inherited Assign(TUIProgressBar.Create(AOwner));
  Self.Assign(FUIControl);
  Result:= Self.AProgressBar;
  Result.Margin:= 0;
  Result.Border.Size:= 0;
  Result.CanMoveHandle:= False;
  Result.Max:= 100;
  Result.Min:= 0;
  Result.Position:= 0;
  Result.Transparent:= False;
end;

procedure TEditorProgressBar.Assign(const AAControl: TUIControl);
begin
  Self.Caption:= TUIProgressBar(AAControl).Caption;
  Self.Image:= TUIProgressBar(AAControl).Image.Formatting;
  Self.ImagePosition:= TUIProgressBar(AAControl).ImagePosition.Formatting;
  Self.FontName:= TUIProgressBar(AAControl).Font.Name;
  Self.FontSize:= TUIProgressBar(AAControl).Font.Size;
  Self.FontColor:= TUIProgressBar(AAControl).Font.Color.First;
  Self.FontStyle:= TUIProgressBar(AAControl).Font.StyleToString;
  Self.Display:= AAControl.EnumToString<TProgressDisplay>(TUIProgressBar(AAControl).Display);
  Self.Max:= TUIProgressBar(AAControl).Max;
  Self.Min:= TUIProgressBar(AAControl).Min;
  Self.Position:= TUIProgressBar(AAControl).Position;
  Self.Transparent:= TUIProgressBar(AAControl).Transparent;
end;

procedure TEditorProgressBar.AssignFrom(const AAControl: TUIControl);
begin
  inherited;
  Self.Assign(AAControl);
end;

function TEditorProgressBar.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorProgressBar(Result).CreateAProgressBar(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
  TEditorProgressBar(Result).Caption:= Self.Caption;
  TEditorProgressBar(Result).Image:= Self.Image;
  TEditorProgressBar(Result).ImagePosition:= Self.ImagePosition;
  TEditorProgressBar(Result).FontName:= Self.FontName;
  TEditorProgressBar(Result).FontSize:= Self.FontSize;
  TEditorProgressBar(Result).FontColor:= Self.FontColor;
  TEditorProgressBar(Result).FontStyle:= Self.FontStyle;
  TEditorProgressBar(Result).Display:= Self.Display;
  TEditorProgressBar(Result).Max:= Self.Max;
  TEditorProgressBar(Result).Min:= Self.Min;
  TEditorProgressBar(Result).Position:= Self.Position;
  TEditorProgressBar(Result).Transparent:= Self.Transparent;
end;

function TEditorProgressBar.GetAProgressBar: TUIProgressBar;
begin
  Result:= TUIProgressBar(FUIControl);
end;

procedure TEditorProgressBar.ResetProgress;
begin
  FPositionImage.Width:= FBGImage.Width * (FPosition/FMax);
  ResetProgressDisplay();
end;

procedure TEditorProgressBar.ResetProgressDisplay;
begin
  case Self.AProgressBar.Display of
    pdNone: FCaptionLabel.Text:= '';
    pdValue: FCaptionLabel.Text:= FPosition.ToString;
    pdPercentage: FCaptionLabel.Text:= Format('%d%%',
      [Round((FPosition / FMax) * 100)]);
    pdCustom: FCaptionLabel.Text:= FCaption;
  end;
end;

procedure TEditorProgressBar.SetCaption(const Value: string);
begin
  FCaption := Value;
  FPropertyList.Value['Caption']:= FCaption;
  Self.AProgressBar.Caption:= FCaption;
  FCaptionLabel.Text:= FCaption;
end;

procedure TEditorProgressBar.SetDisplay(const Value: string);
begin
  FDisplay := Value;
  FPropertyList.Value['Display']:= FDisplay;
  Self.AProgressBar.ChangeDisplay(FDisplay);
  ResetProgressDisplay();
end;

procedure TEditorProgressBar.SetFontColor(const Value: TEditorColor);
begin
  FFontColor := Value;
  FPropertyList.Value['FontColor']:= ColorToHex(FFontColor);
  Self.AProgressBar.Font.Color:= ColorPair(FFontColor, FFontColor);
  FCaptionLabel.FontColor:= FFontColor;
end;

procedure TEditorProgressBar.SetFontName(const Value: string);
begin
  FFontName := Value;
  FPropertyList.Value['FontName']:= FFontName;
  Self.AProgressBar.Font.Name:= FFontName;
  FCaptionLabel.Font.Family:= 'ÑÅºÚ';
end;

procedure TEditorProgressBar.SetFontSize(const Value: Word);
begin
  FFontSize := Value;
  FPropertyList.Value['FontSize']:= FFontSize.ToString;
  Self.AProgressBar.Font.Size:= FFontSize;
  FCaptionLabel.Font.Size:= FFontSize;
end;

procedure TEditorProgressBar.SetFontStyle(const Value: string);
begin
  FFontStyle := Value;
  FPropertyList.Value['FontStyle']:= FFontStyle;
  Self.AProgressBar.Font.ChangeStyle(FFontStyle);
  FCaptionLabel.Font.Style:= Self.AProgressBar.Font.Style;
end;

procedure TEditorProgressBar.SetImage(const Value: string);
begin
  FImage := Value;
  FPropertyList.Value['Image']:= FImage;
  Self.AProgressBar.Image.ParseFrom(FImage);
  Self.ResetImage(FBGImage.Bitmap, Self.AProgressBar.Image);
end;

procedure TEditorProgressBar.SetImagePosition(const Value: string);
begin
  FImagePosition := Value;
  FPropertyList.Value['ImagePosition']:= FImagePosition;
  Self.AProgressBar.ImagePosition.ParseFrom(FImagePosition);
  Self.ResetImage(FPositionImage.Bitmap, Self.AProgressBar.ImagePosition);
end;

procedure TEditorProgressBar.SetMax(const Value: Integer);
begin
  FMax := Value;
  FPropertyList.Value['Max']:= FMax.ToString;
  Self.AProgressBar.Max:= FMax;
  Self.ResetProgress;
end;

procedure TEditorProgressBar.SetMin(const Value: Integer);
begin
  FMin := Value;
  FPropertyList.Value['Min']:= FMin.ToString;
  Self.AProgressBar.Min:= FMin;
end;

procedure TEditorProgressBar.SetPosition(const Value: Integer);
begin
  FPosition := Value;
  FPropertyList.Value['Position']:= FPosition.ToString;
  Self.AProgressBar.Position:= FPosition;
  Self.ResetProgress;
end;

procedure TEditorProgressBar.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  FPropertyList.Value['Transparent']:= FTransparent.ToString(TUseBoolStrs.True);
  Self.AProgressBar.Transparent:= FTransparent;
end;

{$ENDREGION}

{$REGION 'TEditorTrackBar'}

{ TEditorTrackBar }

constructor TEditorTrackBar.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectTrackBar;
  //
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
end;

function TEditorTrackBar.CreateATrackBar(AOwner: TComponent): TUITrackBar;
begin
  FUIControl:= TUITrackBar.Create(AOwner);
  Result:= Self.TrackBar;
//    property Border;
//    property Button;
//    property Color;
//    property FocusRect;
//    property Image;
//    property Increment;
//    property Margin;
//    property Max;
//    property Min;
//    property Position;
//    property Transparent;
end;

procedure TEditorTrackBar.AssignFrom(const AAControl: TUIControl);
begin
  inherited;

end;

function TEditorTrackBar.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorTrackBar(Result).CreateATrackBar(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
end;

function TEditorTrackBar.GetATrackBar: TUITrackBar;
begin
  Result:= TUITrackBar(FUIControl);
end;

{$ENDREGION}

{$REGION 'TEditorMemo'}

{ TEditorMemo }

constructor TEditorMemo.Create(AOwner: TControl);
begin
  inherited;
  FControlType:= ectMemo;
  //
  FBGImage:= TImage.Create(FSelection);
  FBGImage.Parent:= FSelection;
  FBGImage.Align:= TAlignLayout.Client;
  FBGImage.HitTest:= False;
  FBGImage.WrapMode:= TImageWrapMode.Stretch;
end;

function TEditorMemo.CreateAMemo(AOwner: TComponent): TUIMemo;
begin

end;

procedure TEditorMemo.AssignFrom(const AAControl: TUIControl);
begin
  inherited;

end;

function TEditorMemo.Clone: TEditorControl;
begin
  Result:= inherited Clone;
  TEditorMemo(Result).CreateAMemo(Self.Parent.UIControl);
  Result.Left:= Self.Left + 20;
  Result.Top := Self.Top  + 20;
end;

function TEditorMemo.GetAMemo: TUIMemo;
begin

end;

{$ENDREGION}

initialization

RegisterClasses([TEditorControl]);

finalization

UnRegisterClasses([TEditorControl]);

end.

