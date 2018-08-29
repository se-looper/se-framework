unit editor.propeditor;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.Types, System.Math,
  System.Generics.Collections, System.Generics.Defaults,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.Graphics, FMX.StdCtrls, FMX.Dialogs,
  FMX.ListView, FMX.Edit, FMX.SpinBox, FMX.Text, FMX.ListBox, FMX.ListView.Appearances,
  frmImageManager, frmSelectColor;

type
  TParam = record
    ControlID: Integer;
    PropName: string;
    PropValue: string;
  end;

  TEditButtonGroup= record
    Edit: TEdit;
    Button: TButton;
  end;

  TNotifyEventWrapper= class(TComponent)
  private
    FProc: TProc<TObject>;
  public
    constructor Create(Owner: TComponent; Proc: TProc<TObject>);
  published
    procedure Event(Sender: TObject);
  end;

  TPropEditor = class
  private
    FContainer: TControl;
    FOnDataChange: TProc<TParam>;
    FControlID: Integer;
    FInspectorItem: TListViewItem;
    FSelectImageForm: TImageManagerFrm;
    FSelectColorForm: TSelectColorFrm;
    procedure SetOnDataChange(const Value: TProc<TParam>);
    function AddRectangle(const AContainer: TControl; const ATitle: string = ''): TRectangle;
    function AddLabel(const AContainer: TControl; const ATitle: string): TLabel;
  public
    constructor Create(const AContainer: TControl);
    destructor Destroy; override;

    procedure Clear;

    function AddEdit(const APropName, AText: string): TEdit;
    function AddEditButton_GetImage(const APropName, AText: string): TEditButtonGroup;
    function AddEditButton_GetColor(const APropName, AText: string): TEditButtonGroup;
    function AddNumberEdit(const APropName, AText: string; const AValueType: TNumValueType): TSpinBox;
    function AddCheckBox(const APropName: string; const AValue: Boolean): TCheckBox;
    function AddComboBox(const APropName: string): TComboBox;
    function AddSwitch(const APropName: string; const AValue: Boolean): TSwitch;

    procedure AddCheckBoxs(const APropName: string;
      const ATitles: TArray<string>; const AValues: TArray<Boolean>); overload;
    procedure AddCheckBoxs(const APropName: string;
      const ATitles: TArray<string>; const AValues: string); overload;

    property SelectImageForm: TImageManagerFrm read FSelectImageForm write FSelectImageForm;
    property SelectColorForm: TSelectColorFrm read FSelectColorForm write FSelectColorForm;
    property ControlID: Integer read FControlID write FControlID;
    property InspectorItem: TListViewItem read FInspectorItem write FInspectorItem;
    property OnDataChange: TProc<TParam> read FOnDataChange write SetOnDataChange;
  end;

implementation

{ TNotifyEventWrapper }

constructor TNotifyEventWrapper.Create(Owner: TComponent; Proc: TProc<TObject>);
begin
  inherited Create(Owner);
  FProc:= Proc;
end;

procedure TNotifyEventWrapper.Event(Sender: TObject);
begin
  FProc(Sender);
end;

function AnonProc2NotifyEvent(Owner: TComponent; Proc: TProc<TObject>): TNotifyEvent;
begin
  Result:= TNotifyEventWrapper.Create(Owner, Proc).Event;
end;

{ TPropEditor }

procedure TPropEditor.Clear;
begin
  FControlID:= -1;
  FInspectorItem:= nil;
  while FContainer.ControlsCount > 0 do
    FContainer.Controls[0].Free;
  FContainer.Controls.Clear;
  FContainer.Repaint;
end;

constructor TPropEditor.Create(const AContainer: TControl);
begin
  inherited Create;
  FContainer:= AContainer;
end;

destructor TPropEditor.Destroy;
begin

  inherited;
end;

procedure TPropEditor.SetOnDataChange(const Value: TProc<TParam>);
begin
  FOnDataChange := Value;
end;

function TPropEditor.AddRectangle(const AContainer: TControl;
  const ATitle: string): TRectangle;
begin
  Result:= TRectangle.Create(AContainer);
  Result.Parent:= AContainer;
  Result.Align:= TAlignLayout.Top;
  Result.Height:= 40;
  Result.Fill.Kind:= TBrushKind.None;
  Result.Stroke.Dash:= TStrokeDash.Dash;
  Result.Stroke.Color:= TAlphaColorRec.Darkcyan;
  if not ATitle.IsEmpty then
    Self.AddLabel(Result, ATitle);
end;

function TPropEditor.AddLabel(const AContainer: TControl; const ATitle: string): TLabel;
begin
  Result:= TLabel.Create(AContainer);
  Result.Parent:= AContainer;
  Result.Text:= ATitle + ': ';
  Result.AutoSize:= True;
  Result.Align:= TAlignLayout.Left;
  Result.Margins.Rect:= RectF(10, 10, 0, 10);
end;

function TPropEditor.AddEdit(const APropName, AText: string): TEdit;
var
  LControl: TEdit;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer, APropName);
  //
  LControl:= TEdit.Create(LItemContainer);
  LControl.Parent:= LItemContainer;
  LControl.Text:= AText;
  LControl.Align:= TAlignLayout.Client;
  LControl.Margins.Rect:= RectF(10,10,10,10);
  LControl.OnChange:= AnonProc2NotifyEvent(LControl,
    procedure(Sender: TObject)
    var
      LParam: TParam;
    begin
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TEdit(Sender).Text;
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  Result:= LControl;
end;

function TPropEditor.AddEditButton_GetColor(const APropName,
  AText: string): TEditButtonGroup;
var
  LGroup: TEditButtonGroup;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer, APropName);
  //
  LGroup.Edit:= TEdit.Create(LItemContainer);
  LGroup.Edit.Parent:= LItemContainer;
  LGroup.Edit.Text:= AText;
  LGroup.Edit.Align:= TAlignLayout.Client;
  LGroup.Edit.Margins.Rect:= RectF(10,10,10,10);
  LGroup.Edit.OnChange:= AnonProc2NotifyEvent(LGroup.Edit,
    procedure(Sender: TObject)
    var
      LParam: TParam;
    begin
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TEdit(Sender).Text;
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  LGroup.Button:= TButton.Create(LItemContainer);
  LGroup.Button.Parent:= LItemContainer;
  LGroup.Button.Text:= 'Color';
  LGroup.Button.Align:= TAlignLayout.Right;
  LGroup.Button.Margins.Rect:= RectF(0,10,10,10);
  LGroup.Button.Width:= 50;
  LGroup.Button.OnClick:= AnonProc2NotifyEvent(LGroup.Button,
    procedure(Sender: TObject)
    begin

    end);
  LGroup.Button.Enabled:= False;
  Result:= LGroup;
end;

function TPropEditor.AddEditButton_GetImage(const APropName,
  AText: string): TEditButtonGroup;
var
  LGroup: TEditButtonGroup;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer, APropName);
  //
  LGroup.Edit:= TEdit.Create(LItemContainer);
  LGroup.Edit.Parent:= LItemContainer;
  LGroup.Edit.Text:= AText;
  LGroup.Edit.Align:= TAlignLayout.Client;
  LGroup.Edit.Margins.Rect:= RectF(10,10,10,10);
  LGroup.Edit.ReadOnly:= True;
  LGroup.Edit.OnChange:= AnonProc2NotifyEvent(LGroup.Edit,
    procedure(Sender: TObject)
    var
      LParam: TParam;
    begin
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TEdit(Sender).Text;
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  LGroup.Button:= TButton.Create(LItemContainer);
  LGroup.Button.Parent:= LItemContainer;
  LGroup.Button.Text:= '...';
  LGroup.Button.Align:= TAlignLayout.Right;
  LGroup.Button.Margins.Rect:= RectF(0,10,10,10);
  LGroup.Button.Width:= 30;
  LGroup.Button.OnClick:= AnonProc2NotifyEvent(LGroup.Button,
    procedure(Sender: TObject)
    var
      LImageFile: string;
      LRect: TRect;
    begin
      if not FSelectImageForm.SelectImage(LImageFile, LRect) then Exit;
      LGroup.Edit.Text:= Format('%s(%d,%d,%d,%d)',[LImageFile, LRect.Left,
        LRect.Top, LRect.Right, LRect.Bottom]);
    end);
  Result:= LGroup;
end;

function TPropEditor.AddNumberEdit(const APropName, AText: string;
  const AValueType: TNumValueType): TSpinBox;
var
  LControl: TSpinBox;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer, APropName);
  //
  LControl:= TSpinBox.Create(LItemContainer);
  LControl.Parent:= LItemContainer;
  LControl.Text:= AText;
  LControl.Align:= TAlignLayout.Client;
  LControl.Margins.Rect:= RectF(10,10,10,10);
  LControl.ValueType:= AValueType;
  LControl.Max:= MaxInt;
  LControl.OnChange:= AnonProc2NotifyEvent(LControl,
    procedure(Sender: TObject)
    var
      LParam: TParam;
    begin
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TSpinBox(Sender).Text;
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  Result:= LControl;
end;

function TPropEditor.AddCheckBox(const APropName: string;
  const AValue: Boolean): TCheckBox;
var
  LControl: TCheckBox;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer);
  //
  LControl:= TCheckBox.Create(LItemContainer);
  LControl.Parent:= LItemContainer;
  LControl.Align:= TAlignLayout.Client;
  LControl.Margins.Rect:= RectF(10,10,10,10);
  LControl.IsChecked:= AValue;
  LControl.Text:= APropName;
  LControl.OnChange:= AnonProc2NotifyEvent(LControl,
    procedure(Sender: TObject)
    var
      LParam: TParam;
    begin
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TCheckBox(Sender).IsChecked.ToString(TUseBoolStrs.True);
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  Result:= LControl;
end;

function TPropEditor.AddComboBox(const APropName: string): TComboBox;
var
  LControl: TComboBox;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer, APropName);
  //
  LControl:= TComboBox.Create(LItemContainer);
  LControl.Parent:= LItemContainer;
  LControl.Align:= TAlignLayout.Client;
  LControl.Margins.Rect:= RectF(10,10,10,10);
  LControl.OnChange:= AnonProc2NotifyEvent(LControl,
    procedure(Sender: TObject)
    var
      LIndex: Integer;
      LParam: TParam;
    begin
      LIndex:= TComboBox(Sender).ItemIndex;
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TComboBox(Sender).Items[LIndex];
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  Result:= LControl;
end;

function TPropEditor.AddSwitch(const APropName: string;
  const AValue: Boolean): TSwitch;
var
  LControl: TSwitch;
  LItemContainer: TRectangle;
begin
  LItemContainer:= Self.AddRectangle(FContainer, APropName);
  //
  LControl:= TSwitch.Create(LItemContainer);
  LControl.Parent:= LItemContainer;
  LControl.Align:= TAlignLayout.Client;
  LControl.Margins.Rect:= RectF(10,10,180,10);
  LControl.IsChecked:= AValue;
  LControl.OnClick:= AnonProc2NotifyEvent(LControl,
    procedure(Sender: TObject)
    var
      LParam: TParam;
    begin
      LParam.ControlID:= FControlID;
      LParam.PropName:= APropName;
      LParam.PropValue:= TSwitch(Sender).IsChecked.ToString(TUseBoolStrs.True);
      if Assigned(FOnDataChange) then
      begin
        FOnDataChange(LParam);
        if Assigned(FInspectorItem) then
          FInspectorItem.Detail:= LParam.PropValue;
      end;
    end);
  Result:= LControl;
end;

procedure TPropEditor.AddCheckBoxs(const APropName: string;
  const ATitles: TArray<string>; const AValues: string);
var
  I, J: Integer;
  LStringValues: TArray<string>;
  LBooleanValues: TArray<Boolean>;
begin
  LStringValues:= AValues.Split([',']);
  SetLength(LBooleanValues, Length(ATitles));
  for I:= 0 to Length(ATitles) -1 do
  begin
    LBooleanValues[I]:= False;
    for J:= 0 to Length(LStringValues) -1 do
    begin
      if ATitles[I] = LStringValues[J] then
      begin
        LBooleanValues[I]:= True;
        Break;
      end;
    end;
  end;
  Self.AddCheckBoxs(APropName, ATitles, LBooleanValues);
end;

procedure TPropEditor.AddCheckBoxs(const APropName: string;
  const ATitles: TArray<string>; const AValues: TArray<Boolean>);
var
  I: Integer;
  LCheckBox: TCheckBox;
  LItemContainer: TRectangle;
begin
  for I:= Low(ATitles) to High(ATitles) do
  begin
    LItemContainer:= Self.AddRectangle(FContainer);
    //
    LCheckBox:= TCheckBox.Create(LItemContainer);
    LCheckBox.Parent:= LItemContainer;
    LCheckBox.Align:= TAlignLayout.Client;
    LCheckBox.Margins.Rect:= RectF(10,10,10,10);
    LCheckBox.Text:= ATitles[I];
    LCheckBox.IsChecked:= AValues[I];
    LCheckBox.OnChange:= AnonProc2NotifyEvent(LCheckBox,
      procedure(Sender: TObject)
      var
        LParam: TParam;
      begin
        LParam.ControlID:= FControlID;
        LParam.PropName:= APropName;
        LParam.PropValue:= TCheckBox(Sender).Text;
        if Assigned(FOnDataChange) then
        begin
          FOnDataChange(LParam);
          if Assigned(FInspectorItem) then
          begin
            if not TCheckBox(Sender).IsChecked then
              FInspectorItem.Detail:= FInspectorItem.Detail
                .Replace(TCheckBox(Sender).Text, '')
                .Replace(',,','')
                .Trim([','])
            else
            begin
              FInspectorItem.Detail:= FInspectorItem.Detail + ',' + TCheckBox(Sender).Text;
              FInspectorItem.Detail:= FInspectorItem.Detail.Trim([',']);
            end;
          end;
        end;
      end);
  end;
end;

end.
