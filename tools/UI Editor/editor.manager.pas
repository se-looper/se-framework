unit editor.manager;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Types,
  System.UITypes, System.Rtti, System.IOUtils, XSuperObject,
  FMX.Types, FMX.Objects, FMX.Controls, FMX.StdCtrls, FMX.TreeView, FMX.ListView,
  FMX.ListView.Appearances, PXL.Devices, PXL.Canvas,
  se.game.types, se.game.assets, se.game.exlib.freetype, se.game.exlib.ui,
  editor.types, editor.Selection, editor.controls, editor.propeditor;

type
  TEditorManager = class(TUIManager)
  private
    FEditorControlMap: TObjectDictionary<Integer, TEditorControl>;
    FDataStore: TObjectDictionary<string, TStrings>;
    procedure MakeInspectorItem(const AControl: TEditorControl);
  private
    FContainer: TControl;
    FStructure: TTreeView;
    FInspector: TListView;
    FControlID, FDataID: Integer;
    FSelected, FLayoutControlList, FClipboard: TList<TEditorControl>;
    FMultiSelect: Boolean;
    procedure DoControlClick(Sender: TObject);
    function GetData(const DataKey: string): TStrings;
    function GetSelected(const Index: Integer): TEditorControl;
    procedure SetMultiSelect(const Value: Boolean);
    function GetLayoutControlCount: Integer;
  private
    procedure AssignFrom(const AParentItem: TTreeViewItem;
      const AParentControl: TEditorControl; const AParentAControl: TUIControl);
    function FindFormItem(const AItem: TTreeViewItem): TTreeViewItem;

    function AddForm(const AItem: TTreeViewItem): TUIForm; overload;
    function AddButton(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIButton;
    function AddLabel(const AItem: TTreeViewItem; const AOwnerID: Integer): TUILabel;
    function AddEditBox(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIEdit;
    function AddImage(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIImage;
    function AddListBox(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIListBox;
    function AddCheckBox(const AItem: TTreeViewItem; const AOwnerID: Integer): TUICheckBox;
    function AddRadioButton(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIRadioButton;
    function AddProgressBar(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIProgressBar;
    function AddTrackBar(const AItem: TTreeViewItem; const AOwnerID: Integer): TUITrackBar;
    function AddMemo(const AItem: TTreeViewItem; const AOwnerID: Integer): TUIMemo;
  public
    constructor Create(const AOwner: TComponent;
      const ACanvas: TCustomCanvas); override;
    destructor Destroy; override;

    procedure Reset;
    procedure Load(const AFileName: string);
    procedure Save(const AFileName: string);

    procedure LoadFromUIFile(const AFileName: string); override;
    procedure SaveToUIFile(const AFileName: string); override;

    procedure DoPropDataChange(const AParam: TParam);

    procedure AddControl(const AParentItem: TTreeViewItem;
      const AControlType: TEditorControlType); overload;
    procedure AddControl(const AParentItem: TTreeViewItem;
      const AControl: TEditorControl); overload;
    procedure AddForm(const ATreeView: TTreeView); overload;

    procedure Select(const AControlID: Integer);
    procedure Refresh(const AControlID: Integer; const APropName: string = '');
    procedure Delete(const AControlID: Integer);
    procedure Lock(const AControlID: Integer);
    procedure Unlock(const AControlID: Integer);
    procedure Show(const AControlID: Integer);
    procedure Hide(const AControlID: Integer);

    procedure Cut;
    procedure Copy;
    procedure Paste;

    procedure AlignLeft;
    procedure AlignTop;
    procedure AlignRight;
    procedure AlignBottom;
    procedure DistributeH;
    procedure DistributeV;

    procedure MoveLeft(const AValue: Integer);
    procedure MoveRight(const AValue: Integer);
    procedure MoveUp(const AValue: Integer);
    procedure MoveDown(const AValue: Integer);

    property Container: TControl read FContainer write FContainer;
    property Structure: TTreeView read FStructure write FStructure;
    property Inspector: TListView read FInspector write FInspector;
    property Data[const DataKey: string]: TStrings read GetData;
    property Selected[const Index: Integer]: TEditorControl read GetSelected;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property LayoutControlCount: Integer read GetLayoutControlCount;
  end;

implementation

const
  cStartControlID = 1000000;
  cStartDataID = 2000000;

{ TEditorManager }

constructor TEditorManager.Create(const AOwner: TComponent;
  const ACanvas: TCustomCanvas);
begin
  inherited;
  FControlID:= cStartControlID;
  FDataID:= cStartDataID;
  FEditorControlMap:= TObjectDictionary<Integer, TEditorControl>.Create([doOwnsValues]);
  FDataStore:= TObjectDictionary<string, TStrings>.Create([doOwnsValues]);
  FSelected:= TList<TEditorControl>.Create;
  FLayoutControlList:= TList<TEditorControl>.Create;
  FClipboard:= TList<TEditorControl>.Create;
  FMultiSelect:= False;
end;

destructor TEditorManager.Destroy;
begin
  FreeAndNil(FClipboard);
  FreeAndNil(FLayoutControlList);
  FreeAndNil(FSelected);
  FreeAndNil(FEditorControlMap);
  FreeAndNil(FDataStore);
  inherited;
end;

procedure TEditorManager.Reset;
begin
  FControlID:= cStartControlID;
  FStructure.Clear;
  FInspector.Items.Clear;
  FEditorControlMap.Clear;
end;

procedure TEditorManager.Load(const AFileName: string);
var
  LFileExt: string;
begin
  LFileExt:= ExtractFileExt(AFileName);
  if LFileExt = '.ui' then
    Self.LoadFromUIFile(AFileName)
end;

procedure TEditorManager.Save(const AFileName: string);
var
  LFileExt: string;
begin
  LFileExt:= ExtractFileExt(AFileName);
  if LFileExt = '.ui' then
    Self.SaveToUIFile(AFileName)
end;

procedure TEditorManager.LoadFromUIFile(const AFileName: string);
var
  I: Integer;
  LItem: TTreeViewItem;
  LControl: TEditorForm;
begin
  Self.Reset;
  inherited;
  FContainer.Position.X:= Self.Root.Left;
  FContainer.Position.Y:= Self.Root.Top;
  FContainer.Width := Self.Root.Width;
  FContainer.Height:= Self.Root.Height;
  for I:= 0 to Self.Root.ControlCount -1 do
  begin
    Inc(FControlID);
    //
    LItem:= TTreeViewItem.Create(FStructure);
    LItem.Parent:= FStructure;
    LItem.Text:= Self.Root.Controls[I].Name;
    LItem.Tag:= FControlID;
    LItem.Select;
    //
    LControl:= TEditorForm.Create(FContainer);
    LControl.Parent:= nil;
    LControl.ID:= FControlID;
    LControl.AssignFrom(Self.Root.Controls[I]);
    LControl.OnClick:= DoControlClick;
    LControl.Bind(LItem);
    FEditorControlMap.Add(FControlID, LControl);
    LControl.Select;
    LControl.Visible:= I = 0;
    //
    Self.AssignFrom(LItem, LControl, Self.Root.Controls[I]);
  end;
end;

procedure TEditorManager.AssignFrom(const AParentItem: TTreeViewItem;
  const AParentControl: TEditorControl; const AParentAControl: TUIControl);
var
  I: Integer;
  LItem: TTreeViewItem;
  LAControl: TUIControl;
  LControl: TEditorControl;
begin
  if not (AParentAControl is TUIWinControl) then Exit;
  //
  for I:= 0 to TUIWinControl(AParentAControl).ControlCount -1 do
  begin
    LAControl:= TUIWinControl(AParentAControl).Controls[I];
    Inc(FControlID);
    //
    LItem:= TTreeViewItem.Create(AParentItem);
    LItem.Parent:= AParentItem;
    LItem.Text:= LAControl.Name;
    LItem.Tag:= FControlID;
    LItem.Select;
    //
    if LAControl is TUIButton then
      LControl:= TEditorButton.Create(FContainer)
    else if LAControl is TUILabel then
      LControl:= TEditorLabel.Create(FContainer)
    else if LAControl is TUIEdit then
      LControl:= TEditorEditBox.Create(FContainer)
    else if LAControl is TUIImage then
      LControl:= TEditorImage.Create(FContainer)
    else if LAControl is TUIListBox then
      LControl:= TEditorListBox.Create(FContainer)
    else if LAControl is TUICheckBox then
      LControl:= TEditorCheckBox.Create(FContainer)
    else if LAControl is TUIRadioButton then
      LControl:= TEditorRadioButton.Create(FContainer)
    else if LAControl is TUIProgressBar then
      LControl:= TEditorProgressBar.Create(FContainer)
    else if LAControl is TUITrackBar then
      LControl:= TEditorTrackBar.Create(FContainer)
    else raise Exception.Create('unknown control type.');
    //
    LControl.Parent:= AParentControl;
    LControl.ID:= FControlID;
    LControl.AssignFrom(LAControl);
    LControl.OnClick:= DoControlClick;
    LControl.Bind(LItem);
    FEditorControlMap.Add(FControlID, LControl);
    LControl.Select;
    //
    AssignFrom(LItem, LControl, LAControl);
  end;
end;

function TEditorManager.FindFormItem(const AItem: TTreeViewItem): TTreeViewItem;
var
  LControl: TEditorControl;
begin
  if not FEditorControlMap.TryGetValue(AItem.Tag, LControl) then Exit(nil);
  if LControl is TEditorForm then Exit(AItem);
  Result:= Self.FindFormItem(AItem.ParentItem);
end;

procedure TEditorManager.SaveToUIFile(const AFileName: string);
begin
  inherited;
end;

procedure TEditorManager.Select(const AControlID: Integer);
var
  I: Integer;
  LControl: TEditorControl;
begin
  if not FMultiSelect then FSelected.Clear;
  for I:= cStartControlID to FControlID do
  begin
    if not FEditorControlMap.TryGetValue(I, LControl) then continue;
    LControl.Selected:= False;
    if LControl.ID = AControlID then
    begin
      if not FSelected.Contains(LControl) then
        FSelected.Add(LControl)
      else
        FSelected.Remove(LControl);
      //
      if not FMultiSelect then Self.MakeInspectorItem(LControl);
    end;
  end;
  //
  FLayoutControlList.Clear;
  for I:= 0 to FSelected.Count -1 do
  begin
    FSelected.Items[I].Selected:= True;
    if not (FSelected.Items[I] is TEditorForm) then
      FLayoutControlList.Add(FSelected.Items[I]);
  end;
end;

procedure TEditorManager.SetMultiSelect(const Value: Boolean);
begin
  FMultiSelect:= Value;
  FStructure.MultiSelect:= Value;
end;

procedure TEditorManager.MakeInspectorItem(const AControl: TEditorControl);
var
  I: Integer;
  LProperty: TProperty;
  LItem: TListViewItem;
  LStrings: TStrings;
begin
  FInspector.Items.Clear;
  for I:= 0 to AControl.PropertyCount -1 do
  begin
    LProperty:= AControl.&Property[I];
    //
    LItem:= FInspector.Items.Add;
    LItem.Text:= LProperty.Name;
    LItem.Detail:= LProperty.Value;
    LItem.Tag:= Ord(LProperty.&Type);
    case LProperty.&Type of
      ptList, ptEnumSet:
        begin
          LItem.Data['key']:= LProperty.Name;
          if not FDataStore.TryGetValue(LProperty.Name, LStrings) then
          begin
            LStrings:= TStringList.Create;
            LStrings.AddFromArray(LProperty.Items);
            FDataStore.Add(LProperty.Name, LStrings);
          end;
        end;
    end;
  end;
end;

procedure TEditorManager.Refresh(const AControlID: Integer; const APropName: string);
var
  LControl: TEditorControl;
  I, J: Integer;
  LProperty: TProperty;
begin
  if not FEditorControlMap.TryGetValue(AControlID, LControl) then Exit;
  for I:= 0 to LControl.PropertyCount -1 do
  begin
    LProperty:= LControl.&Property[I];
    if not (APropName.IsEmpty or APropName.Equals(LProperty.Name)) then
      Continue;
    for J:= 0 to FInspector.Items.Count -1 do
    begin
      if FInspector.Items[J].Text = LProperty.Name then
      begin
        FInspector.Items[J].Detail:= LProperty.Value;
        Break;
      end;
    end;
  end;
end;

procedure TEditorManager.Delete(const AControlID: Integer);
var
  LControl: TEditorControl;
begin
  if not FEditorControlMap.TryGetValue(AControlID, LControl) then Exit;
  FStructure.RemoveObject(LControl.BindNode);
  if LControl.UIControl.Parent <> nil then
    LControl.UIControl.Parent.RemoveControl(LControl.UIControl)
  else
    Self.Root.RemoveControl(LControl.UIControl);
  //
  FEditorControlMap.Remove(AControlID);
end;

procedure TEditorManager.Lock(const AControlID: Integer);
var
  LControl: TEditorControl;
begin
  if not FEditorControlMap.TryGetValue(AControlID, LControl) then Exit;
  LControl.Locked:= True;
end;

procedure TEditorManager.Unlock(const AControlID: Integer);
var
  LControl: TEditorControl;
begin
  if not FEditorControlMap.TryGetValue(AControlID, LControl) then Exit;
  LControl.Locked:= False;
end;

procedure TEditorManager.Show(const AControlID: Integer);
var
  LControl: TEditorControl;
begin
  if not FEditorControlMap.TryGetValue(AControlID, LControl) then Exit;
  LControl.Visible:= True;
end;

procedure TEditorManager.Hide(const AControlID: Integer);
var
  LControl: TEditorControl;
begin
  if not FEditorControlMap.TryGetValue(AControlID, LControl) then Exit;
  LControl.Visible:= False;
end;

procedure TEditorManager.DoControlClick(Sender: TObject);
begin
  Self.Select(TEditorControl(Sender).ID)
end;

procedure TEditorManager.DoPropDataChange(const AParam: TParam);
var
  LControl: TEditorControl;
  RTX: TRttiContext;
  RT: TRttiType;
  RPList: TArray<TRttiProperty>;
  I: Integer;
  RP: TRttiProperty;
  RV: TValue;
begin
  if not FEditorControlMap.TryGetValue(AParam.ControlID, LControl) then Exit;
  RT:= RTX.GetType(LControl.ClassType);
  RPList:= RT.GetProperties;
  for I:= 0 to Length(RPList) -1 do
  begin
    RP:= RPList[I];
    if RP.Name.Equals(AParam.PropName) then
    begin
      if RP.PropertyType.Name.Equals('Integer') or
         RP.PropertyType.Name.Equals('Cardinal') or
         RP.PropertyType.Name.Equals('Int64') or
         RP.PropertyType.Name.Equals('Word') then
        RV:= RV.From(AParam.PropValue.ToInteger)
      else
      if RP.PropertyType.Name.Equals('string') or
         RP.PropertyType.Name.Equals('String') or
         RP.PropertyType.Name.Equals('TComponentName') then
        RV:= RV.From(AParam.PropValue)
      else
      if RP.PropertyType.Name.Equals('Single') or
         RP.PropertyType.Name.Equals('Double') then
        RV:= RV.From(AParam.PropValue.ToSingle)
      else
      if RP.PropertyType.Name.Equals('Boolean') then
        RV:= RV.From(AParam.PropValue.ToBoolean)
      else
      if RP.PropertyType.Name.Equals('TEditorColor') then
        RV:= RV.From(HexToColor(AParam.PropValue))
      else raise Exception.Create('unknown data type: ' + RP.PropertyType.Name);
      //
      RPList[I].SetValue(LControl, RV);
    end;
  end;
  if AParam.PropName.Equals('Name') then
    FStructure.Selected.Text:= AParam.PropValue;
end;

function TEditorManager.GetData(const DataKey: string): TStrings;
begin
  FDataStore.TryGetValue(DataKey, Result);
end;

function TEditorManager.GetLayoutControlCount: Integer;
begin
  Result:= FLayoutControlList.Count;
end;

function TEditorManager.GetSelected(const Index: Integer): TEditorControl;
begin
  if Index < FSelected.Count -1 then
    Result:= FSelected.Items[Index]
  else Exit(nil);
end;

procedure TEditorManager.AddForm(const ATreeView: TTreeView);
var
  LItem: TTreeViewItem;
begin
  LItem:= TTreeViewItem.Create(ATreeView);
  LItem.Parent:= ATreeView;
  LItem.Text:= Self.AddForm(LItem).Name;
  LItem.Tag:= FControlID;
  LItem.Select;
end;

procedure TEditorManager.AddControl(const AParentItem: TTreeViewItem;
  const AControlType: TEditorControlType);
var
  LItem: TTreeViewItem;
  LControl: TUIControl;
  LFormItem: TTreeViewItem;
begin
  if not Assigned(AParentItem) then Exit;
  FStructure.BeginUpdate;
  try
    LFormItem:= Self.FindFormItem(AParentItem);
    if not Assigned(LFormItem) then Exit;
    //
    LItem:= TTreeViewItem.Create(LFormItem);
    LItem.Parent:= LFormItem;
    case AControlType of
      ectButton: LControl:= Self.AddButton(LItem, LFormItem.Tag);
      ectLabel: LControl:= Self.AddLabel(LItem, LFormItem.Tag);
      ectEditBox: LControl:= Self.AddEditBox(LItem, LFormItem.Tag);
      ectImage: LControl:= Self.AddImage(LItem, LFormItem.Tag);
      ectListBox: LControl:= Self.AddListBox(LItem, LFormItem.Tag);
      ectCheckBox: LControl:= Self.AddCheckBox(LItem, LFormItem.Tag);
      ectRadioButton: LControl:= Self.AddRadioButton(LItem, LFormItem.Tag);
      ectProgressBar: LControl:= Self.AddProgressBar(LItem, LFormItem.Tag);
      ectTrackBar: LControl:= Self.AddTrackBar(LItem, LFormItem.Tag);
      ectMemo: LControl:= Self.AddMemo(LItem, LFormItem.Tag);
      else
      begin
        FreeAndNil(LItem);
        Exit;
      end;
    end;
    LItem.Text:= LControl.Name;
    LItem.Tag:= FControlID;
    LItem.Select;
  finally
    FStructure.EndUpdate;
  end;
end;

procedure TEditorManager.AddControl(const AParentItem: TTreeViewItem;
  const AControl: TEditorControl);
var
  LItem: TTreeViewItem;
  LControl: TEditorControl;
  LFormItem: TTreeViewItem;
begin
  if not Assigned(AParentItem) then Exit;
  FStructure.BeginUpdate;
  try
    LFormItem:= Self.FindFormItem(AParentItem);
    if not Assigned(LFormItem) then Exit;
    //
    LItem:= TTreeViewItem.Create(LFormItem);
    LItem.Parent:= LFormItem;
    //
    Inc(FControlID);
    LControl:= AControl.Clone;
    LControl.ID:= FControlID;
    LControl.OnClick:= DoControlClick;
    LControl.Bind(LItem);
    FEditorControlMap.Add(FControlID, LControl);
    //
    LItem.Text:= LControl.Name;
    LItem.Tag:= FControlID;
    LItem.Select;
  finally
    FStructure.EndUpdate;
  end;
end;

function TEditorManager.AddForm(const AItem: TTreeViewItem): TUIForm;
var
  LControl: TEditorForm;
begin
  Inc(FControlID);
  LControl:= TEditorForm.Create(FContainer);
  LControl.Parent:= nil;
  LControl.ID:= FControlID;
  Result:= LControl.CreateAForm(Self.Root);
  LControl.Image:= AssetManager.Require('editor_form.png').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddButton(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIButton;
var
  LControl: TEditorButton;
begin
  Inc(FControlID);
  LControl:= TEditorButton.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateAButton(LControl.Parent.UIControl);
  LControl.Image:= AssetManager.Require('editor.png', 'button').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddLabel(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUILabel;
var
  LControl: TEditorLabel;
begin
  Inc(FControlID);
  LControl:= TEditorLabel.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateALabel(LControl.Parent.UIControl);
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddEditBox(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIEdit;
var
  LControl: TEditorEditBox;
begin
  Inc(FControlID);
  LControl:= TEditorEditBox.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateAEditBox(LControl.Parent.UIControl);
  LControl.Image:= AssetManager.Require('editor.png', 'editbox').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddImage(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIImage;
var
  LControl: TEditorImage;
begin
  Inc(FControlID);
  LControl:= TEditorImage.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateAImage(LControl.Parent.UIControl);
  LControl.Image:= AssetManager.Require('editor.png', 'image').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddListBox(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIListBox;
var
  LControl: TEditorListBox;
begin
  Inc(FControlID);
  LControl:= TEditorListBox.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateAListBox(LControl.Parent.UIControl);
  LControl.Image:= AssetManager.Require('editor.png', 'listbox').Formatting;
  LControl.ImageDownButton:= AssetManager.Require('editor.png', 'listbox_downbutton').Formatting;
  LControl.ImageUpButton:= AssetManager.Require('editor.png', 'listbox_upbutton').Formatting;
  LControl.ImageScrollButton:= AssetManager.Require('editor.png', 'listbox_scrollbutton').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddCheckBox(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUICheckBox;
var
  LControl: TEditorCheckBox;
begin
  Inc(FControlID);
  LControl:= TEditorCheckBox.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateACheckBox(LControl.Parent.UIControl);
  LControl.Image:= '';
  LControl.ImageBox:= AssetManager.Require('editor.png', 'checkbox_unchecked').Formatting;
  LControl.ImageBoxChecked:= AssetManager.Require('editor.png', 'checkbox_checked').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddRadioButton(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIRadioButton;
var
  LControl: TEditorRadioButton;
begin
  Inc(FControlID);
  LControl:= TEditorRadioButton.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateARadioButton(LControl.Parent.UIControl);
  LControl.Image:= '';
  LControl.ImageBox:= AssetManager.Require('editor.png', 'radiobutton_unchecked').Formatting;
  LControl.ImageBoxChecked:= AssetManager.Require('editor.png', 'radiobutton_checked').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddProgressBar(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIProgressBar;
var
  LControl: TEditorProgressBar;
begin
  Inc(FControlID);
  LControl:= TEditorProgressBar.Create(FContainer);
  LControl.Parent:= FEditorControlMap.Items[AOwnerID];
  LControl.ID:= FControlID;
  Result:= LControl.CreateAProgressBar(LControl.Parent.UIControl);
  LControl.Image:= AssetManager.Require('editor.png', 'progressbar').Formatting;
  LControl.ImagePosition:= AssetManager.Require('editor.png', 'progressbar_position').Formatting;
  LControl.OnClick:= DoControlClick;
  LControl.Bind(AItem);
  FEditorControlMap.Add(FControlID, LControl);
  LControl.Select;
end;

function TEditorManager.AddTrackBar(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUITrackBar;
begin

end;

function TEditorManager.AddMemo(const AItem: TTreeViewItem;
  const AOwnerID: Integer): TUIMemo;
begin

end;

procedure TEditorManager.Cut;
var
  I: Integer;
begin
  FClipboard.Clear;
  for I:= 0 to FSelected.Count -1 do
  begin
    if FSelected.Items[I] is TEditorForm then Continue;
    FSelected.Items[I].CutState:= True;
    FClipboard.Add(FSelected.Items[I]);
  end;
end;

procedure TEditorManager.Copy;
var
  I: Integer;
begin
  FClipboard.Clear;
  for I:= 0 to FSelected.Count -1 do
  begin
    if FSelected.Items[I] is TEditorForm then Continue;
    FSelected.Items[I].CutState:= True;
    FClipboard.Add(FSelected.Items[I]);
  end;
end;

procedure TEditorManager.Paste;
var
  I: Integer;
  LTargetControl: TEditorControl;
  LCutState: Boolean;
begin
  LCutState:= False;
  LTargetControl:= FSelected[0];
  for I:= 0 to FClipboard.Count -1 do
  begin
    Self.AddControl(LTargetControl.BindNode, FClipboard.Items[I]);
    LCutState:= FClipboard.Items[I].CutState;
    if LCutState then Self.Delete(FClipboard.Items[I].ID);
  end;
  //剪切操作后要清空剪贴板
  if LCutState then FClipboard.Clear;
end;

procedure TEditorManager.AlignLeft;
var
  I: Integer;
  LReferControl: TEditorControl;
begin
  if FLayoutControlList.Count <= 1 then Exit;
  LReferControl:= FLayoutControlList.First;
  for I:= 1 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Left:= LReferControl.Left;
end;

procedure TEditorManager.AlignTop;
var
  I: Integer;
  LReferControl: TEditorControl;
begin
  if FLayoutControlList.Count <= 1 then Exit;
  LReferControl:= FLayoutControlList.First;
  for I:= 1 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Top:= LReferControl.Top;
end;

procedure TEditorManager.AlignRight;
var
  I: Integer;
  LReferControl: TEditorControl;
begin
  if FLayoutControlList.Count <= 1 then Exit;
  LReferControl:= FLayoutControlList.First;
  for I:= 1 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Left:=
      LReferControl.Left + (LReferControl.Width - FLayoutControlList.Items[I].Width);
end;

procedure TEditorManager.AlignBottom;
var
  I: Integer;
  LReferControl: TEditorControl;
begin
  if FLayoutControlList.Count <= 1 then Exit;
  LReferControl:= FLayoutControlList.First;
  for I:= 1 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Top:=
      LReferControl.Top + (LReferControl.Height - FLayoutControlList.Items[I].Height);
end;

procedure TEditorManager.DistributeH;
var
  I: Integer;
  LFirstControl, LLastControl: TEditorControl;
  LAllControlWidth, LSpace: Single;
begin
  if FLayoutControlList.Count <= 2 then Exit;
  LFirstControl:= FLayoutControlList.First;
  LLastControl := FLayoutControlList.Last;
  //获取所有控件的总宽度
  LAllControlWidth:= 0;
  for I:= 0 to FLayoutControlList.Count -1 do
    LAllControlWidth:= LAllControlWidth + FLayoutControlList.Items[I].Width;
  //计算每个控件的平均间隔
  LSpace:= (LLastControl.Left+LLastControl.Width-LFirstControl.Left-LAllControlWidth) /
           (FLayoutControlList.Count -1);
  //第一个和最后一个不要移动
  for I:= 1 to FLayoutControlList.Count -2 do
    FLayoutControlList.Items[I].Left:= FLayoutControlList.Items[I-1].Left +
      FLayoutControlList.Items[I-1].Width + LSpace;
end;

procedure TEditorManager.DistributeV;
var
  I: Integer;
  LFirstControl, LLastControl: TEditorControl;
  LAllControlHeight, LSpace: Single;
begin
  if FLayoutControlList.Count <= 2 then Exit;
  LFirstControl:= FLayoutControlList.First;
  LLastControl := FLayoutControlList.Last;
  //获取所有控件的总高度
  LAllControlHeight:= 0;
  for I:= 0 to FLayoutControlList.Count -1 do
    LAllControlHeight:= LAllControlHeight + FLayoutControlList.Items[I].Height;
  //计算每个控件的平均间隔
  LSpace:= (LLastControl.Top+LLastControl.Height-LFirstControl.Top-LAllControlHeight) /
           (FLayoutControlList.Count -1);
  //第一个和最后一个不要移动
  for I:= 1 to FLayoutControlList.Count -2 do
    FLayoutControlList.Items[I].Top:= FLayoutControlList.Items[I-1].Top +
      FLayoutControlList.Items[I-1].Height + LSpace;
end;

procedure TEditorManager.MoveLeft(const AValue: Integer);
var
  I: Integer;
begin
  for I:= 0 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Left:= FLayoutControlList.Items[I].Left - AValue;
end;

procedure TEditorManager.MoveRight(const AValue: Integer);
var
  I: Integer;
begin
  for I:= 0 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Left:= FLayoutControlList.Items[I].Left + AValue;
end;

procedure TEditorManager.MoveUp(const AValue: Integer);
var
  I: Integer;
begin
  for I:= 0 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Top:= FLayoutControlList.Items[I].Top - AValue;
end;

procedure TEditorManager.MoveDown(const AValue: Integer);
var
  I: Integer;
begin
  for I:= 0 to FLayoutControlList.Count -1 do
    FLayoutControlList.Items[I].Top:= FLayoutControlList.Items[I].Top + AValue;
end;

end.
