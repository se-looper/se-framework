unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Rtti, System.ImageList, System.Actions, System.Generics.Collections,
  FMX.ListView.Types, FMX.Types, FMX.Layouts, FMX.Controls, FMX.Forms, FMX.Edit,
  FMX.StdCtrls, FMX.TreeView, FMX.ListView, FMX.Dialogs, FMX.Text, FMX.ListBox,
  FMX.Controls.Presentation, FMX.ImgList, FMX.Menus, FMX.ActnList, FMX.Objects,
  FMX.EditBox, FMX.NumberBox, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  PXL.TypeDef, PXL.Types, PXL.Timing, PXL.Devices, PXL.Canvas, PXL.Providers, PXL.FMBridge,
  se.game.types, se.game.assets, se.game.exlib.freetype, se.game.exlib.ui,
  frmImageManager, frmSelectColor, frmPreview,
  editor.types, editor.configs, editor.manager, editor.controls, editor.propeditor;

type
  TMainFrm = class(TForm)
{$REGION 'Main Form Controls'}
    pnlToolBar: TPanel;
    pnlLeft: TPanel;
    tvStruct: TTreeView;
    Splitter1: TSplitter;
    sbkEditor: TStyleBook;
    Splitter2: TSplitter;
    fsbEditor: TFramedScrollBox;
    sbrEditor: TStatusBar;
    ilEditor: TImageList;
    mbrEditor: TMenuBar;
    miFile: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miEdit: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miInsert: TMenuItem;
    miOptions: TMenuItem;
    miAbout: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem13: TMenuItem;
    alEditor: TActionList;
    actExit: TAction;
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    actUndo: TAction;
    actRedo: TAction;
    actCut: TAction;
    actCopy: TAction;
    actPaste: TAction;
    actBringtoFront: TAction;
    actSendtoBack: TAction;
    actBringForward: TAction;
    actSendBackward: TAction;
    actDelete: TAction;
    actAlignLeft: TAction;
    actAlignRight: TAction;
    actAlignBottom: TAction;
    actAlignTop: TAction;
    actTextAlignLeft: TAction;
    actTextAlignRight: TAction;
    actTextAlignCenter: TAction;
    actTextAlignTop: TAction;
    actTextAlignBottom: TAction;
    actTextAlignMiddle: TAction;
    actAddForm: TAction;
    actAddButton: TAction;
    actAddLabel: TAction;
    actAddEditBox: TAction;
    actAddImage: TAction;
    actAddListBox: TAction;
    actAddCheckBox: TAction;
    actAddRadioButton: TAction;
    actAddProgressBar: TAction;
    actAddTrackBar: TAction;
    actDistributeH: TAction;
    actDistributeV: TAction;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    Panel3: TPanel;
    Button18: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    pnlTool2: TPanel;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    pnlInspector: TPanel;
    lvInspector: TListView;
    pmRender: TPopupMenu;
    Button1: TButton;
    Button2: TButton;
    Button6: TButton;
    Button10: TButton;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Button11: TButton;
    Rectangle3: TRectangle;
    Button12: TButton;
    Rectangle4: TRectangle;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Rectangle5: TRectangle;
    pnlTool1: TPanel;
    Button17: TButton;
    Button19: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    Button24: TButton;
    Button25: TButton;
    Button26: TButton;
    Rectangle6: TRectangle;
    Rectangle7: TRectangle;
    Button27: TButton;
    Rectangle8: TRectangle;
    Button28: TButton;
    Rectangle9: TRectangle;
    Button29: TButton;
    Button30: TButton;
    Button31: TButton;
    Button32: TButton;
    Rectangle10: TRectangle;
    Button33: TButton;
    Button34: TButton;
    Button35: TButton;
    Button36: TButton;
    Rectangle11: TRectangle;
    Button37: TButton;
    Button38: TButton;
    Rectangle12: TRectangle;
    Button39: TButton;
    Button40: TButton;
    Button41: TButton;
    Button42: TButton;
    Button43: TButton;
    Button44: TButton;
    btnLock: TButton;
    Rectangle13: TRectangle;
    Button20: TButton;
    Button45: TButton;
    Button46: TButton;
    Button47: TButton;
    Button49: TButton;
    Button50: TButton;
    Button51: TButton;
    Button52: TButton;
    Button53: TButton;
    Rectangle14: TRectangle;
    Button55: TButton;
    Button56: TButton;
    Rectangle15: TRectangle;
    ckxHidden: TCheckBox;
    Rectangle16: TRectangle;
    btnSetDisplaySize: TButton;
    Rectangle17: TRectangle;
    actLock: TAction;
    actAddMemo: TAction;
    MenuItem53: TMenuItem;
    odlgEditor: TOpenDialog;
    sdlgEditor: TSaveDialog;
    rectPropEditor: TRectangle;
    rectRender: TRectangle;
    actPreview: TAction;
    Button7: TButton;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    actImageManager: TAction;
    MenuItem56: TMenuItem;
    actOptions: TAction;
    MenuItem57: TMenuItem;
    nbWidth: TNumberBox;
    nbHeight: TNumberBox;
    actTexturePacker: TAction;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
{$ENDREGION}
    procedure btnSetDisplaySizeClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pnlRenderClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tvStructChange(Sender: TObject);
    procedure lvInspectorChange(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actAddFormExecute(Sender: TObject);
    procedure actNewExecute(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actSaveExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure OnAddControlExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actLockExecute(Sender: TObject);
    procedure ckxHiddenChange(Sender: TObject);
    procedure actPreviewExecute(Sender: TObject);
    procedure alEditorUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure actImageManagerExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actTexturePackerExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure actAlignLeftExecute(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure actAlignRightExecute(Sender: TObject);
    procedure actAlignTopExecute(Sender: TObject);
    procedure actAlignBottomExecute(Sender: TObject);
    procedure actDistributeHExecute(Sender: TObject);
    procedure actDistributeVExecute(Sender: TObject);
    procedure actCutExecute(Sender: TObject);
    procedure actCopyExecute(Sender: TObject);
    procedure actPasteExecute(Sender: TObject);
  private
    FFMBridge: TFMBridge;
    FScreenScale: Single;
    FDeviceProvider: TGraphicsDeviceProvider;
    FEngineDevice: TCustomDevice;
    FEngineCanvas: TCustomCanvas;
  private
    FEditorManager: TEditorManager;
    FPropEditor: TPropEditor;
    FImageManagerForm: TImageManagerFrm;
    FSelectColorForm: TSelectColorFrm;
    FUIFile: string;
  end;

var
  MainFrm: TMainFrm;

implementation

uses frmOptions, frmTexturePacker;

{$R *.fmx}

procedure TMainFrm.FormCreate(Sender: TObject);
begin
  // PXL
  FFMBridge:= TFMBridge.Create;
  FDeviceProvider:= FFMBridge.CreateProvider;
  FScreenScale:= FFMBridge.ScreenScale;
  FEngineDevice:= FDeviceProvider.CreateDevice;
  if (FEngineDevice is TCustomStateDevice) and
     (not TCustomStateDevice(FEngineDevice).Initialize) then Exit;
  FEngineCanvas:= FDeviceProvider.CreateCanvas(FEngineDevice);
  if not FEngineCanvas.Initialize then Exit;
  // AssetManager
  AssetManager.Canvas:= FEngineCanvas;
  AssetManager.Mapping(Configs.AssetRoot, [TAssetType.atPng, TAssetType.atTTF],
    [TAssetMode.amNormal, TAssetMode.amAtlas]);
  //
  FEditorManager:= TEditorManager.Create(nil, FEngineCanvas);
  FEditorManager.Container:= rectRender;
  FEditorManager.Structure:= tvStruct;
  FEditorManager.Inspector:= lvInspector;
  FEditorManager.Root.Left:= 0;
  FEditorManager.Root.Top:= 0;
  FEditorManager.Root.Width:= Trunc(rectRender.Width);
  FEditorManager.Root.Height:= Trunc(rectRender.Height);
  //
  sdlgEditor.InitialDir:= Configs.SaveDir;
  //
  FImageManagerForm:= TImageManagerFrm.Create(Self);
  FImageManagerForm.Initialize;
  FImageManagerForm.Hide;
  //
  FSelectColorForm:= TSelectColorFrm.Create(Self);
  FSelectColorForm.Hide;
  //
  FPropEditor:= TPropEditor.Create(rectPropEditor);
  FPropEditor.SelectImageForm:= FImageManagerForm;
  FPropEditor.SelectColorForm:= FSelectColorForm;
  FPropEditor.OnDataChange:=
    procedure(AParam: TParam)
    begin
      FEditorManager.DoPropDataChange(AParam);
    end;
  //
  actAddButton.Tag:= Ord(TEditorControlType.ectButton);
  actAddButton.OnExecute:= OnAddControlExecute;
  actAddLabel.Tag:= Ord(TEditorControlType.ectLabel);
  actAddLabel.OnExecute:= OnAddControlExecute;
  actAddEditBox.Tag:= Ord(TEditorControlType.ectEditBox);
  actAddEditBox.OnExecute:= OnAddControlExecute;
  actAddImage.Tag:= Ord(TEditorControlType.ectImage);
  actAddImage.OnExecute:= OnAddControlExecute;
  actAddListBox.Tag:= Ord(TEditorControlType.ectListBox);
  actAddListBox.OnExecute:= OnAddControlExecute;
  actAddCheckBox.Tag:= Ord(TEditorControlType.ectCheckBox);
  actAddCheckBox.OnExecute:= OnAddControlExecute;
  actAddRadioButton.Tag:= Ord(TEditorControlType.ectRadioButton);
  actAddRadioButton.OnExecute:= OnAddControlExecute;
  actAddProgressBar.Tag:= Ord(TEditorControlType.ectProgressBar);
  actAddProgressBar.OnExecute:= OnAddControlExecute;
//  actAddTrackBar.Tag:= Ord(TEditorControlType.ectTrackBar);
//  actAddTrackBar.OnExecute:= OnAddControlExecute;
//  actAddMemo.Tag:= Ord(TEditorControlType.ectMemo);
//  actAddMemo.OnExecute:= OnAddControlExecute;
  //
  actPreview.Enabled:= False;
end;

procedure TMainFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FEngineCanvas);
  FreeAndNil(FEngineDevice);
  FreeAndNil(FDeviceProvider);
  FreeAndNil(FFMBridge);
  //
  FreeAndNil(FEditorManager);
  FreeAndNil(FPropEditor);
  FreeAndNil(FImageManagerForm);
  FreeAndNil(FSelectColorForm);
end;

procedure TMainFrm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  FEditorManager.MultiSelect:= Shift = [ssCtrl];
  if Key = vkLeft then FEditorManager.MoveLeft(2-(Shift = [ssCtrl]).ToInteger);
  if Key = vkRight then FEditorManager.MoveRight(2-(Shift = [ssCtrl]).ToInteger);
  if Key = vkUp then FEditorManager.MoveUp(2-(Shift = [ssCtrl]).ToInteger);
  if Key = vkDown then FEditorManager.MoveDown(2-(Shift = [ssCtrl]).ToInteger);
  if (Shift = [ssCtrl]) and (Key = vkX) then FEditorManager.Cut;
  if (Shift = [ssCtrl]) and (Key = vkC) then FEditorManager.Copy;
  if (Shift = [ssCtrl]) and (Key = vkV) then FEditorManager.Paste;
end;

procedure TMainFrm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  FEditorManager.MultiSelect:= False;
end;

procedure TMainFrm.alEditorUpdate(Action: TBasicAction; var Handled: Boolean);
begin
  actPreview.Enabled     := Assigned(tvStruct.Selected) and
                            Assigned(FEditorManager.Selected[0]);
  actAlignLeft.Enabled   := FEditorManager.LayoutControlCount > 1;
  actAlignTop.Enabled    := actAlignLeft.Enabled;
  actAlignRight.Enabled  := actAlignLeft.Enabled;
  actAlignBottom.Enabled := actAlignLeft.Enabled;
  actDistributeH.Enabled := FEditorManager.LayoutControlCount > 2;
  actDistributeV.Enabled := actDistributeH.Enabled;
end;

procedure TMainFrm.actNewExecute(Sender: TObject);
begin
  if sdlgEditor.Execute then
  begin
    FUIFile:= sdlgEditor.FileName;
    FEditorManager.Reset;
  end;
end;

procedure TMainFrm.actOpenExecute(Sender: TObject);
begin
  if odlgEditor.Execute then
  begin
    FUIFile:= odlgEditor.FileName;
    FEditorManager.Load(FUIFile);
  end;
end;

procedure TMainFrm.actSaveExecute(Sender: TObject);
begin
  if FUIFile.IsEmpty then
  begin
    if sdlgEditor.Execute then
      FUIFile:= sdlgEditor.FileName
    else Exit;
  end;
  FEditorManager.Save(FUIFile);
  ShowMessage('保存成功');
end;

procedure TMainFrm.actSaveAsExecute(Sender: TObject);
begin
  if sdlgEditor.Execute then
  begin
    FEditorManager.Save(sdlgEditor.FileName);
    ShowMessage('保存成功');
  end;
end;

procedure TMainFrm.actPreviewExecute(Sender: TObject);
begin
  try
    TPreviewFrm.ShowMe(Self, Round(rectRender.Width), Round(rectRender.Height),
      FEditorManager.Selected[0].OwnerForm.AForm);
  finally
    AssetManager.Canvas:= FEngineCanvas;
  end;
end;

procedure TMainFrm.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainFrm.actImageManagerExecute(Sender: TObject);
begin
  FImageManagerForm.ShowManager;
end;

procedure TMainFrm.actOptionsExecute(Sender: TObject);
begin
  if TOptionsFrm.ShowMe(Self) then
  begin
    FImageManagerForm.Initialize;
    sdlgEditor.InitialDir:= Configs.SaveDir;
  end;
end;

procedure TMainFrm.actTexturePackerExecute(Sender: TObject);
begin
  TTexturePackerFrm.ShowMe;
end;

procedure TMainFrm.actLockExecute(Sender: TObject);
begin
  if not Assigned(tvStruct.Selected) then Exit;
  if actLock.Checked then
    FEditorManager.Unlock(tvStruct.Selected.Tag)
  else
    FEditorManager.Lock(tvStruct.Selected.Tag);
  //
  actLock.Checked:= not actLock.Checked;
  btnLock.ImageIndex:= actLock.ImageIndex + actLock.Checked.ToInteger;
  btnLock.IsPressed:= actLock.Checked;
end;

procedure TMainFrm.ckxHiddenChange(Sender: TObject);
begin
  if not Assigned(tvStruct.Selected) then Exit;
  if ckxHidden.IsChecked then
    FEditorManager.Hide(tvStruct.Selected.Tag)
  else
    FEditorManager.Show(tvStruct.Selected.Tag);
  //
  FEditorManager.Refresh(tvStruct.Selected.Tag, 'Visible');
end;

procedure TMainFrm.btnSetDisplaySizeClick(Sender: TObject);
begin
  rectRender.Width:= nbWidth.Value;
  rectRender.Height:= nbHeight.Value;
  FEditorManager.Root.Width:= Trunc(nbWidth.Value);
  FEditorManager.Root.Height:= Trunc(nbHeight.Value);
end;

procedure TMainFrm.pnlRenderClick(Sender: TObject);
begin
  tvStruct.Selected:= nil;
  FEditorManager.Select(-1);
end;

procedure TMainFrm.OnAddControlExecute(Sender: TObject);
begin
  if not (Sender is TAction) then Exit;
  if not Assigned(tvStruct.Selected) then Exit;
  FEditorManager.AddControl(tvStruct.Selected, TEditorControlType(TAction(Sender).Tag));
end;

procedure TMainFrm.actAddFormExecute(Sender: TObject);
begin
  FEditorManager.AddForm(tvStruct);
end;

procedure TMainFrm.actCutExecute(Sender: TObject);
begin
  FEditorManager.Cut;
end;

procedure TMainFrm.actCopyExecute(Sender: TObject);
begin
  FEditorManager.Copy;
end;

procedure TMainFrm.actPasteExecute(Sender: TObject);
begin
  FEditorManager.Paste;
end;

procedure TMainFrm.actAlignLeftExecute(Sender: TObject);
begin
  FEditorManager.AlignLeft;
end;

procedure TMainFrm.actAlignTopExecute(Sender: TObject);
begin
  FEditorManager.AlignTop;
end;

procedure TMainFrm.actAlignRightExecute(Sender: TObject);
begin
  FEditorManager.AlignRight;
end;

procedure TMainFrm.actAlignBottomExecute(Sender: TObject);
begin
  FEditorManager.AlignBottom;
end;

procedure TMainFrm.actDistributeHExecute(Sender: TObject);
begin
  FEditorManager.DistributeH;
end;

procedure TMainFrm.actDistributeVExecute(Sender: TObject);
begin
  FEditorManager.DistributeV;
end;

procedure TMainFrm.actDeleteExecute(Sender: TObject);
var
  I: Integer;
begin
  if not Assigned(tvStruct.Selected) then Exit;
  I:= tvStruct.Selected.GlobalIndex - 1;
  FEditorManager.Delete(tvStruct.Selected.Tag);
  if I > 0 then
    tvStruct.ItemByGlobalIndex(I).Select
  else
    tvStruct.Selected:= nil;
end;

procedure TMainFrm.tvStructChange(Sender: TObject);
begin
  if not Assigned(tvStruct.Selected) then Exit;
  FEditorManager.Select(tvStruct.Selected.Tag);
  FPropEditor.Clear;
end;

procedure TMainFrm.lvInspectorChange(Sender: TObject);
var
  LItems: TStrings;
  LSelected: TListViewItem;
begin
  if not Assigned(lvInspector.Selected) then Exit;
  LSelected:= TListViewItem(lvInspector.Selected);
  FPropEditor.Clear;
  FPropEditor.ControlID:= FEditorManager.Selected[0].ID;
  FPropEditor.InspectorItem:= LSelected;
  rectPropEditor.Height:= 40;
  case TPropertyType(lvInspector.Selected.Tag) of
    ptString:
      FPropEditor.AddEdit(LSelected.Text, LSelected.Detail);
    ptInteger:
      FPropEditor.AddNumberEdit(LSelected.Text, LSelected.Detail, TNumValueType.Integer);
    ptFloat:
      FPropEditor.AddNumberEdit(LSelected.Text, LSelected.Detail, TNumValueType.Float);
    ptBoolean:
      FPropEditor.AddCheckBox(LSelected.Text, LSelected.Detail.ToBoolean);
    ptColor:
      FPropEditor.AddEditButton_GetColor(LSelected.Text, LSelected.Detail);
    ptImage:
      FPropEditor.AddEditButton_GetImage(LSelected.Text, LSelected.Detail);
    ptList:
      begin
        LItems:= FEditorManager.Data[LSelected.Data['key'].AsString];
        with FPropEditor.AddComboBox(LSelected.Text) do
        begin
          if Assigned(LItems) then
          begin
            Items.Assign(LItems);
            ItemIndex:= Items.IndexOf(LSelected.Detail);
          end;
        end;
      end;
    ptEnumSet:
      begin
        LItems:= FEditorManager.Data[LSelected.Data['key'].AsString];
        rectPropEditor.Height:= 40 * LItems.Count;
        FPropEditor.AddCheckBoxs(LSelected.Text, LItems.ToStringArray, LSelected.Detail);
      end;
  end;
end;

end.
