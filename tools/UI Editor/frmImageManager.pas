unit frmImageManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.IOUtils,
  System.Variants, System.Generics.Collections, System.ImageList,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ImgList,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.EditBox, FMX.NumberBox, FMX.Edit,
  FMX.Objects, FMX.Layouts, FMX.TreeView, XSuperObject,
  PXL.Types, se.game.types, se.game.assets, editor.types, editor.Selection;

type
  TControlMode = (cmManager, cmSelect);
  TImageManagerFrm = class(TForm)
    sbarImage: TStatusBar;
    Rectangle1: TRectangle;
    btnOK: TButton;
    btnCancel: TButton;
    GroupBox2: TGroupBox;
    nbSelTop: TNumberBox;
    nbSelLeft: TNumberBox;
    nbSelRight: TNumberBox;
    nbSelBottom: TNumberBox;
    Splitter1: TSplitter;
    Rectangle2: TRectangle;
    tvImages: TTreeView;
    Rectangle3: TRectangle;
    edtSearch: TEdit;
    btnSearch: TButton;
    ilImageTree: TImageList;
    txtImageSize: TText;
    Line1: TLine;
    Line2: TLine;
    txtSelectedRect: TText;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnClose: TButton;
    tbZoom: TTrackBar;
    txtZoom: TText;
    Line3: TLine;
    txtPos: TText;
    Rectangle4: TRectangle;
    fsbImage: TFramedScrollBox;
    imgSource: TImage;
    rectChunk: TRectangle;
    rectSelectModes: TRectangle;
    btnClearSearch: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure tvImagesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure imgSourceMouseEnter(Sender: TObject);
    procedure imgSourceMouseLeave(Sender: TObject);
    procedure imgSourceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure imgSourceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure edtSearchKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    FAtlasMap: TDictionary<string, TArray<TTextureChunk>>;
    function ParseAtlasChunk(const AAtlasFile: string): TArray<TTextureChunk>;
  private
    FPreLocatedIndex: Integer;
    procedure ChangeControlMode(const AMode: TControlMode);
    procedure SelectRect(const ARect: TIntRect);
    procedure SelectItem(const ANode: TTreeViewItem; const AItemName: string);
    function Match(const ASource, AMatchText: string): Boolean;
  public
    procedure Initialize;
    procedure ShowManager;
    function SelectImage(out AImageFileName: string; out ARect: TRect): Boolean;
  end;

var
  ImageManagerFrm: TImageManagerFrm;

implementation

{$R *.fmx}

{ TImageManagerFrm }

procedure TImageManagerFrm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TImageManagerFrm.btnCancelClick(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

procedure TImageManagerFrm.btnOKClick(Sender: TObject);
begin
  if not Assigned(tvImages.Selected) then
  begin
    MessageDlg('Select a image.', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    Exit;
  end;
  //
  Close;
  Self.ModalResult:= mrOK;
end;

procedure TImageManagerFrm.FormCreate(Sender: TObject);
begin
  FAtlasMap:= TDictionary<string, TArray<TTextureChunk>>.Create;
  //
  txtImageSize.Text:= 'Image Size: --';
  txtSelectedRect.Text:= 'Selected Rect: --';
  rectChunk.Visible:= False;
  FPreLocatedIndex:= -1;
  btnClearSearch.Visible:= False;
end;

procedure TImageManagerFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FAtlasMap);
end;

procedure TImageManagerFrm.ShowManager;
begin
  Caption:= 'Browse Image';
  ChangeControlMode(TControlMode.cmManager);
  Self.ShowModal;
end;

function TImageManagerFrm.SelectImage(out AImageFileName: string;
  out ARect: TRect): Boolean;
var
  LSelectedItem: TTreeViewItem;
begin
  Caption:= 'Select Image';
  ChangeControlMode(TControlMode.cmSelect);
  Result:= Self.ShowModal = mrOK;
  if Result then
  begin
    case tvImages.Selected.Tag of
      1: LSelectedItem:= tvImages.Selected;
      2: LSelectedItem:= tvImages.Selected.ParentItem;
      else Exit;
    end;
    AImageFileName:= ExtractFileName(LSelectedItem.Text);
    ARect.Left:= Round(nbSelLeft.Value);
    ARect.Top:= Round(nbSelTop.Value);
    ARect.Right:= Round(nbSelRight.Value);
    ARect.Bottom:= Round(nbSelBottom.Value);
  end;
end;

procedure TImageManagerFrm.SelectItem(const ANode: TTreeViewItem;
  const AItemName: string);
var
  I: Integer;
begin
  for I:= 0 to ANode.Count -1 do
    if ANode.Items[I].Text.Equals(AItemName) then
      ANode.Items[I].Select;
end;

procedure TImageManagerFrm.ChangeControlMode(const AMode: TControlMode);
begin
  btnClose.Visible:= AMode=TControlMode.cmManager;
  btnOK.Visible:= AMode=TControlMode.cmSelect;
  btnCancel.Visible:= AMode=TControlMode.cmSelect;
end;

procedure TImageManagerFrm.edtSearchKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  btnClearSearch.Visible:= not edtSearch.Text.IsEmpty;
end;

procedure TImageManagerFrm.imgSourceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  LRect: TRect;
  LImageNode: TTreeViewItem;
  LChunkArray: TArray<TTextureChunk>;
  I: Integer;
begin
  case tvImages.Selected.Tag of
    1: LImageNode:= tvImages.Selected;
    2: LImageNode:= tvImages.Selected.ParentItem;
    else Exit;
  end;
  if not FAtlasMap.TryGetValue(ExtractFileName(LImageNode.Text), LChunkArray) then Exit;
  for I:= 0 to Length(LChunkArray) -1 do
  begin
    if LChunkArray[I].Rect.Contains(Point2i(Round(X), Round(Y))) then
    begin
      SelectRect(LChunkArray[I].Rect);
      SelectItem(LImageNode, LChunkArray[I].Name);
      Exit;
    end;
  end;
end;

procedure TImageManagerFrm.imgSourceMouseEnter(Sender: TObject);
begin
  txtPos.Text:= 'Mouse: x=0, y=0';
end;

procedure TImageManagerFrm.imgSourceMouseLeave(Sender: TObject);
begin
  txtPos.Text:= 'Mouse: --';
end;

procedure TImageManagerFrm.imgSourceMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  txtPos.Text:= Format('Mouse: x=%d, y=%d',[Round(X), Round(Y)]);
end;

function TImageManagerFrm.ParseAtlasChunk(const AAtlasFile: string): TArray<TTextureChunk>;
var
  jo: ISuperObject;
  I: Integer;
begin
  jo:= TSuperObject.ParseFile(AAtlasFile);
  try
    SetLength(Result, jo.A['chunk'].Length);
    for I:= 0 to jo.A['chunk'].Length -1 do
    begin
      Result[I].Name:= jo.A['chunk'].O[I].S['name'];
      Result[I].Rect.Left  := jo.A['chunk'].O[I].I['left'];
      Result[I].Rect.Top   := jo.A['chunk'].O[I].I['top'];
      Result[I].Rect.Right := jo.A['chunk'].O[I].I['right'];
      Result[I].Rect.Bottom:= jo.A['chunk'].O[I].I['bottom'];
    end;
  finally
    jo:= nil;
  end;
end;

procedure TImageManagerFrm.Initialize;
var
  LDirs, LFiles: TStringDynArray;
  LDir: string;
  I, J, K: Integer;
  LRootItem, LDirItem, LFileItem: TTreeViewItem;
  LChunkArray: TArray<TTextureChunk>;
begin
  FPreLocatedIndex:= -1;
  tvImages.Clear;
  tvImages.BeginUpdate;
  try
    LRootItem:= TTreeViewItem.Create(tvImages);
    LRootItem.Parent:= tvImages;
    LRootItem.Text:= AssetManager.Root;
    LRootItem.ImageIndex:= 0;
    //
    LDirs:= TDirectory.GetDirectories(AssetManager.Root, '*', TSearchOption.soTopDirectoryOnly);
    for I:= 0 to Length(LDirs) -1 do
    begin
      LDirItem:= TTreeViewItem.Create(tvImages);
      LDirItem.Parent:= LRootItem;
      LDirItem.Text:= LDirs[I];
      LDirItem.ImageIndex:= 1;
      //
      LDir:= IncludeTrailingPathDelimiter(LDirs[I]);
      LFiles:= TDirectory.GetFiles(LDir, '*.png', TSearchOption.soAllDirectories);
      for J:= 0 to Length(LFiles) -1 do
      begin
        LFileItem:= TTreeViewItem.Create(tvImages);
        LFileItem.Parent:= LDirItem;
        LFileItem.Text:= LFiles[J].Replace(LDir, '');
        LFileItem.Tag:= 1;
        LFileItem.ImageIndex:= LFileItem.Text.CountChar('\') + 2;
        // atlas found
        if FileExists(LFiles[J].Replace('.png', '.atlas')) then
        begin
          LChunkArray:= ParseAtlasChunk(LFiles[J].Replace('.png', '.atlas'));
          FAtlasMap.AddOrSetValue(LFileItem.Text, LChunkArray);
          for K:= 0 to Length(LChunkArray) -1 do
          begin
            with TTreeViewItem.Create(tvImages) do
            begin
              Parent:= LFileItem;
              Text:= LChunkArray[K].Name;
              Tag:= 2;
              ImageIndex:= 5;
            end;
          end;
        end;
      end;
    end;
  finally
    tvImages.EndUpdate;
  end;
  tvImages.Items[0].Expand;
end;

function TImageManagerFrm.Match(const ASource, AMatchText: string): Boolean;
begin
  if AMatchText.Trim.IsEmpty then Exit(True);
  { % text }
  if AMatchText.StartsWith('%') and (not AMatchText.EndsWith('%')) then
    Exit(ASource.EndsWith(AMatchText.Substring(1, AMatchText.Length)));
  { text % }
  if (not AMatchText.StartsWith('%')) and AMatchText.EndsWith('%') then
    Exit(ASource.StartsWith(AMatchText.Substring(0, AMatchText.Length-1)));
  { % text % }
  if AMatchText.StartsWith('%') and AMatchText.EndsWith('%') then
    Exit(ASource.Contains(AMatchText.Substring(1, AMatchText.Length-2)));
  if not (AMatchText.StartsWith('%') or AMatchText.EndsWith('%')) then
    Exit(ASource.Contains(AMatchText));
end;

procedure TImageManagerFrm.SelectRect(const ARect: TIntRect);
begin
  rectChunk.Position.X:= ARect.Left;
  rectChunk.Position.Y:= ARect.Top;
  rectChunk.Width:= ARect.Width;
  rectChunk.Height:= ARect.Height;
  rectChunk.Visible:= True;
  //
  nbSelLeft.Value:= ARect.Left;
  nbSelTop.Value:= ARect.Top;
  nbSelRight.Value:= ARect.Right;
  nbSelBottom.Value:= ARect.Bottom;
  //
  txtSelectedRect.Text:= Format('Selected Rect: %d,%d,%d,%d',
    [ARect.Left, ARect.Top, ARect.Width, ARect.Height]);
end;

procedure TImageManagerFrm.tvImagesChange(Sender: TObject);
var
  LFile: string;
  LRect: TIntRect;
  LChunkArray: TArray<TTextureChunk>;
  I: Integer;
begin
  rectChunk.Visible:= False;
  nbSelLeft.Value:= 0;
  nbSelTop.Value:= 0;
  nbSelRight.Value:= 0;
  nbSelBottom.Value:= 0;
  imgSource.Width:= 512;
  imgSource.Height:= 256;
  imgSource.Bitmap.Clear(TAlphaColorRec.Null);
  txtImageSize.Text:= 'Image Size: --';
  txtSelectedRect.Text:= 'Selected Rect: --';
  txtPos.Text:= 'Mouse: x=0, y=0';
  if not Assigned(tvImages.Selected) then Exit;
  case tvImages.Selected.Tag of
    1:
      begin
        LFile:= tvImages.Selected.ParentItem.Text + '\' + tvImages.Selected.Text;
        imgSource.Bitmap.LoadFromFile(LFile);
        imgSource.Width:= imgSource.Bitmap.Image.Width;
        imgSource.Height:= imgSource.Bitmap.Image.Height;
        //
        LRect.Left:= 0;
        LRect.Top:= 0;
        LRect.Width:= Round(imgSource.Width);
        LRect.Height:= Round(imgSource.Height);
      end;
    2:
      begin
        LFile:= tvImages.Selected.ParentItem.ParentItem.Text + '\' + tvImages.Selected.ParentItem.Text;
        imgSource.Bitmap.LoadFromFile(LFile);
        imgSource.Width:= imgSource.Bitmap.Image.Width;
        imgSource.Height:= imgSource.Bitmap.Image.Height;
        //
        if not FAtlasMap.TryGetValue(tvImages.Selected.ParentItem.Text, LChunkArray) then Exit;
        for I:= 0 to Length(LChunkArray) -1 do
        begin
          if LChunkArray[I].Name.Equals(tvImages.Selected.Text) then
            LRect:= LChunkArray[I].Rect;
        end;
      end;
    else Exit;
  end;
  SelectRect(LRect);
  txtImageSize.Text:= Format('Image Size: %dx%d', [LRect.Width, LRect.Height]);
end;

procedure TImageManagerFrm.btnClearSearchClick(Sender: TObject);
begin
  edtSearch.Text:= '';
  btnClearSearch.Visible:= False;
end;

procedure TImageManagerFrm.btnSearchClick(Sender: TObject);
var
  I: Integer;
  LFound: Boolean;
begin
  for I:= 0 to tvImages.GlobalCount -1 do
  begin
    if not Match(tvImages.ItemByGlobalIndex(I).Text, edtSearch.Text) then continue;
    if I <= FPreLocatedIndex then
      continue
    else
    begin
      tvImages.ItemByGlobalIndex(I).Select;
      FPreLocatedIndex:= I;
      LFound:= True;
      break;
    end;
  end;
  if not LFound then
    FPreLocatedIndex:= -1;
end;

end.
