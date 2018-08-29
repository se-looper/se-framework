unit frmTexturePacker;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  editor.texpacker, FMX.Controls.Presentation, FMX.Edit, FMX.EditBox,
  FMX.SpinBox, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.ListBox;

type
  TTexturePackerFrm = class(TForm)
    sboxPadding: TSpinBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    FramedScrollBox1: TFramedScrollBox;
    PaintBox1: TPaintBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
    procedure Button7Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
  private
    FItems: TCustomPackerItemList;
    procedure DetermineSize(out AWidth: Integer; out AHeight: Integer);
    Procedure ClearImages;
    procedure UpdateImageList;
    procedure DrawPattern(Dest: TCanvas; Index: Cardinal);
  public
    class procedure ShowMe;
  end;

var
  TexturePackerFrm: TTexturePackerFrm;

implementation

{$R *.fmx}

{ TTexturePackerFrm }

class procedure TTexturePackerFrm.ShowMe;
begin
  TexturePackerFrm:= TTexturePackerFrm.Create(nil);
  try
    TexturePackerFrm.ShowModal;
  finally
    FreeAndNil(TexturePackerFrm);
  end;
end;

procedure TTexturePackerFrm.FormCreate(Sender: TObject);
begin
  FItems:= TCustomPackerItemList.Create;
end;

procedure TTexturePackerFrm.FormDestroy(Sender: TObject);
begin
  ClearImages;
  FreeAndNil(FItems);
end;

procedure TTexturePackerFrm.ClearImages;
var
  Index: Integer;
begin
  for Index := 0 to FItems.Count - 1 do
    FItems.List^[Index].Image.Free;
  FItems.Clear;
end;

procedure TTexturePackerFrm.DetermineSize(out AWidth, AHeight: Integer);
var
  Partition: TPartition;
  Index    : Integer;
  ARect    : TRect;
  Done     : Boolean;
  APadding : Integer;
begin
  APadding := Round(sboxPadding.Value);
  AWidth := 16;
  AHeight:= 16;
  repeat
    Partition:= TPartition.Create(0, 0, AWidth, AHeight, ePackingMode_BestFitFromNWCorner);
    Done:= True;
    for Index := 0 to FItems.Count - 1 do
    begin
      Done:= Done and Partition.Insert(FItems[Index].Width + APadding, FItems[Index].Height + APadding, ARect);
      if not Done then Break;
    end;
    Partition.Free;
    if Done then Exit;
    if AWidth <= AHeight then
      AWidth := AWidth  * 2
    else
      AHeight:= AHeight * 2;
  until Done;
end;

procedure TTexturePackerFrm.UpdateImageList;
//var
//  Index: Integer;
//  ListItem: TListItem;
begin
//  lwImages.Items.BeginUpdate;
//  lwImages.Items.Clear;
//
//  for Index := 0 to Items.Count - 1 do
//  begin
//    ListItem:= lwImages.Items.Add;
//    ListItem.Caption:= String(Items.List^[Index].Name);
//
//    if Items.List^[Index].Placed then
//    begin
//      ListItem.ImageIndex:= 0;
//
//      ListItem.SubItems.Add(IntToStr(Items.List^[Index].Position.X));
//      ListItem.SubItems.Add(IntToStr(Items.List^[Index].Position.Y));
//    end else
//    begin
//      ListItem.ImageIndex:= -1;
//
//      ListItem.SubItems.Add('');
//      ListItem.SubItems.Add('');
//    end;
//
//  end;
//
//  lwImages.Items.EndUpdate;
end;

procedure TTexturePackerFrm.DrawPattern(Dest: TCanvas; Index: Cardinal);
//var
//  Pattern: TPHXPackerItem ;
//  rPattern: TRect;
begin
//  Pattern:=Items[Index];
//
//  rPattern.Left  := (Pattern.Position.X                 );
//  rPattern.Top   := (Pattern.Position.Y                 );
//  rPattern.Right := (Pattern.Position.X + Pattern.Width );
//  rPattern.Bottom:= (Pattern.Position.Y + Pattern.Height);
//
//  with Dest do begin
//     Brush.Style:= bsClear;
//
//     Pen.Color  := clWhite;
//     Pen.Width  := 1;
//     Pen.Style  := psSolid;
//     Rectangle(rPattern.Left, rPattern.Top, rPattern.Right, rPattern.Bottom);
//
//     Pen.Color  := clBlack;
//     Pen.Width  := 1;
//     Pen.Style  := psDot;
//     Rectangle(rPattern.Left, rPattern.Top, rPattern.Right, rPattern.Bottom);
//  end;
end;

procedure TTexturePackerFrm.ListBox1Change(Sender: TObject);
begin
//  if (not btnOk.Enabled) and Selected then
//  begin
//    Items.List^[Item.Index].Bitmap.Draw(FBuffer, FTransparent);
//  end;
//  ScrollBox1.Invalidate;
end;

procedure TTexturePackerFrm.CheckBox1Change(Sender: TObject);
begin
//  cbWidth.Enabled := not cbAutoSize.Checked;
//  cbHeight.Enabled:= not cbAutoSize.Checked;
end;

procedure TTexturePackerFrm.Button5Click(Sender: TObject);
//var Index: Integer;
//var Filename: String;
//var Bitmap  : TPHXBitmap;
begin
//  if OpenImageDialog.Execute then
//  begin
//    for Index := 0 to OpenImageDialog.Files.Count - 1 do
//    begin
//      Filename:= OpenImageDialog.Files[Index] ;
//
//      Bitmap:= TPHXBitmap.Create;
//      Bitmap.LoadBitmap(Filename);
//
//      Items.Add( ChangeFileExt(ExtractFileName(Filename), ''), Bitmap);
//    end;
//  end;
//
//  for Index := 0 to Items.Count - 1 do
//  begin
//    Items.List^[Index].Placed:= False;
//  end;
//  btnOk.Enabled:= False;
//
//  UpdateImageList;
end;

procedure TTexturePackerFrm.Button6Click(Sender: TObject);
//var Index: Integer;
begin
//  if lwImages.SelCount = 0 then Exit;
//
//  Items.List^[lwImages.Selected.Index].Bitmap.Free;
//
//  Items.Delete(lwImages.Selected.Index);
//
//  for Index := 0 to Items.Count - 1 do
//  begin
//    Items.List^[Index].Placed:= False;
//  end;
//  btnOk.Enabled:= False;
//
//  UpdateImageList;
end;

procedure TTexturePackerFrm.Button7Click(Sender: TObject);
//var Index: Integer;
begin
//  for Index := 0 to Items.Count - 1 do
//  begin
//    Items.List^[Index].Bitmap.Free;
//    Items.List^[Index].Placed:= False;
//  end;
//  Items.Clear;
//  btnOk.Enabled:= False;
//
//  UpdateImageList;
end;

procedure TTexturePackerFrm.PaintBox1Paint(Sender: TObject; Canvas: TCanvas);
begin
//  PaintBox1.Width:= FBuffer.Width;
//  PaintBox1.Height:= FBuffer.Height;
//
//  PaintBox1.Canvas.Draw(0, 0, FBuffer);
//
//  if btnOk.Enabled and (lwImages.SelCount > 0) then
//  begin
//    DrawPattern(PaintBox1.Canvas, lwImages.Selected.Index);
//  end;
end;

procedure TTexturePackerFrm.Button1Click(Sender: TObject);
//var Partition: TPartition;
//var Index    : Integer;
//var ARect    : TRect;
//var AllPacked: Boolean;
//
//var Source: TRecti;
//var Dest: TVector2i;
//var AWidth  : Integer;
//var AHeight : Integer;
//var APadding: Integer;
begin
//  Screen.Cursor:= crHourGlass;
//
//  Items.Sort;
//
//  if cbAutoSize.Checked then
//  begin
//    DetermineSize(AWidth, AHeight);
//  end else
//  begin
//    AWidth := StrToIntDef( cbWidth.Text , 256);
//    AHeight:= StrToIntDef( cbHeight.Text, 256);
//  end;
//  APadding := Round( edPadding.Value );
//
//
//  Image.Resize(AWidth, AHeight, pfRGBA);
//  Image.Fill(0, 0, 0, 0);
//
//  Partition:= TPartition.Create(0, 0, AWidth, AHeight, ePackingMode_BestFitFromNWCorner);
//
//  AllPacked:= True;
//  for Index := 0 to Items.Count - 1 do
//  begin
//    Items.List^[Index].Placed:= Partition.Insert(Items[Index].Width + APadding, Items[Index].Height + APadding, ARect);
//
//    if Items.List^[Index].Placed then
//    begin
//      Source.Left  := 0;
//      Source.Top   := 0;
//      Source.Right := Items[Index].Width - 1;
//      Source.Bottom:= Items[Index].Height - 1;
//
//      Dest.X:= ARect.Left;
//      Dest.Y:= ARect.Top;
//
//      Image.CopyFrom(Items.List^[Index].Bitmap, Source, Dest);
//
//      Items.List^[Index].Position.X:= ARect.Left;
//      Items.List^[Index].Position.Y:= ARect.Top;
//    end else
//    begin
//      AllPacked:= False;
//    end;
//  end;
//  Partition.Free;
//
//  FBuffer.Canvas.FillRect(FBuffer.Canvas.ClipRect);
//
//  Image.Draw(FBuffer, FTransparent);
//
//  UpdateImageList;
//
//  ScrollBox1.Invalidate;
//  PaintBox1.Invalidate;
//
//  Screen.Cursor:= crDefault;
//
//  if not AllPacked then
//  begin
//    MessageDlg('There was not enough space in the destination image to pack'#13'all the images.', mtInformation, [mbOK], 0)
//  end;
//
//
//  btnOk.Enabled:= True;
end;

end.
