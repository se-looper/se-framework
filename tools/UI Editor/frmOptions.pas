unit frmOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  editor.configs, FMX.Objects;

type
  TOptionsFrm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    Label1: TLabel;
    edtAssetRoot: TEdit;
    btnBrowse: TButton;
    Line1: TLine;
    Label2: TLabel;
    edtSaveDir: TEdit;
    Button1: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    class function ShowMe(AOwner: TForm = nil): Boolean;
  end;

var
  OptionsFrm: TOptionsFrm;

implementation

{$R *.fmx}

{ TOptionsFrm }

procedure TOptionsFrm.btnCancelClick(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

procedure TOptionsFrm.btnOKClick(Sender: TObject);
begin
  Configs.AssetRoot:= edtAssetRoot.Text;
  Configs.SaveDir:= edtSaveDir.Text;
  Configs.Apply;
  Close;
  Self.ModalResult:= mrOK;
end;

class function TOptionsFrm.ShowMe(AOwner: TForm): Boolean;
var
  LForm: TOptionsFrm;
begin
  LForm:= TOptionsFrm.Create(AOwner);
  try
    LForm.edtAssetRoot.Text:= Configs.AssetRoot;
    LForm.edtSaveDir.Text:= Configs.SaveDir;
    Result:= LForm.ShowModal = mrOK;
  finally
    LForm.Free;
  end;
end;

end.
