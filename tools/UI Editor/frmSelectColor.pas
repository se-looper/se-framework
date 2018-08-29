unit frmSelectColor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  PXL.Types, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TSelectColorFrm = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    FSelectedColor: TIntColor;
  public
    class function ShowMe(AOwner: TForm = nil): TIntColor;
  end;

var
  SelectColorFrm: TSelectColorFrm;

implementation

{$R *.fmx}

{ TSelectColorFrm }

procedure TSelectColorFrm.btnCancelClick(Sender: TObject);
begin
  Close;
  Self.ModalResult:= mrCancel;
end;

procedure TSelectColorFrm.btnOKClick(Sender: TObject);
begin
  FSelectedColor:= IntColorWhite;
  Close;
  Self.ModalResult:= mrOK;
end;

class function TSelectColorFrm.ShowMe(AOwner: TForm): TIntColor;
var
  LForm: TSelectColorFrm;
begin
  LForm:= TSelectColorFrm.Create(AOwner);
  try
    if LForm.ShowModal = mrOK then
      Result:= LForm.FSelectedColor
    else Exit(0);
  finally
    LForm.Free;
  end;
end;

end.
