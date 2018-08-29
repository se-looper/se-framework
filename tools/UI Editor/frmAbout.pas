unit frmAbout;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls;

type
  TAboutFrm = class(TForm)
    btnClose: TButton;
  private
    { Private declarations }
  public
    class procedure ShowMe(AOwner: TForm = nil);
  end;

var
  AboutFrm: TAboutFrm;

implementation

{$R *.fmx}

{ TAboutFrm }

class procedure TAboutFrm.ShowMe(AOwner: TForm);
var
  LForm: TAboutFrm;
begin
  LForm:= TAboutFrm.Create(AOwner);
  try
    LForm.ShowModal;
  finally
    LForm.Free;
  end;
end;

end.
