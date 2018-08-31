program Basic;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFm in 'MainFm.pas' {MainForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown:= True;

  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
