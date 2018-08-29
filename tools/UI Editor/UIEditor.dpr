program UIEditor;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMain in 'frmMain.pas' {MainFrm},
  editor.manager in 'editor.manager.pas',
  editor.controls in 'editor.controls.pas',
  editor.types in 'editor.types.pas',
  editor.propeditor in 'editor.propeditor.pas',
  frmSelectColor in 'frmSelectColor.pas' {SelectColorFrm},
  frmOptions in 'frmOptions.pas' {OptionsFrm},
  frmAbout in 'frmAbout.pas' {AboutFrm},
  frmPreview in 'frmPreview.pas' {PreviewFrm},
  editor.configs in 'editor.configs.pas',
  frmTexturePacker in 'frmTexturePacker.pas' {TexturePackerFrm},
  frmImageManager in 'frmImageManager.pas' {ImageManagerFrm},
  editor.texpacker in 'editor.texpacker.pas',
  editor.selection in 'editor.selection.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Title:= 'Tulip-UI Editor';
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TImageManagerFrm, ImageManagerFrm);
  Application.Run;
end.
