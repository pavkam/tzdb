program TZVisualizer;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  Decomposer in 'Decomposer.pas',
  TZDB in '..\TZDBPK\TZDB.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
