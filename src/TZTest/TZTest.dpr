program TZTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$INCLUDE '..\TZDBPK\Version.inc'}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
{$IFNDEF FPC}
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
{$ELSE}
  FPCUnit, TestUtils, TestRegistry,
  ConsoleTestRunner, CustApp,
{$ENDIF}
  TestTZDB in 'TestTZDB.pas',
  TZDB in '..\TZDBPK\TZDB.pas',
  KnownTZCases in 'KnownTZCases.pas',
  TZCAPI in '..\TZDBLIB\TZCAPI.pas';

{$IFDEF FPC}
var
  LRunner: TCustomApplication;
begin
  LRunner := TTestRunner.Create(nil);
  try
    LRunner.Initialize;
    LRunner.Title := 'FPCUnit Console Test Case runner.';
    LRunner.Run;
  finally
    LRunner.Free;
  end;
end.
{$ELSE}

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.
{$ENDIF}