(*
* Copyright (c) 2010-2020, Alexandru Ciobanu (alex+git@ciobanu.org)
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of this library nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

program TZTest;

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