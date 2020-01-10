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

program TZCompile;

{$INCLUDE '../TZDBPK/Version.inc'}

{$IFDEF DELPHI}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  DateUtils,
  Character,
  Classes,
  Types,
  StrUtils,
  TZSchema in 'TZSchema.pas',
  TZStrs in 'TZStrs.pas';

var
  C: Char;
  LInputDir, LOutputFile, LVersion: string;
begin
  { Process parameters }
  WriteLn(CCLIHeader);

  if ParamCount() < 3 then
  begin
    WriteLn(CCLIUsage);
    Exit;
  end;

  LInputDir := ParamStr(1);
  LOutputFile := ParamStr(2);
  LVersion := ParamStr(3);

  { Validate the version number. }
  for C in LVersion do
  begin
    if not CharInSet(C, ['0' .. '9', 'a' .. 'z']) Then
      CLIFatal(Format(CCLIBadVersion, [LVersion]));
  end;

  { Verify input directory }
  if not DirectoryExists(LInputDir) then
    CLIFatal(Format(CCLIBadInputDir, [LInputDir]));

  { Verify output directory }
  if not DirectoryExists(ExtractFilePath(LOutputFile)) then
    CLIFatal(Format(CCLIBadOutputDir, [ExtractFilePath(LOutputFile)]));

  { Start the process! }
  try
    Process(LInputDir, LOutputFile, LVersion);
  except
    on E: Exception do
    begin
      CLIFatal(Format(CCLIGlobalException, [E.ClassName, E.Message]));
    end;
  end;
end.
