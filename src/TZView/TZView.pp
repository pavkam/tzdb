(*
* Copyright (c) 2010-2019, Alexandru Ciobanu (alex+git@ciobanu.org)
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

program TZView;

{$I ../TZDBPK/Version.inc}
{$APPTYPE CONSOLE}

uses
  SysUtils,
  DateUtils,
  Character,
  Classes,
  Types,
  StrUtils,
{$IFNDEF FPC}
  Generics.Collections,
  Generics.Defaults,
{$ELSE}
  FGL,
{$ENDIF}
  TZDB in '../TZDBPK/TZDB.pas',
  Deconstruction;

  procedure PrintHeaderAndExit;
  begin
    WriteLn('tzview - view and compare time zone data. (c) 2019 Alexandru Ciobanu (alex+git@ciobanu.org).');
    WriteLn('usage: tzview command [options...]');
    WriteLn('       --');
    WriteLn('       tzview list [all|aliases|tz]     --  lists all known time zones or aliases, or both.');
    WriteLn('       tzview dump start_year end_year  --  dumps all periods for all known timezones between the given years.');

    Halt(1);
  end;

var
  LCommand: string;
  LStartYear, LEndYear: Integer;
  LTimeZones: {$IFDEF SUPPORTS_TARRAY}TArray<string>{$ELSE}TStringDynArray{$ENDIF};
  S: string;
begin
  if (ParamCount < 1) then PrintHeaderAndExit;

  if LowerCase(ParamStr(1)) = 'list' then
  begin
    if (ParamCount < 2) then
      PrintHeaderAndExit;

    if ParamStr(2) = 'all' then
      LTimeZones := TBundledTimeZone.KnownTimeZones
    else if ParamStr(2) = 'tz' then
      LTimeZones := TBundledTimeZone.KnownTimeZones(false)
    else if ParamStr(2) = 'aliases' then
      LTimeZones := TBundledTimeZone.KnownAliases
    else
      PrintHeaderAndExit;

    for S in LTimeZones do
      WriteLn(S);
  end else if LowerCase(ParamStr(1)) = 'dump' then
  begin
    if (ParamCount < 3) then PrintHeaderAndExit;
    if (not TryStrToInt(ParamStr(2), LStartYear)) or
       (not TryStrToInt(ParamStr(3), LEndYear)) or
       (LStartYear > LEndYear)
    then PrintHeaderAndExit;
  end
  else PrintHeaderAndExit;
end.
