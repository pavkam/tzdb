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

program TZView;

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
{$IFNDEF FPC}
  Generics.Collections,
  Generics.Defaults,
{$ELSE}
  FGL,
{$ENDIF}
  TZDB in '../TZDBPK/TZDB.pas';

const
  CDateTimePatterns: array[0..4] of string = (
    'now',
    'yyyy-MM-dd',
    'yyyy-MM-dd hh:mm',
    'yyyy-MM-dd hh:mm:ss',
    'yyyy-MM-dd hh:mm:ss.zzz'
  );
  COutDateTimeFormat = 'yyyy-MM-dd hh:mm:ss.zzz';

function TryScanDateTime(const AStr: string; out ADateTime: TDateTime): boolean;
var
  I: Integer;
begin
  if SameText(AStr, 'now') then
  begin
    ADateTime := Now;
    Exit(true);
  end;

  for I := Length(CDateTimePatterns) - 1 downto 1 do
  try
    ADateTime := ScanDateTime(CDateTimePatterns[I], AStr);
    Exit(true);
  except
  end;

  Result := false;
end;

function FormatOffset(AOffset: Int64): string;
var
  LHours, LMinutes, LSeconds: Integer;
  LSign: Char;
begin
   if AOffset = 0 then
     Result := '0'
   else begin
    if AOffset < 0 then
      LSign := '-' else LSign := '+';
    AOffset := Abs(AOffset);

    LHours := AOffset div 3600;
    LMinutes := (AOffset mod 3600) div 60;
    LSeconds := AOffset mod 60;

    Result := LSign;
    if LHours > 0 then
      Result := Result + IntToStr(LHours) + 'h';
    if LMinutes > 0 then
      Result := Result + IntToStr(LMinutes) + 'm';
    if LSeconds > 0 then
      Result := Result + IntToStr(LSeconds) + 's';
    end;
end;

procedure PrintHeaderAndExit;
var
  I: Integer;
begin
  WriteLn('tzview - view and compare time zone data. (c) 2019 Alexandru Ciobanu (alex+git@ciobanu.org).');
  WriteLn('database v', TBundledTimeZone.DbVersion, ' provided by IANA (https://www.iana.org/time-zones).');
  WriteLn('usage: tzview command [options...]');
  WriteLn('       tzview list [all|aliases|tz]     --  lists all known time zones or aliases, or both.');
  WriteLn('       tzview dump <timezone> <year>    --  deconstructs a time zone for a given year.');
  WriteLn('       tzview local <timezone> <date>   --  displays info on a given local date/time.');
  WriteLn('       tzview utc <timezone> <date>     --  displays info on a given UTC date/time.');
  WriteLn;
  WriteLn('accepted date/time patterns:');
  for I := 0 to Length(CDateTimePatterns) - 1 do
    WriteLn('       ', CDateTimePatterns[I]);

  Halt(1);
end;

procedure ErrorAndExit(const AMessage: string);
begin
  WriteLn('[ERR] ' + AMessage);
  Halt(2);
end;

var
  LYear: Integer;
  LDate: TDateTime;
  LTimeZones: TStringDynArray;
  LCommand: string;
  S: string;
  LTZ: TBundledTimeZone;
  LSegment: TYearSegment;
begin
  if (ParamCount >= 1) then
    LCommand := Trim(ParamStr(1));

  if SameText(LCommand, 'list') then
  begin
    if (ParamCount < 2) then
      ErrorAndExit('The "list" command expects two other arguments.');

    if ParamStr(2) = 'all' then
      LTimeZones := TBundledTimeZone.KnownTimeZones
    else if ParamStr(2) = 'tz' then
      LTimeZones := TBundledTimeZone.KnownTimeZones(false)
    else if ParamStr(2) = 'aliases' then
      LTimeZones := TBundledTimeZone.KnownAliases
    else
      ErrorAndExit('The "list" command expects either "all", "tz" or "aliases".');

    for S in LTimeZones do
      WriteLn(S);
  end else if SameText(LCommand, 'dump') then
  begin
    if (ParamCount < 2) then
      ErrorAndExit('The "view" command expects two other arguments.');

    if (not TryStrToInt(Trim(ParamStr(3)), LYear)) then
      ErrorAndExit('The "view" command expects a valid year.');

    S := Trim(ParamStr(2));
    try
      LTZ := TBundledTimeZone.Create(S);

      if not SameText(LTZ.ID, S) then
        WriteLn(S + ' (' + LTZ.ID + '):')
      else
        WriteLn(LTZ.ID +  ':');

      WriteLn(
        PadRight('Period', 10),
        PadRight('Start (Local)', 25),
        PadRight('End (Local)', 25),
        PadRight('Abbrv.', 10),
        'Bias'
      );

      for LSegment in LTZ.GetYearBreakdown(LYear) do
      begin
        case LSegment.LocalType of
          lttStandard: S := 'Standard';
          lttDaylight: S := 'Daylight';
          lttAmbiguous: S := 'Ambiguous';
          lttInvalid: S := 'Invalid';
        end;

        WriteLn(
          PadRight(S, 10),
          PadRight(FormatDateTime(COutDateTimeFormat, LSegment.StartsAt), 25),
          PadRight(FormatDateTime(COutDateTimeFormat, LSegment.EndsAt), 25),
          PadRight(LSegment.DisplayName, 10),
          FormatOffset(LSegment.UtcOffset)
        );
      end;
    except
      on E: ETimeZoneInvalid do
        ErrorAndExit('The time zone "' + S + '" cannot be found.');
      on E: EUnknownTimeZoneYear do
        ErrorAndExit('The time zone "' + S + '" does not have data for year ' + IntToStr(LYear) + '.');
    end;
  end else if SameText(LCommand, 'local') or SameText(LCommand, 'utc')  then
  begin
    if (ParamCount < 2) then
      ErrorAndExit('The "' + LCommand + '" command expects two other arguments.');

    if (not TryScanDateTime(Trim(ParamStr(3)), LDate)) then
      ErrorAndExit('The "' + LCommand + '" command expects a valid date/time.');

    S := Trim(ParamStr(2));
    try
      LTZ := TBundledTimeZone.Create(S);

      if SameText(LCommand, 'utc') then
        LDate := LTZ.ToLocalTime(LDate);

      case LTZ.GetLocalTimeType(LDate) of
        lttStandard: S := 'Standard';
        lttDaylight: S := 'Daylight';
        lttAmbiguous: S := 'Ambiguous';
        lttInvalid: S := 'Invalid';
      end;

      WriteLn(
        PadRight('Period', 10),
        PadRight('Local', 25),
        PadRight('UTC', 25),
        PadRight('Abbrv.', 10),
        PadRight('Name', 10),
        'Bias'
      );

      if LTZ.GetLocalTimeType(LDate) = lttInvalid then
      begin
        WriteLn(
          PadRight(S, 10),
          PadRight(FormatDateTime(COutDateTimeFormat, LDate), 25),
          PadRight('', 25),
          PadRight('', 10),
          PadRight('', 10)
        );
      end else
      begin
        WriteLn(
          PadRight(S, 10),
          PadRight(FormatDateTime(COutDateTimeFormat, LDate), 25),
          PadRight(FormatDateTime(COutDateTimeFormat, LTZ.ToUniversalTime(LDate)), 25),
          PadRight(LTZ.GetAbbreviation(LDate), 10),
          PadRight(LTZ.GetDisplayName(LDate), 10),
          FormatOffset(LTZ.GetUtcOffset(LDate))
        );
      end;
    except
      on E: ETimeZoneInvalid do
        ErrorAndExit('The time zone "' + S + '" cannot be found.');
      on E: EUnknownTimeZoneYear do
        ErrorAndExit('The time zone "' + S + '" does not have data for date ' + ParamStr(3) + '.');
    end;
  end
    else PrintHeaderAndExit;
end.
