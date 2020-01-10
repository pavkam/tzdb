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

{$INCLUDE '../TZDBPK/Version.inc'}

unit TZSchema;

interface
uses
  Types,
  SysUtils,
  Classes,
  Character,
  StrUtils,
  TZStrs,
{$IFDEF DELPHI}
  Generics.Collections,
  Generics.Defaults,
  TimeSpan,
  IOUtils,
{$ELSE}
  FGL,
{$ENDIF}
  DateUtils;

type
  TzDayType = (tzdFixed, tzdLast, tzdGEThan, tzdLEThan);

  TzObject = class
    FIndexInFile: Integer;
  end;

  TzDay = class(TzObject)
  private
    FType: TzDayType;

    { For fixed days-of-month }
    FFixedDay: Word;

    { Day of week: relative rules }
    FDayOfWeek: Word;
    FDayIndex: Word;
  end;

  TzRule = class(TzObject)
  private
    FInMonth: Word;
    FOnDay: TzDay;
    FAt: Integer;
    FAtChar: Char;
    FSave: Integer;
    FLetters: string;
  end;

  TzRuleFamily = class(TzObject)
  private type
    { ... }
    TRuleWithSpan = record
      FStart, FEnd: Word;
      FRule: TzRule;
{$IFDEF FPC}
      class operator Equal(const ALeft, ARight: TRuleWithSpan): Boolean;
{$ENDIF}
    end;
  private
    FName: string;
    FRules: {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TRuleWithSpan>;

  public
    constructor Create;
    destructor Destroy; override;

    { Adds a rule to this family with a given year span }
    procedure AddRuleForYearSpan(const AStart, AEnd: Word; const ARule: TzRule);
  end;

  TzZone = class(TzObject)
  private type
    TZoneLine = record
      FGmtOff: Integer;
      FRuleFamily: TzRuleFamily;
      FFormatStr: string;

      FUntilYear, FUntilMonth: Word;
      FUntilDay: TzDay;
      FUntilTime: Integer;
      FUntilChar: Char;
{$IFDEF FPC}
      class operator Equal(const ALeft, ARight: TZoneLine): Boolean;
{$ENDIF}
    end;
  private
    FName: string;
    FLines: {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TZoneLine>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddLine(const AGmtOff: Integer; const ARuleFamily: TzRuleFamily;
      const AFormatStr: string; const AUntilYear, AUntilMonth: Word;
      const AUntilDay: TzDay; const AUntilTime: Integer; const AUntilChar: Char);
  end;

  TzCache = class
  private type
    TAliasEntry = record
      FAlias, FTimeZone: String;
{$IFDEF FPC}
      class operator Equal(const ALeft, ARight: TAliasEntry): Boolean;
{$ENDIF}
    end;
  private
    FDays: {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzDay>;
    FRules: {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzRule>;
    FRuleFamilies: {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzRuleFamily>;
    FZones: {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzZone>;
    FAliases: {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TAliasEntry>;
    FResAliases: {$IFDEF FPC}TFPGMap{$ELSE}TDictionary{$ENDIF}<string, TzZone>;

    function GetUniqueDays: Cardinal;
    function GetUniqueRules: Cardinal;
    function GetUniqueRuleFamilies: Cardinal;

  public
    { Construction and related }
    constructor Create;
    destructor Destroy; override;

    { Caching and shit }
    function GetFixedDay(const ADay: Word): TzDay;
    function GetLastDayOf(const ADayOfWeek: Word): TzDay;
    function GetNthDayOf(const ADayOfWeek, AIndex: Word): TzDay;
    function GetPredDayOf(const ADayOfWeek, AIndex: Word): TzDay;

    function GetRule(const AInMonth: Word; const AOnDay: TzDay;
      const AAt: Integer; const AAtChar: Char; const ASave: Integer; const ALetters: UnicodeString): TzRule;

    function GetRuleFamily(const AName: string): TzRuleFamily;

    function GetZone(const AName: string): TzZone;
    procedure AddAlias(const AAlias, AToZone: string);

    { Properties }
    property UniqueDays: Cardinal read GetUniqueDays;
    property UniqueRules: Cardinal read GetUniqueRules;
    property UniqueRuleFamilies: Cardinal read GetUniqueRuleFamilies;

    { Dumper! }
    procedure Finalize;
    procedure DumpToFile(const AFile, AVersion: string);
  end;

type
  EProcessParseError = class(Exception);
  EProcessRuleError = class(Exception);
  EProcessMonthError = class(Exception);
  EProcessDayError = class(Exception);
  EProcessDayRecurError = class(Exception);
  EProcessTimeError = class(Exception);
  EProcessZoneError = class(Exception);
  EProcessLinkError = class(Exception);

procedure Process(const AInputDir, AOutputFile, AVersion: string);

var
  GlobalCache: TzCache;

implementation

{$IFDEF FPC}
function EndsText(const ASubString: string; const AString: string): Boolean;
begin
  Result := RightStr(AString, Length(ASubString)) = ASubString;
end;

function StartsText(const ASubString: string; const AString: string): Boolean;
begin
  Result := Pos(ASubString, AString) = 1;
end;

function SplitString(const AString: string; const ADelimiters: AnsiString): TStringDynArray;
var
  I, P: Integer;
  LSet: TSysCharSet;
  LOutList: TFPGList<string>;
begin
  LSet := [];
  for I := 1 to Length(ADelimiters) do Include(LSet, ADelimiters[I]);
  LOutList := TFPGList<string>.Create();

  P := 1;
  for I := 1 to Length(AString) do
  begin
    if CharInSet(AString[I], LSet) then
    begin
      if P < I then LOutList.Add(Copy(AString, P, I - P)) else LOutList.Add('');
      P := I + 1;
    end;
  end;

  if P <= I then LOutList.Add(Copy(AString, P, I - P + 1));

  SetLength(Result, LOutList.Count);
  for I := 0 to LOutList.Count - 1 do Result[I] := LOutList[I];
end;

function ListFiles(const ADir: string): TStringDynArray;
var
  I: Integer;
  LInfo: TSearchRec;
  LOutList: TFPGList<string>;
begin
  LOutList := TFPGList<string>.Create();
  if FindFirst(ConcatPaths([ADir, '*']), faAnyFile, LInfo) = 0 then
  begin
    repeat
      if (LInfo.Attr and faDirectory) <> faDirectory then
        LOutList.Add(ConcatPaths([ADir, LInfo.Name]))

    until FindNext(LInfo) <> 0;
  end;

  FindClose(LInfo);

  SetLength(Result, LOutList.Count);
  for I := 0 to LOutList.Count - 1 do Result[I] := LOutList[I];
end;

function ReadFileLines(const AFile: string): TStringDynArray;
var
  I: Integer;
  LContents: TStringList;
begin
  LContents := TStringList.Create();

  Result := nil;
  try
    LContents.LoadFromFile(AFile);

    SetLength(Result, LContents.Count);
    for I := 0 to LContents.Count - 1 do Result[I] := LContents.Strings[I];
  finally
    LContents.Free();
  end;
end;

function CompareStrings(const ALeft, ARight: String): Integer;
begin
  { Compare by name of regions }
  Result := CompareStr(ALeft, ARight);
end;

class operator TzRuleFamily.TRuleWithSpan.Equal(const ALeft, ARight: TzRuleFamily.TRuleWithSpan): Boolean;
begin
  Result :=
    (ALeft.FStart = ARight.FStart) and
    (ALeft.FEnd = ARight.FEnd) and
    (ALeft.FRule = ARight.FRule);
end;

class operator TzZone.TZoneLine.Equal(const ALeft, ARight: TzZone.TZoneLine): Boolean;
begin
  Result :=
    (ALeft.FGmtOff = ARight.FGmtOff) and
    (ALeft.FRuleFamily = ARight.FRuleFamily) and
    (ALeft.FFormatStr = ARight.FFormatStr) and
    (ALeft.FUntilYear = ARight.FUntilYear) and
    (ALeft.FUntilMonth = ARight.FUntilMonth) and
    (ALeft.FUntilDay = ARight.FUntilDay) and
    (ALeft.FUntilTime = ARight.FUntilTime) and
    (ALeft.FUntilChar = ARight.FUntilChar);
end;

class operator TzCache.TAliasEntry.Equal(const ALeft, ARight: TzCache.TAliasEntry): Boolean;
begin
  Result :=
    (ALeft.FAlias = ARight.FAlias) and
    (ALeft.FTimeZone = ARight.FTimeZone);
end;

const SecsPerHour = 60 * 60;

{$ELSE}

function ListFiles(const ADir: string): TStringDynArray;
begin
  Result := TDirectory.GetFiles(ADir);
end;

function ReadFileLines(const AFile: string): TStringDynArray;
begin
  Result := TFile.ReadAllLines(AFile);
end;

{$ENDIF}

function CompareTzZones(const ALeft, ARight: TzZone): Integer;
begin
  { Compare by name of regions }
  Result := CompareText(ALeft.FName, ARight.FName);
end;

function ZapString(const AInput: string): string;
var
  I, X: Integer;
  LWasWS: Boolean;
begin
  SetLength(Result, Length(AInput));
  if Length(AInput) = 0 then
    Exit;

  LWasWS := false;
  X := 1;

  for I := 1 to Length(AInput) do
    if IsWhiteSpace(AInput, I) then
    begin
      { In case we're on a WS }
      if not LWasWS then
      begin
        { Copy the WS only if it's the first one }
        LWasWS := true;
        Result[X] := ' '; { Copy a space instead of any other WS char }
        Inc(X);
      end;
    end else
    begin
      { Kill the WS flag }
      LWasWS := false;

      { Copy the charcter over }
      Result[X] := AInput[I];
      Inc(X);
    end;

  { Now, set the real length of the output }
  SetLength(Result, X - 1);
end;

{ Converts a month abbreviation to a real month ID }
function TzStrToMonth(const Abb: string): Word;
begin
  for Result := Low(CAbbMonthNames) to High(CAbbMonthNames) do
    if SameText(CAbbMonthNames[Result], Abb) then
      Exit;

  raise EProcessMonthError.CreateFmt(CPMBadMonthIdentifier, [Abb]);
end;

{ Converts a day-of-week abbreviation to a real day ID }
function TzStrToDayOfWeek(const AStr: string): Word;
begin
  for Result := Low(CAbbDayNames) to High(CAbbDayNames) do
    if SameText(CAbbDayNames[Result], AStr) then
      Exit;

  raise EProcessDayError.CreateFmt(CPMBadDoWIdentifier, [AStr]);
end;

{ Converts a TZ time-stamp into seconds }
function TzStrToSeconds(const AStr: string; out ARelativityChar: Char): Integer;
var
  LTimeSplit: TStringDynArray;
  LHour, LMinute, LSecond: Integer;
  LNewStr: string;
  LSign: Integer;

begin
  LNewStr := AStr;

  { Remove the s or u suffixes }
  if (LNewStr <> '') and
     (EndsText('s', LNewStr) or
      EndsText('w', LNewStr) or
      EndsText('u', LNewStr) or
      EndsText('z', LNewStr) or
      EndsText('g', LNewStr)) then
  begin
    ARelativityChar := LNewStr[Length(LNewStr)];
    Delete(LNewStr, Length(LNewStr), 1);
  end else
    ARelativityChar := #0;

  LSign := 1;

  if (LNewStr <> '') and (StartsText('-', LNewStr)) then
  begin
    LSign := -1;
    Delete(LNewStr, 1, 1);
  end;

  LTimeSplit := SplitString(LNewStr, ':');

  LHour := 0;
  LMinute := 0;
  LSecond := 0;

  if Length(LTimeSplit) > 0 then
  begin
    if not TryStrToInt(LTimeSplit[0], LHour) then
      raise EProcessTimeError.CreateFmt(CPMBadHour, [LTimeSplit[0]]);
  end;
  if Length(LTimeSplit) > 1 then
  begin
    if not TryStrToInt(LTimeSplit[1], LMinute) then
      raise EProcessTimeError.CreateFmt(CPMBadMinute, [LTimeSplit[1]]);
  end;
  if Length(LTimeSplit) > 2 then
  begin
    if not TryStrToInt(LTimeSplit[2], LSecond) then
      raise EProcessTimeError.CreateFmt(CPMBadSecond, [LTimeSplit[2]]);
  end;
  if (Length(LTimeSplit) < 1) or (Length(LTimeSplit) > 3) then
    raise EProcessTimeError.CreateFmt(CPMBadTimeOfDay, [AStr]);

  { Combine the result }
  Result := LSign * (LHour * SecsPerHour + LMinute * SecsPerMin + LSecond);
end;

{ COnverts a TZ string to a day of teh week }
function TzStrToDay(const AStr: string): TzDay;
var
  LPos: Integer;
  LDoWStr, LIndexStr: string;

  LDoW, LIndex: Word;
begin
  if StartsText(CLastDoW, AStr) then
  begin
    { Reccurence on last day of specified kind }
    LDoWStr := Copy(AStr, 5, Length(AStr));

    try
      LDoW := TzStrToDayOfWeek(LDoWStr);
    except
      raise EProcessDayRecurError.CreateFmt(CPMBadDoWInLast, [LDoWStr]);
    end;

    Result := GlobalCache.GetLastDayOf(LDoW);

  end else if Pos(CNthDoW, AStr) > 0 then
  begin
    LPos := Pos(CNthDoW, AStr);

    { Reccurence on Nth day of specified kind }
    LDoWStr := Copy(AStr, 1, LPos - 1);
    LIndexStr := Copy(AStr, LPos + Length(CNthDoW), Length(AStr));

    try
      LDoW := TzStrToDayOfWeek(LDoWStr);
    except
      raise EProcessDayRecurError.CreateFmt(CPMBadDoWInNth, [LDoWStr]);
    end;

    try
      LIndex := StrToInt(LIndexStr);
    except
      raise EProcessDayRecurError.CreateFmt(CPMBadIndexInNth, [LIndexStr]);
    end;

    Result := GlobalCache.GetNthDayOf(LDoW, LIndex);
  end else if Pos(CPreDoW, AStr) > 0 then
  begin
    LPos := Pos(CPreDoW, AStr);

    { Reccurence on Nth day of specified kind }
    LDoWStr := Copy(AStr, 1, LPos - 1);
    LIndexStr := Copy(AStr, LPos + Length(CPreDoW), Length(AStr));

    try
      LDoW := TzStrToDayOfWeek(LDoWStr);
    except
      raise EProcessDayRecurError.CreateFmt(CPMBadDoWInNth, [LDoWStr]);
    end;

    try
      LIndex := StrToInt(LIndexStr);
    except
      raise EProcessDayRecurError.CreateFmt(CPMBadIndexInNth, [LIndexStr]);
    end;

    Result := GlobalCache.GetPredDayOf(LDoW, LIndex);
  end else
  begin
    try
      LIndex := StrToInt(AStr);
    except
      raise EProcessDayRecurError.CreateFmt(CPMBadIndexInFixed, [AStr]);
    end;

    Result := GlobalCache.GetFixedDay(LIndex);

  end;
end;

{ Reads up a format string }
function TzStrToFormat(const AStr: string): string;
begin
  Result := AnsiDequotedStr(AStr, '"');
end;

procedure ProcessRule(const AParts: TStringDynArray);
var
  LName: string;
  LStartYear, LEndYear, LInMonth: Word;
  LTimeOfDay, LSaveQty: Integer;
  LLetters: string;
  LDay: TzDay;
  LTimeOfDayChar, LDummy: Char;

  LFamily: TzRuleFamily;
  LRule: TzRule;
begin
  if Length(AParts) < 9 then
    raise EProcessRuleError.CreateFmt(CPMBadRuleSplitCount, [Length(AParts)]);

  { The name }
  LName := AParts[1];

  try
    { The start year (can be "min" or a real number) }
    if SameText(AParts[2], CMinDate) then
      LStartYear := 1
    else
      LStartYear := StrToInt(AParts[2]);
  except
    raise EProcessRuleError.CreateFmt(CPMBadRuleFROMField, [AParts[2], LName]);
  end;

  try
    { The end year (can be "only", "max" or a real number) }
    if SameText(AParts[3], COnlyDate) then
      LEndYear := LStartYear
    else if SameText(AParts[3], CMaxDate) then
      LEndYear := 9999
    else
      LEndYear := StrToInt(AParts[3]);
  except
    raise EProcessRuleError.CreateFmt(CPMBadRuleTOField, [AParts[2], LName]);
  end;

  { The type of rule }

  { Rule starts in month  }
  try
    LInMonth := TzStrToMonth(AParts[5]);
  except
    on E: Exception do
      raise EProcessRuleError.CreateFmt(CPMBadRuleINField, [E.Message, LName]);
  end;

  { The ON parameter (it's a rule!) }
  try
    LDay := TzStrToDay(AParts[6]);
  except
    on E: Exception do
      raise EProcessRuleError.CreateFmt(CPMBadRuleONField, [E.Message, LName]);
  end;

  { The time-of-day parameter }
  try
    LTimeOfDay := TzStrToSeconds(AParts[7], LTimeOfDayChar);
  except
    on E: Exception do
      raise EProcessRuleError.CreateFmt(CPMBadRuleATField, [E.Message, LName]);
  end;

  { The bias (aka save) parameter }
  try
    LSaveQty := TzStrToSeconds(AParts[8], LDummy);
  except
    on E: Exception do
      raise EProcessRuleError.CreateFmt(CPMBadRuleSAVEField, [E.Message, LName]);
  end;

  { Optional letters }
  if Length(AParts) = 10 then
    LLetters := AParts[9];

  { Consider - as missing letters }
  if LLetters = '-' then
    LLetters := '';

  { Get the actual rule }
  LFamily := GlobalCache.GetRuleFamily(LName);
  LRule := GlobalCache.GetRule(LInMonth, LDay, LTimeOfDay, LTimeOfDayChar, LSaveQty, LLetters);

  { Register rule for the family }
  LFamily.AddRuleForYearSpan(LStartYear, LEndYear, LRule);
end;

function ProcessZone(const AParts: TStringDynArray; const ZoneName: string): string;
var
  LFixIdx: Integer;
  LName: string;
  LGmtOffset: Integer;
  LRuleName: string;
  LFormatStr: string;

  LUntilYear, LUntilMonth: Word;
  LUntilDay: TzDay;
  LUntilTime: Integer;
  LUntilChar, LDummy: Char;

  LZone: TzZone;
  LRuleFamily: TzRuleFamily;
begin
  if ((ZoneName = '') and (Length(AParts) < 5)) or ((ZoneName <> '') and (Length(AParts) < 4)) then
    raise EProcessZoneError.CreateFmt(CPMBadZoneSplitCount, [Length(AParts)]);

  { The name }
  if ZoneName <> '' then
  begin
    LName := ZoneName;
    LFixIdx := 1;
    Result := '';
  end else
  begin
    LName := AParts[1];
    LFixIdx := 2;
    Result := LName;
  end;

  { The GMTOFF parameter }
  try
    LGmtOffset := TzStrToSeconds(AParts[LFixIdx], LDummy);
  except
    on E: Exception do
      raise EProcessZoneError.CreateFmt(CPMBadZoneGMTOFFField, [E.Message, LName]);
  end;

  { The RULES parameter }
  LRuleName := AParts[LFixIdx + 1];

  { The FORMAT parameter }
  LFormatStr := TzStrToFormat(AParts[LFixIdx + 2]);

  { Defaults for optional parameters }
  LUntilYear := 9999; { max year }
  LUntilMonth := 1;
  LUntilDay := nil;
  LUntilTime := 0;
  LUntilChar := #0;

  { End year (optional) }
  if Length(AParts) > (LFixIdx + 3) then
  begin
    try
      LUntilYear := StrToInt(AParts[LFixIdx + 3]);
    except
      raise EProcessZoneError.CreateFmt(CPMBadZoneUNTILFieldYear, [AParts[LFixIdx + 3], LName]);
    end;

    if Length(AParts) > (LFixIdx + 4) then
    begin

      { End month (optional) }
      try
        LUntilMonth := TzStrToMonth(AParts[LFixIdx + 4]);
      except
        raise EProcessZoneError.CreateFmt(CPMBadZoneUNTILFieldMonth, [AParts[LFixIdx + 4], LName]);
      end;

      if Length(AParts) > (LFixIdx + 5) then
      begin

        { End day (optional) }
        try
          LUntilDay := TzStrToDay(AParts[LFixIdx + 5]);
        except
          on E: Exception do
            raise EProcessZoneError.CreateFmt(CPMBadZoneUNTILFieldDay, [E.Message, LName]);
        end;

        if Length(AParts) > (LFixIdx + 6) then
        begin

          { End time (optional) }
          try
            LUntilTime := TzStrToSeconds(AParts[LFixIdx + 6], LUntilChar);
          except
            raise EProcessZoneError.CreateFmt(CPMBadZoneUNTILFieldTime, [AParts[LFixIdx + 6], LName]);
          end;

        end;

      end;

    end;
  end;

  { Add zone line }
  LZone := GlobalCache.GetZone(LName);

  if LRuleName <> '-' then
    LRuleFamily := GlobalCache.GetRuleFamily(LRuleName)
  else
    LRuleFamily := nil;

  LZone.AddLine(LGmtOffset, LRuleFamily, LFormatStr, LUntilYear, LUntilMonth, LUntilDay, LUntilTime, LUntilChar);
end;

procedure ProcessLink(const AParts: TStringDynArray);
var
  LRealName, LAlias: string;
begin
  if Length(AParts) < 2 then
    raise EProcessZoneError.CreateFmt(CPMBadLineSplitCount, [Length(AParts)]);

  { Real name }
  LRealName := AParts[1];

  if LRealName = '' then
    raise EProcessLinkError.Create(CPMBadLineFROM);

  { Alias name }
  LAlias := AParts[2];

  if LAlias = '' then
    raise EProcessLinkError.Create(CPMBadLineTO);

  { Get real zone }
  GlobalCache.AddAlias(LAlias, LRealName);
end;

procedure RegisterStandardAliases();
begin
  { Register the aliases for standard zone such as GMT+xx }
  GlobalCache.AddAlias('GMT+1', 'Etc/GMT+1');
  GlobalCache.AddAlias('GMT+2', 'Etc/GMT+2');
  GlobalCache.AddAlias('GMT+3', 'Etc/GMT+3');
  GlobalCache.AddAlias('GMT+4', 'Etc/GMT+4');
  GlobalCache.AddAlias('GMT+5', 'Etc/GMT+5');
  GlobalCache.AddAlias('GMT+6', 'Etc/GMT+6');
  GlobalCache.AddAlias('GMT+7', 'Etc/GMT+7');
  GlobalCache.AddAlias('GMT+8', 'Etc/GMT+8');
  GlobalCache.AddAlias('GMT+9', 'Etc/GMT+9');
  GlobalCache.AddAlias('GMT+10', 'Etc/GMT+10');
  GlobalCache.AddAlias('GMT+11', 'Etc/GMT+11');
  GlobalCache.AddAlias('GMT+12', 'Etc/GMT+12');

  { Register the aliases for standard zone such as GMT-xx }
  GlobalCache.AddAlias('GMT-1', 'Etc/GMT-1');
  GlobalCache.AddAlias('GMT-2', 'Etc/GMT-2');
  GlobalCache.AddAlias('GMT-3', 'Etc/GMT-3');
  GlobalCache.AddAlias('GMT-4', 'Etc/GMT-4');
  GlobalCache.AddAlias('GMT-5', 'Etc/GMT-5');
  GlobalCache.AddAlias('GMT-6', 'Etc/GMT-6');
  GlobalCache.AddAlias('GMT-7', 'Etc/GMT-7');
  GlobalCache.AddAlias('GMT-8', 'Etc/GMT-8');
  GlobalCache.AddAlias('GMT-9', 'Etc/GMT-9');
  GlobalCache.AddAlias('GMT-10', 'Etc/GMT-10');
  GlobalCache.AddAlias('GMT-11', 'Etc/GMT-11');
  GlobalCache.AddAlias('GMT-12', 'Etc/GMT-12');
  GlobalCache.AddAlias('GMT-13', 'Etc/GMT-13');
  GlobalCache.AddAlias('GMT-14', 'Etc/GMT-14');

  { Windows mapping }
  {$I WindowsTZ.inc}
end;

function CharToRel(const Ch: Char): string;
begin
  if CharInSet(Ch, ['s', 'S']) then
    Result := 'trStandard'
  else if CharInSet(Ch, ['u', 'g', 'z', 'U', 'G', 'Z']) then
    Result := 'trUniversal'
  else
    Result := 'trLocal';
end;

procedure Process(const AInputDir, AOutputFile, AVersion: string);
var
  LFile, LLine: string;
  LLineIdx: Integer;
  LCommentIdx: Integer;
  LLineSplit: TStringDynArray;
  LZoneName: string;

  LRules, LZones: Integer;
  LAllLines: TStringDynArray;
begin
  LRules := 0;
  LZones := 0;

  { now the work can begin! }
  for LFile in ListFiles(AInputDir) do
  begin
    CLIMessage(Format(CPMStartedFile, [LFile]));

    { read line-by-line }
    LZoneName := '';
    LAllLines := ReadFileLines(LFile);

    for LLineIdx := 0 to Length(LAllLines) - 1 do
    begin
      LLine := LAllLines[LLineIdx];
      LCommentIdx := Pos(CCommentChar, LLine);

      { Remove comments from the code }
      if LCommentIdx > 0 then
        Delete(LLine, LCommentIdx, Length(LLine));

      { Trim it too }
      LLine := TrimRight(ZapString(LLine));

      { Skip empty lines }
      if Length(LLine) = 0 then
        continue;

      { Split the line (using \t and space }
      LLineSplit := SplitString(LLine, #9#32);

      { Continue on bad line }
      if Length(LLineSplit) = 0 then
        continue;

      if SameText(LLineSplit[0], CLinkId) then
      begin
        { Kill zone name }
        LZoneName := '';

        { Process rule! }
        ProcessLink(LLineSplit);
      end else
      if SameText(LLineSplit[0], CRuleId) then
      begin
        { Kill zone name }
        LZoneName := '';

        Inc(LRules);

        { Process rule! }
        ProcessRule(LLineSplit);
      end else
      if SameText(LLineSplit[0], CZoneId) then
      begin
        Inc(LZones);

        { Process zone! }
        LZoneName := ProcessZone(LLineSplit, '');
      end else
      if (LZoneName <> '') and (LLineSplit[0] = '') then
      begin
        { Zone continuation ... }
        ProcessZone(LLineSplit, LZoneName);
      end else
        raise EProcessParseError.CreateFmt(CPMBadFile, [LLine, LFile]);
    end;
  end;

  RegisterStandardAliases();
  GlobalCache.Finalize;

  CLIMessage(Format(CPMStats, [LRules, LZones, GlobalCache.UniqueDays,
    GlobalCache.UniqueRules, GlobalCache.UniqueRuleFamilies, GlobalCache.FResAliases.Count]));

  CLIMessage(Format(CPMStartDump, [AOutputFile]));

  GlobalCache.DumpToFile(AOutputFile, AVersion);

  CLIMessage('Processing finished!');
end;

{ TzCache }

procedure TzCache.AddAlias(const AAlias, AToZone: string);
var
  LAlias: TAliasEntry;
begin
  LAlias.FAlias := AAlias;
  LAlias.FTimeZone := AToZone;

  FAliases.Add(LAlias);
end;

constructor TzCache.Create;
begin
  FDays := {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzDay>.Create(true);
  FRules := {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzRule>.Create(true);
  FRuleFamilies := {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzRuleFamily>.Create(true);
  FZones := {$IFDEF FPC}TFPGObjectList{$ELSE}TObjectList{$ENDIF}<TzZone>.Create();
  FAliases := {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TAliasEntry>.Create();
end;

destructor TzCache.Destroy;
begin
  FDays.Free;
  FRules.Free;
  FRuleFamilies.Free;
  FZones.Free;
  FAliases.Free;
  if FResAliases <> nil then FResAliases.Free;

  inherited;
end;

procedure TzCache.Finalize;
var
  LAliasEntry: TAliasEntry;
  LResult: {$IFDEF FPC}TFPGMap{$ELSE}TDictionary{$ENDIF}<string, TzZone>;

  procedure AddTzAlias(const AAlias: string; const AZone: TzZone);
  var
    LZone: TzZone;
  begin
    if LResult.{$IFDEF FPC}TryGetData{$ELSE}TryGetValue{$ENDIF}(AAlias, LZone) then
      CLIError(Format(CPMAliasExists, [AAlias, LZone.FName, AZone.FName]))
    else
    begin
      CLIMessage(Format(CPMAddedAlias, [AAlias, AZone.FName]));
      LResult.Add(AAlias, AZone);
    end;
  end;

  procedure AddAlias(const AAlias, AToZone: string);
  var
    LZone: TzZone;
  begin
    for LZone in FZones do
    begin
      if SameText(LZone.FName, AToZone) then
      begin
        AddTzAlias(AAlias, LZone);
        Exit;
      end;
    end;

    if LResult.{$IFDEF FPC}TryGetData{$ELSE}TryGetValue{$ENDIF}(AToZone, LZone) then
    begin
      AddTzAlias(AAlias, LZone);
      Exit;
    end;

    CLIError(Format(CPMAliasFailed, [AAlias, AToZone]));
  end;

begin
  LResult := {$IFDEF FPC}TFPGMap{$ELSE}TDictionary{$ENDIF}<string, TzZone>.Create;

  for LAliasEntry in FAliases do
    AddAlias(LAliasEntry.FAlias, LAliasEntry.FTimeZone);

  FResAliases := LResult;
end;

procedure TzCache.DumpToFile(const AFile, AVersion: string);
var
  LFile: TextFile;
  LDay: TzDay;
  LRule: TzRule;
  LFam: TzRuleFamily;
  LZone: TzZone;
  I, X, LGhosts: Integer;
  LDayIdx, LRuleIdx: string;
  LAliasList: {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<string>;
begin
  { SOOOOOORT the collections that require lookup later }
{$IFDEF FPC}
  FZones.Sort(CompareTzZones);
{$ELSE}
  FZones.Sort(TComparer<TzZone>.Construct(CompareTzZones));
{$ENDIF}

  AssignFile(LFile, AFile);
  Rewrite(LFile);
  try
    { ========== HEADER =========== }
    WriteLn(LFile, '{ This file is auto-generated. Do not change its contents since it is highly dependant on the consumer unit. }');
    WriteLn(LFile, 'const');
    WriteLn(LFile, '  CIANAVersion = ''' + AVersion + ''';');
    WriteLn(LFile);
    { ======= Days ======== }
    WriteLn(LFile, 'var');
    WriteLn(LFile, '  { This array contains the definitions of relative days used later on in the rules. }' );
    WriteLn(LFile, '  CRelativeDays: array[0 .. ' + IntToStr(FDays.Count - 1) + '] of TRelativeDay = (');

    for I := 0 to FDays.Count - 1 do
    begin
      LDay := FDays[I];
      LDay.FIndexInFile := I;
      Write(LFile, '    (FDayType: ');

      case LDay.FType of
        tzdFixed:
          Write(LFile, 'dtFixed; FFixedDay: ' + IntToStr(LDay.FFixedDay));

        tzdLast:
          Write(LFile, 'dtLastOfMonth; FLastDayOfWeek: ' + IntToStr(LDay.FDayOfWeek));

        tzdGEThan:
          Write(LFile, 'dtNthOfMonth; FNthDayOfWeek: ' +
              IntToStr(LDay.FDayOfWeek) + '; FNthDayIndex: ' +
              IntToStr(LDay.FDayIndex));

        tzdLEThan:
          Write(LFile, 'dtPredOfMonth; FPredDayOfWeek: ' +
              IntToStr(LDay.FDayOfWeek) + '; FPredDayIndex: ' +
              IntToStr(LDay.FDayIndex));
      end;

      if I = (FDays.Count - 1) then
        WriteLn(LFile, ')')
      else
        WriteLn(LFile, '),');
    end;

    WriteLn(LFile, '  );');
    WriteLn(LFile);

    { ======= RULES ======== }
    WriteLn(LFile, 'var');
    WriteLn(LFile, '  { This array contains the definitions of DST rules. Used by rule families. }');
    WriteLn(LFile, '  CRules: array[0 .. ' + IntToStr(FRules.Count - 1) + '] of TRule = (');

    for I := 0 to FRules.Count - 1 do
    begin
      LRule := FRules[I];
      LRule.FIndexInFile := I;
      WriteLn(LFile, '   {CRules['+inttostr(i)+']}');
      Write(LFile,
        Format('    (FInMonth: %d; FOnDay: @CRelativeDays[%d]; FAt: %d; FAtMode: %s; FOffset: %d; FFmtPart: ''%s'')',
          [LRule.FInMonth, LRule.FOnDay.FIndexInFile, LRule.FAt, CharToRel(LRule.FAtChar), LRule.FSave,
            LRule.FLetters]));

      if I < (FRules.Count - 1) then
        Write(LFile, ',');

      WriteLn(LFile);
    end;

    WriteLn(LFile, '  );');
    WriteLn(LFile);

    { ======= RULE FAMILY PARTS ======== }
    WriteLn(LFile, 'var');

    LGhosts := 0;
    for I := 0 to FRuleFamilies.Count - 1 do
    begin
      LFam := FRuleFamilies[I];

      if LFam.FRules.Count > 0 then
        LFam.FIndexInFile := (I - LGhosts)
      else begin
        Inc(LGhosts);
        { Some inconsitencies in the input files make us mark and skip "phantom" rules }
        LFam.FIndexInFile := -1;
        continue;
      end;

      WriteLn(LFile, '  { Date-bound rules for ' + LFam.FName + ' family }');
      WriteLn(LFile, '  CFamily_' + IntToStr(LFam.FIndexInFile) + '_Arr: array[0 .. ' +
          IntToStr(LFam.FRules.Count - 1) + '] of TYearBoundRule = (');

      { Sort the rule array in desc order to allow simpler lookup }

      for X := 0 to LFam.FRules.Count - 1 do
      begin
        Write(LFile, Format('    (FStart: %d; FEnd: %d; FRule: @CRules[%d])', [LFam.FRules[X].FStart,
          LFam.FRules[X].FEnd, LFam.FRules[X].FRule.FIndexInFile]));

        if X < (LFam.FRules.Count - 1) then
          Write(LFile, ',');

        WriteLn(LFile);
      end;

      WriteLn(LFile, '  );');
      WriteLn(LFile);
    end;


    { ======= RULE FAMILIES ======== }
    WriteLn(LFile, 'var');
    WriteLn(LFile, '  { This array contains rule families. }');

    X := FRuleFamilies.Count - LGhosts;
    WriteLn(LFile, '  CRuleFamilies: array[0 .. ' + IntToStr(X - 1) + '] of TRuleFamily = (');

    for I := 0 to FRuleFamilies.Count - 1 do
    begin
      LFam := FRuleFamilies[I];

      { Skip ghosts }
      if LFam.FIndexInFile = -1 then
        continue;

      Write(LFile, Format('    (FCount: %d; FFirstRule: @CFamily_%d_Arr)', [LFam.FRules.Count, LFam.FIndexInFile]));

      Dec(X);
      if X > 0 then
        Write(LFile, ',');

      WriteLn(LFile);
    end;

    WriteLn(LFile, '  );');
    WriteLn(LFile);

    { ======= ZONE PARTS ======== }
    WriteLn(LFile, 'var');

    LGhosts := 0;
    for I := 0 to FZones.Count - 1 do
    begin
      LZone := FZones[I];

      if LZone.FLines.Count > 0 then
        LZone.FIndexInFile := (I - LGhosts)
      else begin
        Inc(LGhosts);

        { Some inconsitencies in the input files make us mark and skip "phantom" zones }
        LZone.FIndexInFile := -1;
        continue;
      end;

      LZone.FIndexInFile := I;

      WriteLn(LFile, '  { Time periods for ' + LZone.FName + ' zone }');
      WriteLn(LFile, '  CZone_' + IntToStr(I) + '_Arr: array[0 .. ' +
          IntToStr(LZone.FLines.Count - 1) + '] of TPeriod = (');

      for X := 0 to LZone.FLines.Count - 1 do
      begin
        LDay := LZone.FLines[X].FUntilDay;
        if LDay = nil then
          LDayIdx := 'nil'
        else
          LDayIdx := '@CRelativeDays[' + IntToStr(LDay.FIndexInFile) + ']';

        LFam := LZone.FLines[X].FRuleFamily;
        if (LFam = nil) or (LFam.FIndexInFile = -1) then
          LRuleIdx := 'nil'
        else
          LRuleIdx := '@CRuleFamilies[' + IntToStr(LFam.FIndexInFile) + ']';

        Write(LFile, Format('    (FOffset: %d; FRuleFamily: %s; FFmtStr: ''%s''; FUntilYear: %d; ' +
          'FUntilMonth: %d; FUntilDay: %s; FUntilTime: %d; FUntilTimeMode: %s)',
          [LZone.FLines[X].FGmtOff, LRuleIdx, LZone.FLines[X].FFormatStr, LZone.FLines[X].FUntilYear,
           LZone.FLines[X].FUntilMonth, LDayIdx, LZone.FLines[X].FUntilTime, CharToRel(LZone.FLines[X].FUntilChar)]));

        if X < (LZone.FLines.Count - 1) then
          Write(LFile, ',');

        WriteLn(LFile);
      end;

      WriteLn(LFile, '  );');
      WriteLn(LFile);
    end;

    { ======= ZONES ======== }
    WriteLn(LFile, 'var');
    WriteLn(LFile, '  { This array contains zones. }');
    WriteLn(LFile, '  CZones: array[0 .. ' + IntToStr(FZones.Count - LGhosts - 1) + '] of TZone = (');

    for I := 0 to FZones.Count - 1 do
    begin
      LZone := FZones[I];

      if LZone.FIndexInFile = -1 then
        continue;

      Write(LFile, Format('    (FName: ''%s''; FCount: %d; FFirstPeriod: @CZone_%d_Arr)',
        [LZone.FName, LZone.FLines.Count, LZone.FIndexInFile]));

      if I < (FZones.Count - 1) then
        Write(LFile, ',');

      WriteLn(LFile);
    end;

    WriteLn(LFile, '  );');
    WriteLn(LFile);

    { ======= ALIASES ======== }
{$IFDEF FPC}
    LAliasList := TFPGList<string>.Create();
    for I := 0 to FResAliases.Count - 1 do LAliasList.Add(FResAliases.Keys[I]);
    LAliasList.Sort(CompareStrings);
{$ELSE}
    LAliasList := TList<string>.Create(FResAliases.Keys);
    LAliasList.Sort();
{$ENDIF}

    { Calculate the real number of aliases, based on ghost zones }
    LGhosts := 0;
    for I := 0 to LAliasList.Count - 1 do
    begin
      LZone := FResAliases[LAliasList[I]];
      if LZone.FIndexInFile = -1 then
        Inc(LGhosts);
    end;

    WriteLn(LFile, 'var');
    WriteLn(LFile, '  { This array contains zone aliases. }');
    WriteLn(LFile, '  CAliases: array[0 .. ' + IntToStr(LAliasList.Count - LGhosts - 1) + '] of TZoneAlias = (');

    for I := 0 to LAliasList.Count - 1 do
    begin
      LZone := FResAliases[LAliasList[I]];

      if LZone.FIndexInFile = -1 then
        continue;

      Write(LFile, Format('    (FName: ''%s''; FAliasTo: @CZones[%d])',
        [LAliasList[I], LZone.FIndexInFile]));

      if I < (LAliasList.Count - 1) then
        Write(LFile, ',');

      WriteLn(LFile);
    end;

    WriteLn(LFile, '  );');
    WriteLn(LFile);

    LAliasList.Free;
  finally
    CloseFile(LFile);
  end;
end;

function TzCache.GetFixedDay(const ADay: Word): TzDay;
var
  LDay: TzDay;
begin
  for LDay in FDays do
    if (LDay.FType = tzdFixed) and (LDay.FFixedDay = ADay) then
      Exit(LDay);

  Result := TzDay.Create;
  Result.FType := tzdFixed;
  Result.FFixedDay := ADay;

  FDays.Add(Result);
end;

function TzCache.GetLastDayOf(const ADayOfWeek: Word): TzDay;
var
  LDay: TzDay;
begin
  for LDay in FDays do
    if (LDay.FType = tzdLast) and (LDay.FDayOfWeek = ADayOfWeek) then
      Exit(LDay);

  Result := TzDay.Create;
  Result.FType := tzdLast;
  Result.FDayOfWeek := ADayOfWeek;

  FDays.Add(Result);
end;

function TzCache.GetNthDayOf(const ADayOfWeek, AIndex: Word): TzDay;
var
  LDay: TzDay;
begin
  for LDay in FDays do
    if (LDay.FType = tzdGEThan) and (LDay.FDayOfWeek = ADayOfWeek) and
      (LDay.FDayIndex = AIndex) then
      Exit(LDay);

  Result := TzDay.Create;
  Result.FType := tzdGEThan;
  Result.FDayOfWeek := ADayOfWeek;
  Result.FDayIndex := AIndex;

  FDays.Add(Result);
end;

function TzCache.GetPredDayOf(const ADayOfWeek, AIndex: Word): TzDay;
var
  LDay: TzDay;
begin
  for LDay in FDays do
    if (LDay.FType = tzdLEThan) and (LDay.FDayOfWeek = ADayOfWeek) and
      (LDay.FDayIndex = AIndex) then
      Exit(LDay);

  Result := TzDay.Create;
  Result.FType := tzdLEThan;
  Result.FDayOfWeek := ADayOfWeek;
  Result.FDayIndex := AIndex;

  FDays.Add(Result);
end;

function TzCache.GetRule(const AInMonth: Word; const AOnDay: TzDay;
  const AAt: Integer; const AAtChar: Char; const ASave: Integer; const ALetters: string): TzRule;
var
  LRule: TzRule;
begin
  for LRule in FRules do
    if (LRule.FInMonth = AInMonth) and (LRule.FOnDay = AOnDay) and
      (LRule.FAt = AAt) and (LRule.FSave = ASave) and
      (LRule.FLetters = ALetters) then
      Exit(LRule);

  Result := TzRule.Create;
  Result.FInMonth := AInMonth;
  Result.FOnDay := AOnDay;
  Result.FAt := AAt;
  Result.FAtChar := AAtChar;
  Result.FSave := ASave;
  Result.FLetters := ALetters;

  CLIMessage(Format(CPMAddedRule, [AInMonth, Ord(AOnDay.FType), AOnDay.FFixedDay, AOnDay.FDayOfWeek, AOnDay.FDayIndex, AAt, AAtChar, ASave, ALetters]));

  FRules.Add(Result);
end;

function TzCache.GetRuleFamily(const AName: string): TzRuleFamily;
var
  LRuleFamily: TzRuleFamily;
begin
  for LRuleFamily in FRuleFamilies do
    if SameText(LRuleFamily.FName, AName) then
      Exit(LRuleFamily);

  Result := TzRuleFamily.Create;
  Result.FName := AName;

  CLIMessage(Format(CPMAddedRuleFamily, [AName]));

  FRuleFamilies.Add(Result);
end;

function TzCache.GetUniqueDays: Cardinal;
begin
  Result := FDays.Count;
end;

function TzCache.GetUniqueRuleFamilies: Cardinal;
begin
  Result := FRuleFamilies.Count;
end;

function TzCache.GetUniqueRules: Cardinal;
begin
  Result := FRules.Count;
end;

function TzCache.GetZone(const AName: string): TzZone;
var
  LZone: TzZone;
begin
  for LZone in FZones do
    if SameText(LZone.FName, AName) then
      Exit(LZone);

  Result := TzZone.Create;
  Result.FName := AName;

  CLIMessage(Format(CPMAddedZone, [AName]));

  FZones.Add(Result);
end;

{ TzRuleFamily }

procedure TzRuleFamily.AddRuleForYearSpan(const AStart, AEnd: Word;
  const ARule: TzRule);
var
  LSpan: TRuleWithSpan;
begin
  LSpan.FStart := AStart;
  LSpan.FEnd := AEnd;
  LSpan.FRule := ARule;

  FRules.Add(LSpan);
end;

constructor TzRuleFamily.Create;
begin
  FRules := {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TRuleWithSpan>.Create();
end;

destructor TzRuleFamily.Destroy;
begin
  FRules.Free;

  inherited;
end;

{ TzZone }

procedure TzZone.AddLine(const AGmtOff: Integer;
  const ARuleFamily: TzRuleFamily; const AFormatStr: string;
  const AUntilYear, AUntilMonth: Word; const AUntilDay: TzDay;
  const AUntilTime: Integer; const AUntilChar: Char);
var
  LLine: TZoneLine;
begin
  LLine.FGmtOff := AGmtOff;
  LLine.FRuleFamily := ARuleFamily;
  LLine.FFormatStr := AFormatStr;
  LLine.FUntilYear := AUntilYear;
  LLine.FUntilMonth := AUntilMonth;
  LLine.FUntilDay := AUntilDay;
  LLine.FUntilTime := AUntilTime;
  LLine.FUntilChar := AUntilChar;

  FLines.Add(LLine);
end;

constructor TzZone.Create;
begin
  FLines := {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TZoneLine>.Create;
end;

destructor TzZone.Destroy;
begin
  FLines.Free;
  inherited;
end;

initialization
  GlobalCache := TzCache.Create;

finalization
  GlobalCache.Free;

end.
