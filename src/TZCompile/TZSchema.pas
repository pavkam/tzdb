(*
* Copyright (c) 2010, Ciobanu Alexandru
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

unit TZSchema;

interface
uses
  Types,
  SysUtils,
  TimeSpan,
  Classes,
  Character,
  IOUtils,
  StrUtils,
  TZStrs,
  Generics.Collections,
  Generics.Defaults,
  DateUtils;

type
  TzDayType = (tzdFixed, tzdLast, tzdGEThan);

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
    end;
  private
    FName: string;
    FRules: TList<TRuleWithSpan>;

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
    end;
  private
    FName: string;
    FLines: TList<TZoneLine>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddLine(const AGmtOff: Integer; const ARuleFamily: TzRuleFamily;
      const AFormatStr: string; const AUntilYear, AUntilMonth: Word;
      const AUntilDay: TzDay; const AUntilTime: Integer; const AUntilChar: Char);
  end;

  TzCache = class
  private
    FDays: TObjectList<TzDay>;
    FRules: TObjectList<TzRule>;
    FRuleFamilies: TObjectList<TzRuleFamily>;
    FZones: TObjectList<TzZone>;
    FLinks: TDictionary<string, TzZone>;

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

    function GetRule(const AInMonth: Word; const AOnDay: TzDay;
      const AAt: Integer; const AAtChar: Char; const ASave: Integer; const ALetters: string): TzRule;

    function GetRuleFamily(const AName: string): TzRuleFamily;

    function GetZone(const AName: string): TzZone;

    procedure AddAlias(const AAlias: string; const AZone: TzZone); overload;
    function AddAlias(const AAlias, AToZone: string): Boolean; overload;

    { Properties }
    property UniqueDays: Cardinal read GetUniqueDays;
    property UniqueRules: Cardinal read GetUniqueRules;
    property UniqueRuleFamilies: Cardinal read GetUniqueRuleFamilies;

    { Dumper! }
    procedure DumpToFile(const AFile: string);
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

procedure Process(const AInputDir, AOutputFile: string);

var
  GlobalCache: TzCache;

implementation

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

  end else if Pos('>=', AStr) > 0 then
  begin
    LPos := Pos('>=', AStr);

    { Reccurence on Nth day of specified kind }
    LDoWStr := Copy(AStr, 1, LPos - 1);
    LIndexStr := Copy(AStr, LPos + 2, Length(AStr));

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

  { Defaultz for optinal parameters }
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

  LZone: TzZone;
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
  LZone := GlobalCache.GetZone(LRealName);
  GlobalCache.AddAlias(LAlias, LZone);
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

  { WINDOWS translation table (from CLDR project) .. last updated by Pierre Y. on (Mon, 7 Apr 2014) }
  GlobalCache.AddAlias('AUS Central Standard Time', 'Australia/Darwin');
  GlobalCache.AddAlias('AUS Eastern Standard Time', 'Australia/Sydney');
  GlobalCache.AddAlias('Afghanistan Standard Time', 'Asia/Kabul');
  GlobalCache.AddAlias('Alaskan Standard Time', 'America/Anchorage');
  GlobalCache.AddAlias('Arab Standard Time', 'Asia/Riyadh');
  GlobalCache.AddAlias('Arabian Standard Time', 'Asia/Dubai');
  GlobalCache.AddAlias('Arabic Standard Time', 'Asia/Baghdad');
  GlobalCache.AddAlias('Argentina Standard Time', 'America/Buenos_Aires');
  GlobalCache.AddAlias('Atlantic Standard Time', 'America/Halifax');
  GlobalCache.AddAlias('Azerbaijan Standard Time', 'Asia/Baku');
  GlobalCache.AddAlias('Azores Standard Time', 'Atlantic/Azores');
  GlobalCache.AddAlias('Bahia Standard Time', 'America/Bahia');
  GlobalCache.AddAlias('Bangladesh Standard Time', 'Asia/Dhaka');
  GlobalCache.AddAlias('Canada Central Standard Time', 'America/Regina');
  GlobalCache.AddAlias('Cape Verde Standard Time', 'Atlantic/Cape_Verde');
  GlobalCache.AddAlias('Caucasus Standard Time', 'Asia/Yerevan');
  GlobalCache.AddAlias('Cen. Australia Standard Time', 'Australia/Adelaide');
  GlobalCache.AddAlias('Central America Standard Time', 'America/Guatemala');
  GlobalCache.AddAlias('Central Asia Standard Time', 'Asia/Almaty');
  GlobalCache.AddAlias('Central Brazilian Standard Time', 'America/Cuiaba');
  GlobalCache.AddAlias('Central Europe Standard Time', 'Europe/Budapest');
  GlobalCache.AddAlias('Central European Standard Time', 'Europe/Warsaw');
  GlobalCache.AddAlias('Central Pacific Standard Time', 'Pacific/Guadalcanal');
  GlobalCache.AddAlias('Central Standard Time (Mexico)', 'America/Mexico_City');
  GlobalCache.AddAlias('Central Standard Time', 'America/Chicago');
  GlobalCache.AddAlias('China Standard Time', 'Asia/Shanghai');
  GlobalCache.AddAlias('Dateline Standard Time', 'Etc/GMT+12');
  GlobalCache.AddAlias('E. Africa Standard Time', 'Africa/Nairobi');
  GlobalCache.AddAlias('E. Australia Standard Time', 'Australia/Brisbane');
  GlobalCache.AddAlias('E. South America Standard Time', 'America/Sao_Paulo');
  GlobalCache.AddAlias('Eastern Standard Time', 'America/New_York');
  GlobalCache.AddAlias('Egypt Standard Time', 'Africa/Cairo');
  GlobalCache.AddAlias('Ekaterinburg Standard Time', 'Asia/Yekaterinburg');
  GlobalCache.AddAlias('Fiji Standard Time', 'Pacific/Fiji');
  GlobalCache.AddAlias('FLE Standard Time', 'Europe/Kiev');
  GlobalCache.AddAlias('Georgian Standard Time', 'Asia/Tbilisi');
  GlobalCache.AddAlias('GMT Standard Time', 'Europe/London');
  GlobalCache.AddAlias('Greenland Standard Time', 'America/Godthab');
  GlobalCache.AddAlias('Greenwich Standard Time', 'Atlantic/Reykjavik');
  GlobalCache.AddAlias('GTB Standard Time', 'Europe/Bucharest');
  GlobalCache.AddAlias('Hawaiian Standard Time', 'Pacific/Honolulu');
  GlobalCache.AddAlias('India Standard Time', 'Asia/Calcutta');
  GlobalCache.AddAlias('Iran Standard Time', 'Asia/Tehran');
  GlobalCache.AddAlias('Israel Standard Time', 'Asia/Jerusalem');
  GlobalCache.AddAlias('Jordan Standard Time', 'Asia/Amman');
  GlobalCache.AddAlias('Kaliningrad Standard Time', 'Europe/Kaliningrad');
  GlobalCache.AddAlias('Korea Standard Time', 'Asia/Seoul');
  GlobalCache.AddAlias('Libya Standard Time', 'Africa/Tripoli');
  GlobalCache.AddAlias('Magadan Standard Time', 'Asia/Magadan');
  GlobalCache.AddAlias('Mauritius Standard Time', 'Indian/Mauritius');
  GlobalCache.AddAlias('Middle East Standard Time', 'Asia/Beirut');
  GlobalCache.AddAlias('Montevideo Standard Time', 'America/Montevideo');
  GlobalCache.AddAlias('Morocco Standard Time', 'Africa/Casablanca');
  GlobalCache.AddAlias('Mountain Standard Time (Mexico)', 'America/Chihuahua');
  GlobalCache.AddAlias('Mountain Standard Time', 'America/Denver');
  GlobalCache.AddAlias('Myanmar Standard Time', 'Asia/Rangoon');
  GlobalCache.AddAlias('N. Central Asia Standard Time', 'Asia/Novosibirsk');
  GlobalCache.AddAlias('Namibia Standard Time', 'Africa/Windhoek');
  GlobalCache.AddAlias('Nepal Standard Time', 'Asia/Katmandu');
  GlobalCache.AddAlias('New Zealand Standard Time', 'Pacific/Auckland');
  GlobalCache.AddAlias('Newfoundland Standard Time', 'America/St_Johns');
  GlobalCache.AddAlias('North Asia East Standard Time', 'Asia/Irkutsk');
  GlobalCache.AddAlias('North Asia Standard Time', 'Asia/Krasnoyarsk');
  GlobalCache.AddAlias('Pacific SA Standard Time', 'America/Santiago');
  GlobalCache.AddAlias('Pacific Standard Time (Mexico)', 'America/Santa_Isabel');
  GlobalCache.AddAlias('Pacific Standard Time', 'America/Los_Angeles');
  GlobalCache.AddAlias('Pakistan Standard Time', 'Asia/Karachi');
  GlobalCache.AddAlias('Paraguay Standard Time', 'America/Asuncion');
  GlobalCache.AddAlias('Romance Standard Time', 'Europe/Paris');
  GlobalCache.AddAlias('Russian Standard Time', 'Europe/Moscow');
  GlobalCache.AddAlias('SA Eastern Standard Time', 'America/Cayenne');
  GlobalCache.AddAlias('SA Pacific Standard Time', 'America/Bogota');
  GlobalCache.AddAlias('SA Western Standard Time', 'America/La_Paz');
  GlobalCache.AddAlias('Samoa Standard Time', 'Pacific/Apia');
  GlobalCache.AddAlias('SE Asia Standard Time', 'Asia/Bangkok');
  GlobalCache.AddAlias('Singapore Standard Time', 'Asia/Singapore');
  GlobalCache.AddAlias('South Africa Standard Time', 'Africa/Johannesburg');
  GlobalCache.AddAlias('Sri Lanka Standard Time', 'Asia/Colombo');
  GlobalCache.AddAlias('Syria Standard Time', 'Asia/Damascus');
  GlobalCache.AddAlias('Taipei Standard Time', 'Asia/Taipei');
  GlobalCache.AddAlias('Tasmania Standard Time', 'Australia/Hobart');
  GlobalCache.AddAlias('Tokyo Standard Time', 'Asia/Tokyo');
  GlobalCache.AddAlias('Tonga Standard Time', 'Pacific/Tongatapu');
  GlobalCache.AddAlias('Turkey Standard Time', 'Europe/Istanbul');
  GlobalCache.AddAlias('Ulaanbaatar Standard Time', 'Asia/Ulaanbaatar');
  GlobalCache.AddAlias('US Eastern Standard Time', 'America/Indianapolis');
  GlobalCache.AddAlias('US Mountain Standard Time', 'America/Phoenix');
  GlobalCache.AddAlias('UTC+12', 'Etc/GMT-12');
  GlobalCache.AddAlias('UTC-02', 'Etc/GMT+2');
  GlobalCache.AddAlias('UTC-11', 'Etc/GMT+11');
  GlobalCache.AddAlias('Venezuela Standard Time', 'America/Caracas');
  GlobalCache.AddAlias('Vladivostok Standard Time', 'Asia/Vladivostok');
  GlobalCache.AddAlias('W. Australia Standard Time', 'Australia/Perth');
  GlobalCache.AddAlias('W. Central Africa Standard Time', 'Africa/Lagos');
  GlobalCache.AddAlias('W. Europe Standard Time', 'Europe/Berlin');
  GlobalCache.AddAlias('West Asia Standard Time', 'Asia/Tashkent');
  GlobalCache.AddAlias('West Pacific Standard Time', 'Pacific/Port_Moresby');
  GlobalCache.AddAlias('Yakutsk Standard Time', 'Asia/Yakutsk');

  { Missing translations in CLDR table }
//  GlobalCache.AddAlias('E. Europe Standard Time', 'Europe/Minsk'); { unmappable }
  GlobalCache.AddAlias('Kamchatka Standard Time', 'Asia/Kamchatka');
  GlobalCache.AddAlias('Mid-Atlantic Standard Time', 'Etc/GMT+2');
end;

function CharToRel(const Ch: Char): string;
begin
  if CharInSet(Ch, ['w', 'W', #0]) then
    Result := 'trLocal'
  else if CharInSet(Ch, ['s', 's']) then
    Result := 'trStandard'
  else if CharInSet(Ch, ['u', 'g', 'z', 'U', 'G', 'Z']) then
    Result := 'trUniversal'
  else begin
    Result := 'trLocal';
  end;
end;

procedure Process(const AInputDir, AOutputFile: string);
var
  LFile, LLine: string;
  LLineIdx: Integer;
  LCommentIdx: Integer;
  LLineSplit: TStringDynArray;
  LZoneName: string;

  LRules, LZones, LLinks: Integer;
  LAllLines: TStringDynArray;
begin
  LRules := 0;
  LZones := 0;
  LLinks := 0;

  { now the work can begin! }
  for LFile in TDirectory.GetFiles(AInputDir) do
  begin
    { read line-by-line }
    LZoneName := '';
    LAllLines := TFile.ReadAllLines(LFile);

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

        Inc(LLinks);

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

  Writeln(Format('Processed %d rules and %d zones', [LRules, LZones]));
  Writeln(Format('  => %d day parts', [GlobalCache.UniqueDays]));
  Writeln(Format('  => %d rules', [GlobalCache.UniqueRules]));
  Writeln(Format('  => %d rule families', [GlobalCache.UniqueRuleFamilies]));
  Writeln(Format('  => %d aliases', [LLinks]));

  Write('Dumping to "', AOutputFile, '" ...');

  GlobalCache.DumpToFile(AOutputFile);
  Writeln('[DONE]');
end;


{ TzCache }

procedure TzCache.AddAlias(const AAlias: string; const AZone: TzZone);
var
  LZone: TzZone;
begin
  if FLinks.TryGetValue(AAlias, LZone) then
    raise EInvalidOp.CreateFmt
      ('Alias "%s" (to "%s") already registered with zone "%s".',
      [AAlias, AZone.FName, LZone.FName]);

  FLinks.Add(AAlias, AZone);
end;

function TzCache.AddAlias(const AAlias, AToZone: string): boolean;
var
  LZone: TzZone;
begin
  for LZone in FZones do
    if SameText(LZone.FName, AToZone) then
    begin
      { Only add alias if the "to" zone actually exists }
      AddAlias(AAlias, LZone);
      Exit(true);
    end;

  if FLinks.TryGetValue(AToZone, LZone) then
  begin
    AddAlias(AAlias, LZone);
    Exit(True);
  end;

  { failed! }
  Result := false;
end;

constructor TzCache.Create;
begin
  FDays := TObjectList<TzDay>.Create(true);
  FRules := TObjectList<TzRule>.Create(true);
  FRuleFamilies := TObjectList<TzRuleFamily>.Create(true);
  FZones := TObjectList<TzZone>.Create();
  FLinks := TDictionary<string, TzZone>.Create();
end;

destructor TzCache.Destroy;
begin
  FDays.Free;
  FRules.Free;
  FRuleFamilies.Free;
  FZones.Free;
  FLinks.Free;

  inherited;
end;

procedure TzCache.DumpToFile(const AFile: string);
var
  LWriter: TStreamWriter;
  LDay: TzDay;
  LRule: TzRule;
  LFam: TzRuleFamily;
  LZone: TzZone;
  I, X, LGhosts: Integer;
  LDayIdx, LRuleIdx: string;
  LAliasList: TList<string>;
begin
  { SOOOOOORT the collections that require lookup later }
  FZones.Sort(TComparer<TzZone>.Construct(
    function(const Left, Right: TzZone): Integer
    begin
      { Compare by name of regions }
      Result := CompareText(Left.FName, Right.FName);
    end
  ));

  LWriter := TStreamWriter.Create(AFile);
  try
    { ========== HEADER =========== }
    LWriter.WriteLine
      ('{ This file is auto-generated. Do not change its contents since it is highly dependant on the consumer unit. }'
      );

    { ======= Days ======== }
    LWriter.WriteLine('var');
    LWriter.WriteLine
      ('  { This array contains the definitions of relative days used later on in the rules. }'
      );
    LWriter.WriteLine('  CRelativeDays: array[0 .. ' + IntToStr(FDays.Count - 1)
        + '] of TRelativeDay = (');

    for I := 0 to FDays.Count - 1 do
    begin
      LDay := FDays[I];
      LDay.FIndexInFile := I;
      LWriter.Write('    (FDayType: ');

      case LDay.FType of
        tzdFixed:
          LWriter.Write('dtFixed; FFixedDay: ' + IntToStr(LDay.FFixedDay));

        tzdLast:
          LWriter.Write('dtLastOfMonth; FLastDayOfWeek: ' +
              IntToStr(LDay.FDayOfWeek));

        tzdGEThan:
          LWriter.Write('tdNthOfMonth; FNthDayOfWeek: ' +
              IntToStr(LDay.FDayOfWeek) + '; FDayIndex: ' +
              IntToStr(LDay.FDayIndex));
      end;

      if I = (FDays.Count - 1) then
        LWriter.WriteLine(')')
      else
        LWriter.WriteLine('),');
    end;

    LWriter.WriteLine('  );');
    LWriter.WriteLine;

    { ======= RULES ======== }
    LWriter.WriteLine('var');
    LWriter.WriteLine
      ('  { This array contains the definitions of DST rules. Used by rule families. }'
      );
    LWriter.WriteLine('  CRules: array[0 .. ' + IntToStr(FRules.Count - 1) +
        '] of TRule = (');

    for I := 0 to FRules.Count - 1 do
    begin
      LRule := FRules[I];
      LRule.FIndexInFile := I;
      LWriter.WriteLine('   {CRules['+inttostr(i)+']}');
      LWriter.Write
        (Format('    (FInMonth: %d; FOnDay: @CRelativeDays[%d]; FAt: %d; FAtMode: %s; FOffset: %d; FFmtPart: ''%s'')',
          [LRule.FInMonth, LRule.FOnDay.FIndexInFile, LRule.FAt, CharToRel(LRule.FAtChar), LRule.FSave,
            LRule.FLetters]));

      if I < (FRules.Count - 1) then
        LWriter.Write(',');

      LWriter.WriteLine();
    end;

    LWriter.WriteLine('  );');
    LWriter.WriteLine;

    { ======= RULE FAMILY PARTS ======== }
    LWriter.WriteLine('var');

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

      LWriter.WriteLine('  { Date-bound rules for ' + LFam.FName + ' family }');
      LWriter.WriteLine('  CFamily_' + IntToStr(LFam.FIndexInFile) + '_Arr: array[0 .. ' +
          IntToStr(LFam.FRules.Count - 1) + '] of TYearBoundRule = (');

      { Sort the rule array in desc order to allow simpler lookup }


      for X := 0 to LFam.FRules.Count - 1 do
      begin
        LWriter.Write(Format('    (FStart: %d; FEnd: %d; FRule: @CRules[%d])', [LFam.FRules[X].FStart,
          LFam.FRules[X].FEnd, LFam.FRules[X].FRule.FIndexInFile]));

        if X < (LFam.FRules.Count - 1) then
          LWriter.Write(',');

        LWriter.WriteLine();
      end;

      LWriter.WriteLine('  );');
      LWriter.WriteLine;
    end;


    { ======= RULE FAMILIES ======== }
    LWriter.WriteLine('var');
    LWriter.WriteLine
      ('  { This array contains rule families. }'
      );
    LWriter.WriteLine('  CRuleFamilies: array[0 .. ' + IntToStr(FRuleFamilies.Count - LGhosts - 1) +
        '] of TRuleFamily = (');

    for I := 0 to FRuleFamilies.Count - 1 do
    begin
      LFam := FRuleFamilies[I];

      { Skip ghosts }
      if LFam.FIndexInFile = -1 then
        continue;

      LWriter.Write(Format('    (FCount: %d; FFirstRule: @CFamily_%d_Arr)', [LFam.FRules.Count, LFam.FIndexInFile]));

      if I < (FRuleFamilies.Count - 1) then
        LWriter.Write(',');

      LWriter.WriteLine();
    end;

    LWriter.WriteLine('  );');
    LWriter.WriteLine;

    { ======= ZONE PARTS ======== }
    LWriter.WriteLine('var');

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

      LWriter.WriteLine('  { Time periods for ' + LZone.FName + ' zone }');
      LWriter.WriteLine('  CZone_' + IntToStr(I) + '_Arr: array[0 .. ' +
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

        LWriter.Write(Format('    (FOffset: %d; FRuleFamily: %s; FFmtStr: ''%s''; FUntilYear: %d; ' +
          'FUntilMonth: %d; FUntilDay: %s; FUntilTime: %d; FUntilTimeMode: %s)',
          [LZone.FLines[X].FGmtOff, LRuleIdx, LZone.FLines[X].FFormatStr, LZone.FLines[X].FUntilYear,
           LZone.FLines[X].FUntilMonth, LDayIdx, LZone.FLines[X].FUntilTime, CharToRel(LZone.FLines[X].FUntilChar)]));

        if X < (LZone.FLines.Count - 1) then
          LWriter.Write(',');

        LWriter.WriteLine();
      end;

      LWriter.WriteLine('  );');
      LWriter.WriteLine;
    end;

    { ======= ZONES ======== }
    LWriter.WriteLine('var');
    LWriter.WriteLine
      ('  { This array contains zones. }'
      );
    LWriter.WriteLine('  CZones: array[0 .. ' + IntToStr(FZones.Count - LGhosts - 1) +
        '] of TZone = (');

    for I := 0 to FZones.Count - 1 do
    begin
      LZone := FZones[I];

      if LZone.FIndexInFile = -1 then
        continue;

      LWriter.Write(Format('    (FName: ''%s''; FCount: %d; FFirstPeriod: @CZone_%d_Arr)',
        [LZone.FName, LZone.FLines.Count, LZone.FIndexInFile]));

      if I < (FZones.Count - 1) then
        LWriter.Write(',');

      LWriter.WriteLine();
    end;

    LWriter.WriteLine('  );');
    LWriter.WriteLine;

    { ======= ALIASES ======== }
    LAliasList := TList<string>.Create(FLinks.Keys);
    LAliasList.Sort;

    { Calculate the real number of aliases, based on ghost zones }
    LGhosts := 0;
    for I := 0 to LAliasList.Count - 1 do
    begin
      LZone := FLinks[LAliasList[I]];
      if LZone.FIndexInFile = -1 then
        Inc(LGhosts);
    end;

    LWriter.WriteLine('var');
    LWriter.WriteLine
      ('  { This array contains zone aliases. }'
      );
    LWriter.WriteLine('  CAliases: array[0 .. ' + IntToStr(LAliasList.Count - LGhosts - 1) +
        '] of TZoneAlias = (');

    for I := 0 to LAliasList.Count - 1 do
    begin
      LZone := FLinks[LAliasList[I]];

      if LZone.FIndexInFile = -1 then
        continue;

      LWriter.Write(Format('    (FName: ''%s''; FAliasTo: @CZones[%d])',
        [LAliasList[I], LZone.FIndexInFile]));

      if I < (LAliasList.Count - 1) then
        LWriter.Write(',');

      LWriter.WriteLine();
    end;

    LWriter.WriteLine('  );');
    LWriter.WriteLine;

    LAliasList.Free;
  finally
    LWriter.Free;
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
  FRules := TList<TRuleWithSpan>.Create();
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
  FLines := TList<TZoneLine>.Create;
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
