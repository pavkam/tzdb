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

{ General conditional section. Checks for specific RTL "features" shared across
  FreePascal and different versions of Delphi. Recommended version is Delphi XE though ... }

{$INCLUDE 'Version.inc'}
unit TZDB;

interface
uses
  SysUtils,
  DateUtils,
  Types,
{$IFDEF DELPHI}
  TimeSpan,
  Generics.Collections,
  Generics.Defaults,
{$ELSE}
  FGL,
  SyncObjs,
{$ENDIF}
  Classes;

type
{$IFNDEF DELPHI}
  ///  <summary>Exception thrown when the passed local time is invalid.</summary>
  ELocalTimeInvalid = class(Exception);

  ///  <summary>Defines four types that a local date/time type can be in.</summary>
  TLocalTimeType = (
    ///  <summary>The local time is in the Standard year period.</summary>
    lttStandard,
    ///  <summary>The local time is in the DST year period.</summary>
    lttDaylight,
    ///  <summary>The local time is in DST -> Standard year period.</summary>
    lttAmbiguous,
    ///  <summary>The local time is in the Standard -> DST year period.</summary>
    lttInvalid
  );
{$ENDIF}

  ///  <summary>Exception type used to signal the caller code that a requested time zone
  ///  is not present in the bundled database or that its format is invalid.</summary>
  ETimeZoneInvalid = class(Exception);

  ///  <summary>Exception type used to signal the caller code that date/time year details are not
  ///  bundled for the given time zone.</summary>
  EUnknownTimeZoneYear = class(Exception);

  ///  Special type used to manipulate TDateTime in millisecond precision.
  ///  This is an internal type.
  TPreciseTime = type Int64;

  /// <summary>Represents a specific date/time segment of the year.</summary>
  /// <remarks>A calendar year in most time zones is divided into standard/ambiguous/daylight/invalid/standard segments.</remarks>
  TYearSegment = record
  private
    FStartsAt, FEndsAt: TPreciseTime;
    FType: TLocalTimeType;
    FName: string;
    FPeriodOffset, FBias: Int64;

    function GetUtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF}; inline;
    function GetStartsAt: TDateTime;
    function GetEndsAt: TDateTime;
  public
    /// <summary>The date/time when the segment starts.</summary>
    /// <returns>A date/time value representing the start of the segment.</returns>
    property StartsAt: TDateTime read GetStartsAt;

    /// <summary>The date/time when the segment ends.</summary>
    /// <returns>A date/time value representing the end of the segment.</returns>
    property EndsAt: TDateTime read GetEndsAt;

    /// <summary>The type of the segment.</summary>
    /// <returns>An enum value representing the type of the segment.</returns>
    property LocalType: TLocalTimeType read FType;

    /// <summary>The time zone display name used to describe the segment.</summary>
    /// <returns>The string value of the display name.</returns>
    property DisplayName: string read FName;

    /// <summary>The time zone UTC offset.</summary>
    /// <returns>The UTC offset including the DST bias.</returns>
    property UtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF} read GetUtcOffset;

{$IFDEF FPC}
    /// <summary>Equality operator used to compare two values of this type</summary>
    /// <param name="ALeft">The first segment to compare.</summary>
    /// <param name="ARight">The second segment to compare.</summary>
    /// <returns><c>True</c> if the segments are equal.</returns>
    class operator Equal(const ALeft, ARight: TYearSegment): Boolean;
{$ENDIF}
  end;

  /// <summary>An array of year segments.</summary>
  TYearSegmentArray = array of TYearSegment;

  ///  <summary>A timezone class implementation that retreives its data from the bundled database.</summary>
  ///  <remarks>This class inherits the standard <c>TTimeZone</c> class in Delphi XE.</remarks>
  TBundledTimeZone = class
  private
    FZone: Pointer; { PZone }

{$IFNDEF DELPHI}
    FSegmentsByYearLock: TCriticalSection;
{$ENDIF}
    FSegmentsByYear: {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<Word, TYearSegmentArray>;

    function GetSegment(const AYear: Word; const APreciseTime: TPreciseTime;
      const AForceDaylight: Boolean; const AFailOnInvalid: Boolean): TYearSegment;
    function GetSegmentUtc(const AYear: Word; const APreciseTime: TPreciseTime): TYearSegment;
    function TryFindSegment(const AYear: Word; const AType: TLocalTimeType; const ARev: Boolean;
      out ASegment: TYearSegment): Boolean;
    function GetCurrentAbbreviation: string;
    function GetCurrentDisplayName: string;
    function GetCurrentUtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};

  protected
    ///  <summary>Returns the ID of the timezone. An ID is a string that should uniquely identify the timezone.</summary>
    ///  <returns>The ID of the timezone.</returns>
    function DoGetID: string;
   public
    ///  <summary>Creates a new instance of this timezone class.</summary>
    ///  <param name="ATimeZoneID">The ID of the timezone to use (ex. "Europe/Bucharest").</param>
    ///  <exception cref="TZDB|ETimeZoneInvalid">The specified ID cannot be found in the bundled database.</exception>
    constructor Create(const ATimeZoneID: string);

    ///  <summary>Destroys the current instance.</summary>
    destructor Destroy; override;

    ///  <summary>Returns a list of known time zones.</summary>
    ///  <param name="AIncludeAliases">Pass <c>True</c> to include time zone aliases into the list.</param>
    ///  <returns>An array of strings representing the IDs of the known time zones.</returns>
    class function KnownTimeZones(const
      AIncludeAliases: Boolean = False): TStringDynArray;

    ///  <summary>Returns a list of known time zone aliases.</summary>
    ///  <returns>An array of strings representing the aliases of the known time zones.</returns>
    class function KnownAliases: TStringDynArray;

    ///  <summary>Returns the time zone name for a given alias.</summary>
    ///  <param name="AAlias">The alias to lookup.</param>
    ///  <returns>The name of the time zone, if found.</returns>
    ///  <exception cref="TZDB|ETimeZoneInvalid">The specified alias cannot be found in the bundled database.</exception>
    class function GetTimeZoneFromAlias(const AAliasID: string): string; inline;

    ///  <summary>Returns an instance of this time zone class.</summary>
    ///  <param name="ATimeZoneID">The ID of the timezone to use (ex. "Europe/Bucharest").</param>
    ///  <exception cref="TZDB|ETimeZoneInvalid">The specified ID cannot be found in the bundled database.</exception>
    class function GetTimeZone(const ATimeZoneID: string): TBundledTimeZone;

    ///  <summary>Returns the version of the TZDB component.</summary>
    ///  <returns>A string representing the version of the source.</returns>
    class function Version: string;

    ///  <summary>Returns the version of compiled TZDB database.</summary>
    ///  <returns>A string representing the compiled version.</returns>
    class function DbVersion: string;

    ///  <summary>Breaks a given year into components segments.</summary>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function GetYearBreakdown(const AYear: Word): TYearSegmentArray;

    ///  <summary>Get the starting date/time of daylight period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start time of daylight saving period in the local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function DaylightTimeStart(const AYear: Word): TDateTime; inline;

    ///  <summary>Get the starting date/time of standard period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start date/time of standard period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function StandardTimeStart(const AYear: Word): TDateTime; inline;

    ///  <summary>Get the starting date/time of invalid period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start date/time of invalid period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function InvalidTimeStart(const AYear: word): TDateTime; inline;

    ///  <summary>Get the starting date/time of ambiguous period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start date/time of ambiguous period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function AmbiguousTimeStart(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of daylight saving period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The end date/time of daylight saving period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function DaylightTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of standard period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The ending date/time of standard period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function StandardTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of invalid period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The end date/time of invalid period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function InvalidTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of ambiguous period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The end date/time of ambiguous period in local time; or '00/00/0000 00:00:00.000' is there is no such date/time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function AmbiguousTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Determines if the timezone has daylight saving period.</summary>
    ///  <param name="AYear">The year to check.</param>
    ///  <returns><c>true</c> if the timezone has daylight saving time in the specified year.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function HasDaylightTime(const AYear: word): Boolean; inline;

    ///  <summary>Converts an UTC date/time to ISO8601 date time string.</summary>
    ///  <param name="ADateTime">The UTC date/time to convert.</param>
    ///  <returns>The ISO8601 date/time string that corresponds to the passed UTC time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function ToISO8601Format(const ADateTime: TDateTime): String;

    ///  <summary>Generates an abbreviation string for the given local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="AForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>A string containing the abbreviation.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    function GetAbbreviation(const ADateTime: TDateTime; const AForceDaylight: Boolean = false): string;

    ///  <summary>Generates a diplay string for the given local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="AForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>A string containing the display name.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    function GetDisplayName(const ADateTime: TDateTime; const AForceDaylight: Boolean = false): string;

    ///  <summary>Returns the type of the local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns>An enumeration value specifying the type of the local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;

    ///  <summary>Checks whether the specified local time is ambiguous.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns><c>True</c> if the local time is ambiguous; <c>False</c> otherwise.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function IsAmbiguousTime(const ADateTime: TDateTime): Boolean;

    ///  <summary>Checks whether the specified local time is daylight.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="AForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns><c>True</c> if the local time is ambiguous; <c>False</c> otherwise.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function IsDaylightTime(const ADateTime: TDateTime; const AForceDaylight: Boolean = false): Boolean;

    ///  <summary>Checks whether the specified local time is invalid.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns><c>True</c> if the local time is invalid; <c>False</c> otherwise.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function IsInvalidTime(const ADateTime: TDateTime): Boolean; inline;

    ///  <summary>Checks whether the specified local time is standard.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="AForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns><c>True</c> if the local time is standard; <c>False</c> otherwise.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function IsStandardTime(const ADateTime: TDateTime; const AForceDaylight: Boolean = false): Boolean;

    ///  <summary>Returns the UTC offset of the given local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="AForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>The UTC offset of the given local time. Subtract this value from the passed local time to obtain an UTC time.</returns>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function GetUtcOffset(const ADateTime: TDateTime; const AForceDaylight: Boolean = false):
      {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};

    ///  <summary>Converts an UTC time to a local time.</summary>
    ///  <param name="ADateTime">The UTC time.</param>
    ///  <returns>The local time that corresponds to the passed UTC time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function ToLocalTime(const ADateTime: TDateTime): TDateTime;

    ///  <summary>Converts a local time to an UTC time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="AForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>The UTC time that corresponds to the passed local time.</returns>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function ToUniversalTime(const ADateTime: TDateTime;
      const AForceDaylight: Boolean = false): TDateTime;

    ///  <summary>Returns the ID of the timezone. An ID is a string that should uniquely identify the timezone.</summary>
    ///  <returns>The ID of the timezone.</returns>
    property ID: string read DoGetID;

    ///  <summary>Returns the current time zone's display name string.</summary>
    ///  <returns>A string containing the display name.</returns>
    property DisplayName: string read GetCurrentDisplayName;

    ///  <summary>Returns the current time zone's abbreviation string.</summary>
    ///  <returns>A string containing the abbreviation.</returns>
    property Abbreviation: string read GetCurrentAbbreviation;

    ///  <summary>Returns the current time zone's UTC offset.</summary>
    ///  <returns>The current UTC offset.</returns>
    property UtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF} read GetCurrentUtcOffset;
  end;

implementation

resourcestring
  SNoBundledTZForName = 'Could not find any data for timezone "%s".';
  SDateTimeNotResolvable = 'The date [%s] does not match any known period of timezone "%s".';
  SYearNotResolvable = 'The year [%d] does not match any known period of timezone "%s".';
  SInvalidLocalTime = 'Local date/time value %s is invalid (does not exist in the time zone).';

const
  CComponentVersion = '2.1.0.123';

type
  { Day type. Specifies the "relative" day in a month }
  TDayType = (dtFixed, dtLastOfMonth, dtNthOfMonth, dtPredOfMonth);

  { Specifies the mode in which a time value is specified }
  TTimeMode = (trLocal, trStandard, trUniversal);

  { Stores the information about the relative days }
  TRelativeDay = record
    case FDayType: TDayType of
      dtFixed:
        (FFixedDay: Word);
      dtLastOfMonth:
        (FLastDayOfWeek: Word);
      dtNthOfMonth:
        (FNthDayOfWeek: Word; FNthDayIndex: Word);
      dtPredOfMonth:
        (FPredDayOfWeek: Word; FPredDayIndex: Word);
  end;

  { Pointer to a relative day }
  PRelativeDay = ^TRelativeDay;

  { Defines a rule used for DST changes }
  TRule = record
    FInMonth: Word; { The month (1 - 12) when DST change occurs }
    FOnDay: PRelativeDay; { Pointer to a TRelativeDay value }
    FAt: Int64; { Time, in seconds }
    FAtMode: TTimeMode; { Time relation mode }
    FOffset: Int64; { Offset from GMT in seconds }
    FFmtPart: string;
    { A symbolic string used later when building short TZ names }
  end;

  { Pointer to a rule }
  PRule = ^TRule;

  { Defines a rule that also has a validity date defined }
  TYearBoundRule = record
    FStart: Word; { The year in which the rule starts to apply }
    FEnd: Word; { The year in which the rule ends to apply }
    FRule: PRule; { A pointer to the actual rule }
  end;

  { Pointer to a year-bound rule entry }
  PYearBoundRule = ^TYearBoundRule;

  { Defines a rule family. If fact it is a set of rules combined under the same ID }
  TRuleFamily = record
    FCount: Integer; { Count of rule in the current family }
    FFirstRule: PYearBoundRule;
    { Pointer to the first rule in a static array defined previously }
  end;

  { A pointer to a rule family }
  PRuleFamily = ^TRuleFamily;

  { A period of some years (for a zone) that defines specific DST rules and offsets }
  TPeriod = record
    FOffset: Integer; { GMT offset in seconds for this period of time }
    FRuleFamily: PRuleFamily;
    { Pointer to the family if rules that apply to this period }
    FFmtStr: string;
    { Format string that will get translated in certain conditions }
    FUntilYear, FUntilMonth: Word; { Period is valid until this Year/Month }
    FUntilDay: PRelativeDay; { Period is valid until this Day in Year/Month }
    FUntilTime: Int64;
    FUntilTimeMode: TTimeMode; { Time relation mode }
    { Period is valid until this time of day Day in Year/Month. In seconds }
  end;

  { Pointer to a TPeriod }
  PPeriod = ^TPeriod;

  { Defines a time-zone. }
  TZone = record
    FName: string; { Zone name (aka Europe/Romania, Europe/London etc) }
    FCount: Integer; { Count of periods defined by this zone }
    FFirstPeriod: PPeriod; { Pointer to the first TPeriod for this zone }
  end;

  { Pointer to a zone object }
  PZone = ^TZone;

  { Alias to a zone }
  TZoneAlias = record
    FName: string; { Name of the zone to alias }
    FAliasTo: PZone; { Pointer to aliased zone }
  end;

{$INCLUDE 'TZDB.inc'}

const
  CNullDateTime = -DateDelta;

function DateTimeToPreciseTime(const ADateTime: TDateTime): TPreciseTime; inline;
var
  D, MS: Int64;
begin
  D := Round(ADateTime * MSecsPerDay);
  MS := D div MSecsPerDay;
  Result := ((DateDelta + MS) * MSecsPerDay) + (Abs(D) mod MSecsPerDay);
end;

function PreciseTimeToDateTime(const APreciseTime: TPreciseTime): TDateTime; inline;
var
  D, MS: Int64;
begin
  D := (Int64(PUInt64(@APreciseTime)^ div Cardinal(MSecsPerDay)) - DateDelta) * MSecsPerDay;
  MS := Int64(PUInt64(@APreciseTime)^ mod Cardinal(MSecsPerDay));

  if D < 0 then
    MS := -MS;

  Result := (D + MS) / MSecsPerDay;
end;

function PreciseTimeToStr(const APreciseTime: TPreciseTime): string; inline;
begin
  Result := FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', PreciseTimeToDateTime(APreciseTime));
end;

function IncMillisecond(const APreciseTime: TPreciseTime; const AMilliseconds: Int64): TPreciseTime; inline;
begin
  Result := APreciseTime + AMilliseconds;
end;

function IncSecond(const APreciseTime: TPreciseTime; const ASeconds: Int64): TPreciseTime; inline;
begin
  Result := IncMillisecond(APreciseTime, ASeconds * 1000);
end;

function IncMinute(const APreciseTime: TPreciseTime; const AMinutes: Int64): TPreciseTime; inline;
begin
  Result := IncSecond(APreciseTime, AMinutes * 60);
end;

function IncHour(const APreciseTime: TPreciseTime; const AHours: Int64): TPreciseTime; inline;
begin
  Result := IncMinute(APreciseTime, AHours * 60);
end;

function IncDay(const APreciseTime: TPreciseTime; const ADays: Int64): TPreciseTime; inline;
begin
  Result := IncHour(APreciseTime, ADays * 24);
end;

function IncWeek(const APreciseTime: TPreciseTime; const ADays: Int64): TPreciseTime; inline;
begin
  Result := IncDay(APreciseTime, ADays * 7);
end;

function EncodePreciseDate(const AYear, AMonth, ADay: Word): TPreciseTime; inline;
begin
  Result := DateTimeToPreciseTime(EncodeDate(AYear, AMonth, ADay));
end;

function DayOfTheWeek(const APreciseTime: TPreciseTime): Word; inline;
begin
  Result := DateUtils.DayOfTheWeek(PreciseTimeToDateTime(APreciseTime));
end;

function DayOf(const APreciseTime: TPreciseTime): Word; inline;
begin
  Result := DateUtils.DayOf(PreciseTimeToDateTime(APreciseTime));
end;

function MonthOf(const APreciseTime: TPreciseTime): Word; inline;
begin
  Result := DateUtils.MonthOf(PreciseTimeToDateTime(APreciseTime));
end;

function YearOf(const APreciseTime: TPreciseTime): Word; inline;
begin
  Result := DateUtils.YearOf(PreciseTimeToDateTime(APreciseTime));
end;

function ComparePreciseTime(const A, B: TPreciseTime): Integer; inline;
begin
  if A > B then Result := 1
  else if A < B then Result := -1
  else Result := 0;
end;

function EncodeDateMonthLastDayOfWeek(const AYear, AMonth, ADayOfWeek: Word): TPreciseTime;
var
  LDoW: Word;
begin
  { Generate a date that looks like: Year/Month/(Last Day of Month) }
  Result := EncodePreciseDate(AYear, AMonth, DaysInAMonth(AYear, AMonth));

  { Get the day of week for this newly crafted date }
  LDoW := DayOfTheWeek(Result);

  { We're too far off now, let's decrease the number of days till we get to the desired one }
  if LDoW > ADayOfWeek then
    Result := IncDay(Result, -1 * (LDoW - ADayOfWeek))
  else if LDoW < ADayOfWeek then
    Result := IncDay(Result, -1 * (DaysPerWeek - ADayOfWeek + LDoW));
end;

function EncodeDateMonthFirstDayOfWeek(const AYear, AMonth, ADayOfWeek: Word): TPreciseTime;
var
  LDoW: Word;
begin
  { Generate a date that looks like: Year/Month/1st }
  Result := EncodePreciseDate(AYear, AMonth, 1);

  { Get the day of week for this newly crafted date }
  LDoW := DayOfTheWeek(Result);

  { We're too far off now, let's decrease the number of days till we get to the desired one }
  if LDoW > ADayOfWeek then
    Result := IncDay(Result, DaysPerWeek - LDoW + ADayOfWeek)
  else if (LDoW < ADayOfWeek) Then
    Result := IncDay(Result, ADayOfWeek - LDoW);
end;

function EncodeDateMonthFirstDayOfWeekAfter(const AYear, AMonth, ADayOfWeek, AAfter: Word): TPreciseTime;
begin
  { Generate a date with the given day of week as first in month }
  Result := EncodeDateMonthFirstDayOfWeek(AYear, AMonth, ADayOfWeek);

  { Iterate until we've surpassed our min requirement }
  while DayOf(Result) < AAfter do
  begin
    Result := IncWeek(Result, 1);

    { Safe-guard! If we've gotten to another month, get back a week and stop. }
    if MonthOf(Result) <> AMonth then
    begin
      Result := IncWeek(Result, -1);
      break;
    end
  end;
end;

function EncodeDateMonthFirstDayOfWeekBefore(const AYear, AMonth, ADayOfWeek, ABefore: Word): TPreciseTime;
var
  LWeekDayDiff : Integer;
begin
  { Generate a date with ABefore as the Day in AMonth and AYear }
  Result := EncodePreciseDate(AYear, AMonth, ABefore);

  { Adjust Date by difference in DayOfWeek of Date and ADayOfWeek.  If that difference is negative subtract a week. }
  LWeekDayDiff := DayOfTheWeek(Result) - ADayOfWeek;
  if LWeekDayDiff > 0 then
    Result := IncDay(Result, -LWeekDayDiff)
  else if LWeekDayDiff < 0 then
    Result := IncDay(Result, - (LWeekDayDiff + 7));
end;

function RelativeToPreciseTime(const AYear, AMonth: Word; const ARelativeDay: PRelativeDay;
  const ATimeOfDay: Int64): TPreciseTime;
begin
  Result := 0;

  { Special case - if there is no day defined then there is no time also. Exit with only the date part. }
  if ARelativeDay = nil then
    Result := EncodePreciseDate(AYear, AMonth, 1)
  else if ARelativeDay^.FDayType = dtFixed then
    Result := EncodePreciseDate(AYear, AMonth, ARelativeDay^.FFixedDay)
  else if ARelativeDay^.FDayType = dtLastOfMonth then
    Result := EncodeDateMonthLastDayOfWeek(AYear, AMonth, ARelativeDay^.FLastDayOfWeek)
  else if ARelativeDay^.FDayType = dtNthOfMonth then
    Result := EncodeDateMonthFirstDayOfWeekAfter(AYear, AMonth, ARelativeDay^.FNthDayOfWeek, ARelativeDay^.FNthDayIndex)
  else if ARelativeDay^.FDayType = dtPredOfMonth then
    Result := EncodeDateMonthFirstDayOfWeekBefore(AYear, AMonth, ARelativeDay^.FPredDayOfWeek, ARelativeDay^.FPredDayIndex);

  { Attach the time part now }
  Result := IncSecond(Result, ATimeOfDay);
end;

function FormatAbbreviation(const APeriod: PPeriod; const ARule: PRule;
  const ALocalTimeType: TLocalTimeType): string;
var
  LDelimIndex: Integer;
begin

{
  From IANA TZDB  https://data.iana.org/time-zones/tz-how-to.html

  The FORMAT column specifies the usual abbreviation of the time zone name. It can have one of three forms:
    * A string of three or more characters that are either ASCII alphanumerics, \93+\94, or \93-\94, in which case that\92s the abbreviation.
    * A pair of strings separated by a slash (\91/\92), in which case the first string is the abbreviation for the standard
      time name and the second string is the abbreviation for the daylight saving time name.
    * A string containing \93%s,\94 in which case the \93%s\94 will be replaced by the text in the appropriate Rule\92s LETTER column.
}
  LDelimIndex := Pos('/', APeriod^.FFmtStr);
  if LDelimIndex > 0 then
  begin
    case ALocalTimeType of
      lttStandard: Result := Copy(APeriod^.FFmtStr, 1, LDelimIndex - 1);
      lttDaylight: Result := Copy(APeriod^.FFmtStr, LDelimIndex + 1, Length(APeriod^.FFmtStr));
    end;
  end
  else if Pos('%s', APeriod^.FFmtStr) > 0 then
  begin
    { There is a place holder in the format string. Replace if with the current letter in the rule }
    if ARule <> nil then
      Result := Format(APeriod^.FFmtStr, [ARule^.FFmtPart])
    else
      Result := Format(APeriod^.FFmtStr, ['']);
    { In case no rule is defined, replace the placeholder with an empty string }
  end else
    Result := APeriod^.FFmtStr;
end;

type
  { Stored the data for an "observed rule" which is either a rule or a rule-less segment of a year. }
  TObservedRule = record
    FPeriod: PPeriod;
    FRule: PRule;
    FStartsOn: TPreciseTime;
    FYear: Word;
    FNegDst: Boolean;

    function Bias: Int64; inline;
    function UtcOffset: Int64; inline;

{$IFDEF FPC}
    class operator Equal(const A, B: TObservedRule): Boolean;
{$ENDIF}
  end;

{$IFDEF FPC}
  class operator TObservedRule.Equal(const A, B: TObservedRule): Boolean;
  begin
    Result :=
      (A.FPeriod = B.FPeriod) and
      (A.FRule = B.FRule) and
      (A.FStartsOn = B.FStartsOn) and
      (A.FYear = B.FYear) and
      (A.FNegDst = B.FNegDst);
  end;
{$ENDIF}

  function TObservedRule.Bias: Int64;
  begin
    if FRule <> nil then
      Result := FRule^.FOffset
    else
      Result := 0;
  end;

  function TObservedRule.UtcOffset: Int64;
  begin
    Result := FPeriod^.FOffset + Bias;
  end;

type
  TObservedRuleArray = array of TObservedRule;
  TPRuleArray = array of PRule;
  PPRuleAndYear = ^TPRuleAndYear;
  TPRuleAndYear = record
    FYear: Word;
    FRule: PRule;
{$IFDEF FPC}
    class operator Equal(const ALeft, ARight: TPRuleAndYear): Boolean;
{$ENDIF}
  end;

{$IFDEF FPC}
  class operator TPRuleAndYear.Equal(const ALeft, ARight: TPRuleAndYear): Boolean;
  begin
    Result := (ALeft.FYear = ARight.FYear) and (ALeft.FRule = ARight.FRule);
  end;
{$ENDIF}

function CompareRulesByStartTime(A, B: TPRuleAndYear): Integer; inline;
var
  L, R: TPreciseTime;
begin
  L := RelativeToPreciseTime(A.FYear, A.FRule^.FInMonth, A.FRule^.FOnDay, A.FRule^.FAt);
  R := RelativeToPreciseTime(B.FYear, B.FRule^.FInMonth, B.FRule^.FOnDay, B.FRule^.FAt);
  Result := ComparePreciseTime(L, R);
end;

function GetPeriodRulesForYear(const APeriod: PPeriod; const AYear: Word): TPRuleArray;
var
  LRule: PYearBoundRule;
  I: Integer;
  LRules: {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TPRuleAndYear>;
{$IFDEF DELPHI}
  LComparer: IComparer<TPRuleAndYear>;
{$ENDIF}
  LElem: TPRuleAndYear;
begin
  Result := nil;

  { Check whether we actually have a fule family attached }
  if APeriod^.FRuleFamily <> nil then
  begin
    LRules := {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TPRuleAndYear>.Create;

    { Iterate over all rules in the period. }
    LRule := APeriod^.FRuleFamily^.FFirstRule;
    for I := 0 to APeriod^.FRuleFamily^.FCount - 1 do
    begin
      if (AYear >= LRule^.FStart) and (AYear <= LRule^.FEnd) then
      begin
        LElem.FYear := AYear;
        LElem.FRule := LRule^.FRule;

        LRules.Add(LElem);
      end;

      { Go to next rule }
      Inc(LRule);
    end;

    { Sort the list ascending by the activation date/time }
{$IFDEF FPC}
    LRules.Sort(@CompareRulesByStartTime);
{$ELSE}
    LComparer := TComparer<TPRuleAndYear>.Construct(function(const A, B: TPRuleAndYear): Integer
    begin
      Result := CompareRulesByStartTime(A, B);
    end);
    LRules.Sort(LComparer);
{$ENDIF}

    SetLength(Result, LRules.Count);
    for I := 0 to LRules.Count - 1 do
      Result[I] := LRules[I].FRule;

    LRules.Free;
  end;
end;

function GetObservedRulesForYear(const AZone: PZone; const AYear: Word): TObservedRuleArray;
var
  LPeriod: PPeriod;
  LStart, LEnd: TPreciseTime;
  LRules: TPRuleArray;
  I, X, L: Integer;
  Z: Int64;
  PR: PRule;
  LY1: {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TObservedRule>;
  LYMinus1, LYPlus1, LR, LSk: TObservedRule;
begin
  { Mark all intermediary data as un-initialized. <-- this is date ZERO which is the start point. }
  LStart := DateTimeToPreciseTime(CNullDateTime);

  LYMinus1.FPeriod := nil;
  LYPlus1.FPeriod := nil;

  LY1 := {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TObservedRule>.Create;
  try
    { Iterate over all periods in the zone. }
    LPeriod := AZone^.FFirstPeriod;
    for I := 0 to AZone^.FCount - 1 do
    begin
      { Calculate the end date of the period }
      LEnd := RelativeToPreciseTime(
        LPeriod^.FUntilYear, LPeriod^.FUntilMonth, LPeriod^.FUntilDay, LPeriod^.FUntilTime);

      { Try to get the last rule for the period (needed to calculate boundary) }
      LRules := GetPeriodRulesForYear(LPeriod, LPeriod^.FUntilYear);
      if LPeriod^.FUntilDay <> nil then
      begin
        { Adjust the end of the period according to the last rule in it. }
        if Length(LRules) > 0 then
          Z := LRules[Length(LRules) - 1]^.FOffset
        else
          Z := 0;

        case LPeriod^.FUntilTimeMode of
          trStandard:
            LEnd := IncSecond(LEnd, Z);
          trUniversal:
            LEnd := IncSecond(LEnd, LPeriod^.FOffset + Z);
        end;
      end;

      { Extract the last millisecond in the end to mark the end of the period. }
      LEnd := IncMilliSecond(LEnd, -1);

      { Collect last rule of the previous year. }
      if (YearOf(LStart) <= AYear - 1) and (YearOf(LEnd) >= AYear - 1) then
      begin
        { Load the rules for previous year. }
        LRules := GetPeriodRulesForYear(LPeriod, AYear - 1);

        LYMinus1.FPeriod := LPeriod;
        LYMinus1.FStartsOn := LStart;
        LYMinus1.FYear := AYear - 1;
        LYMinus1.FNegDst := false;

        if Length(LRules) > 0 then
        begin
          LYMinus1.FRule := LRules[Length(LRules) - 1];

          { Determine the DST sign in period. }
          for PR in LRules do
            if PR^.FOffset < 0 then begin LYMinus1.FNegDst := true; break; end;
        end else
          LYMinus1.FRule := nil;
      end;

      { Collect first rule of the next year. }
      if (YearOf(LStart) <= AYear + 1) and (YearOf(LEnd) >= AYear + 1) and (LYPlus1.FPeriod = nil) then
      begin
        { Load the rules for following year. }
        LRules := GetPeriodRulesForYear(LPeriod, AYear + 1);

        LYPlus1.FPeriod := LPeriod;
        LYPlus1.FStartsOn := LStart;
        LYPlus1.FYear := AYear + 1;
        LYPlus1.FNegDst := false;

        if Length(LRules) > 0 then
        begin
          LYPlus1.FRule := LRules[0];

          { Determine the DST sign in period. }
          for PR in LRules do
            if PR^.FOffset < 0 then begin LYPlus1.FNegDst := true; break; end;
        end else
          LYPlus1.FRule := nil;
      end;

      { Collect all the rules for the year we're looking for. }
      if (YearOf(LStart) <= AYear) and (YearOf(LEnd) >= AYear) then
      begin
        { Load the rules for this year. }
        LRules := GetPeriodRulesForYear(LPeriod, AYear);

        LR.FPeriod := LPeriod;
        LR.FStartsOn := LStart;
        LR.FYear := AYear;
        LR.FNegDst := false;

        { Determine the DST sign in period. }
        for PR in LRules do
          if PR^.FOffset < 0 then begin LR.FNegDst := true; break; end;

        if Length(LRules) > 0 then
        begin
          { Pump all the rules in }
          for X := 0 to Length(LRules) - 1 do
          begin
            LR.FRule := LRules[X];
            LY1.Add(LR);
          end;
        end else
        begin
          { No rules available for this period/year. Still have to add something to indicate that. }
          LR.FRule := nil;
          LY1.Add(LR);
        end;
      end;

      { Update the start of the next period as the end of the current one and iterate next. }
      LStart := IncMillisecond(LEnd, 1);
      Inc(LPeriod);
    end;

    { Add the extra rules from prev and next years into the list }
    if LYMinus1.FPeriod <> nil then
      LY1.Insert(0, LYMinus1);
    if LYPlus1.FPeriod <> nil then
      LY1.Add(LYPlus1);

    { Re-calculate the start dates now and moveto result. }
    SetLength(Result, LY1.Count);

    L := 0;
    LSk.FPeriod := nil;

    for I := 0 to LY1.Count - 1 do
    begin
      LR := LY1[I];
      LStart := LR.FStartsOn;

      if LR.FRule <> nil then
      begin
        { This is an actual rule, we can calculate the start time properly based on its' data }
        LR.FStartsOn := RelativeToPreciseTime(LR.FYear, LR.FRule^.FInMonth, LR.FRule^.FOnDay, LR.FRule^.FAt);
        if LR.FRule^.FOnDay <> nil then
        begin
          case LR.FRule^.FAtMode of
            trStandard:
            begin
              if (I > 0) and (LY1[I - 1].FRule <> nil) then
                LR.FStartsOn := IncSecond(LR.FStartsOn, LY1[I - 1].FRule^.FOffset);
            end;
            trUniversal:
            begin
              if I > 0 then
              begin
                { Adjust to local time based on previous rule }
                LR.FStartsOn := IncSecond(LR.FStartsOn, LY1[I - 1].FPeriod^.FOffset);
                if LY1[I - 1].FRule <> nil then
                  LR.FStartsOn := IncSecond(LR.FStartsOn, LY1[I - 1].FRule^.FOffset);
              end;
            end;
          end;
        end;
      end
      else
      begin
        { This is not technically a rule - just naked segment. We'll need to infer data. }
        if YearOf(LR.FStartsOn) < LR.FYear then
          LR.FStartsOn := EncodePreciseDate(LR.FYear, 1, 1);
      end;

      { Very special case in here. Need suppress overlapping rules form different periods but same rule family.
        Also important to preserve the last overlapping rule as the last active on in the new period. }
      if ComparePreciseTime(LStart, LR.FStartsOn) <= 0 then
      begin
        if (LSk.FPeriod <> nil) and
          ((LSk.FPeriod <> LR.FPeriod) or (LSk.FYear <> LR.FYear)) then
          begin
            { The last skipped rule is the last one in the period/year. Assume that is still active }
            Result[L] := LSk;
            Inc(L);
          end;

        { Save the current rule as well. }
        Result[L] := LR;
        Inc(L);

        LSk.FPeriod := nil;
      end else
      begin
        { Save last skipped rule and reset its start to the start of its period. }
        LSk := LR;
        LSk.FStartsOn := LStart;
      end;

    end;

    SetLength(Result, L);
  finally
    LY1.Free;
  end;
end;

function BreakdownYearIntoSegments(const AZone: PZone; const AYear: Word): TYearSegmentArray;
var
  X, Z: Integer;
  LSegment: TYearSegment;
  LObsRules: TObservedRuleArray;
  LRule, LNextRule: TObservedRule;
  LEnd: TPreciseTime;
  LCarryDelta, LDelta: Int64;
  LSegments: {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TYearSegment>;
  LYStart, LYEnd: TPreciseTime;
begin
  Result := nil;
  LCarryDelta := 0;

  LSegments := {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TYearSegment>.Create;
  try
    { Get all rules that intersect this year in some way (this means some rules from Year +- 1 will apply) }
    LObsRules := GetObservedRulesForYear(AZone, AYear);

    for X := 0 to Length(LObsRules) - 1 do
    begin
      { Get current rule and next rule. Both are used to calculate things. }
      LRule := LObsRules[X];
      if X < Length(LObsRules) - 1 then
        LNextRule := LObsRules[X + 1]
      else
        LNextRule.FPeriod := nil;

      { Fill in standard details. }
      LSegment.FPeriodOffset := LRule.FPeriod^.FOffset;
      LSegment.FBias := LRule.Bias;

      if not LRule.FNegDst then
      begin
        { 99.9% of zone have positive, normal DST offsets }
        if LSegment.FBias > 0 then
          LSegment.FType := lttDaylight
        else
          LSegment.FType := lttStandard;

        LSegment.FName := FormatAbbreviation(LRule.FPeriod, LRule.FRule, LSegment.FType);
      end else
      begin
        { For the rest we have to invert the logic }
        if LSegment.FBias < 0 then
        begin
          LSegment.FType := lttStandard;
          LSegment.FName := FormatAbbreviation(LRule.FPeriod, LRule.FRule, lttDaylight);
        end else
        begin
          LSegment.FType := lttDaylight;
          LSegment.FName := FormatAbbreviation(LRule.FPeriod, LRule.FRule, lttStandard);
        end;
      end;

      LSegment.FStartsAt := IncSecond(LRule.FStartsOn, LCarryDelta);

      { If there is another rule following, calculate the boundary and introduce the invalid/ambiguous regions. }
      if LNextRule.FPeriod <> nil then
      begin
        { Calculate the overall delta between two segments. }
        LDelta := LNextRule.UtcOffset - LRule.UtcOffset;

        { Add the core segment. }
        if LDelta < 0 then
        begin
          LCarryDelta := 0;
          LEnd := IncSecond(LNextRule.FStartsOn, LDelta);
        end else
        begin
          LCarryDelta := LDelta;
          LEnd := LNextRule.FStartsOn;
        end;

        LSegment.FEndsAt := IncMillisecond(LEnd, -1);
        LSegments.Add(LSegment);

        if LDelta > 0 then
        begin
          { This is a positive bias. This means we have an invalid region. }
          LSegment.FType := lttInvalid;
          LSegment.FBias := 0;
          LSegment.FStartsAt := LEnd;
          LSegment.FEndsAt := IncMillisecond(IncSecond(LSegment.FStartsAt, LDelta), -1);
          LSegments.Add(LSegment);
        end
        else if LDelta < 0 then
        begin
          { This is a negative bias. This means we have an ambiguous region. }
          LSegment.FType := lttAmbiguous;
          LSegment.FStartsAt := LEnd;
          LSegment.FEndsAt := IncMillisecond(IncSecond(LSegment.FStartsAt, -LDelta), -1);
          LSegments.Add(LSegment);
        end;
      end else
      begin
        { Just a placeholder of "to the end of time". }
        LSegment.FEndsAt := IncMilliSecond(EncodePreciseDate(LRule.FYear + 1, 1, 1), -1);
        LSegments.Add(LSegment);
      end;
    end;

    { Finalize the wortk by clipping the boundaries. }
    LYStart := EncodePreciseDate(AYear, 1, 1);
    LYEnd := IncMilliSecond(EncodePreciseDate(AYear + 1, 1, 1), -1);

    SetLength(Result, LSegments.Count);
    Z := 0;
    for X := 0 to LSegments.Count - 1 do
    begin
      LSegment := LSegments[X];

      if ComparePreciseTime(LSegment.FEndsAt, LYStart) < 0 then
        continue;
      if ComparePreciseTime(LSegment.FStartsAt, LYEnd) > 0 then
        break;
      if ComparePreciseTime(LSegment.FStartsAt, LYStart) < 0 then
        LSegment.FStartsAt := LYStart;
      if ComparePreciseTime(LSegment.FEndsAt, LYEnd) > 0 then
        LSegment.FEndsAt := LYEnd;

      Result[Z] := LSegment;
      Inc(Z);
    end;

    SetLength(Result, Z);
  finally
    LSegments.Free;
  end;
end;

var
{$IFNDEF DELPHI}
  FTimeZoneCacheLock: TCriticalSection;
{$ENDIF}
  FTimeZoneCache: {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<string, TBundledTimeZone>;

{ TYearSegment }

function TYearSegment.GetEndsAt: TDateTime;
begin
  Result := PreciseTimeToDateTime(FEndsAt);
end;

function TYearSegment.GetStartsAt: TDateTime;
begin
  Result := PreciseTimeToDateTime(FStartsAt);
end;

function TYearSegment.GetUtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
  Result := {$IFDEF DELPHI}TTimeSpan.FromSeconds(FPeriodOffset + FBias){$ELSE}FPeriodOffset + FBias{$ENDIF};
end;

{$IFDEF FPC}
class operator TYearSegment.Equal(const ALeft, ARight: TYearSegment): Boolean;
begin
  Result :=
    (ALeft.FStartsAt = ARight.FStartsAt) and
    (ALeft.FEndsAt = ARight.FEndsAt) and
    (ALeft.FType = ARight.FType) and
    (ALeft.FName = ARight.FName) and
    (ALeft.FPeriodOffset = ARight.FPeriodOffset) and
    (ALeft.FBias = ARight.FBias);
end;
{$ENDIF}

{ TBundledTimeZone }

function TBundledTimeZone.AmbiguousTimeEnd(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttAmbiguous, false, LSegment) then
    Result := LSegment.EndsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.AmbiguousTimeStart(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttAmbiguous, true, LSegment) then
    Result := LSegment.StartsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.InvalidTimeEnd(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttInvalid, false, LSegment) then
    Result := LSegment.EndsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.InvalidTimeStart(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttInvalid, true, LSegment) then
    Result := LSegment.StartsAt
  else
    Result := CNullDateTime;
end;

constructor TBundledTimeZone.Create(const ATimeZoneID: string);
var
  LIndex: Integer;
begin
  FSegmentsByYear := {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<Word, TYearSegmentArray>.Create;

{$IFNDEF DELPHI}
  FSegmentsByYearLock := TCriticalSection.Create;
{$ENDIF}

  { First, search in the CZones array }
  for LIndex := Low(CZones) to High(CZones) do
    if SameText(CZones[LIndex].FName, ATimeZoneID) then
    begin
      FZone := @CZones[LIndex];
      break;
    end;

  { Second, search in the aliases array }
  if FZone = nil then
    for LIndex := Low(CAliases) to High(CAliases) do
      if SameText(CAliases[LIndex].FName, ATimeZoneID) then
      begin
        FZone := CAliases[LIndex].FAliasTo;
        break;
      end;

  { Throw exception on error }
  if FZone = nil then
    raise ETimeZoneInvalid.CreateResFmt(@SNoBundledTZForName, [ATimeZoneID]);
end;

function TBundledTimeZone.DaylightTimeEnd(const AYear: Word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttDaylight, true, LSegment) then
    Result := LSegment.EndsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.DaylightTimeStart(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttDaylight, false, LSegment) then
    Result := LSegment.StartsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.ToISO8601Format(const ADateTime: TDateTime): string;
const
  CZFormat = '%.4d-%.2d-%.2d %.2d:%.2d:%.2d.%.3dZ';
  CFullFormat = '%.4d-%.2d-%.2d %.2d:%.2d:%.2d.%.3d%s%.2d:%.2d';
var
  LSegment: TYearSegment;
  LYear, LMonth, LDay, LHours, LMins, LSecs, LMillis: Word;
  LBias, LBiasHours, LBiasMinutes: Int64;
  LBiasSign: Char;
begin
  LSegment := GetSegmentUtc(DateUtils.YearOf(ADateTime), DateTimeToPreciseTime(ADateTime));
  LBias := (LSegment.FPeriodOffset + LSegment.FBias) div SecsPerMin;

  { Decode the local time (as we will include the bias into the repr.) }
  DecodeDateTime(ADateTime, LYear, LMonth, LDay, LHours, LMins, LSecs, LMillis);

  if LBias = 0 then
    Result := Format(CZFormat, [LYear, LMonth, LDay, LHours, LMins, LSecs, LMillis])
  else
  begin
    if (LBias >= 0) then LBiasSign := '+' else LBiasSign := '-';
    LBiasHours := Abs(LBias) div MinsPerHour;
    LBiasMinutes := Abs(LBias) mod MinsPerHour;

    Result := Format(CFullFormat,
      [LYear, LMonth, LDay, LHours, LMins, LSecs, LMillis, LBiasSign, LBiasHours, LBiasMinutes]);
  end;
end;

procedure ForEachYearlySegment(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  if AData <> nil then
    SetLength(TYearSegmentArray(AData), 0);

  AContinue := True;
end;

destructor TBundledTimeZone.Destroy;
begin
  { Free each rule }
  if Assigned(FSegmentsByYear) then
    FSegmentsByYear.Free;
{$IFNDEF DELPHI}
  FSegmentsByYearLock.Free;
{$ENDIF}

  inherited;
end;

function TBundledTimeZone.DoGetID: string;
begin
  { Get the Id of the time zone from the stored var }
  Result := PZone(FZone)^.FName;
end;

function TBundledTimeZone.GetAbbreviation(const ADateTime: TDateTime; const AForceDaylight: Boolean): string;
const
  CGMT = 'GMT';
  CMinus = '-';
  CPlus = '+';
  CSemi = ':';
  CDigitFmt = '%.2d';

  function FmtPart(const APart: Word): string;
  begin
    Result := Format(CDigitFmt, [APart]);
  end;

var
  LSegment: TYearSegment;
  LOffset, LHours, LMinutes, LSeconds: Int64;
begin
  { Get the UTC offset for the given time. }
  LSegment := GetSegment(DateUtils.YearOf(ADateTime), DateTimeToPreciseTime(ADateTime),
    AForceDaylight, false);
  LOffset := LSegment.FPeriodOffset + LSegment.FBias;

  { Start with GMT }
  Result := CGMT;

  { Nothing for zero offset }
  if LOffset = 0 then
    Exit;

  { Calculate the hh:mm:ss parts }
  LSeconds := Abs(LOffset);
  LHours := LSeconds div (SecsPerMin * MinsPerHour);
  Dec(LSeconds, LHours * SecsPerMin * MinsPerHour);
  LMinutes := LSeconds div SecsPerMin; Dec(LSeconds, LMinutes * SecsPerMin);

  { Add the sign }
  if LOffset < 0 then
    Result := Result + CMinus
  else
    Result := Result + CPlus;

  { And now add the remaining pieces }
  Result := Result + FmtPart(LHours);
  if (LMinutes <> 0) or (LSeconds <> 0) then
    Result := Result + CSemi + FmtPart(LMinutes);
  if LSeconds <> 0 then
    Result := Result + CSemi + FmtPart(LSeconds);
end;

function TBundledTimeZone.GetCurrentAbbreviation: string;
begin
  { Call GetAbbreviation for current local time. }
  Result := GetAbbreviation(Now);
end;

function TBundledTimeZone.GetCurrentDisplayName: string;
begin
  { Call GetDisplayName for current local time. }
  Result := GetDisplayName(Now);
end;

function TBundledTimeZone.GetCurrentUtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
  { Call GetUtcOffset for current local time. }
  Result := GetUtcOffset(Now);
end;

function TBundledTimeZone.GetDisplayName(const ADateTime: TDateTime;
  const AForceDaylight: Boolean): string;
begin
  Result := GetSegment(DateUtils.YearOf(ADateTime), DateTimeToPreciseTime(ADateTime),
    AForceDaylight, true).DisplayName;
end;

function TBundledTimeZone.GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;
begin
  Result := GetSegment(DateUtils.YearOf(ADateTime),
    DateTimeToPreciseTime(ADateTime), true, false).LocalType;
end;

function TBundledTimeZone.GetUtcOffset(const ADateTime: TDateTime; const AForceDaylight: Boolean):
  {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
  Result := GetSegment(DateUtils.YearOf(ADateTime), DateTimeToPreciseTime(ADateTime), AForceDaylight, true).UtcOffset;
end;

function TBundledTimeZone.IsAmbiguousTime(const ADateTime: TDateTime): Boolean;
begin
  { Call GetLocalTimeType and check the result for lttInvalid }
  Result := GetLocalTimeType(ADateTime) = lttAmbiguous;
end;

function TBundledTimeZone.IsDaylightTime(const ADateTime: TDateTime; const AForceDaylight: Boolean): Boolean;
var
  LType: TLocalTimeType;
begin
  { Call GetLocalTimeType and store the result }
  LType := GetLocalTimeType(ADateTime);

  { If the type is daylight or ambiguous with forcing set to on. }
  Result := (LType = lttDaylight) or
    ((LType = lttAmbiguous) and AForceDaylight);
end;

function TBundledTimeZone.IsInvalidTime(const ADateTime: TDateTime): Boolean;
begin
  { Call GetLocalTimeType and check the result for lttInvalid }
  Result := GetLocalTimeType(ADateTime) = lttInvalid;
end;

function TBundledTimeZone.IsStandardTime(const ADateTime: TDateTime; const AForceDaylight: Boolean): Boolean;
var
  LType: TLocalTimeType;
begin
  { Call GetLocalTimeType and store the result }
  LType := GetLocalTimeType(ADateTime);

  { If the type is standard or ambiguous with forcing set to off. }
  Result := (LType = lttStandard) or
    ((LType = lttAmbiguous) and not AForceDaylight);
end;

function TBundledTimeZone.ToLocalTime(const ADateTime: TDateTime): TDateTime;
var
  LPreciseTime: TPreciseTime;
  LSegment: TYearSegment;
begin
  LPreciseTime := DateTimeToPreciseTime(ADateTime);
  LSegment := GetSegmentUtc(DateUtils.YearOf(ADateTime), LPreciseTime);

  LPreciseTime := IncSecond(LPreciseTime, LSegment.FPeriodOffset + LSegment.FBias);
  Result := PreciseTimeToDateTime(LPreciseTime);
end;

function TBundledTimeZone.ToUniversalTime(const ADateTime: TDateTime;
  const AForceDaylight: Boolean): TDateTime;
var
  LPreciseTime: TPreciseTime;
  LSegment: TYearSegment;
begin
  LPreciseTime := DateTimeToPreciseTime(ADateTime);
  LSegment := GetSegment(DateUtils.YearOf(ADateTime), LPreciseTime, AForceDaylight, true);

  LPreciseTime := IncSecond(LPreciseTime, -(LSegment.FPeriodOffset + LSegment.FBias));
  Result := PreciseTimeToDateTime(LPreciseTime);
end;

function TBundledTimeZone.GetSegment(
  const AYear: Word; const APreciseTime: TPreciseTime; const AForceDaylight: Boolean;
  const AFailOnInvalid: Boolean): TYearSegment;
var
  LSegments: TYearSegmentArray;
  I: Integer;
begin
  LSegments := GetYearBreakdown(AYear);
  for I := Low(LSegments) to High(LSegments) do
  begin
    if (ComparePreciseTime(LSegments[I].FStartsAt, APreciseTime) <= 0) and
       (ComparePreciseTime(LSegments[I].FEndsAt, APreciseTime) >= 0) then
    begin
      { This segment matches our time }
      if AFailOnInvalid and (LSegments[I].FType = lttInvalid) then
        raise ELocalTimeInvalid.CreateResFmt(@SInvalidLocalTime, [PreciseTimeToStr(APreciseTime)]);

      if not AForceDaylight and (LSegments[I].FType = lttAmbiguous) then
      begin
        { Requiring the next segment as part of the query. }
        if (I < High(LSegments)) and (LSegments[I + 1].FType = lttStandard) then
        begin
          Result := LSegments[I];
          Result.FName := LSegments[I + 1].FName;
          Result.FBias := LSegments[I + 1].FBias;

          Exit;
        end;
      end else
        Exit(LSegments[I]);
    end;
  end;

  { Catch all issue. }
  raise EUnknownTimeZoneYear.CreateResFmt(@SDateTimeNotResolvable,
    [PreciseTimeToStr(APreciseTime), DoGetID()]);
end;

function TBundledTimeZone.GetSegmentUtc(const AYear: Word; const APreciseTime: TPreciseTime): TYearSegment;
var
  LSegment: TYearSegment;
  LLocal: TPreciseTime;
begin
  for LSegment in GetYearBreakdown(AYear) do
  begin

    if LSegment.FType = lttAmbiguous then
    begin
      { Check with both period offset only }
      LLocal := IncSecond(APreciseTime, LSegment.FPeriodOffset);

      if YearOf(LLocal) <> AYear then
      begin
        { Crossed the year threshold. Pass on to next year. }
        Exit(GetSegmentUtc(YearOf(LLocal), APreciseTime));
      end;

      if (ComparePreciseTime(LSegment.FStartsAt, LLocal) <= 0) and
         (ComparePreciseTime(LSegment.FEndsAt, LLocal) >= 0) then
         begin
           { Special case when non-biased Ambiguous found - erase it. }
           Result := LSegment;
           Result.FBias := 0;
           Exit;
         end;
    end;

    if LSegment.FType <> lttInvalid then
    begin
      { Check for normal segments. }
      LLocal := IncSecond(APreciseTime, LSegment.FPeriodOffset + LSegment.FBias);
      if YearOf(LLocal) <> AYear then
      begin
        { Crossed the year threshold. Pass on to next year. }
        Exit(GetSegmentUtc(YearOf(LLocal), APreciseTime));
      end;

      if (ComparePreciseTime(LSegment.FStartsAt, LLocal) <= 0) and
         (ComparePreciseTime(LSegment.FEndsAt, LLocal) >= 0) then
         Exit(LSegment);
    end;

  end;

  { Catch all issue. }
  raise EUnknownTimeZoneYear.CreateResFmt(@SDateTimeNotResolvable,
    [PreciseTimeToStr(APreciseTime), DoGetID()]);
end;

class function TBundledTimeZone.GetTimeZone(const ATimeZoneID: string): TBundledTimeZone;
var
  LOut: TBundledTimeZone;
begin
  { Access the cache }
{$IFDEF DELPHI}
  MonitorEnter(FTimeZoneCache);
{$ELSE}
  FTimeZoneCacheLock.Enter();
{$ENDIF}
  try
    { Check if we know this TZ }
    if not FTimeZoneCache.{$IFNDEF FPC}TryGetValue{$ELSE}TryGetData{$ENDIF}(UpperCase(ATimeZoneID), Result) then
    begin
      Result := TBundledTimeZone.Create(UpperCase(ATimeZoneID));

      { Check if maybe we used an alias and need to change things }
      if FTimeZoneCache.{$IFNDEF FPC}TryGetValue{$ELSE}TryGetData{$ENDIF}(UpperCase(Result.ID), LOut) then
      begin
        Result.Free;
        Result := LOut;
      end else
        FTimeZoneCache.Add(UpperCase(Result.ID), Result);
    end;
  finally
{$IFDEF DELPHI}
  MonitorExit(FTimeZoneCache);
{$ELSE}
  FTimeZoneCacheLock.Leave;
{$ENDIF}
  end;
end;

class function TBundledTimeZone.GetTimezoneFromAlias(const AAliasID: string): string;
begin
  Result := GetTimeZone(AAliasID).ID;
end;

function TBundledTimeZone.GetYearBreakdown(const AYear: Word): TYearSegmentArray;
begin
  { Guard for upper and lower date/time limits }
  if (AYear < 1) or (AYear > 9998) then
    raise EUnknownTimeZoneYear.CreateResFmt(@SYearNotResolvable, [AYear, DoGetID()]);

  Result := nil;

{$IFDEF DELPHI}
  MonitorEnter(FSegmentsByYear);
{$ELSE}
  FSegmentsByYearLock.Enter;
{$ENDIF}
  try
    { Check if we have a cached list of matching rules for this date's year }
    if not FSegmentsByYear.{$IFDEF DELPHI}TryGetValue{$ELSE}TryGetData{$ENDIF}(AYear, Result) then
    begin
      Result := BreakdownYearIntoSegments(FZone, AYear);

      if Length(Result) = 0 then
        raise EUnknownTimeZoneYear.CreateResFmt(@SYearNotResolvable, [AYear, DoGetID()]);

      { Register the new array into the dictionary }
      FSegmentsByYear.Add(AYear, Result);
    end;

  finally
{$IFDEF DELPHI}
    MonitorExit(FSegmentsByYear);
{$ELSE}
    FSegmentsByYearLock.Leave;
{$ENDIF}
  end;
end;

class function TBundledTimeZone.DbVersion: string;
begin
  { This value comes from 'TZDB.inc' }
  Result := CIANAVersion;
end;

class function TBundledTimeZone.KnownAliases: TStringDynArray;
var
  I: Integer;
begin
  { Prepare the output array }
  SetLength(Result, Length(CAliases));

  { Copy the aliases in (if requested) }
  for I := Low(CAliases) to High(CAliases) do
  begin
    Result[I] := CAliases[I].FName;
  end;
end;

class function TBundledTimeZone.KnownTimeZones(const AIncludeAliases: Boolean): TStringDynArray;
var
  I, LIndex: Integer;
begin
  { Prepare the output array }
  if AIncludeAliases then
    SetLength(Result, Length(CZones) + Length(CAliases))
  else
    SetLength(Result, Length(CZones));

  { Copy the zones in }
  LIndex := 0;
  for I := Low(CZones) to High(CZones) do
  begin
    Result[LIndex] := CZones[I].FName;
    Inc(LIndex);
  end;

  { Copy the aliases in (if requested) }
  if AIncludeAliases then
    for I := Low(CAliases) to High(CAliases) do
    begin
      Result[LIndex] := CAliases[I].FName;
      Inc(LIndex);
    end;
end;

function TBundledTimeZone.HasDaylightTime(const AYear: Word): Boolean;
var
  LSegment: TYearSegment;
begin
  Result := TryFindSegment(AYear, lttDaylight, false, LSegment);
end;

function TBundledTimeZone.StandardTimeEnd(const AYear: Word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttStandard, true, LSegment) then
    Result := LSegment.EndsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.StandardTimeStart(const aYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttStandard, false, LSegment) then
    Result := LSegment.StartsAt
  else
    Result := CNullDateTime;
end;

function TBundledTimeZone.TryFindSegment(const AYear: Word; const AType: TLocalTimeType;
   const ARev: Boolean; out ASegment: TYearSegment): Boolean;
var
  LSegments: TYearSegmentArray;
  I: Integer;
begin
  LSegments := GetYearBreakdown(AYear);

  { Invalid case but we'll handle it. }
  if Length(LSegments) = 0 then
    Exit(false);

  { Special case of one segment. }
  if Length(LSegments) = 1 then
  begin
    if LSegments[0].FType = AType then
    begin
      ASegment := LSegments[0];
      Exit(true);
    end;

    Exit(false);
  end;

  { If the type is not the first, just find it. }
  if ARev then
  begin
    for I := Low(LSegments) to High(LSegments) do
    begin
      if LSegments[I].FType = AType then
      begin
        ASegment := LSegments[I];
        Exit(true);
      end;
    end;
  end else
  begin
    for I := High(LSegments) downto Low(LSegments) do
    begin
      if LSegments[I].FType = AType then
      begin
        ASegment := LSegments[I];
        Exit(true);
      end;
    end;
  end;

  { Nothing found. }
  Exit(false);
end;

class function TBundledTimeZone.Version: string;
begin
  Result := CComponentVersion;
end;

procedure FinalizeDict(const ADict: {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<string, TBundledTimeZone>);
{$IFDEF FPC}
var
  I: Integer;
begin
    for I := 0 to ADict.Count - 1 do ADict.Data[I].Free;
{$ELSE}
var
  LTZ: TBundledTimeZone;
begin
  for LTZ in ADict.Values do LTZ.Free;
{$ENDIF}
  FTimeZoneCache.Free;
end;

initialization
{$IFNDEF DELPHI}
  FTimeZoneCacheLock := TCriticalSection.Create;
{$ENDIF}

  FTimeZoneCache := {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<string, TBundledTimeZone>.Create;

finalization
  FinalizeDict(FTimeZoneCache);
{$IFNDEF DELPHI}
  FTimeZoneCacheLock.Free;
{$ENDIF}

end.
