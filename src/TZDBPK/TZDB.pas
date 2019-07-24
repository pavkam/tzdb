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

  /// <summary>Represents a specific date/time segment of the year.</summary>
  /// <remarks>A calendar year in most time zones is divided into standard/ambiguous/daylight/invalid/standard segments.</remarks>
  TYearSegment = record
  private
    FStartsAt, FEndsAt: TDateTime;
    FType: TLocalTimeType;
    FName: string;
    FPeriodOffset, FBias: Int64;

    function GetUtcOffset: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};
  public
    /// <summary>The date/time when the segment starts.</summary>
    /// <returns>A date/time value representing the start of the segment.</returns>
    property StartsAt: TDateTime read FStartsAt;

    /// <summary>The date/time when the segment ends.</summary>
    /// <returns>A date/time value representing the end of the segment.</returns>
    property EndsAt: TDateTime read FEndsAt;

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
    FPeriods: {$IFDEF DELPHI}TObjectList{$ELSE}TFPGObjectList{$ENDIF}<TObject>; { TCompiledPeriod }

{$IFNDEF DELPHI}
    FSegmentsByYearLock: TCriticalSection;
{$ENDIF}
    FSegmentsByYear: {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<Word, TYearSegmentArray>;

    procedure CompilePeriods;
    function CompileYearBreakdown(const AYear: Word): TYearSegmentArray;

    function GetSegment(const ADateTime: TDateTime; const AForceDaylight: Boolean;
      const AFailOnInvalid: Boolean): TYearSegment;
    function GetSegmentUtc(const AYear: Word; const ADateTime: TDateTime): TYearSegment;
    function TryFindSegment(const AYear: Word; const AType: TLocalTimeType; const ARev: Boolean; out ASegment: TYearSegment): Boolean;
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

    ///  <summary>Returns the version of compiled TZDB database.</summary>
    ///  <returns>A string representing the compiled version.</returns>
    class function DbVersion: string; inline;

    ///  <summary>Breaks a given year into components segments.</summary>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function GetYearBreakdown(const AYear: Word): TYearSegmentArray;

    ///  <summary>Get the starting date/time of daylight period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start time of daylight saving period in the local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function DaylightTimeStart(const AYear: Word): TDateTime; inline;

    ///  <summary>Get the starting date/time of standard period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start date/time of standard period in local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function StandardTimeStart(const AYear: Word): TDateTime; inline;

    ///  <summary>Get the starting date/time of invalid period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start date/time of invalid period in local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function InvalidTimeStart(const AYear: word): TDateTime; inline;

    ///  <summary>Get the starting date/time of ambiguous period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The start date/time of ambiguous period in local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function AmbiguousTimeStart(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of daylight saving period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The end date/time of daylight saving period in local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function DaylightTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of standard period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The ending date/time of standard period in local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function StandardTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of invalid period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The end date/time of invalid period in local time.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified year is not in the bundled database.</exception>
    function InvalidTimeEnd(const AYear: word): TDateTime; inline;

    ///  <summary>Get the ending date/time of ambiguous period.</summary>
    ///  <remarks>This function considers the first period of this type and will not work properly for complicated time zones.</remarks>
    ///  <param name="AYear">The year to get data for.</param>
    ///  <returns>The end date/time of ambiguous period in local time.</returns>
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
    function GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType; inline;

    ///  <summary>Checks whether the specified local time is ambiguous.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns><c>True</c> if the local time is ambiguous; <c>False</c> otherwise.</returns>
    ///  <exception cref="TZDB|EUnknownTimeZoneYear">The specified date/time year is not in the bundled database.</exception>
    function IsAmbiguousTime(const ADateTime: TDateTime): Boolean; inline;

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
      {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF}; inline;

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
      const AForceDaylight: Boolean = false): TDateTime; inline;

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
  SDateTimeNotResolvable =
    'The date [%s] cannot be matched to a period in the bundled timezone "%s".';
  SYearNotResolvable =
    'The year [%d] cannot is not in the bundled timezone "%s".';
  SInvalidLocalTime = 'Local date/time value %s is invalid (does not exist in the time zone).';

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

function EncodeDateMonthLastDayOfWeek(const AYear, AMonth, ADayOfWeek: Word): TDateTime;
var
  LDoW: Word;
begin
  { Generate a date that looks like: Year/Month/(Last Day of Month) }
  Result := EncodeDate(AYear, AMonth, DaysInAMonth(AYear, AMonth));

  { Get the day of week for this newly crafted date }
  LDoW := DayOfTheWeek(Result);

  { We're too far off now, let's decrease the number of days till we get to the desired one }
  if LDoW > ADayOfWeek then
    Result := IncDay(Result, -1 * (LDoW - ADayOfWeek))
  else if LDoW < ADayOfWeek then
    Result := IncDay(Result, -1 * (DaysPerWeek - ADayOfWeek + LDoW));
end;

function EncodeDateMonthFirstDayOfWeek(const AYear, AMonth, ADayOfWeek: Word): TDateTime;
var
  LDoW: Word;
begin
  { Generate a date that looks like: Year/Month/1st }
  Result := EncodeDate(AYear, AMonth, 1);

  { Get the day of week for this newly crafted date }
  LDoW := DayOfTheWeek(Result);

  { We're too far off now, let's decrease the number of days till we get to the desired one }
  if LDoW > ADayOfWeek then
    Result := IncDay(Result, DaysPerWeek - LDoW + ADayOfWeek)
  else if (LDoW < ADayOfWeek) Then
    Result := IncDay(Result, ADayOfWeek - LDoW);
end;

function EncodeDateMonthFirstDayOfWeekAfter(const AYear, AMonth, ADayOfWeek, AAfter: Word): TDateTime;
begin
  { Generate a date with the given day of week as first in month }
  Result := EncodeDateMonthFirstDayOfWeek(AYear, AMonth, ADayOfWeek);

  { Iterate until we've surpassed our min requirement }
  while DayOf(Result) < AAfter do
  begin
    Result := IncWeek(Result);

    { Safe-guard! If we've gotten to another month, get back a week and stop. }
    if MonthOf(Result) <> AMonth then
    begin
      Result := IncWeek(Result, -1);
      break;
    end
  end;
end;

function EncodeDateMonthFirstDayOfWeekBefore(const AYear, AMonth, ADayOfWeek, ABefore: Word): TDateTime;
var
  LWeekDayDiff : Integer;
begin
  { Generate a date with ABefore as the Day in AMonth and AYear }
  Result := EncodeDate(AYear, AMonth, ABefore);

  { Adjust Date by difference in DayOfWeek of Date and ADayOfWeek.  If that difference is negative subtract a week. }
  LWeekDayDiff := DayOfTheWeek(Result) - ADayOfWeek;
  if LWeekDayDiff > 0 then
    Result := IncDay(Result, -LWeekDayDiff)
  else if LWeekDayDiff < 0 then
    Result := IncDay(Result, - (LWeekDayDiff + 7));
end;

function RelativeToDateTime(const AYear, AMonth: Word; const ARelativeDay: PRelativeDay; const ATimeOfDay: Int64): TDateTime;
begin
  Result := 0;

  { Special case - if there is no day defined then there is no time also. Exit with only the date part. }
  if ARelativeDay = nil then
    Result := EncodeDate(AYear, AMonth, 1)
  else if ARelativeDay^.FDayType = dtFixed then
    Result := EncodeDate(AYear, AMonth, ARelativeDay^.FFixedDay)
  else if ARelativeDay^.FDayType = dtLastOfMonth then
    Result := EncodeDateMonthLastDayOfWeek(AYear, AMonth, ARelativeDay^.FLastDayOfWeek)
  else if ARelativeDay^.FDayType = dtNthOfMonth then
    Result := EncodeDateMonthFirstDayOfWeekAfter(AYear, AMonth, ARelativeDay^.FNthDayOfWeek, ARelativeDay^.FNthDayIndex)
  else if ARelativeDay^.FDayType = dtPredOfMonth then
    Result := EncodeDateMonthFirstDayOfWeekBefore(AYear, AMonth, ARelativeDay^.FPredDayOfWeek, ARelativeDay^.FPredDayIndex);

  { Attach the time part now }
  Result := IncSecond(Result, ATimeOfDay);
end;

function FormatAbbreviation(const APeriod: PPeriod; const ARule: PRule; const ALocalTimeType: TLocalTimeType): string;
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
  TCompiledPeriod = class;

  { Contains a compiled rule }
  TCompiledRule = class
  strict private
    FStartsOn: TDateTime;

    function GetStartsOn: TDateTime;
    function GetUtcOffset: Int64;
  private
    FPeriod: TCompiledPeriod;
    FRule: PRule;
    FTimeMode: TTimeMode;

    FOffset: Int64;
    FNext, FPrev: TCompiledRule;

  public
    constructor Create(const APeriod: TCompiledPeriod; const ARule: PRule; const AStartsOn: TDateTime;
      const AOffset: Int64; const ATimeMode: TTimeMode);

    property StartsOn: TDateTime read GetStartsOn;
    property UtcOffset: Int64 read GetUtcOffset;
  end;

  TCompiledRuleArray = array of TCompiledRule;

  { Contains a compiled period (easier for lookup) }
  TCompiledPeriod = class
  private
    FPeriod: PPeriod;
    FFrom, FUntil: TDateTime;

    function GetLastRuleForYear(const AYear: Word): PRule;
  public
    { Basic stuffs }
    constructor Create(const APeriod: PPeriod; const AFrom, AUntil: TDateTime);

    function CompileRulesForYear(const AYear: Word): TCompiledRuleArray;
  end;

var
{$IFNDEF DELPHI}
  FTimeZoneCacheLock: TCriticalSection;
{$ENDIF}
  FTimeZoneCache: {$IFDEF DELPHI}TDictionary{$ELSE}TFPGMap{$ENDIF}<string, TBundledTimeZone>;

{$IFDEF FPC}
function CompiledPeriodComparison(ALeft, ARight: Pointer): Integer;
begin
  { Use standard DT comparison operation }
  Result := CompareDateTime(TCompiledPeriod(ALeft).FUntil,
    TCompiledPeriod(ARight).FUntil);
end;

function CompiledRuleComparison(ALeft, ARight: Pointer): Integer;
begin
  { Use standard DT comparison operation }
  Result := CompareDateTime(TCompiledRule(ALeft).StartsOn,
    TCompiledRule(ARight).StartsOn);
end;
{$ENDIF}

{ TCompiledPeriod }

constructor TCompiledPeriod.Create(const APeriod: PPeriod; const AFrom, AUntil: TDateTime);
begin
  FPeriod := APeriod;
  FUntil := AUntil;
  FFrom := AFrom;
end;

function TCompiledPeriod.CompileRulesForYear(const AYear: Word): TCompiledRuleArray;
var
  LCurrRule: PYearBoundRule;
  LLastYearRule: PRule;
  LAbsolute: TDateTime;
  I: Integer;
  LRules: {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TCompiledRule>;
{$IFDEF DELPHI}
  LComparer: IComparer<TCompiledRule>;
{$ENDIF}
begin
  { Initialize the compiled list }
  LRules := {$IFDEF DELPHI}TList{$ELSE}TFPGList{$ENDIF}<TCompiledRule>.Create;

  { Check whether we actually have a fule family attached }
  if FPeriod^.FRuleFamily <> nil then
  begin
    { Let's start with the last active rule from last year }
    LLastYearRule := GetLastRuleForYear(AYear - 1);

    { Add the the last year rule since 1 jan 00:00 this year }
    if LLastYearRule <> nil then
      LRules.Add(TCompiledRule.Create(Self, LLastYearRule, IncSecond(EncodeDate(AYear, 1, 1), -1*(LLastYearRule^.FOffset)),
        LLastYearRule^.FOffset, trStandard));

    { Obtain the first rule in chain }
    LCurrRule := FPeriod^.FRuleFamily^.FFirstRule;

    for I := 0 to FPeriod^.FRuleFamily^.FCount - 1 do
    begin
      { Check we're in the required year }
      if (AYear >= LCurrRule^.FStart) and (AYear <= LCurrRule^.FEnd) then
      begin
        { Obtain the absolute date when the rule activates in this year }
        LAbsolute := RelativeToDateTime(AYear,
            LCurrRule^.FRule^.FInMonth, LCurrRule^.FRule^.FOnDay,
            LCurrRule^.FRule^.FAt);

        { Add the new compiled rule to the list }
        LRules.Add(TCompiledRule.Create(Self, LCurrRule^.FRule, LAbsolute,
            LCurrRule^.FRule^.FOffset, LCurrRule^.FRule^.FAtMode));
      end;

      { Go to next rule }
      Inc(LCurrRule);
    end;

    { Sort the list ascending by the activation date/time }
{$IFDEF FPC}
    LRules.Sort(@CompiledRuleComparison);
{$ELSE}
    LComparer := TComparer<TCompiledRule>.Construct(function(const ALeft, ARight: TCompiledRule): Integer
    begin
        Result := CompareDateTime(ALeft.StartsOn, ARight.StartsOn);
    end);

    LRules.Sort(LComparer);
{$ENDIF}
    { Create a linked list based on offsets and their nexts (will be used on type getting) }
    SetLength(Result, LRules.Count);
    for I := 0 to LRules.Count - 1 do
    begin
      { Set N[I].Next -> N[I + 1] }
      if I < (LRules.Count - 1) then
        TCompiledRule(LRules[I]).FNext := TCompiledRule(LRules[I + 1]);

      { Set N[I].Prev -> N[I - 1] }
      if I > 0 then
        TCompiledRule(LRules[I]).FPrev := TCompiledRule(LRules[I - 1]);

      Result[I] := LRules[I];
    end;
  end;

  LRules.Free;
end;

function TCompiledPeriod.GetLastRuleForYear(const AYear: Word): PRule;
var
  LCurrRule: PYearBoundRule;
  LAbsolute, LBestChoice: TDateTime;
  I: Integer;
begin
  { Default to nothing obviously }
  Result := nil;

  { Check whether we actually have a fule family attached }
  if FPeriod^.FRuleFamily = nil then
    exit;

  { Obtain the first rule in chain }
  LCurrRule := FPeriod^.FRuleFamily^.FFirstRule;
  LBestChoice := 0;

  for I := 0 to FPeriod^.FRuleFamily^.FCount - 1 do
  begin
    { Check we're in the required year }
    if (AYear >= LCurrRule^.FStart) and (AYear <= LCurrRule^.FEnd) then
    begin

      { Obtain the absolute date when the rule activates in this year }
      LAbsolute := RelativeToDateTime(AYear, LCurrRule^.FRule^.FInMonth,
        LCurrRule^.FRule^.FOnDay, LCurrRule^.FRule^.FAt);

      { Select this rule if it's better suited }
      if CompareDateTime(LAbsolute, LBestChoice) >= 0 then
      begin
        LBestChoice := LAbsolute;
        Result := LCurrRule^.FRule;
      end;
    end;

    { Go to next rule }
    Inc(LCurrRule);
  end;
end;

{ TCompiledRule }

constructor TCompiledRule.Create(const APeriod: TCompiledPeriod; const ARule: PRule;
  const AStartsOn: TDateTime; const AOffset: Int64; const ATimeMode: TTimeMode);
begin
  FPeriod := APeriod;
  FRule := ARule;
  FStartsOn := AStartsOn;
  FOffset := AOffset;
  FTimeMode := ATimeMode;
end;

function TCompiledRule.GetStartsOn: TDateTime;
begin
  Result := FStartsOn;
  // Adjust the value based on the specified time mode.
  case FTimeMode of
    trLocal:
      begin
        if (FOffset <> 0) then
          Result := IncSecond(Result, FOffset)
        else if (FPrev <> nil) and (FPrev.FOffset <> 0) then
          Result := IncSecond(Result, (-1*FPrev.FOffset))
        else if (FNext <> nil) and (FNext.FOffset <> 0) then
          Result := IncSecond(Result, (-1*FNext.FOffset))
      end;
    //This value is specified in the currect period's statndard time. Add the rule offset to get to local time.
    trStandard: Result := IncSecond(Result, FOffset);
    //This value is specified in universal time. Add both the standard deviation plus the local time
    trUniversal: Result := IncSecond(Result, FPeriod.FPeriod^.FOffset + FOffset);
  end;
end;

function TCompiledRule.GetUtcOffset: Int64;
begin
  Result := FPeriod.FPeriod^.FOffset + FOffset;
end;

{ TYearSegment }

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
    Result := LSegment.FEndsAt
  else
    Result := 0;
end;

function TBundledTimeZone.AmbiguousTimeStart(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttAmbiguous, true, LSegment) then
    Result := LSegment.FStartsAt
  else
    Result := 0;
end;

function TBundledTimeZone.InvalidTimeEnd(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttInvalid, false, LSegment) then
    Result := LSegment.FEndsAt
  else
    Result := 0;
end;

function TBundledTimeZone.InvalidTimeStart(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttInvalid, true, LSegment) then
    Result := LSegment.FStartsAt
  else
    Result := 0;
end;

procedure TBundledTimeZone.CompilePeriods;
var
  LCompiledPeriod: TCompiledPeriod;
  LCurrentPeriod: PPeriod;
  LStart: TDateTime;
  LAbsolute: TDateTime;
  LRule: PRule;
  I: Integer;
{$IFDEF DELPHI}
  LComparer: IComparer<TObject>;
{$ENDIF}
begin
  LCurrentPeriod := PZone(FZone)^.FFirstPeriod;
  LStart := 0;

  for I := 0 to PZone(FZone)^.FCount - 1 do
  begin
    { Calculate the end date }
    LAbsolute := RelativeToDateTime(LCurrentPeriod^.FUntilYear,
        LCurrentPeriod^.FUntilMonth, LCurrentPeriod^.FUntilDay,
        LCurrentPeriod^.FUntilTime);

    { Set the approperiate values }
    LCompiledPeriod := TCompiledPeriod.Create(LCurrentPeriod, LStart, IncMilliSecond(LAbsolute, -1));

    { Get the last rule defined in the period }
    if LCurrentPeriod^.FUntilDay <> nil then
    begin
      LRule := LCompiledPeriod.GetLastRuleForYear(LCurrentPeriod^.FUntilYear);

      if LRule <> nil then
      begin
        { Adjust the value based on the specified time mode (do nothing for local mode) }
        case LCurrentPeriod^.FUntilTimeMode of
          trStandard:
            { The period uses its standard time. Adjust to it }
            LCompiledPeriod.FUntil := IncSecond(LAbsolute, LRule^.FOffset);

          trUniversal:
            { This value is specified in universal time. Add both the standard deviation plus the local time }
            LCompiledPeriod.FUntil := IncSecond(LAbsolute, LCurrentPeriod^.FOffset + LRule^.FOffset);
        end;
      end;
    end;

    { Put the compiled period to a list }
    FPeriods.Add(LCompiledPeriod);

    { Set the last "until" }
    LStart := IncMillisecond(LCompiledPeriod.FUntil, 1);

    { Move to the next period in the zone }
    Inc(LCurrentPeriod);
  end;

  { Sort the list ascending }
{$IFDEF FPC}
    FPeriods.Sort(@CompiledPeriodComparison);
{$ELSE}
    LComparer := TComparer<TObject>.Construct(function(const ALeft, ARight: TObject): Integer
    begin
        Result := CompareDateTime(TCompiledPeriod(ALeft).FUntil, TCompiledPeriod(ARight).FUntil);
    end);

    FPeriods.Sort(LComparer);
{$ENDIF}
end;

function TBundledTimeZone.CompileYearBreakdown(const AYear: Word): TYearSegmentArray;
var
  I, X: Integer;
  LPeriod: TCompiledPeriod;
  LRules: {$IFDEF DELPHI}TObjectList{$ELSE}TFPGObjectList{$ENDIF}<TCompiledRule>;
  LRule, LNextRule: TCompiledRule;
  LSegment: TYearSegment;
  LPrdStart, LEnd, LYStart, LYEnd: TDateTime;
  LCarryDelta, LDelta: Int64;
  LComp: TCompiledRuleArray;
begin
  Result := nil;
  LCarryDelta := 0;

  LYStart := EncodeDate(AYear, 1, 1);
  LYEnd := IncMillisecond(EncodeDate(AYear + 1, 1, 1), -1);
  LRules := {$IFDEF DELPHI}TObjectList{$ELSE}TFPGObjectList{$ENDIF}<TCompiledRule>.Create(true);

  try
    for I := 0 to FPeriods.Count - 1 do
    begin
      LPeriod := TCompiledPeriod(FPeriods[I]);

      { Make sure we're skipping stuff we don't want. }
      if YearOf(LPeriod.FUntil) < AYear then continue;
      if YearOf(LPeriod.FFrom) > AYear then break;

      { This period is somehow containing the year we're looking for. Normally there would only be one period per year.
        But there are a few zone with two periods; maybe three? }
      LComp := LPeriod.CompileRulesForYear(AYear);

      { Calculate the actual start of the period. }
      LPrdStart := LPeriod.FFrom;
      if (LPrdStart < LYStart) then LPrdStart := LYStart;

      { Copy the rules into the general list. }
      if (Length(LComp) = 0) or (CompareDateTime(LPrdStart, LComp[0].StartsOn) < 0) then
      begin
        { There is a gap between the start of the period and the first rule! Compensate. }
        LRules.Add(TCompiledRule.Create(LPeriod, nil, LPrdStart, 0, trStandard));
      end;

      for X := Low(LComp) to High(LComp) do
        LRules.Add(LComp[X]);
    end;

    for X := 0 to LRules.Count - 1 do
    begin
      { Get current rule and next rule. Both are used to calculate things. }
      LRule := LRules[X];
      if X < LRules.Count - 1 then
        LNextRule := LRules[X + 1]
      else
        LNextRule := nil;

      LSegment.FPeriodOffset := LRule.FPeriod.FPeriod^.FOffset;
      LSegment.FBias := LRule.FOffset;

      if (LNextRule <> nil) and (LNextRule.FPeriod = LRule.FPeriod) then
      begin
        if LNextRule.UtcOffset >= LRule.UtcOffset then
          LSegment.FType := lttStandard
        else
          LSegment.FType := lttDaylight;
      end else
      begin
        if (LCarryDelta <> 0) or ((LRule.FPrev = nil) and (LRule.FNext = nil)) then
          LSegment.FType := lttStandard
        else
          LSegment.FType := lttDaylight;
      end;

      { This is ridiculous but IsStandard might not be the same as this and the rules are ... well, rules. }
      if LRule.FOffset = 0 then
        LSegment.FName := FormatAbbreviation(LRule.FPeriod.FPeriod, LRule.FRule, lttStandard)
      else
        LSegment.FName := FormatAbbreviation(LRule.FPeriod.FPeriod, LRule.FRule, lttDaylight);

      LSegment.FStartsAt := IncSecond(LRule.StartsOn, LCarryDelta);

      { If there is another rule following, calculate the boundary and introduce the invalid/ambiguous regions. }
      if LNextRule <> nil then
      begin
        { Calculate the overall delta between two segments. }
        LDelta := LNextRule.UtcOffset - LRule.UtcOffset;

        { Add the core segment. }
        if LDelta < 0 then
        begin
          LCarryDelta := -LDelta;
          LEnd := LNextRule.StartsOn;
        end else
        begin
          LCarryDelta := 0;
          LEnd := IncSecond(LNextRule.StartsOn, -LDelta);
        end;

        LSegment.FEndsAt := IncMillisecond(LEnd, -1);

        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := LSegment;

        if LDelta > 0 then
        begin
          { This is a positive bias. This means we have an invalid region. }
          LSegment.FType := lttInvalid;
          LSegment.FBias := 0;
          LSegment.FStartsAt := LEnd;
          LSegment.FEndsAt := IncMillisecond(IncSecond(LSegment.FStartsAt, LDelta), -1);

          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := LSegment;
        end
        else if LDelta < 0 then
        begin
          { This is a negative bias. This means we have an ambiguous region. }
          LSegment.FType := lttAmbiguous;
          LSegment.FStartsAt := LEnd;
          LSegment.FEndsAt := IncMillisecond(IncSecond(LSegment.FStartsAt, - LDelta), -1);

          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := LSegment;
        end;
      end else
      begin
        { Just the end of the year -- NOT CORRECT -- needs to take into account the first rule from next year. }
        LSegment.FEndsAt := LYEnd;
        if LSegment.FEndsAt > LRule.FPeriod.FUntil then
          LSegment.FEndsAt := LRule.FPeriod.FUntil;

        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := LSegment;
      end;

    end;
  finally
    LRules.Free;
  end;

  { For the case there is simply no bundled data... }
  if Length(Result) = 0 then
    raise EUnknownTimeZoneYear.CreateResFmt(@SYearNotResolvable, [AYear, DoGetID()]);
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

  { Initialize internals }
  FPeriods := {$IFDEF DELPHI}TObjectList{$ELSE}TFPGObjectList{$ENDIF}<TObject>.Create(true);

  CompilePeriods();
end;

function TBundledTimeZone.DaylightTimeEnd(const AYear: Word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttDaylight, true, LSegment) then
    Result := LSegment.FEndsAt
  else
    Result := 0;
end;

function TBundledTimeZone.DaylightTimeStart(const AYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttDaylight, false, LSegment) then
    Result := LSegment.FStartsAt
  else
    Result := 0;
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
  LSegment := GetSegmentUtc(YearOf(ADateTime), ADateTime);
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
  if Assigned(FPeriods) then
    FPeriods.Free;

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
  LSegment := GetSegment(ADateTime, AForceDaylight, false);
  LOffset := LSegment.FPeriodOffset + LSegment.FBias;

  { Start with GMT }
  Result := CGMT;

  { Nothing for zero offset }
  if LOffset = 0 then
    Exit;

  { Calculate the hh:mm:ss parts }
  LSeconds := Abs(LOffset);
  LHours := LSeconds div (SecsPerMin * MinsPerHour); Dec(LSeconds, LHours * SecsPerMin * MinsPerHour);
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

function TBundledTimeZone.GetDisplayName(const ADateTime: TDateTime; const AForceDaylight: Boolean): string;
begin
  Result := GetSegment(ADateTime, AForceDaylight, true).FName;
end;

function TBundledTimeZone.GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;
begin
  Result := GetSegment(ADateTime, true, false).FType;
end;

function TBundledTimeZone.GetUtcOffset(const ADateTime: TDateTime; const AForceDaylight: Boolean):
  {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
  Result := GetSegment(ADateTime, AForceDaylight, true).UtcOffset;
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
  LSegment: TYearSegment;
begin
  { Get approximate }
  LSegment := GetSegmentUtc(YearOf(ADateTime), ADateTime);
  Result := IncSecond(ADateTime, LSegment.FPeriodOffset + LSegment.FBias);
end;

function TBundledTimeZone.ToUniversalTime(const ADateTime: TDateTime; const AForceDaylight: Boolean): TDateTime;
var
  LSegment: TYearSegment;
begin
  LSegment := GetSegment(ADateTime, AForceDaylight, true);
  Result := IncSecond(ADateTime, -(LSegment.FPeriodOffset + LSegment.FBias));
end;

function TBundledTimeZone.GetSegment(const ADateTime: TDateTime; const AForceDaylight: Boolean; const AFailOnInvalid: Boolean): TYearSegment;
var
  LSegments: TYearSegmentArray;
  I: Integer;
begin
  LSegments := GetYearBreakdown(YearOf(ADateTime));
  for I := Low(LSegments) to High(LSegments) do
  begin
    if (CompareDateTime(LSegments[I].FStartsAt, ADateTime) <= 0) and
       (CompareDateTime(LSegments[I].FEndsAt, ADateTime) >= 0) then
    begin
      { This segment matches our time }
      if AFailOnInvalid and (LSegments[I].FType = lttInvalid) then
        raise ELocalTimeInvalid.CreateResFmt(@SInvalidLocalTime, [DateTimeToStr(ADateTime)]);

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
  raise EUnknownTimeZoneYear.CreateResFmt(@SDateTimeNotResolvable, [DateTimeToStr(ADateTime), DoGetID()]);
end;

function TBundledTimeZone.GetSegmentUtc(const AYear: Word; const ADateTime: TDateTime): TYearSegment;
var
  LSegment: TYearSegment;
  LLocal: TDateTime;
begin
  for LSegment in GetYearBreakdown(AYear) do
  begin

    if LSegment.FType = lttAmbiguous then
    begin
      { Check with both period offset only }
      LLocal := IncSecond(ADateTime, LSegment.FPeriodOffset);

      if YearOf(LLocal) > AYear then
      begin
        { Crossed the year threshold. Pass on to next year. }
        Exit(GetSegmentUtc(YearOf(LLocal), ADateTime));
      end;

      if (CompareDateTime(LSegment.FStartsAt, LLocal) <= 0) and
         (CompareDateTime(LSegment.FEndsAt, LLocal) >= 0) then
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
      LLocal := IncSecond(ADateTime, LSegment.FPeriodOffset + LSegment.FBias);
      if YearOf(LLocal) > AYear then
      begin
        { Crossed the year threshold. Pass on to next year. }
        Exit(GetSegmentUtc(YearOf(LLocal), ADateTime));
      end;

      if (CompareDateTime(LSegment.FStartsAt, LLocal) <= 0) and
         (CompareDateTime(LSegment.FEndsAt, LLocal) >= 0) then
         Exit(LSegment);
    end;

  end;

  { Catch all issue. }
  raise EUnknownTimeZoneYear.CreateResFmt(@SDateTimeNotResolvable, [DateTimeToStr(ADateTime), DoGetID()]);
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
      Result := CompileYearBreakdown(AYear);

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
    Result := LSegment.FEndsAt
  else
    Result := 0;
end;

function TBundledTimeZone.StandardTimeStart(const aYear: word): TDateTime;
var
  LSegment: TYearSegment;
begin
  if TryFindSegment(AYear, lttStandard, false, LSegment) then
    Result := LSegment.FStartsAt
  else
    Result := 0;
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
