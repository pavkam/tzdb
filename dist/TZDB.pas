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

{$IFDEF FPC}
  {$IFDEF VER3} // FPC 3.x
    {$MODE DELPHI}
    {$MODESWITCH UnicodeStrings}
    {$MODESWITCH ADVANCEDRECORDS}
    {$CODEPAGE UTF8}
    {$DEFINE TZDB_SUPPORTED_COMPILER}
    {$WARN 04104 OFF} // Data is ASCII so these string warnings can be safely ignored.
    {$WARN 04105 OFF} // Ditto
  {$ENDIF}
{$ELSE}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF DECLARED(CompilerVersion)}
      {$IF CompilerVersion >= 22} // Delphi XE +
        {$DEFINE TZDB_SUPPORTED_COMPILER}
        {$DEFINE DELPHI}
      {$IFEND}
    {$IFEND}
  {$ENDIF}
{$ENDIF}

{$IFNDEF TZDB_SUPPORTED_COMPILER}
  {$MESSAGE ERROR 'TZDB requires at least Delphi XE or FreePascal 3.0 to compile!'}
{$ENDIF}

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

{ This file is auto-generated. Do not change its contents since it is highly dependant on the consumer unit. }
const
  CIANAVersion = '2019c';

var
  { This array contains the definitions of relative days used later on in the rules. }
  CRelativeDays: array[0 .. 79] of TRelativeDay = (
    (FDayType: dtFixed; FFixedDay: 1),
    (FDayType: dtFixed; FFixedDay: 15),
    (FDayType: dtFixed; FFixedDay: 2),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 1),
    (FDayType: dtFixed; FFixedDay: 23),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 15),
    (FDayType: dtFixed; FFixedDay: 3),
    (FDayType: dtFixed; FFixedDay: 30),
    (FDayType: dtFixed; FFixedDay: 31),
    (FDayType: dtFixed; FFixedDay: 5),
    (FDayType: dtFixed; FFixedDay: 20),
    (FDayType: dtFixed; FFixedDay: 18),
    (FDayType: dtFixed; FFixedDay: 13),
    (FDayType: dtFixed; FFixedDay: 7),
    (FDayType: dtFixed; FFixedDay: 25),
    (FDayType: dtFixed; FFixedDay: 4),
    (FDayType: dtFixed; FFixedDay: 28),
    (FDayType: dtFixed; FFixedDay: 17),
    (FDayType: dtFixed; FFixedDay: 6),
    (FDayType: dtFixed; FFixedDay: 26),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 8),
    (FDayType: dtFixed; FFixedDay: 14),
    (FDayType: dtFixed; FFixedDay: 21),
    (FDayType: dtFixed; FFixedDay: 11),
    (FDayType: dtFixed; FFixedDay: 16),
    (FDayType: dtFixed; FFixedDay: 9),
    (FDayType: dtFixed; FFixedDay: 29),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 11),
    (FDayType: dtFixed; FFixedDay: 27),
    (FDayType: dtFixed; FFixedDay: 19),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 22),
    (FDayType: dtFixed; FFixedDay: 12),
    (FDayType: dtFixed; FFixedDay: 24),
    (FDayType: dtFixed; FFixedDay: 22),
    (FDayType: dtFixed; FFixedDay: 10),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 9),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 2),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 16),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 23),
    (FDayType: dtLastOfMonth; FLastDayOfWeek: 7),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 19),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 24),
    (FDayType: dtLastOfMonth; FLastDayOfWeek: 6),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 21),
    (FDayType: dtFixed; FFixedDay: 8),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 1; FNthDayIndex: 2),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 14),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 1; FNthDayIndex: 15),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 1; FNthDayIndex: 1),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 1),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 4),
    (FDayType: dtLastOfMonth; FLastDayOfWeek: 1),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 5; FNthDayIndex: 1),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 8),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 24),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 25),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 21),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 13),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 12),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 31),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 28),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 18),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 17),
    (FDayType: dtPredOfMonth; FPredDayOfWeek: 5; FPredDayIndex: 1),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 5; FNthDayIndex: 23),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 5; FNthDayIndex: 15),
    (FDayType: dtLastOfMonth; FLastDayOfWeek: 5),
    (FDayType: dtLastOfMonth; FLastDayOfWeek: 4),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 7),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 7),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 4; FNthDayIndex: 8),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 5; FNthDayIndex: 21),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 1; FNthDayIndex: 9),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 1; FNthDayIndex: 24),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 25),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 6; FNthDayIndex: 15),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 5),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 1; FNthDayIndex: 23),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 4; FNthDayIndex: 1),
    (FDayType: dtNthOfMonth; FNthDayOfWeek: 7; FNthDayIndex: 12)
  );

var
  { This array contains the definitions of DST rules. Used by rule families. }
  CRules: array[0 .. 1186] of TRule = (
   {CRules[0]}
    (FInMonth: 12; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[2]}
    (FInMonth: 10; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[3]}
    (FInMonth: 3; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[4]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[5]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[6]}
    (FInMonth: 6; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[7]}
    (FInMonth: 8; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[8]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[9]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[10]}
    (FInMonth: 12; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[11]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[12]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[13]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[14]}
    (FInMonth: 1; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[15]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[16]}
    (FInMonth: 3; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[17]}
    (FInMonth: 10; FOnDay: @CRelativeDays[5]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[18]}
    (FInMonth: 3; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[19]}
    (FInMonth: 12; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[20]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[21]}
    (FInMonth: 3; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[22]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[23]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 39600; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[24]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[25]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[26]}
    (FInMonth: 12; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[27]}
    (FInMonth: 1; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[28]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[29]}
    (FInMonth: 11; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[30]}
    (FInMonth: 3; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[31]}
    (FInMonth: 10; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[32]}
    (FInMonth: 2; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[33]}
    (FInMonth: 2; FOnDay: @CRelativeDays[13]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[34]}
    (FInMonth: 10; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[35]}
    (FInMonth: 1; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[36]}
    (FInMonth: 2; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[37]}
    (FInMonth: 10; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[38]}
    (FInMonth: 2; FOnDay: @CRelativeDays[17]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[39]}
    (FInMonth: 10; FOnDay: @CRelativeDays[10]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[40]}
    (FInMonth: 2; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[41]}
    (FInMonth: 1; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[42]}
    (FInMonth: 10; FOnDay: @CRelativeDays[27]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[43]}
    (FInMonth: 2; FOnDay: @CRelativeDays[5]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[44]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[45]}
    (FInMonth: 2; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[46]}
    (FInMonth: 10; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[47]}
    (FInMonth: 2; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[48]}
    (FInMonth: 2; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[49]}
    (FInMonth: 11; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[50]}
    (FInMonth: 10; FOnDay: @CRelativeDays[29]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[51]}
    (FInMonth: 11; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[52]}
    (FInMonth: 2; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[53]}
    (FInMonth: 2; FOnDay: @CRelativeDays[30]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[54]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[55]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[56]}
    (FInMonth: 11; FOnDay: @CRelativeDays[6]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[57]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[58]}
    (FInMonth: 11; FOnDay: @CRelativeDays[4]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[59]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[60]}
    (FInMonth: 3; FOnDay: @CRelativeDays[21]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[61]}
    (FInMonth: 10; FOnDay: @CRelativeDays[35]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[62]}
    (FInMonth: 3; FOnDay: @CRelativeDays[35]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[63]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[64]}
    (FInMonth: 4; FOnDay: @CRelativeDays[31]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[65]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[66]}
    (FInMonth: 9; FOnDay: @CRelativeDays[28]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[67]}
    (FInMonth: 4; FOnDay: @CRelativeDays[15]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[68]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[69]}
    (FInMonth: 5; FOnDay: @CRelativeDays[36]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[70]}
    (FInMonth: 8; FOnDay: @CRelativeDays[37]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[71]}
    (FInMonth: 4; FOnDay: @CRelativeDays[38]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[72]}
    (FInMonth: 9; FOnDay: @CRelativeDays[36]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[73]}
    (FInMonth: 5; FOnDay: @CRelativeDays[35]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[74]}
    (FInMonth: 8; FOnDay: @CRelativeDays[35]; FAt: 14400; FAtMode: trUniversal; FOffset: 3600; FFmtPart: ''),
   {CRules[75]}
    (FInMonth: 4; FOnDay: @CRelativeDays[36]; FAt: 10800; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[76]}
    (FInMonth: 5; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[77]}
    (FInMonth: 4; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[78]}
    (FInMonth: 11; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[79]}
    (FInMonth: 2; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[80]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[81]}
    (FInMonth: 3; FOnDay: @CRelativeDays[40]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[82]}
    (FInMonth: 1; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[83]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[84]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[85]}
    (FInMonth: 9; FOnDay: @CRelativeDays[35]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[86]}
    (FInMonth: 4; FOnDay: @CRelativeDays[37]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[87]}
    (FInMonth: 4; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[88]}
    (FInMonth: 9; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[89]}
    (FInMonth: 10; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[90]}
    (FInMonth: 10; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[91]}
    (FInMonth: 2; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[92]}
    (FInMonth: 9; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[93]}
    (FInMonth: 4; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[94]}
    (FInMonth: 3; FOnDay: @CRelativeDays[30]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[95]}
    (FInMonth: 1; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[96]}
    (FInMonth: 3; FOnDay: @CRelativeDays[41]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[97]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[98]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[99]}
    (FInMonth: 3; FOnDay: @CRelativeDays[42]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[100]}
    (FInMonth: 10; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[101]}
    (FInMonth: 8; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[102]}
    (FInMonth: 12; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[103]}
    (FInMonth: 3; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[104]}
    (FInMonth: 5; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[105]}
    (FInMonth: 11; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[106]}
    (FInMonth: 1; FOnDay: @CRelativeDays[17]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[107]}
    (FInMonth: 3; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[108]}
    (FInMonth: 4; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[109]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[110]}
    (FInMonth: 5; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[111]}
    (FInMonth: 12; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[112]}
    (FInMonth: 4; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[113]}
    (FInMonth: 6; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[114]}
    (FInMonth: 4; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[115]}
    (FInMonth: 7; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[116]}
    (FInMonth: 1; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trLocal; FOffset: 5400; FFmtPart: ''),
   {CRules[117]}
    (FInMonth: 3; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[118]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[119]}
    (FInMonth: 12; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[120]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[121]}
    (FInMonth: 12; FOnDay: @CRelativeDays[29]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[122]}
    (FInMonth: 12; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[123]}
    (FInMonth: 12; FOnDay: @CRelativeDays[17]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[124]}
    (FInMonth: 4; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[125]}
    (FInMonth: 3; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[126]}
    (FInMonth: 12; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[127]}
    (FInMonth: 2; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[128]}
    (FInMonth: 12; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[129]}
    (FInMonth: 3; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[130]}
    (FInMonth: 10; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[131]}
    (FInMonth: 10; FOnDay: @CRelativeDays[43]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[132]}
    (FInMonth: 10; FOnDay: @CRelativeDays[11]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[133]}
    (FInMonth: 9; FOnDay: @CRelativeDays[29]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[134]}
    (FInMonth: 3; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[135]}
    (FInMonth: 10; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[136]}
    (FInMonth: 3; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[137]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[138]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[139]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[140]}
    (FInMonth: 1; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[141]}
    (FInMonth: 11; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[142]}
    (FInMonth: 2; FOnDay: @CRelativeDays[4]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[143]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 7200; FFmtPart: '+02'),
   {CRules[144]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: '+00'),
   {CRules[145]}
    (FInMonth: 5; FOnDay: @CRelativeDays[22]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[146]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[147]}
    (FInMonth: 4; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[148]}
    (FInMonth: 9; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[149]}
    (FInMonth: 3; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[150]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[151]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[152]}
    (FInMonth: 9; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[153]}
    (FInMonth: 3; FOnDay: @CRelativeDays[16]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[154]}
    (FInMonth: 10; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[155]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[156]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[157]}
    (FInMonth: 3; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[158]}
    (FInMonth: 10; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[159]}
    (FInMonth: 4; FOnDay: @CRelativeDays[37]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[160]}
    (FInMonth: 9; FOnDay: @CRelativeDays[37]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[161]}
    (FInMonth: 4; FOnDay: @CRelativeDays[35]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[162]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[163]}
    (FInMonth: 11; FOnDay: @CRelativeDays[37]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[164]}
    (FInMonth: 2; FOnDay: @CRelativeDays[38]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[165]}
    (FInMonth: 5; FOnDay: @CRelativeDays[36]; FAt: 3600; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'BDST'),
   {CRules[166]}
    (FInMonth: 8; FOnDay: @CRelativeDays[35]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[167]}
    (FInMonth: 4; FOnDay: @CRelativeDays[36]; FAt: 3600; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'BDST'),
   {CRules[168]}
    (FInMonth: 9; FOnDay: @CRelativeDays[37]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[169]}
    (FInMonth: 4; FOnDay: @CRelativeDays[45]; FAt: 3600; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'BDST'),
   {CRules[170]}
    (FInMonth: 7; FOnDay: @CRelativeDays[35]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[171]}
    (FInMonth: 3; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[172]}
    (FInMonth: 4; FOnDay: @CRelativeDays[12]; FAt: 3600; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'BDST'),
   {CRules[173]}
    (FInMonth: 8; FOnDay: @CRelativeDays[34]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[174]}
    (FInMonth: 11; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[175]}
    (FInMonth: 3; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[176]}
    (FInMonth: 10; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[177]}
    (FInMonth: 10; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[178]}
    (FInMonth: 4; FOnDay: @CRelativeDays[46]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[179]}
    (FInMonth: 10; FOnDay: @CRelativeDays[43]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[180]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[181]}
    (FInMonth: 10; FOnDay: @CRelativeDays[38]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[182]}
    (FInMonth: 3; FOnDay: @CRelativeDays[40]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[183]}
    (FInMonth: 2; FOnDay: @CRelativeDays[11]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[184]}
    (FInMonth: 3; FOnDay: @CRelativeDays[37]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[185]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 3600; FFmtPart: 'BST'),
   {CRules[186]}
    (FInMonth: 10; FOnDay: @CRelativeDays[38]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[187]}
    (FInMonth: 10; FOnDay: @CRelativeDays[30]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[188]}
    (FInMonth: 10; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trUniversal; FOffset: -3600; FFmtPart: ''),
   {CRules[189]}
    (FInMonth: 3; FOnDay: @CRelativeDays[37]; FAt: 7200; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[190]}
    (FInMonth: 10; FOnDay: @CRelativeDays[38]; FAt: 7200; FAtMode: trUniversal; FOffset: -3600; FFmtPart: ''),
   {CRules[191]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[192]}
    (FInMonth: 10; FOnDay: @CRelativeDays[38]; FAt: 3600; FAtMode: trUniversal; FOffset: -3600; FFmtPart: ''),
   {CRules[193]}
    (FInMonth: 10; FOnDay: @CRelativeDays[30]; FAt: 3600; FAtMode: trUniversal; FOffset: -3600; FFmtPart: ''),
   {CRules[194]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: -3600; FFmtPart: ''),
   {CRules[195]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trUniversal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[196]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[197]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[198]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[199]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trUniversal; FOffset: 0; FFmtPart: ''),
   {CRules[200]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[201]}
    (FInMonth: 4; FOnDay: @CRelativeDays[47]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[202]}
    (FInMonth: 9; FOnDay: @CRelativeDays[47]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[203]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[204]}
    (FInMonth: 11; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[205]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[206]}
    (FInMonth: 10; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[207]}
    (FInMonth: 4; FOnDay: @CRelativeDays[48]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[208]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[209]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[210]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[211]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[212]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[213]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[214]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[215]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[216]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[217]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[218]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[219]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'MST'),
   {CRules[220]}
    (FInMonth: 12; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'MMT'),
   {CRules[221]}
    (FInMonth: 5; FOnDay: @CRelativeDays[8]; FAt: 79200; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'MDST'),
   {CRules[222]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'MST'),
   {CRules[223]}
    (FInMonth: 5; FOnDay: @CRelativeDays[8]; FAt: 82800; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'MDST'),
   {CRules[224]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trUniversal; FOffset: 3600; FFmtPart: 'MSD'),
   {CRules[225]}
    (FInMonth: 8; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'MSK'),
   {CRules[226]}
    (FInMonth: 2; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'MSD'),
   {CRules[227]}
    (FInMonth: 3; FOnDay: @CRelativeDays[10]; FAt: 82800; FAtMode: trLocal; FOffset: 7200; FFmtPart: '+05'),
   {CRules[228]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'MSD'),
   {CRules[229]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[230]}
    (FInMonth: 6; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[231]}
    (FInMonth: 11; FOnDay: @CRelativeDays[2]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[232]}
    (FInMonth: 4; FOnDay: @CRelativeDays[34]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[233]}
    (FInMonth: 5; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[234]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[235]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[236]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[237]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[238]}
    (FInMonth: 5; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[239]}
    (FInMonth: 5; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[240]}
    (FInMonth: 5; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[241]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[242]}
    (FInMonth: 5; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[243]}
    (FInMonth: 10; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[244]}
    (FInMonth: 4; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[245]}
    (FInMonth: 9; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[246]}
    (FInMonth: 4; FOnDay: @CRelativeDays[11]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[247]}
    (FInMonth: 4; FOnDay: @CRelativeDays[9]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[248]}
    (FInMonth: 9; FOnDay: @CRelativeDays[12]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[249]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[250]}
    (FInMonth: 10; FOnDay: @CRelativeDays[13]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[251]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[252]}
    (FInMonth: 4; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[253]}
    (FInMonth: 4; FOnDay: @CRelativeDays[11]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[254]}
    (FInMonth: 4; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[255]}
    (FInMonth: 9; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[256]}
    (FInMonth: 3; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[257]}
    (FInMonth: 10; FOnDay: @CRelativeDays[49]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[258]}
    (FInMonth: 3; FOnDay: @CRelativeDays[0]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[259]}
    (FInMonth: 2; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[260]}
    (FInMonth: 10; FOnDay: @CRelativeDays[4]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[261]}
    (FInMonth: 3; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[262]}
    (FInMonth: 10; FOnDay: @CRelativeDays[14]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[263]}
    (FInMonth: 3; FOnDay: @CRelativeDays[14]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[264]}
    (FInMonth: 4; FOnDay: @CRelativeDays[22]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[265]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[266]}
    (FInMonth: 4; FOnDay: @CRelativeDays[15]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[267]}
    (FInMonth: 4; FOnDay: @CRelativeDays[17]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[268]}
    (FInMonth: 4; FOnDay: @CRelativeDays[25]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[269]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[270]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[271]}
    (FInMonth: 4; FOnDay: @CRelativeDays[22]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[272]}
    (FInMonth: 4; FOnDay: @CRelativeDays[12]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[273]}
    (FInMonth: 4; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[274]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[275]}
    (FInMonth: 3; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[276]}
    (FInMonth: 4; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[277]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[278]}
    (FInMonth: 4; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[279]}
    (FInMonth: 3; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[280]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[281]}
    (FInMonth: 11; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[282]}
    (FInMonth: 2; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[283]}
    (FInMonth: 9; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[284]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[285]}
    (FInMonth: 5; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[286]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[287]}
    (FInMonth: 4; FOnDay: @CRelativeDays[49]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[288]}
    (FInMonth: 9; FOnDay: @CRelativeDays[26]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[289]}
    (FInMonth: 9; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[290]}
    (FInMonth: 5; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[291]}
    (FInMonth: 4; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[292]}
    (FInMonth: 4; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[293]}
    (FInMonth: 5; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[294]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[295]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[296]}
    (FInMonth: 8; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[297]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[298]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[299]}
    (FInMonth: 5; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[300]}
    (FInMonth: 8; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[301]}
    (FInMonth: 5; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[302]}
    (FInMonth: 8; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[303]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[304]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[305]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[306]}
    (FInMonth: 3; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[307]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[308]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[309]}
    (FInMonth: 10; FOnDay: @CRelativeDays[15]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[310]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[311]}
    (FInMonth: 6; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[312]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[313]}
    (FInMonth: 3; FOnDay: @CRelativeDays[32]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[314]}
    (FInMonth: 3; FOnDay: @CRelativeDays[25]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[315]}
    (FInMonth: 5; FOnDay: @CRelativeDays[19]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[316]}
    (FInMonth: 4; FOnDay: @CRelativeDays[10]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[317]}
    (FInMonth: 4; FOnDay: @CRelativeDays[31]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[318]}
    (FInMonth: 4; FOnDay: @CRelativeDays[11]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[319]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[320]}
    (FInMonth: 4; FOnDay: @CRelativeDays[13]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[321]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[322]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[323]}
    (FInMonth: 3; FOnDay: @CRelativeDays[19]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[324]}
    (FInMonth: 4; FOnDay: @CRelativeDays[1]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[325]}
    (FInMonth: 11; FOnDay: @CRelativeDays[11]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[326]}
    (FInMonth: 5; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[327]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[328]}
    (FInMonth: 3; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[329]}
    (FInMonth: 11; FOnDay: @CRelativeDays[2]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[330]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[331]}
    (FInMonth: 10; FOnDay: @CRelativeDays[15]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[332]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[333]}
    (FInMonth: 10; FOnDay: @CRelativeDays[44]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[334]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[335]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[336]}
    (FInMonth: 3; FOnDay: @CRelativeDays[16]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[337]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[338]}
    (FInMonth: 4; FOnDay: @CRelativeDays[18]; FAt: 10800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[339]}
    (FInMonth: 5; FOnDay: @CRelativeDays[23]; FAt: 7200; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'M'),
   {CRules[340]}
    (FInMonth: 6; FOnDay: @CRelativeDays[26]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[341]}
    (FInMonth: 4; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[342]}
    (FInMonth: 5; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[343]}
    (FInMonth: 9; FOnDay: @CRelativeDays[32]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[344]}
    (FInMonth: 11; FOnDay: @CRelativeDays[11]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[345]}
    (FInMonth: 7; FOnDay: @CRelativeDays[13]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[346]}
    (FInMonth: 4; FOnDay: @CRelativeDays[13]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[347]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[348]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[349]}
    (FInMonth: 11; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[350]}
    (FInMonth: 4; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[351]}
    (FInMonth: 11; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[352]}
    (FInMonth: 4; FOnDay: @CRelativeDays[23]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[353]}
    (FInMonth: 10; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[354]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[355]}
    (FInMonth: 9; FOnDay: @CRelativeDays[32]; FAt: 14400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[356]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 32400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[357]}
    (FInMonth: 9; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[358]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[359]}
    (FInMonth: 4; FOnDay: @CRelativeDays[1]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[360]}
    (FInMonth: 11; FOnDay: @CRelativeDays[32]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[361]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[362]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[363]}
    (FInMonth: 4; FOnDay: @CRelativeDays[50]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[364]}
    (FInMonth: 4; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[365]}
    (FInMonth: 10; FOnDay: @CRelativeDays[4]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[366]}
    (FInMonth: 5; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[367]}
    (FInMonth: 6; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[368]}
    (FInMonth: 6; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[369]}
    (FInMonth: 4; FOnDay: @CRelativeDays[18]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[370]}
    (FInMonth: 2; FOnDay: @CRelativeDays[29]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[371]}
    (FInMonth: 10; FOnDay: @CRelativeDays[22]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[372]}
    (FInMonth: 11; FOnDay: @CRelativeDays[24]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[373]}
    (FInMonth: 3; FOnDay: @CRelativeDays[29]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[374]}
    (FInMonth: 6; FOnDay: @CRelativeDays[4]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[375]}
    (FInMonth: 4; FOnDay: @CRelativeDays[26]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[376]}
    (FInMonth: 10; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[377]}
    (FInMonth: 2; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[378]}
    (FInMonth: 11; FOnDay: @CRelativeDays[36]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[379]}
    (FInMonth: 3; FOnDay: @CRelativeDays[36]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[380]}
    (FInMonth: 3; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[381]}
    (FInMonth: 10; FOnDay: @CRelativeDays[30]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[382]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[383]}
    (FInMonth: 10; FOnDay: @CRelativeDays[7]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[384]}
    (FInMonth: 10; FOnDay: @CRelativeDays[26]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[385]}
    (FInMonth: 6; FOnDay: @CRelativeDays[6]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[386]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[387]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[388]}
    (FInMonth: 3; FOnDay: @CRelativeDays[25]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[389]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[390]}
    (FInMonth: 3; FOnDay: @CRelativeDays[0]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[391]}
    (FInMonth: 10; FOnDay: @CRelativeDays[15]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[392]}
    (FInMonth: 3; FOnDay: @CRelativeDays[10]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[393]}
    (FInMonth: 9; FOnDay: @CRelativeDays[11]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[394]}
    (FInMonth: 6; FOnDay: @CRelativeDays[21]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[395]}
    (FInMonth: 9; FOnDay: @CRelativeDays[1]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[396]}
    (FInMonth: 3; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[397]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[398]}
    (FInMonth: 3; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[399]}
    (FInMonth: 10; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[400]}
    (FInMonth: 2; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[401]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[402]}
    (FInMonth: 5; FOnDay: @CRelativeDays[30]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[403]}
    (FInMonth: 9; FOnDay: @CRelativeDays[32]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[404]}
    (FInMonth: 9; FOnDay: @CRelativeDays[30]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[405]}
    (FInMonth: 6; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[406]}
    (FInMonth: 5; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[407]}
    (FInMonth: 6; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[408]}
    (FInMonth: 5; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[409]}
    (FInMonth: 5; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[410]}
    (FInMonth: 4; FOnDay: @CRelativeDays[16]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[411]}
    (FInMonth: 9; FOnDay: @CRelativeDays[17]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[412]}
    (FInMonth: 10; FOnDay: @CRelativeDays[9]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[413]}
    (FInMonth: 10; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[414]}
    (FInMonth: 10; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[415]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[416]}
    (FInMonth: 4; FOnDay: @CRelativeDays[9]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[417]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[418]}
    (FInMonth: 9; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[419]}
    (FInMonth: 4; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[420]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[421]}
    (FInMonth: 9; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[422]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[423]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[424]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'AMT'),
   {CRules[425]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[426]}
    (FInMonth: 9; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'AMT'),
   {CRules[427]}
    (FInMonth: 4; FOnDay: @CRelativeDays[48]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[428]}
    (FInMonth: 9; FOnDay: @CRelativeDays[51]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'AMT'),
   {CRules[429]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[430]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'AMT'),
   {CRules[431]}
    (FInMonth: 6; FOnDay: @CRelativeDays[52]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[432]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[433]}
    (FInMonth: 5; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'NST'),
   {CRules[434]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[435]}
    (FInMonth: 5; FOnDay: @CRelativeDays[33]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[436]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[437]}
    (FInMonth: 4; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[438]}
    (FInMonth: 4; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[439]}
    (FInMonth: 4; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[440]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[441]}
    (FInMonth: 6; FOnDay: @CRelativeDays[2]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[442]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[443]}
    (FInMonth: 5; FOnDay: @CRelativeDays[8]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[444]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[445]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[446]}
    (FInMonth: 5; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[447]}
    (FInMonth: 6; FOnDay: @CRelativeDays[17]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[448]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[449]}
    (FInMonth: 2; FOnDay: @CRelativeDays[16]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[450]}
    (FInMonth: 10; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[451]}
    (FInMonth: 2; FOnDay: @CRelativeDays[26]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[452]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[453]}
    (FInMonth: 2; FOnDay: @CRelativeDays[32]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[454]}
    (FInMonth: 10; FOnDay: @CRelativeDays[9]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[455]}
    (FInMonth: 3; FOnDay: @CRelativeDays[53]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[456]}
    (FInMonth: 4; FOnDay: @CRelativeDays[14]; FAt: 79200; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'M'),
   {CRules[457]}
    (FInMonth: 8; FOnDay: @CRelativeDays[1]; FAt: 79200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[458]}
    (FInMonth: 10; FOnDay: @CRelativeDays[54]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[459]}
    (FInMonth: 4; FOnDay: @CRelativeDays[17]; FAt: 79200; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'M'),
   {CRules[460]}
    (FInMonth: 8; FOnDay: @CRelativeDays[55]; FAt: 79200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[461]}
    (FInMonth: 4; FOnDay: @CRelativeDays[56]; FAt: 79200; FAtMode: trStandard; FOffset: 7200; FFmtPart: 'M'),
   {CRules[462]}
    (FInMonth: 3; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[463]}
    (FInMonth: 9; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[464]}
    (FInMonth: 5; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[465]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[466]}
    (FInMonth: 4; FOnDay: @CRelativeDays[36]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[467]}
    (FInMonth: 5; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[468]}
    (FInMonth: 4; FOnDay: @CRelativeDays[18]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[469]}
    (FInMonth: 10; FOnDay: @CRelativeDays[49]; FAt: 86400; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[470]}
    (FInMonth: 4; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[471]}
    (FInMonth: 6; FOnDay: @CRelativeDays[24]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[472]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 86400; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[473]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'M'),
   {CRules[474]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[475]}
    (FInMonth: 10; FOnDay: @CRelativeDays[13]; FAt: 86400; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[476]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[477]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[478]}
    (FInMonth: 4; FOnDay: @CRelativeDays[57]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[479]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[480]}
    (FInMonth: 4; FOnDay: @CRelativeDays[58]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[481]}
    (FInMonth: 3; FOnDay: @CRelativeDays[28]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[482]}
    (FInMonth: 6; FOnDay: @CRelativeDays[6]; FAt: 43200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[483]}
    (FInMonth: 6; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[484]}
    (FInMonth: 8; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[485]}
    (FInMonth: 5; FOnDay: @CRelativeDays[48]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[486]}
    (FInMonth: 10; FOnDay: @CRelativeDays[48]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[487]}
    (FInMonth: 3; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[488]}
    (FInMonth: 10; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[489]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[490]}
    (FInMonth: 3; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[491]}
    (FInMonth: 10; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[492]}
    (FInMonth: 5; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[493]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[494]}
    (FInMonth: 12; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[495]}
    (FInMonth: 9; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[496]}
    (FInMonth: 4; FOnDay: @CRelativeDays[37]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[497]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[498]}
    (FInMonth: 4; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[499]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[500]}
    (FInMonth: 4; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[501]}
    (FInMonth: 7; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[502]}
    (FInMonth: 10; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[503]}
    (FInMonth: 6; FOnDay: @CRelativeDays[6]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[504]}
    (FInMonth: 10; FOnDay: @CRelativeDays[59]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[505]}
    (FInMonth: 3; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[506]}
    (FInMonth: 3; FOnDay: @CRelativeDays[22]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[507]}
    (FInMonth: 10; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[508]}
    (FInMonth: 6; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[509]}
    (FInMonth: 7; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[510]}
    (FInMonth: 4; FOnDay: @CRelativeDays[10]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[511]}
    (FInMonth: 9; FOnDay: @CRelativeDays[16]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[512]}
    (FInMonth: 3; FOnDay: @CRelativeDays[10]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[513]}
    (FInMonth: 6; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 2400; FFmtPart: '-0020'),
   {CRules[514]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: '-01'),
   {CRules[515]}
    (FInMonth: 6; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: '+01'),
   {CRules[516]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'GMT'),
   {CRules[517]}
    (FInMonth: 3; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[518]}
    (FInMonth: 4; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[519]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 9000; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[520]}
    (FInMonth: 10; FOnDay: @CRelativeDays[14]; FAt: 9000; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[521]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 9000; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[522]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 9000; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[523]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[524]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[525]}
    (FInMonth: 5; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[526]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 9000; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[527]}
    (FInMonth: 5; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[528]}
    (FInMonth: 4; FOnDay: @CRelativeDays[42]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[529]}
    (FInMonth: 9; FOnDay: @CRelativeDays[42]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[530]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[531]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[532]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[533]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[534]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[535]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 14400; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[536]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 18000; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[537]}
    (FInMonth: 6; FOnDay: @CRelativeDays[29]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[538]}
    (FInMonth: 12; FOnDay: @CRelativeDays[8]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[539]}
    (FInMonth: 6; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[540]}
    (FInMonth: 10; FOnDay: @CRelativeDays[31]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[541]}
    (FInMonth: 3; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[542]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[543]}
    (FInMonth: 1; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[544]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[545]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[546]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[547]}
    (FInMonth: 4; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[548]}
    (FInMonth: 10; FOnDay: @CRelativeDays[8]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[549]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[550]}
    (FInMonth: 5; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[551]}
    (FInMonth: 9; FOnDay: @CRelativeDays[27]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[552]}
    (FInMonth: 4; FOnDay: @CRelativeDays[27]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[553]}
    (FInMonth: 12; FOnDay: @CRelativeDays[0]; FAt: 12600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[554]}
    (FInMonth: 4; FOnDay: @CRelativeDays[12]; FAt: 12600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[555]}
    (FInMonth: 11; FOnDay: @CRelativeDays[7]; FAt: 12600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[556]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 12600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[557]}
    (FInMonth: 10; FOnDay: @CRelativeDays[60]; FAt: 12600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[558]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[559]}
    (FInMonth: 10; FOnDay: @CRelativeDays[59]; FAt: 12600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[560]}
    (FInMonth: 3; FOnDay: @CRelativeDays[61]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[561]}
    (FInMonth: 4; FOnDay: @CRelativeDays[37]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[562]}
    (FInMonth: 10; FOnDay: @CRelativeDays[37]; FAt: 12600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[563]}
    (FInMonth: 12; FOnDay: @CRelativeDays[7]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[564]}
    (FInMonth: 5; FOnDay: @CRelativeDays[12]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[565]}
    (FInMonth: 10; FOnDay: @CRelativeDays[22]; FAt: 12600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[566]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[567]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[568]}
    (FInMonth: 3; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[569]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[570]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[571]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[572]}
    (FInMonth: 11; FOnDay: @CRelativeDays[17]; FAt: 82800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[573]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[574]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[575]}
    (FInMonth: 4; FOnDay: @CRelativeDays[29]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[576]}
    (FInMonth: 11; FOnDay: @CRelativeDays[7]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[577]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[578]}
    (FInMonth: 10; FOnDay: @CRelativeDays[8]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[579]}
    (FInMonth: 4; FOnDay: @CRelativeDays[49]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[580]}
    (FInMonth: 10; FOnDay: @CRelativeDays[42]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[581]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[582]}
    (FInMonth: 10; FOnDay: @CRelativeDays[16]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[583]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[584]}
    (FInMonth: 3; FOnDay: @CRelativeDays[62]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[585]}
    (FInMonth: 11; FOnDay: @CRelativeDays[9]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[586]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 12600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[587]}
    (FInMonth: 3; FOnDay: @CRelativeDays[61]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[588]}
    (FInMonth: 4; FOnDay: @CRelativeDays[37]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[589]}
    (FInMonth: 10; FOnDay: @CRelativeDays[37]; FAt: 9000; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[590]}
    (FInMonth: 10; FOnDay: @CRelativeDays[37]; FAt: 12600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[591]}
    (FInMonth: 12; FOnDay: @CRelativeDays[7]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[592]}
    (FInMonth: 5; FOnDay: @CRelativeDays[12]; FAt: 12600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[593]}
    (FInMonth: 4; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[594]}
    (FInMonth: 10; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[595]}
    (FInMonth: 10; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[596]}
    (FInMonth: 3; FOnDay: @CRelativeDays[10]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[597]}
    (FInMonth: 10; FOnDay: @CRelativeDays[10]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[598]}
    (FInMonth: 9; FOnDay: @CRelativeDays[33]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[599]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[600]}
    (FInMonth: 3; FOnDay: @CRelativeDays[22]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[601]}
    (FInMonth: 9; FOnDay: @CRelativeDays[22]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[602]}
    (FInMonth: 9; FOnDay: @CRelativeDays[10]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[603]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[604]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[605]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[606]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 10800; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[607]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 10800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[608]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[609]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[610]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[611]}
    (FInMonth: 4; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[612]}
    (FInMonth: 5; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'DD'),
   {CRules[613]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[614]}
    (FInMonth: 9; FOnDay: @CRelativeDays[1]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[615]}
    (FInMonth: 11; FOnDay: @CRelativeDays[23]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[616]}
    (FInMonth: 4; FOnDay: @CRelativeDays[10]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[617]}
    (FInMonth: 10; FOnDay: @CRelativeDays[29]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[618]}
    (FInMonth: 4; FOnDay: @CRelativeDays[31]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[619]}
    (FInMonth: 9; FOnDay: @CRelativeDays[12]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[620]}
    (FInMonth: 6; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[621]}
    (FInMonth: 9; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[622]}
    (FInMonth: 6; FOnDay: @CRelativeDays[23]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[623]}
    (FInMonth: 9; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[624]}
    (FInMonth: 6; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[625]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[626]}
    (FInMonth: 4; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[627]}
    (FInMonth: 9; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[628]}
    (FInMonth: 7; FOnDay: @CRelativeDays[13]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[629]}
    (FInMonth: 10; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[630]}
    (FInMonth: 4; FOnDay: @CRelativeDays[10]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[631]}
    (FInMonth: 8; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[632]}
    (FInMonth: 8; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[633]}
    (FInMonth: 9; FOnDay: @CRelativeDays[12]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[634]}
    (FInMonth: 5; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[635]}
    (FInMonth: 8; FOnDay: @CRelativeDays[14]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[636]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[637]}
    (FInMonth: 9; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[638]}
    (FInMonth: 5; FOnDay: @CRelativeDays[11]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[639]}
    (FInMonth: 9; FOnDay: @CRelativeDays[13]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[640]}
    (FInMonth: 9; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[641]}
    (FInMonth: 4; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[642]}
    (FInMonth: 9; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[643]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[644]}
    (FInMonth: 9; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[645]}
    (FInMonth: 3; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[646]}
    (FInMonth: 8; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[647]}
    (FInMonth: 3; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[648]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[649]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[650]}
    (FInMonth: 9; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[651]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[652]}
    (FInMonth: 9; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[653]}
    (FInMonth: 8; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[654]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[655]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[656]}
    (FInMonth: 3; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[657]}
    (FInMonth: 9; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[658]}
    (FInMonth: 3; FOnDay: @CRelativeDays[10]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[659]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[660]}
    (FInMonth: 9; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[661]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[662]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[663]}
    (FInMonth: 4; FOnDay: @CRelativeDays[25]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[664]}
    (FInMonth: 9; FOnDay: @CRelativeDays[32]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[665]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[666]}
    (FInMonth: 10; FOnDay: @CRelativeDays[13]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[667]}
    (FInMonth: 3; FOnDay: @CRelativeDays[16]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[668]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[669]}
    (FInMonth: 4; FOnDay: @CRelativeDays[13]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[670]}
    (FInMonth: 9; FOnDay: @CRelativeDays[33]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[671]}
    (FInMonth: 4; FOnDay: @CRelativeDays[63]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[672]}
    (FInMonth: 10; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[673]}
    (FInMonth: 10; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[674]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[675]}
    (FInMonth: 10; FOnDay: @CRelativeDays[9]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[676]}
    (FInMonth: 9; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[677]}
    (FInMonth: 9; FOnDay: @CRelativeDays[31]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[678]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[679]}
    (FInMonth: 9; FOnDay: @CRelativeDays[4]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[680]}
    (FInMonth: 3; FOnDay: @CRelativeDays[64]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[681]}
    (FInMonth: 5; FOnDay: @CRelativeDays[49]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[682]}
    (FInMonth: 9; FOnDay: @CRelativeDays[53]; FAt: 90000; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[683]}
    (FInMonth: 4; FOnDay: @CRelativeDays[49]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[684]}
    (FInMonth: 6; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[685]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[686]}
    (FInMonth: 4; FOnDay: @CRelativeDays[52]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[687]}
    (FInMonth: 10; FOnDay: @CRelativeDays[52]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[688]}
    (FInMonth: 4; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[689]}
    (FInMonth: 4; FOnDay: @CRelativeDays[17]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[690]}
    (FInMonth: 9; FOnDay: @CRelativeDays[65]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[691]}
    (FInMonth: 9; FOnDay: @CRelativeDays[66]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[692]}
    (FInMonth: 3; FOnDay: @CRelativeDays[67]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[693]}
    (FInMonth: 3; FOnDay: @CRelativeDays[67]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[694]}
    (FInMonth: 10; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[695]}
    (FInMonth: 10; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[696]}
    (FInMonth: 10; FOnDay: @CRelativeDays[66]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[697]}
    (FInMonth: 12; FOnDay: @CRelativeDays[10]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[698]}
    (FInMonth: 4; FOnDay: @CRelativeDays[68]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[699]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 9000; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[700]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 9000; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[701]}
    (FInMonth: 9; FOnDay: @CRelativeDays[31]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[702]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[703]}
    (FInMonth: 9; FOnDay: @CRelativeDays[69]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[704]}
    (FInMonth: 5; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[705]}
    (FInMonth: 9; FOnDay: @CRelativeDays[44]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[706]}
    (FInMonth: 5; FOnDay: @CRelativeDays[10]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[707]}
    (FInMonth: 9; FOnDay: @CRelativeDays[26]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[708]}
    (FInMonth: 5; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[709]}
    (FInMonth: 9; FOnDay: @CRelativeDays[62]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[710]}
    (FInMonth: 5; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[711]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[712]}
    (FInMonth: 6; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[713]}
    (FInMonth: 10; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[714]}
    (FInMonth: 5; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[715]}
    (FInMonth: 9; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 1200; FFmtPart: ''),
   {CRules[716]}
    (FInMonth: 12; FOnDay: @CRelativeDays[21]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[717]}
    (FInMonth: 4; FOnDay: @CRelativeDays[42]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[718]}
    (FInMonth: 9; FOnDay: @CRelativeDays[42]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[719]}
    (FInMonth: 3; FOnDay: @CRelativeDays[42]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[720]}
    (FInMonth: 9; FOnDay: @CRelativeDays[42]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[721]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[722]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[723]}
    (FInMonth: 4; FOnDay: @CRelativeDays[65]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[724]}
    (FInMonth: 10; FOnDay: @CRelativeDays[65]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[725]}
    (FInMonth: 9; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[726]}
    (FInMonth: 9; FOnDay: @CRelativeDays[70]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[727]}
    (FInMonth: 3; FOnDay: @CRelativeDays[66]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[728]}
    (FInMonth: 9; FOnDay: @CRelativeDays[52]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[729]}
    (FInMonth: 8; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[730]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 60; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[731]}
    (FInMonth: 8; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[732]}
    (FInMonth: 9; FOnDay: @CRelativeDays[22]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[733]}
    (FInMonth: 9; FOnDay: @CRelativeDays[71]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[734]}
    (FInMonth: 10; FOnDay: @CRelativeDays[71]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[735]}
    (FInMonth: 3; FOnDay: @CRelativeDays[66]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[736]}
    (FInMonth: 3; FOnDay: @CRelativeDays[54]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[737]}
    (FInMonth: 10; FOnDay: @CRelativeDays[42]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[738]}
    (FInMonth: 11; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[739]}
    (FInMonth: 2; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[740]}
    (FInMonth: 4; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[741]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[742]}
    (FInMonth: 3; FOnDay: @CRelativeDays[33]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[743]}
    (FInMonth: 9; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[744]}
    (FInMonth: 4; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[745]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[746]}
    (FInMonth: 4; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[747]}
    (FInMonth: 2; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[748]}
    (FInMonth: 10; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[749]}
    (FInMonth: 3; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[750]}
    (FInMonth: 10; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[751]}
    (FInMonth: 3; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[752]}
    (FInMonth: 4; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[753]}
    (FInMonth: 3; FOnDay: @CRelativeDays[51]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[754]}
    (FInMonth: 11; FOnDay: @CRelativeDays[52]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[755]}
    (FInMonth: 2; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'W'),
   {CRules[756]}
    (FInMonth: 8; FOnDay: @CRelativeDays[21]; FAt: 82800; FAtMode: trUniversal; FOffset: 3600; FFmtPart: 'P'),
   {CRules[757]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[758]}
    (FInMonth: 2; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[759]}
    (FInMonth: 6; FOnDay: @CRelativeDays[12]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[760]}
    (FInMonth: 5; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[761]}
    (FInMonth: 3; FOnDay: @CRelativeDays[21]; FAt: 7260; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[762]}
    (FInMonth: 1; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[763]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[764]}
    (FInMonth: 6; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[765]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[766]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 60; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[767]}
    (FInMonth: 6; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[768]}
    (FInMonth: 10; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[769]}
    (FInMonth: 4; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[770]}
    (FInMonth: 9; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[771]}
    (FInMonth: 5; FOnDay: @CRelativeDays[9]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[772]}
    (FInMonth: 8; FOnDay: @CRelativeDays[31]; FAt: 82800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[773]}
    (FInMonth: 5; FOnDay: @CRelativeDays[3]; FAt: 82800; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[774]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 82800; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[775]}
    (FInMonth: 5; FOnDay: @CRelativeDays[72]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[776]}
    (FInMonth: 10; FOnDay: @CRelativeDays[45]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[777]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[778]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 60; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[779]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 60; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[780]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 60; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'DD'),
   {CRules[781]}
    (FInMonth: 3; FOnDay: @CRelativeDays[20]; FAt: 60; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[782]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 60; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[783]}
    (FInMonth: 5; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[784]}
    (FInMonth: 8; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[785]}
    (FInMonth: 9; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[786]}
    (FInMonth: 5; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[787]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[788]}
    (FInMonth: 5; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[789]}
    (FInMonth: 9; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[790]}
    (FInMonth: 9; FOnDay: @CRelativeDays[73]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[791]}
    (FInMonth: 10; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[792]}
    (FInMonth: 6; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[793]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[794]}
    (FInMonth: 5; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[795]}
    (FInMonth: 6; FOnDay: @CRelativeDays[20]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[796]}
    (FInMonth: 9; FOnDay: @CRelativeDays[20]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[797]}
    (FInMonth: 6; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[798]}
    (FInMonth: 9; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[799]}
    (FInMonth: 5; FOnDay: @CRelativeDays[28]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[800]}
    (FInMonth: 9; FOnDay: @CRelativeDays[56]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[801]}
    (FInMonth: 5; FOnDay: @CRelativeDays[29]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[802]}
    (FInMonth: 5; FOnDay: @CRelativeDays[15]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[803]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 84600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[804]}
    (FInMonth: 10; FOnDay: @CRelativeDays[19]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[805]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[806]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[807]}
    (FInMonth: 9; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[808]}
    (FInMonth: 9; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[809]}
    (FInMonth: 9; FOnDay: @CRelativeDays[74]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[810]}
    (FInMonth: 4; FOnDay: @CRelativeDays[74]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[811]}
    (FInMonth: 11; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[812]}
    (FInMonth: 4; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[813]}
    (FInMonth: 9; FOnDay: @CRelativeDays[17]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[814]}
    (FInMonth: 5; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[815]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[816]}
    (FInMonth: 5; FOnDay: @CRelativeDays[31]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[817]}
    (FInMonth: 10; FOnDay: @CRelativeDays[12]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[818]}
    (FInMonth: 9; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[819]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[820]}
    (FInMonth: 4; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[821]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[822]}
    (FInMonth: 4; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[823]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[824]}
    (FInMonth: 5; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[825]}
    (FInMonth: 9; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[826]}
    (FInMonth: 5; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[827]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 7200; FFmtPart: 'DD'),
   {CRules[828]}
    (FInMonth: 2; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[829]}
    (FInMonth: 6; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[830]}
    (FInMonth: 12; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[831]}
    (FInMonth: 4; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[832]}
    (FInMonth: 12; FOnDay: @CRelativeDays[24]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'W'),
   {CRules[833]}
    (FInMonth: 5; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[834]}
    (FInMonth: 2; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[835]}
    (FInMonth: 7; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[836]}
    (FInMonth: 6; FOnDay: @CRelativeDays[31]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[837]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[838]}
    (FInMonth: 4; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[839]}
    (FInMonth: 9; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[840]}
    (FInMonth: 10; FOnDay: @CRelativeDays[36]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: '-0530'),
   {CRules[841]}
    (FInMonth: 2; FOnDay: @CRelativeDays[35]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'CST'),
   {CRules[842]}
    (FInMonth: 12; FOnDay: @CRelativeDays[9]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'CDT'),
   {CRules[843]}
    (FInMonth: 2; FOnDay: @CRelativeDays[25]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'CST'),
   {CRules[844]}
    (FInMonth: 12; FOnDay: @CRelativeDays[11]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'CDT'),
   {CRules[845]}
    (FInMonth: 2; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'CST'),
   {CRules[846]}
    (FInMonth: 2; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[847]}
    (FInMonth: 6; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[848]}
    (FInMonth: 1; FOnDay: @CRelativeDays[75]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[849]}
    (FInMonth: 3; FOnDay: @CRelativeDays[1]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[850]}
    (FInMonth: 6; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[851]}
    (FInMonth: 10; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[852]}
    (FInMonth: 6; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[853]}
    (FInMonth: 9; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[854]}
    (FInMonth: 5; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[855]}
    (FInMonth: 4; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[856]}
    (FInMonth: 9; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[857]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[858]}
    (FInMonth: 10; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[859]}
    (FInMonth: 5; FOnDay: @CRelativeDays[13]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[860]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[861]}
    (FInMonth: 5; FOnDay: @CRelativeDays[76]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[862]}
    (FInMonth: 3; FOnDay: @CRelativeDays[46]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[863]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[864]}
    (FInMonth: 10; FOnDay: @CRelativeDays[18]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[865]}
    (FInMonth: 10; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[866]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[867]}
    (FInMonth: 3; FOnDay: @CRelativeDays[20]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[868]}
    (FInMonth: 11; FOnDay: @CRelativeDays[12]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[869]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[870]}
    (FInMonth: 10; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'EDT'),
   {CRules[871]}
    (FInMonth: 2; FOnDay: @CRelativeDays[16]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'EST'),
   {CRules[872]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: '-0430'),
   {CRules[873]}
    (FInMonth: 2; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'EST'),
   {CRules[874]}
    (FInMonth: 1; FOnDay: @CRelativeDays[10]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'EST'),
   {CRules[875]}
    (FInMonth: 1; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'EST'),
   {CRules[876]}
    (FInMonth: 11; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[877]}
    (FInMonth: 2; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[878]}
    (FInMonth: 5; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[879]}
    (FInMonth: 3; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[880]}
    (FInMonth: 5; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[881]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[882]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[883]}
    (FInMonth: 8; FOnDay: @CRelativeDays[48]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[884]}
    (FInMonth: 3; FOnDay: @CRelativeDays[37]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[885]}
    (FInMonth: 6; FOnDay: @CRelativeDays[77]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[886]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[887]}
    (FInMonth: 6; FOnDay: @CRelativeDays[22]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[888]}
    (FInMonth: 9; FOnDay: @CRelativeDays[23]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[889]}
    (FInMonth: 11; FOnDay: @CRelativeDays[29]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[890]}
    (FInMonth: 10; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[891]}
    (FInMonth: 9; FOnDay: @CRelativeDays[24]; FAt: 3600; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[892]}
    (FInMonth: 4; FOnDay: @CRelativeDays[14]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[893]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 82800; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[894]}
    (FInMonth: 10; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[895]}
    (FInMonth: 3; FOnDay: @CRelativeDays[32]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[896]}
    (FInMonth: 9; FOnDay: @CRelativeDays[33]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[897]}
    (FInMonth: 4; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[898]}
    (FInMonth: 10; FOnDay: @CRelativeDays[28]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[899]}
    (FInMonth: 7; FOnDay: @CRelativeDays[14]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[900]}
    (FInMonth: 7; FOnDay: @CRelativeDays[31]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[901]}
    (FInMonth: 5; FOnDay: @CRelativeDays[18]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[902]}
    (FInMonth: 4; FOnDay: @CRelativeDays[66]; FAt: 0; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[903]}
    (FInMonth: 9; FOnDay: @CRelativeDays[67]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[904]}
    (FInMonth: 9; FOnDay: @CRelativeDays[78]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[905]}
    (FInMonth: 8; FOnDay: @CRelativeDays[67]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[906]}
    (FInMonth: 8; FOnDay: @CRelativeDays[10]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[907]}
    (FInMonth: 8; FOnDay: @CRelativeDays[34]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[908]}
    (FInMonth: 9; FOnDay: @CRelativeDays[25]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[909]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[910]}
    (FInMonth: 6; FOnDay: @CRelativeDays[19]; FAt: 86400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[911]}
    (FInMonth: 7; FOnDay: @CRelativeDays[8]; FAt: 86400; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[912]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 1200; FFmtPart: ''),
   {CRules[913]}
    (FInMonth: 12; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[914]}
    (FInMonth: 10; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[915]}
    (FInMonth: 10; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[916]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[917]}
    (FInMonth: 4; FOnDay: @CRelativeDays[15]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[918]}
    (FInMonth: 3; FOnDay: @CRelativeDays[66]; FAt: 3600; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[919]}
    (FInMonth: 10; FOnDay: @CRelativeDays[66]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[920]}
    (FInMonth: 10; FOnDay: @CRelativeDays[34]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[921]}
    (FInMonth: 3; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[922]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[923]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[924]}
    (FInMonth: 9; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[925]}
    (FInMonth: 11; FOnDay: @CRelativeDays[29]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[926]}
    (FInMonth: 2; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[927]}
    (FInMonth: 11; FOnDay: @CRelativeDays[11]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[928]}
    (FInMonth: 6; FOnDay: @CRelativeDays[23]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[929]}
    (FInMonth: 10; FOnDay: @CRelativeDays[26]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[930]}
    (FInMonth: 6; FOnDay: @CRelativeDays[6]; FAt: 43200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[931]}
    (FInMonth: 6; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[932]}
    (FInMonth: 6; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[933]}
    (FInMonth: 8; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[934]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[935]}
    (FInMonth: 8; FOnDay: @CRelativeDays[44]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[936]}
    (FInMonth: 4; FOnDay: @CRelativeDays[6]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[937]}
    (FInMonth: 7; FOnDay: @CRelativeDays[8]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[938]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[939]}
    (FInMonth: 7; FOnDay: @CRelativeDays[10]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[940]}
    (FInMonth: 8; FOnDay: @CRelativeDays[10]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[941]}
    (FInMonth: 7; FOnDay: @CRelativeDays[13]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[942]}
    (FInMonth: 8; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[943]}
    (FInMonth: 6; FOnDay: @CRelativeDays[16]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[944]}
    (FInMonth: 8; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[945]}
    (FInMonth: 6; FOnDay: @CRelativeDays[21]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[946]}
    (FInMonth: 7; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[947]}
    (FInMonth: 6; FOnDay: @CRelativeDays[9]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[948]}
    (FInMonth: 7; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[949]}
    (FInMonth: 5; FOnDay: @CRelativeDays[22]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[950]}
    (FInMonth: 7; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[951]}
    (FInMonth: 5; FOnDay: @CRelativeDays[12]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[952]}
    (FInMonth: 6; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[953]}
    (FInMonth: 5; FOnDay: @CRelativeDays[9]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[954]}
    (FInMonth: 6; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[955]}
    (FInMonth: 4; FOnDay: @CRelativeDays[29]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[956]}
    (FInMonth: 5; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[957]}
    (FInMonth: 4; FOnDay: @CRelativeDays[23]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[958]}
    (FInMonth: 5; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[959]}
    (FInMonth: 3; FOnDay: @CRelativeDays[28]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[960]}
    (FInMonth: 5; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[961]}
    (FInMonth: 3; FOnDay: @CRelativeDays[29]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[962]}
    (FInMonth: 4; FOnDay: @CRelativeDays[4]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[963]}
    (FInMonth: 3; FOnDay: @CRelativeDays[34]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[964]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[965]}
    (FInMonth: 2; FOnDay: @CRelativeDays[4]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[966]}
    (FInMonth: 4; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[967]}
    (FInMonth: 2; FOnDay: @CRelativeDays[1]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[968]}
    (FInMonth: 3; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[969]}
    (FInMonth: 2; FOnDay: @CRelativeDays[13]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[970]}
    (FInMonth: 3; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[971]}
    (FInMonth: 1; FOnDay: @CRelativeDays[4]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[972]}
    (FInMonth: 2; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[973]}
    (FInMonth: 1; FOnDay: @CRelativeDays[21]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[974]}
    (FInMonth: 2; FOnDay: @CRelativeDays[11]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[975]}
    (FInMonth: 12; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[976]}
    (FInMonth: 2; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[977]}
    (FInMonth: 12; FOnDay: @CRelativeDays[33]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[978]}
    (FInMonth: 1; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[979]}
    (FInMonth: 12; FOnDay: @CRelativeDays[21]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[980]}
    (FInMonth: 1; FOnDay: @CRelativeDays[11]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[981]}
    (FInMonth: 11; FOnDay: @CRelativeDays[16]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[982]}
    (FInMonth: 1; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[983]}
    (FInMonth: 11; FOnDay: @CRelativeDays[10]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[984]}
    (FInMonth: 12; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[985]}
    (FInMonth: 11; FOnDay: @CRelativeDays[9]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[986]}
    (FInMonth: 12; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[987]}
    (FInMonth: 10; FOnDay: @CRelativeDays[16]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[988]}
    (FInMonth: 12; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[989]}
    (FInMonth: 10; FOnDay: @CRelativeDays[29]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[990]}
    (FInMonth: 11; FOnDay: @CRelativeDays[4]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[991]}
    (FInMonth: 10; FOnDay: @CRelativeDays[15]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[992]}
    (FInMonth: 11; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[993]}
    (FInMonth: 9; FOnDay: @CRelativeDays[19]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[994]}
    (FInMonth: 9; FOnDay: @CRelativeDays[11]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[995]}
    (FInMonth: 9; FOnDay: @CRelativeDays[2]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[996]}
    (FInMonth: 10; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[997]}
    (FInMonth: 8; FOnDay: @CRelativeDays[14]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[998]}
    (FInMonth: 8; FOnDay: @CRelativeDays[34]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[999]}
    (FInMonth: 9; FOnDay: @CRelativeDays[22]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1000]}
    (FInMonth: 8; FOnDay: @CRelativeDays[2]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1001]}
    (FInMonth: 9; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1002]}
    (FInMonth: 7; FOnDay: @CRelativeDays[32]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1003]}
    (FInMonth: 8; FOnDay: @CRelativeDays[16]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1004]}
    (FInMonth: 7; FOnDay: @CRelativeDays[25]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1005]}
    (FInMonth: 8; FOnDay: @CRelativeDays[10]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1006]}
    (FInMonth: 7; FOnDay: @CRelativeDays[0]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1007]}
    (FInMonth: 8; FOnDay: @CRelativeDays[9]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1008]}
    (FInMonth: 6; FOnDay: @CRelativeDays[4]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1009]}
    (FInMonth: 7; FOnDay: @CRelativeDays[16]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1010]}
    (FInMonth: 6; FOnDay: @CRelativeDays[13]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1011]}
    (FInMonth: 7; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1012]}
    (FInMonth: 5; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1013]}
    (FInMonth: 7; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1014]}
    (FInMonth: 5; FOnDay: @CRelativeDays[1]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1015]}
    (FInMonth: 6; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1016]}
    (FInMonth: 5; FOnDay: @CRelativeDays[13]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1017]}
    (FInMonth: 6; FOnDay: @CRelativeDays[23]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1018]}
    (FInMonth: 4; FOnDay: @CRelativeDays[16]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1019]}
    (FInMonth: 6; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1020]}
    (FInMonth: 4; FOnDay: @CRelativeDays[12]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1021]}
    (FInMonth: 5; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1022]}
    (FInMonth: 4; FOnDay: @CRelativeDays[9]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1023]}
    (FInMonth: 5; FOnDay: @CRelativeDays[34]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1024]}
    (FInMonth: 3; FOnDay: @CRelativeDays[16]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1025]}
    (FInMonth: 5; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1026]}
    (FInMonth: 3; FOnDay: @CRelativeDays[31]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1027]}
    (FInMonth: 3; FOnDay: @CRelativeDays[15]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1028]}
    (FInMonth: 4; FOnDay: @CRelativeDays[44]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1029]}
    (FInMonth: 2; FOnDay: @CRelativeDays[17]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1030]}
    (FInMonth: 3; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1031]}
    (FInMonth: 2; FOnDay: @CRelativeDays[25]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1032]}
    (FInMonth: 3; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1033]}
    (FInMonth: 2; FOnDay: @CRelativeDays[0]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1034]}
    (FInMonth: 3; FOnDay: @CRelativeDays[13]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1035]}
    (FInMonth: 1; FOnDay: @CRelativeDays[24]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1036]}
    (FInMonth: 1; FOnDay: @CRelativeDays[44]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1037]}
    (FInMonth: 2; FOnDay: @CRelativeDays[31]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1038]}
    (FInMonth: 12; FOnDay: @CRelativeDays[8]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1039]}
    (FInMonth: 2; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1040]}
    (FInMonth: 12; FOnDay: @CRelativeDays[24]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1041]}
    (FInMonth: 1; FOnDay: @CRelativeDays[10]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1042]}
    (FInMonth: 12; FOnDay: @CRelativeDays[13]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1043]}
    (FInMonth: 1; FOnDay: @CRelativeDays[23]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1044]}
    (FInMonth: 11; FOnDay: @CRelativeDays[33]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1045]}
    (FInMonth: 1; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1046]}
    (FInMonth: 11; FOnDay: @CRelativeDays[21]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1047]}
    (FInMonth: 12; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1048]}
    (FInMonth: 11; FOnDay: @CRelativeDays[18]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1049]}
    (FInMonth: 12; FOnDay: @CRelativeDays[23]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1050]}
    (FInMonth: 10; FOnDay: @CRelativeDays[22]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1051]}
    (FInMonth: 10; FOnDay: @CRelativeDays[12]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1052]}
    (FInMonth: 11; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1053]}
    (FInMonth: 10; FOnDay: @CRelativeDays[9]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1054]}
    (FInMonth: 11; FOnDay: @CRelativeDays[25]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1055]}
    (FInMonth: 9; FOnDay: @CRelativeDays[10]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1056]}
    (FInMonth: 10; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1057]}
    (FInMonth: 9; FOnDay: @CRelativeDays[23]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1058]}
    (FInMonth: 10; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1059]}
    (FInMonth: 8; FOnDay: @CRelativeDays[28]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1060]}
    (FInMonth: 8; FOnDay: @CRelativeDays[29]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1061]}
    (FInMonth: 9; FOnDay: @CRelativeDays[4]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1062]}
    (FInMonth: 8; FOnDay: @CRelativeDays[23]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1063]}
    (FInMonth: 9; FOnDay: @CRelativeDays[1]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1064]}
    (FInMonth: 7; FOnDay: @CRelativeDays[19]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1065]}
    (FInMonth: 7; FOnDay: @CRelativeDays[11]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1066]}
    (FInMonth: 8; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1067]}
    (FInMonth: 7; FOnDay: @CRelativeDays[34]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1068]}
    (FInMonth: 8; FOnDay: @CRelativeDays[21]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1069]}
    (FInMonth: 6; FOnDay: @CRelativeDays[14]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1070]}
    (FInMonth: 7; FOnDay: @CRelativeDays[7]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1071]}
    (FInMonth: 6; FOnDay: @CRelativeDays[24]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1072]}
    (FInMonth: 7; FOnDay: @CRelativeDays[22]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1073]}
    (FInMonth: 6; FOnDay: @CRelativeDays[0]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1074]}
    (FInMonth: 7; FOnDay: @CRelativeDays[12]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1075]}
    (FInMonth: 5; FOnDay: @CRelativeDays[32]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1076]}
    (FInMonth: 6; FOnDay: @CRelativeDays[16]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1077]}
    (FInMonth: 5; FOnDay: @CRelativeDays[24]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1078]}
    (FInMonth: 6; FOnDay: @CRelativeDays[10]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1079]}
    (FInMonth: 4; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1080]}
    (FInMonth: 4; FOnDay: @CRelativeDays[33]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1081]}
    (FInMonth: 5; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1082]}
    (FInMonth: 4; FOnDay: @CRelativeDays[21]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1083]}
    (FInMonth: 5; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1084]}
    (FInMonth: 3; FOnDay: @CRelativeDays[7]; FAt: 10800; FAtMode: trLocal; FOffset: -3600; FFmtPart: ''),
   {CRules[1085]}
    (FInMonth: 5; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1086]}
    (FInMonth: 3; FOnDay: @CRelativeDays[22]; FAt: 0; FAtMode: trLocal; FOffset: -3600; FFmtPart: 'WAT'),
   {CRules[1087]}
    (FInMonth: 9; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'CAT'),
   {CRules[1088]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: -3600; FFmtPart: 'WAT'),
   {CRules[1089]}
    (FInMonth: 9; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1090]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1091]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[1092]}
    (FInMonth: 2; FOnDay: @CRelativeDays[14]; FAt: 82800; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'S'),
   {CRules[1093]}
    (FInMonth: 4; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1094]}
    (FInMonth: 9; FOnDay: @CRelativeDays[32]; FAt: 0; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1095]}
    (FInMonth: 9; FOnDay: @CRelativeDays[7]; FAt: 3600; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1096]}
    (FInMonth: 1; FOnDay: @CRelativeDays[0]; FAt: 60; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1097]}
    (FInMonth: 3; FOnDay: @CRelativeDays[14]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1098]}
    (FInMonth: 1; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1099]}
    (FInMonth: 3; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1100]}
    (FInMonth: 9; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1101]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1102]}
    (FInMonth: 10; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1103]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1104]}
    (FInMonth: 3; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1105]}
    (FInMonth: 11; FOnDay: @CRelativeDays[17]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1106]}
    (FInMonth: 12; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1107]}
    (FInMonth: 2; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1108]}
    (FInMonth: 10; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1109]}
    (FInMonth: 2; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1110]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1111]}
    (FInMonth: 3; FOnDay: @CRelativeDays[6]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1112]}
    (FInMonth: 3; FOnDay: @CRelativeDays[33]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1113]}
    (FInMonth: 3; FOnDay: @CRelativeDays[13]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1114]}
    (FInMonth: 3; FOnDay: @CRelativeDays[10]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1115]}
    (FInMonth: 4; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1116]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1117]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1118]}
    (FInMonth: 3; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: 'S'),
   {CRules[1119]}
    (FInMonth: 10; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1120]}
    (FInMonth: 10; FOnDay: @CRelativeDays[30]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1121]}
    (FInMonth: 8; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1122]}
    (FInMonth: 3; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1123]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[1124]}
    (FInMonth: 10; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[1125]}
    (FInMonth: 8; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[1126]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1127]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[1128]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1129]}
    (FInMonth: 2; FOnDay: @CRelativeDays[39]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1130]}
    (FInMonth: 11; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1131]}
    (FInMonth: 3; FOnDay: @CRelativeDays[39]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1132]}
    (FInMonth: 10; FOnDay: @CRelativeDays[43]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1133]}
    (FInMonth: 3; FOnDay: @CRelativeDays[3]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1134]}
    (FInMonth: 1; FOnDay: @CRelativeDays[61]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1135]}
    (FInMonth: 1; FOnDay: @CRelativeDays[61]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1136]}
    (FInMonth: 1; FOnDay: @CRelativeDays[79]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1137]}
    (FInMonth: 11; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1138]}
    (FInMonth: 6; FOnDay: @CRelativeDays[28]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1139]}
    (FInMonth: 1; FOnDay: @CRelativeDays[26]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1140]}
    (FInMonth: 9; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1141]}
    (FInMonth: 1; FOnDay: @CRelativeDays[19]; FAt: 60; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1142]}
    (FInMonth: 8; FOnDay: @CRelativeDays[8]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1143]}
    (FInMonth: 9; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1144]}
    (FInMonth: 12; FOnDay: @CRelativeDays[24]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1145]}
    (FInMonth: 2; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1146]}
    (FInMonth: 5; FOnDay: @CRelativeDays[19]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1147]}
    (FInMonth: 8; FOnDay: @CRelativeDays[33]; FAt: 7260; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1148]}
    (FInMonth: 4; FOnDay: @CRelativeDays[32]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1149]}
    (FInMonth: 8; FOnDay: @CRelativeDays[16]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1150]}
    (FInMonth: 12; FOnDay: @CRelativeDays[3]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1151]}
    (FInMonth: 12; FOnDay: @CRelativeDays[0]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1152]}
    (FInMonth: 3; FOnDay: @CRelativeDays[2]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1153]}
    (FInMonth: 11; FOnDay: @CRelativeDays[18]; FAt: 7200; FAtMode: trLocal; FOffset: 3600; FFmtPart: 'S'),
   {CRules[1154]}
    (FInMonth: 3; FOnDay: @CRelativeDays[15]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'M'),
   {CRules[1155]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trLocal; FOffset: 1800; FFmtPart: 'S'),
   {CRules[1156]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'M'),
   {CRules[1157]}
    (FInMonth: 4; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: 'M'),
   {CRules[1158]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 1800; FFmtPart: 'S'),
   {CRules[1159]}
    (FInMonth: 1; FOnDay: @CRelativeDays[0]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: 'S'),
   {CRules[1160]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1161]}
    (FInMonth: 11; FOnDay: @CRelativeDays[3]; FAt: 9900; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1162]}
    (FInMonth: 2; FOnDay: @CRelativeDays[39]; FAt: 9900; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1163]}
    (FInMonth: 10; FOnDay: @CRelativeDays[39]; FAt: 9900; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1164]}
    (FInMonth: 3; FOnDay: @CRelativeDays[3]; FAt: 9900; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1165]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1166]}
    (FInMonth: 10; FOnDay: @CRelativeDays[20]; FAt: 9900; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1167]}
    (FInMonth: 10; FOnDay: @CRelativeDays[3]; FAt: 9900; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1168]}
    (FInMonth: 3; FOnDay: @CRelativeDays[5]; FAt: 9900; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1169]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: 'D'),
   {CRules[1170]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 9900; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1171]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 9900; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1172]}
    (FInMonth: 11; FOnDay: @CRelativeDays[31]; FAt: 0; FAtMode: trLocal; FOffset: 1800; FFmtPart: ''),
   {CRules[1173]}
    (FInMonth: 4; FOnDay: @CRelativeDays[49]; FAt: 14400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1174]}
    (FInMonth: 9; FOnDay: @CRelativeDays[42]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1175]}
    (FInMonth: 4; FOnDay: @CRelativeDays[3]; FAt: 14400; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1176]}
    (FInMonth: 9; FOnDay: @CRelativeDays[39]; FAt: 10800; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1177]}
    (FInMonth: 10; FOnDay: @CRelativeDays[13]; FAt: 7200; FAtMode: trStandard; FOffset: 3600; FFmtPart: ''),
   {CRules[1178]}
    (FInMonth: 3; FOnDay: @CRelativeDays[29]; FAt: 7200; FAtMode: trStandard; FOffset: 0; FFmtPart: ''),
   {CRules[1179]}
    (FInMonth: 1; FOnDay: @CRelativeDays[39]; FAt: 7200; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1180]}
    (FInMonth: 1; FOnDay: @CRelativeDays[5]; FAt: 10800; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1181]}
    (FInMonth: 9; FOnDay: @CRelativeDays[14]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1182]}
    (FInMonth: 3; FOnDay: @CRelativeDays[38]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1183]}
    (FInMonth: 10; FOnDay: @CRelativeDays[4]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1184]}
    (FInMonth: 9; FOnDay: @CRelativeDays[38]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: ''),
   {CRules[1185]}
    (FInMonth: 1; FOnDay: @CRelativeDays[38]; FAt: 0; FAtMode: trLocal; FOffset: 0; FFmtPart: ''),
   {CRules[1186]}
    (FInMonth: 10; FOnDay: @CRelativeDays[38]; FAt: 0; FAtMode: trLocal; FOffset: 3600; FFmtPart: '')
  );

var
  { Date-bound rules for Arg family }
  CFamily_0_Arr: array[0 .. 28] of TYearBoundRule = (
    (FStart: 1930; FEnd: 1930; FRule: @CRules[0]),
    (FStart: 1931; FEnd: 1931; FRule: @CRules[1]),
    (FStart: 1931; FEnd: 1931; FRule: @CRules[2]),
    (FStart: 1932; FEnd: 1940; FRule: @CRules[3]),
    (FStart: 1932; FEnd: 1939; FRule: @CRules[4]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[5]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[6]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[2]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[7]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[2]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[3]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[8]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[9]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[10]),
    (FStart: 1964; FEnd: 1966; FRule: @CRules[3]),
    (FStart: 1964; FEnd: 1966; FRule: @CRules[2]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[11]),
    (FStart: 1967; FEnd: 1968; FRule: @CRules[12]),
    (FStart: 1968; FEnd: 1969; FRule: @CRules[13]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[14]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[15]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[0]),
    (FStart: 1989; FEnd: 1993; FRule: @CRules[16]),
    (FStart: 1989; FEnd: 1992; FRule: @CRules[17]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[12]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[18]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[19]),
    (FStart: 2008; FEnd: 2009; FRule: @CRules[20]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[17])
  );

  { Date-bound rules for SanLuis family }
  CFamily_1_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 2008; FEnd: 2009; FRule: @CRules[21]),
    (FStart: 2007; FEnd: 2008; FRule: @CRules[22])
  );

  { Date-bound rules for Brazil family }
  CFamily_2_Arr: array[0 .. 56] of TYearBoundRule = (
    (FStart: 1931; FEnd: 1931; FRule: @CRules[23]),
    (FStart: 1932; FEnd: 1933; FRule: @CRules[1]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[24]),
    (FStart: 1949; FEnd: 1952; FRule: @CRules[0]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[25]),
    (FStart: 1951; FEnd: 1952; FRule: @CRules[1]),
    (FStart: 1953; FEnd: 1953; FRule: @CRules[3]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[26]),
    (FStart: 1964; FEnd: 1964; FRule: @CRules[3]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[27]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[28]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[0]),
    (FStart: 1966; FEnd: 1968; FRule: @CRules[3]),
    (FStart: 1966; FEnd: 1967; FRule: @CRules[4]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[29]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[30]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[31]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[32]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[31]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[33]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[34]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[35]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[2]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[36]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[37]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[38]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[39]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[40]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[31]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[41]),
    (FStart: 1993; FEnd: 1995; FRule: @CRules[42]),
    (FStart: 1994; FEnd: 1995; FRule: @CRules[43]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[36]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[44]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[45]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[44]),
    (FStart: 1998; FEnd: 1998; FRule: @CRules[3]),
    (FStart: 1998; FEnd: 1998; FRule: @CRules[46]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[47]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[24]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[48]),
    (FStart: 2000; FEnd: 2001; FRule: @CRules[22]),
    (FStart: 2001; FEnd: 2006; FRule: @CRules[43]),
    (FStart: 2002; FEnd: 2002; FRule: @CRules[49]),
    (FStart: 2003; FEnd: 2003; FRule: @CRules[50]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[29]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[34]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[51]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[52]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[22]),
    (FStart: 2008; FEnd: 2017; FRule: @CRules[17]),
    (FStart: 2008; FEnd: 2011; FRule: @CRules[43]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[53]),
    (FStart: 2013; FEnd: 2014; FRule: @CRules[43]),
    (FStart: 2015; FEnd: 2015; FRule: @CRules[53]),
    (FStart: 2016; FEnd: 2019; FRule: @CRules[43]),
    (FStart: 2018; FEnd: 2018; FRule: @CRules[54])
  );

  { Date-bound rules for Chile family }
  CFamily_3_Arr: array[0 .. 33] of TYearBoundRule = (
    (FStart: 1927; FEnd: 1931; FRule: @CRules[55]),
    (FStart: 1928; FEnd: 1932; FRule: @CRules[1]),
    (FStart: 1968; FEnd: 1968; FRule: @CRules[56]),
    (FStart: 1969; FEnd: 1969; FRule: @CRules[57]),
    (FStart: 1969; FEnd: 1969; FRule: @CRules[58]),
    (FStart: 1970; FEnd: 1970; FRule: @CRules[59]),
    (FStart: 1971; FEnd: 1971; FRule: @CRules[60]),
    (FStart: 1970; FEnd: 1972; FRule: @CRules[61]),
    (FStart: 1972; FEnd: 1986; FRule: @CRules[62]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[63]),
    (FStart: 1974; FEnd: 1987; FRule: @CRules[61]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[64]),
    (FStart: 1988; FEnd: 1990; FRule: @CRules[62]),
    (FStart: 1988; FEnd: 1989; FRule: @CRules[61]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[65]),
    (FStart: 1991; FEnd: 1996; FRule: @CRules[62]),
    (FStart: 1991; FEnd: 1997; FRule: @CRules[61]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[57]),
    (FStart: 1998; FEnd: 1998; FRule: @CRules[62]),
    (FStart: 1998; FEnd: 1998; FRule: @CRules[66]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[67]),
    (FStart: 1999; FEnd: 2010; FRule: @CRules[61]),
    (FStart: 2000; FEnd: 2007; FRule: @CRules[62]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[57]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[62]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[68]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[69]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[70]),
    (FStart: 2012; FEnd: 2014; FRule: @CRules[71]),
    (FStart: 2012; FEnd: 2014; FRule: @CRules[72]),
    (FStart: 2016; FEnd: 2018; FRule: @CRules[73]),
    (FStart: 2016; FEnd: 2018; FRule: @CRules[74]),
    (FStart: 2019; FEnd: 9999; FRule: @CRules[75]),
    (FStart: 2019; FEnd: 9999; FRule: @CRules[72])
  );

  { Date-bound rules for CO family }
  CFamily_4_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1992; FEnd: 1992; FRule: @CRules[76]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[77])
  );

  { Date-bound rules for Ecuador family }
  CFamily_5_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1992; FEnd: 1992; FRule: @CRules[78]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[79])
  );

  { Date-bound rules for Falk family }
  CFamily_6_Arr: array[0 .. 11] of TYearBoundRule = (
    (FStart: 1937; FEnd: 1938; FRule: @CRules[80]),
    (FStart: 1938; FEnd: 1942; FRule: @CRules[81]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[8]),
    (FStart: 1940; FEnd: 1942; FRule: @CRules[80]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[82]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[80]),
    (FStart: 1984; FEnd: 1985; FRule: @CRules[83]),
    (FStart: 1984; FEnd: 1984; FRule: @CRules[84]),
    (FStart: 1985; FEnd: 2000; FRule: @CRules[85]),
    (FStart: 1986; FEnd: 2000; FRule: @CRules[86]),
    (FStart: 2001; FEnd: 2010; FRule: @CRules[87]),
    (FStart: 2001; FEnd: 2010; FRule: @CRules[88])
  );

  { Date-bound rules for Para family }
  CFamily_7_Arr: array[0 .. 21] of TYearBoundRule = (
    (FStart: 1975; FEnd: 1988; FRule: @CRules[8]),
    (FStart: 1975; FEnd: 1978; FRule: @CRules[3]),
    (FStart: 1979; FEnd: 1991; FRule: @CRules[1]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[89]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[8]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[44]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[3]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[90]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[28]),
    (FStart: 1993; FEnd: 1995; FRule: @CRules[8]),
    (FStart: 1994; FEnd: 1995; FRule: @CRules[91]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[3]),
    (FStart: 1996; FEnd: 2001; FRule: @CRules[12]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[91]),
    (FStart: 1998; FEnd: 2001; FRule: @CRules[16]),
    (FStart: 2002; FEnd: 2004; FRule: @CRules[13]),
    (FStart: 2002; FEnd: 2003; FRule: @CRules[92]),
    (FStart: 2004; FEnd: 2009; FRule: @CRules[17]),
    (FStart: 2005; FEnd: 2009; FRule: @CRules[21]),
    (FStart: 2010; FEnd: 9999; FRule: @CRules[12]),
    (FStart: 2010; FEnd: 2012; FRule: @CRules[93]),
    (FStart: 2013; FEnd: 9999; FRule: @CRules[94])
  );

  { Date-bound rules for Peru family }
  CFamily_8_Arr: array[0 .. 9] of TYearBoundRule = (
    (FStart: 1938; FEnd: 1938; FRule: @CRules[95]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[1]),
    (FStart: 1938; FEnd: 1939; FRule: @CRules[80]),
    (FStart: 1939; FEnd: 1940; FRule: @CRules[96]),
    (FStart: 1986; FEnd: 1987; FRule: @CRules[95]),
    (FStart: 1986; FEnd: 1987; FRule: @CRules[1]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[95]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[1]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[95]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[1])
  );

  { Date-bound rules for Uruguay family }
  CFamily_9_Arr: array[0 .. 47] of TYearBoundRule = (
    (FStart: 1923; FEnd: 1925; FRule: @CRules[97]),
    (FStart: 1924; FEnd: 1926; FRule: @CRules[1]),
    (FStart: 1933; FEnd: 1938; FRule: @CRules[98]),
    (FStart: 1934; FEnd: 1941; FRule: @CRules[99]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[97]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[100]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[101]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[102]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[103]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[104]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[105]),
    (FStart: 1960; FEnd: 1960; FRule: @CRules[106]),
    (FStart: 1960; FEnd: 1960; FRule: @CRules[107]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[108]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[109]),
    (FStart: 1968; FEnd: 1968; FRule: @CRules[110]),
    (FStart: 1968; FEnd: 1968; FRule: @CRules[111]),
    (FStart: 1970; FEnd: 1970; FRule: @CRules[112]),
    (FStart: 1970; FEnd: 1970; FRule: @CRules[113]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[114]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[115]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[116]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[117]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[118]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[119]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[120]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[121]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[107]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[122]),
    (FStart: 1978; FEnd: 1979; FRule: @CRules[16]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[123]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[124]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[125]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[126]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[127]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[128]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[129]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[130]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[52]),
    (FStart: 1990; FEnd: 1991; FRule: @CRules[131]),
    (FStart: 1991; FEnd: 1992; FRule: @CRules[16]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[132]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[127]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[133]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[134]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[135]),
    (FStart: 2006; FEnd: 2015; FRule: @CRules[136]),
    (FStart: 2006; FEnd: 2014; FRule: @CRules[137])
  );

  { Date-bound rules for SystemV family }
  CFamily_10_Arr: array[0 .. 7] of TYearBoundRule = (
    (FStart: 1; FEnd: 1973; FRule: @CRules[138]),
    (FStart: 1; FEnd: 1973; FRule: @CRules[139]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[140]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[141]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[142]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[139]),
    (FStart: 1976; FEnd: 9999; FRule: @CRules[138]),
    (FStart: 1976; FEnd: 9999; FRule: @CRules[139])
  );

  { Date-bound rules for Troll family }
  CFamily_11_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 2005; FEnd: 9999; FRule: @CRules[143]),
    (FStart: 2004; FEnd: 9999; FRule: @CRules[144])
  );

  { Date-bound rules for GB-Eire family }
  CFamily_12_Arr: array[0 .. 64] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[145]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[146]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[147]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[148]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[149]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[150]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[151]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[152]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[153]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[154]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[155]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[156]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[157]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[158]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[159]),
    (FStart: 1923; FEnd: 1924; FRule: @CRules[160]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[161]),
    (FStart: 1925; FEnd: 1926; FRule: @CRules[159]),
    (FStart: 1925; FEnd: 1938; FRule: @CRules[162]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[161]),
    (FStart: 1928; FEnd: 1929; FRule: @CRules[159]),
    (FStart: 1930; FEnd: 1930; FRule: @CRules[161]),
    (FStart: 1931; FEnd: 1932; FRule: @CRules[159]),
    (FStart: 1933; FEnd: 1933; FRule: @CRules[161]),
    (FStart: 1934; FEnd: 1934; FRule: @CRules[159]),
    (FStart: 1935; FEnd: 1935; FRule: @CRules[161]),
    (FStart: 1936; FEnd: 1937; FRule: @CRules[159]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[161]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[159]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[163]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[164]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[165]),
    (FStart: 1941; FEnd: 1943; FRule: @CRules[166]),
    (FStart: 1942; FEnd: 1944; FRule: @CRules[167]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[168]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[169]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[170]),
    (FStart: 1945; FEnd: 1946; FRule: @CRules[162]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[161]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[171]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[172]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[173]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[174]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[175]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[176]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[155]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[177]),
    (FStart: 1950; FEnd: 1952; FRule: @CRules[178]),
    (FStart: 1950; FEnd: 1952; FRule: @CRules[179]),
    (FStart: 1953; FEnd: 1953; FRule: @CRules[159]),
    (FStart: 1953; FEnd: 1960; FRule: @CRules[162]),
    (FStart: 1954; FEnd: 1954; FRule: @CRules[161]),
    (FStart: 1955; FEnd: 1956; FRule: @CRules[159]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[161]),
    (FStart: 1958; FEnd: 1959; FRule: @CRules[159]),
    (FStart: 1960; FEnd: 1960; FRule: @CRules[161]),
    (FStart: 1961; FEnd: 1963; FRule: @CRules[180]),
    (FStart: 1961; FEnd: 1968; FRule: @CRules[181]),
    (FStart: 1964; FEnd: 1967; FRule: @CRules[182]),
    (FStart: 1968; FEnd: 1968; FRule: @CRules[183]),
    (FStart: 1972; FEnd: 1980; FRule: @CRules[184]),
    (FStart: 1972; FEnd: 1980; FRule: @CRules[181]),
    (FStart: 1981; FEnd: 1995; FRule: @CRules[185]),
    (FStart: 1981; FEnd: 1989; FRule: @CRules[186]),
    (FStart: 1990; FEnd: 1995; FRule: @CRules[187])
  );

  { Date-bound rules for EU family }
  CFamily_13_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1977; FEnd: 1980; FRule: @CRules[195]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[196]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[197]),
    (FStart: 1979; FEnd: 1995; FRule: @CRules[196]),
    (FStart: 1981; FEnd: 9999; FRule: @CRules[198]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[199])
  );

  { Date-bound rules for Eire family }
  CFamily_14_Arr: array[0 .. 6] of TYearBoundRule = (
    (FStart: 1971; FEnd: 1971; FRule: @CRules[188]),
    (FStart: 1972; FEnd: 1980; FRule: @CRules[189]),
    (FStart: 1972; FEnd: 1980; FRule: @CRules[190]),
    (FStart: 1981; FEnd: 9999; FRule: @CRules[191]),
    (FStart: 1981; FEnd: 1989; FRule: @CRules[192]),
    (FStart: 1990; FEnd: 1995; FRule: @CRules[193]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[194])
  );

  { Date-bound rules for W-Eur family }
  CFamily_15_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1977; FEnd: 1980; FRule: @CRules[195]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[196]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[197]),
    (FStart: 1979; FEnd: 1995; FRule: @CRules[196]),
    (FStart: 1981; FEnd: 9999; FRule: @CRules[198]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[199])
  );

  { Date-bound rules for C-Eur family }
  CFamily_16_Arr: array[0 .. 16] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[200]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[197]),
    (FStart: 1917; FEnd: 1918; FRule: @CRules[201]),
    (FStart: 1917; FEnd: 1918; FRule: @CRules[202]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[203]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[204]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[205]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[206]),
    (FStart: 1944; FEnd: 1945; FRule: @CRules[207]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[208]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[209]),
    (FStart: 1977; FEnd: 1980; FRule: @CRules[210]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[211]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[212]),
    (FStart: 1979; FEnd: 1995; FRule: @CRules[211]),
    (FStart: 1981; FEnd: 9999; FRule: @CRules[213]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[214])
  );

  { Date-bound rules for E-Eur family }
  CFamily_17_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1977; FEnd: 1980; FRule: @CRules[215]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[216]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[9]),
    (FStart: 1979; FEnd: 1995; FRule: @CRules[216]),
    (FStart: 1981; FEnd: 9999; FRule: @CRules[217]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[218])
  );

  { Date-bound rules for Russia family }
  CFamily_18_Arr: array[0 .. 15] of TYearBoundRule = (
    (FStart: 1917; FEnd: 1917; FRule: @CRules[219]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[220]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[221]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[222]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[223]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[224]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[225]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[226]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[227]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[228]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[9]),
    (FStart: 1981; FEnd: 1984; FRule: @CRules[229]),
    (FStart: 1981; FEnd: 1983; FRule: @CRules[9]),
    (FStart: 1984; FEnd: 1995; FRule: @CRules[211]),
    (FStart: 1985; FEnd: 2010; FRule: @CRules[213]),
    (FStart: 1996; FEnd: 2010; FRule: @CRules[214])
  );

  { Date-bound rules for Albania family }
  CFamily_19_Arr: array[0 .. 24] of TYearBoundRule = (
    (FStart: 1940; FEnd: 1940; FRule: @CRules[230]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[231]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[205]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[232]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[233]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[234]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[235]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[234]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[236]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[237]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[238]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[234]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[239]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[9]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[240]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[241]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[242]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[243]),
    (FStart: 1981; FEnd: 1981; FRule: @CRules[244]),
    (FStart: 1981; FEnd: 1981; FRule: @CRules[245]),
    (FStart: 1982; FEnd: 1982; FRule: @CRules[236]),
    (FStart: 1982; FEnd: 1982; FRule: @CRules[237]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[246]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[9]),
    (FStart: 1984; FEnd: 1984; FRule: @CRules[229])
  );

  { Date-bound rules for Austria family }
  CFamily_20_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1920; FRule: @CRules[247]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[248]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[249]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[250]),
    (FStart: 1947; FEnd: 1948; FRule: @CRules[251]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[252]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[253]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[254]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[255])
  );

  { Date-bound rules for Belgium family }
  CFamily_21_Arr: array[0 .. 33] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[256]),
    (FStart: 1918; FEnd: 1919; FRule: @CRules[257]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[258]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[259]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[260]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[261]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[262]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[263]),
    (FStart: 1922; FEnd: 1927; FRule: @CRules[257]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[264]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[265]),
    (FStart: 1925; FEnd: 1925; FRule: @CRules[266]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[267]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[268]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[269]),
    (FStart: 1928; FEnd: 1938; FRule: @CRules[270]),
    (FStart: 1929; FEnd: 1929; FRule: @CRules[271]),
    (FStart: 1930; FEnd: 1930; FRule: @CRules[272]),
    (FStart: 1931; FEnd: 1931; FRule: @CRules[273]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[274]),
    (FStart: 1933; FEnd: 1933; FRule: @CRules[275]),
    (FStart: 1934; FEnd: 1934; FRule: @CRules[276]),
    (FStart: 1935; FEnd: 1935; FRule: @CRules[277]),
    (FStart: 1936; FEnd: 1936; FRule: @CRules[273]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[278]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[279]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[280]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[281]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[282]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[283]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[284]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[209]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[285]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[250])
  );

  { Date-bound rules for Bulg family }
  CFamily_22_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1979; FEnd: 1979; FRule: @CRules[286]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[197]),
    (FStart: 1980; FEnd: 1982; FRule: @CRules[287]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[288]),
    (FStart: 1981; FEnd: 1981; FRule: @CRules[289])
  );

  { Date-bound rules for Czech family }
  CFamily_23_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1945; FEnd: 1945; FRule: @CRules[207]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[212]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[290]),
    (FStart: 1946; FEnd: 1949; FRule: @CRules[251]),
    (FStart: 1947; FEnd: 1948; FRule: @CRules[291]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[292])
  );

  { Date-bound rules for Denmark family }
  CFamily_24_Arr: array[0 .. 10] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[293]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[294]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[295]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[284]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[296]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[297]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[298]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[299]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[300]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[301]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[302])
  );

  { Date-bound rules for Thule family }
  CFamily_25_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1991; FEnd: 1992; FRule: @CRules[303]),
    (FStart: 1991; FEnd: 1992; FRule: @CRules[304]),
    (FStart: 1993; FEnd: 2006; FRule: @CRules[305]),
    (FStart: 1993; FEnd: 2006; FRule: @CRules[139]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[306]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[307])
  );

  { Date-bound rules for Finland family }
  CFamily_26_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1942; FEnd: 1942; FRule: @CRules[308]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[309]),
    (FStart: 1981; FEnd: 1982; FRule: @CRules[213]),
    (FStart: 1981; FEnd: 1982; FRule: @CRules[310])
  );

  { Date-bound rules for France family }
  CFamily_27_Arr: array[0 .. 41] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[311]),
    (FStart: 1916; FEnd: 1919; FRule: @CRules[312]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[313]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[314]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[258]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[259]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[260]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[261]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[262]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[263]),
    (FStart: 1922; FEnd: 1938; FRule: @CRules[257]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[315]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[265]),
    (FStart: 1925; FEnd: 1925; FRule: @CRules[266]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[267]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[268]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[269]),
    (FStart: 1929; FEnd: 1929; FRule: @CRules[316]),
    (FStart: 1930; FEnd: 1930; FRule: @CRules[317]),
    (FStart: 1931; FEnd: 1931; FRule: @CRules[318]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[319]),
    (FStart: 1933; FEnd: 1933; FRule: @CRules[263]),
    (FStart: 1934; FEnd: 1934; FRule: @CRules[320]),
    (FStart: 1935; FEnd: 1935; FRule: @CRules[321]),
    (FStart: 1936; FEnd: 1936; FRule: @CRules[318]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[322]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[323]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[324]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[325]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[282]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[326]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[327]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[328]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[329]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[330]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[331]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[332]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[333]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[334]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[335]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[336]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[337])
  );

  { Date-bound rules for Germany family }
  CFamily_28_Arr: array[0 .. 7] of TYearBoundRule = (
    (FStart: 1946; FEnd: 1946; FRule: @CRules[249]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[250]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[251]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[338]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[339]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[340]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[253]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[341])
  );

  { Date-bound rules for SovietZone family }
  CFamily_29_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1945; FEnd: 1945; FRule: @CRules[342]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[343]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[344])
  );

  { Date-bound rules for Greece family }
  CFamily_30_Arr: array[0 .. 18] of TYearBoundRule = (
    (FStart: 1932; FEnd: 1932; FRule: @CRules[345]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[118]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[346]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[231]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[347]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[243]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[348]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[349]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[350]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[351]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[352]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[353]),
    (FStart: 1977; FEnd: 1978; FRule: @CRules[210]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[354]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[355]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[356]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[357]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[229]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[255])
  );

  { Date-bound rules for Hungary family }
  CFamily_31_Arr: array[0 .. 17] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[358]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[335]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[359]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[360]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[361]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[362]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[277]),
    (FStart: 1946; FEnd: 1949; FRule: @CRules[251]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[363]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[364]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[365]),
    (FStart: 1954; FEnd: 1955; FRule: @CRules[366]),
    (FStart: 1954; FEnd: 1955; FRule: @CRules[237]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[367]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[216]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[368]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[310]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[369])
  );

  { Date-bound rules for Iceland family }
  CFamily_32_Arr: array[0 .. 15] of TYearBoundRule = (
    (FStart: 1917; FEnd: 1919; FRule: @CRules[370]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[371]),
    (FStart: 1918; FEnd: 1919; FRule: @CRules[372]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[373]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[374]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[375]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[376]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[377]),
    (FStart: 1940; FEnd: 1941; FRule: @CRules[378]),
    (FStart: 1941; FEnd: 1942; FRule: @CRules[379]),
    (FStart: 1943; FEnd: 1946; FRule: @CRules[380]),
    (FStart: 1942; FEnd: 1948; FRule: @CRules[381]),
    (FStart: 1947; FEnd: 1967; FRule: @CRules[382]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[383]),
    (FStart: 1950; FEnd: 1966; FRule: @CRules[381]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[384])
  );

  { Date-bound rules for Italy family }
  CFamily_33_Arr: array[0 .. 40] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[385]),
    (FStart: 1916; FEnd: 1917; FRule: @CRules[386]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[387]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[388]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[389]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[390]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[391]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[392]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[393]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[394]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[204]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[205]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[206]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[284]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[283]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[284]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[395]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[396]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[397]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[398]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[399]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[400]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[401]),
    (FStart: 1966; FEnd: 1968; FRule: @CRules[402]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[403]),
    (FStart: 1967; FEnd: 1969; FRule: @CRules[404]),
    (FStart: 1969; FEnd: 1969; FRule: @CRules[405]),
    (FStart: 1970; FEnd: 1970; FRule: @CRules[406]),
    (FStart: 1970; FEnd: 1970; FRule: @CRules[216]),
    (FStart: 1971; FEnd: 1972; FRule: @CRules[402]),
    (FStart: 1971; FEnd: 1971; FRule: @CRules[216]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[9]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[407]),
    (FStart: 1973; FEnd: 1974; FRule: @CRules[216]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[408]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[405]),
    (FStart: 1975; FEnd: 1977; FRule: @CRules[216]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[409]),
    (FStart: 1977; FEnd: 1979; FRule: @CRules[402]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[9]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[241])
  );

  { Date-bound rules for Latvia family }
  CFamily_34_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1989; FEnd: 1996; FRule: @CRules[213]),
    (FStart: 1989; FEnd: 1996; FRule: @CRules[211])
  );

  { Date-bound rules for Lux family }
  CFamily_35_Arr: array[0 .. 22] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[293]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[197]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[410]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[411]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[201]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[202]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[258]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[412]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[259]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[413]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[261]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[414]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[263]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[415]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[264]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[270]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[265]),
    (FStart: 1924; FEnd: 1928; FRule: @CRules[415]),
    (FStart: 1925; FEnd: 1925; FRule: @CRules[416]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[267]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[268]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[269]),
    (FStart: 1929; FEnd: 1929; FRule: @CRules[316])
  );

  { Date-bound rules for Malta family }
  CFamily_36_Arr: array[0 .. 6] of TYearBoundRule = (
    (FStart: 1973; FEnd: 1973; FRule: @CRules[417]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[418]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[419]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[420]),
    (FStart: 1975; FEnd: 1979; FRule: @CRules[291]),
    (FStart: 1975; FEnd: 1980; FRule: @CRules[421]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[277])
  );

  { Date-bound rules for Moldova family }
  CFamily_37_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1997; FEnd: 9999; FRule: @CRules[213]),
    (FStart: 1997; FEnd: 9999; FRule: @CRules[422])
  );

  { Date-bound rules for Romania family }
  CFamily_38_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1932; FEnd: 1932; FRule: @CRules[464]),
    (FStart: 1932; FEnd: 1939; FRule: @CRules[465]),
    (FStart: 1933; FEnd: 1939; FRule: @CRules[466]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[467]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[216]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[416]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[196]),
    (FStart: 1991; FEnd: 1993; FRule: @CRules[217]),
    (FStart: 1991; FEnd: 1993; FRule: @CRules[216])
  );

  { Date-bound rules for Neth family }
  CFamily_39_Arr: array[0 .. 19] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[423]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[424]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[425]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[426]),
    (FStart: 1918; FEnd: 1921; FRule: @CRules[427]),
    (FStart: 1918; FEnd: 1921; FRule: @CRules[428]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[429]),
    (FStart: 1922; FEnd: 1936; FRule: @CRules[430]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[431]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[429]),
    (FStart: 1925; FEnd: 1925; FRule: @CRules[431]),
    (FStart: 1926; FEnd: 1931; FRule: @CRules[432]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[433]),
    (FStart: 1933; FEnd: 1936; FRule: @CRules[432]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[433]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[348]),
    (FStart: 1937; FEnd: 1939; FRule: @CRules[270]),
    (FStart: 1938; FEnd: 1939; FRule: @CRules[434]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[284]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[209])
  );

  { Date-bound rules for Norway family }
  CFamily_40_Arr: array[0 .. 6] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[435]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[241]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[284]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[212]),
    (FStart: 1959; FEnd: 1964; FRule: @CRules[436]),
    (FStart: 1959; FEnd: 1965; FRule: @CRules[421]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[437])
  );

  { Date-bound rules for Poland family }
  CFamily_41_Arr: array[0 .. 19] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1919; FRule: @CRules[209]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[438]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[274]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[206]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[439]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[362]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[440]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[250]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[299]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[251]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[253]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[341]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[441]),
    (FStart: 1957; FEnd: 1958; FRule: @CRules[196]),
    (FStart: 1958; FEnd: 1958; FRule: @CRules[442]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[443]),
    (FStart: 1959; FEnd: 1961; FRule: @CRules[444]),
    (FStart: 1960; FEnd: 1960; FRule: @CRules[445]),
    (FStart: 1961; FEnd: 1964; FRule: @CRules[446]),
    (FStart: 1962; FEnd: 1964; FRule: @CRules[196])
  );

  { Date-bound rules for Port family }
  CFamily_42_Arr: array[0 .. 49] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[447]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[448]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[449]),
    (FStart: 1917; FEnd: 1921; FRule: @CRules[450]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[258]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[449]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[451]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[449]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[452]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[450]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[267]),
    (FStart: 1926; FEnd: 1929; FRule: @CRules[257]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[268]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[269]),
    (FStart: 1929; FEnd: 1929; FRule: @CRules[316]),
    (FStart: 1931; FEnd: 1931; FRule: @CRules[318]),
    (FStart: 1931; FEnd: 1932; FRule: @CRules[257]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[319]),
    (FStart: 1934; FEnd: 1934; FRule: @CRules[320]),
    (FStart: 1934; FEnd: 1938; FRule: @CRules[257]),
    (FStart: 1935; FEnd: 1935; FRule: @CRules[321]),
    (FStart: 1936; FEnd: 1936; FRule: @CRules[318]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[322]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[323]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[324]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[325]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[453]),
    (FStart: 1940; FEnd: 1941; FRule: @CRules[454]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[416]),
    (FStart: 1942; FEnd: 1945; FRule: @CRules[455]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[456]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[457]),
    (FStart: 1942; FEnd: 1945; FRule: @CRules[458]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[459]),
    (FStart: 1943; FEnd: 1945; FRule: @CRules[460]),
    (FStart: 1944; FEnd: 1945; FRule: @CRules[461]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[287]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[257]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[210]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[251]),
    (FStart: 1951; FEnd: 1965; FRule: @CRules[210]),
    (FStart: 1951; FEnd: 1965; FRule: @CRules[251]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[462]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[463]),
    (FStart: 1978; FEnd: 1979; FRule: @CRules[215]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[9]),
    (FStart: 1979; FEnd: 1982; FRule: @CRules[196]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[217]),
    (FStart: 1981; FEnd: 1982; FRule: @CRules[198]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[213])
  );

  { Date-bound rules for Spain family }
  CFamily_43_Arr: array[0 .. 29] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[324]),
    (FStart: 1918; FEnd: 1919; FRule: @CRules[389]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[468]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[452]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[391]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[267]),
    (FStart: 1926; FEnd: 1929; FRule: @CRules[469]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[268]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[470]),
    (FStart: 1929; FEnd: 1929; FRule: @CRules[316]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[471]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[472]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[319]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[473]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[474]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[475]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[476]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[477]),
    (FStart: 1943; FEnd: 1946; FRule: @CRules[478]),
    (FStart: 1943; FEnd: 1944; FRule: @CRules[444]),
    (FStart: 1945; FEnd: 1946; FRule: @CRules[196]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[200]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[479]),
    (FStart: 1974; FEnd: 1975; FRule: @CRules[480]),
    (FStart: 1974; FEnd: 1975; FRule: @CRules[444]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[481]),
    (FStart: 1976; FEnd: 1977; FRule: @CRules[196]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[319]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[284]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[212])
  );

  { Date-bound rules for SpainAfrica family }
  CFamily_44_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1967; FEnd: 1967; FRule: @CRules[482]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[9]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[483]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[118]),
    (FStart: 1976; FEnd: 1977; FRule: @CRules[235]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[7]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[255]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[405]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[484])
  );

  { Date-bound rules for Swiss family }
  CFamily_45_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1941; FEnd: 1942; FRule: @CRules[485]),
    (FStart: 1941; FEnd: 1942; FRule: @CRules[486])
  );

  { Date-bound rules for Turkey family }
  CFamily_46_Arr: array[0 .. 44] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[235]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[9]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[487]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[488]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[489]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[237]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[490]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[491]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[492]),
    (FStart: 1924; FEnd: 1925; FRule: @CRules[9]),
    (FStart: 1925; FEnd: 1925; FRule: @CRules[235]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[348]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[493]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[494]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[495]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[229]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[491]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[405]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[9]),
    (FStart: 1947; FEnd: 1948; FRule: @CRules[496]),
    (FStart: 1947; FEnd: 1951; FRule: @CRules[497]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[498]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[499]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[500]),
    (FStart: 1962; FEnd: 1962; FRule: @CRules[501]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[502]),
    (FStart: 1964; FEnd: 1964; FRule: @CRules[295]),
    (FStart: 1964; FEnd: 1964; FRule: @CRules[9]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[503]),
    (FStart: 1973; FEnd: 1976; FRule: @CRules[504]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[277]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[505]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[506]),
    (FStart: 1977; FEnd: 1978; FRule: @CRules[210]),
    (FStart: 1977; FEnd: 1978; FRule: @CRules[507]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[508]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[509]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[208]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[510]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[511]),
    (FStart: 1986; FEnd: 1993; FRule: @CRules[198]),
    (FStart: 1986; FEnd: 1995; FRule: @CRules[196]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[512]),
    (FStart: 1995; FEnd: 2006; FRule: @CRules[198]),
    (FStart: 1996; FEnd: 2006; FRule: @CRules[199])
  );

  { Date-bound rules for SL family }
  CFamily_47_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1935; FEnd: 1942; FRule: @CRules[513]),
    (FStart: 1935; FEnd: 1942; FRule: @CRules[514]),
    (FStart: 1957; FEnd: 1962; FRule: @CRules[515]),
    (FStart: 1957; FEnd: 1962; FRule: @CRules[516])
  );

  { Date-bound rules for NT_YK family }
  CFamily_48_Arr: array[0 .. 11] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[661]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[768]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[826]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[567]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[757]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[827]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[139]),
    (FStart: 1980; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1980; FEnd: 2006; FRule: @CRules[139]),
    (FStart: 1987; FEnd: 2006; FRule: @CRules[305])
  );

  { Date-bound rules for Mexico family }
  CFamily_49_Arr: array[0 .. 13] of TYearBoundRule = (
    (FStart: 1939; FEnd: 1939; FRule: @CRules[828]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[829]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[830]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[831]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[832]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[833]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[834]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[835]),
    (FStart: 1996; FEnd: 2000; FRule: @CRules[305]),
    (FStart: 1996; FEnd: 2000; FRule: @CRules[139]),
    (FStart: 2001; FEnd: 2001; FRule: @CRules[527]),
    (FStart: 2001; FEnd: 2001; FRule: @CRules[304]),
    (FStart: 2002; FEnd: 9999; FRule: @CRules[305]),
    (FStart: 2002; FEnd: 9999; FRule: @CRules[139])
  );

  { Date-bound rules for Mont family }
  CFamily_50_Arr: array[0 .. 19] of TYearBoundRule = (
    (FStart: 1917; FEnd: 1917; FRule: @CRules[517]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[518]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[519]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[520]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[521]),
    (FStart: 1920; FEnd: 1922; FRule: @CRules[522]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[523]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[524]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[525]),
    (FStart: 1924; FEnd: 1926; FRule: @CRules[526]),
    (FStart: 1925; FEnd: 1926; FRule: @CRules[527]),
    (FStart: 1927; FEnd: 1937; FRule: @CRules[528]),
    (FStart: 1927; FEnd: 1937; FRule: @CRules[529]),
    (FStart: 1938; FEnd: 1940; FRule: @CRules[530]),
    (FStart: 1938; FEnd: 1939; FRule: @CRules[531]),
    (FStart: 1946; FEnd: 1973; FRule: @CRules[138]),
    (FStart: 1945; FEnd: 1948; FRule: @CRules[304]),
    (FStart: 1949; FEnd: 1950; FRule: @CRules[139]),
    (FStart: 1951; FEnd: 1956; FRule: @CRules[304]),
    (FStart: 1957; FEnd: 1973; FRule: @CRules[139])
  );

  { Date-bound rules for Canada family }
  CFamily_51_Arr: array[0 .. 9] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[661]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[768]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[757]),
    (FStart: 1974; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1974; FEnd: 2006; FRule: @CRules[139]),
    (FStart: 1987; FEnd: 2006; FRule: @CRules[305]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[306]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[307])
  );

  { Date-bound rules for NZ family }
  CFamily_52_Arr: array[0 .. 15] of TYearBoundRule = (
    (FStart: 1927; FEnd: 1927; FRule: @CRules[1153]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[1154]),
    (FStart: 1928; FEnd: 1933; FRule: @CRules[1155]),
    (FStart: 1929; FEnd: 1933; FRule: @CRules[1156]),
    (FStart: 1934; FEnd: 1940; FRule: @CRules[1157]),
    (FStart: 1934; FEnd: 1940; FRule: @CRules[1158]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[1159]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[1160]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[1107]),
    (FStart: 1975; FEnd: 1988; FRule: @CRules[1103]),
    (FStart: 1976; FEnd: 1989; FRule: @CRules[1104]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[1165]),
    (FStart: 1990; FEnd: 2006; FRule: @CRules[1117]),
    (FStart: 1990; FEnd: 2007; FRule: @CRules[1110]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[1169]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1116])
  );

  { Date-bound rules for PRC family }
  CFamily_53_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1986; FEnd: 1986; FRule: @CRules[550]),
    (FStart: 1986; FEnd: 1991; FRule: @CRules[551]),
    (FStart: 1987; FEnd: 1991; FRule: @CRules[552])
  );

  { Date-bound rules for Zion family }
  CFamily_54_Arr: array[0 .. 86] of TYearBoundRule = (
    (FStart: 1940; FEnd: 1940; FRule: @CRules[539]),
    (FStart: 1942; FEnd: 1944; FRule: @CRules[567]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[608]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[569]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[609]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[610]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[611]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[567]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[612]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[613]),
    (FStart: 1948; FEnd: 1949; FRule: @CRules[610]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[549]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[609]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[614]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[569]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[615]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[616]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[617]),
    (FStart: 1953; FEnd: 1953; FRule: @CRules[618]),
    (FStart: 1953; FEnd: 1953; FRule: @CRules[619]),
    (FStart: 1954; FEnd: 1954; FRule: @CRules[620]),
    (FStart: 1954; FEnd: 1954; FRule: @CRules[621]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[622]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[623]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[624]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[625]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[626]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[627]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[628]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[629]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[630]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[631]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[632]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[633]),
    (FStart: 1984; FEnd: 1984; FRule: @CRules[634]),
    (FStart: 1984; FEnd: 1984; FRule: @CRules[635]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[636]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[637]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[638]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[639]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[547]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[640]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[641]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[642]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[643]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[644]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[645]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[646]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[647]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[648]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[649]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[650]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[651]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[652]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[569]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[653]),
    (FStart: 1995; FEnd: 1995; FRule: @CRules[654]),
    (FStart: 1995; FEnd: 1995; FRule: @CRules[644]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[541]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[655]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[656]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[657]),
    (FStart: 1998; FEnd: 1998; FRule: @CRules[658]),
    (FStart: 1998; FEnd: 1998; FRule: @CRules[650]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[659]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[660]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[661]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[662]),
    (FStart: 2001; FEnd: 2001; FRule: @CRules[663]),
    (FStart: 2001; FEnd: 2001; FRule: @CRules[664]),
    (FStart: 2002; FEnd: 2002; FRule: @CRules[665]),
    (FStart: 2002; FEnd: 2002; FRule: @CRules[666]),
    (FStart: 2003; FEnd: 2003; FRule: @CRules[667]),
    (FStart: 2003; FEnd: 2003; FRule: @CRules[668]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[669]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[670]),
    (FStart: 2005; FEnd: 2012; FRule: @CRules[671]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[672]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[673]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[674]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[675]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[676]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[677]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[678]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[679]),
    (FStart: 2013; FEnd: 9999; FRule: @CRules[680]),
    (FStart: 2013; FEnd: 9999; FRule: @CRules[139])
  );

  { Date-bound rules for EUAsia family }
  CFamily_55_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1981; FEnd: 9999; FRule: @CRules[198]),
    (FStart: 1979; FEnd: 1995; FRule: @CRules[196]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[199])
  );

  { Date-bound rules for E-EurAsia family }
  CFamily_56_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1981; FEnd: 9999; FRule: @CRules[532]),
    (FStart: 1979; FEnd: 1995; FRule: @CRules[216]),
    (FStart: 1996; FEnd: 9999; FRule: @CRules[218])
  );

  { Date-bound rules for RussiaAsia family }
  CFamily_57_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1981; FEnd: 1984; FRule: @CRules[533]),
    (FStart: 1981; FEnd: 1983; FRule: @CRules[9]),
    (FStart: 1984; FEnd: 1995; FRule: @CRules[211]),
    (FStart: 1985; FEnd: 2010; FRule: @CRules[534]),
    (FStart: 1996; FEnd: 2010; FRule: @CRules[214])
  );

  { Date-bound rules for Armenia family }
  CFamily_58_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 2011; FEnd: 2011; FRule: @CRules[534]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[214])
  );

  { Date-bound rules for Azer family }
  CFamily_59_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1997; FEnd: 2015; FRule: @CRules[535]),
    (FStart: 1997; FEnd: 2015; FRule: @CRules[536])
  );

  { Date-bound rules for Dhaka family }
  CFamily_60_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 2009; FEnd: 2009; FRule: @CRules[537]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[538])
  );

  { Date-bound rules for Shang family }
  CFamily_61_Arr: array[0 .. 11] of TYearBoundRule = (
    (FStart: 1940; FEnd: 1940; FRule: @CRules[539]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[540]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[541]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[542]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[543]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[544]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[545]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[546]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[547]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[548]),
    (FStart: 1948; FEnd: 1949; FRule: @CRules[549]),
    (FStart: 1948; FEnd: 1949; FRule: @CRules[546])
  );

  { Date-bound rules for HK family }
  CFamily_62_Arr: array[0 .. 13] of TYearBoundRule = (
    (FStart: 1946; FEnd: 1946; FRule: @CRules[419]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[553]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[554]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[555]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[556]),
    (FStart: 1948; FEnd: 1952; FRule: @CRules[557]),
    (FStart: 1949; FEnd: 1953; FRule: @CRules[558]),
    (FStart: 1953; FEnd: 1964; FRule: @CRules[559]),
    (FStart: 1954; FEnd: 1964; FRule: @CRules[560]),
    (FStart: 1965; FEnd: 1976; FRule: @CRules[561]),
    (FStart: 1965; FEnd: 1976; FRule: @CRules[562]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[563]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[564]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[565])
  );

  { Date-bound rules for Taiwan family }
  CFamily_63_Arr: array[0 .. 14] of TYearBoundRule = (
    (FStart: 1946; FEnd: 1946; FRule: @CRules[545]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[566]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[547]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[567]),
    (FStart: 1948; FEnd: 1951; FRule: @CRules[549]),
    (FStart: 1948; FEnd: 1951; FRule: @CRules[566]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[568]),
    (FStart: 1952; FEnd: 1954; FRule: @CRules[567]),
    (FStart: 1953; FEnd: 1959; FRule: @CRules[569]),
    (FStart: 1955; FEnd: 1961; FRule: @CRules[566]),
    (FStart: 1960; FEnd: 1961; FRule: @CRules[539]),
    (FStart: 1974; FEnd: 1975; FRule: @CRules[569]),
    (FStart: 1974; FEnd: 1975; FRule: @CRules[566]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[570]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[566])
  );

  { Date-bound rules for Macau family }
  CFamily_64_Arr: array[0 .. 26] of TYearBoundRule = (
    (FStart: 1942; FEnd: 1943; FRule: @CRules[571]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[572]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[573]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[574]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[573]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[575]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[576]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[577]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[578]),
    (FStart: 1949; FEnd: 1950; FRule: @CRules[579]),
    (FStart: 1949; FEnd: 1950; FRule: @CRules[580]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[581]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[582]),
    (FStart: 1952; FEnd: 1953; FRule: @CRules[579]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[583]),
    (FStart: 1953; FEnd: 1954; FRule: @CRules[580]),
    (FStart: 1954; FEnd: 1956; FRule: @CRules[584]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[585]),
    (FStart: 1956; FEnd: 1964; FRule: @CRules[586]),
    (FStart: 1957; FEnd: 1964; FRule: @CRules[587]),
    (FStart: 1965; FEnd: 1973; FRule: @CRules[588]),
    (FStart: 1965; FEnd: 1966; FRule: @CRules[589]),
    (FStart: 1967; FEnd: 1976; FRule: @CRules[590]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[591]),
    (FStart: 1975; FEnd: 1976; FRule: @CRules[588]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[592]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[590])
  );

  { Date-bound rules for Cyprus family }
  CFamily_65_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1975; FEnd: 1975; FRule: @CRules[593]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[594]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[295]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[595]),
    (FStart: 1977; FEnd: 1980; FRule: @CRules[215]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[463]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[234]),
    (FStart: 1979; FEnd: 1997; FRule: @CRules[216]),
    (FStart: 1981; FEnd: 1998; FRule: @CRules[217])
  );

  { Date-bound rules for Iran family }
  CFamily_66_Arr: array[0 .. 100] of TYearBoundRule = (
    (FStart: 1978; FEnd: 1980; FRule: @CRules[596]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[597]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[393]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[598]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[599]),
    (FStart: 1992; FEnd: 1995; FRule: @CRules[600]),
    (FStart: 1991; FEnd: 1995; FRule: @CRules[601]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[596]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[602]),
    (FStart: 1997; FEnd: 1999; FRule: @CRules[600]),
    (FStart: 1997; FEnd: 1999; FRule: @CRules[601]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[596]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[602]),
    (FStart: 2001; FEnd: 2003; FRule: @CRules[600]),
    (FStart: 2001; FEnd: 2003; FRule: @CRules[601]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[596]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[602]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[600]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[601]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[596]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[602]),
    (FStart: 2009; FEnd: 2011; FRule: @CRules[600]),
    (FStart: 2009; FEnd: 2011; FRule: @CRules[601]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[596]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[602]),
    (FStart: 2013; FEnd: 2015; FRule: @CRules[600]),
    (FStart: 2013; FEnd: 2015; FRule: @CRules[601]),
    (FStart: 2016; FEnd: 2016; FRule: @CRules[596]),
    (FStart: 2016; FEnd: 2016; FRule: @CRules[602]),
    (FStart: 2017; FEnd: 2019; FRule: @CRules[600]),
    (FStart: 2017; FEnd: 2019; FRule: @CRules[601]),
    (FStart: 2020; FEnd: 2020; FRule: @CRules[596]),
    (FStart: 2020; FEnd: 2020; FRule: @CRules[602]),
    (FStart: 2021; FEnd: 2023; FRule: @CRules[600]),
    (FStart: 2021; FEnd: 2023; FRule: @CRules[601]),
    (FStart: 2024; FEnd: 2024; FRule: @CRules[596]),
    (FStart: 2024; FEnd: 2024; FRule: @CRules[602]),
    (FStart: 2025; FEnd: 2027; FRule: @CRules[600]),
    (FStart: 2025; FEnd: 2027; FRule: @CRules[601]),
    (FStart: 2028; FEnd: 2029; FRule: @CRules[596]),
    (FStart: 2028; FEnd: 2029; FRule: @CRules[602]),
    (FStart: 2030; FEnd: 2031; FRule: @CRules[600]),
    (FStart: 2030; FEnd: 2031; FRule: @CRules[601]),
    (FStart: 2032; FEnd: 2033; FRule: @CRules[596]),
    (FStart: 2032; FEnd: 2033; FRule: @CRules[602]),
    (FStart: 2034; FEnd: 2035; FRule: @CRules[600]),
    (FStart: 2034; FEnd: 2035; FRule: @CRules[601]),
    (FStart: 2036; FEnd: 2037; FRule: @CRules[596]),
    (FStart: 2036; FEnd: 2037; FRule: @CRules[602]),
    (FStart: 2038; FEnd: 2039; FRule: @CRules[600]),
    (FStart: 2038; FEnd: 2039; FRule: @CRules[601]),
    (FStart: 2040; FEnd: 2041; FRule: @CRules[596]),
    (FStart: 2040; FEnd: 2041; FRule: @CRules[602]),
    (FStart: 2042; FEnd: 2043; FRule: @CRules[600]),
    (FStart: 2042; FEnd: 2043; FRule: @CRules[601]),
    (FStart: 2044; FEnd: 2045; FRule: @CRules[596]),
    (FStart: 2044; FEnd: 2045; FRule: @CRules[602]),
    (FStart: 2046; FEnd: 2047; FRule: @CRules[600]),
    (FStart: 2046; FEnd: 2047; FRule: @CRules[601]),
    (FStart: 2048; FEnd: 2049; FRule: @CRules[596]),
    (FStart: 2048; FEnd: 2049; FRule: @CRules[602]),
    (FStart: 2050; FEnd: 2051; FRule: @CRules[600]),
    (FStart: 2050; FEnd: 2051; FRule: @CRules[601]),
    (FStart: 2052; FEnd: 2053; FRule: @CRules[596]),
    (FStart: 2052; FEnd: 2053; FRule: @CRules[602]),
    (FStart: 2054; FEnd: 2055; FRule: @CRules[600]),
    (FStart: 2054; FEnd: 2055; FRule: @CRules[601]),
    (FStart: 2056; FEnd: 2057; FRule: @CRules[596]),
    (FStart: 2056; FEnd: 2057; FRule: @CRules[602]),
    (FStart: 2058; FEnd: 2059; FRule: @CRules[600]),
    (FStart: 2058; FEnd: 2059; FRule: @CRules[601]),
    (FStart: 2060; FEnd: 2062; FRule: @CRules[596]),
    (FStart: 2060; FEnd: 2062; FRule: @CRules[602]),
    (FStart: 2063; FEnd: 2063; FRule: @CRules[600]),
    (FStart: 2063; FEnd: 2063; FRule: @CRules[601]),
    (FStart: 2064; FEnd: 2066; FRule: @CRules[596]),
    (FStart: 2064; FEnd: 2066; FRule: @CRules[602]),
    (FStart: 2067; FEnd: 2067; FRule: @CRules[600]),
    (FStart: 2067; FEnd: 2067; FRule: @CRules[601]),
    (FStart: 2068; FEnd: 2070; FRule: @CRules[596]),
    (FStart: 2068; FEnd: 2070; FRule: @CRules[602]),
    (FStart: 2071; FEnd: 2071; FRule: @CRules[600]),
    (FStart: 2071; FEnd: 2071; FRule: @CRules[601]),
    (FStart: 2072; FEnd: 2074; FRule: @CRules[596]),
    (FStart: 2072; FEnd: 2074; FRule: @CRules[602]),
    (FStart: 2075; FEnd: 2075; FRule: @CRules[600]),
    (FStart: 2075; FEnd: 2075; FRule: @CRules[601]),
    (FStart: 2076; FEnd: 2078; FRule: @CRules[596]),
    (FStart: 2076; FEnd: 2078; FRule: @CRules[602]),
    (FStart: 2079; FEnd: 2079; FRule: @CRules[600]),
    (FStart: 2079; FEnd: 2079; FRule: @CRules[601]),
    (FStart: 2080; FEnd: 2082; FRule: @CRules[596]),
    (FStart: 2080; FEnd: 2082; FRule: @CRules[602]),
    (FStart: 2083; FEnd: 2083; FRule: @CRules[600]),
    (FStart: 2083; FEnd: 2083; FRule: @CRules[601]),
    (FStart: 2084; FEnd: 2086; FRule: @CRules[596]),
    (FStart: 2084; FEnd: 2086; FRule: @CRules[602]),
    (FStart: 2087; FEnd: 2087; FRule: @CRules[600]),
    (FStart: 2087; FEnd: 2087; FRule: @CRules[601]),
    (FStart: 2088; FEnd: 9999; FRule: @CRules[596]),
    (FStart: 2088; FEnd: 9999; FRule: @CRules[602])
  );

  { Date-bound rules for Iraq family }
  CFamily_67_Arr: array[0 .. 7] of TYearBoundRule = (
    (FStart: 1982; FEnd: 1982; FRule: @CRules[603]),
    (FStart: 1982; FEnd: 1984; FRule: @CRules[9]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[604]),
    (FStart: 1984; FEnd: 1985; FRule: @CRules[533]),
    (FStart: 1985; FEnd: 1990; FRule: @CRules[196]),
    (FStart: 1986; FEnd: 1990; FRule: @CRules[605]),
    (FStart: 1991; FEnd: 2007; FRule: @CRules[606]),
    (FStart: 1991; FEnd: 2007; FRule: @CRules[607])
  );

  { Date-bound rules for Japan family }
  CFamily_68_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1948; FEnd: 1948; FRule: @CRules[681]),
    (FStart: 1948; FEnd: 1951; FRule: @CRules[682]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[683]),
    (FStart: 1950; FEnd: 1951; FRule: @CRules[681])
  );

  { Date-bound rules for Jordan family }
  CFamily_69_Arr: array[0 .. 30] of TYearBoundRule = (
    (FStart: 1973; FEnd: 1973; FRule: @CRules[684]),
    (FStart: 1973; FEnd: 1975; FRule: @CRules[9]),
    (FStart: 1974; FEnd: 1977; FRule: @CRules[235]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[362]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[9]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[685]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[241]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[229]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[9]),
    (FStart: 1986; FEnd: 1988; FRule: @CRules[686]),
    (FStart: 1986; FEnd: 1990; FRule: @CRules[687]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[238]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[688]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[689]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[245]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[498]),
    (FStart: 1992; FEnd: 1993; FRule: @CRules[687]),
    (FStart: 1993; FEnd: 1998; FRule: @CRules[686]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[690]),
    (FStart: 1995; FEnd: 1998; FRule: @CRules[690]),
    (FStart: 1999; FEnd: 1999; FRule: @CRules[348]),
    (FStart: 1999; FEnd: 2002; FRule: @CRules[691]),
    (FStart: 2000; FEnd: 2001; FRule: @CRules[692]),
    (FStart: 2002; FEnd: 2012; FRule: @CRules[693]),
    (FStart: 2003; FEnd: 2003; FRule: @CRules[694]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[695]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[691]),
    (FStart: 2006; FEnd: 2011; FRule: @CRules[696]),
    (FStart: 2013; FEnd: 2013; FRule: @CRules[697]),
    (FStart: 2014; FEnd: 9999; FRule: @CRules[693]),
    (FStart: 2014; FEnd: 9999; FRule: @CRules[696])
  );

  { Date-bound rules for Kyrgyz family }
  CFamily_70_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1992; FEnd: 1996; FRule: @CRules[698]),
    (FStart: 1992; FEnd: 1996; FRule: @CRules[216]),
    (FStart: 1997; FEnd: 2005; FRule: @CRules[699]),
    (FStart: 1997; FEnd: 2004; FRule: @CRules[700])
  );

  { Date-bound rules for ROK family }
  CFamily_71_Arr: array[0 .. 13] of TYearBoundRule = (
    (FStart: 1948; FEnd: 1948; FRule: @CRules[539]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[701]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[702]),
    (FStart: 1949; FEnd: 1951; FRule: @CRules[703]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[569]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[704]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[634]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[705]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[706]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[707]),
    (FStart: 1957; FEnd: 1960; FRule: @CRules[708]),
    (FStart: 1957; FEnd: 1960; FRule: @CRules[709]),
    (FStart: 1987; FEnd: 1988; FRule: @CRules[710]),
    (FStart: 1987; FEnd: 1988; FRule: @CRules[711])
  );

  { Date-bound rules for Lebanon family }
  CFamily_72_Arr: array[0 .. 23] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1920; FRule: @CRules[487]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[488]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[489]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[237]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[490]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[491]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[500]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[420]),
    (FStart: 1957; FEnd: 1961; FRule: @CRules[235]),
    (FStart: 1957; FEnd: 1961; FRule: @CRules[9]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[712]),
    (FStart: 1972; FEnd: 1977; FRule: @CRules[9]),
    (FStart: 1973; FEnd: 1977; FRule: @CRules[235]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[685]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[241]),
    (FStart: 1984; FEnd: 1987; FRule: @CRules[235]),
    (FStart: 1984; FEnd: 1991; FRule: @CRules[713]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[405]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[714]),
    (FStart: 1990; FEnd: 1992; FRule: @CRules[235]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[243]),
    (FStart: 1993; FEnd: 9999; FRule: @CRules[217]),
    (FStart: 1993; FEnd: 1998; FRule: @CRules[216]),
    (FStart: 1999; FEnd: 9999; FRule: @CRules[218])
  );

  { Date-bound rules for NBorneo family }
  CFamily_73_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1935; FEnd: 1941; FRule: @CRules[715]),
    (FStart: 1935; FEnd: 1941; FRule: @CRules[716])
  );

  { Date-bound rules for Mongol family }
  CFamily_74_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1983; FEnd: 1984; FRule: @CRules[533]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[9]),
    (FStart: 1985; FEnd: 1998; FRule: @CRules[532]),
    (FStart: 1984; FEnd: 1998; FRule: @CRules[216]),
    (FStart: 2001; FEnd: 2001; FRule: @CRules[717]),
    (FStart: 2001; FEnd: 2006; FRule: @CRules[718]),
    (FStart: 2002; FEnd: 2006; FRule: @CRules[719]),
    (FStart: 2015; FEnd: 2016; FRule: @CRules[719]),
    (FStart: 2015; FEnd: 2016; FRule: @CRules[720])
  );

  { Date-bound rules for Pakistan family }
  CFamily_75_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 2002; FEnd: 2002; FRule: @CRules[466]),
    (FStart: 2002; FEnd: 2002; FRule: @CRules[497]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[405]),
    (FStart: 2008; FEnd: 2009; FRule: @CRules[362]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[470])
  );

  { Date-bound rules for EgyptAsia family }
  CFamily_76_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1957; FEnd: 1957; FRule: @CRules[714]),
    (FStart: 1957; FEnd: 1958; FRule: @CRules[9]),
    (FStart: 1958; FEnd: 1958; FRule: @CRules[235]),
    (FStart: 1959; FEnd: 1967; FRule: @CRules[721]),
    (FStart: 1959; FEnd: 1965; FRule: @CRules[722]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[607])
  );

  { Date-bound rules for Palestine family }
  CFamily_77_Arr: array[0 .. 23] of TYearBoundRule = (
    (FStart: 1999; FEnd: 2005; FRule: @CRules[723]),
    (FStart: 1999; FEnd: 2003; FRule: @CRules[724]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[197]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[206]),
    (FStart: 2006; FEnd: 2007; FRule: @CRules[229]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[725]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[726]),
    (FStart: 2008; FEnd: 2009; FRule: @CRules[727]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[118]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[728]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[490]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[729]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[730]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[7]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[731]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[241]),
    (FStart: 2012; FEnd: 2014; FRule: @CRules[693]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[732]),
    (FStart: 2013; FEnd: 2013; FRule: @CRules[733]),
    (FStart: 2014; FEnd: 2015; FRule: @CRules[734]),
    (FStart: 2015; FEnd: 2015; FRule: @CRules[735]),
    (FStart: 2016; FEnd: 2018; FRule: @CRules[736]),
    (FStart: 2016; FEnd: 9999; FRule: @CRules[737]),
    (FStart: 2019; FEnd: 9999; FRule: @CRules[727])
  );

  { Date-bound rules for Phil family }
  CFamily_78_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1936; FEnd: 1936; FRule: @CRules[738]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[739]),
    (FStart: 1954; FEnd: 1954; FRule: @CRules[740]),
    (FStart: 1954; FEnd: 1954; FRule: @CRules[741]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[742]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[743])
  );

  { Date-bound rules for Syria family }
  CFamily_79_Arr: array[0 .. 40] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1923; FRule: @CRules[291]),
    (FStart: 1920; FEnd: 1923; FRule: @CRules[251]),
    (FStart: 1962; FEnd: 1962; FRule: @CRules[744]),
    (FStart: 1962; FEnd: 1962; FRule: @CRules[212]),
    (FStart: 1963; FEnd: 1965; FRule: @CRules[297]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[745]),
    (FStart: 1964; FEnd: 1964; FRule: @CRules[212]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[745]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[746]),
    (FStart: 1966; FEnd: 1976; FRule: @CRules[212]),
    (FStart: 1967; FEnd: 1978; FRule: @CRules[297]),
    (FStart: 1977; FEnd: 1978; FRule: @CRules[298]),
    (FStart: 1983; FEnd: 1984; FRule: @CRules[292]),
    (FStart: 1983; FEnd: 1984; FRule: @CRules[212]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[747]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[748]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[749]),
    (FStart: 1987; FEnd: 1988; FRule: @CRules[750]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[751]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[277]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[212]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[203]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[745]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[229]),
    (FStart: 1991; FEnd: 1992; FRule: @CRules[9]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[752]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[490]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[463]),
    (FStart: 1994; FEnd: 1996; FRule: @CRules[229]),
    (FStart: 1994; FEnd: 2005; FRule: @CRules[9]),
    (FStart: 1997; FEnd: 1998; FRule: @CRules[753]),
    (FStart: 1999; FEnd: 2006; FRule: @CRules[229]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[725]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[727]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[754]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[686]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[362]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[727]),
    (FStart: 2010; FEnd: 2011; FRule: @CRules[686]),
    (FStart: 2012; FEnd: 9999; FRule: @CRules[727]),
    (FStart: 2009; FEnd: 9999; FRule: @CRules[696])
  );

  { Date-bound rules for US family }
  CFamily_80_Arr: array[0 .. 12] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1919; FRule: @CRules[303]),
    (FStart: 1918; FEnd: 1919; FRule: @CRules[139]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[757]),
    (FStart: 1967; FEnd: 2006; FRule: @CRules[139]),
    (FStart: 1967; FEnd: 1973; FRule: @CRules[138]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[140]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[758]),
    (FStart: 1976; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1987; FEnd: 2006; FRule: @CRules[305]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[306]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[307])
  );

  { Date-bound rules for NYC family }
  CFamily_81_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1920; FRule: @CRules[303]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[139]),
    (FStart: 1921; FEnd: 1966; FRule: @CRules[138]),
    (FStart: 1921; FEnd: 1954; FRule: @CRules[304]),
    (FStart: 1955; FEnd: 1966; FRule: @CRules[139])
  );

  { Date-bound rules for Chicago family }
  CFamily_82_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1920; FRule: @CRules[759]),
    (FStart: 1920; FEnd: 1921; FRule: @CRules[139]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[303]),
    (FStart: 1922; FEnd: 1966; FRule: @CRules[138]),
    (FStart: 1922; FEnd: 1954; FRule: @CRules[304]),
    (FStart: 1955; FEnd: 1966; FRule: @CRules[139])
  );

  { Date-bound rules for Denver family }
  CFamily_83_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1921; FRule: @CRules[303]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[139]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[760]),
    (FStart: 1965; FEnd: 1966; FRule: @CRules[138]),
    (FStart: 1965; FEnd: 1966; FRule: @CRules[139])
  );

  { Date-bound rules for CA family }
  CFamily_84_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1948; FEnd: 1948; FRule: @CRules[761]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[762]),
    (FStart: 1950; FEnd: 1966; FRule: @CRules[763]),
    (FStart: 1950; FEnd: 1961; FRule: @CRules[304]),
    (FStart: 1962; FEnd: 1966; FRule: @CRules[139])
  );

  { Date-bound rules for Indianapolis family }
  CFamily_85_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1941; FEnd: 1941; FRule: @CRules[764]),
    (FStart: 1941; FEnd: 1954; FRule: @CRules[304]),
    (FStart: 1946; FEnd: 1954; FRule: @CRules[138])
  );

  { Date-bound rules for Marengo family }
  CFamily_86_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1951; FEnd: 1951; FRule: @CRules[138]),
    (FStart: 1951; FEnd: 1951; FRule: @CRules[304]),
    (FStart: 1954; FEnd: 1960; FRule: @CRules[138]),
    (FStart: 1954; FEnd: 1960; FRule: @CRules[304])
  );

  { Date-bound rules for Vincennes family }
  CFamily_87_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1946; FEnd: 1946; FRule: @CRules[138]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[304]),
    (FStart: 1953; FEnd: 1954; FRule: @CRules[138]),
    (FStart: 1953; FEnd: 1959; FRule: @CRules[304]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[549]),
    (FStart: 1956; FEnd: 1963; FRule: @CRules[138]),
    (FStart: 1960; FEnd: 1960; FRule: @CRules[139]),
    (FStart: 1961; FEnd: 1961; FRule: @CRules[304]),
    (FStart: 1962; FEnd: 1963; FRule: @CRules[139])
  );

  { Date-bound rules for Perry family }
  CFamily_88_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1955; FEnd: 1955; FRule: @CRules[549]),
    (FStart: 1955; FEnd: 1960; FRule: @CRules[304]),
    (FStart: 1956; FEnd: 1963; FRule: @CRules[138]),
    (FStart: 1961; FEnd: 1963; FRule: @CRules[139])
  );

  { Date-bound rules for Pike family }
  CFamily_89_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1955; FEnd: 1955; FRule: @CRules[549]),
    (FStart: 1955; FEnd: 1960; FRule: @CRules[304]),
    (FStart: 1956; FEnd: 1964; FRule: @CRules[138]),
    (FStart: 1961; FEnd: 1964; FRule: @CRules[139])
  );

  { Date-bound rules for Starke family }
  CFamily_90_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1947; FEnd: 1961; FRule: @CRules[138]),
    (FStart: 1947; FEnd: 1954; FRule: @CRules[304]),
    (FStart: 1955; FEnd: 1956; FRule: @CRules[139]),
    (FStart: 1957; FEnd: 1958; FRule: @CRules[304]),
    (FStart: 1959; FEnd: 1961; FRule: @CRules[139])
  );

  { Date-bound rules for Pulaski family }
  CFamily_91_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1946; FEnd: 1960; FRule: @CRules[138]),
    (FStart: 1946; FEnd: 1954; FRule: @CRules[304]),
    (FStart: 1955; FEnd: 1956; FRule: @CRules[139]),
    (FStart: 1957; FEnd: 1960; FRule: @CRules[304])
  );

  { Date-bound rules for Louisville family }
  CFamily_92_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1921; FEnd: 1921; FRule: @CRules[523]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[765]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[138]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[304]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[766]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[767]),
    (FStart: 1950; FEnd: 1961; FRule: @CRules[138]),
    (FStart: 1950; FEnd: 1955; FRule: @CRules[304]),
    (FStart: 1956; FEnd: 1961; FRule: @CRules[139])
  );

  { Date-bound rules for Detroit family }
  CFamily_93_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1948; FEnd: 1948; FRule: @CRules[138]),
    (FStart: 1948; FEnd: 1948; FRule: @CRules[304])
  );

  { Date-bound rules for Menominee family }
  CFamily_94_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1946; FEnd: 1946; FRule: @CRules[138]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[304]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[138]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[139])
  );

  { Date-bound rules for StJohns family }
  CFamily_95_Arr: array[0 .. 18] of TYearBoundRule = (
    (FStart: 1917; FEnd: 1917; FRule: @CRules[769]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[770]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[771]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[772]),
    (FStart: 1920; FEnd: 1935; FRule: @CRules[773]),
    (FStart: 1920; FEnd: 1935; FRule: @CRules[774]),
    (FStart: 1936; FEnd: 1941; FRule: @CRules[775]),
    (FStart: 1936; FEnd: 1941; FRule: @CRules[776]),
    (FStart: 1946; FEnd: 1950; FRule: @CRules[710]),
    (FStart: 1946; FEnd: 1950; FRule: @CRules[777]),
    (FStart: 1951; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1951; FEnd: 1959; FRule: @CRules[304]),
    (FStart: 1960; FEnd: 1986; FRule: @CRules[139]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[778]),
    (FStart: 1987; FEnd: 2006; FRule: @CRules[779]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[780]),
    (FStart: 1989; FEnd: 2006; FRule: @CRules[778]),
    (FStart: 2007; FEnd: 2011; FRule: @CRules[781]),
    (FStart: 2007; FEnd: 2010; FRule: @CRules[782])
  );

  { Date-bound rules for Halifax family }
  CFamily_96_Arr: array[0 .. 40] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[569]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[566]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[783]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[784]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[704]),
    (FStart: 1921; FEnd: 1922; FRule: @CRules[652]),
    (FStart: 1922; FEnd: 1922; FRule: @CRules[643]),
    (FStart: 1923; FEnd: 1925; FRule: @CRules[708]),
    (FStart: 1923; FEnd: 1923; FRule: @CRules[642]),
    (FStart: 1924; FEnd: 1924; FRule: @CRules[637]),
    (FStart: 1925; FEnd: 1925; FRule: @CRules[785]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[786]),
    (FStart: 1926; FEnd: 1926; FRule: @CRules[640]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[549]),
    (FStart: 1927; FEnd: 1927; FRule: @CRules[787]),
    (FStart: 1928; FEnd: 1931; FRule: @CRules[788]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[789]),
    (FStart: 1929; FEnd: 1929; FRule: @CRules[644]),
    (FStart: 1930; FEnd: 1930; FRule: @CRules[637]),
    (FStart: 1931; FEnd: 1932; FRule: @CRules[790]),
    (FStart: 1932; FEnd: 1932; FRule: @CRules[549]),
    (FStart: 1933; FEnd: 1933; FRule: @CRules[643]),
    (FStart: 1933; FEnd: 1933; FRule: @CRules[791]),
    (FStart: 1934; FEnd: 1934; FRule: @CRules[706]),
    (FStart: 1934; FEnd: 1934; FRule: @CRules[655]),
    (FStart: 1935; FEnd: 1935; FRule: @CRules[792]),
    (FStart: 1935; FEnd: 1935; FRule: @CRules[793]),
    (FStart: 1936; FEnd: 1936; FRule: @CRules[539]),
    (FStart: 1936; FEnd: 1936; FRule: @CRules[657]),
    (FStart: 1937; FEnd: 1938; FRule: @CRules[708]),
    (FStart: 1937; FEnd: 1941; FRule: @CRules[790]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[794]),
    (FStart: 1940; FEnd: 1941; FRule: @CRules[708]),
    (FStart: 1946; FEnd: 1949; FRule: @CRules[138]),
    (FStart: 1946; FEnd: 1949; FRule: @CRules[304]),
    (FStart: 1951; FEnd: 1954; FRule: @CRules[138]),
    (FStart: 1951; FEnd: 1954; FRule: @CRules[304]),
    (FStart: 1956; FEnd: 1959; FRule: @CRules[138]),
    (FStart: 1956; FEnd: 1959; FRule: @CRules[304]),
    (FStart: 1962; FEnd: 1973; FRule: @CRules[138]),
    (FStart: 1962; FEnd: 1973; FRule: @CRules[139])
  );

  { Date-bound rules for Moncton family }
  CFamily_97_Arr: array[0 .. 12] of TYearBoundRule = (
    (FStart: 1933; FEnd: 1935; FRule: @CRules[795]),
    (FStart: 1933; FEnd: 1935; FRule: @CRules[796]),
    (FStart: 1936; FEnd: 1938; FRule: @CRules[797]),
    (FStart: 1936; FEnd: 1938; FRule: @CRules[798]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[799]),
    (FStart: 1939; FEnd: 1941; FRule: @CRules[800]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[801]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[802]),
    (FStart: 1946; FEnd: 1972; FRule: @CRules[138]),
    (FStart: 1946; FEnd: 1956; FRule: @CRules[304]),
    (FStart: 1957; FEnd: 1972; FRule: @CRules[139]),
    (FStart: 1993; FEnd: 2006; FRule: @CRules[778]),
    (FStart: 1993; FEnd: 2006; FRule: @CRules[779])
  );

  { Date-bound rules for Toronto family }
  CFamily_98_Arr: array[0 .. 21] of TYearBoundRule = (
    (FStart: 1919; FEnd: 1919; FRule: @CRules[803]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[804]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[805]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[787]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[806]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[807]),
    (FStart: 1922; FEnd: 1923; FRule: @CRules[710]),
    (FStart: 1922; FEnd: 1926; FRule: @CRules[808]),
    (FStart: 1924; FEnd: 1927; FRule: @CRules[527]),
    (FStart: 1927; FEnd: 1937; FRule: @CRules[809]),
    (FStart: 1928; FEnd: 1937; FRule: @CRules[810]),
    (FStart: 1938; FEnd: 1940; FRule: @CRules[138]),
    (FStart: 1938; FEnd: 1939; FRule: @CRules[304]),
    (FStart: 1945; FEnd: 1946; FRule: @CRules[304]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[138]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[530]),
    (FStart: 1947; FEnd: 1948; FRule: @CRules[531]),
    (FStart: 1949; FEnd: 1949; FRule: @CRules[811]),
    (FStart: 1950; FEnd: 1973; FRule: @CRules[138]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[141]),
    (FStart: 1951; FEnd: 1956; FRule: @CRules[304]),
    (FStart: 1957; FEnd: 1973; FRule: @CRules[139])
  );

  { Date-bound rules for Winn family }
  CFamily_99_Arr: array[0 .. 23] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[812]),
    (FStart: 1916; FEnd: 1916; FRule: @CRules[813]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[661]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[768]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[814]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[815]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[304]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[816]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[817]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[138]),
    (FStart: 1947; FEnd: 1949; FRule: @CRules[304]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[523]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[757]),
    (FStart: 1951; FEnd: 1960; FRule: @CRules[138]),
    (FStart: 1951; FEnd: 1958; FRule: @CRules[304]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[139]),
    (FStart: 1960; FEnd: 1960; FRule: @CRules[304]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[138]),
    (FStart: 1963; FEnd: 1963; FRule: @CRules[818]),
    (FStart: 1966; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1966; FEnd: 2005; FRule: @CRules[139]),
    (FStart: 1987; FEnd: 2005; FRule: @CRules[305])
  );

  { Date-bound rules for Regina family }
  CFamily_100_Arr: array[0 .. 16] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[661]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[768]),
    (FStart: 1930; FEnd: 1934; FRule: @CRules[708]),
    (FStart: 1930; FEnd: 1934; FRule: @CRules[819]),
    (FStart: 1937; FEnd: 1941; FRule: @CRules[820]),
    (FStart: 1937; FEnd: 1937; FRule: @CRules[821]),
    (FStart: 1938; FEnd: 1938; FRule: @CRules[819]),
    (FStart: 1939; FEnd: 1941; FRule: @CRules[821]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[304]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[822]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[823]),
    (FStart: 1947; FEnd: 1957; FRule: @CRules[138]),
    (FStart: 1947; FEnd: 1957; FRule: @CRules[304]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[138]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[139])
  );

  { Date-bound rules for Swift family }
  CFamily_101_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1957; FEnd: 1957; FRule: @CRules[138]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[139]),
    (FStart: 1959; FEnd: 1961; FRule: @CRules[138]),
    (FStart: 1959; FEnd: 1959; FRule: @CRules[139]),
    (FStart: 1960; FEnd: 1961; FRule: @CRules[304])
  );

  { Date-bound rules for Edm family }
  CFamily_102_Arr: array[0 .. 12] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1919; FRule: @CRules[822]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[768]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[824]),
    (FStart: 1920; FEnd: 1923; FRule: @CRules[138]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[139]),
    (FStart: 1921; FEnd: 1923; FRule: @CRules[304]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[304]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[138]),
    (FStart: 1947; FEnd: 1947; FRule: @CRules[304]),
    (FStart: 1972; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1972; FEnd: 2006; FRule: @CRules[139])
  );

  { Date-bound rules for Vanc family }
  CFamily_103_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1918; FRule: @CRules[661]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[768]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[755]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[756]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[757]),
    (FStart: 1946; FEnd: 1986; FRule: @CRules[138]),
    (FStart: 1946; FEnd: 1946; FRule: @CRules[825]),
    (FStart: 1947; FEnd: 1961; FRule: @CRules[304]),
    (FStart: 1962; FEnd: 2006; FRule: @CRules[139])
  );

  { Date-bound rules for Bahamas family }
  CFamily_104_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1964; FEnd: 1975; FRule: @CRules[139]),
    (FStart: 1964; FEnd: 1975; FRule: @CRules[138])
  );

  { Date-bound rules for Barb family }
  CFamily_105_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1977; FEnd: 1977; FRule: @CRules[836]),
    (FStart: 1977; FEnd: 1978; FRule: @CRules[837]),
    (FStart: 1978; FEnd: 1980; FRule: @CRules[838]),
    (FStart: 1979; FEnd: 1979; FRule: @CRules[757]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[839])
  );

  { Date-bound rules for Belize family }
  CFamily_106_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1918; FEnd: 1942; FRule: @CRules[840]),
    (FStart: 1919; FEnd: 1943; FRule: @CRules[841]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[842]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[843]),
    (FStart: 1982; FEnd: 1982; FRule: @CRules[844]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[845])
  );

  { Date-bound rules for CR family }
  CFamily_107_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 1979; FEnd: 1980; FRule: @CRules[846]),
    (FStart: 1979; FEnd: 1980; FRule: @CRules[847]),
    (FStart: 1991; FEnd: 1992; FRule: @CRules[848]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[741]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[849])
  );

  { Date-bound rules for Cuba family }
  CFamily_108_Arr: array[0 .. 38] of TYearBoundRule = (
    (FStart: 1928; FEnd: 1928; FRule: @CRules[850]),
    (FStart: 1928; FEnd: 1928; FRule: @CRules[851]),
    (FStart: 1940; FEnd: 1942; FRule: @CRules[852]),
    (FStart: 1940; FEnd: 1942; FRule: @CRules[853]),
    (FStart: 1945; FEnd: 1946; FRule: @CRules[852]),
    (FStart: 1945; FEnd: 1946; FRule: @CRules[853]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[539]),
    (FStart: 1965; FEnd: 1965; FRule: @CRules[793]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[854]),
    (FStart: 1966; FEnd: 1966; FRule: @CRules[791]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[855]),
    (FStart: 1967; FEnd: 1968; FRule: @CRules[856]),
    (FStart: 1968; FEnd: 1968; FRule: @CRules[636]),
    (FStart: 1969; FEnd: 1977; FRule: @CRules[530]),
    (FStart: 1969; FEnd: 1971; FRule: @CRules[857]),
    (FStart: 1972; FEnd: 1974; FRule: @CRules[858]),
    (FStart: 1975; FEnd: 1977; FRule: @CRules[857]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[859]),
    (FStart: 1978; FEnd: 1990; FRule: @CRules[821]),
    (FStart: 1979; FEnd: 1980; FRule: @CRules[860]),
    (FStart: 1981; FEnd: 1985; FRule: @CRules[861]),
    (FStart: 1986; FEnd: 1989; FRule: @CRules[862]),
    (FStart: 1990; FEnd: 1997; FRule: @CRules[863]),
    (FStart: 1991; FEnd: 1995; FRule: @CRules[821]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[864]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[865]),
    (FStart: 1998; FEnd: 1999; FRule: @CRules[866]),
    (FStart: 1998; FEnd: 2003; FRule: @CRules[857]),
    (FStart: 2000; FEnd: 2003; FRule: @CRules[863]),
    (FStart: 2004; FEnd: 2004; FRule: @CRules[866]),
    (FStart: 2006; FEnd: 2010; FRule: @CRules[857]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[867]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[860]),
    (FStart: 2009; FEnd: 2010; FRule: @CRules[867]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[860]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[868]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[569]),
    (FStart: 2012; FEnd: 9999; FRule: @CRules[869]),
    (FStart: 2013; FEnd: 9999; FRule: @CRules[867])
  );

  { Date-bound rules for DR family }
  CFamily_109_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1966; FEnd: 1966; FRule: @CRules[870]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[871]),
    (FStart: 1969; FEnd: 1973; FRule: @CRules[872]),
    (FStart: 1970; FEnd: 1970; FRule: @CRules[873]),
    (FStart: 1971; FEnd: 1971; FRule: @CRules[874]),
    (FStart: 1972; FEnd: 1974; FRule: @CRules[875])
  );

  { Date-bound rules for Salv family }
  CFamily_110_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1987; FEnd: 1988; FRule: @CRules[708]),
    (FStart: 1987; FEnd: 1988; FRule: @CRules[531])
  );

  { Date-bound rules for Guat family }
  CFamily_111_Arr: array[0 .. 7] of TYearBoundRule = (
    (FStart: 1973; FEnd: 1973; FRule: @CRules[876]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[877]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[878]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[627]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[879]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[639]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[643]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[566])
  );

  { Date-bound rules for Haiti family }
  CFamily_112_Arr: array[0 .. 10] of TYearBoundRule = (
    (FStart: 1983; FEnd: 1983; FRule: @CRules[880]),
    (FStart: 1984; FEnd: 1987; FRule: @CRules[530]),
    (FStart: 1983; FEnd: 1987; FRule: @CRules[857]),
    (FStart: 1988; FEnd: 1997; FRule: @CRules[881]),
    (FStart: 1988; FEnd: 1997; FRule: @CRules[882]),
    (FStart: 2005; FEnd: 2006; FRule: @CRules[863]),
    (FStart: 2005; FEnd: 2006; FRule: @CRules[857]),
    (FStart: 2012; FEnd: 2015; FRule: @CRules[306]),
    (FStart: 2012; FEnd: 2015; FRule: @CRules[307]),
    (FStart: 2017; FEnd: 9999; FRule: @CRules[306]),
    (FStart: 2017; FEnd: 9999; FRule: @CRules[307])
  );

  { Date-bound rules for Hond family }
  CFamily_113_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1987; FEnd: 1988; FRule: @CRules[708]),
    (FStart: 1987; FEnd: 1988; FRule: @CRules[531]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[708]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[883])
  );

  { Date-bound rules for Nic family }
  CFamily_114_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1979; FEnd: 1980; FRule: @CRules[884]),
    (FStart: 1979; FEnd: 1980; FRule: @CRules[885]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[641]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[819]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[524]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[886])
  );

  { Date-bound rules for Algeria family }
  CFamily_115_Arr: array[0 .. 21] of TYearBoundRule = (
    (FStart: 1916; FEnd: 1916; FRule: @CRules[311]),
    (FStart: 1916; FEnd: 1919; FRule: @CRules[312]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[313]),
    (FStart: 1918; FEnd: 1918; FRule: @CRules[314]),
    (FStart: 1919; FEnd: 1919; FRule: @CRules[258]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[259]),
    (FStart: 1920; FEnd: 1920; FRule: @CRules[260]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[261]),
    (FStart: 1921; FEnd: 1921; FRule: @CRules[887]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[888]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[889]),
    (FStart: 1944; FEnd: 1945; FRule: @CRules[207]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[890]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[891]),
    (FStart: 1971; FEnd: 1971; FRule: @CRules[892]),
    (FStart: 1971; FEnd: 1971; FRule: @CRules[893]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[239]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[894]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[895]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[896]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[897]),
    (FStart: 1980; FEnd: 1980; FRule: @CRules[750])
  );

  { Date-bound rules for Egypt family }
  CFamily_116_Arr: array[0 .. 31] of TYearBoundRule = (
    (FStart: 1940; FEnd: 1940; FRule: @CRules[501]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[9]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[470]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[420]),
    (FStart: 1942; FEnd: 1944; FRule: @CRules[229]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[898]),
    (FStart: 1943; FEnd: 1945; FRule: @CRules[362]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[499]),
    (FStart: 1957; FEnd: 1957; FRule: @CRules[714]),
    (FStart: 1957; FEnd: 1958; FRule: @CRules[9]),
    (FStart: 1958; FEnd: 1958; FRule: @CRules[235]),
    (FStart: 1959; FEnd: 1981; FRule: @CRules[721]),
    (FStart: 1959; FEnd: 1965; FRule: @CRules[722]),
    (FStart: 1966; FEnd: 1994; FRule: @CRules[607]),
    (FStart: 1982; FEnd: 1982; FRule: @CRules[899]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[900]),
    (FStart: 1984; FEnd: 1988; FRule: @CRules[721]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[901]),
    (FStart: 1990; FEnd: 1994; FRule: @CRules[721]),
    (FStart: 1995; FEnd: 2010; FRule: @CRules[902]),
    (FStart: 1995; FEnd: 2005; FRule: @CRules[903]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[601]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[904]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[905]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[906]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[907]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[908]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[903]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[909]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[910]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[911]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[903])
  );

  { Date-bound rules for Ghana family }
  CFamily_117_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1920; FEnd: 1942; FRule: @CRules[912]),
    (FStart: 1920; FEnd: 1942; FRule: @CRules[913])
  );

  { Date-bound rules for Libya family }
  CFamily_118_Arr: array[0 .. 16] of TYearBoundRule = (
    (FStart: 1951; FEnd: 1951; FRule: @CRules[914]),
    (FStart: 1952; FEnd: 1952; FRule: @CRules[82]),
    (FStart: 1953; FEnd: 1953; FRule: @CRules[915]),
    (FStart: 1954; FEnd: 1954; FRule: @CRules[82]),
    (FStart: 1955; FEnd: 1955; FRule: @CRules[916]),
    (FStart: 1956; FEnd: 1956; FRule: @CRules[82]),
    (FStart: 1982; FEnd: 1984; FRule: @CRules[229]),
    (FStart: 1982; FEnd: 1985; FRule: @CRules[9]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[254]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[917]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[237]),
    (FStart: 1987; FEnd: 1989; FRule: @CRules[229]),
    (FStart: 1987; FEnd: 1989; FRule: @CRules[9]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[917]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[243]),
    (FStart: 2013; FEnd: 2013; FRule: @CRules[918]),
    (FStart: 2013; FEnd: 2013; FRule: @CRules[919])
  );

  { Date-bound rules for Mauritius family }
  CFamily_119_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1982; FEnd: 1982; FRule: @CRules[920]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[921]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[922]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[923])
  );

  { Date-bound rules for Morocco family }
  CFamily_120_Arr: array[0 .. 182] of TYearBoundRule = (
    (FStart: 1939; FEnd: 1939; FRule: @CRules[924]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[925]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[926]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[927]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[928]),
    (FStart: 1950; FEnd: 1950; FRule: @CRules[929]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[930]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[9]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[931]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[118]),
    (FStart: 1976; FEnd: 1977; FRule: @CRules[603]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[7]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[255]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[932]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[484]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[932]),
    (FStart: 2008; FEnd: 2008; FRule: @CRules[118]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[932]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[933]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[934]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[935]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[936]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[937]),
    (FStart: 2012; FEnd: 2013; FRule: @CRules[938]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[939]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[940]),
    (FStart: 2012; FEnd: 2012; FRule: @CRules[722]),
    (FStart: 2013; FEnd: 2013; FRule: @CRules[941]),
    (FStart: 2013; FEnd: 2013; FRule: @CRules[942]),
    (FStart: 2013; FEnd: 2018; FRule: @CRules[422]),
    (FStart: 2014; FEnd: 2018; FRule: @CRules[534]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[943]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[944]),
    (FStart: 2015; FEnd: 2015; FRule: @CRules[945]),
    (FStart: 2015; FEnd: 2015; FRule: @CRules[946]),
    (FStart: 2016; FEnd: 2016; FRule: @CRules[947]),
    (FStart: 2016; FEnd: 2016; FRule: @CRules[948]),
    (FStart: 2017; FEnd: 2017; FRule: @CRules[949]),
    (FStart: 2017; FEnd: 2017; FRule: @CRules[950]),
    (FStart: 2018; FEnd: 2018; FRule: @CRules[951]),
    (FStart: 2018; FEnd: 2018; FRule: @CRules[952]),
    (FStart: 2019; FEnd: 2019; FRule: @CRules[953]),
    (FStart: 2019; FEnd: 2019; FRule: @CRules[954]),
    (FStart: 2020; FEnd: 2020; FRule: @CRules[955]),
    (FStart: 2020; FEnd: 2020; FRule: @CRules[956]),
    (FStart: 2021; FEnd: 2021; FRule: @CRules[957]),
    (FStart: 2021; FEnd: 2021; FRule: @CRules[958]),
    (FStart: 2022; FEnd: 2022; FRule: @CRules[959]),
    (FStart: 2022; FEnd: 2022; FRule: @CRules[960]),
    (FStart: 2023; FEnd: 2023; FRule: @CRules[961]),
    (FStart: 2023; FEnd: 2023; FRule: @CRules[962]),
    (FStart: 2024; FEnd: 2024; FRule: @CRules[963]),
    (FStart: 2024; FEnd: 2024; FRule: @CRules[964]),
    (FStart: 2025; FEnd: 2025; FRule: @CRules[965]),
    (FStart: 2025; FEnd: 2025; FRule: @CRules[966]),
    (FStart: 2026; FEnd: 2026; FRule: @CRules[967]),
    (FStart: 2026; FEnd: 2026; FRule: @CRules[968]),
    (FStart: 2027; FEnd: 2027; FRule: @CRules[969]),
    (FStart: 2027; FEnd: 2027; FRule: @CRules[970]),
    (FStart: 2028; FEnd: 2028; FRule: @CRules[971]),
    (FStart: 2028; FEnd: 2028; FRule: @CRules[972]),
    (FStart: 2029; FEnd: 2029; FRule: @CRules[973]),
    (FStart: 2029; FEnd: 2029; FRule: @CRules[974]),
    (FStart: 2029; FEnd: 2029; FRule: @CRules[975]),
    (FStart: 2030; FEnd: 2030; FRule: @CRules[976]),
    (FStart: 2030; FEnd: 2030; FRule: @CRules[977]),
    (FStart: 2031; FEnd: 2031; FRule: @CRules[978]),
    (FStart: 2031; FEnd: 2031; FRule: @CRules[979]),
    (FStart: 2032; FEnd: 2032; FRule: @CRules[980]),
    (FStart: 2032; FEnd: 2032; FRule: @CRules[981]),
    (FStart: 2033; FEnd: 2033; FRule: @CRules[982]),
    (FStart: 2033; FEnd: 2033; FRule: @CRules[983]),
    (FStart: 2033; FEnd: 2033; FRule: @CRules[984]),
    (FStart: 2034; FEnd: 2034; FRule: @CRules[985]),
    (FStart: 2034; FEnd: 2034; FRule: @CRules[986]),
    (FStart: 2035; FEnd: 2035; FRule: @CRules[987]),
    (FStart: 2035; FEnd: 2035; FRule: @CRules[988]),
    (FStart: 2036; FEnd: 2036; FRule: @CRules[989]),
    (FStart: 2036; FEnd: 2036; FRule: @CRules[990]),
    (FStart: 2037; FEnd: 2037; FRule: @CRules[991]),
    (FStart: 2037; FEnd: 2037; FRule: @CRules[992]),
    (FStart: 2038; FEnd: 2038; FRule: @CRules[993]),
    (FStart: 2038; FEnd: 2038; FRule: @CRules[750]),
    (FStart: 2039; FEnd: 2039; FRule: @CRules[994]),
    (FStart: 2039; FEnd: 2039; FRule: @CRules[365]),
    (FStart: 2040; FEnd: 2040; FRule: @CRules[995]),
    (FStart: 2040; FEnd: 2040; FRule: @CRules[996]),
    (FStart: 2041; FEnd: 2041; FRule: @CRules[997]),
    (FStart: 2041; FEnd: 2041; FRule: @CRules[357]),
    (FStart: 2042; FEnd: 2042; FRule: @CRules[998]),
    (FStart: 2042; FEnd: 2042; FRule: @CRules[999]),
    (FStart: 2043; FEnd: 2043; FRule: @CRules[1000]),
    (FStart: 2043; FEnd: 2043; FRule: @CRules[1001]),
    (FStart: 2044; FEnd: 2044; FRule: @CRules[1002]),
    (FStart: 2044; FEnd: 2044; FRule: @CRules[1003]),
    (FStart: 2045; FEnd: 2045; FRule: @CRules[1004]),
    (FStart: 2045; FEnd: 2045; FRule: @CRules[1005]),
    (FStart: 2046; FEnd: 2046; FRule: @CRules[1006]),
    (FStart: 2046; FEnd: 2046; FRule: @CRules[1007]),
    (FStart: 2047; FEnd: 2047; FRule: @CRules[1008]),
    (FStart: 2047; FEnd: 2047; FRule: @CRules[1009]),
    (FStart: 2048; FEnd: 2048; FRule: @CRules[1010]),
    (FStart: 2048; FEnd: 2048; FRule: @CRules[1011]),
    (FStart: 2049; FEnd: 2049; FRule: @CRules[1012]),
    (FStart: 2049; FEnd: 2049; FRule: @CRules[1013]),
    (FStart: 2050; FEnd: 2050; FRule: @CRules[1014]),
    (FStart: 2050; FEnd: 2050; FRule: @CRules[1015]),
    (FStart: 2051; FEnd: 2051; FRule: @CRules[1016]),
    (FStart: 2051; FEnd: 2051; FRule: @CRules[1017]),
    (FStart: 2052; FEnd: 2052; FRule: @CRules[1018]),
    (FStart: 2052; FEnd: 2052; FRule: @CRules[1019]),
    (FStart: 2053; FEnd: 2053; FRule: @CRules[1020]),
    (FStart: 2053; FEnd: 2053; FRule: @CRules[1021]),
    (FStart: 2054; FEnd: 2054; FRule: @CRules[1022]),
    (FStart: 2054; FEnd: 2054; FRule: @CRules[1023]),
    (FStart: 2055; FEnd: 2055; FRule: @CRules[1024]),
    (FStart: 2055; FEnd: 2055; FRule: @CRules[1025]),
    (FStart: 2056; FEnd: 2056; FRule: @CRules[1026]),
    (FStart: 2056; FEnd: 2056; FRule: @CRules[962]),
    (FStart: 2057; FEnd: 2057; FRule: @CRules[1027]),
    (FStart: 2057; FEnd: 2057; FRule: @CRules[1028]),
    (FStart: 2058; FEnd: 2058; FRule: @CRules[1029]),
    (FStart: 2058; FEnd: 2058; FRule: @CRules[1030]),
    (FStart: 2059; FEnd: 2059; FRule: @CRules[1031]),
    (FStart: 2059; FEnd: 2059; FRule: @CRules[1032]),
    (FStart: 2060; FEnd: 2060; FRule: @CRules[1033]),
    (FStart: 2060; FEnd: 2060; FRule: @CRules[1034]),
    (FStart: 2061; FEnd: 2061; FRule: @CRules[1035]),
    (FStart: 2061; FEnd: 2061; FRule: @CRules[972]),
    (FStart: 2062; FEnd: 2062; FRule: @CRules[1036]),
    (FStart: 2062; FEnd: 2062; FRule: @CRules[1037]),
    (FStart: 2062; FEnd: 2062; FRule: @CRules[1038]),
    (FStart: 2063; FEnd: 2063; FRule: @CRules[1039]),
    (FStart: 2063; FEnd: 2063; FRule: @CRules[1040]),
    (FStart: 2064; FEnd: 2064; FRule: @CRules[1041]),
    (FStart: 2064; FEnd: 2064; FRule: @CRules[1042]),
    (FStart: 2065; FEnd: 2065; FRule: @CRules[1043]),
    (FStart: 2065; FEnd: 2065; FRule: @CRules[1044]),
    (FStart: 2066; FEnd: 2066; FRule: @CRules[1045]),
    (FStart: 2066; FEnd: 2066; FRule: @CRules[1046]),
    (FStart: 2066; FEnd: 2066; FRule: @CRules[1047]),
    (FStart: 2067; FEnd: 2067; FRule: @CRules[1048]),
    (FStart: 2067; FEnd: 2067; FRule: @CRules[1049]),
    (FStart: 2068; FEnd: 2068; FRule: @CRules[1050]),
    (FStart: 2068; FEnd: 2068; FRule: @CRules[988]),
    (FStart: 2069; FEnd: 2069; FRule: @CRules[1051]),
    (FStart: 2069; FEnd: 2069; FRule: @CRules[1052]),
    (FStart: 2070; FEnd: 2070; FRule: @CRules[1053]),
    (FStart: 2070; FEnd: 2070; FRule: @CRules[1054]),
    (FStart: 2071; FEnd: 2071; FRule: @CRules[1055]),
    (FStart: 2071; FEnd: 2071; FRule: @CRules[1056]),
    (FStart: 2072; FEnd: 2072; FRule: @CRules[1057]),
    (FStart: 2072; FEnd: 2072; FRule: @CRules[1058]),
    (FStart: 2073; FEnd: 2073; FRule: @CRules[1059]),
    (FStart: 2073; FEnd: 2073; FRule: @CRules[890]),
    (FStart: 2074; FEnd: 2074; FRule: @CRules[1060]),
    (FStart: 2074; FEnd: 2074; FRule: @CRules[1061]),
    (FStart: 2075; FEnd: 2075; FRule: @CRules[1062]),
    (FStart: 2075; FEnd: 2075; FRule: @CRules[1063]),
    (FStart: 2076; FEnd: 2076; FRule: @CRules[1064]),
    (FStart: 2076; FEnd: 2076; FRule: @CRules[1001]),
    (FStart: 2077; FEnd: 2077; FRule: @CRules[1065]),
    (FStart: 2077; FEnd: 2077; FRule: @CRules[1066]),
    (FStart: 2078; FEnd: 2078; FRule: @CRules[1067]),
    (FStart: 2078; FEnd: 2078; FRule: @CRules[1068]),
    (FStart: 2079; FEnd: 2079; FRule: @CRules[1069]),
    (FStart: 2079; FEnd: 2079; FRule: @CRules[1070]),
    (FStart: 2080; FEnd: 2080; FRule: @CRules[1071]),
    (FStart: 2080; FEnd: 2080; FRule: @CRules[1072]),
    (FStart: 2081; FEnd: 2081; FRule: @CRules[1073]),
    (FStart: 2081; FEnd: 2081; FRule: @CRules[1074]),
    (FStart: 2082; FEnd: 2082; FRule: @CRules[1075]),
    (FStart: 2082; FEnd: 2082; FRule: @CRules[1076]),
    (FStart: 2083; FEnd: 2083; FRule: @CRules[1077]),
    (FStart: 2083; FEnd: 2083; FRule: @CRules[1078]),
    (FStart: 2084; FEnd: 2084; FRule: @CRules[1079]),
    (FStart: 2084; FEnd: 2084; FRule: @CRules[1017]),
    (FStart: 2085; FEnd: 2085; FRule: @CRules[1080]),
    (FStart: 2085; FEnd: 2085; FRule: @CRules[1081]),
    (FStart: 2086; FEnd: 2086; FRule: @CRules[1082]),
    (FStart: 2086; FEnd: 2086; FRule: @CRules[1083]),
    (FStart: 2087; FEnd: 2087; FRule: @CRules[1084]),
    (FStart: 2087; FEnd: 2087; FRule: @CRules[1085])
  );

  { Date-bound rules for Namibia family }
  CFamily_121_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1994; FEnd: 1994; FRule: @CRules[1086]),
    (FStart: 1994; FEnd: 2017; FRule: @CRules[1087]),
    (FStart: 1995; FEnd: 2017; FRule: @CRules[1088])
  );

  { Date-bound rules for SA family }
  CFamily_122_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1942; FEnd: 1943; FRule: @CRules[1089]),
    (FStart: 1943; FEnd: 1944; FRule: @CRules[1090])
  );

  { Date-bound rules for Sudan family }
  CFamily_123_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1970; FEnd: 1970; FRule: @CRules[235]),
    (FStart: 1970; FEnd: 1985; FRule: @CRules[695]),
    (FStart: 1971; FEnd: 1971; FRule: @CRules[685]),
    (FStart: 1972; FEnd: 1985; FRule: @CRules[1091])
  );

  { Date-bound rules for Tunisia family }
  CFamily_124_Arr: array[0 .. 24] of TYearBoundRule = (
    (FStart: 1939; FEnd: 1939; FRule: @CRules[324]),
    (FStart: 1939; FEnd: 1939; FRule: @CRules[325]),
    (FStart: 1940; FEnd: 1940; FRule: @CRules[1092]),
    (FStart: 1941; FEnd: 1941; FRule: @CRules[493]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[256]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[231]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[205]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[1093]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[437]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[206]),
    (FStart: 1944; FEnd: 1945; FRule: @CRules[207]),
    (FStart: 1944; FEnd: 1944; FRule: @CRules[491]),
    (FStart: 1945; FEnd: 1945; FRule: @CRules[420]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[685]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[1094]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[235]),
    (FStart: 1978; FEnd: 1978; FRule: @CRules[9]),
    (FStart: 1988; FEnd: 1988; FRule: @CRules[405]),
    (FStart: 1988; FEnd: 1990; FRule: @CRules[216]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[490]),
    (FStart: 1990; FEnd: 1990; FRule: @CRules[235]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[235]),
    (FStart: 2005; FEnd: 2005; FRule: @CRules[1095]),
    (FStart: 2006; FEnd: 2008; FRule: @CRules[213]),
    (FStart: 2006; FEnd: 2008; FRule: @CRules[214])
  );

  { Date-bound rules for Aus family }
  CFamily_125_Arr: array[0 .. 6] of TYearBoundRule = (
    (FStart: 1917; FEnd: 1917; FRule: @CRules[1096]),
    (FStart: 1917; FEnd: 1917; FRule: @CRules[1097]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[1098]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[1099]),
    (FStart: 1942; FEnd: 1942; FRule: @CRules[1100]),
    (FStart: 1943; FEnd: 1944; FRule: @CRules[1101]),
    (FStart: 1943; FEnd: 1943; FRule: @CRules[1102])
  );

  { Date-bound rules for AW family }
  CFamily_126_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1974; FEnd: 1974; FRule: @CRules[1103]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[1104]),
    (FStart: 1983; FEnd: 1983; FRule: @CRules[1103]),
    (FStart: 1984; FEnd: 1984; FRule: @CRules[1104]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[1105]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[1104]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[1106]),
    (FStart: 2007; FEnd: 2009; FRule: @CRules[1101]),
    (FStart: 2007; FEnd: 2008; FRule: @CRules[1103])
  );

  { Date-bound rules for AQ family }
  CFamily_127_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1971; FEnd: 1971; FRule: @CRules[1103]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[1107]),
    (FStart: 1989; FEnd: 1991; FRule: @CRules[1103]),
    (FStart: 1990; FEnd: 1992; FRule: @CRules[1104])
  );

  { Date-bound rules for Holiday family }
  CFamily_128_Arr: array[0 .. 1] of TYearBoundRule = (
    (FStart: 1992; FEnd: 1993; FRule: @CRules[1103]),
    (FStart: 1993; FEnd: 1994; FRule: @CRules[1104])
  );

  { Date-bound rules for AS family }
  CFamily_129_Arr: array[0 .. 14] of TYearBoundRule = (
    (FStart: 1971; FEnd: 1985; FRule: @CRules[1103]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[1108]),
    (FStart: 1987; FEnd: 2007; FRule: @CRules[1103]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[1109]),
    (FStart: 1973; FEnd: 1985; FRule: @CRules[1104]),
    (FStart: 1986; FEnd: 1990; FRule: @CRules[1110]),
    (FStart: 1991; FEnd: 1991; FRule: @CRules[1111]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[1112]),
    (FStart: 1993; FEnd: 1993; FRule: @CRules[1113]),
    (FStart: 1994; FEnd: 1994; FRule: @CRules[1114]),
    (FStart: 1995; FEnd: 2005; FRule: @CRules[1101]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[1115]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[1101]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1116]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1117])
  );

  { Date-bound rules for AT family }
  CFamily_130_Arr: array[0 .. 18] of TYearBoundRule = (
    (FStart: 1967; FEnd: 1967; FRule: @CRules[1117]),
    (FStart: 1968; FEnd: 1968; FRule: @CRules[1101]),
    (FStart: 1968; FEnd: 1985; FRule: @CRules[1103]),
    (FStart: 1969; FEnd: 1971; FRule: @CRules[1118]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[1107]),
    (FStart: 1973; FEnd: 1981; FRule: @CRules[1104]),
    (FStart: 1982; FEnd: 1983; FRule: @CRules[1101]),
    (FStart: 1984; FEnd: 1986; FRule: @CRules[1104]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[1119]),
    (FStart: 1987; FEnd: 1990; FRule: @CRules[1110]),
    (FStart: 1987; FEnd: 1987; FRule: @CRules[1120]),
    (FStart: 1988; FEnd: 1990; FRule: @CRules[1103]),
    (FStart: 1991; FEnd: 1999; FRule: @CRules[1117]),
    (FStart: 1991; FEnd: 2005; FRule: @CRules[1101]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[1121]),
    (FStart: 2001; FEnd: 9999; FRule: @CRules[1117]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[1116]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[1101]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1116])
  );

  { Date-bound rules for AV family }
  CFamily_131_Arr: array[0 .. 13] of TYearBoundRule = (
    (FStart: 1971; FEnd: 1985; FRule: @CRules[1103]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[1107]),
    (FStart: 1973; FEnd: 1985; FRule: @CRules[1104]),
    (FStart: 1986; FEnd: 1990; FRule: @CRules[1110]),
    (FStart: 1986; FEnd: 1987; FRule: @CRules[1119]),
    (FStart: 1988; FEnd: 1999; FRule: @CRules[1103]),
    (FStart: 1991; FEnd: 1994; FRule: @CRules[1104]),
    (FStart: 1995; FEnd: 2005; FRule: @CRules[1101]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[1121]),
    (FStart: 2001; FEnd: 2007; FRule: @CRules[1103]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[1116]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[1101]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1116]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1117])
  );

  { Date-bound rules for AN family }
  CFamily_132_Arr: array[0 .. 15] of TYearBoundRule = (
    (FStart: 1971; FEnd: 1985; FRule: @CRules[1103]),
    (FStart: 1972; FEnd: 1972; FRule: @CRules[1109]),
    (FStart: 1973; FEnd: 1981; FRule: @CRules[1104]),
    (FStart: 1982; FEnd: 1982; FRule: @CRules[1116]),
    (FStart: 1983; FEnd: 1985; FRule: @CRules[1104]),
    (FStart: 1986; FEnd: 1989; FRule: @CRules[1110]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[1108]),
    (FStart: 1987; FEnd: 1999; FRule: @CRules[1103]),
    (FStart: 1990; FEnd: 1995; FRule: @CRules[1104]),
    (FStart: 1996; FEnd: 2005; FRule: @CRules[1101]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[1121]),
    (FStart: 2001; FEnd: 2007; FRule: @CRules[1103]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[1116]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[1101]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1116]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1117])
  );

  { Date-bound rules for LH family }
  CFamily_133_Arr: array[0 .. 13] of TYearBoundRule = (
    (FStart: 1981; FEnd: 1984; FRule: @CRules[922]),
    (FStart: 1982; FEnd: 1985; FRule: @CRules[1122]),
    (FStart: 1985; FEnd: 1985; FRule: @CRules[1123]),
    (FStart: 1986; FEnd: 1989; FRule: @CRules[1090]),
    (FStart: 1986; FEnd: 1986; FRule: @CRules[1124]),
    (FStart: 1987; FEnd: 1999; FRule: @CRules[1123]),
    (FStart: 1990; FEnd: 1995; FRule: @CRules[1122]),
    (FStart: 1996; FEnd: 2005; FRule: @CRules[923]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[1125]),
    (FStart: 2001; FEnd: 2007; FRule: @CRules[1123]),
    (FStart: 2006; FEnd: 2006; FRule: @CRules[1126]),
    (FStart: 2007; FEnd: 2007; FRule: @CRules[923]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1126]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1127])
  );

  { Date-bound rules for Fiji family }
  CFamily_134_Arr: array[0 .. 10] of TYearBoundRule = (
    (FStart: 1998; FEnd: 1999; FRule: @CRules[1128]),
    (FStart: 1999; FEnd: 2000; FRule: @CRules[1129]),
    (FStart: 2009; FEnd: 2009; FRule: @CRules[1130]),
    (FStart: 2010; FEnd: 2010; FRule: @CRules[1131]),
    (FStart: 2010; FEnd: 2013; FRule: @CRules[1132]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[1133]),
    (FStart: 2012; FEnd: 2013; FRule: @CRules[1134]),
    (FStart: 2014; FEnd: 2014; FRule: @CRules[1135]),
    (FStart: 2014; FEnd: 2018; FRule: @CRules[1128]),
    (FStart: 2015; FEnd: 9999; FRule: @CRules[1136]),
    (FStart: 2019; FEnd: 9999; FRule: @CRules[1137])
  );

  { Date-bound rules for Guam family }
  CFamily_135_Arr: array[0 .. 13] of TYearBoundRule = (
    (FStart: 1959; FEnd: 1959; FRule: @CRules[1138]),
    (FStart: 1961; FEnd: 1961; FRule: @CRules[1139]),
    (FStart: 1967; FEnd: 1967; FRule: @CRules[1140]),
    (FStart: 1969; FEnd: 1969; FRule: @CRules[1141]),
    (FStart: 1969; FEnd: 1969; FRule: @CRules[764]),
    (FStart: 1969; FEnd: 1969; FRule: @CRules[1142]),
    (FStart: 1970; FEnd: 1971; FRule: @CRules[138]),
    (FStart: 1970; FEnd: 1971; FRule: @CRules[1143]),
    (FStart: 1973; FEnd: 1973; FRule: @CRules[1144]),
    (FStart: 1974; FEnd: 1974; FRule: @CRules[1145]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[1146]),
    (FStart: 1976; FEnd: 1976; FRule: @CRules[1147]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[1148]),
    (FStart: 1977; FEnd: 1977; FRule: @CRules[1149])
  );

  { Date-bound rules for NC family }
  CFamily_136_Arr: array[0 .. 3] of TYearBoundRule = (
    (FStart: 1977; FEnd: 1978; FRule: @CRules[1150]),
    (FStart: 1978; FEnd: 1979; FRule: @CRules[48]),
    (FStart: 1996; FEnd: 1996; FRule: @CRules[1151]),
    (FStart: 1997; FEnd: 1997; FRule: @CRules[1152])
  );

  { Date-bound rules for Chatham family }
  CFamily_137_Arr: array[0 .. 8] of TYearBoundRule = (
    (FStart: 1974; FEnd: 1974; FRule: @CRules[1161]),
    (FStart: 1975; FEnd: 1975; FRule: @CRules[1162]),
    (FStart: 1975; FEnd: 1988; FRule: @CRules[1163]),
    (FStart: 1976; FEnd: 1989; FRule: @CRules[1164]),
    (FStart: 1989; FEnd: 1989; FRule: @CRules[1166]),
    (FStart: 1990; FEnd: 2006; FRule: @CRules[1167]),
    (FStart: 1990; FEnd: 2007; FRule: @CRules[1168]),
    (FStart: 2007; FEnd: 9999; FRule: @CRules[1170]),
    (FStart: 2008; FEnd: 9999; FRule: @CRules[1171])
  );

  { Date-bound rules for Cook family }
  CFamily_138_Arr: array[0 .. 2] of TYearBoundRule = (
    (FStart: 1978; FEnd: 1978; FRule: @CRules[1172]),
    (FStart: 1979; FEnd: 1991; FRule: @CRules[16]),
    (FStart: 1979; FEnd: 1990; FRule: @CRules[98])
  );

  { Date-bound rules for WS family }
  CFamily_139_Arr: array[0 .. 4] of TYearBoundRule = (
    (FStart: 2010; FEnd: 2010; FRule: @CRules[80]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[1173]),
    (FStart: 2011; FEnd: 2011; FRule: @CRules[1174]),
    (FStart: 2012; FEnd: 9999; FRule: @CRules[1175]),
    (FStart: 2012; FEnd: 9999; FRule: @CRules[1176])
  );

  { Date-bound rules for Tonga family }
  CFamily_140_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1999; FEnd: 1999; FRule: @CRules[1177]),
    (FStart: 2000; FEnd: 2000; FRule: @CRules[1178]),
    (FStart: 2000; FEnd: 2001; FRule: @CRules[1128]),
    (FStart: 2001; FEnd: 2002; FRule: @CRules[1179]),
    (FStart: 2016; FEnd: 2016; FRule: @CRules[1128]),
    (FStart: 2017; FEnd: 2017; FRule: @CRules[1180])
  );

  { Date-bound rules for Vanuatu family }
  CFamily_141_Arr: array[0 .. 5] of TYearBoundRule = (
    (FStart: 1983; FEnd: 1983; FRule: @CRules[1181]),
    (FStart: 1984; FEnd: 1991; FRule: @CRules[1182]),
    (FStart: 1984; FEnd: 1984; FRule: @CRules[1183]),
    (FStart: 1985; FEnd: 1991; FRule: @CRules[1184]),
    (FStart: 1992; FEnd: 1993; FRule: @CRules[1185]),
    (FStart: 1992; FEnd: 1992; FRule: @CRules[1186])
  );

var
  { This array contains rule families. }
  CRuleFamilies: array[0 .. 141] of TRuleFamily = (
    (FCount: 29; FFirstRule: @CFamily_0_Arr),
    (FCount: 2; FFirstRule: @CFamily_1_Arr),
    (FCount: 57; FFirstRule: @CFamily_2_Arr),
    (FCount: 34; FFirstRule: @CFamily_3_Arr),
    (FCount: 2; FFirstRule: @CFamily_4_Arr),
    (FCount: 2; FFirstRule: @CFamily_5_Arr),
    (FCount: 12; FFirstRule: @CFamily_6_Arr),
    (FCount: 22; FFirstRule: @CFamily_7_Arr),
    (FCount: 10; FFirstRule: @CFamily_8_Arr),
    (FCount: 48; FFirstRule: @CFamily_9_Arr),
    (FCount: 8; FFirstRule: @CFamily_10_Arr),
    (FCount: 2; FFirstRule: @CFamily_11_Arr),
    (FCount: 65; FFirstRule: @CFamily_12_Arr),
    (FCount: 6; FFirstRule: @CFamily_13_Arr),
    (FCount: 7; FFirstRule: @CFamily_14_Arr),
    (FCount: 6; FFirstRule: @CFamily_15_Arr),
    (FCount: 17; FFirstRule: @CFamily_16_Arr),
    (FCount: 6; FFirstRule: @CFamily_17_Arr),
    (FCount: 16; FFirstRule: @CFamily_18_Arr),
    (FCount: 25; FFirstRule: @CFamily_19_Arr),
    (FCount: 9; FFirstRule: @CFamily_20_Arr),
    (FCount: 34; FFirstRule: @CFamily_21_Arr),
    (FCount: 5; FFirstRule: @CFamily_22_Arr),
    (FCount: 6; FFirstRule: @CFamily_23_Arr),
    (FCount: 11; FFirstRule: @CFamily_24_Arr),
    (FCount: 6; FFirstRule: @CFamily_25_Arr),
    (FCount: 4; FFirstRule: @CFamily_26_Arr),
    (FCount: 42; FFirstRule: @CFamily_27_Arr),
    (FCount: 8; FFirstRule: @CFamily_28_Arr),
    (FCount: 3; FFirstRule: @CFamily_29_Arr),
    (FCount: 19; FFirstRule: @CFamily_30_Arr),
    (FCount: 18; FFirstRule: @CFamily_31_Arr),
    (FCount: 16; FFirstRule: @CFamily_32_Arr),
    (FCount: 41; FFirstRule: @CFamily_33_Arr),
    (FCount: 2; FFirstRule: @CFamily_34_Arr),
    (FCount: 23; FFirstRule: @CFamily_35_Arr),
    (FCount: 7; FFirstRule: @CFamily_36_Arr),
    (FCount: 2; FFirstRule: @CFamily_37_Arr),
    (FCount: 9; FFirstRule: @CFamily_38_Arr),
    (FCount: 20; FFirstRule: @CFamily_39_Arr),
    (FCount: 7; FFirstRule: @CFamily_40_Arr),
    (FCount: 20; FFirstRule: @CFamily_41_Arr),
    (FCount: 50; FFirstRule: @CFamily_42_Arr),
    (FCount: 30; FFirstRule: @CFamily_43_Arr),
    (FCount: 9; FFirstRule: @CFamily_44_Arr),
    (FCount: 2; FFirstRule: @CFamily_45_Arr),
    (FCount: 45; FFirstRule: @CFamily_46_Arr),
    (FCount: 4; FFirstRule: @CFamily_47_Arr),
    (FCount: 12; FFirstRule: @CFamily_48_Arr),
    (FCount: 14; FFirstRule: @CFamily_49_Arr),
    (FCount: 20; FFirstRule: @CFamily_50_Arr),
    (FCount: 10; FFirstRule: @CFamily_51_Arr),
    (FCount: 16; FFirstRule: @CFamily_52_Arr),
    (FCount: 3; FFirstRule: @CFamily_53_Arr),
    (FCount: 87; FFirstRule: @CFamily_54_Arr),
    (FCount: 3; FFirstRule: @CFamily_55_Arr),
    (FCount: 3; FFirstRule: @CFamily_56_Arr),
    (FCount: 5; FFirstRule: @CFamily_57_Arr),
    (FCount: 2; FFirstRule: @CFamily_58_Arr),
    (FCount: 2; FFirstRule: @CFamily_59_Arr),
    (FCount: 2; FFirstRule: @CFamily_60_Arr),
    (FCount: 12; FFirstRule: @CFamily_61_Arr),
    (FCount: 14; FFirstRule: @CFamily_62_Arr),
    (FCount: 15; FFirstRule: @CFamily_63_Arr),
    (FCount: 27; FFirstRule: @CFamily_64_Arr),
    (FCount: 9; FFirstRule: @CFamily_65_Arr),
    (FCount: 101; FFirstRule: @CFamily_66_Arr),
    (FCount: 8; FFirstRule: @CFamily_67_Arr),
    (FCount: 4; FFirstRule: @CFamily_68_Arr),
    (FCount: 31; FFirstRule: @CFamily_69_Arr),
    (FCount: 4; FFirstRule: @CFamily_70_Arr),
    (FCount: 14; FFirstRule: @CFamily_71_Arr),
    (FCount: 24; FFirstRule: @CFamily_72_Arr),
    (FCount: 2; FFirstRule: @CFamily_73_Arr),
    (FCount: 9; FFirstRule: @CFamily_74_Arr),
    (FCount: 5; FFirstRule: @CFamily_75_Arr),
    (FCount: 6; FFirstRule: @CFamily_76_Arr),
    (FCount: 24; FFirstRule: @CFamily_77_Arr),
    (FCount: 6; FFirstRule: @CFamily_78_Arr),
    (FCount: 41; FFirstRule: @CFamily_79_Arr),
    (FCount: 13; FFirstRule: @CFamily_80_Arr),
    (FCount: 5; FFirstRule: @CFamily_81_Arr),
    (FCount: 6; FFirstRule: @CFamily_82_Arr),
    (FCount: 5; FFirstRule: @CFamily_83_Arr),
    (FCount: 5; FFirstRule: @CFamily_84_Arr),
    (FCount: 3; FFirstRule: @CFamily_85_Arr),
    (FCount: 4; FFirstRule: @CFamily_86_Arr),
    (FCount: 9; FFirstRule: @CFamily_87_Arr),
    (FCount: 4; FFirstRule: @CFamily_88_Arr),
    (FCount: 4; FFirstRule: @CFamily_89_Arr),
    (FCount: 5; FFirstRule: @CFamily_90_Arr),
    (FCount: 4; FFirstRule: @CFamily_91_Arr),
    (FCount: 9; FFirstRule: @CFamily_92_Arr),
    (FCount: 2; FFirstRule: @CFamily_93_Arr),
    (FCount: 4; FFirstRule: @CFamily_94_Arr),
    (FCount: 19; FFirstRule: @CFamily_95_Arr),
    (FCount: 41; FFirstRule: @CFamily_96_Arr),
    (FCount: 13; FFirstRule: @CFamily_97_Arr),
    (FCount: 22; FFirstRule: @CFamily_98_Arr),
    (FCount: 24; FFirstRule: @CFamily_99_Arr),
    (FCount: 17; FFirstRule: @CFamily_100_Arr),
    (FCount: 5; FFirstRule: @CFamily_101_Arr),
    (FCount: 13; FFirstRule: @CFamily_102_Arr),
    (FCount: 9; FFirstRule: @CFamily_103_Arr),
    (FCount: 2; FFirstRule: @CFamily_104_Arr),
    (FCount: 5; FFirstRule: @CFamily_105_Arr),
    (FCount: 6; FFirstRule: @CFamily_106_Arr),
    (FCount: 5; FFirstRule: @CFamily_107_Arr),
    (FCount: 39; FFirstRule: @CFamily_108_Arr),
    (FCount: 6; FFirstRule: @CFamily_109_Arr),
    (FCount: 2; FFirstRule: @CFamily_110_Arr),
    (FCount: 8; FFirstRule: @CFamily_111_Arr),
    (FCount: 11; FFirstRule: @CFamily_112_Arr),
    (FCount: 4; FFirstRule: @CFamily_113_Arr),
    (FCount: 6; FFirstRule: @CFamily_114_Arr),
    (FCount: 22; FFirstRule: @CFamily_115_Arr),
    (FCount: 32; FFirstRule: @CFamily_116_Arr),
    (FCount: 2; FFirstRule: @CFamily_117_Arr),
    (FCount: 17; FFirstRule: @CFamily_118_Arr),
    (FCount: 4; FFirstRule: @CFamily_119_Arr),
    (FCount: 183; FFirstRule: @CFamily_120_Arr),
    (FCount: 3; FFirstRule: @CFamily_121_Arr),
    (FCount: 2; FFirstRule: @CFamily_122_Arr),
    (FCount: 4; FFirstRule: @CFamily_123_Arr),
    (FCount: 25; FFirstRule: @CFamily_124_Arr),
    (FCount: 7; FFirstRule: @CFamily_125_Arr),
    (FCount: 9; FFirstRule: @CFamily_126_Arr),
    (FCount: 4; FFirstRule: @CFamily_127_Arr),
    (FCount: 2; FFirstRule: @CFamily_128_Arr),
    (FCount: 15; FFirstRule: @CFamily_129_Arr),
    (FCount: 19; FFirstRule: @CFamily_130_Arr),
    (FCount: 14; FFirstRule: @CFamily_131_Arr),
    (FCount: 16; FFirstRule: @CFamily_132_Arr),
    (FCount: 14; FFirstRule: @CFamily_133_Arr),
    (FCount: 11; FFirstRule: @CFamily_134_Arr),
    (FCount: 14; FFirstRule: @CFamily_135_Arr),
    (FCount: 4; FFirstRule: @CFamily_136_Arr),
    (FCount: 9; FFirstRule: @CFamily_137_Arr),
    (FCount: 3; FFirstRule: @CFamily_138_Arr),
    (FCount: 5; FFirstRule: @CFamily_139_Arr),
    (FCount: 6; FFirstRule: @CFamily_140_Arr),
    (FCount: 6; FFirstRule: @CFamily_141_Arr)
  );

var
  { Time periods for Africa/Abidjan zone }
  CZone_0_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -968; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Accra zone }
  CZone_1_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -52; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[117]; FFmtStr: 'GMT/+0020'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Addis_Ababa zone }
  CZone_2_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 9288; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1870; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9320; FRuleFamily: nil; FFmtStr: 'ADMT'; FUntilYear: 1936; FUntilMonth: 5; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Algiers zone }
  CZone_3_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 732; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1891; FUntilMonth: 3; FUntilDay: @CRelativeDays[1]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: 561; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1911; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[115]; FFmtStr: 'WE%sT'; FUntilYear: 1940; FUntilMonth: 2; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[115]; FFmtStr: 'CE%sT'; FUntilYear: 1946; FUntilMonth: 10; FUntilDay: @CRelativeDays[13]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1956; FUntilMonth: 1; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1963; FUntilMonth: 4; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[115]; FFmtStr: 'WE%sT'; FUntilYear: 1977; FUntilMonth: 10; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[115]; FFmtStr: 'CE%sT'; FUntilYear: 1979; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[115]; FFmtStr: 'WE%sT'; FUntilYear: 1981; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Asmara zone }
  CZone_4_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 9332; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1870; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9332; FRuleFamily: nil; FFmtStr: 'AMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9320; FRuleFamily: nil; FFmtStr: 'ADMT'; FUntilYear: 1936; FUntilMonth: 5; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Bamako zone }
  CZone_5_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -1920; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1934; FUntilMonth: 2; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1960; FUntilMonth: 6; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Bangui zone }
  CZone_6_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 4460; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Banjul zone }
  CZone_7_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -3996; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3996; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1935; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1964; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Bissau zone }
  CZone_8_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -3740; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1975; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Blantyre zone }
  CZone_9_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 8400; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Brazzaville zone }
  CZone_10_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 3668; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Bujumbura zone }
  CZone_11_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 7048; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Cairo zone }
  CZone_12_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 7509; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[116]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Casablanca zone }
  CZone_13_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -1820; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1913; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[120]; FFmtStr: '+00/+01'; FUntilYear: 1984; FUntilMonth: 3; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: '+01'; FUntilYear: 1986; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[120]; FFmtStr: '+00/+01'; FUntilYear: 2018; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[120]; FFmtStr: '+01/+00'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Ceuta zone }
  CZone_14_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -1276; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 85124; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1918; FUntilMonth: 5; FUntilDay: @CRelativeDays[18]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WEST'; FUntilYear: 1918; FUntilMonth: 10; FUntilDay: @CRelativeDays[13]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1924; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[43]; FFmtStr: 'WE%sT'; FUntilYear: 1929; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[44]; FFmtStr: 'WE%sT'; FUntilYear: 1984; FUntilMonth: 3; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1986; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Conakry zone }
  CZone_15_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -3292; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1934; FUntilMonth: 2; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1960; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Dakar zone }
  CZone_16_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -4184; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1941; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Dar_es_Salaam zone }
  CZone_17_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 9428; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 1948; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9900; FRuleFamily: nil; FFmtStr: '+0245'; FUntilYear: 1961; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Djibouti zone }
  CZone_18_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 10356; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Douala zone }
  CZone_19_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 2328; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/El_Aaiun zone }
  CZone_20_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -3168; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1934; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1976; FUntilMonth: 4; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[120]; FFmtStr: '+00/+01'; FUntilYear: 2018; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[120]; FFmtStr: '+01/+00'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Freetown zone }
  CZone_21_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -3180; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1882; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3180; FRuleFamily: nil; FFmtStr: 'FMT'; FUntilYear: 1913; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[47]; FFmtStr: '%s'; FUntilYear: 1957; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[47]; FFmtStr: 'GMT/+01'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Gaborone zone }
  CZone_22_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 6220; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1885; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5400; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 1943; FUntilMonth: 9; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAST'; FUntilYear: 1944; FUntilMonth: 3; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Harare zone }
  CZone_23_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 7452; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Johannesburg zone }
  CZone_24_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 6720; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1892; FUntilMonth: 2; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5400; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[122]; FFmtStr: 'SAST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Juba zone }
  CZone_25_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 7588; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[123]; FFmtStr: 'CA%sT'; FUntilYear: 2000; FUntilMonth: 1; FUntilDay: @CRelativeDays[1]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Kampala zone }
  CZone_26_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 7780; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1928; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 1930; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9000; FRuleFamily: nil; FFmtStr: '+0230'; FUntilYear: 1948; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9900; FRuleFamily: nil; FFmtStr: '+0245'; FUntilYear: 1957; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Khartoum zone }
  CZone_27_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 7808; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[123]; FFmtStr: 'CA%sT'; FUntilYear: 2000; FUntilMonth: 1; FUntilDay: @CRelativeDays[1]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 2017; FUntilMonth: 11; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Kigali zone }
  CZone_28_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 7216; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1935; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Kinshasa zone }
  CZone_29_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 3672; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1897; FUntilMonth: 11; FUntilDay: @CRelativeDays[25]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Lagos zone }
  CZone_30_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 816; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Libreville zone }
  CZone_31_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 2268; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Lome zone }
  CZone_32_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 292; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1893; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Luanda zone }
  CZone_33_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 3176; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1892; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3124; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trUniversal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Lubumbashi zone }
  CZone_34_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 6592; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1897; FUntilMonth: 11; FUntilDay: @CRelativeDays[25]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Lusaka zone }
  CZone_35_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 6788; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Malabo zone }
  CZone_36_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 2108; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1963; FUntilMonth: 12; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Maputo zone }
  CZone_37_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 7820; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'CAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Maseru zone }
  CZone_38_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 6600; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1943; FUntilMonth: 9; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1944; FUntilMonth: 3; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Mbabane zone }
  CZone_39_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 7464; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Mogadishu zone }
  CZone_40_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 10888; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1893; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9000; FRuleFamily: nil; FFmtStr: '+0230'; FUntilYear: 1957; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Monrovia zone }
  CZone_41_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -2588; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1882; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -2588; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1919; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -2670; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1972; FUntilMonth: 1; FUntilDay: @CRelativeDays[13]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Nairobi zone }
  CZone_42_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 8836; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1928; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 1930; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9000; FRuleFamily: nil; FFmtStr: '+0230'; FUntilYear: 1940; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9900; FRuleFamily: nil; FFmtStr: '+0245'; FUntilYear: 1960; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Ndjamena zone }
  CZone_43_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 3612; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 1979; FUntilMonth: 10; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAST'; FUntilYear: 1980; FUntilMonth: 3; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Niamey zone }
  CZone_44_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 508; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1934; FUntilMonth: 2; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1960; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Nouakchott zone }
  CZone_45_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -3828; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1934; FUntilMonth: 2; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1960; FUntilMonth: 11; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Ouagadougou zone }
  CZone_46_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -364; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Porto-Novo zone }
  CZone_47_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 628; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1934; FUntilMonth: 2; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Sao_Tome zone }
  CZone_48_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 1616; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -2205; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 2018; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 3600; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'WAT'; FUntilYear: 2019; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Timbuktu zone }
  CZone_49_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -724; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Tripoli zone }
  CZone_50_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 3164; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[118]; FFmtStr: 'CE%sT'; FUntilYear: 1959; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1982; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[118]; FFmtStr: 'CE%sT'; FUntilYear: 1990; FUntilMonth: 5; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1996; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[118]; FFmtStr: 'CE%sT'; FUntilYear: 1997; FUntilMonth: 10; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2012; FUntilMonth: 11; FUntilDay: @CRelativeDays[34]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[118]; FFmtStr: 'CE%sT'; FUntilYear: 2013; FUntilMonth: 10; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Tunis zone }
  CZone_51_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 2444; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1881; FUntilMonth: 5; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 561; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1911; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[124]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Africa/Windhoek zone }
  CZone_52_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 4104; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1892; FUntilMonth: 2; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5400; FRuleFamily: nil; FFmtStr: '+0130'; FUntilYear: 1903; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1942; FUntilMonth: 9; FUntilDay: @CRelativeDays[10]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1943; FUntilMonth: 3; FUntilDay: @CRelativeDays[22]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'SAST'; FUntilYear: 1990; FUntilMonth: 3; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[121]; FFmtStr: '%s'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Adak zone }
  CZone_53_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 44002; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 45875; FUntilTimeMode: trLocal),
    (FOffset: -42398; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'N%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1967; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'B%sT'; FUntilYear: 1983; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AH%sT'; FUntilYear: 1983; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'H%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Anchorage zone }
  CZone_54_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 50424; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 52297; FUntilTimeMode: trLocal),
    (FOffset: -35976; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'A%sT'; FUntilYear: 1967; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: 'AHST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AH%sT'; FUntilYear: 1983; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1983; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Anguilla zone }
  CZone_55_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -15136; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 3; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Antigua zone }
  CZone_56_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -14832; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 3; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1951; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Araguaina zone }
  CZone_57_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -11568; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 9; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1995; FUntilMonth: 9; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2003; FUntilMonth: 9; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2012; FUntilMonth: 10; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2013; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Buenos_Aires zone }
  CZone_58_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -14028; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Catamarca zone }
  CZone_59_Arr: array[0 .. 11] of TPeriod = (
    (FOffset: -15788; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/ComodRivadavia zone }
  CZone_60_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -16200; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Cordoba zone }
  CZone_61_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Jujuy zone }
  CZone_62_Arr: array[0 .. 12] of TPeriod = (
    (FOffset: -15672; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 3; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1990; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/La_Rioja zone }
  CZone_63_Arr: array[0 .. 11] of TPeriod = (
    (FOffset: -16044; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 5; FUntilDay: @CRelativeDays[13]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Mendoza zone }
  CZone_64_Arr: array[0 .. 15] of TPeriod = (
    (FOffset: -16516; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 3; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1990; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1992; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 5; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 9; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Rio_Gallegos zone }
  CZone_65_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -16612; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Salta zone }
  CZone_66_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -15700; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/San_Juan zone }
  CZone_67_Arr: array[0 .. 11] of TPeriod = (
    (FOffset: -16444; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 5; FUntilDay: @CRelativeDays[13]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 5; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 7; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/San_Luis zone }
  CZone_68_Arr: array[0 .. 15] of TPeriod = (
    (FOffset: -15924; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1990; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1990; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 5; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 7; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 1; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[1]; FFmtStr: '-04/-03'; FUntilYear: 2009; FUntilMonth: 10; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Tucuman zone }
  CZone_69_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -15652; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Argentina/Ushuaia zone }
  CZone_70_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -16392; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15408; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2004; FUntilMonth: 5; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 2008; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Aruba zone }
  CZone_71_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -16824; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 2; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16200; FRuleFamily: nil; FFmtStr: '-0430'; FUntilYear: 1965; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Asuncion zone }
  CZone_72_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -13840; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -13840; FRuleFamily: nil; FFmtStr: 'AMT'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1972; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1974; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[7]; FFmtStr: '-04/-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Atikokan zone }
  CZone_73_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -21988; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 1940; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CDT'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Bahia zone }
  CZone_74_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -9244; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2003; FUntilMonth: 9; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2011; FUntilMonth: 10; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2012; FUntilMonth: 10; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Bahia_Banderas zone }
  CZone_75_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -25260; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 86340; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1931; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1932; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1949; FUntilMonth: 1; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1970; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'M%sT'; FUntilYear: 2010; FUntilMonth: 4; FUntilDay: @CRelativeDays[15]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Barbados zone }
  CZone_76_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -14309; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14309; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1932; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[105]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Belem zone }
  CZone_77_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -11636; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Belize zone }
  CZone_78_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -21168; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[106]; FFmtStr: '%s'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Blanc-Sablon zone }
  CZone_79_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -13708; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1970; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Boa_Vista zone }
  CZone_80_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -14560; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1999; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Bogota zone }
  CZone_81_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -17776; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 3; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -17776; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1914; FUntilMonth: 11; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[4]; FFmtStr: '-05/-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Boise zone }
  CZone_82_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -27889; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44111; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1923; FUntilMonth: 5; FUntilDay: @CRelativeDays[12]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1974; FUntilMonth: 2; FUntilDay: @CRelativeDays[6]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Cambridge_Bay zone }
  CZone_83_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'M%sT'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2000; FUntilMonth: 11; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 2001; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Campo_Grande zone }
  CZone_84_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -13108; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Cancun zone }
  CZone_85_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -20824; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 776; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1981; FUntilMonth: 12; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'E%sT'; FUntilYear: 1998; FUntilMonth: 8; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 2015; FUntilMonth: 2; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Caracas zone }
  CZone_86_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -16064; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16060; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1912; FUntilMonth: 2; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16200; FRuleFamily: nil; FFmtStr: '-0430'; FUntilYear: 1965; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2007; FUntilMonth: 12; FUntilDay: @CRelativeDays[25]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -16200; FRuleFamily: nil; FFmtStr: '-0430'; FUntilYear: 2016; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 9000; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Cayenne zone }
  CZone_87_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -12560; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1967; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Cayman zone }
  CZone_88_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -19532; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18430; FRuleFamily: nil; FFmtStr: 'KMT'; FUntilYear: 1912; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Chicago zone }
  CZone_89_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -21036; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43764; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[82]; FFmtStr: 'C%sT'; FUntilYear: 1936; FUntilMonth: 3; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1936; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[82]; FFmtStr: 'C%sT'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[82]; FFmtStr: 'C%sT'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Chihuahua zone }
  CZone_90_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -25460; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 86140; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1931; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1932; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 1998; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1998; FUntilMonth: 4; FUntilDay: @CRelativeDays[3]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Coral_Harbour zone }
  CZone_91_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -19960; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'E%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Costa_Rica zone }
  CZone_92_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -20173; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -20173; FRuleFamily: nil; FFmtStr: 'SJMT'; FUntilYear: 1921; FUntilMonth: 1; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[107]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Creston zone }
  CZone_93_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -27964; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1918; FUntilMonth: 6; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Cuiaba zone }
  CZone_94_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -13460; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 2003; FUntilMonth: 9; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Curacao zone }
  CZone_95_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -16547; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 2; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16200; FRuleFamily: nil; FFmtStr: '-0430'; FUntilYear: 1965; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Danmarkshavn zone }
  CZone_96_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -4480; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1980; FUntilMonth: 4; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[13]; FFmtStr: '-03/-02'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Dawson zone }
  CZone_97_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -33460; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'Y%sT'; FUntilYear: 1973; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'P%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Dawson_Creek zone }
  CZone_98_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -28856; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'P%sT'; FUntilYear: 1947; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[103]; FFmtStr: 'P%sT'; FUntilYear: 1972; FUntilMonth: 8; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Denver zone }
  CZone_99_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -25196; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43204; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[83]; FFmtStr: 'M%sT'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[83]; FFmtStr: 'M%sT'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Detroit zone }
  CZone_100_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -19931; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1915; FUntilMonth: 5; FUntilDay: @CRelativeDays[1]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[93]; FFmtStr: 'E%sT'; FUntilYear: 1967; FUntilMonth: 6; FUntilDay: @CRelativeDays[21]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1973; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1975; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1975; FUntilMonth: 4; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Dominica zone }
  CZone_101_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -14736; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Edmonton zone }
  CZone_102_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -27232; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1906; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[102]; FFmtStr: 'M%sT'; FUntilYear: 1987; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Eirunepe zone }
  CZone_103_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -16768; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-05/-04'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1993; FUntilMonth: 9; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-05/-04'; FUntilYear: 1994; FUntilMonth: 9; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 2008; FUntilMonth: 6; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2013; FUntilMonth: 11; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/El_Salvador zone }
  CZone_104_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -21408; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[110]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Ensenada zone }
  CZone_105_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -27988; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 812; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1949; FUntilMonth: 1; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Fortaleza zone }
  CZone_106_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -9240; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 9; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1999; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2001; FUntilMonth: 9; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2002; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Fort_Nelson zone }
  CZone_107_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -29447; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[103]; FFmtStr: 'P%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1947; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[103]; FFmtStr: 'P%sT'; FUntilYear: 1987; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'P%sT'; FUntilYear: 2015; FUntilMonth: 3; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Glace_Bay zone }
  CZone_108_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -14388; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1902; FUntilMonth: 6; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1953; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[96]; FFmtStr: 'A%sT'; FUntilYear: 1954; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 1972; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[96]; FFmtStr: 'A%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Godthab zone }
  CZone_109_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -12416; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1980; FUntilMonth: 4; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[13]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Goose_Bay zone }
  CZone_110_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -14500; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12652; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12652; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'N%sT'; FUntilYear: 1919; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12652; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1935; FUntilMonth: 3; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1936; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'N%sT'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'N%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'N%sT'; FUntilYear: 1966; FUntilMonth: 3; FUntilDay: @CRelativeDays[1]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'A%sT'; FUntilYear: 2011; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Grand_Turk zone }
  CZone_111_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -17072; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18430; FRuleFamily: nil; FFmtStr: 'KMT'; FUntilYear: 1912; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1979; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 2015; FUntilMonth: 11; FUntilDay: @CRelativeDays[3]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 2018; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Grenada zone }
  CZone_112_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -14820; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Guadeloupe zone }
  CZone_113_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -14768; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 6; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Guatemala zone }
  CZone_114_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -21724; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1918; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[111]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Guayaquil zone }
  CZone_115_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -19160; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18840; FRuleFamily: nil; FFmtStr: 'QMT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[5]; FFmtStr: '-05/-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Guyana zone }
  CZone_116_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -13960; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1915; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -13500; FRuleFamily: nil; FFmtStr: '-0345'; FUntilYear: 1975; FUntilMonth: 7; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1991; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Halifax zone }
  CZone_117_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -15264; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1902; FUntilMonth: 6; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[96]; FFmtStr: 'A%sT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1919; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[96]; FFmtStr: 'A%sT'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[96]; FFmtStr: 'A%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Havana zone }
  CZone_118_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -19768; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -19776; FRuleFamily: nil; FFmtStr: 'HMT'; FUntilYear: 1925; FUntilMonth: 7; FUntilDay: @CRelativeDays[29]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[108]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Hermosillo zone }
  CZone_119_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -26632; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 84968; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1931; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1932; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1949; FUntilMonth: 1; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1970; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'M%sT'; FUntilYear: 1999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Indianapolis zone }
  CZone_120_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -20678; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44122; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[85]; FFmtStr: 'C%sT'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[85]; FFmtStr: 'C%sT'; FUntilYear: 1955; FUntilMonth: 4; FUntilDay: @CRelativeDays[32]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1957; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1958; FUntilMonth: 4; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Knox zone }
  CZone_121_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -20790; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44010; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1947; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[90]; FFmtStr: 'C%sT'; FUntilYear: 1962; FUntilMonth: 4; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1963; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Marengo zone }
  CZone_122_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -20723; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44077; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1951; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[86]; FFmtStr: 'C%sT'; FUntilYear: 1961; FUntilMonth: 4; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CDT'; FUntilYear: 1974; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1976; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Petersburg zone }
  CZone_123_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -20947; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43853; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1955; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[89]; FFmtStr: 'C%sT'; FUntilYear: 1965; FUntilMonth: 4; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1966; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1977; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 2007; FUntilMonth: 11; FUntilDay: @CRelativeDays[15]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Tell_City zone }
  CZone_124_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -20823; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43977; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[88]; FFmtStr: 'C%sT'; FUntilYear: 1964; FUntilMonth: 4; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1967; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1969; FUntilMonth: 4; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Vevay zone }
  CZone_125_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -20416; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44384; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1954; FUntilMonth: 4; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1973; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Vincennes zone }
  CZone_126_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -21007; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43793; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[87]; FFmtStr: 'C%sT'; FUntilYear: 1964; FUntilMonth: 4; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 2007; FUntilMonth: 11; FUntilDay: @CRelativeDays[15]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Indiana/Winamac zone }
  CZone_127_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -20785; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44015; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[91]; FFmtStr: 'C%sT'; FUntilYear: 1961; FUntilMonth: 4; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2006; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 2007; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Inuvik zone }
  CZone_128_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1953; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'P%sT'; FUntilYear: 1979; FUntilMonth: 4; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'M%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Iqaluit zone }
  CZone_129_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1942; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'E%sT'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Jamaica zone }
  CZone_130_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -18430; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18430; FRuleFamily: nil; FFmtStr: 'KMT'; FUntilYear: 1912; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1984; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Juneau zone }
  CZone_131_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 54139; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 56012; FUntilTimeMode: trLocal),
    (FOffset: -32261; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1980; FUntilMonth: 4; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1980; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1983; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1983; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Kentucky/Louisville zone }
  CZone_132_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -20582; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44218; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1921; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[92]; FFmtStr: 'C%sT'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[92]; FFmtStr: 'C%sT'; FUntilYear: 1961; FUntilMonth: 7; FUntilDay: @CRelativeDays[4]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1968; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CDT'; FUntilYear: 1974; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Kentucky/Monticello zone }
  CZone_133_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -20364; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44436; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1968; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/La_Paz zone }
  CZone_134_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -16356; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16356; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16356; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1932; FUntilMonth: 3; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Lima zone }
  CZone_135_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -18492; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18516; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 7; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[8]; FFmtStr: '-05/-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Los_Angeles zone }
  CZone_136_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -28378; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43622; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[84]; FFmtStr: 'P%sT'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Maceio zone }
  CZone_137_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -8572; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 9; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1995; FUntilMonth: 10; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1996; FUntilMonth: 9; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1999; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2001; FUntilMonth: 9; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2002; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Managua zone }
  CZone_138_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -20708; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -20712; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1934; FUntilMonth: 6; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1973; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1975; FUntilMonth: 2; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[114]; FFmtStr: 'C%sT'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 14400; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1992; FUntilMonth: 9; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1993; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[114]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Manaus zone }
  CZone_139_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -14404; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1993; FUntilMonth: 9; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 1994; FUntilMonth: 9; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Martinique zone }
  CZone_140_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -14660; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14660; FRuleFamily: nil; FFmtStr: 'FFMT'; FUntilYear: 1911; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 1980; FUntilMonth: 4; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'ADT'; FUntilYear: 1980; FUntilMonth: 9; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Matamoros zone }
  CZone_141_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -24000; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 84000; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1988; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1989; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 2010; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Mazatlan zone }
  CZone_142_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -25540; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 86060; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1931; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1932; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1949; FUntilMonth: 1; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1970; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Menominee zone }
  CZone_143_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -21027; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1885; FUntilMonth: 9; FUntilDay: @CRelativeDays[11]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[94]; FFmtStr: 'C%sT'; FUntilYear: 1969; FUntilMonth: 4; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1973; FUntilMonth: 4; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Merida zone }
  CZone_144_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -21508; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 92; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1981; FUntilMonth: 12; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1982; FUntilMonth: 12; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Metlakatla zone }
  CZone_145_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 54822; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 56695; FUntilTimeMode: trLocal),
    (FOffset: -31578; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1983; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 2015; FUntilMonth: 11; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 2018; FUntilMonth: 11; FUntilDay: @CRelativeDays[15]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 2019; FUntilMonth: 1; FUntilDay: @CRelativeDays[10]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Mexico_City zone }
  CZone_146_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -23796; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 1404; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1931; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1932; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 2001; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 2002; FUntilMonth: 2; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Miquelon zone }
  CZone_147_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -13480; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 5; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 1980; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1987; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[51]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Moncton zone }
  CZone_148_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: -15548; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 12; FUntilDay: @CRelativeDays[25]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1902; FUntilMonth: 6; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1933; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[97]; FFmtStr: 'A%sT'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[97]; FFmtStr: 'A%sT'; FUntilYear: 1973; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1993; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[97]; FFmtStr: 'A%sT'; FUntilYear: 2007; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Monterrey zone }
  CZone_149_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -24076; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 83924; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1988; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 1989; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Montevideo zone }
  CZone_150_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -13491; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -13491; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1923; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-0330/-03'; FUntilYear: 1942; FUntilMonth: 12; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-0230'; FUntilYear: 1960; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-02'; FUntilYear: 1968; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-0230'; FUntilYear: 1970; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-02'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-0130'; FUntilYear: 1974; FUntilMonth: 3; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-0230'; FUntilYear: 1974; FUntilMonth: 12; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[9]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Montreal zone }
  CZone_151_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -17656; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[50]; FFmtStr: 'E%sT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1919; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[50]; FFmtStr: 'E%sT'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[50]; FFmtStr: 'E%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Montserrat zone }
  CZone_152_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -14932; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Nassau zone }
  CZone_153_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -18570; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 3; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[104]; FFmtStr: 'E%sT'; FUntilYear: 1976; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/New_York zone }
  CZone_154_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -17762; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43438; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[81]; FFmtStr: 'E%sT'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[81]; FFmtStr: 'E%sT'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Nipigon zone }
  CZone_155_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -21184; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1940; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EDT'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Nome zone }
  CZone_156_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 46702; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 48575; FUntilTimeMode: trLocal),
    (FOffset: -39698; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'N%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'NST'; FUntilYear: 1967; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'B%sT'; FUntilYear: 1983; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1983; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Noronha zone }
  CZone_157_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -7780; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-02/-01'; FUntilYear: 1990; FUntilMonth: 9; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1999; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-02/-01'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 2001; FUntilMonth: 9; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-02/-01'; FUntilYear: 2002; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/North_Dakota/Beulah zone }
  CZone_158_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -24427; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 43973; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 2010; FUntilMonth: 11; FUntilDay: @CRelativeDays[13]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/North_Dakota/Center zone }
  CZone_159_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -24312; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44088; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1992; FUntilMonth: 10; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/North_Dakota/New_Salem zone }
  CZone_160_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -24339; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 44061; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 2003; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Ojinaga zone }
  CZone_161_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -25060; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 140; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1931; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1931; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1932; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'C%sT'; FUntilYear: 1998; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1998; FUntilMonth: 4; FUntilDay: @CRelativeDays[3]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'M%sT'; FUntilYear: 2010; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Panama zone }
  CZone_162_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -19088; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -19176; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1908; FUntilMonth: 4; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Pangnirtung zone }
  CZone_163_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1921; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'A%sT'; FUntilYear: 1995; FUntilMonth: 4; FUntilDay: @CRelativeDays[3]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Paramaribo zone }
  CZone_164_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -13240; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -13252; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1935; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -13236; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1945; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: nil; FFmtStr: '-0330'; FUntilYear: 1984; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Phoenix zone }
  CZone_165_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -26898; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 41502; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1944; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1944; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1944; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 1968; FUntilMonth: 3; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Port-au-Prince zone }
  CZone_166_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -17360; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -17340; FRuleFamily: nil; FFmtStr: 'PPMT'; FUntilYear: 1917; FUntilMonth: 1; FUntilDay: @CRelativeDays[32]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[112]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Porto_Velho zone }
  CZone_167_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -15336; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Port_of_Spain zone }
  CZone_168_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -14764; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 3; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Puerto_Rico zone }
  CZone_169_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -15865; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1899; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'A%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Punta_Arenas zone }
  CZone_170_Arr: array[0 .. 12] of TPeriod = (
    (FOffset: -17020; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1910; FUntilMonth: 1; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1918; FUntilMonth: 9; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1927; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-05/-04'; FUntilYear: 1932; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1942; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1942; FUntilMonth: 8; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1947; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1947; FUntilMonth: 5; FUntilDay: @CRelativeDays[22]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-04/-03'; FUntilYear: 2016; FUntilMonth: 12; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Rainy_River zone }
  CZone_171_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -22696; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 1940; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CDT'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Rankin_Inlet zone }
  CZone_172_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1957; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'C%sT'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2001; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Recife zone }
  CZone_173_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -8376; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1990; FUntilMonth: 9; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1999; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 2001; FUntilMonth: 9; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 2002; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Regina zone }
  CZone_174_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -25116; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[100]; FFmtStr: 'M%sT'; FUntilYear: 1960; FUntilMonth: 4; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Resolute zone }
  CZone_175_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1947; FUntilMonth: 8; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'C%sT'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2001; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 2006; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 2007; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Rio_Branco zone }
  CZone_176_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -16272; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-05/-04'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 2008; FUntilMonth: 6; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2013; FUntilMonth: 11; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Rosario zone }
  CZone_177_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -14560; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -15404; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1920; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1930; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1991; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 2000; FUntilMonth: 3; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Santarem zone }
  CZone_178_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -13128; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-04/-03'; FUntilYear: 1988; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 2008; FUntilMonth: 6; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Santiago zone }
  CZone_179_Arr: array[0 .. 13] of TPeriod = (
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1910; FUntilMonth: 1; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1918; FUntilMonth: 9; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16966; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1927; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-05/-04'; FUntilYear: 1932; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1942; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1942; FUntilMonth: 8; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1946; FUntilMonth: 7; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 1946; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 1947; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1947; FUntilMonth: 5; FUntilDay: @CRelativeDays[22]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-04/-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Santo_Domingo zone }
  CZone_180_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -16776; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -16800; FRuleFamily: nil; FFmtStr: 'SDMT'; FUntilYear: 1933; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[109]; FFmtStr: '%s'; FUntilYear: 1974; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 2000; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 2000; FUntilMonth: 12; FUntilDay: @CRelativeDays[6]; FUntilTime: 3600; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Sao_Paulo zone }
  CZone_181_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -11188; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 1963; FUntilMonth: 10; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1964; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[2]; FFmtStr: '-03/-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Scoresbysund zone }
  CZone_182_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -5272; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1980; FUntilMonth: 4; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[16]; FFmtStr: '-02/-01'; FUntilYear: 1981; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: '-01/+00'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Sitka zone }
  CZone_183_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 53927; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 55800; FUntilTimeMode: trLocal),
    (FOffset: -32473; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1983; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1983; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/St_Johns zone }
  CZone_184_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -12652; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12652; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'N%sT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12652; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'N%sT'; FUntilYear: 1919; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12652; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'N%sT'; FUntilYear: 1935; FUntilMonth: 3; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'N%sT'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'N%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[95]; FFmtStr: 'N%sT'; FUntilYear: 2011; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -12600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'N%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/St_Kitts zone }
  CZone_185_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -15052; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 3; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/St_Lucia zone }
  CZone_186_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -14640; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14640; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/St_Thomas zone }
  CZone_187_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -15584; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/St_Vincent zone }
  CZone_188_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -14696; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14696; FRuleFamily: nil; FFmtStr: 'KMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Swift_Current zone }
  CZone_189_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -25880; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'M%sT'; FUntilYear: 1946; FUntilMonth: 4; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[100]; FFmtStr: 'M%sT'; FUntilYear: 1950; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[101]; FFmtStr: 'M%sT'; FUntilYear: 1972; FUntilMonth: 4; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Tegucigalpa zone }
  CZone_190_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -20932; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[113]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Thule zone }
  CZone_191_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -16508; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[25]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Thunder_Bay zone }
  CZone_192_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -21420; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1910; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1970; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[98]; FFmtStr: 'E%sT'; FUntilYear: 1973; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Tijuana zone }
  CZone_193_Arr: array[0 .. 18] of TPeriod = (
    (FOffset: -28084; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 716; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1924; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1927; FUntilMonth: 6; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 1930; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1931; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PDT'; FUntilYear: 1931; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PWT'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trUniversal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PPT'; FUntilYear: 1945; FUntilMonth: 11; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1948; FUntilMonth: 4; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PDT'; FUntilYear: 1949; FUntilMonth: 1; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1954; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[84]; FFmtStr: 'P%sT'; FUntilYear: 1961; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: 'PST'; FUntilYear: 1976; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'P%sT'; FUntilYear: 2001; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 2002; FUntilMonth: 2; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[49]; FFmtStr: 'P%sT'; FUntilYear: 2010; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Toronto zone }
  CZone_194_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -19052; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1919; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[98]; FFmtStr: 'E%sT'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[98]; FFmtStr: 'E%sT'; FUntilYear: 1974; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Tortola zone }
  CZone_195_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -15508; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Vancouver zone }
  CZone_196_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -29548; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[103]; FFmtStr: 'P%sT'; FUntilYear: 1987; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Whitehorse zone }
  CZone_197_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -32412; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'Y%sT'; FUntilYear: 1967; FUntilMonth: 5; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'P%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Winnipeg zone }
  CZone_198_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -23316; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1887; FUntilMonth: 7; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[99]; FFmtStr: 'C%sT'; FUntilYear: 2006; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Yakutat zone }
  CZone_199_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 52865; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 10; FUntilDay: @CRelativeDays[29]; FUntilTime: 54738; FUntilTimeMode: trLocal),
    (FOffset: -33535; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: nil; FFmtStr: 'YST'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: nil; FFmtStr: 'YST'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'Y%sT'; FUntilYear: 1983; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'AK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for America/Yellowknife zone }
  CZone_200_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1935; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[48]; FFmtStr: 'M%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Casey zone }
  CZone_201_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1969; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 2009; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[9]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 2011; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2012; FUntilMonth: 2; FUntilDay: @CRelativeDays[22]; FUntilTime: 61200; FUntilTimeMode: trUniversal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 2016; FUntilMonth: 10; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2018; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 14400; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Davis zone }
  CZone_202_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1957; FUntilMonth: 1; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1964; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1969; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 2009; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[34]; FUntilTime: 72000; FUntilTimeMode: trUniversal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 2011; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 2012; FUntilMonth: 2; FUntilDay: @CRelativeDays[22]; FUntilTime: 72000; FUntilTimeMode: trUniversal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/DumontDUrville zone }
  CZone_203_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1947; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1952; FUntilMonth: 1; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1956; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Macquarie zone }
  CZone_204_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1899; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEDT'; FUntilYear: 1917; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1919; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1948; FUntilMonth: 3; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[130]; FFmtStr: 'AE%sT'; FUntilYear: 2010; FUntilMonth: 4; FUntilDay: @CRelativeDays[15]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Mawson zone }
  CZone_205_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1954; FUntilMonth: 2; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2009; FUntilMonth: 10; FUntilDay: @CRelativeDays[11]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/McMurdo zone }
  CZone_206_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1956; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[52]; FFmtStr: 'NZ%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Palmer zone }
  CZone_207_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1965; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-04/-03'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[0]; FFmtStr: '-03/-02'; FUntilYear: 1982; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-04/-03'; FUntilYear: 2016; FUntilMonth: 12; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Rothera zone }
  CZone_208_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1976; FUntilMonth: 12; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Syowa zone }
  CZone_209_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1957; FUntilMonth: 1; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Troll zone }
  CZone_210_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 2005; FUntilMonth: 2; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[11]; FFmtStr: '%s'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Antarctica/Vostok zone }
  CZone_211_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1957; FUntilMonth: 12; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Aden zone }
  CZone_212_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 10794; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1950; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Almaty zone }
  CZone_213_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 18468; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+06/+07'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+06/+07'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Amman zone }
  CZone_214_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 8624; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[69]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Anadyr zone }
  CZone_215_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 42596; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 46800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+13/+14'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+12/+13'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+12/+13'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Aqtau zone }
  CZone_216_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 12064; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1994; FUntilMonth: 9; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Aqtobe zone }
  CZone_217_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 13720; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Ashgabat zone }
  CZone_218_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 14012; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Atyrau zone }
  CZone_219_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 12464; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1999; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Baghdad zone }
  CZone_220_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 10660; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10656; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1982; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[67]; FFmtStr: '+03/+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Bahrain zone }
  CZone_221_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 12140; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1972; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Baku zone }
  CZone_222_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 11964; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1957; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+03/+04'; FUntilYear: 1992; FUntilMonth: 9; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[55]; FFmtStr: '+04/+05'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[59]; FFmtStr: '+04/+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Bangkok zone }
  CZone_223_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 24124; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 24124; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1920; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Barnaul zone }
  CZone_224_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 20100; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1995; FUntilMonth: 5; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2016; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Beirut zone }
  CZone_225_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 8520; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[72]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Bishkek zone }
  CZone_226_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 17904; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+06/+07'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 8; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[70]; FFmtStr: '+05/+06'; FUntilYear: 2005; FUntilMonth: 8; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Brunei zone }
  CZone_227_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 27580; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1926; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1933; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Chita zone }
  CZone_228_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 27232; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+08/+09'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 2016; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Choibalsan zone }
  CZone_229_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 27480; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1978; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1983; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[74]; FFmtStr: '+09/+10'; FUntilYear: 2008; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[74]; FFmtStr: '+08/+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Chongqing zone }
  CZone_230_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 25580; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1928; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1980; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[53]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Colombo zone }
  CZone_231_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 19164; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19172; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1906; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1942; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1945; FUntilMonth: 10; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1996; FUntilMonth: 5; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23400; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1996; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 1800; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2006; FUntilMonth: 4; FUntilDay: @CRelativeDays[1]; FUntilTime: 1800; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Damascus zone }
  CZone_232_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 8712; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[79]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Dhaka zone }
  CZone_233_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 21700; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21200; FRuleFamily: nil; FFmtStr: 'HMT'; FUntilYear: 1941; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23400; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1942; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23400; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1951; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2009; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[60]; FFmtStr: '+06/+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Dili zone }
  CZone_234_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 30140; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[22]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1976; FUntilMonth: 5; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 2000; FUntilMonth: 9; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Dubai zone }
  CZone_235_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 13272; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Dushanbe zone }
  CZone_236_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 16512; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+06/+07'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 9; FUntilDay: @CRelativeDays[25]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Famagusta zone }
  CZone_237_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 8148; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 11; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[65]; FFmtStr: 'EE%sT'; FUntilYear: 1998; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[55]; FFmtStr: 'EE%sT'; FUntilYear: 2016; FUntilMonth: 9; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 2017; FUntilMonth: 10; FUntilDay: @CRelativeDays[26]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[55]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Gaza zone }
  CZone_238_Arr: array[0 .. 11] of TPeriod = (
    (FOffset: 8272; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[54]; FFmtStr: 'EET/EEST'; FUntilYear: 1948; FUntilMonth: 5; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[76]; FFmtStr: 'EE%sT'; FUntilYear: 1967; FUntilMonth: 6; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[54]; FFmtStr: 'I%sT'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[69]; FFmtStr: 'EE%sT'; FUntilYear: 1999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[77]; FFmtStr: 'EE%sT'; FUntilYear: 2008; FUntilMonth: 8; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2008; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[77]; FFmtStr: 'EE%sT'; FUntilYear: 2010; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[77]; FFmtStr: 'EE%sT'; FUntilYear: 2011; FUntilMonth: 8; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2012; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[77]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Hanoi zone }
  CZone_239_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 25404; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1906; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25590; FRuleFamily: nil; FFmtStr: 'PLMT'; FUntilYear: 1911; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1942; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1945; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1947; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1954; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Harbin zone }
  CZone_240_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 30404; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1928; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 30600; FRuleFamily: nil; FFmtStr: '+0830'; FUntilYear: 1932; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1940; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1966; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 30600; FRuleFamily: nil; FFmtStr: '+0830'; FUntilYear: 1980; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[53]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Hebron zone }
  CZone_241_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 8423; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[54]; FFmtStr: 'EET/EEST'; FUntilYear: 1948; FUntilMonth: 5; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[76]; FFmtStr: 'EE%sT'; FUntilYear: 1967; FUntilMonth: 6; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[54]; FFmtStr: 'I%sT'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[69]; FFmtStr: 'EE%sT'; FUntilYear: 1999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[77]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Hong_Kong zone }
  CZone_242_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 27402; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1904; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 2202; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'HKT'; FUntilYear: 1941; FUntilMonth: 6; FUntilDay: @CRelativeDays[1]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'HKST'; FUntilYear: 1941; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 14400; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'HKWT'; FUntilYear: 1941; FUntilMonth: 12; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'JST'; FUntilYear: 1945; FUntilMonth: 11; FUntilDay: @CRelativeDays[11]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[62]; FFmtStr: 'HK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Hovd zone }
  CZone_243_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 21996; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1978; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[74]; FFmtStr: '+07/+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Ho_Chi_Minh zone }
  CZone_244_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 25600; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1906; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25590; FRuleFamily: nil; FFmtStr: 'PLMT'; FUntilYear: 1911; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1942; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1945; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1947; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1955; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1959; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1975; FUntilMonth: 6; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Irkutsk zone }
  CZone_245_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 25025; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25025; FRuleFamily: nil; FFmtStr: 'IMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+08/+09'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+08/+09'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Jakarta zone }
  CZone_246_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 25632; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1867; FUntilMonth: 8; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25632; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1923; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 85632; FUntilTimeMode: trLocal),
    (FOffset: 26400; FRuleFamily: nil; FFmtStr: '+0720'; FUntilYear: 1932; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1942; FUntilMonth: 3; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1948; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1950; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1964; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: 'WIB'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Jayapura zone }
  CZone_247_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 33768; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1932; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1944; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: nil; FFmtStr: '+0930'; FUntilYear: 1964; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'WIT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Jerusalem zone }
  CZone_248_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 8454; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 8440; FRuleFamily: nil; FFmtStr: 'JMT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[54]; FFmtStr: 'I%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kabul zone }
  CZone_249_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 16608; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1945; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 16200; FRuleFamily: nil; FFmtStr: '+0430'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kamchatka zone }
  CZone_250_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 38076; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 11; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+12/+13'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+12/+13'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Karachi zone }
  CZone_251_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 16092; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1907; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1942; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1945; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1951; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1971; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[75]; FFmtStr: 'PK%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kashgar zone }
  CZone_252_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 18236; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1928; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1940; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1980; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[53]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kathmandu zone }
  CZone_253_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 20476; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1986; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 20700; FRuleFamily: nil; FFmtStr: '+0545'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Khandyga zone }
  CZone_254_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 32533; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+08/+09'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 2004; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2011; FUntilMonth: 9; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kolkata zone }
  CZone_255_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 21208; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1854; FUntilMonth: 6; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21200; FRuleFamily: nil; FFmtStr: 'HMT'; FUntilYear: 1870; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19270; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1906; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 1941; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 1942; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1945; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Krasnoyarsk zone }
  CZone_256_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 22286; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kuala_Lumpur zone }
  CZone_257_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 24406; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 24925; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1905; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1933; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+0720'; FUntilYear: 1936; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 26400; FRuleFamily: nil; FFmtStr: '+0720'; FUntilYear: 1941; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1982; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kuching zone }
  CZone_258_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 26480; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1926; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1933; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[73]; FFmtStr: '+08/+0820'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Kuwait zone }
  CZone_259_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 11516; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1950; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Macau zone }
  CZone_260_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 27250; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1904; FUntilMonth: 10; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1941; FUntilMonth: 12; FUntilDay: @CRelativeDays[22]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[64]; FFmtStr: '+09/+10'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 86400; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[64]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Magadan zone }
  CZone_261_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 36192; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2016; FUntilMonth: 4; FUntilDay: @CRelativeDays[32]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Makassar zone }
  CZone_262_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 28656; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28656; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1932; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[25]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'WITA'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Manila zone }
  CZone_263_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -57360; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 29040; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1899; FUntilMonth: 5; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[78]; FFmtStr: 'P%sT'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'JST'; FUntilYear: 1944; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[78]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Muscat zone }
  CZone_264_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 14064; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Nicosia zone }
  CZone_265_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 8008; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 11; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[65]; FFmtStr: 'EE%sT'; FUntilYear: 1998; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[55]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Novokuznetsk zone }
  CZone_266_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 20928; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Novosibirsk zone }
  CZone_267_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 19900; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[21]; FUntilTime: 21600; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1993; FUntilMonth: 5; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2016; FUntilMonth: 7; FUntilDay: @CRelativeDays[32]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Omsk zone }
  CZone_268_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 17610; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 11; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+05/+06'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Oral zone }
  CZone_269_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 12324; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Phnom_Penh zone }
  CZone_270_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 25180; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1906; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25590; FRuleFamily: nil; FFmtStr: 'PLMT'; FUntilYear: 1911; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1942; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1945; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Pontianak zone }
  CZone_271_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 26240; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 26240; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1932; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1942; FUntilMonth: 1; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1948; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1950; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1964; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'WITA'; FUntilYear: 1988; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: 'WIB'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Pyongyang zone }
  CZone_272_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 30180; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 30600; FRuleFamily: nil; FFmtStr: 'KST'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'JST'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'KST'; FUntilYear: 2015; FUntilMonth: 8; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 30600; FRuleFamily: nil; FFmtStr: 'KST'; FUntilYear: 2018; FUntilMonth: 5; FUntilDay: @CRelativeDays[15]; FUntilTime: 84600; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'KST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Qatar zone }
  CZone_273_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 12368; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1972; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Qostanay zone }
  CZone_274_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 15268; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Qyzylorda zone }
  CZone_275_Arr: array[0 .. 11] of TPeriod = (
    (FOffset: 15712; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1991; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+06/+07'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 2004; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2018; FUntilMonth: 12; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Riyadh zone }
  CZone_276_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 11212; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1947; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Sakhalin zone }
  CZone_277_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 34248; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 8; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1997; FUntilMonth: 3; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2016; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Samarkand zone }
  CZone_278_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 16073; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1981; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1981; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1982; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Seoul zone }
  CZone_279_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 30472; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 30600; FRuleFamily: nil; FFmtStr: 'KST'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'JST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[71]; FFmtStr: 'K%sT'; FUntilYear: 1954; FUntilMonth: 3; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 30600; FRuleFamily: @CRuleFamilies[71]; FFmtStr: 'K%sT'; FUntilYear: 1961; FUntilMonth: 8; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[71]; FFmtStr: 'K%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Shanghai zone }
  CZone_280_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 29143; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[61]; FFmtStr: 'C%sT'; FUntilYear: 1949; FUntilMonth: 5; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[53]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Singapore zone }
  CZone_281_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 24925; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 24925; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1905; FUntilMonth: 6; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1933; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+0720'; FUntilYear: 1936; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 26400; FRuleFamily: nil; FFmtStr: '+0720'; FUntilYear: 1941; FUntilMonth: 9; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1942; FUntilMonth: 2; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 27000; FRuleFamily: nil; FFmtStr: '+0730'; FUntilYear: 1982; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Srednekolymsk zone }
  CZone_282_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 36892; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Taipei zone }
  CZone_283_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 29160; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1896; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: 'CST'; FUntilYear: 1937; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'JST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[22]; FUntilTime: 3600; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[63]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Tashkent zone }
  CZone_284_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 16631; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+06/+07'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+05/+06'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Tbilisi zone }
  CZone_285_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: 10751; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10751; FRuleFamily: nil; FFmtStr: 'TBMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1957; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+03/+04'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[56]; FFmtStr: '+03/+04'; FUntilYear: 1994; FUntilMonth: 9; FUntilDay: @CRelativeDays[39]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[56]; FFmtStr: '+04/+05'; FUntilYear: 1996; FUntilMonth: 10; FUntilDay: @CRelativeDays[39]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1997; FUntilMonth: 3; FUntilDay: @CRelativeDays[39]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[56]; FFmtStr: '+04/+05'; FUntilYear: 2004; FUntilMonth: 6; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+03/+04'; FUntilYear: 2005; FUntilMonth: 3; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Tehran zone }
  CZone_286_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 12344; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1916; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 12344; FRuleFamily: nil; FFmtStr: 'TMT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 12600; FRuleFamily: nil; FFmtStr: '+0330'; FUntilYear: 1977; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[66]; FFmtStr: '+04/+05'; FUntilYear: 1979; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 12600; FRuleFamily: @CRuleFamilies[66]; FFmtStr: '+0330/+0430'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Tel_Aviv zone }
  CZone_287_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 8344; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 8460; FRuleFamily: nil; FFmtStr: 'JMT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[54]; FFmtStr: 'I%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Thimphu zone }
  CZone_288_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 21516; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1947; FUntilMonth: 8; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 19800; FRuleFamily: nil; FFmtStr: '+0530'; FUntilYear: 1987; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Tokyo zone }
  CZone_289_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 33539; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1887; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 54000; FUntilTimeMode: trUniversal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[68]; FFmtStr: 'J%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Tomsk zone }
  CZone_290_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 20391; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+07/+08'; FUntilYear: 2002; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+06/+07'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2016; FUntilMonth: 5; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Ulaanbaatar zone }
  CZone_291_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 25652; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1905; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1978; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[74]; FFmtStr: '+08/+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Urumqi zone }
  CZone_292_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 21020; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1928; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Ust-Nera zone }
  CZone_293_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 34374; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 1981; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+11/+12'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 2011; FUntilMonth: 9; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Vientiane zone }
  CZone_294_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 24624; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1906; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25590; FRuleFamily: nil; FFmtStr: 'PLMT'; FUntilYear: 1911; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1942; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1945; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 1947; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1955; FUntilMonth: 4; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Vladivostok zone }
  CZone_295_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 31651; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 11; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+10/+11'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Yakutsk zone }
  CZone_296_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 31138; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 12; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+08/+09'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+09/+10'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Yangon zone }
  CZone_297_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 23087; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23087; FRuleFamily: nil; FFmtStr: 'RMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23400; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 1942; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23400; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Yekaterinburg zone }
  CZone_298_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 14553; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 13505; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[1]; FUntilTime: 14400; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+05/+06'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+05/+06'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Asia/Yerevan zone }
  CZone_299_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 10680; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1957; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+03/+04'; FUntilYear: 1995; FUntilMonth: 9; FUntilDay: @CRelativeDays[32]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[57]; FFmtStr: '+04/+05'; FUntilYear: 2011; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[58]; FFmtStr: '+04/+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Azores zone }
  CZone_300_Arr: array[0 .. 14] of TPeriod = (
    (FOffset: -6160; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -6872; FRuleFamily: nil; FFmtStr: 'HMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-02/-01'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: @CRelativeDays[14]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+00'; FUntilYear: 1942; FUntilMonth: 8; FUntilDay: @CRelativeDays[1]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-02/-01'; FUntilYear: 1943; FUntilMonth: 4; FUntilDay: @CRelativeDays[17]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+00'; FUntilYear: 1943; FUntilMonth: 8; FUntilDay: @CRelativeDays[16]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-02/-01'; FUntilYear: 1944; FUntilMonth: 4; FUntilDay: @CRelativeDays[33]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+00'; FUntilYear: 1944; FUntilMonth: 8; FUntilDay: @CRelativeDays[19]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-02/-01'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[22]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+00'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: @CRelativeDays[14]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -7200; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-02/-01'; FUntilYear: 1966; FUntilMonth: 4; FUntilDay: @CRelativeDays[6]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-01/+00'; FUntilYear: 1983; FUntilMonth: 9; FUntilDay: @CRelativeDays[14]; FUntilTime: 3600; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[15]; FFmtStr: '-01/+00'; FUntilYear: 1992; FUntilMonth: 9; FUntilDay: @CRelativeDays[28]; FUntilTime: 3600; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'WE%sT'; FUntilYear: 1993; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: '-01/+00'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Bermuda zone }
  CZone_301_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -15558; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1930; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: 'AST'; FUntilYear: 1974; FUntilMonth: 4; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[51]; FFmtStr: 'A%sT'; FUntilYear: 1976; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'A%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Canary zone }
  CZone_302_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -3696; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1922; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1946; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 3600; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1980; FUntilMonth: 4; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WEST'; FUntilYear: 1980; FUntilMonth: 9; FUntilDay: @CRelativeDays[16]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'WE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Cape_Verde zone }
  CZone_303_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -5644; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1942; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 1945; FUntilMonth: 10; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 1975; FUntilMonth: 11; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Faroe zone }
  CZone_304_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -1624; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 1; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1981; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'WE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Jan_Mayen zone }
  CZone_305_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Madeira zone }
  CZone_306_Arr: array[0 .. 12] of TPeriod = (
    (FOffset: -4056; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -4056; FRuleFamily: nil; FFmtStr: 'FMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-01/+00'; FUntilYear: 1942; FUntilMonth: 4; FUntilDay: @CRelativeDays[14]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+01'; FUntilYear: 1942; FUntilMonth: 8; FUntilDay: @CRelativeDays[1]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-01/+00'; FUntilYear: 1943; FUntilMonth: 4; FUntilDay: @CRelativeDays[17]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+01'; FUntilYear: 1943; FUntilMonth: 8; FUntilDay: @CRelativeDays[16]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-01/+00'; FUntilYear: 1944; FUntilMonth: 4; FUntilDay: @CRelativeDays[33]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+01'; FUntilYear: 1944; FUntilMonth: 8; FUntilDay: @CRelativeDays[19]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-01/+00'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[22]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '+01'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: @CRelativeDays[14]; FUntilTime: 79200; FUntilTimeMode: trStandard),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[42]; FFmtStr: '-01/+00'; FUntilYear: 1966; FUntilMonth: 4; FUntilDay: @CRelativeDays[6]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[42]; FFmtStr: 'WE%sT'; FUntilYear: 1983; FUntilMonth: 9; FUntilDay: @CRelativeDays[14]; FUntilTime: 3600; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'WE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Reykjavik zone }
  CZone_307_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -5280; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1908; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -3600; FRuleFamily: @CRuleFamilies[32]; FFmtStr: '-01/+00'; FUntilYear: 1968; FUntilMonth: 4; FUntilDay: @CRelativeDays[13]; FUntilTime: 3600; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/South_Georgia zone }
  CZone_308_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -8768; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/Stanley zone }
  CZone_309_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -13884; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -13884; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1912; FUntilMonth: 3; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[6]; FFmtStr: '-04/-03'; FUntilYear: 1983; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: @CRuleFamilies[6]; FFmtStr: '-03/-02'; FUntilYear: 1985; FUntilMonth: 9; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -14400; FRuleFamily: @CRuleFamilies[6]; FFmtStr: '-04/-03'; FUntilYear: 2010; FUntilMonth: 9; FUntilDay: @CRelativeDays[9]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Atlantic/St_Helena zone }
  CZone_310_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -1368; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -1368; FRuleFamily: nil; FFmtStr: 'JMT'; FUntilYear: 1951; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Adelaide zone }
  CZone_311_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 33260; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'ACST'; FUntilYear: 1899; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AC%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: @CRuleFamilies[129]; FFmtStr: 'AC%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Brisbane zone }
  CZone_312_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 36728; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[127]; FFmtStr: 'AE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Broken_Hill zone }
  CZone_313_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 33948; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEST'; FUntilYear: 1896; FUntilMonth: 8; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'ACST'; FUntilYear: 1899; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AC%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: @CRuleFamilies[132]; FFmtStr: 'AC%sT'; FUntilYear: 2000; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: @CRuleFamilies[129]; FFmtStr: 'AC%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Currie zone }
  CZone_314_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 34528; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEDT'; FUntilYear: 1917; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1971; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[130]; FFmtStr: 'AE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Darwin zone }
  CZone_315_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 31400; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: 'ACST'; FUntilYear: 1899; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34200; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AC%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Eucla zone }
  CZone_316_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 30928; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 31500; FRuleFamily: @CRuleFamilies[125]; FFmtStr: '+0845/+0945'; FUntilYear: 1943; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 31500; FRuleFamily: @CRuleFamilies[126]; FFmtStr: '+0845/+0945'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Hobart zone }
  CZone_317_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 35356; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEDT'; FUntilYear: 1917; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1967; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[130]; FFmtStr: 'AE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Lindeman zone }
  CZone_318_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 35756; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[127]; FFmtStr: 'AE%sT'; FUntilYear: 1992; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[128]; FFmtStr: 'AE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Lord_Howe zone }
  CZone_319_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 38180; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'AEST'; FUntilYear: 1981; FUntilMonth: 3; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 37800; FRuleFamily: @CRuleFamilies[133]; FFmtStr: '+1030/+1130'; FUntilYear: 1985; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 37800; FRuleFamily: @CRuleFamilies[133]; FFmtStr: '+1030/+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Melbourne zone }
  CZone_320_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 34792; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[131]; FFmtStr: 'AE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Perth zone }
  CZone_321_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 27804; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 12; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AW%sT'; FUntilYear: 1943; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 28800; FRuleFamily: @CRuleFamilies[126]; FFmtStr: 'AW%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Australia/Sydney zone }
  CZone_322_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 36292; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[125]; FFmtStr: 'AE%sT'; FUntilYear: 1971; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[132]; FFmtStr: 'AE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for CET zone }
  CZone_323_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for CST6CDT zone }
  CZone_324_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'C%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for EET zone }
  CZone_325_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for EST zone }
  CZone_326_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: 'EST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for EST5EDT zone }
  CZone_327_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -18000; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'E%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT zone }
  CZone_328_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+1 zone }
  CZone_329_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -3600; FRuleFamily: nil; FFmtStr: '-01'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+10 zone }
  CZone_330_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: '-10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+11 zone }
  CZone_331_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+12 zone }
  CZone_332_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -43200; FRuleFamily: nil; FFmtStr: '-12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+2 zone }
  CZone_333_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -7200; FRuleFamily: nil; FFmtStr: '-02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+3 zone }
  CZone_334_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -10800; FRuleFamily: nil; FFmtStr: '-03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+4 zone }
  CZone_335_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -14400; FRuleFamily: nil; FFmtStr: '-04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+5 zone }
  CZone_336_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+6 zone }
  CZone_337_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -21600; FRuleFamily: nil; FFmtStr: '-06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+7 zone }
  CZone_338_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: '-07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+8 zone }
  CZone_339_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: '-08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT+9 zone }
  CZone_340_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -32400; FRuleFamily: nil; FFmtStr: '-09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-1 zone }
  CZone_341_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: '+01'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-10 zone }
  CZone_342_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-11 zone }
  CZone_343_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-12 zone }
  CZone_344_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-13 zone }
  CZone_345_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 46800; FRuleFamily: nil; FFmtStr: '+13'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-14 zone }
  CZone_346_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 50400; FRuleFamily: nil; FFmtStr: '+14'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-2 zone }
  CZone_347_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: '+02'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-3 zone }
  CZone_348_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-4 zone }
  CZone_349_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-5 zone }
  CZone_350_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-6 zone }
  CZone_351_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-7 zone }
  CZone_352_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-8 zone }
  CZone_353_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 28800; FRuleFamily: nil; FFmtStr: '+08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/GMT-9 zone }
  CZone_354_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Etc/UTC zone }
  CZone_355_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'UTC'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Amsterdam zone }
  CZone_356_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 1172; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1835; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 1172; FRuleFamily: @CRuleFamilies[39]; FFmtStr: '%s'; FUntilYear: 1937; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 1200; FRuleFamily: @CRuleFamilies[39]; FFmtStr: '+0020/+0120'; FUntilYear: 1940; FUntilMonth: 5; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[39]; FFmtStr: 'CE%sT'; FUntilYear: 1977; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Andorra zone }
  CZone_357_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 364; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1946; FUntilMonth: 9; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1985; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Astrakhan zone }
  CZone_358_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 11532; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 2016; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Athens zone }
  CZone_359_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 5692; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 9; FUntilDay: @CRelativeDays[21]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5692; FRuleFamily: nil; FFmtStr: 'AMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[16]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[30]; FFmtStr: 'EE%sT'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[30]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 4; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[30]; FFmtStr: 'EE%sT'; FUntilYear: 1981; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Belfast zone }
  CZone_360_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -1420; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 8; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -1521; FRuleFamily: nil; FFmtStr: 'DMT'; FUntilYear: 1916; FUntilMonth: 5; FUntilDay: @CRelativeDays[22]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -1521; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1968; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1971; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'GMT/BST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Belgrade zone }
  CZone_361_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 4920; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[11]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1982; FUntilMonth: 11; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Berlin zone }
  CZone_362_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 3208; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1893; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[32]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[29]; FFmtStr: 'CE%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[28]; FFmtStr: 'CE%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Brussels zone }
  CZone_363_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 1050; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 1050; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1892; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 1050; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'WET'; FUntilYear: 1914; FUntilMonth: 11; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1916; FUntilMonth: 5; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1918; FUntilMonth: 11; FUntilDay: @CRelativeDays[23]; FUntilTime: 39600; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[21]; FFmtStr: 'WE%sT'; FUntilYear: 1940; FUntilMonth: 5; FUntilDay: @CRelativeDays[10]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 9; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[21]; FFmtStr: 'CE%sT'; FUntilYear: 1977; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Bucharest zone }
  CZone_364_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 6264; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1891; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 6264; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1931; FUntilMonth: 7; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[38]; FFmtStr: 'EE%sT'; FUntilYear: 1981; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'EE%sT'; FUntilYear: 1991; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[38]; FFmtStr: 'EE%sT'; FUntilYear: 1994; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Budapest zone }
  CZone_365_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 4580; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1918; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[31]; FFmtStr: 'CE%sT'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[31]; FFmtStr: 'CE%sT'; FUntilYear: 1980; FUntilMonth: 9; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Chisinau zone }
  CZone_366_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 6920; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 6900; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1918; FUntilMonth: 2; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 6264; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1931; FUntilMonth: 7; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[38]; FFmtStr: 'EE%sT'; FUntilYear: 1940; FUntilMonth: 8; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EEST'; FUntilYear: 1941; FUntilMonth: 7; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 8; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1990; FUntilMonth: 5; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'EE%sT'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[37]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Copenhagen zone }
  CZone_367_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 3020; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3020; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1894; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[24]; FFmtStr: 'CE%sT'; FUntilYear: 1942; FUntilMonth: 11; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[24]; FFmtStr: 'CE%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Dublin zone }
  CZone_368_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: -1500; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 8; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -1521; FRuleFamily: nil; FFmtStr: 'DMT'; FUntilYear: 1916; FUntilMonth: 5; FUntilDay: @CRelativeDays[22]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: -1521; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1921; FUntilMonth: 12; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: 'GMT/IST'; FUntilYear: 1940; FUntilMonth: 2; FUntilDay: @CRelativeDays[14]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 1946; FUntilMonth: 10; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1947; FUntilMonth: 3; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'IST'; FUntilYear: 1947; FUntilMonth: 11; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1948; FUntilMonth: 4; FUntilDay: @CRelativeDays[11]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: 'GMT/IST'; FUntilYear: 1968; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[14]; FFmtStr: 'IST/GMT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Gibraltar zone }
  CZone_369_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -1284; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 8; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1957; FUntilMonth: 4; FUntilDay: @CRelativeDays[21]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1982; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Guernsey zone }
  CZone_370_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -609; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1913; FUntilMonth: 6; FUntilDay: @CRelativeDays[11]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1940; FUntilMonth: 7; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1968; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1971; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'GMT/BST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Helsinki zone }
  CZone_371_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 5989; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1878; FUntilMonth: 5; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5989; FRuleFamily: nil; FFmtStr: 'HMT'; FUntilYear: 1921; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[26]; FFmtStr: 'EE%sT'; FUntilYear: 1983; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Isle_of_Man zone }
  CZone_372_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -1075; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1883; FUntilMonth: 3; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1968; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1971; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'GMT/BST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Istanbul zone }
  CZone_373_Arr: array[0 .. 12] of TPeriod = (
    (FOffset: 6952; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7016; FRuleFamily: nil; FFmtStr: 'IMT'; FUntilYear: 1910; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[46]; FFmtStr: 'EE%sT'; FUntilYear: 1978; FUntilMonth: 6; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[46]; FFmtStr: '+03/+04'; FUntilYear: 1984; FUntilMonth: 11; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[46]; FFmtStr: 'EE%sT'; FUntilYear: 2007; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 2014; FUntilMonth: 3; FUntilDay: @CRelativeDays[7]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2014; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 2015; FUntilMonth: 10; FUntilDay: @CRelativeDays[14]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EEST'; FUntilYear: 2015; FUntilMonth: 11; FUntilDay: @CRelativeDays[44]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 2016; FUntilMonth: 9; FUntilDay: @CRelativeDays[13]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Jersey zone }
  CZone_374_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -506; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1898; FUntilMonth: 6; FUntilDay: @CRelativeDays[23]; FUntilTime: 57600; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1940; FUntilMonth: 7; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1968; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1971; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'GMT/BST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Kaliningrad zone }
  CZone_375_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 4920; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1893; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[41]; FFmtStr: 'EE%sT'; FUntilYear: 1946; FUntilMonth: 4; FUntilDay: @CRelativeDays[13]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'EE%sT'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Kiev zone }
  CZone_376_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 7324; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7324; FRuleFamily: nil; FFmtStr: 'KMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 9; FUntilDay: @CRelativeDays[10]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1943; FUntilMonth: 11; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1990; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EEST'; FUntilYear: 1991; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1995; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Kirov zone }
  CZone_377_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 11928; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Lisbon zone }
  CZone_378_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -2205; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -2205; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[42]; FFmtStr: 'WE%sT'; FUntilYear: 1966; FUntilMonth: 4; FUntilDay: @CRelativeDays[6]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1976; FUntilMonth: 9; FUntilDay: @CRelativeDays[19]; FUntilTime: 3600; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[42]; FFmtStr: 'WE%sT'; FUntilYear: 1983; FUntilMonth: 9; FUntilDay: @CRelativeDays[14]; FUntilTime: 3600; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[15]; FFmtStr: 'WE%sT'; FUntilYear: 1992; FUntilMonth: 9; FUntilDay: @CRelativeDays[28]; FUntilTime: 3600; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 1996; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'WE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Ljubljana zone }
  CZone_379_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 3484; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[11]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1982; FUntilMonth: 11; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/London zone }
  CZone_380_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -75; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1847; FUntilMonth: 12; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1968; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'BST'; FUntilYear: 1971; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trUniversal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[12]; FFmtStr: '%s'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'GMT/BST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Luxembourg zone }
  CZone_381_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 1476; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1904; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[35]; FFmtStr: 'CE%sT'; FUntilYear: 1918; FUntilMonth: 11; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[35]; FFmtStr: 'WE%sT'; FUntilYear: 1929; FUntilMonth: 10; FUntilDay: @CRelativeDays[18]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[21]; FFmtStr: 'WE%sT'; FUntilYear: 1940; FUntilMonth: 5; FUntilDay: @CRelativeDays[21]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'WE%sT'; FUntilYear: 1944; FUntilMonth: 9; FUntilDay: @CRelativeDays[11]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[21]; FFmtStr: 'CE%sT'; FUntilYear: 1977; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Madrid zone }
  CZone_382_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -884; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 85516; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[43]; FFmtStr: 'WE%sT'; FUntilYear: 1940; FUntilMonth: 3; FUntilDay: @CRelativeDays[24]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[43]; FFmtStr: 'CE%sT'; FUntilYear: 1979; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Malta zone }
  CZone_383_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 3484; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1893; FUntilMonth: 11; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[33]; FFmtStr: 'CE%sT'; FUntilYear: 1973; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[36]; FFmtStr: 'CE%sT'; FUntilYear: 1981; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Minsk zone }
  CZone_384_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 6616; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 6600; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 6; FUntilDay: @CRelativeDays[16]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 7; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1990; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'EE%sT'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Monaco zone }
  CZone_385_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 1772; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1891; FUntilMonth: 3; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 561; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1911; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[27]; FFmtStr: 'WE%sT'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[27]; FFmtStr: 'CE%sT'; FUntilYear: 1977; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Moscow zone }
  CZone_386_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: 9017; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9017; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1916; FUntilMonth: 7; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 9079; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '%s'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '%s'; FUntilYear: 1921; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1922; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'EE%sT'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Oslo zone }
  CZone_387_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 2580; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[40]; FFmtStr: 'CE%sT'; FUntilYear: 1940; FUntilMonth: 8; FUntilDay: @CRelativeDays[34]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[40]; FFmtStr: 'CE%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Paris zone }
  CZone_388_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 561; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1891; FUntilMonth: 3; FUntilDay: @CRelativeDays[1]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: 561; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1911; FUntilMonth: 3; FUntilDay: @CRelativeDays[23]; FUntilTime: 60; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[27]; FFmtStr: 'WE%sT'; FUntilYear: 1940; FUntilMonth: 6; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 8; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 0; FRuleFamily: @CRuleFamilies[27]; FFmtStr: 'WE%sT'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[27]; FFmtStr: 'CE%sT'; FUntilYear: 1977; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Prague zone }
  CZone_389_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 3464; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1850; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3464; FRuleFamily: nil; FFmtStr: 'PMT'; FUntilYear: 1891; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[25]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[23]; FFmtStr: 'CE%sT'; FUntilYear: 1946; FUntilMonth: 12; FUntilDay: @CRelativeDays[0]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'GMT'; FUntilYear: 1947; FUntilMonth: 2; FUntilDay: @CRelativeDays[4]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[23]; FFmtStr: 'CE%sT'; FUntilYear: 1979; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Riga zone }
  CZone_390_Arr: array[0 .. 14] of TPeriod = (
    (FOffset: 5794; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5794; FRuleFamily: nil; FFmtStr: 'RMT'; FUntilYear: 1918; FUntilMonth: 4; FUntilDay: @CRelativeDays[1]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 5794; FRuleFamily: nil; FFmtStr: 'LST'; FUntilYear: 1918; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 5794; FRuleFamily: nil; FFmtStr: 'RMT'; FUntilYear: 1919; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 5794; FRuleFamily: nil; FFmtStr: 'LST'; FUntilYear: 1919; FUntilMonth: 5; FUntilDay: @CRelativeDays[33]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 5794; FRuleFamily: nil; FFmtStr: 'RMT'; FUntilYear: 1926; FUntilMonth: 5; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1940; FUntilMonth: 8; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 10; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EEST'; FUntilYear: 1989; FUntilMonth: 9; FUntilDay: @CRelativeDays[39]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[34]; FFmtStr: 'EE%sT'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 2000; FUntilMonth: 2; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2001; FUntilMonth: 1; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Rome zone }
  CZone_391_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 2996; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1866; FUntilMonth: 12; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 2996; FRuleFamily: nil; FFmtStr: 'RMT'; FUntilYear: 1893; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 85796; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[33]; FFmtStr: 'CE%sT'; FUntilYear: 1943; FUntilMonth: 9; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 6; FUntilDay: @CRelativeDays[15]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[33]; FFmtStr: 'CE%sT'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Samara zone }
  CZone_392_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 12020; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1935; FUntilMonth: 1; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+02/+03'; FUntilYear: 1991; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1991; FUntilMonth: 10; FUntilDay: @CRelativeDays[10]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 2010; FUntilMonth: 3; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Sarajevo zone }
  CZone_393_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 4420; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[11]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1982; FUntilMonth: 11; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Saratov zone }
  CZone_394_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 11058; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1988; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 2016; FUntilMonth: 12; FUntilDay: @CRelativeDays[15]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Simferopol zone }
  CZone_395_Arr: array[0 .. 15] of TPeriod = (
    (FOffset: 8184; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 8160; FRuleFamily: nil; FFmtStr: 'SMT'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 11; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 4; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1990; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1990; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1994; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'MSK/MSD'; FUntilYear: 1996; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSD'; FUntilYear: 1996; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 10800; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1997; FUntilMonth: 3; FUntilDay: @CRelativeDays[39]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 2014; FUntilMonth: 3; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Skopje zone }
  CZone_396_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 5144; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[11]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1982; FUntilMonth: 11; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Sofia zone }
  CZone_397_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 5596; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7016; FRuleFamily: nil; FFmtStr: 'IMT'; FUntilYear: 1894; FUntilMonth: 11; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1942; FUntilMonth: 11; FUntilDay: @CRelativeDays[2]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1979; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[22]; FFmtStr: 'EE%sT'; FUntilYear: 1982; FUntilMonth: 9; FUntilDay: @CRelativeDays[19]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'EE%sT'; FUntilYear: 1991; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1997; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Stockholm zone }
  CZone_398_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 4332; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1879; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3614; FRuleFamily: nil; FFmtStr: 'SET'; FUntilYear: 1900; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1916; FUntilMonth: 5; FUntilDay: @CRelativeDays[21]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1916; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 3600; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1980; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Tallinn zone }
  CZone_399_Arr: array[0 .. 12] of TPeriod = (
    (FOffset: 5940; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5940; FRuleFamily: nil; FFmtStr: 'TMT'; FUntilYear: 1918; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5940; FRuleFamily: nil; FFmtStr: 'TMT'; FUntilYear: 1921; FUntilMonth: 5; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1940; FUntilMonth: 8; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 9; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 9; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EEST'; FUntilYear: 1989; FUntilMonth: 9; FUntilDay: @CRelativeDays[32]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'EE%sT'; FUntilYear: 1998; FUntilMonth: 9; FUntilDay: @CRelativeDays[33]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 14400; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2002; FUntilMonth: 2; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Tirane zone }
  CZone_400_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 4760; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1914; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1940; FUntilMonth: 6; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[19]; FFmtStr: 'CE%sT'; FUntilYear: 1984; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Tiraspol zone }
  CZone_401_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 7112; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 6900; FRuleFamily: nil; FFmtStr: 'CMT'; FUntilYear: 1918; FUntilMonth: 2; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 6264; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1931; FUntilMonth: 7; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[38]; FFmtStr: 'EE%sT'; FUntilYear: 1940; FUntilMonth: 8; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EEST'; FUntilYear: 1941; FUntilMonth: 7; FUntilDay: @CRelativeDays[17]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 8; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'EE%sT'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Ulyanovsk zone }
  CZone_402_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 11616; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1919; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trUniversal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+02/+03'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: @CRelativeDays[29]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 2016; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Uzhgorod zone }
  CZone_403_Arr: array[0 .. 10] of TPeriod = (
    (FOffset: 5352; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1940; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1944; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1945; FUntilMonth: 6; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1990; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1990; FUntilMonth: 7; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1992; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1995; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Vaduz zone }
  CZone_404_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 2284; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1894; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[45]; FFmtStr: 'CE%sT'; FUntilYear: 1981; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Vienna zone }
  CZone_405_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 3921; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1893; FUntilMonth: 4; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[20]; FFmtStr: 'CE%sT'; FUntilYear: 1940; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1945; FUntilMonth: 4; FUntilDay: @CRelativeDays[31]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[20]; FFmtStr: 'CE%sT'; FUntilYear: 1981; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Vilnius zone }
  CZone_406_Arr: array[0 .. 14] of TPeriod = (
    (FOffset: 6076; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5040; FRuleFamily: nil; FFmtStr: 'WMT'; FUntilYear: 1917; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5736; FRuleFamily: nil; FFmtStr: 'KMT'; FUntilYear: 1919; FUntilMonth: 10; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1920; FUntilMonth: 7; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1920; FUntilMonth: 10; FUntilDay: @CRelativeDays[25]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1940; FUntilMonth: 8; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 6; FUntilDay: @CRelativeDays[32]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1989; FUntilMonth: 3; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'EE%sT'; FUntilYear: 1991; FUntilMonth: 9; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'EE%sT'; FUntilYear: 1998; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1998; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 1999; FUntilMonth: 10; FUntilDay: @CRelativeDays[8]; FUntilTime: 3600; FUntilTimeMode: trUniversal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 2003; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Volgograd zone }
  CZone_407_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: 10660; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1920; FUntilMonth: 1; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1961; FUntilMonth: 11; FUntilDay: @CRelativeDays[23]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+04/+05'; FUntilYear: 1988; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 1992; FUntilMonth: 3; FUntilDay: @CRelativeDays[26]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: '+03/+04'; FUntilYear: 2011; FUntilMonth: 3; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 2014; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: '+03'; FUntilYear: 2018; FUntilMonth: 10; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Warsaw zone }
  CZone_408_Arr: array[0 .. 8] of TPeriod = (
    (FOffset: 5040; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 5040; FRuleFamily: nil; FFmtStr: 'WMT'; FUntilYear: 1915; FUntilMonth: 8; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1918; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 10800; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[41]; FFmtStr: 'EE%sT'; FUntilYear: 1922; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[41]; FFmtStr: 'CE%sT'; FUntilYear: 1940; FUntilMonth: 6; FUntilDay: @CRelativeDays[4]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1944; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[41]; FFmtStr: 'CE%sT'; FUntilYear: 1977; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[15]; FFmtStr: 'CE%sT'; FUntilYear: 1988; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Zagreb zone }
  CZone_409_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 3832; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1884; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[11]; FUntilTime: 82800; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1945; FUntilMonth: 5; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CEST'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[24]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 3600; FRuleFamily: nil; FFmtStr: 'CET'; FUntilYear: 1982; FUntilMonth: 11; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Zaporozhye zone }
  CZone_410_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 8440; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 8400; FRuleFamily: nil; FFmtStr: '+0220'; FUntilYear: 1924; FUntilMonth: 5; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: nil; FFmtStr: 'EET'; FUntilYear: 1930; FUntilMonth: 6; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'MSK'; FUntilYear: 1941; FUntilMonth: 8; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'CE%sT'; FUntilYear: 1943; FUntilMonth: 10; FUntilDay: @CRelativeDays[14]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: @CRuleFamilies[18]; FFmtStr: 'MSK/MSD'; FUntilYear: 1991; FUntilMonth: 3; FUntilDay: @CRelativeDays[8]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[17]; FFmtStr: 'EE%sT'; FUntilYear: 1995; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 7200; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'EE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Europe/Zurich zone }
  CZone_411_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 2048; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1853; FUntilMonth: 7; FUntilDay: @CRelativeDays[24]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 1786; FRuleFamily: nil; FFmtStr: 'BMT'; FUntilYear: 1894; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[45]; FFmtStr: 'CE%sT'; FUntilYear: 1981; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'CE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Factory zone }
  CZone_412_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for HST zone }
  CZone_413_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: 'HST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Antananarivo zone }
  CZone_414_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 11404; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 1954; FUntilMonth: 2; FUntilDay: @CRelativeDays[28]; FUntilTime: 82800; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAST'; FUntilYear: 1954; FUntilMonth: 5; FUntilDay: @CRelativeDays[26]; FUntilTime: 82800; FUntilTimeMode: trStandard),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Chagos zone }
  CZone_415_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 17380; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1907; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 1996; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 21600; FRuleFamily: nil; FFmtStr: '+06'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Christmas zone }
  CZone_416_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 25372; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1895; FUntilMonth: 2; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 25200; FRuleFamily: nil; FFmtStr: '+07'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Cocos zone }
  CZone_417_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 23260; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1900; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 23400; FRuleFamily: nil; FFmtStr: '+0630'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Comoro zone }
  CZone_418_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 10384; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Kerguelen zone }
  CZone_419_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 0; FRuleFamily: nil; FFmtStr: '-00'; FUntilYear: 1950; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Mahe zone }
  CZone_420_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 13308; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1906; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Maldives zone }
  CZone_421_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 17640; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 17640; FRuleFamily: nil; FFmtStr: 'MMT'; FUntilYear: 1960; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 18000; FRuleFamily: nil; FFmtStr: '+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Mauritius zone }
  CZone_422_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 13800; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1907; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: @CRuleFamilies[119]; FFmtStr: '+04/+05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Mayotte zone }
  CZone_423_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 10856; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 10800; FRuleFamily: nil; FFmtStr: 'EAT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Indian/Reunion zone }
  CZone_424_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 13312; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 6; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 14400; FRuleFamily: nil; FFmtStr: '+04'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for MET zone }
  CZone_425_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 3600; FRuleFamily: @CRuleFamilies[16]; FFmtStr: 'ME%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for MST zone }
  CZone_426_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -25200; FRuleFamily: nil; FFmtStr: 'MST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for MST7MDT zone }
  CZone_427_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'M%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Apia zone }
  CZone_428_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 45184; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1892; FUntilMonth: 7; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -41216; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -41400; FRuleFamily: nil; FFmtStr: '-1130'; FUntilYear: 1950; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: @CRuleFamilies[139]; FFmtStr: '-11/-10'; FUntilYear: 2011; FUntilMonth: 12; FUntilDay: @CRelativeDays[26]; FUntilTime: 86400; FUntilTimeMode: trLocal),
    (FOffset: 46800; FRuleFamily: @CRuleFamilies[139]; FFmtStr: '+13/+14'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Auckland zone }
  CZone_429_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 41944; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1868; FUntilMonth: 11; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 41400; FRuleFamily: @CRuleFamilies[52]; FFmtStr: 'NZ%sT'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[52]; FFmtStr: 'NZ%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Bougainville zone }
  CZone_430_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: 37336; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 35312; FRuleFamily: nil; FFmtStr: 'PMMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1942; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: @CRelativeDays[22]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2014; FUntilMonth: 12; FUntilDay: @CRelativeDays[16]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Chatham zone }
  CZone_431_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 44028; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1868; FUntilMonth: 11; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 44100; FRuleFamily: nil; FFmtStr: '+1215'; FUntilYear: 1946; FUntilMonth: 1; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 45900; FRuleFamily: @CRuleFamilies[137]; FFmtStr: '+1245/+1345'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Chuuk zone }
  CZone_432_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: -49972; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36428; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1914; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1919; FUntilMonth: 2; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Easter zone }
  CZone_433_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -26248; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1890; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -26248; FRuleFamily: nil; FFmtStr: 'EMT'; FUntilYear: 1932; FUntilMonth: 9; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -25200; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-07/-06'; FUntilYear: 1982; FUntilMonth: 3; FUntilDay: @CRelativeDays[21]; FUntilTime: 10800; FUntilTimeMode: trUniversal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[3]; FFmtStr: '-06/-05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Efate zone }
  CZone_434_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 40396; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[141]; FFmtStr: '+11/+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Enderbury zone }
  CZone_435_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -41060; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -43200; FRuleFamily: nil; FFmtStr: '-12'; FUntilYear: 1979; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-11'; FUntilYear: 1994; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 46800; FRuleFamily: nil; FFmtStr: '+13'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Fakaofo zone }
  CZone_436_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -41096; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-11'; FUntilYear: 2011; FUntilMonth: 12; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 46800; FRuleFamily: nil; FFmtStr: '+13'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Fiji zone }
  CZone_437_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 42944; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1915; FUntilMonth: 10; FUntilDay: @CRelativeDays[19]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: @CRuleFamilies[134]; FFmtStr: '+12/+13'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Funafuti zone }
  CZone_438_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 43012; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Galapagos zone }
  CZone_439_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -21504; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1931; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -18000; FRuleFamily: nil; FFmtStr: '-05'; FUntilYear: 1986; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -21600; FRuleFamily: @CRuleFamilies[5]; FFmtStr: '-06/-05'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Gambier zone }
  CZone_440_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -32388; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -32400; FRuleFamily: nil; FFmtStr: '-09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Guadalcanal zone }
  CZone_441_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 38388; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Guam zone }
  CZone_442_Arr: array[0 .. 5] of TPeriod = (
    (FOffset: -51660; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34740; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'GST'; FUntilYear: 1941; FUntilMonth: 12; FUntilDay: @CRelativeDays[34]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1944; FUntilMonth: 7; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: @CRuleFamilies[135]; FFmtStr: 'G%sT'; FUntilYear: 2000; FUntilMonth: 12; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'ChST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Honolulu zone }
  CZone_443_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -37886; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1896; FUntilMonth: 1; FUntilDay: @CRelativeDays[12]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -37800; FRuleFamily: nil; FFmtStr: 'HST'; FUntilYear: 1933; FUntilMonth: 4; FUntilDay: @CRelativeDays[7]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -37800; FRuleFamily: nil; FFmtStr: 'HDT'; FUntilYear: 1933; FUntilMonth: 5; FUntilDay: @CRelativeDays[22]; FUntilTime: 43200; FUntilTimeMode: trLocal),
    (FOffset: -37800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'H%sT'; FUntilYear: 1947; FUntilMonth: 6; FUntilDay: @CRelativeDays[44]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: 'HST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Johnston zone }
  CZone_444_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: 'HST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Kiritimati zone }
  CZone_445_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -37760; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -38400; FRuleFamily: nil; FFmtStr: '-1040'; FUntilYear: 1979; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: '-10'; FUntilYear: 1994; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 50400; FRuleFamily: nil; FFmtStr: '+14'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Kosrae zone }
  CZone_446_Arr: array[0 .. 9] of TPeriod = (
    (FOffset: -47284; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39116; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1914; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1919; FUntilMonth: 2; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1937; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 1999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Kwajalein zone }
  CZone_447_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 40160; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1937; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1944; FUntilMonth: 2; FUntilDay: @CRelativeDays[18]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -43200; FRuleFamily: nil; FFmtStr: '-12'; FUntilYear: 1993; FUntilMonth: 8; FUntilDay: @CRelativeDays[10]; FUntilTime: 86400; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Majuro zone }
  CZone_448_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: 41088; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1914; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1919; FUntilMonth: 2; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1937; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1944; FUntilMonth: 1; FUntilDay: @CRelativeDays[7]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Marquesas zone }
  CZone_449_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -33480; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -34200; FRuleFamily: nil; FFmtStr: '-0930'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Midway zone }
  CZone_450_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -42568; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-11'; FUntilYear: 1956; FUntilMonth: 6; FUntilDay: @CRelativeDays[6]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-10'; FUntilYear: 1956; FUntilMonth: 9; FUntilDay: @CRelativeDays[2]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Nauru zone }
  CZone_451_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: 40060; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1921; FUntilMonth: 1; FUntilDay: @CRelativeDays[1]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 41400; FRuleFamily: nil; FFmtStr: '+1130'; FUntilYear: 1942; FUntilMonth: 8; FUntilDay: @CRelativeDays[26]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 9; FUntilDay: @CRelativeDays[44]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 41400; FRuleFamily: nil; FFmtStr: '+1130'; FUntilYear: 1979; FUntilMonth: 2; FUntilDay: @CRelativeDays[34]; FUntilTime: 7200; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Niue zone }
  CZone_452_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: -40780; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -40800; FRuleFamily: nil; FFmtStr: '-1120'; FUntilYear: 1951; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -41400; FRuleFamily: nil; FFmtStr: '-1130'; FUntilYear: 1978; FUntilMonth: 10; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: '-11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Norfolk zone }
  CZone_453_Arr: array[0 .. 6] of TPeriod = (
    (FOffset: 40312; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 40320; FRuleFamily: nil; FFmtStr: '+1112'; FUntilYear: 1951; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 41400; FRuleFamily: nil; FFmtStr: '+1130'; FUntilYear: 1974; FUntilMonth: 10; FUntilDay: @CRelativeDays[28]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 41400; FRuleFamily: nil; FFmtStr: '+1230'; FUntilYear: 1975; FUntilMonth: 3; FUntilDay: @CRelativeDays[2]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 41400; FRuleFamily: nil; FFmtStr: '+1130'; FUntilYear: 2015; FUntilMonth: 10; FUntilDay: @CRelativeDays[15]; FUntilTime: 7200; FUntilTimeMode: trStandard),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 2019; FUntilMonth: 7; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[132]; FFmtStr: '+11/+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Noumea zone }
  CZone_454_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 39948; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 1; FUntilDay: @CRelativeDays[12]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: @CRuleFamilies[136]; FFmtStr: '+11/+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Pago_Pago zone }
  CZone_455_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 45432; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1892; FUntilMonth: 7; FUntilDay: @CRelativeDays[9]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -40968; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1911; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -39600; FRuleFamily: nil; FFmtStr: 'SST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Palau zone }
  CZone_456_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -54124; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32276; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Pitcairn zone }
  CZone_457_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -31220; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -30600; FRuleFamily: nil; FFmtStr: '-0830'; FUntilYear: 1998; FUntilMonth: 4; FUntilDay: @CRelativeDays[28]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -28800; FRuleFamily: nil; FFmtStr: '-08'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Pohnpei zone }
  CZone_458_Arr: array[0 .. 7] of TPeriod = (
    (FOffset: -48428; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 37972; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1914; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1919; FUntilMonth: 2; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 1937; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 1941; FUntilMonth: 4; FUntilDay: @CRelativeDays[0]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1945; FUntilMonth: 8; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 39600; FRuleFamily: nil; FFmtStr: '+11'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Port_Moresby zone }
  CZone_459_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: 35320; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1880; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 35312; FRuleFamily: nil; FFmtStr: 'PMMT'; FUntilYear: 1895; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Rarotonga zone }
  CZone_460_Arr: array[0 .. 2] of TPeriod = (
    (FOffset: -38344; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -37800; FRuleFamily: nil; FFmtStr: '-1030'; FUntilYear: 1978; FUntilMonth: 11; FUntilDay: @CRelativeDays[31]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: @CRuleFamilies[138]; FFmtStr: '-10/-0930'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Saipan zone }
  CZone_461_Arr: array[0 .. 4] of TPeriod = (
    (FOffset: -51420; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1844; FUntilMonth: 12; FUntilDay: @CRelativeDays[8]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 34980; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 32400; FRuleFamily: nil; FFmtStr: '+09'; FUntilYear: 1969; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: '+10'; FUntilYear: 2000; FUntilMonth: 12; FUntilDay: @CRelativeDays[4]; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 36000; FRuleFamily: nil; FFmtStr: 'ChST'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Tahiti zone }
  CZone_462_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: -35896; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1912; FUntilMonth: 10; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: -36000; FRuleFamily: nil; FFmtStr: '-10'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Tarawa zone }
  CZone_463_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 41524; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Tongatapu zone }
  CZone_464_Arr: array[0 .. 3] of TPeriod = (
    (FOffset: 44360; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 44400; FRuleFamily: nil; FFmtStr: '+1220'; FUntilYear: 1941; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 46800; FRuleFamily: nil; FFmtStr: '+13'; FUntilYear: 1999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 46800; FRuleFamily: @CRuleFamilies[140]; FFmtStr: '+13/+14'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Wake zone }
  CZone_465_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 39988; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for Pacific/Wallis zone }
  CZone_466_Arr: array[0 .. 1] of TPeriod = (
    (FOffset: 44120; FRuleFamily: nil; FFmtStr: 'LMT'; FUntilYear: 1901; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal),
    (FOffset: 43200; FRuleFamily: nil; FFmtStr: '+12'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for PST8PDT zone }
  CZone_467_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: -28800; FRuleFamily: @CRuleFamilies[80]; FFmtStr: 'P%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

  { Time periods for WET zone }
  CZone_468_Arr: array[0 .. 0] of TPeriod = (
    (FOffset: 0; FRuleFamily: @CRuleFamilies[13]; FFmtStr: 'WE%sT'; FUntilYear: 9999; FUntilMonth: 1; FUntilDay: nil; FUntilTime: 0; FUntilTimeMode: trLocal)
  );

var
  { This array contains zones. }
  CZones: array[0 .. 468] of TZone = (
    (FName: 'Africa/Abidjan'; FCount: 2; FFirstPeriod: @CZone_0_Arr),
    (FName: 'Africa/Accra'; FCount: 2; FFirstPeriod: @CZone_1_Arr),
    (FName: 'Africa/Addis_Ababa'; FCount: 3; FFirstPeriod: @CZone_2_Arr),
    (FName: 'Africa/Algiers'; FCount: 10; FFirstPeriod: @CZone_3_Arr),
    (FName: 'Africa/Asmara'; FCount: 4; FFirstPeriod: @CZone_4_Arr),
    (FName: 'Africa/Bamako'; FCount: 4; FFirstPeriod: @CZone_5_Arr),
    (FName: 'Africa/Bangui'; FCount: 2; FFirstPeriod: @CZone_6_Arr),
    (FName: 'Africa/Banjul'; FCount: 4; FFirstPeriod: @CZone_7_Arr),
    (FName: 'Africa/Bissau'; FCount: 3; FFirstPeriod: @CZone_8_Arr),
    (FName: 'Africa/Blantyre'; FCount: 2; FFirstPeriod: @CZone_9_Arr),
    (FName: 'Africa/Brazzaville'; FCount: 2; FFirstPeriod: @CZone_10_Arr),
    (FName: 'Africa/Bujumbura'; FCount: 2; FFirstPeriod: @CZone_11_Arr),
    (FName: 'Africa/Cairo'; FCount: 2; FFirstPeriod: @CZone_12_Arr),
    (FName: 'Africa/Casablanca'; FCount: 5; FFirstPeriod: @CZone_13_Arr),
    (FName: 'Africa/Ceuta'; FCount: 9; FFirstPeriod: @CZone_14_Arr),
    (FName: 'Africa/Conakry'; FCount: 4; FFirstPeriod: @CZone_15_Arr),
    (FName: 'Africa/Dakar'; FCount: 3; FFirstPeriod: @CZone_16_Arr),
    (FName: 'Africa/Dar_es_Salaam'; FCount: 4; FFirstPeriod: @CZone_17_Arr),
    (FName: 'Africa/Djibouti'; FCount: 2; FFirstPeriod: @CZone_18_Arr),
    (FName: 'Africa/Douala'; FCount: 2; FFirstPeriod: @CZone_19_Arr),
    (FName: 'Africa/El_Aaiun'; FCount: 4; FFirstPeriod: @CZone_20_Arr),
    (FName: 'Africa/Freetown'; FCount: 4; FFirstPeriod: @CZone_21_Arr),
    (FName: 'Africa/Gaborone'; FCount: 5; FFirstPeriod: @CZone_22_Arr),
    (FName: 'Africa/Harare'; FCount: 2; FFirstPeriod: @CZone_23_Arr),
    (FName: 'Africa/Johannesburg'; FCount: 3; FFirstPeriod: @CZone_24_Arr),
    (FName: 'Africa/Juba'; FCount: 3; FFirstPeriod: @CZone_25_Arr),
    (FName: 'Africa/Kampala'; FCount: 5; FFirstPeriod: @CZone_26_Arr),
    (FName: 'Africa/Khartoum'; FCount: 4; FFirstPeriod: @CZone_27_Arr),
    (FName: 'Africa/Kigali'; FCount: 2; FFirstPeriod: @CZone_28_Arr),
    (FName: 'Africa/Kinshasa'; FCount: 2; FFirstPeriod: @CZone_29_Arr),
    (FName: 'Africa/Lagos'; FCount: 2; FFirstPeriod: @CZone_30_Arr),
    (FName: 'Africa/Libreville'; FCount: 2; FFirstPeriod: @CZone_31_Arr),
    (FName: 'Africa/Lome'; FCount: 2; FFirstPeriod: @CZone_32_Arr),
    (FName: 'Africa/Luanda'; FCount: 3; FFirstPeriod: @CZone_33_Arr),
    (FName: 'Africa/Lubumbashi'; FCount: 2; FFirstPeriod: @CZone_34_Arr),
    (FName: 'Africa/Lusaka'; FCount: 2; FFirstPeriod: @CZone_35_Arr),
    (FName: 'Africa/Malabo'; FCount: 3; FFirstPeriod: @CZone_36_Arr),
    (FName: 'Africa/Maputo'; FCount: 2; FFirstPeriod: @CZone_37_Arr),
    (FName: 'Africa/Maseru'; FCount: 4; FFirstPeriod: @CZone_38_Arr),
    (FName: 'Africa/Mbabane'; FCount: 2; FFirstPeriod: @CZone_39_Arr),
    (FName: 'Africa/Mogadishu'; FCount: 4; FFirstPeriod: @CZone_40_Arr),
    (FName: 'Africa/Monrovia'; FCount: 4; FFirstPeriod: @CZone_41_Arr),
    (FName: 'Africa/Nairobi'; FCount: 5; FFirstPeriod: @CZone_42_Arr),
    (FName: 'Africa/Ndjamena'; FCount: 4; FFirstPeriod: @CZone_43_Arr),
    (FName: 'Africa/Niamey'; FCount: 4; FFirstPeriod: @CZone_44_Arr),
    (FName: 'Africa/Nouakchott'; FCount: 4; FFirstPeriod: @CZone_45_Arr),
    (FName: 'Africa/Ouagadougou'; FCount: 2; FFirstPeriod: @CZone_46_Arr),
    (FName: 'Africa/Porto-Novo'; FCount: 3; FFirstPeriod: @CZone_47_Arr),
    (FName: 'Africa/Sao_Tome'; FCount: 5; FFirstPeriod: @CZone_48_Arr),
    (FName: 'Africa/Timbuktu'; FCount: 2; FFirstPeriod: @CZone_49_Arr),
    (FName: 'Africa/Tripoli'; FCount: 9; FFirstPeriod: @CZone_50_Arr),
    (FName: 'Africa/Tunis'; FCount: 3; FFirstPeriod: @CZone_51_Arr),
    (FName: 'Africa/Windhoek'; FCount: 6; FFirstPeriod: @CZone_52_Arr),
    (FName: 'America/Adak'; FCount: 9; FFirstPeriod: @CZone_53_Arr),
    (FName: 'America/Anchorage'; FCount: 8; FFirstPeriod: @CZone_54_Arr),
    (FName: 'America/Anguilla'; FCount: 2; FFirstPeriod: @CZone_55_Arr),
    (FName: 'America/Antigua'; FCount: 3; FFirstPeriod: @CZone_56_Arr),
    (FName: 'America/Araguaina'; FCount: 7; FFirstPeriod: @CZone_57_Arr),
    (FName: 'America/Argentina/Buenos_Aires'; FCount: 7; FFirstPeriod: @CZone_58_Arr),
    (FName: 'America/Argentina/Catamarca'; FCount: 12; FFirstPeriod: @CZone_59_Arr),
    (FName: 'America/Argentina/ComodRivadavia'; FCount: 11; FFirstPeriod: @CZone_60_Arr),
    (FName: 'America/Argentina/Cordoba'; FCount: 9; FFirstPeriod: @CZone_61_Arr),
    (FName: 'America/Argentina/Jujuy'; FCount: 13; FFirstPeriod: @CZone_62_Arr),
    (FName: 'America/Argentina/La_Rioja'; FCount: 12; FFirstPeriod: @CZone_63_Arr),
    (FName: 'America/Argentina/Mendoza'; FCount: 16; FFirstPeriod: @CZone_64_Arr),
    (FName: 'America/Argentina/Rio_Gallegos'; FCount: 10; FFirstPeriod: @CZone_65_Arr),
    (FName: 'America/Argentina/Salta'; FCount: 10; FFirstPeriod: @CZone_66_Arr),
    (FName: 'America/Argentina/San_Juan'; FCount: 12; FFirstPeriod: @CZone_67_Arr),
    (FName: 'America/Argentina/San_Luis'; FCount: 16; FFirstPeriod: @CZone_68_Arr),
    (FName: 'America/Argentina/Tucuman'; FCount: 11; FFirstPeriod: @CZone_69_Arr),
    (FName: 'America/Argentina/Ushuaia'; FCount: 10; FFirstPeriod: @CZone_70_Arr),
    (FName: 'America/Aruba'; FCount: 3; FFirstPeriod: @CZone_71_Arr),
    (FName: 'America/Asuncion'; FCount: 5; FFirstPeriod: @CZone_72_Arr),
    (FName: 'America/Atikokan'; FCount: 5; FFirstPeriod: @CZone_73_Arr),
    (FName: 'America/Bahia'; FCount: 5; FFirstPeriod: @CZone_74_Arr),
    (FName: 'America/Bahia_Banderas'; FCount: 11; FFirstPeriod: @CZone_75_Arr),
    (FName: 'America/Barbados'; FCount: 3; FFirstPeriod: @CZone_76_Arr),
    (FName: 'America/Belem'; FCount: 3; FFirstPeriod: @CZone_77_Arr),
    (FName: 'America/Belize'; FCount: 2; FFirstPeriod: @CZone_78_Arr),
    (FName: 'America/Blanc-Sablon'; FCount: 3; FFirstPeriod: @CZone_79_Arr),
    (FName: 'America/Boa_Vista'; FCount: 5; FFirstPeriod: @CZone_80_Arr),
    (FName: 'America/Bogota'; FCount: 3; FFirstPeriod: @CZone_81_Arr),
    (FName: 'America/Boise'; FCount: 5; FFirstPeriod: @CZone_82_Arr),
    (FName: 'America/Cambridge_Bay'; FCount: 6; FFirstPeriod: @CZone_83_Arr),
    (FName: 'America/Campo_Grande'; FCount: 2; FFirstPeriod: @CZone_84_Arr),
    (FName: 'America/Cancun'; FCount: 5; FFirstPeriod: @CZone_85_Arr),
    (FName: 'America/Caracas'; FCount: 6; FFirstPeriod: @CZone_86_Arr),
    (FName: 'America/Cayenne'; FCount: 3; FFirstPeriod: @CZone_87_Arr),
    (FName: 'America/Cayman'; FCount: 3; FFirstPeriod: @CZone_88_Arr),
    (FName: 'America/Chicago'; FCount: 8; FFirstPeriod: @CZone_89_Arr),
    (FName: 'America/Chihuahua'; FCount: 10; FFirstPeriod: @CZone_90_Arr),
    (FName: 'America/Coral_Harbour'; FCount: 3; FFirstPeriod: @CZone_91_Arr),
    (FName: 'America/Costa_Rica'; FCount: 3; FFirstPeriod: @CZone_92_Arr),
    (FName: 'America/Creston'; FCount: 4; FFirstPeriod: @CZone_93_Arr),
    (FName: 'America/Cuiaba'; FCount: 4; FFirstPeriod: @CZone_94_Arr),
    (FName: 'America/Curacao'; FCount: 3; FFirstPeriod: @CZone_95_Arr),
    (FName: 'America/Danmarkshavn'; FCount: 4; FFirstPeriod: @CZone_96_Arr),
    (FName: 'America/Dawson'; FCount: 4; FFirstPeriod: @CZone_97_Arr),
    (FName: 'America/Dawson_Creek'; FCount: 4; FFirstPeriod: @CZone_98_Arr),
    (FName: 'America/Denver'; FCount: 6; FFirstPeriod: @CZone_99_Arr),
    (FName: 'America/Detroit'; FCount: 10; FFirstPeriod: @CZone_100_Arr),
    (FName: 'America/Dominica'; FCount: 2; FFirstPeriod: @CZone_101_Arr),
    (FName: 'America/Edmonton'; FCount: 3; FFirstPeriod: @CZone_102_Arr),
    (FName: 'America/Eirunepe'; FCount: 7; FFirstPeriod: @CZone_103_Arr),
    (FName: 'America/El_Salvador'; FCount: 2; FFirstPeriod: @CZone_104_Arr),
    (FName: 'America/Ensenada'; FCount: 7; FFirstPeriod: @CZone_105_Arr),
    (FName: 'America/Fortaleza'; FCount: 7; FFirstPeriod: @CZone_106_Arr),
    (FName: 'America/Fort_Nelson'; FCount: 6; FFirstPeriod: @CZone_107_Arr),
    (FName: 'America/Glace_Bay'; FCount: 6; FFirstPeriod: @CZone_108_Arr),
    (FName: 'America/Godthab'; FCount: 3; FFirstPeriod: @CZone_109_Arr),
    (FName: 'America/Goose_Bay'; FCount: 10; FFirstPeriod: @CZone_110_Arr),
    (FName: 'America/Grand_Turk'; FCount: 6; FFirstPeriod: @CZone_111_Arr),
    (FName: 'America/Grenada'; FCount: 2; FFirstPeriod: @CZone_112_Arr),
    (FName: 'America/Guadeloupe'; FCount: 2; FFirstPeriod: @CZone_113_Arr),
    (FName: 'America/Guatemala'; FCount: 2; FFirstPeriod: @CZone_114_Arr),
    (FName: 'America/Guayaquil'; FCount: 3; FFirstPeriod: @CZone_115_Arr),
    (FName: 'America/Guyana'; FCount: 4; FFirstPeriod: @CZone_116_Arr),
    (FName: 'America/Halifax'; FCount: 7; FFirstPeriod: @CZone_117_Arr),
    (FName: 'America/Havana'; FCount: 3; FFirstPeriod: @CZone_118_Arr),
    (FName: 'America/Hermosillo'; FCount: 11; FFirstPeriod: @CZone_119_Arr),
    (FName: 'America/Indiana/Indianapolis'; FCount: 11; FFirstPeriod: @CZone_120_Arr),
    (FName: 'America/Indiana/Knox'; FCount: 7; FFirstPeriod: @CZone_121_Arr),
    (FName: 'America/Indiana/Marengo'; FCount: 9; FFirstPeriod: @CZone_122_Arr),
    (FName: 'America/Indiana/Petersburg'; FCount: 8; FFirstPeriod: @CZone_123_Arr),
    (FName: 'America/Indiana/Tell_City'; FCount: 8; FFirstPeriod: @CZone_124_Arr),
    (FName: 'America/Indiana/Vevay'; FCount: 6; FFirstPeriod: @CZone_125_Arr),
    (FName: 'America/Indiana/Vincennes'; FCount: 8; FFirstPeriod: @CZone_126_Arr),
    (FName: 'America/Indiana/Winamac'; FCount: 8; FFirstPeriod: @CZone_127_Arr),
    (FName: 'America/Inuvik'; FCount: 4; FFirstPeriod: @CZone_128_Arr),
    (FName: 'America/Iqaluit'; FCount: 4; FFirstPeriod: @CZone_129_Arr),
    (FName: 'America/Jamaica'; FCount: 5; FFirstPeriod: @CZone_130_Arr),
    (FName: 'America/Juneau'; FCount: 10; FFirstPeriod: @CZone_131_Arr),
    (FName: 'America/Kentucky/Louisville'; FCount: 9; FFirstPeriod: @CZone_132_Arr),
    (FName: 'America/Kentucky/Monticello'; FCount: 5; FFirstPeriod: @CZone_133_Arr),
    (FName: 'America/La_Paz'; FCount: 4; FFirstPeriod: @CZone_134_Arr),
    (FName: 'America/Lima'; FCount: 3; FFirstPeriod: @CZone_135_Arr),
    (FName: 'America/Los_Angeles'; FCount: 4; FFirstPeriod: @CZone_136_Arr),
    (FName: 'America/Maceio'; FCount: 9; FFirstPeriod: @CZone_137_Arr),
    (FName: 'America/Managua'; FCount: 9; FFirstPeriod: @CZone_138_Arr),
    (FName: 'America/Manaus'; FCount: 5; FFirstPeriod: @CZone_139_Arr),
    (FName: 'America/Martinique'; FCount: 5; FFirstPeriod: @CZone_140_Arr),
    (FName: 'America/Matamoros'; FCount: 5; FFirstPeriod: @CZone_141_Arr),
    (FName: 'America/Mazatlan'; FCount: 10; FFirstPeriod: @CZone_142_Arr),
    (FName: 'America/Menominee'; FCount: 5; FFirstPeriod: @CZone_143_Arr),
    (FName: 'America/Merida'; FCount: 4; FFirstPeriod: @CZone_144_Arr),
    (FName: 'America/Metlakatla'; FCount: 10; FFirstPeriod: @CZone_145_Arr),
    (FName: 'America/Mexico_City'; FCount: 9; FFirstPeriod: @CZone_146_Arr),
    (FName: 'America/Miquelon'; FCount: 4; FFirstPeriod: @CZone_147_Arr),
    (FName: 'America/Moncton'; FCount: 9; FFirstPeriod: @CZone_148_Arr),
    (FName: 'America/Monterrey'; FCount: 4; FFirstPeriod: @CZone_149_Arr),
    (FName: 'America/Montevideo'; FCount: 11; FFirstPeriod: @CZone_150_Arr),
    (FName: 'America/Montreal'; FCount: 7; FFirstPeriod: @CZone_151_Arr),
    (FName: 'America/Montserrat'; FCount: 2; FFirstPeriod: @CZone_152_Arr),
    (FName: 'America/Nassau'; FCount: 3; FFirstPeriod: @CZone_153_Arr),
    (FName: 'America/New_York'; FCount: 6; FFirstPeriod: @CZone_154_Arr),
    (FName: 'America/Nipigon'; FCount: 4; FFirstPeriod: @CZone_155_Arr),
    (FName: 'America/Nome'; FCount: 9; FFirstPeriod: @CZone_156_Arr),
    (FName: 'America/Noronha'; FCount: 7; FFirstPeriod: @CZone_157_Arr),
    (FName: 'America/North_Dakota/Beulah'; FCount: 3; FFirstPeriod: @CZone_158_Arr),
    (FName: 'America/North_Dakota/Center'; FCount: 3; FFirstPeriod: @CZone_159_Arr),
    (FName: 'America/North_Dakota/New_Salem'; FCount: 3; FFirstPeriod: @CZone_160_Arr),
    (FName: 'America/Ojinaga'; FCount: 11; FFirstPeriod: @CZone_161_Arr),
    (FName: 'America/Panama'; FCount: 3; FFirstPeriod: @CZone_162_Arr),
    (FName: 'America/Pangnirtung'; FCount: 5; FFirstPeriod: @CZone_163_Arr),
    (FName: 'America/Paramaribo'; FCount: 5; FFirstPeriod: @CZone_164_Arr),
    (FName: 'America/Phoenix'; FCount: 7; FFirstPeriod: @CZone_165_Arr),
    (FName: 'America/Port-au-Prince'; FCount: 3; FFirstPeriod: @CZone_166_Arr),
    (FName: 'America/Porto_Velho'; FCount: 3; FFirstPeriod: @CZone_167_Arr),
    (FName: 'America/Port_of_Spain'; FCount: 2; FFirstPeriod: @CZone_168_Arr),
    (FName: 'America/Puerto_Rico'; FCount: 4; FFirstPeriod: @CZone_169_Arr),
    (FName: 'America/Punta_Arenas'; FCount: 13; FFirstPeriod: @CZone_170_Arr),
    (FName: 'America/Rainy_River'; FCount: 4; FFirstPeriod: @CZone_171_Arr),
    (FName: 'America/Rankin_Inlet'; FCount: 4; FFirstPeriod: @CZone_172_Arr),
    (FName: 'America/Recife'; FCount: 7; FFirstPeriod: @CZone_173_Arr),
    (FName: 'America/Regina'; FCount: 3; FFirstPeriod: @CZone_174_Arr),
    (FName: 'America/Resolute'; FCount: 6; FFirstPeriod: @CZone_175_Arr),
    (FName: 'America/Rio_Branco'; FCount: 5; FFirstPeriod: @CZone_176_Arr),
    (FName: 'America/Rosario'; FCount: 8; FFirstPeriod: @CZone_177_Arr),
    (FName: 'America/Santarem'; FCount: 4; FFirstPeriod: @CZone_178_Arr),
    (FName: 'America/Santiago'; FCount: 14; FFirstPeriod: @CZone_179_Arr),
    (FName: 'America/Santo_Domingo'; FCount: 6; FFirstPeriod: @CZone_180_Arr),
    (FName: 'America/Sao_Paulo'; FCount: 4; FFirstPeriod: @CZone_181_Arr),
    (FName: 'America/Scoresbysund'; FCount: 4; FFirstPeriod: @CZone_182_Arr),
    (FName: 'America/Sitka'; FCount: 8; FFirstPeriod: @CZone_183_Arr),
    (FName: 'America/St_Johns'; FCount: 8; FFirstPeriod: @CZone_184_Arr),
    (FName: 'America/St_Kitts'; FCount: 2; FFirstPeriod: @CZone_185_Arr),
    (FName: 'America/St_Lucia'; FCount: 3; FFirstPeriod: @CZone_186_Arr),
    (FName: 'America/St_Thomas'; FCount: 2; FFirstPeriod: @CZone_187_Arr),
    (FName: 'America/St_Vincent'; FCount: 3; FFirstPeriod: @CZone_188_Arr),
    (FName: 'America/Swift_Current'; FCount: 5; FFirstPeriod: @CZone_189_Arr),
    (FName: 'America/Tegucigalpa'; FCount: 2; FFirstPeriod: @CZone_190_Arr),
    (FName: 'America/Thule'; FCount: 2; FFirstPeriod: @CZone_191_Arr),
    (FName: 'America/Thunder_Bay'; FCount: 7; FFirstPeriod: @CZone_192_Arr),
    (FName: 'America/Tijuana'; FCount: 19; FFirstPeriod: @CZone_193_Arr),
    (FName: 'America/Toronto'; FCount: 6; FFirstPeriod: @CZone_194_Arr),
    (FName: 'America/Tortola'; FCount: 2; FFirstPeriod: @CZone_195_Arr),
    (FName: 'America/Vancouver'; FCount: 3; FFirstPeriod: @CZone_196_Arr),
    (FName: 'America/Whitehorse'; FCount: 4; FFirstPeriod: @CZone_197_Arr),
    (FName: 'America/Winnipeg'; FCount: 3; FFirstPeriod: @CZone_198_Arr),
    (FName: 'America/Yakutat'; FCount: 7; FFirstPeriod: @CZone_199_Arr),
    (FName: 'America/Yellowknife'; FCount: 3; FFirstPeriod: @CZone_200_Arr),
    (FName: 'Antarctica/Casey'; FCount: 8; FFirstPeriod: @CZone_201_Arr),
    (FName: 'Antarctica/Davis'; FCount: 8; FFirstPeriod: @CZone_202_Arr),
    (FName: 'Antarctica/DumontDUrville'; FCount: 4; FFirstPeriod: @CZone_203_Arr),
    (FName: 'Antarctica/Macquarie'; FCount: 8; FFirstPeriod: @CZone_204_Arr),
    (FName: 'Antarctica/Mawson'; FCount: 3; FFirstPeriod: @CZone_205_Arr),
    (FName: 'Antarctica/McMurdo'; FCount: 2; FFirstPeriod: @CZone_206_Arr),
    (FName: 'Antarctica/Palmer'; FCount: 5; FFirstPeriod: @CZone_207_Arr),
    (FName: 'Antarctica/Rothera'; FCount: 2; FFirstPeriod: @CZone_208_Arr),
    (FName: 'Antarctica/Syowa'; FCount: 2; FFirstPeriod: @CZone_209_Arr),
    (FName: 'Antarctica/Troll'; FCount: 2; FFirstPeriod: @CZone_210_Arr),
    (FName: 'Antarctica/Vostok'; FCount: 2; FFirstPeriod: @CZone_211_Arr),
    (FName: 'Asia/Aden'; FCount: 2; FFirstPeriod: @CZone_212_Arr),
    (FName: 'Asia/Almaty'; FCount: 6; FFirstPeriod: @CZone_213_Arr),
    (FName: 'Asia/Amman'; FCount: 2; FFirstPeriod: @CZone_214_Arr),
    (FName: 'Asia/Anadyr'; FCount: 8; FFirstPeriod: @CZone_215_Arr),
    (FName: 'Asia/Aqtau'; FCount: 9; FFirstPeriod: @CZone_216_Arr),
    (FName: 'Asia/Aqtobe'; FCount: 9; FFirstPeriod: @CZone_217_Arr),
    (FName: 'Asia/Ashgabat'; FCount: 5; FFirstPeriod: @CZone_218_Arr),
    (FName: 'Asia/Atyrau'; FCount: 9; FFirstPeriod: @CZone_219_Arr),
    (FName: 'Asia/Baghdad'; FCount: 4; FFirstPeriod: @CZone_220_Arr),
    (FName: 'Asia/Bahrain'; FCount: 3; FFirstPeriod: @CZone_221_Arr),
    (FName: 'Asia/Baku'; FCount: 7; FFirstPeriod: @CZone_222_Arr),
    (FName: 'Asia/Bangkok'; FCount: 3; FFirstPeriod: @CZone_223_Arr),
    (FName: 'Asia/Barnaul'; FCount: 9; FFirstPeriod: @CZone_224_Arr),
    (FName: 'Asia/Beirut'; FCount: 2; FFirstPeriod: @CZone_225_Arr),
    (FName: 'Asia/Bishkek'; FCount: 6; FFirstPeriod: @CZone_226_Arr),
    (FName: 'Asia/Brunei'; FCount: 3; FFirstPeriod: @CZone_227_Arr),
    (FName: 'Asia/Chita'; FCount: 8; FFirstPeriod: @CZone_228_Arr),
    (FName: 'Asia/Choibalsan'; FCount: 5; FFirstPeriod: @CZone_229_Arr),
    (FName: 'Asia/Chongqing'; FCount: 3; FFirstPeriod: @CZone_230_Arr),
    (FName: 'Asia/Colombo'; FCount: 9; FFirstPeriod: @CZone_231_Arr),
    (FName: 'Asia/Damascus'; FCount: 2; FFirstPeriod: @CZone_232_Arr),
    (FName: 'Asia/Dhaka'; FCount: 7; FFirstPeriod: @CZone_233_Arr),
    (FName: 'Asia/Dili'; FCount: 5; FFirstPeriod: @CZone_234_Arr),
    (FName: 'Asia/Dubai'; FCount: 2; FFirstPeriod: @CZone_235_Arr),
    (FName: 'Asia/Dushanbe'; FCount: 5; FFirstPeriod: @CZone_236_Arr),
    (FName: 'Asia/Famagusta'; FCount: 5; FFirstPeriod: @CZone_237_Arr),
    (FName: 'Asia/Gaza'; FCount: 12; FFirstPeriod: @CZone_238_Arr),
    (FName: 'Asia/Hanoi'; FCount: 8; FFirstPeriod: @CZone_239_Arr),
    (FName: 'Asia/Harbin'; FCount: 6; FFirstPeriod: @CZone_240_Arr),
    (FName: 'Asia/Hebron'; FCount: 6; FFirstPeriod: @CZone_241_Arr),
    (FName: 'Asia/Hong_Kong'; FCount: 6; FFirstPeriod: @CZone_242_Arr),
    (FName: 'Asia/Hovd'; FCount: 3; FFirstPeriod: @CZone_243_Arr),
    (FName: 'Asia/Ho_Chi_Minh'; FCount: 10; FFirstPeriod: @CZone_244_Arr),
    (FName: 'Asia/Irkutsk'; FCount: 8; FFirstPeriod: @CZone_245_Arr),
    (FName: 'Asia/Jakarta'; FCount: 9; FFirstPeriod: @CZone_246_Arr),
    (FName: 'Asia/Jayapura'; FCount: 4; FFirstPeriod: @CZone_247_Arr),
    (FName: 'Asia/Jerusalem'; FCount: 3; FFirstPeriod: @CZone_248_Arr),
    (FName: 'Asia/Kabul'; FCount: 3; FFirstPeriod: @CZone_249_Arr),
    (FName: 'Asia/Kamchatka'; FCount: 7; FFirstPeriod: @CZone_250_Arr),
    (FName: 'Asia/Karachi'; FCount: 6; FFirstPeriod: @CZone_251_Arr),
    (FName: 'Asia/Kashgar'; FCount: 4; FFirstPeriod: @CZone_252_Arr),
    (FName: 'Asia/Kathmandu'; FCount: 3; FFirstPeriod: @CZone_253_Arr),
    (FName: 'Asia/Khandyga'; FCount: 9; FFirstPeriod: @CZone_254_Arr),
    (FName: 'Asia/Kolkata'; FCount: 8; FFirstPeriod: @CZone_255_Arr),
    (FName: 'Asia/Krasnoyarsk'; FCount: 7; FFirstPeriod: @CZone_256_Arr),
    (FName: 'Asia/Kuala_Lumpur'; FCount: 9; FFirstPeriod: @CZone_257_Arr),
    (FName: 'Asia/Kuching'; FCount: 5; FFirstPeriod: @CZone_258_Arr),
    (FName: 'Asia/Kuwait'; FCount: 2; FFirstPeriod: @CZone_259_Arr),
    (FName: 'Asia/Macau'; FCount: 4; FFirstPeriod: @CZone_260_Arr),
    (FName: 'Asia/Magadan'; FCount: 8; FFirstPeriod: @CZone_261_Arr),
    (FName: 'Asia/Makassar'; FCount: 5; FFirstPeriod: @CZone_262_Arr),
    (FName: 'Asia/Manila'; FCount: 5; FFirstPeriod: @CZone_263_Arr),
    (FName: 'Asia/Muscat'; FCount: 2; FFirstPeriod: @CZone_264_Arr),
    (FName: 'Asia/Nicosia'; FCount: 3; FFirstPeriod: @CZone_265_Arr),
    (FName: 'Asia/Novokuznetsk'; FCount: 7; FFirstPeriod: @CZone_266_Arr),
    (FName: 'Asia/Novosibirsk'; FCount: 9; FFirstPeriod: @CZone_267_Arr),
    (FName: 'Asia/Omsk'; FCount: 7; FFirstPeriod: @CZone_268_Arr),
    (FName: 'Asia/Oral'; FCount: 10; FFirstPeriod: @CZone_269_Arr),
    (FName: 'Asia/Phnom_Penh'; FCount: 6; FFirstPeriod: @CZone_270_Arr),
    (FName: 'Asia/Pontianak'; FCount: 9; FFirstPeriod: @CZone_271_Arr),
    (FName: 'Asia/Pyongyang'; FCount: 6; FFirstPeriod: @CZone_272_Arr),
    (FName: 'Asia/Qatar'; FCount: 3; FFirstPeriod: @CZone_273_Arr),
    (FName: 'Asia/Qostanay'; FCount: 9; FFirstPeriod: @CZone_274_Arr),
    (FName: 'Asia/Qyzylorda'; FCount: 12; FFirstPeriod: @CZone_275_Arr),
    (FName: 'Asia/Riyadh'; FCount: 2; FFirstPeriod: @CZone_276_Arr),
    (FName: 'Asia/Sakhalin'; FCount: 9; FFirstPeriod: @CZone_277_Arr),
    (FName: 'Asia/Samarkand'; FCount: 7; FFirstPeriod: @CZone_278_Arr),
    (FName: 'Asia/Seoul'; FCount: 6; FFirstPeriod: @CZone_279_Arr),
    (FName: 'Asia/Shanghai'; FCount: 3; FFirstPeriod: @CZone_280_Arr),
    (FName: 'Asia/Singapore'; FCount: 9; FFirstPeriod: @CZone_281_Arr),
    (FName: 'Asia/Srednekolymsk'; FCount: 7; FFirstPeriod: @CZone_282_Arr),
    (FName: 'Asia/Taipei'; FCount: 4; FFirstPeriod: @CZone_283_Arr),
    (FName: 'Asia/Tashkent'; FCount: 5; FFirstPeriod: @CZone_284_Arr),
    (FName: 'Asia/Tbilisi'; FCount: 11; FFirstPeriod: @CZone_285_Arr),
    (FName: 'Asia/Tehran'; FCount: 5; FFirstPeriod: @CZone_286_Arr),
    (FName: 'Asia/Tel_Aviv'; FCount: 3; FFirstPeriod: @CZone_287_Arr),
    (FName: 'Asia/Thimphu'; FCount: 3; FFirstPeriod: @CZone_288_Arr),
    (FName: 'Asia/Tokyo'; FCount: 2; FFirstPeriod: @CZone_289_Arr),
    (FName: 'Asia/Tomsk'; FCount: 9; FFirstPeriod: @CZone_290_Arr),
    (FName: 'Asia/Ulaanbaatar'; FCount: 3; FFirstPeriod: @CZone_291_Arr),
    (FName: 'Asia/Urumqi'; FCount: 2; FFirstPeriod: @CZone_292_Arr),
    (FName: 'Asia/Ust-Nera'; FCount: 9; FFirstPeriod: @CZone_293_Arr),
    (FName: 'Asia/Vientiane'; FCount: 8; FFirstPeriod: @CZone_294_Arr),
    (FName: 'Asia/Vladivostok'; FCount: 7; FFirstPeriod: @CZone_295_Arr),
    (FName: 'Asia/Yakutsk'; FCount: 7; FFirstPeriod: @CZone_296_Arr),
    (FName: 'Asia/Yangon'; FCount: 5; FFirstPeriod: @CZone_297_Arr),
    (FName: 'Asia/Yekaterinburg'; FCount: 8; FFirstPeriod: @CZone_298_Arr),
    (FName: 'Asia/Yerevan'; FCount: 7; FFirstPeriod: @CZone_299_Arr),
    (FName: 'Atlantic/Azores'; FCount: 15; FFirstPeriod: @CZone_300_Arr),
    (FName: 'Atlantic/Bermuda'; FCount: 4; FFirstPeriod: @CZone_301_Arr),
    (FName: 'Atlantic/Canary'; FCount: 5; FFirstPeriod: @CZone_302_Arr),
    (FName: 'Atlantic/Cape_Verde'; FCount: 5; FFirstPeriod: @CZone_303_Arr),
    (FName: 'Atlantic/Faroe'; FCount: 3; FFirstPeriod: @CZone_304_Arr),
    (FName: 'Atlantic/Jan_Mayen'; FCount: 1; FFirstPeriod: @CZone_305_Arr),
    (FName: 'Atlantic/Madeira'; FCount: 13; FFirstPeriod: @CZone_306_Arr),
    (FName: 'Atlantic/Reykjavik'; FCount: 3; FFirstPeriod: @CZone_307_Arr),
    (FName: 'Atlantic/South_Georgia'; FCount: 2; FFirstPeriod: @CZone_308_Arr),
    (FName: 'Atlantic/Stanley'; FCount: 6; FFirstPeriod: @CZone_309_Arr),
    (FName: 'Atlantic/St_Helena'; FCount: 3; FFirstPeriod: @CZone_310_Arr),
    (FName: 'Australia/Adelaide'; FCount: 4; FFirstPeriod: @CZone_311_Arr),
    (FName: 'Australia/Brisbane'; FCount: 3; FFirstPeriod: @CZone_312_Arr),
    (FName: 'Australia/Broken_Hill'; FCount: 6; FFirstPeriod: @CZone_313_Arr),
    (FName: 'Australia/Currie'; FCount: 5; FFirstPeriod: @CZone_314_Arr),
    (FName: 'Australia/Darwin'; FCount: 3; FFirstPeriod: @CZone_315_Arr),
    (FName: 'Australia/Eucla'; FCount: 3; FFirstPeriod: @CZone_316_Arr),
    (FName: 'Australia/Hobart'; FCount: 5; FFirstPeriod: @CZone_317_Arr),
    (FName: 'Australia/Lindeman'; FCount: 4; FFirstPeriod: @CZone_318_Arr),
    (FName: 'Australia/Lord_Howe'; FCount: 4; FFirstPeriod: @CZone_319_Arr),
    (FName: 'Australia/Melbourne'; FCount: 3; FFirstPeriod: @CZone_320_Arr),
    (FName: 'Australia/Perth'; FCount: 3; FFirstPeriod: @CZone_321_Arr),
    (FName: 'Australia/Sydney'; FCount: 3; FFirstPeriod: @CZone_322_Arr),
    (FName: 'CET'; FCount: 1; FFirstPeriod: @CZone_323_Arr),
    (FName: 'CST6CDT'; FCount: 1; FFirstPeriod: @CZone_324_Arr),
    (FName: 'EET'; FCount: 1; FFirstPeriod: @CZone_325_Arr),
    (FName: 'EST'; FCount: 1; FFirstPeriod: @CZone_326_Arr),
    (FName: 'EST5EDT'; FCount: 1; FFirstPeriod: @CZone_327_Arr),
    (FName: 'Etc/GMT'; FCount: 1; FFirstPeriod: @CZone_328_Arr),
    (FName: 'Etc/GMT+1'; FCount: 1; FFirstPeriod: @CZone_329_Arr),
    (FName: 'Etc/GMT+10'; FCount: 1; FFirstPeriod: @CZone_330_Arr),
    (FName: 'Etc/GMT+11'; FCount: 1; FFirstPeriod: @CZone_331_Arr),
    (FName: 'Etc/GMT+12'; FCount: 1; FFirstPeriod: @CZone_332_Arr),
    (FName: 'Etc/GMT+2'; FCount: 1; FFirstPeriod: @CZone_333_Arr),
    (FName: 'Etc/GMT+3'; FCount: 1; FFirstPeriod: @CZone_334_Arr),
    (FName: 'Etc/GMT+4'; FCount: 1; FFirstPeriod: @CZone_335_Arr),
    (FName: 'Etc/GMT+5'; FCount: 1; FFirstPeriod: @CZone_336_Arr),
    (FName: 'Etc/GMT+6'; FCount: 1; FFirstPeriod: @CZone_337_Arr),
    (FName: 'Etc/GMT+7'; FCount: 1; FFirstPeriod: @CZone_338_Arr),
    (FName: 'Etc/GMT+8'; FCount: 1; FFirstPeriod: @CZone_339_Arr),
    (FName: 'Etc/GMT+9'; FCount: 1; FFirstPeriod: @CZone_340_Arr),
    (FName: 'Etc/GMT-1'; FCount: 1; FFirstPeriod: @CZone_341_Arr),
    (FName: 'Etc/GMT-10'; FCount: 1; FFirstPeriod: @CZone_342_Arr),
    (FName: 'Etc/GMT-11'; FCount: 1; FFirstPeriod: @CZone_343_Arr),
    (FName: 'Etc/GMT-12'; FCount: 1; FFirstPeriod: @CZone_344_Arr),
    (FName: 'Etc/GMT-13'; FCount: 1; FFirstPeriod: @CZone_345_Arr),
    (FName: 'Etc/GMT-14'; FCount: 1; FFirstPeriod: @CZone_346_Arr),
    (FName: 'Etc/GMT-2'; FCount: 1; FFirstPeriod: @CZone_347_Arr),
    (FName: 'Etc/GMT-3'; FCount: 1; FFirstPeriod: @CZone_348_Arr),
    (FName: 'Etc/GMT-4'; FCount: 1; FFirstPeriod: @CZone_349_Arr),
    (FName: 'Etc/GMT-5'; FCount: 1; FFirstPeriod: @CZone_350_Arr),
    (FName: 'Etc/GMT-6'; FCount: 1; FFirstPeriod: @CZone_351_Arr),
    (FName: 'Etc/GMT-7'; FCount: 1; FFirstPeriod: @CZone_352_Arr),
    (FName: 'Etc/GMT-8'; FCount: 1; FFirstPeriod: @CZone_353_Arr),
    (FName: 'Etc/GMT-9'; FCount: 1; FFirstPeriod: @CZone_354_Arr),
    (FName: 'Etc/UTC'; FCount: 1; FFirstPeriod: @CZone_355_Arr),
    (FName: 'Europe/Amsterdam'; FCount: 6; FFirstPeriod: @CZone_356_Arr),
    (FName: 'Europe/Andorra'; FCount: 4; FFirstPeriod: @CZone_357_Arr),
    (FName: 'Europe/Astrakhan'; FCount: 9; FFirstPeriod: @CZone_358_Arr),
    (FName: 'Europe/Athens'; FCount: 6; FFirstPeriod: @CZone_359_Arr),
    (FName: 'Europe/Belfast'; FCount: 7; FFirstPeriod: @CZone_360_Arr),
    (FName: 'Europe/Belgrade'; FCount: 7; FFirstPeriod: @CZone_361_Arr),
    (FName: 'Europe/Berlin'; FCount: 5; FFirstPeriod: @CZone_362_Arr),
    (FName: 'Europe/Brussels'; FCount: 9; FFirstPeriod: @CZone_363_Arr),
    (FName: 'Europe/Bucharest'; FCount: 7; FFirstPeriod: @CZone_364_Arr),
    (FName: 'Europe/Budapest'; FCount: 6; FFirstPeriod: @CZone_365_Arr),
    (FName: 'Europe/Chisinau'; FCount: 10; FFirstPeriod: @CZone_366_Arr),
    (FName: 'Europe/Copenhagen'; FCount: 6; FFirstPeriod: @CZone_367_Arr),
    (FName: 'Europe/Dublin'; FCount: 11; FFirstPeriod: @CZone_368_Arr),
    (FName: 'Europe/Gibraltar'; FCount: 4; FFirstPeriod: @CZone_369_Arr),
    (FName: 'Europe/Guernsey'; FCount: 7; FFirstPeriod: @CZone_370_Arr),
    (FName: 'Europe/Helsinki'; FCount: 4; FFirstPeriod: @CZone_371_Arr),
    (FName: 'Europe/Isle_of_Man'; FCount: 5; FFirstPeriod: @CZone_372_Arr),
    (FName: 'Europe/Istanbul'; FCount: 13; FFirstPeriod: @CZone_373_Arr),
    (FName: 'Europe/Jersey'; FCount: 7; FFirstPeriod: @CZone_374_Arr),
    (FName: 'Europe/Kaliningrad'; FCount: 7; FFirstPeriod: @CZone_375_Arr),
    (FName: 'Europe/Kiev'; FCount: 9; FFirstPeriod: @CZone_376_Arr),
    (FName: 'Europe/Kirov'; FCount: 8; FFirstPeriod: @CZone_377_Arr),
    (FName: 'Europe/Lisbon'; FCount: 8; FFirstPeriod: @CZone_378_Arr),
    (FName: 'Europe/Ljubljana'; FCount: 6; FFirstPeriod: @CZone_379_Arr),
    (FName: 'Europe/London'; FCount: 5; FFirstPeriod: @CZone_380_Arr),
    (FName: 'Europe/Luxembourg'; FCount: 7; FFirstPeriod: @CZone_381_Arr),
    (FName: 'Europe/Madrid'; FCount: 4; FFirstPeriod: @CZone_382_Arr),
    (FName: 'Europe/Malta'; FCount: 4; FFirstPeriod: @CZone_383_Arr),
    (FName: 'Europe/Minsk'; FCount: 9; FFirstPeriod: @CZone_384_Arr),
    (FName: 'Europe/Monaco'; FCount: 5; FFirstPeriod: @CZone_385_Arr),
    (FName: 'Europe/Moscow'; FCount: 11; FFirstPeriod: @CZone_386_Arr),
    (FName: 'Europe/Oslo'; FCount: 5; FFirstPeriod: @CZone_387_Arr),
    (FName: 'Europe/Paris'; FCount: 7; FFirstPeriod: @CZone_388_Arr),
    (FName: 'Europe/Prague'; FCount: 7; FFirstPeriod: @CZone_389_Arr),
    (FName: 'Europe/Riga'; FCount: 15; FFirstPeriod: @CZone_390_Arr),
    (FName: 'Europe/Rome'; FCount: 6; FFirstPeriod: @CZone_391_Arr),
    (FName: 'Europe/Samara'; FCount: 10; FFirstPeriod: @CZone_392_Arr),
    (FName: 'Europe/Sarajevo'; FCount: 6; FFirstPeriod: @CZone_393_Arr),
    (FName: 'Europe/Saratov'; FCount: 9; FFirstPeriod: @CZone_394_Arr),
    (FName: 'Europe/Simferopol'; FCount: 16; FFirstPeriod: @CZone_395_Arr),
    (FName: 'Europe/Skopje'; FCount: 6; FFirstPeriod: @CZone_396_Arr),
    (FName: 'Europe/Sofia'; FCount: 10; FFirstPeriod: @CZone_397_Arr),
    (FName: 'Europe/Stockholm'; FCount: 6; FFirstPeriod: @CZone_398_Arr),
    (FName: 'Europe/Tallinn'; FCount: 13; FFirstPeriod: @CZone_399_Arr),
    (FName: 'Europe/Tirane'; FCount: 4; FFirstPeriod: @CZone_400_Arr),
    (FName: 'Europe/Tiraspol'; FCount: 9; FFirstPeriod: @CZone_401_Arr),
    (FName: 'Europe/Ulyanovsk'; FCount: 9; FFirstPeriod: @CZone_402_Arr),
    (FName: 'Europe/Uzhgorod'; FCount: 11; FFirstPeriod: @CZone_403_Arr),
    (FName: 'Europe/Vaduz'; FCount: 3; FFirstPeriod: @CZone_404_Arr),
    (FName: 'Europe/Vienna'; FCount: 8; FFirstPeriod: @CZone_405_Arr),
    (FName: 'Europe/Vilnius'; FCount: 15; FFirstPeriod: @CZone_406_Arr),
    (FName: 'Europe/Volgograd'; FCount: 10; FFirstPeriod: @CZone_407_Arr),
    (FName: 'Europe/Warsaw'; FCount: 9; FFirstPeriod: @CZone_408_Arr),
    (FName: 'Europe/Zagreb'; FCount: 6; FFirstPeriod: @CZone_409_Arr),
    (FName: 'Europe/Zaporozhye'; FCount: 8; FFirstPeriod: @CZone_410_Arr),
    (FName: 'Europe/Zurich'; FCount: 4; FFirstPeriod: @CZone_411_Arr),
    (FName: 'Factory'; FCount: 1; FFirstPeriod: @CZone_412_Arr),
    (FName: 'HST'; FCount: 1; FFirstPeriod: @CZone_413_Arr),
    (FName: 'Indian/Antananarivo'; FCount: 4; FFirstPeriod: @CZone_414_Arr),
    (FName: 'Indian/Chagos'; FCount: 3; FFirstPeriod: @CZone_415_Arr),
    (FName: 'Indian/Christmas'; FCount: 2; FFirstPeriod: @CZone_416_Arr),
    (FName: 'Indian/Cocos'; FCount: 2; FFirstPeriod: @CZone_417_Arr),
    (FName: 'Indian/Comoro'; FCount: 2; FFirstPeriod: @CZone_418_Arr),
    (FName: 'Indian/Kerguelen'; FCount: 2; FFirstPeriod: @CZone_419_Arr),
    (FName: 'Indian/Mahe'; FCount: 2; FFirstPeriod: @CZone_420_Arr),
    (FName: 'Indian/Maldives'; FCount: 3; FFirstPeriod: @CZone_421_Arr),
    (FName: 'Indian/Mauritius'; FCount: 2; FFirstPeriod: @CZone_422_Arr),
    (FName: 'Indian/Mayotte'; FCount: 2; FFirstPeriod: @CZone_423_Arr),
    (FName: 'Indian/Reunion'; FCount: 2; FFirstPeriod: @CZone_424_Arr),
    (FName: 'MET'; FCount: 1; FFirstPeriod: @CZone_425_Arr),
    (FName: 'MST'; FCount: 1; FFirstPeriod: @CZone_426_Arr),
    (FName: 'MST7MDT'; FCount: 1; FFirstPeriod: @CZone_427_Arr),
    (FName: 'Pacific/Apia'; FCount: 5; FFirstPeriod: @CZone_428_Arr),
    (FName: 'Pacific/Auckland'; FCount: 3; FFirstPeriod: @CZone_429_Arr),
    (FName: 'Pacific/Bougainville'; FCount: 6; FFirstPeriod: @CZone_430_Arr),
    (FName: 'Pacific/Chatham'; FCount: 3; FFirstPeriod: @CZone_431_Arr),
    (FName: 'Pacific/Chuuk'; FCount: 7; FFirstPeriod: @CZone_432_Arr),
    (FName: 'Pacific/Easter'; FCount: 4; FFirstPeriod: @CZone_433_Arr),
    (FName: 'Pacific/Efate'; FCount: 2; FFirstPeriod: @CZone_434_Arr),
    (FName: 'Pacific/Enderbury'; FCount: 4; FFirstPeriod: @CZone_435_Arr),
    (FName: 'Pacific/Fakaofo'; FCount: 3; FFirstPeriod: @CZone_436_Arr),
    (FName: 'Pacific/Fiji'; FCount: 2; FFirstPeriod: @CZone_437_Arr),
    (FName: 'Pacific/Funafuti'; FCount: 2; FFirstPeriod: @CZone_438_Arr),
    (FName: 'Pacific/Galapagos'; FCount: 3; FFirstPeriod: @CZone_439_Arr),
    (FName: 'Pacific/Gambier'; FCount: 2; FFirstPeriod: @CZone_440_Arr),
    (FName: 'Pacific/Guadalcanal'; FCount: 2; FFirstPeriod: @CZone_441_Arr),
    (FName: 'Pacific/Guam'; FCount: 6; FFirstPeriod: @CZone_442_Arr),
    (FName: 'Pacific/Honolulu'; FCount: 5; FFirstPeriod: @CZone_443_Arr),
    (FName: 'Pacific/Johnston'; FCount: 1; FFirstPeriod: @CZone_444_Arr),
    (FName: 'Pacific/Kiritimati'; FCount: 4; FFirstPeriod: @CZone_445_Arr),
    (FName: 'Pacific/Kosrae'; FCount: 10; FFirstPeriod: @CZone_446_Arr),
    (FName: 'Pacific/Kwajalein'; FCount: 7; FFirstPeriod: @CZone_447_Arr),
    (FName: 'Pacific/Majuro'; FCount: 8; FFirstPeriod: @CZone_448_Arr),
    (FName: 'Pacific/Marquesas'; FCount: 2; FFirstPeriod: @CZone_449_Arr),
    (FName: 'Pacific/Midway'; FCount: 4; FFirstPeriod: @CZone_450_Arr),
    (FName: 'Pacific/Nauru'; FCount: 5; FFirstPeriod: @CZone_451_Arr),
    (FName: 'Pacific/Niue'; FCount: 4; FFirstPeriod: @CZone_452_Arr),
    (FName: 'Pacific/Norfolk'; FCount: 7; FFirstPeriod: @CZone_453_Arr),
    (FName: 'Pacific/Noumea'; FCount: 2; FFirstPeriod: @CZone_454_Arr),
    (FName: 'Pacific/Pago_Pago'; FCount: 3; FFirstPeriod: @CZone_455_Arr),
    (FName: 'Pacific/Palau'; FCount: 3; FFirstPeriod: @CZone_456_Arr),
    (FName: 'Pacific/Pitcairn'; FCount: 3; FFirstPeriod: @CZone_457_Arr),
    (FName: 'Pacific/Pohnpei'; FCount: 8; FFirstPeriod: @CZone_458_Arr),
    (FName: 'Pacific/Port_Moresby'; FCount: 3; FFirstPeriod: @CZone_459_Arr),
    (FName: 'Pacific/Rarotonga'; FCount: 3; FFirstPeriod: @CZone_460_Arr),
    (FName: 'Pacific/Saipan'; FCount: 5; FFirstPeriod: @CZone_461_Arr),
    (FName: 'Pacific/Tahiti'; FCount: 2; FFirstPeriod: @CZone_462_Arr),
    (FName: 'Pacific/Tarawa'; FCount: 2; FFirstPeriod: @CZone_463_Arr),
    (FName: 'Pacific/Tongatapu'; FCount: 4; FFirstPeriod: @CZone_464_Arr),
    (FName: 'Pacific/Wake'; FCount: 2; FFirstPeriod: @CZone_465_Arr),
    (FName: 'Pacific/Wallis'; FCount: 2; FFirstPeriod: @CZone_466_Arr),
    (FName: 'PST8PDT'; FCount: 1; FFirstPeriod: @CZone_467_Arr),
    (FName: 'WET'; FCount: 1; FFirstPeriod: @CZone_468_Arr)
  );

var
  { This array contains zone aliases. }
  CAliases: array[0 .. 367] of TZoneAlias = (
    (FName: 'AUS Central Standard Time'; FAliasTo: @CZones[315]),
    (FName: 'AUS Eastern Standard Time'; FAliasTo: @CZones[322]),
    (FName: 'Afghanistan Standard Time'; FAliasTo: @CZones[249]),
    (FName: 'Africa/Addis_Ababa'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Asmara'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Asmera'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Bamako'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Bangui'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Banjul'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Blantyre'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Brazzaville'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Bujumbura'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Conakry'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Dakar'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Dar_es_Salaam'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Djibouti'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Douala'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Freetown'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Gaborone'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Harare'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Kampala'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Kigali'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Kinshasa'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Libreville'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Lome'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Luanda'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Lubumbashi'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Lusaka'; FAliasTo: @CZones[37]),
    (FName: 'Africa/Malabo'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Maseru'; FAliasTo: @CZones[24]),
    (FName: 'Africa/Mbabane'; FAliasTo: @CZones[24]),
    (FName: 'Africa/Mogadishu'; FAliasTo: @CZones[42]),
    (FName: 'Africa/Niamey'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Nouakchott'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Ouagadougou'; FAliasTo: @CZones[0]),
    (FName: 'Africa/Porto-Novo'; FAliasTo: @CZones[30]),
    (FName: 'Africa/Timbuktu'; FAliasTo: @CZones[0]),
    (FName: 'Alaskan Standard Time'; FAliasTo: @CZones[54]),
    (FName: 'Aleutian Standard Time'; FAliasTo: @CZones[53]),
    (FName: 'Altai Standard Time'; FAliasTo: @CZones[224]),
    (FName: 'America/Anguilla'; FAliasTo: @CZones[168]),
    (FName: 'America/Antigua'; FAliasTo: @CZones[168]),
    (FName: 'America/Argentina/ComodRivadavia'; FAliasTo: @CZones[59]),
    (FName: 'America/Aruba'; FAliasTo: @CZones[95]),
    (FName: 'America/Atka'; FAliasTo: @CZones[53]),
    (FName: 'America/Buenos_Aires'; FAliasTo: @CZones[58]),
    (FName: 'America/Catamarca'; FAliasTo: @CZones[59]),
    (FName: 'America/Cayman'; FAliasTo: @CZones[162]),
    (FName: 'America/Coral_Harbour'; FAliasTo: @CZones[73]),
    (FName: 'America/Cordoba'; FAliasTo: @CZones[61]),
    (FName: 'America/Dominica'; FAliasTo: @CZones[168]),
    (FName: 'America/Ensenada'; FAliasTo: @CZones[193]),
    (FName: 'America/Fort_Wayne'; FAliasTo: @CZones[120]),
    (FName: 'America/Grenada'; FAliasTo: @CZones[168]),
    (FName: 'America/Guadeloupe'; FAliasTo: @CZones[168]),
    (FName: 'America/Indianapolis'; FAliasTo: @CZones[120]),
    (FName: 'America/Jujuy'; FAliasTo: @CZones[62]),
    (FName: 'America/Knox_IN'; FAliasTo: @CZones[121]),
    (FName: 'America/Kralendijk'; FAliasTo: @CZones[95]),
    (FName: 'America/Louisville'; FAliasTo: @CZones[132]),
    (FName: 'America/Lower_Princes'; FAliasTo: @CZones[95]),
    (FName: 'America/Marigot'; FAliasTo: @CZones[168]),
    (FName: 'America/Mendoza'; FAliasTo: @CZones[64]),
    (FName: 'America/Montreal'; FAliasTo: @CZones[194]),
    (FName: 'America/Montserrat'; FAliasTo: @CZones[168]),
    (FName: 'America/Porto_Acre'; FAliasTo: @CZones[176]),
    (FName: 'America/Rosario'; FAliasTo: @CZones[61]),
    (FName: 'America/Santa_Isabel'; FAliasTo: @CZones[193]),
    (FName: 'America/Shiprock'; FAliasTo: @CZones[99]),
    (FName: 'America/St_Barthelemy'; FAliasTo: @CZones[168]),
    (FName: 'America/St_Kitts'; FAliasTo: @CZones[168]),
    (FName: 'America/St_Lucia'; FAliasTo: @CZones[168]),
    (FName: 'America/St_Thomas'; FAliasTo: @CZones[168]),
    (FName: 'America/St_Vincent'; FAliasTo: @CZones[168]),
    (FName: 'America/Tortola'; FAliasTo: @CZones[168]),
    (FName: 'America/Virgin'; FAliasTo: @CZones[168]),
    (FName: 'Antarctica/McMurdo'; FAliasTo: @CZones[429]),
    (FName: 'Antarctica/South_Pole'; FAliasTo: @CZones[429]),
    (FName: 'Arab Standard Time'; FAliasTo: @CZones[276]),
    (FName: 'Arabian Standard Time'; FAliasTo: @CZones[235]),
    (FName: 'Arabic Standard Time'; FAliasTo: @CZones[220]),
    (FName: 'Arctic/Longyearbyen'; FAliasTo: @CZones[387]),
    (FName: 'Argentina Standard Time'; FAliasTo: @CZones[58]),
    (FName: 'Asia/Aden'; FAliasTo: @CZones[276]),
    (FName: 'Asia/Ashkhabad'; FAliasTo: @CZones[218]),
    (FName: 'Asia/Bahrain'; FAliasTo: @CZones[273]),
    (FName: 'Asia/Calcutta'; FAliasTo: @CZones[255]),
    (FName: 'Asia/Chongqing'; FAliasTo: @CZones[280]),
    (FName: 'Asia/Chungking'; FAliasTo: @CZones[280]),
    (FName: 'Asia/Dacca'; FAliasTo: @CZones[233]),
    (FName: 'Asia/Harbin'; FAliasTo: @CZones[280]),
    (FName: 'Asia/Istanbul'; FAliasTo: @CZones[373]),
    (FName: 'Asia/Kashgar'; FAliasTo: @CZones[292]),
    (FName: 'Asia/Katmandu'; FAliasTo: @CZones[253]),
    (FName: 'Asia/Kuwait'; FAliasTo: @CZones[276]),
    (FName: 'Asia/Macao'; FAliasTo: @CZones[260]),
    (FName: 'Asia/Muscat'; FAliasTo: @CZones[235]),
    (FName: 'Asia/Phnom_Penh'; FAliasTo: @CZones[223]),
    (FName: 'Asia/Rangoon'; FAliasTo: @CZones[297]),
    (FName: 'Asia/Saigon'; FAliasTo: @CZones[244]),
    (FName: 'Asia/Tel_Aviv'; FAliasTo: @CZones[248]),
    (FName: 'Asia/Thimbu'; FAliasTo: @CZones[288]),
    (FName: 'Asia/Ujung_Pandang'; FAliasTo: @CZones[262]),
    (FName: 'Asia/Ulan_Bator'; FAliasTo: @CZones[291]),
    (FName: 'Asia/Vientiane'; FAliasTo: @CZones[223]),
    (FName: 'Astrakhan Standard Time'; FAliasTo: @CZones[358]),
    (FName: 'Atlantic Standard Time'; FAliasTo: @CZones[117]),
    (FName: 'Atlantic/Faeroe'; FAliasTo: @CZones[304]),
    (FName: 'Atlantic/Jan_Mayen'; FAliasTo: @CZones[387]),
    (FName: 'Atlantic/St_Helena'; FAliasTo: @CZones[0]),
    (FName: 'Aus Central W. Standard Time'; FAliasTo: @CZones[316]),
    (FName: 'Australia/ACT'; FAliasTo: @CZones[322]),
    (FName: 'Australia/Canberra'; FAliasTo: @CZones[322]),
    (FName: 'Australia/LHI'; FAliasTo: @CZones[319]),
    (FName: 'Australia/NSW'; FAliasTo: @CZones[322]),
    (FName: 'Australia/North'; FAliasTo: @CZones[315]),
    (FName: 'Australia/Queensland'; FAliasTo: @CZones[312]),
    (FName: 'Australia/South'; FAliasTo: @CZones[311]),
    (FName: 'Australia/Tasmania'; FAliasTo: @CZones[317]),
    (FName: 'Australia/Victoria'; FAliasTo: @CZones[320]),
    (FName: 'Australia/West'; FAliasTo: @CZones[321]),
    (FName: 'Australia/Yancowinna'; FAliasTo: @CZones[313]),
    (FName: 'Azerbaijan Standard Time'; FAliasTo: @CZones[222]),
    (FName: 'Azores Standard Time'; FAliasTo: @CZones[300]),
    (FName: 'Bahia Standard Time'; FAliasTo: @CZones[74]),
    (FName: 'Bangladesh Standard Time'; FAliasTo: @CZones[233]),
    (FName: 'Belarus Standard Time'; FAliasTo: @CZones[384]),
    (FName: 'Bougainville Standard Time'; FAliasTo: @CZones[430]),
    (FName: 'Brazil/Acre'; FAliasTo: @CZones[176]),
    (FName: 'Brazil/DeNoronha'; FAliasTo: @CZones[157]),
    (FName: 'Brazil/East'; FAliasTo: @CZones[181]),
    (FName: 'Brazil/West'; FAliasTo: @CZones[139]),
    (FName: 'Canada Central Standard Time'; FAliasTo: @CZones[174]),
    (FName: 'Canada/Atlantic'; FAliasTo: @CZones[117]),
    (FName: 'Canada/Central'; FAliasTo: @CZones[198]),
    (FName: 'Canada/Eastern'; FAliasTo: @CZones[194]),
    (FName: 'Canada/Mountain'; FAliasTo: @CZones[102]),
    (FName: 'Canada/Newfoundland'; FAliasTo: @CZones[184]),
    (FName: 'Canada/Pacific'; FAliasTo: @CZones[196]),
    (FName: 'Canada/Saskatchewan'; FAliasTo: @CZones[174]),
    (FName: 'Canada/Yukon'; FAliasTo: @CZones[197]),
    (FName: 'Cape Verde Standard Time'; FAliasTo: @CZones[303]),
    (FName: 'Caucasus Standard Time'; FAliasTo: @CZones[299]),
    (FName: 'Cen. Australia Standard Time'; FAliasTo: @CZones[311]),
    (FName: 'Central America Standard Time'; FAliasTo: @CZones[114]),
    (FName: 'Central Asia Standard Time'; FAliasTo: @CZones[213]),
    (FName: 'Central Brazilian Standard Time'; FAliasTo: @CZones[94]),
    (FName: 'Central Europe Standard Time'; FAliasTo: @CZones[365]),
    (FName: 'Central European Standard Time'; FAliasTo: @CZones[408]),
    (FName: 'Central Pacific Standard Time'; FAliasTo: @CZones[441]),
    (FName: 'Central Standard Time'; FAliasTo: @CZones[89]),
    (FName: 'Central Standard Time (Mexico)'; FAliasTo: @CZones[146]),
    (FName: 'Chatham Islands Standard Time'; FAliasTo: @CZones[431]),
    (FName: 'Chile/Continental'; FAliasTo: @CZones[179]),
    (FName: 'Chile/EasterIsland'; FAliasTo: @CZones[433]),
    (FName: 'China Standard Time'; FAliasTo: @CZones[280]),
    (FName: 'Cuba'; FAliasTo: @CZones[118]),
    (FName: 'Cuba Standard Time'; FAliasTo: @CZones[118]),
    (FName: 'Dateline Standard Time'; FAliasTo: @CZones[332]),
    (FName: 'E. Africa Standard Time'; FAliasTo: @CZones[42]),
    (FName: 'E. Australia Standard Time'; FAliasTo: @CZones[312]),
    (FName: 'E. Europe Standard Time'; FAliasTo: @CZones[366]),
    (FName: 'E. South America Standard Time'; FAliasTo: @CZones[181]),
    (FName: 'Easter Island Standard Time'; FAliasTo: @CZones[433]),
    (FName: 'Eastern Standard Time'; FAliasTo: @CZones[154]),
    (FName: 'Eastern Standard Time (Mexico)'; FAliasTo: @CZones[85]),
    (FName: 'Egypt'; FAliasTo: @CZones[12]),
    (FName: 'Egypt Standard Time'; FAliasTo: @CZones[12]),
    (FName: 'Eire'; FAliasTo: @CZones[368]),
    (FName: 'Ekaterinburg Standard Time'; FAliasTo: @CZones[298]),
    (FName: 'Etc/GMT+0'; FAliasTo: @CZones[328]),
    (FName: 'Etc/GMT-0'; FAliasTo: @CZones[328]),
    (FName: 'Etc/GMT0'; FAliasTo: @CZones[328]),
    (FName: 'Etc/Greenwich'; FAliasTo: @CZones[328]),
    (FName: 'Etc/UCT'; FAliasTo: @CZones[355]),
    (FName: 'Etc/Universal'; FAliasTo: @CZones[355]),
    (FName: 'Etc/Zulu'; FAliasTo: @CZones[355]),
    (FName: 'Europe/Belfast'; FAliasTo: @CZones[380]),
    (FName: 'Europe/Bratislava'; FAliasTo: @CZones[389]),
    (FName: 'Europe/Busingen'; FAliasTo: @CZones[411]),
    (FName: 'Europe/Guernsey'; FAliasTo: @CZones[380]),
    (FName: 'Europe/Isle_of_Man'; FAliasTo: @CZones[380]),
    (FName: 'Europe/Jersey'; FAliasTo: @CZones[380]),
    (FName: 'Europe/Ljubljana'; FAliasTo: @CZones[361]),
    (FName: 'Europe/Mariehamn'; FAliasTo: @CZones[371]),
    (FName: 'Europe/Nicosia'; FAliasTo: @CZones[265]),
    (FName: 'Europe/Podgorica'; FAliasTo: @CZones[361]),
    (FName: 'Europe/San_Marino'; FAliasTo: @CZones[391]),
    (FName: 'Europe/Sarajevo'; FAliasTo: @CZones[361]),
    (FName: 'Europe/Skopje'; FAliasTo: @CZones[361]),
    (FName: 'Europe/Tiraspol'; FAliasTo: @CZones[366]),
    (FName: 'Europe/Vaduz'; FAliasTo: @CZones[411]),
    (FName: 'Europe/Vatican'; FAliasTo: @CZones[391]),
    (FName: 'Europe/Zagreb'; FAliasTo: @CZones[361]),
    (FName: 'FLE Standard Time'; FAliasTo: @CZones[376]),
    (FName: 'Fiji Standard Time'; FAliasTo: @CZones[437]),
    (FName: 'GB'; FAliasTo: @CZones[380]),
    (FName: 'GB-Eire'; FAliasTo: @CZones[380]),
    (FName: 'GMT'; FAliasTo: @CZones[328]),
    (FName: 'GMT Standard Time'; FAliasTo: @CZones[380]),
    (FName: 'GMT+0'; FAliasTo: @CZones[328]),
    (FName: 'GMT+1'; FAliasTo: @CZones[329]),
    (FName: 'GMT+10'; FAliasTo: @CZones[330]),
    (FName: 'GMT+11'; FAliasTo: @CZones[331]),
    (FName: 'GMT+12'; FAliasTo: @CZones[332]),
    (FName: 'GMT+2'; FAliasTo: @CZones[333]),
    (FName: 'GMT+3'; FAliasTo: @CZones[334]),
    (FName: 'GMT+4'; FAliasTo: @CZones[335]),
    (FName: 'GMT+5'; FAliasTo: @CZones[336]),
    (FName: 'GMT+6'; FAliasTo: @CZones[337]),
    (FName: 'GMT+7'; FAliasTo: @CZones[338]),
    (FName: 'GMT+8'; FAliasTo: @CZones[339]),
    (FName: 'GMT+9'; FAliasTo: @CZones[340]),
    (FName: 'GMT-0'; FAliasTo: @CZones[328]),
    (FName: 'GMT-1'; FAliasTo: @CZones[341]),
    (FName: 'GMT-10'; FAliasTo: @CZones[342]),
    (FName: 'GMT-11'; FAliasTo: @CZones[343]),
    (FName: 'GMT-12'; FAliasTo: @CZones[344]),
    (FName: 'GMT-13'; FAliasTo: @CZones[345]),
    (FName: 'GMT-14'; FAliasTo: @CZones[346]),
    (FName: 'GMT-2'; FAliasTo: @CZones[347]),
    (FName: 'GMT-3'; FAliasTo: @CZones[348]),
    (FName: 'GMT-4'; FAliasTo: @CZones[349]),
    (FName: 'GMT-5'; FAliasTo: @CZones[350]),
    (FName: 'GMT-6'; FAliasTo: @CZones[351]),
    (FName: 'GMT-7'; FAliasTo: @CZones[352]),
    (FName: 'GMT-8'; FAliasTo: @CZones[353]),
    (FName: 'GMT-9'; FAliasTo: @CZones[354]),
    (FName: 'GMT0'; FAliasTo: @CZones[328]),
    (FName: 'GTB Standard Time'; FAliasTo: @CZones[364]),
    (FName: 'Georgian Standard Time'; FAliasTo: @CZones[285]),
    (FName: 'Greenland Standard Time'; FAliasTo: @CZones[109]),
    (FName: 'Greenwich'; FAliasTo: @CZones[328]),
    (FName: 'Greenwich Standard Time'; FAliasTo: @CZones[307]),
    (FName: 'Haiti Standard Time'; FAliasTo: @CZones[166]),
    (FName: 'Hawaiian Standard Time'; FAliasTo: @CZones[443]),
    (FName: 'Hongkong'; FAliasTo: @CZones[242]),
    (FName: 'Iceland'; FAliasTo: @CZones[307]),
    (FName: 'India Standard Time'; FAliasTo: @CZones[255]),
    (FName: 'Indian/Antananarivo'; FAliasTo: @CZones[42]),
    (FName: 'Indian/Comoro'; FAliasTo: @CZones[42]),
    (FName: 'Indian/Mayotte'; FAliasTo: @CZones[42]),
    (FName: 'Iran'; FAliasTo: @CZones[286]),
    (FName: 'Iran Standard Time'; FAliasTo: @CZones[286]),
    (FName: 'Israel'; FAliasTo: @CZones[248]),
    (FName: 'Israel Standard Time'; FAliasTo: @CZones[248]),
    (FName: 'Jamaica'; FAliasTo: @CZones[130]),
    (FName: 'Japan'; FAliasTo: @CZones[289]),
    (FName: 'Jordan Standard Time'; FAliasTo: @CZones[214]),
    (FName: 'Kaliningrad Standard Time'; FAliasTo: @CZones[375]),
    (FName: 'Korea Standard Time'; FAliasTo: @CZones[279]),
    (FName: 'Kwajalein'; FAliasTo: @CZones[447]),
    (FName: 'Libya'; FAliasTo: @CZones[50]),
    (FName: 'Libya Standard Time'; FAliasTo: @CZones[50]),
    (FName: 'Line Islands Standard Time'; FAliasTo: @CZones[445]),
    (FName: 'Lord Howe Standard Time'; FAliasTo: @CZones[319]),
    (FName: 'Magadan Standard Time'; FAliasTo: @CZones[261]),
    (FName: 'Magallanes Standard Time'; FAliasTo: @CZones[170]),
    (FName: 'Marquesas Standard Time'; FAliasTo: @CZones[449]),
    (FName: 'Mauritius Standard Time'; FAliasTo: @CZones[422]),
    (FName: 'Mexico/BajaNorte'; FAliasTo: @CZones[193]),
    (FName: 'Mexico/BajaSur'; FAliasTo: @CZones[142]),
    (FName: 'Mexico/General'; FAliasTo: @CZones[146]),
    (FName: 'Middle East Standard Time'; FAliasTo: @CZones[225]),
    (FName: 'Montevideo Standard Time'; FAliasTo: @CZones[150]),
    (FName: 'Morocco Standard Time'; FAliasTo: @CZones[13]),
    (FName: 'Mountain Standard Time'; FAliasTo: @CZones[99]),
    (FName: 'Mountain Standard Time (Mexico)'; FAliasTo: @CZones[90]),
    (FName: 'Myanmar Standard Time'; FAliasTo: @CZones[297]),
    (FName: 'N. Central Asia Standard Time'; FAliasTo: @CZones[267]),
    (FName: 'NZ'; FAliasTo: @CZones[429]),
    (FName: 'NZ-CHAT'; FAliasTo: @CZones[431]),
    (FName: 'Namibia Standard Time'; FAliasTo: @CZones[52]),
    (FName: 'Navajo'; FAliasTo: @CZones[99]),
    (FName: 'Nepal Standard Time'; FAliasTo: @CZones[253]),
    (FName: 'New Zealand Standard Time'; FAliasTo: @CZones[429]),
    (FName: 'Newfoundland Standard Time'; FAliasTo: @CZones[184]),
    (FName: 'Norfolk Standard Time'; FAliasTo: @CZones[453]),
    (FName: 'North Asia East Standard Time'; FAliasTo: @CZones[245]),
    (FName: 'North Asia Standard Time'; FAliasTo: @CZones[256]),
    (FName: 'North Korea Standard Time'; FAliasTo: @CZones[272]),
    (FName: 'Omsk Standard Time'; FAliasTo: @CZones[268]),
    (FName: 'PRC'; FAliasTo: @CZones[280]),
    (FName: 'Pacific SA Standard Time'; FAliasTo: @CZones[179]),
    (FName: 'Pacific Standard Time'; FAliasTo: @CZones[136]),
    (FName: 'Pacific Standard Time (Mexico)'; FAliasTo: @CZones[193]),
    (FName: 'Pacific/Johnston'; FAliasTo: @CZones[443]),
    (FName: 'Pacific/Midway'; FAliasTo: @CZones[455]),
    (FName: 'Pacific/Ponape'; FAliasTo: @CZones[458]),
    (FName: 'Pacific/Saipan'; FAliasTo: @CZones[442]),
    (FName: 'Pacific/Samoa'; FAliasTo: @CZones[455]),
    (FName: 'Pacific/Truk'; FAliasTo: @CZones[432]),
    (FName: 'Pacific/Yap'; FAliasTo: @CZones[432]),
    (FName: 'Pakistan Standard Time'; FAliasTo: @CZones[251]),
    (FName: 'Paraguay Standard Time'; FAliasTo: @CZones[72]),
    (FName: 'Poland'; FAliasTo: @CZones[408]),
    (FName: 'Portugal'; FAliasTo: @CZones[378]),
    (FName: 'Qyzylorda Standard Time'; FAliasTo: @CZones[275]),
    (FName: 'ROC'; FAliasTo: @CZones[283]),
    (FName: 'ROK'; FAliasTo: @CZones[279]),
    (FName: 'Romance Standard Time'; FAliasTo: @CZones[388]),
    (FName: 'Russia Time Zone 10'; FAliasTo: @CZones[282]),
    (FName: 'Russia Time Zone 11'; FAliasTo: @CZones[250]),
    (FName: 'Russia Time Zone 3'; FAliasTo: @CZones[392]),
    (FName: 'Russian Standard Time'; FAliasTo: @CZones[386]),
    (FName: 'SA Eastern Standard Time'; FAliasTo: @CZones[87]),
    (FName: 'SA Pacific Standard Time'; FAliasTo: @CZones[81]),
    (FName: 'SA Western Standard Time'; FAliasTo: @CZones[134]),
    (FName: 'SE Asia Standard Time'; FAliasTo: @CZones[223]),
    (FName: 'Saint Pierre Standard Time'; FAliasTo: @CZones[147]),
    (FName: 'Sakhalin Standard Time'; FAliasTo: @CZones[277]),
    (FName: 'Samoa Standard Time'; FAliasTo: @CZones[428]),
    (FName: 'Sao Tome Standard Time'; FAliasTo: @CZones[48]),
    (FName: 'Saratov Standard Time'; FAliasTo: @CZones[394]),
    (FName: 'Singapore'; FAliasTo: @CZones[281]),
    (FName: 'Singapore Standard Time'; FAliasTo: @CZones[281]),
    (FName: 'South Africa Standard Time'; FAliasTo: @CZones[24]),
    (FName: 'Sri Lanka Standard Time'; FAliasTo: @CZones[231]),
    (FName: 'Sudan Standard Time'; FAliasTo: @CZones[27]),
    (FName: 'Syria Standard Time'; FAliasTo: @CZones[232]),
    (FName: 'Taipei Standard Time'; FAliasTo: @CZones[283]),
    (FName: 'Tasmania Standard Time'; FAliasTo: @CZones[317]),
    (FName: 'Tocantins Standard Time'; FAliasTo: @CZones[57]),
    (FName: 'Tokyo Standard Time'; FAliasTo: @CZones[289]),
    (FName: 'Tomsk Standard Time'; FAliasTo: @CZones[290]),
    (FName: 'Tonga Standard Time'; FAliasTo: @CZones[464]),
    (FName: 'Transbaikal Standard Time'; FAliasTo: @CZones[228]),
    (FName: 'Turkey'; FAliasTo: @CZones[373]),
    (FName: 'Turkey Standard Time'; FAliasTo: @CZones[373]),
    (FName: 'Turks And Caicos Standard Time'; FAliasTo: @CZones[111]),
    (FName: 'UCT'; FAliasTo: @CZones[355]),
    (FName: 'US Eastern Standard Time'; FAliasTo: @CZones[120]),
    (FName: 'US Mountain Standard Time'; FAliasTo: @CZones[165]),
    (FName: 'US/Alaska'; FAliasTo: @CZones[54]),
    (FName: 'US/Aleutian'; FAliasTo: @CZones[53]),
    (FName: 'US/Arizona'; FAliasTo: @CZones[165]),
    (FName: 'US/Central'; FAliasTo: @CZones[89]),
    (FName: 'US/East-Indiana'; FAliasTo: @CZones[120]),
    (FName: 'US/Eastern'; FAliasTo: @CZones[154]),
    (FName: 'US/Hawaii'; FAliasTo: @CZones[443]),
    (FName: 'US/Indiana-Starke'; FAliasTo: @CZones[121]),
    (FName: 'US/Michigan'; FAliasTo: @CZones[100]),
    (FName: 'US/Mountain'; FAliasTo: @CZones[99]),
    (FName: 'US/Pacific'; FAliasTo: @CZones[136]),
    (FName: 'US/Pacific-New'; FAliasTo: @CZones[136]),
    (FName: 'US/Samoa'; FAliasTo: @CZones[455]),
    (FName: 'UTC'; FAliasTo: @CZones[355]),
    (FName: 'UTC+12'; FAliasTo: @CZones[344]),
    (FName: 'UTC+13'; FAliasTo: @CZones[345]),
    (FName: 'UTC-02'; FAliasTo: @CZones[333]),
    (FName: 'UTC-08'; FAliasTo: @CZones[339]),
    (FName: 'UTC-09'; FAliasTo: @CZones[340]),
    (FName: 'UTC-11'; FAliasTo: @CZones[331]),
    (FName: 'Ulaanbaatar Standard Time'; FAliasTo: @CZones[291]),
    (FName: 'Universal'; FAliasTo: @CZones[355]),
    (FName: 'Venezuela Standard Time'; FAliasTo: @CZones[86]),
    (FName: 'Vladivostok Standard Time'; FAliasTo: @CZones[295]),
    (FName: 'Volgograd Standard Time'; FAliasTo: @CZones[407]),
    (FName: 'W-SU'; FAliasTo: @CZones[386]),
    (FName: 'W. Australia Standard Time'; FAliasTo: @CZones[321]),
    (FName: 'W. Central Africa Standard Time'; FAliasTo: @CZones[30]),
    (FName: 'W. Europe Standard Time'; FAliasTo: @CZones[362]),
    (FName: 'W. Mongolia Standard Time'; FAliasTo: @CZones[243]),
    (FName: 'West Asia Standard Time'; FAliasTo: @CZones[284]),
    (FName: 'West Bank Standard Time'; FAliasTo: @CZones[241]),
    (FName: 'West Pacific Standard Time'; FAliasTo: @CZones[459]),
    (FName: 'Yakutsk Standard Time'; FAliasTo: @CZones[296]),
    (FName: 'Zulu'; FAliasTo: @CZones[355])
  );



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
