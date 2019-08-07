The entirety of the API surface in TZDB is located in the [TZDB.pas](https://github.com/pavkam/tzdb/blob/master/src/TZDBPK/TZDB.pas) file. There are few type defined in the unit and it is very easy to get started with.

This page lists the types, their methods and their uses.

## `TLocalTimeType`
> Defines four types that a local date/time type can be in.
* `lttStandard`
  > The local time is in the Standard year period.
* `lttDaylight`
  > The local time is in the DST year period.
* `lttAmbiguous`
  > The local time is in DST -> Standard year period.
* `lttInvalid`
  > The local time is in the Standard -> DST year period.

## `TYearSegment`
> Record that describes a particular date/time segment within a calendar year. Instances of this record are returned by `TBundledTimeZone.GetYearBreakdown` method.

```pascal
property StartsAt: TDateTime;
```
> The date/time value representing the start of the segment (incl. milliseconds).

```pascal
property EndsAt: TDateTime;
```
> The date/time value representing the end of the segment (incl. milliseconds).

```pascal
property LocalType: TLocalTimeType;
```
> Indicates the segment's local time type.

```pascal
property DisplayName: string;
```
> The time zone display name used to describe the segment.

```pascal
property UtcOffset: TTimeSpan;
```
> Time zone offset (including the DST bias).

## `TYearSegmentArray`
> An array of `TYearSegment` which is returned by `TBundledTimeZone.GetYearBreakdown` method.

## `ELocalTimeInvalid`
  > Exception thrown when the passed local time is invalid.
## `ETimeZoneInvalid`
  > Exception type used to signal the caller code that a requested time zone is not present in the bundled database or that its format is invalid.
## `EUnknownTimeZoneYear`
  > Exception type used to signal the caller code that date/time year details are not bundled for the given time zone.

## `TBundledTimeZone`
```pascal 
constructor Create(const ATimeZoneID: string)
```
> Creates a new instance of this timezone class.
> `ATimeZoneID` is the ID of the timezone to use (ex. _"Europe/Bucharest"_).
> Raises `ETimeZoneInvalid` if the specified ID cannot be found in the bundled database.
>
>⚠️**It is recommended that `GetTimeZone` is used instead of this constructor. The instances will be cached and reused in this case.**

```pascal
destructor Destroy; override;
```
> Destroys the current instance.
>
>⚠️**Do not call this method if the object was obtained through a call to `GetTimeZone`.**

```pascal
class function KnownTimeZones(const 
    AIncludeAliases: Boolean = False): TStringDynArray;
```
> Returns a list of known time zones.
> Set `AIncludeAliases` to `True` to include time zone aliases into the list.
> Returns an array of strings representing the IDs of the known time zones.

```pascal
class function GetTimeZoneFromAlias(const AAliasID: string): string;
```
> Returns the time zone name for a given alias.
> The `AAlias` argument is alias to lookup
> Returns the name of the time zone.
> Raises `ETimeZoneInvalid` if the specified alias cannot be found in the bundled database.

```pascal
class function GetTimeZone(const ATimeZoneID: string): TBundledTimeZone;
```
> Returns an instance of this time zone class.
> Argument `ATimeZoneID` is the ID of the timezone to use (ex. _"Europe/Bucharest"_).
> Raises `ETimeZoneInvalid` if the specified ID cannot be found in the bundled database.

```pascal
class function Version: string;
```
> Returns the current version of the TZDB component. Example: `2.0.1.105`.

```pascal
class function DbVersion: string;
```
> Returns the current version of the IANA TZDB bundled with this unit. Example: `2019b`.

```pascal
function GetYearBreakdown(const AYear: Word): TYearSegmentArray;
```
> Breaks a given year into components segments.
> Argument `AYear` is the year to get data for.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.

```pascal
function DaylightTimeStart(const AYear: Word): TDateTime;
```
> Obtains the start date/time of daylight saving period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.**

```pascal
function StandardTimeStart(const AYear: Word): TDateTime;
```
> Obtains the start date/time of standard period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function InvalidTimeStart(const AYear: word): TDateTime;
```
> Obtains the start date/time of invalid period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function AmbiguousTimeStart(const AYear: word): TDateTime;
```
> Obtains the start date/time of ambiguous period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function DaylightTimeEnd(const AYear: word): TDateTime;
```
> Obtains the end date/time of daylight saving period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function StandardTimeEnd(const AYear: word): TDateTime;
```
> Obtains the end date/time of standard period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function InvalidTimeEnd(const AYear: word): TDateTime;
```
> Obtains the end date/time of invalid period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function AmbiguousTimeEnd(const AYear: word): TDateTime;
```
> Obtains the end date/time of ambiguous period in the local time.
> Argument `AYear` is the year to get data for.
> Returns the requested date/time  or `00/00/0000 00:00:00.000` if there's no such period in the given year.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.
>
>⚠️**This function considers the first period of this type and will not work properly for complicated time zones.** 

```pascal
function HasDaylightTime(const AYear: word): Boolean;
```
> Determines if the time zone has daylight saving period.
> Argument `AYear` is the year to check.
> Raises `EUnknownTimeZoneYear` if the specified year is not in the bundled database.

```pascal
  function ToISO8601Format(const ADateTime: TDateTime): String;
```
> Converts an UTC date/time to the full ISO-8601 date time string.
> Argument `ADateTime` is the UTC date/time to format.
> Raises `EUnknownTimeZoneYear` if the date year is not in the bundled database.

```pascal
function GetAbbreviation(const ADateTime: TDateTime; 
    const AForceDaylight: Boolean = false): string;
```
> Generates an abbreviation string for the given local time.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the specified date/time year is not in the bundled database.
> Raises `ELocalTimeInvalid` if the specified local time is invalid.

```pascal
function GetDisplayName(const ADateTime: TDateTime; 
    const AForceDaylight: Boolean = false): string;
```
> Generates a display string for the given local time.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the specified date/time year is not in the bundled database.
> Raises `ELocalTimeInvalid` if the specified local time is invalid.

```pascal
function GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;
```
> Returns the type of the local time.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the specified date/time year is not in the bundled database.
> Raises `ELocalTimeInvalid` if the specified local time is invalid.

```pascal
function IsAmbiguousTime(const ADateTime: TDateTime): Boolean;
```
> Checks whether the specified local time is ambiguous.
> Argument `ADateTime` is the local time.
> Raises `EUnknownTimeZoneYear` if the specified date/time year is not in the bundled database.

```pascal
function IsDaylightTime(const ADateTime: TDateTime; 
   const AForceDaylight: Boolean = false): Boolean;
```
> Checks whether the specified local time is in the daylight saving period.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the year of the specified date/time year is not in the bundled database.

```pascal
function IsInvalidTime(const ADateTime: TDateTime): Boolean;
```
> Checks whether the specified local time is invalid.
> Argument `ADateTime` is the local time.
> Raises `EUnknownTimeZoneYear` if the year of the specified date/time year is not in the bundled database.

```pascal
function IsStandardTime(const ADateTime: TDateTime; 
    const AForceDaylight: Boolean = false): Boolean;
```
> Checks whether the specified local time is standard.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the year of the specified date/time year is not in the bundled database.

```pascal
function GetUtcOffset(const ADateTime: TDateTime; 
  const AForceDaylight: Boolean = false): TTimeSpan;
```
> Returns the UTC offset of the given local time.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the year of the specified date/time year is not in the bundled database.
> Raises `ELocalTimeInvalid` if the specified local time is invalid.

```pascal
function ToLocalTime(const ADateTime: TDateTime): TDateTime;
```
> Converts an UTC time to a local time.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the year of the specified date/time year is not in the bundled database.

```pascal
function ToUniversalTime(const ADateTime: TDateTime;
    const AForceDaylight: Boolean = false): TDateTime;
```
> Converts a local time to an UTC time.
> Argument `ADateTime` is the local time.
> `Argument` AForceDaylight specifies if ambiguous periods should be treated as DST rather than standard time.
> Raises `EUnknownTimeZoneYear` if the year of the specified date/time year is not in the bundled database.
> Raises `ELocalTimeInvalid` if the specified local time is invalid.

```pascal
property ID: string;
```
> Returns the ID of the timezone. An ID is a string that should uniquely identify the timezone.
  
```pascal
property DisplayName: string;
```
> Returns the current time zone's display name string.
  
```pascal
property Abbreviation: string;
```
> Returns the current time zone's abbreviation string.
  
```pascal
property UtcOffset: TTimeSpan
```
> Returns the current time zone's UTC offset.