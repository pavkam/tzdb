# TZDB - IANA Time Zone Database for Delphi/FreePascal

[![Test](https://github.com/pavkam/tzdb/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/pavkam/tzdb/actions/workflows/test.yml)
[![TZDB/CLDR Bump](https://github.com/pavkam/tzdb/actions/workflows/bump.yml/badge.svg?branch=master)](https://github.com/pavkam/tzdb/actions/workflows/bump.yml)

## Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Common Examples](#common-examples)
- [Important Considerations](#important-considerations)
- [Updating TZDB](#updating-tzdb)
- [Tools](#tools)
- [License](#license)

## Introduction

**TZDB** is an offline, in-process compiled database for [IANA's TZDB project](https://www.iana.org/time-zones). It provides reliable time zone conversion and calculation capabilities without requiring external services or runtime dependencies.

The current version is compiled with **2025b** version of IANA TZDB and the latest Windows alias translation table (from CLDR project).

The source code is compatible with **Delphi XE+** and **FreePascal 3+**, though some components are only available for Delphi.

📚 [API Documentation](https://github.com/pavkam/tzdb/wiki/API-Documentation) | 💻 [Code Examples](https://github.com/pavkam/tzdb/wiki/Code-Examples)

## Features

- **Offline operation**: No network dependencies or external services required
- **Small footprint**: Single-file implementation with pre-compiled TZDB database
- **Windows alias support**: Easily convert between Windows time zone names and IANA identifiers
- **Comprehensive API**: Rich set of methods for time zone conversions and calculations
- **Pure Pascal implementation**: No external dependencies or DLLs

## Installation

### Option 1: Direct inclusion (simplest)

Download [TZDB.pas](https://raw.githubusercontent.com/pavkam/tzdb/master/dist/TZDB.pas) and add it to your project.

### Option 2: Git submodule

```bash
git submodule add https://github.com/pavkam/tzdb.git
```

## Usage

Simply include the TZDB unit in your `uses` clause:

```pascal
uses TZDB;
```

## Common Examples

### Get a time zone and convert local time to UTC

```pascal
uses TZDB;

var
  LTimeZone: TBundledTimeZone;
begin
  // Get time zone by IANA identifier
  LTimeZone := TBundledTimeZone.GetTimeZone('Africa/Cairo');

  // Convert local time to UTC
  WriteLn(LTimeZone.ToUniversalTime(Now));
end;
```

### Using Windows time zone names

```pascal
uses TZDB;

var
  LTimeZone: TBundledTimeZone;
begin
  // Get time zone by Windows identifier
  LTimeZone := TBundledTimeZone.GetTimeZone('Eastern Standard Time');

  // Display information
  WriteLn('IANA ID: ', LTimeZone.ID);
  WriteLn('Current offset: ', LTimeZone.UtcOffset);
end;
```

### Check for ambiguous or invalid times

```pascal
uses TZDB;

var
  LTimeZone: TBundledTimeZone;
  LLocalTime: TDateTime;
  LTimeType: TLocalTimeType;
begin
  LTimeZone := TBundledTimeZone.GetTimeZone('America/New_York');
  LLocalTime := EncodeDate(2023, 11, 5) + EncodeTime(1, 30, 0, 0); // Fall DST transition

  // Check what type of local time this is
  LTimeType := LTimeZone.GetLocalTimeType(LLocalTime);

  case LTimeType of
    lttInvalid: WriteLn('This time does not exist (skipped during DST start)');
    lttAmbiguous: WriteLn('This time is ambiguous (occurs twice during DST end)');
    lttStandard: WriteLn('This is a standard time');
    lttDaylight: WriteLn('This is a daylight saving time');
  end;
end;
```

## Important Considerations

| Issue | Description | Handling |
|-------|-------------|----------|
| **Invalid IDs** | Unknown time zone IDs cause exceptions | `TBundledTimeZone.Create` throws `ETimeZoneInvalid` |
| **Year range** | The database covers a limited year range | Methods may throw `EUnknownTimeZoneYear` for dates outside coverage |
| **Ambiguous times** | During DST→Standard transitions, some hours occur twice | Use `AForceDaylight` parameter to control interpretation |
| **Invalid times** | During Standard→DST transitions, some hours are skipped | Handle `ELocalTimeInvalid` exceptions or check with `GetLocalTimeType` |
| **Daylight support** | Not all time zones have daylight saving time | Use `HasDaylightTime` to check before DST operations |
| **Multiple periods** | Some zones have multiple DST periods in a year | Don't assume a single daylight/standard transition per year |
| **Dynamic properties** | Display names and offsets change throughout the year | Don't rely on `DisplayName`, `Abbreviation`, or `UtcOffset` as constants |

## Updating TZDB

### Automatic Updates

This project follows IANA releases closely, typically with a 1-2 week delay.

### Manual Updates

To update manually:

1. Use the [update-compile.sh](https://raw.githubusercontent.com/pavkam/tzdb/master/update-compile.sh) script
2. Run under MacOS, Linux, or Windows WSL
3. Requires Free Pascal compiler

## Tools

### Time Zone Visualizer

A development tool to display time zone details (Windows only):

![Screen shot](media/tz_vis.jpg)

## License

This project is available under [MIT License](LICENSE).
