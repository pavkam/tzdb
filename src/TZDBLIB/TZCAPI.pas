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

{$INCLUDE '..\TZDBPK\Version.inc'}

unit TZCAPI;
interface
type
  ///  <summary>Time zone enumerator procedure.</summary>
  ///  <param name="Data">A data object provided by the consumer code.</param>
  ///  <param name="Name">The name of the currently enumerated time zone.</param>
  ///  <returns>Consmer must return <c>True</c> to interrupt the enumeraton process.</returns>
  TZ_EnumProc = function(Data: Pointer; Name: PChar): Boolean; stdcall;

  ///  <summary>A <c>Pointer</c> to a <see cref="TZCAPI|TZ_Instance"/> stucture.</summary>
  PTZ_Instance = ^TZ_Instance;

  ///  <summary>An opaque structure that identifies a time zone object.</summary>
  TZ_Instance = record
    FTZName: PChar;
    FTZObject: Pointer;
  end;

  ///  <summary>A <c>Pointer</c> to a <see cref="TZCAPI|TZ_Time"/> type.</summary>
  PTZ_Time = ^TZ_Time;

  ///  <summary>An <c>Int64</c> representing the C/UNIX time format.</summary>
  TZ_Time = Int64;

  ///  <summary>A somewhat generic type used for most API methods in this library.</summary>
  ///  <remarks>All negative values of this type signify an error. See <c>ERROR_*</c> constants for reference.
  ///  Zero and the positive spectrum of values is function dependant. Each function will provide appropriate documentation.</remarks>
  TZ_Result = Integer;

const
  ///  <summary>The local time is in the Standard year period.</summary>
  LOCAL_TIME_STANDARD = 1;
  ///  <summary>The local time is in the DST year period.</summary>
  LOCAL_TIME_DAYLIGHT = 2;
  ///  <summary>The local time is in DST -> Standard year period.</summary>
  LOCAL_TIME_AMBIGUOUS = 3;
  ///  <summary>The local time is in the Standard -> DST year period.</summary>
  LOCAL_TIME_INVALID = 4;
  ///  <summary>A generic error. Source or cause unknwown.</summary>
  ERROR_UNKNOWN = -1;
  ///  <summary>An argument passed to the function was invalid.</summary>
  ERROR_INVALID_ARG = -2;
  ///  <summary>The time zone instance passed to the function was detected as being disposed.</summary>
  ERROR_INST_DISPOSED = -3;
  ///  <summary>The local time value passed to the function was invalid in the given time zone.</summary>
  ERROR_INVALID_LOCAL_TIME = -4;

  ///  <summary>Lists all known/supported time zone names.</summary>
  ///  <param name="Data">User provided data that is passed along to <paramref name="EnumProc"/>.</param>
  ///  <param name="IncludeAliases">Pass <c>True</c> to include time zone aliases into the list.</param>
  ///  <param name="EnumProc">The enumerator procedure called for each item in the list.</param>
  ///  <returns><c>True</c> is returned if the enumeration process was interrupted by the consumer.
  ///  <c>False</c> is returned otherwise.</returns>
  function TZ_EnumTimeZones(IncludeAliases: Boolean; Data: Pointer; EnumProc: TZ_EnumProc): Boolean; stdcall;

  ///  <summary>Looks up a time zone object by the given <paramref name="Name"/>.</summary>
  ///  <param name="Name">The name time zone name to lookup. A <c>nil</c> value will be ignored.</param>
  ///  <returns>A <c>Pointer</c> to a data structure containing the time zone object. Should be treated as an opaque
  ///  <c>Pointer</c> by consumers. A <c>nil</c> value is returned if the time zone cannot be looked up.</returns>
  function TZ_InitInstance(Name: PChar): PTZ_Instance; stdcall;

  ///  <summary>Releases a previously acquired instance.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <returns>A zero or a positive value means success. See <c>ERROR_*</c> constants for possile errors (if the value is negative).</returns>
  function TZ_ReleaseInstance(Instance: PTZ_Instance): TZ_Result; stdcall;

  ///  <summary>Generates an abbreviation string for the given local time.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <param name="Time">The local time (C format).</param>
  ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
  ///  <param name="Abbrev">The output string parameter. Should be allocated.</param>
  ///  <param name="AbbrevLen">The number of characters that <paramref name="Abbrev"/> can hold (excluding trailing \0).</param>
  ///  <returns>The length of the actual name. If the value is negative, then an error occured.
  ///  See <c>ERROR_*</c> constants for possile errors.</returns>
  ///  <remarks>It is important to note that the consumer must specify an allocated memory block pointed by <paramref name="Abbrev"/>.
  ///  The <paramref name="AbbrevLen"/> parameter must contain the number of bytes allocated. This method will fill this memory
  ///  with the actual abbreviation and return the actual length of the abbreviation. There may be cases when the passed memory block
  ///  is smaller than the actual length of the abbreviation. In those cases, only the part that will fit will be copied into the
  ///  provided memory block.</remarks>
  function TZ_GetAbbreviation(Instance: PTZ_Instance; Time: TZ_Time;
    ForceDaylight: Boolean; Abbrev: PChar; AbbrevLen: Integer): TZ_Result; stdcall;

  ///  <summary>Generates a diplay name for the given local time.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <param name="Time">The local time (C format).</param>
  ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
  ///  <param name="DispName">The output string parameter. Should be allocated.</param>
  ///  <param name="DispNameLen">The number of characters that <paramref name="DispName"/> can hold (excluding trailing \0).</param>
  ///  <returns>The length of the actual name. If the value is negative, then an error occured.
  ///  See <c>ERROR_*</c> constants for possile errors.</returns>
  ///  <remarks>It is important to note that the consumer must specify an allocated memory block pointed by <paramref name="DispName"/>.
  ///  The <paramref name="DispNameLen"/> parameter must contain the number of bytes allocated. This method will fill this memory
  ///  with the actual name and return the actual length of the name. There may be cases when the passed memory block
  ///  is smaller than the actual length of the name. In those cases, only the part that will fit will be copied into the
  ///  provided memory block.</remarks>
  function TZ_GetDisplayName(Instance: PTZ_Instance; Time: TZ_Time;
    ForceDaylight: Boolean; DispName: PChar; DispNameLen: Integer): TZ_Result; stdcall;

  ///  <summary>Returns the type of the local time.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <param name="Time">The local time.</param>
  ///  <returns>A zero or a positive result means success. See <c>LOCAL_TIME_*</c> constants for possible types.
  ///  See <c>ERROR_*</c> constants for possile errors (if the value is negative).</returns>
  function TZ_GetLocalTimeType(Instance: PTZ_Instance; Time: TZ_Time): TZ_Result; stdcall;

  ///  <summary>Returns the UTC offset of the given local time.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <param name="Time">The local time.</param>
  ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
  ///  <param name="TimeType">An output parameter to which the UTC offset if copied.</param>
  ///  <returns>A zero or a positive value means success. See <c>ERROR_*</c> constants for possile errors (if the value is negative).</returns>
  function TZ_GetUtcOffset(Instance: PTZ_Instance; Time: TZ_Time; ForceDaylight: Boolean; Offset: PInteger): TZ_Result; stdcall;

  ///  <summary>Converts an UTC time to a local time.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <param name="UtcTime">The UTC time to be converted.</param>
  ///  <param name="LocalTime">The output parameter to hold the converted local time.</param>
  ///  <returns>A zero or a positive value means success. See <c>ERROR_*</c> constants for possile errors (if the value is negative).</returns>
  function TZ_UtcToLocal(Instance: PTZ_Instance; UtcTime: TZ_Time; LocalTime: PTZ_Time): TZ_Result; stdcall;

  ///  <summary>Converts a local time to UTC time.</summary>
  ///  <param name="Instance">The time zone instance.</param>
  ///  <param name="LocalTime">The local time to be converted.</param>
  ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
  ///  <param name="UtcTime">The output parameter to hold the converted UTC time.</param>
  ///  <returns>A zero or a positive value means success. See <c>ERROR_*</c> constants for possile errors (if the value is negative).</returns>
  function TZ_LocalToUtc(Instance: PTZ_Instance; LocalTime: TZ_Time; ForceDaylight: Boolean; UtcTime: PTZ_Time): TZ_Result; stdcall;

implementation
uses
  SysUtils,
  DateUtils,
  Types,
{$IFDEF DELPHI}TimeSpan,{$ENDIF}
  TZDB;

const
  SUCCESS = 1;

function TZ_EnumTimeZones(IncludeAliases: Boolean; Data: Pointer; EnumProc: TZ_EnumProc): Boolean;
var
  I: Integer;
  LArray: {$IFDEF SUPPORTS_TARRAY}TArray<string>{$ELSE}TStringDynArray{$ENDIF};
  LName: String;
begin
  Result := false;

  if @EnumProc = nil then
    Exit;

  LArray := TBundledTimeZone.KnownTimeZones(IncludeAliases);

  { Iterate over all known records. }
  for I := 0 to Length(LArray) - 1 do
  begin
    LName := LArray[I];

    if EnumProc(Data, PChar(LName)) then
    begin
      { If the enum proc returns true, stop iterating. }
      Result := true;
      Break;
    end;
  end;
end;

function TZ_InitInstance(Name: PChar): PTZ_Instance; stdcall;
var
  LObject: TBundledTimeZone;
begin
  if (Name = nil) then
    Exit(nil);

  { In case of any inner exception, do not leak it outside. }
  try
    LObject := TBundledTimeZone.GetTimeZone(Name);
  except
    Exit(nil);
  end;

  { Set result }
  New(Result);
  Result^.FTZObject := LObject;
  Result.FTZName := PChar(LObject.ID);
end;

function TZ_ReleaseInstance(Instance: PTZ_Instance): TZ_Result;
begin
  { Verify parameters }
  if Instance = nil then
    Exit(ERROR_INVALID_ARG);
  if Instance^.FTZObject = nil then
    Exit(ERROR_INST_DISPOSED);

  { In case of any inner exception, do not leak it outside. }
  Instance^.FTZObject := nil;
  Dispose(Instance);

  Result := SUCCESS;
end;

function TZ_GetAbbreviation(Instance: PTZ_Instance; Time: TZ_Time; ForceDaylight: Boolean; Abbrev: PChar; AbbrevLen: Integer): Integer;
var
  LValue: string;
begin
  { Verify parameters }
  if (Instance = nil) or (Abbrev = nil) or (AbbrevLen < 1)then
    Exit(ERROR_INVALID_ARG);
  if Instance^.FTZObject = nil then
    Exit(ERROR_INST_DISPOSED);

  { Get the value we need }
  try
    LValue := TBundledTimeZone(Instance^.FTZObject).GetAbbreviation(UnixToDateTime(Time), ForceDaylight);
  except
    on ELocalTimeInvalid do
      Exit(ERROR_INVALID_LOCAL_TIME);
    on Exception do
      Exit(ERROR_UNKNOWN);
  end;

  { Init the result }
  StrLCopy(Abbrev, PChar(LValue), AbbrevLen);

  { Return the actual length of the name }
  Result := Length(LValue);
end;

function TZ_GetDisplayName(Instance: PTZ_Instance; Time: TZ_Time; ForceDaylight: Boolean; DispName: PChar; DispNameLen: Integer): TZ_Result;
var
  LValue: string;
begin
  { Verify parameters }
  if (Instance = nil) or (DispName = nil) or (DispNameLen < 1)then
    Exit(ERROR_INVALID_ARG);
  if Instance^.FTZObject = nil then
    Exit(ERROR_INST_DISPOSED);

  { Get the value we need }
  try
    LValue := TBundledTimeZone(Instance^.FTZObject).GetDisplayName(UnixToDateTime(Time), ForceDaylight);
  except
    on ELocalTimeInvalid do
      Exit(ERROR_INVALID_LOCAL_TIME);
    on Exception do
      Exit(ERROR_UNKNOWN);
  end;

  { Init the result }
  StrLCopy(DispName, PChar(LValue), DispNameLen);

  { Return the actual length of the name }
  Result := Length(LValue);
end;

function TZ_GetLocalTimeType(Instance: PTZ_Instance; Time: TZ_Time): TZ_Result;
var
  LValue: TLocalTimeType;
begin
  { Verify parameters }
  if Instance = nil then
    Exit(ERROR_INVALID_ARG);
  if Instance^.FTZObject = nil then
    Exit(ERROR_INST_DISPOSED);

  { Get the value we need }
  try
    LValue := TBundledTimeZone(Instance^.FTZObject).GetLocalTimeType(UnixToDateTime(Time));
  except
    on ELocalTimeInvalid do
      Exit(ERROR_INVALID_LOCAL_TIME);
    on Exception do
      Exit(ERROR_UNKNOWN);
  end;

  { Get the value }
  case LValue of
    lttStandard: Result := LOCAL_TIME_STANDARD;
    lttDaylight: Result := LOCAL_TIME_DAYLIGHT;
    lttAmbiguous: Result := LOCAL_TIME_AMBIGUOUS;
    lttInvalid: Result := LOCAL_TIME_INVALID;
    else
      Result := ERROR_UNKNOWN;
  end;
end;

function TZ_GetUtcOffset(Instance: PTZ_Instance; Time: TZ_Time; ForceDaylight: Boolean; Offset: PInteger): TZ_Result;
var
  LValue: {$IFDEF DELPHI}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
  { Verify parameters }
  if Instance = nil then
    Exit(ERROR_INVALID_ARG);
  if Instance^.FTZObject = nil then
    Exit(ERROR_INST_DISPOSED);

  { Get the value we need }
  try
    LValue := TBundledTimeZone(Instance^.FTZObject).GetUtcOffset(UnixToDateTime(Time), ForceDaylight);

    { Transform into milliseconds }
    Offset^ := {$IFDEF DELPHI}Round(LValue.TotalSeconds){$ELSE}LValue{$ENDIF} * 1000;
  except
    on ELocalTimeInvalid do
      Exit(ERROR_INVALID_LOCAL_TIME);
    on Exception do
      Exit(ERROR_UNKNOWN);
  end;

  Result := SUCCESS;
end;

function TZ_UtcToLocal(Instance: PTZ_Instance; UtcTime: TZ_Time; LocalTime: PTZ_Time): TZ_Result; stdcall;
begin
  { Get the value we need }
  try
    LocalTime^ := DateTimeToUnix(TBundledTimeZone(Instance^.FTZObject).ToLocalTime(UnixToDateTime(UtcTime)));
  except
    Exit(ERROR_UNKNOWN);
  end;

  Result := SUCCESS;
end;

function TZ_LocalToUtc(Instance: PTZ_Instance; LocalTime: TZ_Time; ForceDaylight: Boolean; UtcTime: PTZ_Time): TZ_Result;
begin
  { Verify parameters }
  if Instance = nil then
    Exit(ERROR_INVALID_ARG);
  if Instance^.FTZObject = nil then
    Exit(ERROR_INST_DISPOSED);

  { Get the value we need }
  try
    UtcTime^ := DateTimeToUnix(TBundledTimeZone(Instance^.FTZObject).ToUniversalTime(UnixToDateTime(LocalTime), ForceDaylight));
  except
    on ELocalTimeInvalid do
      Exit(ERROR_INVALID_LOCAL_TIME);
    on Exception do
      Exit(ERROR_UNKNOWN);
  end;

  Result := SUCCESS;
end;

end.
