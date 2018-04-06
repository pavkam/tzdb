(*
* Copyright (c) 2010-2013, Ciobanu Alexandru
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
unit TestTZDB;
interface

uses
  TestFramework,
{$IFNDEF SUPPORTS_TARRAY}Types,{$ENDIF}
{$IFDEF SUPPORTS_TTIMESPAN}TimeSpan,{$ENDIF}
  Generics.Collections,
  Classes,
  SysUtils,
  TypInfo,
  DateUtils,
  TZDB,
  KnownTZCases;

type
{$IFDEF SUPPORTS_TARRAY}
  TStringDynArray = TArray<string>;
{$ENDIF}

  { The actual test }
  TTZDBTest = class(TTestCase)
  private
    function ProcessPeriod(const ATZ: TBundledTimeZone; const AStart: TDateTime; out AEnd: TDateTime;
      out AType: TLocalTimeType; out AAbbr_DST, AAbbr_STD, ADisp_DST, ADisp_STD: string; out ABias_DST, ABias_STD: Int64): Boolean;

    function Decompose(const ATimeZone: TBundledTimeZone; const AYear: Word): TList<TDecomposedPeriod>;

    procedure CompareKnown(const AConst: array of TDecomposedPeriod; const AZoneId: string; const AYear: Word);

    procedure DumpShit(const aTimezoneID: string; const aYear: Word;
      const aFilename: string);

  published
    procedure Test_TZ_Contructor;
    procedure Test_TZ_GetTimeZone;
    procedure Test_TZ_KnownTimeZones;
    procedure Test_TZ_ISO8601_Conversion;

    procedure Test_Africa_Cairo_2010;
    procedure Test_Africa_Cairo_2009;
    procedure Test_Europe_Bucharest_2010;
    procedure Test_Africa_Accra_1997;
    procedure Test_America_Araguaina_1950;
    procedure Test_Etc_GTM12_2010;
    procedure Test_Etc_GTMMin9_1991;
    procedure Test_Europe_London_2018;
    procedure Test_America_St_Johns_2018;
  end;


  TTZDBTimezoneTest = class(TTestCase)
  const
    FMT_D_T_ISO = 'yyyy-mm-dd hh:nn:ss';
  private
    fTimeZoneID: string;
    fTimeZone: TBundledTimeZone;
    fYear : Word;

    fStdEnd,
    fInvStart,
    fInvEnd,
    fDstStart,
    fDstEnd,
    fAmbStart,
    fAmbEnd,
    fStdStart :tdatetime;
    function RandomDate(aFromDatetime, aToDatetime : TDateTime) : TDateTime;
  protected
    procedure Setup; override;
    procedure TearDown; override;

  published
    procedure Test_StandardTimeEnd;
    procedure Test_DaylightTimeStart;
    procedure Test_DaylightTimeEnd;
    procedure Test_StandardTimeStart;

    procedure Test_InvalidTimeStart;
    procedure Test_InvalidTimeEnd;
    procedure Test_AmbiguousTimeStart;
    procedure Test_AmbiguousTimeEnd;

    procedure Test_InvalidTime;
    procedure Test_AmbiguousTime;
    procedure Test_StandardTime;
    procedure Test_DaylightTime;

    procedure Test_OperatesDST;

  end;

  TTZDB_St_Johns_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure Setup; override;
    procedure TearDown; override;
  end;

  TTZDB_London_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure Setup; override;
    procedure TearDown; override;
  end;

  TTZDB_Canberra_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure Setup; override;
    procedure TearDown; override;
  end;

  TTZDB_NewYork_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure Setup; override;
    procedure TearDown; override;
  end;

implementation

uses
  math;

{ TTZDBTest }

function TTZDBTest.ProcessPeriod(
  const ATZ: TBundledTimeZone;
  const AStart: TDateTime;
  out AEnd: TDateTime;
  out AType: TLocalTimeType;
  out AAbbr_DST, AAbbr_STD, ADisp_DST, ADisp_STD: string;
  out ABias_DST, ABias_STD: Int64): Boolean;

  function SecondsBetweenNoAbs(const A1, A2: TDateTime): Int64;
  begin
    Result := SecondsBetween(A1, A2) * CompareDateTime(A2, A1);
  end;

var
  LYearOfStart: Word;
  LLocal, LUtc_AsDST, LUtc_AsSTD: TDateTime;
  LFailed: Boolean;
begin
  Result := false;
  LYearOfStart := YearOf(AStart);

  { Get the type of the local time in the starting time. Continue with the whole
    period that has the same type. }
  AType := ATZ.GetLocalTimeType(AStart);

  if (AType = lttStandard) or (AType = lttDaylight) or (AType = lttAmbiguous) then
  begin
    AAbbr_DST := ATZ.GetAbbreviation(AStart, True);
    AAbbr_STD := ATZ.GetAbbreviation(AStart, False);

    ADisp_DST := ATZ.GetDisplayName(AStart, True);
    ADisp_STD := ATZ.GetDisplayName(AStart, False);

{$IFDEF SUPPORTS_TTIMESPAN}
    ABias_DST := Round(ATZ.GetUtcOffset(AStart, True).TotalSeconds);
    ABias_STD := Round(ATZ.GetUtcOffset(AStart, False).TotalSeconds);
{$ELSE}
    ABias_DST := ATZ.GetUtcOffset(AStart, True);
    ABias_STD := ATZ.GetUtcOffset(AStart, False);
{$ENDIF}

  end else
  begin
    AAbbr_DST := '';
    AAbbr_STD := '';
    ADisp_DST :='';
    ADisp_STD := '';
    ABias_DST := 0;
    ABias_STD := 0;
  end;

  { Standard "other kind of" tests }
  if AType = lttAmbiguous then
  begin
    { TRUEs }
    CheckTrue(ATZ.IsAmbiguousTime(AStart), '[lttAmbiguous] <> IsAmbiguousTime');
    CheckTrue(ATZ.IsStandardTime(AStart, False), '[lttAmbiguous] <> IsStandardTime(False)');
    CheckTrue(ATZ.IsDaylightTime(AStart, True), '[lttAmbiguous] <> IsDaylightTime(True)');

    { FALSEs }
    CheckFalse(ATZ.IsStandardTime(AStart, True), '[lttAmbiguous] = IsStandardTime(True)');
    CheckFalse(ATZ.IsDaylightTime(AStart, False), '[lttAmbiguous] = IsDaylightTime(False)');
    CheckFalse(ATZ.IsInvalidTime(AStart), '[lttAmbiguous] = IsInvalidTime');

    { Conversion tests }
    LUtc_AsDST := ATZ.ToUniversalTime(AStart, True);
    LUtc_AsSTD := ATZ.ToUniversalTime(AStart, False);
    CheckEquals(
      Abs(ABias_DST - ABias_STD),
      SecondsBetween(LUtc_AsDST, LUtc_AsSTD), '[lttAmbiguous] Expected DST-STD offset between conversions.');

    LLocal := ATZ.ToLocalTime(LUtc_AsDST);
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttAmbiguous] Expected ToLocalTime to return the correct value from Utc (DST)');

    LLocal := ATZ.ToLocalTime(LUtc_AsSTD);
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttAmbiguous] Expected ToLocalTime to return the correct value from Utc (STD)');

    CheckEquals(ABias_DST, SecondsBetweenNoAbs(LUtc_AsDST, AStart), '[lttAmbiguous] Expected OFFSET to be correct for DST.');
    CheckEquals(ABias_STD, SecondsBetweenNoAbs(LUtc_AsSTD, AStart), '[lttAmbiguous] Expected OFFSET to be correct for STD.');
  end;

  if AType = lttInvalid then
  begin
    { TRUEs }
    CheckTrue(ATZ.IsInvalidTime(AStart), '[lttInvalid] <> IsInvalidTime');

    { FALSEs }
    CheckFalse(ATZ.IsStandardTime(AStart, True), '[lttInvalid] = IsStandardTime(True)');
    CheckFalse(ATZ.IsStandardTime(AStart, False), '[lttInvalid] = IsStandardTime(False)');
    CheckFalse(ATZ.IsDaylightTime(AStart, True), '[lttInvalid] = IsDaylightTime(True)');
    CheckFalse(ATZ.IsDaylightTime(AStart, False), '[lttInvalid] = IsDaylightTime(False)');
    CheckFalse(ATZ.IsAmbiguousTime(AStart), '[lttInvalid] = IsAmbiguousTime');

    { Conversion tests }
    try
      ATZ.ToUniversalTime(AStart, True);
      LFailed := false;
    except
      on E: ELocalTimeInvalid do
        LFailed := true;
    end;
    CheckTrue(LFailed, '[lttInvalid] Expected ELocalTimeInvalid in ToUniversalTime(True)');

    try
      ATZ.ToUniversalTime(AStart, False);
      LFailed := false;
    except
      on E: ELocalTimeInvalid do
        LFailed := true;
    end;
    CheckTrue(LFailed, '[lttInvalid] Expected ELocalTimeInvalid in ToUniversalTime(False)');
  end;

  if AType = lttStandard then
  begin
    { TRUEs }
    CheckTrue(ATZ.IsStandardTime(AStart, False), '[lttStandard] <> IsStandardTime(False)');
    CheckTrue(ATZ.IsStandardTime(AStart, True), '[lttStandard] <> IsStandardTime(True)');

    { FALSEs }
    CheckFalse(ATZ.IsDaylightTime(AStart, True), '[lttStandard] = IsDaylightTime(True)');
    CheckFalse(ATZ.IsDaylightTime(AStart, False), '[lttStandard] = IsDaylightTime(False)');
    CheckFalse(ATZ.IsAmbiguousTime(AStart), '[lttStandard] = IsAmbiguousTime');
    CheckFalse(ATZ.IsInvalidTime(AStart), '[lttStandard] = IsInvalidTime');

    { Conversion tests }
    LUtc_AsDST := ATZ.ToUniversalTime(AStart, True);
    LUtc_AsSTD := ATZ.ToUniversalTime(AStart, False);
    CheckEquals(0, CompareDateTime(LUtc_AsDST, LUtc_AsSTD), '[lttStandard] Expected ToUniversalTime to be consistent across DST vs STD.');

    LLocal := ATZ.ToLocalTime(LUtc_AsDST);
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttStandard] Expected ToLocalTime to return the correct value from Utc');

    CheckEquals(ABias_DST, SecondsBetweenNoAbs(LUtc_AsDST, AStart), '[lttStandard] Expected OFFSET to be correct for DST.');
    CheckEquals(ABias_STD, SecondsBetweenNoAbs(LUtc_AsSTD, AStart), '[lttStandard] Expected OFFSET to be correct for STD.');
  end;

  if AType = lttDaylight then
  begin
    { TRUEs }
    CheckTrue(ATZ.IsDaylightTime(AStart, False), '[lttDaylight] <> IsDaylightTime(False)');
    CheckTrue(ATZ.IsDaylightTime(AStart, True), '[lttDaylight] <> IsDaylightTime(True)');

    { FALSEs }
    CheckFalse(ATZ.IsStandardTime(AStart, True), '[lttDaylight] = IsStandardTime(True)');
    CheckFalse(ATZ.IsStandardTime(AStart, False), '[lttDaylight] = IsStandardTime(False)');
    CheckFalse(ATZ.IsAmbiguousTime(AStart), '[lttDaylight] = IsAmbiguousTime');
    CheckFalse(ATZ.IsInvalidTime(AStart), '[lttDaylight] = IsInvalidTime');

    { Conversion tests }
    LUtc_AsDST := ATZ.ToUniversalTime(AStart, True);
    LUtc_AsSTD := ATZ.ToUniversalTime(AStart, False);
    CheckEquals(0, CompareDateTime(LUtc_AsDST, LUtc_AsSTD), '[lttDaylight] Expected ToUniversalTime to be consistent across DST vs STD.');

    LLocal := ATZ.ToLocalTime(LUtc_AsDST);
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttDaylight] Expected ToLocalTime to return the correct value from Utc');

    CheckEquals(ABias_DST, SecondsBetweenNoAbs(LUtc_AsDST, AStart), '[lttDaylight] Expected OFFSET to be correct for DST.');
    CheckEquals(ABias_STD, SecondsBetweenNoAbs(LUtc_AsSTD, AStart), '[lttDaylight] Expected OFFSET to be correct for STD.');
  end;

  { --------------- Progress by hours }
  AEnd := AStart;
  while ATZ.GetLocalTimeType(AEnd) = AType do
  begin
    { Increase by an hour }
    AEnd := IncHour(AEnd, 1);

    { We reached the year's end }
    if YearOf(AEnd) <> LYearOfStart then
      break;
  end;

  { Remove the hour to be on the change spot }
  AEnd := IncHour(AEnd, -1);

  { ------------------- Progress by minutes }
  while ATZ.GetLocalTimeType(AEnd) = AType do
  begin
    { Increase by an hour }
    AEnd := IncMinute(AEnd, 1);

    { We reached the year's end }
    if YearOf(AEnd) <> LYearOfStart then
      break;
  end;

  { Remove the hour to be on the change spot }
  AEnd := IncMinute(AEnd, -1);

  { ------------------- Progress by second }
  while ATZ.GetLocalTimeType(AEnd) = AType do
  begin
    { Increase by an hour }
    AEnd := IncSecond(AEnd, 1);

    { We reached the year's end }
    if YearOf(AEnd) <> LYearOfStart then
    begin
      Result := true;
      break;
    end;
  end;

  { Remove the hour to be on the change spot }
  AEnd := IncSecond(AEnd, -1);
end;

procedure TTZDBTest.CompareKnown(const AConst: array of TDecomposedPeriod; const AZoneId: string; const AYear: Word);
var
  LTZ: TBundledTimeZone;
  LDecomposed: TList<TDecomposedPeriod>;
  I: Integer;
  LExp, LAct: TDecomposedPeriod;
  LNow: TDateTime;
  LNowBias: Int64;
  LNowAbbrev, LNowDispName: string;
begin
  LTZ := nil;

  try
    LTZ := TBundledTimeZone.GetTimeZone(AZoneId);
  except
    on ETimeZoneInvalid do
      Fail('TZDB seems to miss information about "' + AZoneId + '" time zone.');
  end;

  { Other pre-checks }
  CheckEquals(AZoneId, LTZ.ID, 'ID property failed for "' + AZoneId + '" time zone.');

  { Get properties }
  LNow := Now;

{$IFDEF SUPPORTS_TTIMESPAN}
  LNowBias := Round(LTZ.UtcOffset.TotalSeconds);
{$ELSE}
  LNowBias := LTZ.UtcOffset;
{$ENDIF}

  LNowAbbrev := LTZ.Abbreviation;
  LNowDispName := LTZ.DisplayName;

  CheckEquals(
{$IFDEF SUPPORTS_TTIMESPAN}
    Round(LTZ.GetUtcOffset(LNow).TotalSeconds),
{$ELSE}
    LTZ.GetUtcOffset(LNow),
{$ENDIF}
    LNowBias,
    'UtcOffset property failed for "' + AZoneId + '" time zone.'
  );

  CheckEquals(LTZ.GetAbbreviation(LNow), LNowAbbrev, 'Abbreviation property failed for "' + AZoneId + '" time zone.');
  CheckEquals(LTZ.GetDisplayName(LNow), LNowDispName, 'DisplayName property failed for "' + AZoneId + '" time zone.');

  { Decompose the period now }
  LDecomposed := Decompose(LTZ, AYear);

  try
    CheckTrue(LDecomposed <> nil, 'Expected an initialized decomposed list.');
    CheckTrue(LDecomposed.Count > 0, 'Expected a decomposed list with at leat one element.');

    CheckEquals(Length(AConst), LDecomposed.Count, 'Expected the decomposed count to be correct');

    { Now compare one-by-one }
    for I := 0 to Length(AConst) - 1 do
    begin
      { Compare each period }
      LExp := AConst[I];
      LAct := LDecomposed[I];

      CheckEquals(0, CompareDateTime(LExp.FStartsAt, LAct.FStartsAt), 'Start dates differ for ' + IntToStr(I) + ' period.');
      CheckEquals(0, CompareDateTime(LExp.FEndsAt, LAct.FEndsAt), 'End dates differ for ' + IntToStr(I) + ' period.');
      CheckEquals(Ord(LExp.FType), Ord(LAct.FType), 'Local time types differs for ' + IntToStr(I) + ' period.');
      CheckEquals(LExp.FAbbrv_AsDST, LAct.FAbbrv_AsDST, 'DST abbreviation differs for ' + IntToStr(I) + ' period.');
      CheckEquals(LExp.FAbbrv_AsSTD, LAct.FAbbrv_AsSTD, 'STD abbreviation differs for ' + IntToStr(I) + ' period.');
      CheckEquals(LExp.FName_AsDST, LAct.FName_AsDST, 'DST name differs for ' + IntToStr(I) + ' period.');
      CheckEquals(LExp.FName_AsSTD, LAct.FName_AsSTD, 'STD name differs for ' + IntToStr(I) + ' period.');
      CheckEquals(LExp.FBias_AsDST, LAct.FBias_AsDST, 'DST bias differs for ' + IntToStr(I) + ' period.');
      CheckEquals(LExp.FBias_AsSTD, LAct.FBias_AsSTD, 'STD bias differs for ' + IntToStr(I) + ' period.');
    end;

  finally
    { Free the list }
    LDecomposed.Free;
  end;
end;

function TTZDBTest.Decompose(const ATimeZone: TBundledTimeZone; const AYear: Word): TList<TDecomposedPeriod>;
var
  LShouldStop: Boolean;
  LStart, LEnd: TDateTime;
  LRec: TDecomposedPeriod;
begin
  { Start the process from the beggining of the year }
  LStart := EncodeDateTime(AYear, MonthJanuary, 1, 0, 0, 0, 0);
  Result := TList<TDecomposedPeriod>.Create();

  LShouldStop := false;

  while (not LShouldStop) do
  begin
    LShouldStop := ProcessPeriod(
      ATimeZone,
      LStart,
      LEnd,
      LRec.FType,
      LRec.FAbbrv_AsDST,
      LRec.FAbbrv_AsSTD,
      LRec.FName_AsDST,
      LRec.FName_AsSTD,
      LRec.FBias_AsDST,
      LRec.FBias_AsSTD
    );

    { Create a decomposed period }
    LRec.FStartsAt := LStart;
    LRec.FEndsAt := LEnd;

    { Push the period }
    Result.Add(LRec);

    { Adjust the start to the new end }
    LStart := IncSecond(LEnd, 1);
  end;
end;

procedure TTZDBTest.Test_Africa_Accra_1997;
begin
  CompareKnown(CAfrica_Accra_1997, 'Africa/Accra', 1997);
end;

procedure TTZDBTest.Test_Africa_Cairo_2009;
begin
  //DumpShit('Africa/Cairo', 2009, 'D:\SourceCode\Temp\Cairo_2009.txt');
  CompareKnown(CAfrica_Cairo_2009, 'Africa/Cairo', 2009);
end;

procedure TTZDBTest.Test_Africa_Cairo_2010;
begin
  //DumpShit('Africa/Cairo', 2010, 'D:\SourceCode\Temp\Cairo_2010.txt');
  CompareKnown(CAfrica_Cairo_2010, 'Africa/Cairo', 2010);
end;

procedure TTZDBTest.Test_America_Araguaina_1950;
begin
  //DumpShit('America/Araguaina', 1950, 'D:\SourceCode\Temp\Araguaina_1950.txt');
  CompareKnown(CAmerica_Araguaina_1950, 'America/Araguaina', 1950);
end;

procedure TTZDBTest.Test_America_St_Johns_2018;
begin
  //DumpShit('America/St_Johns', 2018, 'D:\SourceCode\Temp\St_Johns_2018.txt');
  CompareKnown(CAmerica_St_Johns_2018, 'America/St_Johns', 2018);
end;

procedure TTZDBTest.Test_Etc_GTM12_2010;
begin
  CompareKnown(CEtc_GMT12_2010, 'Etc/GMT+12', 2010);
end;

procedure TTZDBTest.Test_Etc_GTMMin9_1991;
begin
  CompareKnown(CEtc_GMTMin9_1991, 'Etc/GMT-9', 1991);
end;

procedure TTZDBTest.Test_Europe_Bucharest_2010;
begin
  CompareKnown(CEurope_Bucharest_2010, 'Europe/Bucharest', 2010);
end;

procedure TTZDBTest.Test_Europe_London_2018;
begin
  //DumpShit('Europe/London', 2018, 'D:\SourceCode\Temp\London_2010.txt');
  CompareKnown(CEurope_London_2018, 'Europe/London', 2018);
end;

procedure TTZDBTest.Test_TZ_Contructor;
var
  LTZ: TBundledTimeZone;
  LWasEx: Boolean;
begin
  { First try the exception }
  try
    TBundledTimeZone.Create('blah');
    LWasEx := false;
  except
    on E: ETimeZoneInvalid do
      LWasEx := true;
  end;

  CheckTrue(LWasEx, 'Expected ETimeZoneInvalid but got nothing.');

  { Second try out to load something that is there 100% }
  LTZ := TBundledTimeZone.Create('Etc/GMT+1');
  try
    CheckEquals('Etc/GMT+1', LTZ.ID);
  finally
    LTZ.Free;
  end;

  { And now try to load up an alias }
  LTZ := TBundledTimeZone.Create('GMT+1');
  try
    CheckEquals('Etc/GMT+1', LTZ.ID);
  finally
    LTZ.Free;
  end;
end;

procedure TTZDBTest.Test_TZ_GetTimeZone;
var
  LTZ: TBundledTimeZone;
  LWasEx: Boolean;
begin
  { First try the exception }
  try
    TBundledTimeZone.GetTimeZone('blah');
    LWasEx := false;
  except
    on E: ETimeZoneInvalid do
      LWasEx := true;
  end;

  CheckTrue(LWasEx, 'Expected ETimeZoneInvalid but got nothing.');

  { Second try out to load something that is there 100% }
  LTZ := TBundledTimeZone.GetTimeZone('Etc/GMT+1');
  CheckEquals('Etc/GMT+1', LTZ.ID);
  CheckTrue(LTZ = TBundledTimeZone.GetTimeZone('Etc/GMT+1'));

  { And now try to load up an alias }
  LTZ := TBundledTimeZone.GetTimeZone('GMT+1');
  CheckEquals('Etc/GMT+1', LTZ.ID);
  CheckTrue(LTZ = TBundledTimeZone.GetTimeZone('Etc/GMT+1'));
end;

procedure TTZDBTest.Test_TZ_KnownTimeZones;
var
  L1, L2: TStringDynArray;
  I: Integer;
begin
  { Load names with no aliases }
  L1 := TBundledTimeZone.KnownTimeZones(false);

  { Load names with aliases }
  L2 := TBundledTimeZone.KnownTimeZones(true);

  CheckTrue(Length(L2) >= Length(L1), 'Array with aliases whould be longer.');

  for I := 0 to Length(L1) - 1 do
    CheckEquals(L1[I], L2[I], 'Expected same order for known tables');
end;

procedure ttzdbTest.Test_TZ_ISO8601_Conversion;
var
  LTZ: TBundledTimeZone;
  sTimezone: string;
  sDatetime: string;
  Datetime: TDateTime;
begin

  {
  When local daylight time is about to reach
  Sunday, 1 April 2018, 03:00:00 clocks are turned backward 1 hour to
  Sunday, 1 April 2018, 02:00:00 local standard time instead.
  }

  sTimezone := 'Australia/Canberra';
  LTZ := TBundledTimeZone.Create(sTimezone);
  try
    //Local time 2018-04-01 02:00:00  DST
    Datetime  := EncodeDateTime(2018,3,31,15,00,00,0); //UTC Datetime
    sDatetime := LTZ.ToISO8601Str(Datetime);
    CheckEquals('2018-04-01 02:00:00.0+11:00', sDatetime, sTimezone);

    //Local time 2018-04-01 02:00:00  STD
    Datetime  := EncodeDateTime(2018,3,31,16,00,00,0); //UTC Datetime
    sDatetime := LTZ.ToISO8601Str(Datetime);
    CheckEquals('2018-04-01 02:00:00.0+10:00', sDatetime, sTimezone);
  finally
   LTZ.Free;
  end;

  {
  When local daylight time is about to reach
  Sunday, 28 October 2018, 02:00:00 clocks are turned backward 1 hour to
  Sunday, 28 October 2018, 01:00:00 local standard time instead.
  }

  sTimezone := 'Europe/London';
  LTZ := TBundledTimeZone.Create(sTimezone);
  try
    //Local time 2018-10-28 02:00:00  DST
    Datetime  := EncodeDateTime(2018,10,28,00,00,00,0); //UTC Datetime
    sDatetime := LTZ.ToISO8601Str(Datetime);
    CheckEquals('2018-10-28 01:00:00.0+01:00', sDatetime, sTimezone);

    //Local time 2018-10-28 02:00:00  STD
    Datetime  := EncodeDateTime(2018,10,28,01,00,00,0); //UTC Datetime
    sDatetime := LTZ.ToISO8601Str(Datetime);
    CheckEquals('2018-10-28 01:00:00.0+00:00', sDatetime, sTimezone);
  finally
   LTZ.Free;
  end;

  

end;

//  -- Generates proper constants out of what we need
procedure TTZDBTest.DumpShit(const aTimezoneID: string; const aYear: Word;
  const aFilename: string);
var
  LTZ: TBundledTimeZone;
  LDec: TList<TDecomposedPeriod>;
  LPer: TDecomposedPeriod;
  LWr: TStreamWriter;
  I: Integer;
begin
  LTZ := TBundledTimeZone.GetTimeZone(aTimezoneID);
  LDec := Decompose(LTZ, aYear);

  FormatSettings.DecimalSeparator := '.';
  LWr := TStreamWriter.Create(aFilename);
  LWr.WriteLine('const');
  LWr.WriteLine('  CDump: array[0 .. ' + IntToStr(LDec.Count - 1) + '] of TDecomposedPeriod = (');
  for I := 0 to LDec.Count - 1 do
  begin
    LPer := LDec[I];

    LWr.Write('    (FStartsAt: ' + FloatToStr(LPer.FStartsAt) + '; FEndsAt: ' + FloatToStr(LPer.FEndsAt) + '; FType: ' + GetEnumName(TypeInfo(TLocalTimeType), Ord(LPer.FType)) + '; ');
    LWr.WriteLine('FAbbrv_AsDST: ''' + LPer.FAbbrv_AsDST + '''; FAbbrv_AsSTD: ''' + LPer.FAbbrv_AsSTD + '''; ');
    LWr.Write('      FName_AsDST: ''' + LPer.FName_AsDST + '''; FName_AsSTD: ''' + LPer.FName_AsSTD + '''; FBias_AsDST: ' + IntToStr(LPer.FBias_AsDST) + '; FBias_AsSTD: ' + IntToStr(LPer.FBias_AsSTD));

    if I < (LDec.Count - 1) then
      LWr.WriteLine('),')
    else
      LWr.WriteLine(')');
  end;
  LWr.WriteLine('  );');

  LWr.Free;
  LDec.Free;
end;


{ TTZDBTimezoneTest }

function TTZDBTimezoneTest.RandomDate(aFromDatetime,
  aToDatetime: TDateTime): TDateTime;
var
  iFrom,
  iTo: Cardinal;
  iDay :Cardinal;
  fTime: Single;
begin
  Result := 0.0;
  Randomize;
  iFrom := Trunc(aFromDatetime);
  iTo := Trunc(aToDatetime);

  iDay := RandomRange(ifrom, ito);

  repeat
    fTime := Random;
    Result := iDay + fTime;
  until (Result >= aFromDatetime) and (Result <= aToDatetime);
end;

procedure TTZDBTimezoneTest.Setup;
begin
  inherited;
  fTimeZone := TBundledTimeZone.Create(fTimeZoneID);
end;

procedure TTZDBTimezoneTest.TearDown;
begin
  fTimeZone.Free;
  inherited;
end;

procedure TTZDBTimezoneTest.Test_AmbiguousTime;
var
  lType : TLocalTimeType;
  AmbiguousDt: TDateTime;
begin
  AmbiguousDt := RandomDate(fAmbStart, fAmbEnd);
  lType := fTimeZone.GetLocalTimeType(AmbiguousDt);
  CheckEquals(ord(lttAmbiguous), ord(lType), 'Expected local Ambiguous time type for: '+
    DateTimeToStr(AmbiguousDt));
end;

procedure TTZDBTimezoneTest.Test_AmbiguousTimeEnd;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.AmbiguousTimeEnd(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fAmbEnd);
  CheckEquals(ExpDatetime, ActDattime, 'AmbiguousTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_AmbiguousTimeStart;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.AmbiguousTimeStart(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fAmbStart);
  CheckEquals(ExpDatetime, ActDattime, 'AmbiguousTimeStart');
end;

procedure TTZDBTimezoneTest.Test_DaylightTime;
var
  lType : TLocalTimeType;
begin
  lType := fTimeZone.GetLocalTimeType(fDstStart);
  CheckEquals(ord(lttDaylight), ord(lType), 'Expected local Daylight time type');
end;

procedure TTZDBTimezoneTest.Test_DaylightTimeEnd;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.DaylightTimeEnd(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fDstEnd);
  CheckEquals(ExpDatetime, ActDattime, 'DaylightTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_DaylightTimeStart;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.DaylightTimeStart(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fDstStart);
  CheckEquals(ExpDatetime, ActDattime, 'DaylightTimeStart');
end;

procedure TTZDBTimezoneTest.Test_InvalidTime;
var
  InvalidDt: TDateTime;
  lType : TLocalTimeType;
begin
  InvalidDt := RandomDate(fInvStart, fInvEnd);

  lType := fTimeZone.GetLocalTimeType(InvalidDt);
  CheckEquals(ord(lttInvalid), ord(lType), 'Expected local Invalid time type for:' +
    datetimetostr(InvalidDt));
end;

procedure TTZDBTimezoneTest.Test_InvalidTimeEnd;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.InvalidTimeEnd(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fInvEnd);
  CheckEquals(ExpDatetime, ActDattime, 'InvalidTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_InvalidTimeStart;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.InvalidTimeStart(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fInvStart);
  CheckEquals(ExpDatetime, ActDattime, 'InvalidTimeStart');
end;

procedure TTZDBTimezoneTest.Test_OperatesDST;
var
  OperatesdDST: Boolean;
begin
  OperatesdDST := fTimeZone.OperatesDayligtTime(fYear);
  CheckTrue(OperatesdDST, 'Operates DaylightSaving:');
end;

procedure TTZDBTimezoneTest.Test_StandardTime;
var
  lType : TLocalTimeType;
begin
  lType := fTimeZone.GetLocalTimeType(fStdStart);
  CheckEquals(ord(lttStandard), ord(lType), 'Expected local Standard time type');
end;

procedure TTZDBTimezoneTest.Test_StandardTimeEnd;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.StandardTimeEnd(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fStdEnd);
  CheckEquals(ExpDatetime, ActDattime, 'StandardTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_StandardTimeStart;
var
  ExpDatetime: string;
  ActDattime: string;
begin
  DateTimeToString(ActDattime, FMT_D_T_ISO, fTimeZone.StardardTimeStart(fYear));
  DateTimeToString(ExpDatetime, FMT_D_T_ISO, fStdStart);
  CheckEquals(ExpDatetime, ActDattime, 'StandardTimeStart');
end;

{ TTZDB_St_Johns_2018_Test }

procedure TTZDB_St_Johns_2018_Test.Setup;
begin
  fyear := 2018;
  FtimeZoneID := 'America/St_Johns';

  {
  When local standard time was about to reach
  Sunday, 11 March 2018, 02:00:00 clocks were turned forward 1 hour to
  Sunday, 11 March 2018, 03:00:00 local daylight time instead.

  When local daylight time is about to reach
  Sunday, 4 November 2018, 02:00:00 clocks are turned backward 1 hour to
  Sunday, 4 November 2018, 01:00:00 local standard time instead.
  }
  fStdEnd   := 43170 +((1/86400)*7199);   //2018-03-11 01:59:59
  fInvStart := 43170 +((1/86400)*7200);   //2018-03-11 02:00:00
  fInvEnd   := 43170 +((1/86400)*10799);  //2018-03-11 02:59:59
  fDstStart := 43170 +((1/86400)*10800);  //2018-03-11 03:00:00
  fDstEnd   := 43408 +((1/86400)*3599);   //2018-11-04 00:59:59
  fAmbStart := 43408 +((1/86400)*3600);   //2018-11-04 01:00:00
  fAmbEnd   := 43408 +((1/86400)*7199);   //2018-11-04 01:59:59
  fStdStart := 43408 +((1/86400)*7200);   //2018-11-04 02:00:00


  inherited;
end;

procedure TTZDB_St_Johns_2018_Test.TearDown;
begin
  inherited;
end;

{ TTZDB_London_2018_Test }

procedure TTZDB_London_2018_Test.Setup;
begin
  fyear := 2018;
  FtimeZoneID := 'Europe/London';

  {
  When local standard time is about to reach
  Sunday, 25 March 2018, 01:00:00 clocks are turned forward 1 hour to
  Sunday, 25 March 2018, 02:00:00 local daylight time instead.

  When local daylight time is about to reach
  Sunday, 28 October 2018, 02:00:00 clocks are turned backward 1 hour to
  Sunday, 28 October 2018, 01:00:00 local standard time instead.
  }

  fStdEnd   := 43184 +((1/86400)*3599); //2018-03-25 00:59:59
  fInvStart := 43184 +((1/86400)*3600); //2018-03-25 01:00:00
  fInvEnd   := 43184 +((1/86400)*7199); //2018-03-25 01:59:59
  fDstStart := 43184 +((1/86400)*7200); //2018-03-25 02:00:00
  fDstEnd   := 43401 +((1/86400)*3599); //2018-10-28 00:59:59
  fAmbStart := 43401 +((1/86400)*3600); //2018-10-28 01:00:00
  fAmbEnd   := 43401 +((1/86400)*7199); //2018-10-28 01:59:59
  fStdStart := 43401 +((1/86400)*7200); //2018-10-28 02:00:00

  inherited;
end;

procedure TTZDB_London_2018_Test.TearDown;
begin
  inherited;
end;

{ TTZDB_Canberra_2018_Test }

procedure TTZDB_Canberra_2018_Test.Setup;
begin
  fyear := 2018;
  FtimeZoneID := 'Australia/Canberra';

  {
  When local daylight time is about to reach
  Sunday, 1 April 2018, 03:00:00 clocks are turned backward 1 hour to
  Sunday, 1 April 2018, 02:00:00 local standard time instead.

  When local standard time is about to reach
  Sunday, 7 October 2018, 02:00:00 clocks are turned forward 1 hour to
  Sunday, 7 October 2018, 03:00:00 local daylight time instead.

  }
  fDstEnd   := 43191 +((1/86400)*7199);   //2018-04-01 01:59:59
  fAmbStart := 43191 +((1/86400)*7200);   //2018-04-01 02:00:00
  fAmbEnd   := 43191 +((1/86400)*10799);  //2018-04-01 02:59:59
  fStdStart := 43191 +((1/86400)*10800);  //2018-04-01 03:00:00
  fStdEnd   := 43380 +((1/86400)*7199);   //2018-10-07 01:59:59
  fInvStart := 43380 +((1/86400)*7200);   //2018-10-07 02:00:00
  fInvEnd   := 43380 +((1/86400)*10799);   //2018-10-07 01:59:59
  fDstStart := 43380 +((1/86400)*10800);   //2018-10-07 02:00:00


  inherited;
end;

procedure TTZDB_Canberra_2018_Test.TearDown;
begin
  inherited;
end;

{ TTZDB_NewYork_2018_Test }

procedure TTZDB_NewYork_2018_Test.Setup;
begin
  fyear := 2018;
  FtimeZoneID := 'America/New_York';

  {
  When local standard time was about to reach
  Sunday, 11 March 2018, 02:00:00 clocks were turned forward 1 hour to
  Sunday, 11 March 2018, 03:00:00 local daylight time instead.

  When local daylight time is about to reach
  Sunday, 4 November 2018, 02:00:00 clocks are turned backward 1 hour to
  Sunday, 4 November 2018, 01:00:00 local standard time instead.
  }
  fStdEnd   := 43170 +((1/86400)*7199);   //2018-03-11 01:59:59
  fInvStart := 43170 +((1/86400)*7200);   //2018-03-11 02:00:00
  fInvEnd   := 43170 +((1/86400)*10799);  //2018-03-11 02:59:59
  fDstStart := 43170 +((1/86400)*10800);  //2018-03-11 03:00:00
  fDstEnd   := 43408 +((1/86400)*3599);   //2018-11-04 00:59:59
  fAmbStart := 43408 +((1/86400)*3600);   //2018-11-04 01:00:00
  fAmbEnd   := 43408 +((1/86400)*7199);   //2018-11-04 01:59:59
  fStdStart := 43408 +((1/86400)*7200);   //2018-11-04 02:00:00

  inherited;
end;

procedure TTZDB_NewYork_2018_Test.TearDown;
begin
  inherited;
end;

initialization
  RegisterTest(TTZDBTest.Suite);
  RegisterTest(TTZDB_St_Johns_2018_Test.Suite);
  RegisterTest(TTZDB_London_2018_Test.Suite);
  RegisterTest(TTZDB_Canberra_2018_Test.Suite);
  RegisterTest(TTZDB_NewYork_2018_Test.Suite);
end.

