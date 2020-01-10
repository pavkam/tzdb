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

unit TestTZDB;
interface

uses
{$IFDEF DELPHI}
  TestFramework,
  TimeSpan,
  Generics.Collections,
{$ELSE}
  Types,
  FPCUnit, TestUtils, TestRegistry,
  FGL,
{$ENDIF}
  Classes,
  SysUtils,
  TypInfo,
  DateUtils,
  TZDB,
  KnownTZCases;

type
{$IFDEF DELPHI}
  TStringDynArray = TArray<string>;
{$ENDIF}

  { The actual test }
  TTZDBTest = class(TTestCase)
  private
    function ProcessPeriod(const ATZ: TBundledTimeZone; const AStart: TDateTime; out AEnd: TDateTime;
      out AType: TLocalTimeType; out AAbbr_DST, AAbbr_STD, ADisp_DST, ADisp_STD: string; out ABias_DST, ABias_STD: Int64): Boolean;

    function Decompose(const ATimeZone: TBundledTimeZone; const AYear: Word): {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TDecomposedPeriod>;
    procedure CompareKnown(const AConst: array of TDecomposedPeriod; const AZoneId: string; const AYear: Word);

  published
    procedure Test_TZ_Contructor;
    procedure Test_TZ_GetTimeZone;
    procedure Test_TZ_KnownTimeZones;
    procedure Test_TZ_ISO8601_Conversion;
    procedure Test_TZ_DbVersion;
    procedure Test_TZ_Version;

    procedure Test_TZ_GetYearBreakdown_Cairo_1900;
    procedure Test_TZ_GetYearBreakdown_Cairo_2012;
    procedure Test_TZ_GetYearBreakdown_Bucharest_2014;
    procedure Test_TZ_GetYearBreakdown_Dublin_2019;
    procedure Test_TZ_GetYearBreakdown_Sao_Paulo_2014;
    procedure Test_TZ_GetYearBreakdown_Cairo_2014;
    procedure Test_TZ_GetYearBreakdown_Apia_2010;
    procedure Test_TZ_GetYearBreakdown_Apia_2011;
    procedure Test_TZ_GetYearBreakdown_Andorra_1900;
    procedure Test_TZ_GetYearBreakdown_Sofia_1880;
    procedure Test_TZ_GetYearBreakdown_Sofia_1879;
    procedure Test_TZ_GetYearBreakdown_Paris_0001;
    procedure Test_TZ_GetYearBreakdown_UTC_9998;

    procedure Test_TZ_GetLocalTimeType_Validation;

    procedure Test_TZ_ToLocal_Regression_1;
    procedure Test_TZ_ToLocal_Regression_2;
    procedure Test_TZ_ToLocal_Regression_3;
    procedure Test_TZ_GetAbbreviation_Regression_1;

	procedure Test_TZ_ToLocal_AtYearBoundary_WithPositiveOffset;
	procedure Test_TZ_ToLocal_AtYearBoundary_WithNegativeOffset;

    procedure Test_Africa_Cairo_2010;
    procedure Test_Africa_Cairo_2009;
    procedure Test_Europe_Bucharest_2010;
    procedure Test_Africa_Accra_1997;
    procedure Test_America_Araguaina_1950;
    procedure Test_Etc_GTM12_2010;
    procedure Test_Etc_GTMMin9_1991;
    procedure Test_Europe_London_2018;
    procedure Test_America_St_Johns_2018;
    procedure Test_Asia_Jerusalem_2005;
    procedure Test_Asia_Jerusalem_2006;
  end;


  TTZDBTimezoneTest = class(TTestCase)
  const
    FMT_D_T_ISO = 'yyyy-mm-dd hh:nn:ss';
  private
    FTimeZoneID: string;
    FTZ: TBundledTimeZone;
    FYear : Word;

    fStdEnd,
    FInvStart,
    FInvEnd,
    FDstStart,
    FDstEnd,
    FAmbStart,
    FAmbEnd,
    FStdStart: TDateTime;

    function RandomDate(AFromDateTime, AToDateTime: TDateTime) : TDateTime;
  protected
    procedure SetUp; override;
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
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTZDB_London_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTZDB_Canberra_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTZDB_NewYork_2018_Test = class(TTZDBTimezoneTest)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  Math;

{$IFDEF FPC}
function MillisecondsBetween(const A1, A2: TDateTime): Int64; inline;
begin
  Result := Round(Abs(TimeStampToMSecs(DateTimeToTimeStamp(A1)) - TimeStampToMSecs(DateTimeToTimeStamp(A2))));
end;

function SecondsBetween(const A1, A2: TDateTime): Int64; inline;
begin
  Result := MillisecondsBetween(A1, A2) div 1000;
end;
{$ENDIF}

function SecondsBetweenNoAbs(const A1, A2: TDateTime): Int64; inline;
begin
  Result := SecondsBetween(A1, A2) * CompareDateTime(A2, A1);
end;

{ TTZDBTest }

function TTZDBTest.ProcessPeriod(
  const ATZ: TBundledTimeZone;
  const AStart: TDateTime;
  out AEnd: TDateTime;
  out AType: TLocalTimeType;
  out AAbbr_DST, AAbbr_STD, ADisp_DST, ADisp_STD: string;
  out ABias_DST, ABias_STD: Int64): Boolean;
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

{$IFDEF DELPHI}
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
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttAmbiguous] Expected ToLocalTime to return the correct value from Utc (DST). ' +
      DateTimeToStr(LUtc_AsDST) + ' > ' + DateTimeToStr(AStart) + ' <> ' + DateTimeToStr(LLocal));

    LLocal := ATZ.ToLocalTime(LUtc_AsSTD);
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttAmbiguous] Expected ToLocalTime to return the correct value from Utc (STD). ' +
      DateTimeToStr(LUtc_AsSTD) + ' > ' + DateTimeToStr(AStart) + ' <> ' + DateTimeToStr(LLocal));

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
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttStandard] Expected ToLocalTime to return the correct value from Utc. ' +
      DateTimeToStr(LUtc_AsDST) + ' > ' + DateTimeToStr(AStart) + ' <> ' + DateTimeToStr(LLocal));

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
    CheckEquals(0, CompareDateTime(AStart, LLocal), '[lttDaylight] Expected ToLocalTime to return the correct value from Utc. ' +
      DateTimeToStr(LUtc_AsDST) + ' > ' + DateTimeToStr(AStart) + ' <> ' + DateTimeToStr(LLocal));

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

  { ------------------- Progress by millis }
  while ATZ.GetLocalTimeType(AEnd) = AType do
  begin
    { Increase by an hour }
    AEnd := IncMillisecond(AEnd, 1);

    { We reached the year's end }
    if YearOf(AEnd) <> LYearOfStart then
    begin
      Result := true;
      break;
    end;
  end;

  { Remove the hour to be on the change spot }
  AEnd := IncMillisecond(AEnd, -1);
end;

procedure TTZDBTest.CompareKnown(const AConst: array of TDecomposedPeriod; const AZoneId: string; const AYear: Word);
var
  LTZ: TBundledTimeZone;
  LDecomposed: {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TDecomposedPeriod>;
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

{$IFDEF DELPHI}
  LNowBias := Round(LTZ.UtcOffset.TotalSeconds);
{$ELSE}
  LNowBias := LTZ.UtcOffset;
{$ENDIF}

  LNowAbbrev := LTZ.Abbreviation;
  LNowDispName := LTZ.DisplayName;

  CheckEquals(
{$IFDEF DELPHI}
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

function TTZDBTest.Decompose(const ATimeZone: TBundledTimeZone; const AYear: Word): {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TDecomposedPeriod>;
var
  LShouldStop: Boolean;
  LStart, LEnd: TDateTime;
  LRec: TDecomposedPeriod;
begin
  { Start the process from the beggining of the year }
  LStart := EncodeDateTime(AYear, 1, 1, 0, 0, 0, 0);
  Result := {$IFDEF FPC}TFPGList{$ELSE}TList{$ENDIF}<TDecomposedPeriod>.Create();

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
  CompareKnown(CAfrica_Cairo_2009, 'Africa/Cairo', 2009);
end;

procedure TTZDBTest.Test_Africa_Cairo_2010;
begin
  CompareKnown(CAfrica_Cairo_2010, 'Africa/Cairo', 2010);
end;

procedure TTZDBTest.Test_America_Araguaina_1950;
begin
  CompareKnown(CAmerica_Araguaina_1950, 'America/Araguaina', 1950);
end;

procedure TTZDBTest.Test_America_St_Johns_2018;
begin
  CompareKnown(CAmerica_St_Johns_2018, 'America/St_Johns', 2018);
end;

procedure TTZDBTest.Test_Asia_Jerusalem_2005;
begin
  CompareKnown(CAsia_Jerusalem_2005, 'Asia/Jerusalem', 2005);
end;

procedure TTZDBTest.Test_Asia_Jerusalem_2006;
begin
  CompareKnown(CAsia_Jerusalem_2006, 'Asia/Jerusalem', 2006);
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
  CompareKnown(CEurope_London_2018, 'Europe/London', 2018);
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Bucharest_2014;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Europe/Bucharest');
  LSegments := LTZ.GetYearBreakdown(2014);

  CheckEquals(5, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDate(2014, 1, 1), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 3, 30, 2, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('EET', LSegments[0].DisplayName);
  CheckEquals(7200, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 3, 30, 3, 0, 0, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 3, 30, 3, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[1].LocalType));
  CheckEquals('EET', LSegments[1].DisplayName);
  CheckEquals(7200, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 3, 30, 4, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 26, 2, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[2].LocalType));
  CheckEquals('EEST', LSegments[2].DisplayName);
  CheckEquals(10800, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 4 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 26, 3, 0, 0, 0), LSegments[3].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 26, 3, 59, 59, 999), LSegments[3].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[3].LocalType));
  CheckEquals('EEST', LSegments[3].DisplayName);
  CheckEquals(10800, LSegments[3].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 5 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 26, 4, 0, 0, 0), LSegments[4].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 12, 31, 23, 59, 59, 999), LSegments[4].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[4].LocalType));
  CheckEquals('EET', LSegments[4].DisplayName);
  CheckEquals(7200, LSegments[4].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Cairo_1900;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Africa/Cairo');
  LSegments := LTZ.GetYearBreakdown(1900);

  CheckEquals(3, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 9, 30, 23, 54, 50, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('LMT', LSegments[0].DisplayName);
  CheckEquals(7509, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 9, 30, 23, 54, 51, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 9, 30, 23, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[1].LocalType));
  CheckEquals('LMT', LSegments[1].DisplayName);
  CheckEquals(7509, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 10, 1, 0, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 12, 31, 23, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[2].LocalType));
  CheckEquals('EET', LSegments[2].DisplayName);
  CheckEquals(7200, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Andorra_1900;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Europe/Andorra');
  LSegments := LTZ.GetYearBreakdown(1900);

  CheckEquals(2, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 12, 31, 23, 53, 55, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('LMT', LSegments[0].DisplayName);
  CheckEquals(364, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 12, 31, 23, 53, 56, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1900, 12, 31, 23, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[1].LocalType));
  CheckEquals('LMT', LSegments[1].DisplayName);
  CheckEquals(364, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Sofia_1880;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Europe/Sofia');
  LSegments := LTZ.GetYearBreakdown(1880);

  CheckEquals(2, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1880, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1880, 1, 1, 0, 23, 39, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[0].LocalType));
  CheckEquals('LMT', LSegments[0].DisplayName);
  CheckEquals(5596, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1880, 1, 1, 0, 23, 40, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1880, 12, 31, 23, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[1].LocalType));
  CheckEquals('IMT', LSegments[1].DisplayName);
  CheckEquals(7016, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Sofia_1879;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Europe/Sofia');
  LSegments := LTZ.GetYearBreakdown(1879);

  CheckEquals(1, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1879, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1879, 12, 31, 23, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('LMT', LSegments[0].DisplayName);
  CheckEquals(5596, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Paris_0001;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Europe/Sofia');
  LSegments := LTZ.GetYearBreakdown(1);

  CheckEquals(1, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(1, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(1, 12, 31, 23, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('LMT', LSegments[0].DisplayName);
  CheckEquals(5596, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Apia_2010;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Pacific/Apia');
  LSegments := LTZ.GetYearBreakdown(2010);

  CheckEquals(3, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2010, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2010, 9, 25, 23, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('-11', LSegments[0].DisplayName);
  CheckEquals(-39600, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2010, 9, 26, 0, 0, 0, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2010, 9, 26, 0, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[1].LocalType));
  CheckEquals('-11', LSegments[1].DisplayName);
  CheckEquals(-39600, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2010, 9, 26, 1, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2010, 12, 31, 23, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[2].LocalType));
  CheckEquals('-10', LSegments[2].DisplayName);
  CheckEquals(-36000, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Apia_2011;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Pacific/Apia');
  LSegments := LTZ.GetYearBreakdown(2011);

  CheckEquals(7, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 4, 2, 2, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[0].LocalType));
  CheckEquals('-10', LSegments[0].DisplayName);
  CheckEquals(-36000, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

   { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 4, 2, 3, 0, 0, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 4, 2, 3, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[1].LocalType));
  CheckEquals('-10', LSegments[1].DisplayName);
  CheckEquals(-36000, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 4, 2, 4, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 9, 24, 2, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[2].LocalType));
  CheckEquals('-11', LSegments[2].DisplayName);
  CheckEquals(-39600, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 4 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 9, 24, 3, 0, 0, 0), LSegments[3].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 9, 24, 3, 59, 59, 999), LSegments[3].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[3].LocalType));
  CheckEquals('-11', LSegments[3].DisplayName);
  CheckEquals(-39600, LSegments[3].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 5 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 9, 24, 4, 0, 0, 0), LSegments[4].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 12, 29, 23, 59, 59, 999), LSegments[4].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[4].LocalType));
  CheckEquals('-10', LSegments[4].DisplayName);
  CheckEquals(-36000, LSegments[4].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 6 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 12, 30, 0, 0, 0, 0), LSegments[5].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 12, 30, 23, 59, 59, 999), LSegments[5].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[5].LocalType));
  CheckEquals('-10', LSegments[5].DisplayName);
  CheckEquals(-39600, LSegments[5].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 7 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 12, 31, 0, 0, 0, 0), LSegments[6].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2011, 12, 31, 23, 59, 59, 999), LSegments[6].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[6].LocalType));
  CheckEquals('+14', LSegments[6].DisplayName);
  CheckEquals(50400, LSegments[6].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Cairo_2012;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Africa/Cairo');
  LSegments := LTZ.GetYearBreakdown(2012);

  CheckEquals(1, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDate(2012, 1, 1), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2012, 12, 31, 23, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('EET', LSegments[0].DisplayName);
  CheckEquals(7200, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetAbbreviation_Regression_1;
var
  LTZ: TBundledTimeZone;
  C: string;
begin
  LTZ := TBundledTimeZone.GetTimeZone('Europe/London');
  C := LTZ.GetAbbreviation(EncodeDateTime(2018, 10, 28, 1, 0, 0, 0), true);

  CheckEquals('GMT+01', C);
end;

procedure TTZDBTest.Test_TZ_ToLocal_AtYearBoundary_WithPositiveOffset;
var
  LTZ: TBundledTimeZone;
  LLocal: TDateTime;
begin
  LTZ := TBundledTimeZone.GetTimeZone('Pacific/Auckland');
  LLocal := LTZ.ToLocalTime(EncodeDateTime(2019, 12, 31, 23, 0, 0, 0));
  
  CheckEquals(0, CompareDateTime(EncodeDateTime(2020, 1, 1, 12, 0, 0, 0), LLocal), DateTimeToStr(LLocal));
end;

procedure TTZDBTest.Test_TZ_ToLocal_AtYearBoundary_WithNegativeOffset;
var
  LTZ: TBundledTimeZone;
  LLocal: TDateTime;
begin
  LTZ := TBundledTimeZone.GetTimeZone('America/Los_Angeles');
  LLocal := LTZ.ToLocalTime(EncodeDateTime(2020, 1, 1, 1, 0, 0, 0));
  
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 12, 31, 17, 0, 0, 0), LLocal), DateTimeToStr(LLocal));
end;

procedure TTZDBTest.Test_TZ_GetLocalTimeType_Validation;
const
  CTestZones: array[0..5] of string = ('Africa/Cairo', 'Africa/Cairo', 'Africa/Cairo', 'Europe/Dublin', 'Europe/London', 'America/Sao_Paulo');
  CTestYears: array[0..5] of Word = (1900, 2012, 2014, 2018, 2018, 2017);

var
  I: Integer;
  LTZ: TBundledTimeZone;
  LSegment: TYearSegment;
begin
  for I := 0 to Length(CTestZones) - 1 do
  begin
    LTZ := TBundledTimeZone.Create(CTestZones[I]);
    for LSegment in LTZ.GetYearBreakdown(CTestYears[I]) do
    begin
      CheckEquals(Ord(LSegment.LocalType), Ord(LTZ.GetLocalTimeType(LSegment.StartsAt)),
        'Segment start local time type discrepancy. Zone: "' + LTZ.ID + '"; year: ' + IntToStr(CTestYears[I]));
      CheckEquals(Ord(LSegment.LocalType), Ord(LTZ.GetLocalTimeType(LSegment.EndsAt)),
        'Segment end local time type discrepancy. Zone: "' + LTZ.ID + '"; year: ' + IntToStr(CTestYears[I]));
    end;
  end;
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Cairo_2014;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Africa/Cairo');
  LSegments := LTZ.GetYearBreakdown(2014);

  CheckEquals(9, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 5, 15, 23, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('EET', LSegments[0].DisplayName);
  CheckEquals(7200, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

   { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 5, 16, 0, 0, 0, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 5, 16, 0, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[1].LocalType));
  CheckEquals('EET', LSegments[1].DisplayName);
  CheckEquals(7200, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 5, 16, 1, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 6, 26, 22, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[2].LocalType));
  CheckEquals('EEST', LSegments[2].DisplayName);
  CheckEquals(10800, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 4 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 6, 26, 23, 0, 0, 0), LSegments[3].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 6, 26, 23, 59, 59, 999), LSegments[3].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[3].LocalType));
  CheckEquals('EEST', LSegments[3].DisplayName);
  CheckEquals(10800, LSegments[3].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 5 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 6, 27, 0, 0, 0, 0), LSegments[4].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 7, 31, 23, 59, 59, 999), LSegments[4].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[4].LocalType));
  CheckEquals('EET', LSegments[4].DisplayName);
  CheckEquals(7200, LSegments[4].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 5 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 8, 1, 0, 0, 0, 0), LSegments[5].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 8, 1, 0, 59, 59, 999), LSegments[5].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[5].LocalType));
  CheckEquals('EET', LSegments[5].DisplayName);
  CheckEquals(7200, LSegments[5].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 6 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 8, 1, 1, 0, 0, 0), LSegments[6].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 9, 25, 22, 59, 59, 999), LSegments[6].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[6].LocalType));
  CheckEquals('EEST', LSegments[6].DisplayName);
  CheckEquals(10800, LSegments[6].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 7 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 9, 25, 23, 0, 0, 0), LSegments[7].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 9, 25, 23, 59, 59, 999), LSegments[7].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[7].LocalType));
  CheckEquals('EEST', LSegments[7].DisplayName);
  CheckEquals(10800, LSegments[7].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 8 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 9, 26, 0, 0, 0, 0), LSegments[8].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 12, 31, 23, 59, 59, 999), LSegments[8].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[8].LocalType));
  CheckEquals('EET', LSegments[8].DisplayName);
  CheckEquals(7200, LSegments[8].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Dublin_2019;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('Europe/Dublin');
  LSegments := LTZ.GetYearBreakdown(2019);

  CheckEquals(5, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 3, 31, 0, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('GMT', LSegments[0].DisplayName);
  CheckEquals(0, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 3, 31, 1, 0, 0, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 3, 31, 1, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[1].LocalType));
  CheckEquals('GMT', LSegments[1].DisplayName);
  CheckEquals(3600, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 3, 31, 2, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 10, 27, 0, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[2].LocalType));
  CheckEquals('IST', LSegments[2].DisplayName);
  CheckEquals(3600, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 4 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 10, 27, 1, 0, 0, 0), LSegments[3].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 10, 27, 1, 59, 59, 999), LSegments[3].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[3].LocalType));
  CheckEquals('IST', LSegments[3].DisplayName);
  CheckEquals(3600, LSegments[3].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 5 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 10, 27, 2, 0, 0, 0), LSegments[4].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2019, 12, 31, 23, 59, 59, 999), LSegments[4].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[4].LocalType));
  CheckEquals('GMT', LSegments[4].DisplayName);
  CheckEquals(0, LSegments[4].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_Sao_Paulo_2014;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('America/Sao_Paulo');
  LSegments := LTZ.GetYearBreakdown(2014);

  CheckEquals(5, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 2, 15, 22, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[0].LocalType));
  CheckEquals('-02', LSegments[0].DisplayName);
  CheckEquals(-7200, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 2 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 2, 15, 23, 0, 0, 0), LSegments[1].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 2, 15, 23, 59, 59, 999), LSegments[1].EndsAt));
  CheckEquals(Ord(lttAmbiguous), Ord(LSegments[1].LocalType));
  CheckEquals('-02', LSegments[1].DisplayName);
  CheckEquals(-7200, LSegments[1].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 3 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 2, 16, 0, 0, 0, 0), LSegments[2].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 18, 23, 59, 59, 999), LSegments[2].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[2].LocalType));
  CheckEquals('-03', LSegments[2].DisplayName);
  CheckEquals(-10800, LSegments[2].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 4 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 19, 0, 0, 0, 0), LSegments[3].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 19, 0, 59, 59, 999), LSegments[3].EndsAt));
  CheckEquals(Ord(lttInvalid), Ord(LSegments[3].LocalType));
  CheckEquals('-03', LSegments[3].DisplayName);
  CheckEquals(-10800, LSegments[3].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});

  { Segment 5 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 10, 19, 1, 0, 0, 0), LSegments[4].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(2014, 12, 31, 23, 59, 59, 999), LSegments[4].EndsAt));
  CheckEquals(Ord(lttDaylight), Ord(LSegments[4].LocalType));
  CheckEquals('-02', LSegments[4].DisplayName);
  CheckEquals(-7200, LSegments[4].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
end;

procedure TTZDBTest.Test_TZ_GetYearBreakdown_UTC_9998;
var
  LTZ: TBundledTimeZone;
  LSegments: TYearSegmentArray;
begin
  LTZ := TBundledTimeZone.Create('UTC');
  LSegments := LTZ.GetYearBreakdown(9998);

  CheckEquals(1, Length(LSegments));

  { Segment 1 }
  CheckEquals(0, CompareDateTime(EncodeDateTime(9998, 1, 1, 0, 0, 0, 0), LSegments[0].StartsAt));
  CheckEquals(0, CompareDateTime(EncodeDateTime(9998, 12, 31, 23, 59, 59, 999), LSegments[0].EndsAt));
  CheckEquals(Ord(lttStandard), Ord(LSegments[0].LocalType));
  CheckEquals('UTC', LSegments[0].DisplayName);
  CheckEquals(0, LSegments[0].UtcOffset{$IFDEF DELPHI}.TotalSeconds{$ENDIF});
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

procedure TTZDBTest.Test_TZ_DbVersion;
var
  V: string;
begin
  V := TBundledTimeZone.DbVersion;

  CheckEquals(5, Length(V));
  CheckTrue(StrToInt(Copy(V, 1, 4)) >= 2019);

  CheckTrue((Ord(V[5]) >= Ord('a')) and (Ord(V[5]) <= Ord('z')));
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

procedure TTZDBTest.Test_TZ_ToLocal_Regression_1;
var
  LTZ: TBundledTimeZone;
  C: TDateTime;
begin
  LTZ := TBundledTimeZone.GetTimeZone('Europe/London');
  C := LTZ.ToLocalTime(EncodeDateTime(2018, 03, 25, 1, 0, 0, 0));

  CheckEquals('2018-03-25 02:00:00.000+01:00', LTZ.ToISO8601Format(C));
end;

procedure TTZDBTest.Test_TZ_ToLocal_Regression_2;
var
  LTZ: TBundledTimeZone;
  C: TDateTime;
begin
  LTZ := TBundledTimeZone.GetTimeZone('Europe/Bucharest');

  C := LTZ.ToLocalTime(EncodeDateTime(2010, 10, 31, 1, 0, 0, 0));
  CheckEquals('2010-10-31 03:00:00.000+02:00', LTZ.ToISO8601Format(C));

  C := LTZ.ToLocalTime(EncodeDateTime(2010, 10, 31, 2, 0, 0, 0));
  CheckEquals('2010-10-31 04:00:00.000+02:00', LTZ.ToISO8601Format(C));
end;

procedure TTZDBTest.Test_TZ_ToLocal_Regression_3;
var
  LTZ: TBundledTimeZone;
  C: TDateTime;
begin
  LTZ := TBundledTimeZone.GetTimeZone('Africa/Cairo');
  C := LTZ.ToLocalTime(EncodeDateTime(2010, 8, 10, 21, 0, 0, 0));

  CheckEquals('2010-08-10 23:00:00.000+02:00', LTZ.ToISO8601Format(C));
end;

procedure TTZDBTest.Test_TZ_Version;
begin
  CheckTrue(Length(TBundledTimeZone.Version) >= 7);
end;

procedure TTZDBTest.Test_TZ_ISO8601_Conversion;
var
  LTZ: TBundledTimeZone;
  LTimeZone: string;
  LDateTimeStr: string;
  LDateTime: TDateTime;
begin
  {
  When local daylight time is about to reach
  Sunday, 1 April 2018, 03:00:00 clocks are turned backward 1 hour to
  Sunday, 1 April 2018, 02:00:00 local standard time instead.
  }

  LTimeZone := 'Australia/Canberra';
  LTZ := TBundledTimeZone.Create(LTimeZone);
  try
    //Local time 2018-04-01 02:00:00  DST
    LDateTime  := EncodeDateTime(2018,3,31,15,00,00,0); //UTC Datetime
    LDateTimeStr := LTZ.ToISO8601Format(LDateTime);
    CheckEquals('2018-03-31 15:00:00.000+11:00', LDateTimeStr, LTimeZone);

    //Local time 2018-04-01 02:00:00  STD
    LDateTime  := EncodeDateTime(2018,3,31,16,00,00,0); //UTC Datetime
    LDateTimeStr := LTZ.ToISO8601Format(LDateTime);
    CheckEquals('2018-03-31 16:00:00.000+10:00', LDateTimeStr, LTimeZone);
  finally
   LTZ.Free;
  end;

  {
  When local daylight time is about to reach
  Sunday, 28 October 2018, 02:00:00 clocks are turned backward 1 hour to
  Sunday, 28 October 2018, 01:00:00 local standard time instead.
  }

  LTimeZone := 'Europe/London';
  LTZ := TBundledTimeZone.Create(LTimeZone);
  try
    //Local time 2018-10-28 02:00:00  DST
    LDateTime  := EncodeDateTime(2018,10,28,00,00,00,0); //UTC Datetime
    LDateTimeStr := LTZ.ToISO8601Format(LDateTime);
    CheckEquals('2018-10-28 00:00:00.000+01:00', LDateTimeStr, LTimeZone);

    //Local time 2018-10-28 02:00:00  STD
    LDateTime  := EncodeDateTime(2018,10,28,01,00,00,0); //UTC Datetime
    LDateTimeStr := LTZ.ToISO8601Format(LDateTime);
    CheckEquals('2018-10-28 01:00:00.000Z', LDateTimeStr, LTimeZone);
  finally
   LTZ.Free;
  end;

end;

{ TTZDBTimezoneTest }

function TTZDBTimezoneTest.RandomDate(AFromDatetime, AToDatetime: TDateTime): TDateTime;
var
  iFrom,
  iTo: Cardinal;
  iDay :Cardinal;
  fTime: Single;
begin
  Randomize;
  iFrom := Trunc(aFromDatetime);
  iTo := Trunc(aToDatetime);

  iDay := RandomRange(ifrom, ito);

  repeat
    fTime := Random;
    Result := iDay + fTime;
  until (Result >= AFromDatetime) and (Result <= AToDatetime);
end;

procedure TTZDBTimezoneTest.Setup;
begin
  inherited;
  FTZ := TBundledTimeZone.Create(fTimeZoneID);
end;

procedure TTZDBTimezoneTest.TearDown;
begin
  FTZ.Free;
  inherited;
end;

procedure TTZDBTimezoneTest.Test_AmbiguousTime;
var
  LType : TLocalTimeType;
  LAmbiguousDateTime: TDateTime;
begin
  LAmbiguousDateTime := RandomDate(fAmbStart, fAmbEnd);
  LType := FTZ.GetLocalTimeType(LAmbiguousDateTime);
  CheckEquals(ord(lttAmbiguous), Ord(LType), 'Expected local Ambiguous time type for: '+
    DateTimeToStr(LAmbiguousDateTime));
end;

procedure TTZDBTimezoneTest.Test_AmbiguousTimeEnd;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.AmbiguousTimeEnd(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fAmbEnd);
  CheckEquals(LExpDateTime, LActDateTime, 'AmbiguousTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_AmbiguousTimeStart;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.AmbiguousTimeStart(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fAmbStart);
  CheckEquals(LExpDateTime, LActDateTime, 'AmbiguousTimeStart');
end;

procedure TTZDBTimezoneTest.Test_DaylightTime;
var
  lType : TLocalTimeType;
begin
  lType := FTZ.GetLocalTimeType(fDstStart);
  CheckEquals(ord(lttDaylight), ord(lType), 'Expected local Daylight time type');
end;

procedure TTZDBTimezoneTest.Test_DaylightTimeEnd;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.DaylightTimeEnd(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fDstEnd);
  CheckEquals(LExpDateTime, LActDateTime, 'DaylightTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_DaylightTimeStart;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.DaylightTimeStart(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fDstStart);

  CheckEquals(LExpDateTime, LActDateTime, 'DaylightTimeStart');
end;

procedure TTZDBTimezoneTest.Test_InvalidTime;
var
  InvalidDt: TDateTime;
  lType : TLocalTimeType;
begin
  InvalidDt := RandomDate(fInvStart, fInvEnd);

  lType := FTZ.GetLocalTimeType(InvalidDt);
  CheckEquals(ord(lttInvalid), ord(lType), 'Expected local Invalid time type for:' +
    datetimetostr(InvalidDt));
end;

procedure TTZDBTimezoneTest.Test_InvalidTimeEnd;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.InvalidTimeEnd(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fInvEnd);
  CheckEquals(LExpDateTime, LActDateTime, 'InvalidTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_InvalidTimeStart;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.InvalidTimeStart(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fInvStart);

  CheckEquals(LExpDateTime, LActDateTime, 'InvalidTimeStart');
end;

procedure TTZDBTimezoneTest.Test_OperatesDST;
var
  OperatesdDST: Boolean;
begin
  OperatesdDST := FTZ.HasDaylightTime(fYear);
  CheckTrue(OperatesdDST, 'Has DaylightSaving:');
end;

procedure TTZDBTimezoneTest.Test_StandardTime;
var
  lType : TLocalTimeType;
begin
  lType := FTZ.GetLocalTimeType(fStdStart);
  CheckEquals(ord(lttStandard), ord(lType), 'Expected local Standard time type');
end;

procedure TTZDBTimezoneTest.Test_StandardTimeEnd;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.StandardTimeEnd(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fStdEnd);

  CheckEquals(LExpDateTime, LActDateTime, 'StandardTimeEnd');
end;

procedure TTZDBTimezoneTest.Test_StandardTimeStart;
var
  LExpDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  LActDateTime: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
begin
  DateTimeToString(LActDateTime, FMT_D_T_ISO, FTZ.StandardTimeStart(fYear));
  DateTimeToString(LExpDateTime, FMT_D_T_ISO, fStdStart);

  CheckEquals(LExpDateTime, LActDateTime, 'StandardTimeStart');
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
  fInvEnd   := 43380 +((1/86400)*10799);   //2018-10-07 02:59:59
  fDstStart := 43380 +((1/86400)*10800);   //2018-10-07 03:00:00


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
  RegisterTest(TTZDBTest{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TTZDB_St_Johns_2018_Test{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TTZDB_London_2018_Test{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TTZDB_Canberra_2018_Test{$IFNDEF FPC}.Suite{$ENDIF});
  RegisterTest(TTZDB_NewYork_2018_Test{$IFNDEF FPC}.Suite{$ENDIF});
end.
