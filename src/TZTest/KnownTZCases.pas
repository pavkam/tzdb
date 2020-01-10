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

unit KnownTZCases;
interface
uses
  SysUtils,
  DateUtils,
  TZDB;

type
  { Special record containing decomposed stuff }
  TDecomposedPeriod = record
    FStartsAt, FEndsAt: TDateTime;
    FType: TLocalTimeType;
    FAbbrv_AsDST,
     FAbbrv_AsSTD,
      FName_AsDST,
       FName_AsSTD: string;
    FBias_AsDST,
     FBias_AsSTD: Int64;
{$IFDEF FPC}
      class operator Equal(const ALeft, ARight: TDecomposedPeriod): Boolean;
{$ENDIF}
  end;

const
  CEurope_Bucharest_2010: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 40179; FEndsAt: 40265.1249999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 40265.1250115625; FEndsAt: 40265.1666666551; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 40265.1666782292; FEndsAt: 40482.1249999884; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EEST'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 40482.1250115625; FEndsAt: 40482.1666666551; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EET'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 40482.1666782292; FEndsAt: 40543.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200)
  );

const
  CEtc_GMT12_2010: array[0 .. 0] of TDecomposedPeriod = (
    (FStartsAt: 40179; FEndsAt: 40543.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT-12'; FAbbrv_AsSTD: 'GMT-12';
      FName_AsDST: '-12'; FName_AsSTD: '-12'; FBias_AsDST: -43200; FBias_AsSTD: -43200)
  );

const
  CEtc_GMTMin9_1991: array[0 .. 0] of TDecomposedPeriod = (
    (FStartsAt: 33239; FEndsAt: 33603.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+09'; FAbbrv_AsSTD: 'GMT+09';
      FName_AsDST: '+09'; FName_AsSTD: '+09'; FBias_AsDST: 32400; FBias_AsSTD: 32400)
  );

const
  CAfrica_Accra_1997: array[0 .. 0] of TDecomposedPeriod = (
    (FStartsAt: 35431; FEndsAt: 35795.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT'; FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'GMT'; FName_AsSTD: 'GMT'; FBias_AsDST: 0; FBias_AsSTD: 0)
  );

const
  CAmerica_Araguaina_1950: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 18264; FEndsAt: 18368.9999999884; FType: lttDaylight; FAbbrv_AsDST: 'GMT-02'; FAbbrv_AsSTD: 'GMT-02';
      FName_AsDST: '-02'; FName_AsSTD: '-02'; FBias_AsDST: -7200; FBias_AsSTD: -7200),
    (FStartsAt: 18369.0000115625; FEndsAt: 18369.0416666551; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT-02'; FAbbrv_AsSTD: 'GMT-03';
      FName_AsDST: '-02'; FName_AsSTD: '-03'; FBias_AsDST: -7200; FBias_AsSTD: -10800),
    (FStartsAt: 18369.0416782292; FEndsAt: 18597.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT-03'; FAbbrv_AsSTD: 'GMT-03';
      FName_AsDST: '-03'; FName_AsSTD: '-03'; FBias_AsDST: -10800; FBias_AsSTD: -10800),
    (FStartsAt: 18598.0000115625; FEndsAt: 18598.0416666551; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 18598.0416782292; FEndsAt: 18628.9999999884; FType: lttDaylight; FAbbrv_AsDST: 'GMT-02'; FAbbrv_AsSTD: 'GMT-02';
      FName_AsDST: '-02'; FName_AsSTD: '-02'; FBias_AsDST: -7200; FBias_AsSTD: -7200)
  );

const
  CAfrica_Cairo_2009: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 39814; FEndsAt: 39926.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 39927.0000115625; FEndsAt: 39927.0416666551; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 39927.0416782292; FEndsAt: 40045.9583333218; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EEST'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 40045.9583448958; FEndsAt: 40045.9999999884; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EET'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 40046.0000115625; FEndsAt: 40178.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200)
  );

const
  CAfrica_Cairo_2010: array[0 .. 8] of TDecomposedPeriod = (
    (FStartsAt: 40179; FEndsAt: 40297.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 40298.0000115625; FEndsAt: 40298.0416666551; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 40298.0416782292; FEndsAt: 40400.9583333218; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EEST'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 40400.9583448958; FEndsAt: 40400.9999999884; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EET'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 40401.0000115625; FEndsAt: 40430.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 40431.0000115625; FEndsAt: 40431.0416666551; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 40431.0416782292; FEndsAt: 40451.9583333218; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EEST'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 40451.9583448958; FEndsAt: 40451.9999999884; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EET'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 40452.0000115625; FEndsAt: 40543.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200)
  );

const
  CEurope_London_2018: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 43101; FEndsAt: 43184.0416666551; FType: lttStandard; FAbbrv_AsDST: 'GMT'; FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'GMT'; FName_AsSTD: 'GMT'; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 43184.0416782292; FEndsAt: 43184.0833333218; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 43184.0833448958; FEndsAt: 43401.0416666551; FType: lttDaylight; FAbbrv_AsDST: 'GMT+01'; FAbbrv_AsSTD: 'GMT+01';
      FName_AsDST: 'BST'; FName_AsSTD: 'BST'; FBias_AsDST: 3600; FBias_AsSTD: 3600),
    (FStartsAt: 43401.0416782292; FEndsAt: 43401.0833333218; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+01'; FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'BST'; FName_AsSTD: 'GMT'; FBias_AsDST: 3600; FBias_AsSTD: 0),
    (FStartsAt: 43401.0833448958; FEndsAt: 43465.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT'; FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'GMT'; FName_AsSTD: 'GMT'; FBias_AsDST: 0; FBias_AsSTD: 0)
  );

const
  CAmerica_St_Johns_2018: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 43101; FEndsAt: 43170.0833333218; FType: lttStandard; FAbbrv_AsDST: 'GMT-03:30'; FAbbrv_AsSTD: 'GMT-03:30';
      FName_AsDST: 'NST'; FName_AsSTD: 'NST'; FBias_AsDST: -12600; FBias_AsSTD: -12600),
    (FStartsAt: 43170.0833448958; FEndsAt: 43170.1249999884; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 43170.1250115625; FEndsAt: 43408.0416666551; FType: lttDaylight; FAbbrv_AsDST: 'GMT-02:30'; FAbbrv_AsSTD: 'GMT-02:30';
      FName_AsDST: 'NDT'; FName_AsSTD: 'NDT'; FBias_AsDST: -9000; FBias_AsSTD: -9000),
    (FStartsAt: 43408.0416782292; FEndsAt: 43408.0833333218; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT-02:30'; FAbbrv_AsSTD: 'GMT-03:30';
      FName_AsDST: 'NDT'; FName_AsSTD: 'NST'; FBias_AsDST: -9000; FBias_AsSTD: -12600),
    (FStartsAt: 43408.0833448958; FEndsAt: 43465.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT-03:30'; FAbbrv_AsSTD: 'GMT-03:30';
      FName_AsDST: 'NST'; FName_AsSTD: 'NST'; FBias_AsDST: -12600; FBias_AsSTD: -12600)
  );

const
  CAsia_Jerusalem_2005: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 38353; FEndsAt: 38443.0833333218; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'IST'; FName_AsSTD: 'IST'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 38443.0833448958; FEndsAt: 38443.1249999884; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 38443.1250115625; FEndsAt: 38634.0416666551; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'IDT'; FName_AsSTD: 'IDT'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 38634.0416782292; FEndsAt: 38634.0833333218; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'IDT'; FName_AsSTD: 'IST'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 38634.0833448958; FEndsAt: 38717.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'IST'; FName_AsSTD: 'IST'; FBias_AsDST: 7200; FBias_AsSTD: 7200)
  );

const
  CAsia_Jerusalem_2006: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 38718; FEndsAt: 38807.0833333218; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'IST'; FName_AsSTD: 'IST'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 38807.0833448958; FEndsAt: 38807.1249999884; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 38807.1250115625; FEndsAt: 38991.0416666551; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'IDT'; FName_AsSTD: 'IDT'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 38991.0416782292; FEndsAt: 38991.0833333218; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'IDT'; FName_AsSTD: 'IST'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 38991.0833448958; FEndsAt: 39082.9999999884; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'IST'; FName_AsSTD: 'IST'; FBias_AsDST: 7200; FBias_AsSTD: 7200)
  );



implementation

{$IFDEF FPC}
class operator TDecomposedPeriod.Equal(const ALeft, ARight: TDecomposedPeriod): Boolean;
begin
  Result :=
    (ALeft.FStartsAt = ARight.FStartsAt) and
    (ALeft.FEndsAt = ARight.FEndsAt) and
    (ALeft.FType = ARight.FType) and
    (ALeft.FAbbrv_AsDST = ARight.FAbbrv_AsDST) and
    (ALeft.FAbbrv_AsSTD = ARight.FAbbrv_AsSTD) and
    (ALeft.FName_AsDST = ARight.FName_AsDST) and
    (ALeft.FName_AsSTD = ARight.FName_AsSTD) and
    (ALeft.FBias_AsDST = ARight.FBias_AsDST) and
    (ALeft.FBias_AsSTD = ARight.FBias_AsSTD);
end;
{$ENDIF}

end.
