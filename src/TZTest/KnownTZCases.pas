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
  end;

const
  CEurope_Bucharest_2010: array[0 .. 4] of TDecomposedPeriod = (
    (FStartsAt: 40179; FEndsAt: 40265.1249884259; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200),
    (FStartsAt: 40265.125; FEndsAt: 40265.1666550926; FType: lttInvalid; FAbbrv_AsDST: ''; FAbbrv_AsSTD: '';
      FName_AsDST: ''; FName_AsSTD: ''; FBias_AsDST: 0; FBias_AsSTD: 0),
    (FStartsAt: 40265.1666666667; FEndsAt: 40482.1249884259; FType: lttDaylight; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EEST'; FBias_AsDST: 10800; FBias_AsSTD: 10800),
    (FStartsAt: 40482.125; FEndsAt: 40482.1666550926; FType: lttAmbiguous; FAbbrv_AsDST: 'GMT+03'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST'; FName_AsSTD: 'EET'; FBias_AsDST: 10800; FBias_AsSTD: 7200),
    (FStartsAt: 40482.1666666667; FEndsAt: 40543.9999884259; FType: lttStandard; FAbbrv_AsDST: 'GMT+02'; FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET'; FName_AsSTD: 'EET'; FBias_AsDST: 7200; FBias_AsSTD: 7200)
  );

const
  CEtc_GMT12_2010: array[0 .. 0] of TDecomposedPeriod = (
    (FStartsAt: 40179; FEndsAt: 40543.9999884259; FType: lttStandard; FAbbrv_AsDST: 'GMT-12'; FAbbrv_AsSTD: 'GMT-12';
      FName_AsDST: '-12'; FName_AsSTD: '-12'; FBias_AsDST: -43200; FBias_AsSTD: -43200)
  );

const
  CEtc_GMTMin9_1991: array[0 .. 0] of TDecomposedPeriod = (
    (FStartsAt: 33239; FEndsAt: 33603.9999884259; FType: lttStandard; FAbbrv_AsDST: 'GMT+09'; FAbbrv_AsSTD: 'GMT+09';
      FName_AsDST: '+09'; FName_AsSTD: '+09'; FBias_AsDST: 32400; FBias_AsSTD: 32400)
  );


const
  CAfrica_Accra_1997: array[0 .. 0] of TDecomposedPeriod = (
    ( FStartsAt: 35431;
      FEndsAt: 35795.9999884259;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT';
      FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'GMT';
      FName_AsSTD: 'GMT';
      FBias_AsDST: 0;
      FBias_AsSTD: 0)
  );

const
  CAmerica_Araguaina_1950: array[0 .. 4] of TDecomposedPeriod = (
    ( FStartsAt: 18264;
      FEndsAt: 18369.0416550926;
      FType: lttDaylight;
      FAbbrv_AsDST: 'GMT-02';
      FAbbrv_AsSTD: 'GMT-02';
      FName_AsDST: '-02';
      FName_AsSTD: '-02';
      FBias_AsDST: -7200;
      FBias_AsSTD: -7200),

    ( FStartsAt: 18369.0416666667;
      FEndsAt: 18369.0833217593;
      FType: lttAmbiguous;
      FAbbrv_AsDST: 'GMT-02';
      FAbbrv_AsSTD: 'GMT-03';
      FName_AsDST: '-02';
      FName_AsSTD: '-03';
      FBias_AsDST: -7200;
      FBias_AsSTD: -10800),

    ( FStartsAt: 18369.0833333333;
      FEndsAt: 18597.9583217593;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT-03';
      FAbbrv_AsSTD: 'GMT-03';
      FName_AsDST: '-03';
      FName_AsSTD: '-03';
      FBias_AsDST: -10800;
      FBias_AsSTD: -10800),

    ( FStartsAt: 18597.9583333333;
      FEndsAt: 18597.9999884259;
      FType: lttInvalid;
      FAbbrv_AsDST: '';
      FAbbrv_AsSTD: '';
      FName_AsDST: '';
      FName_AsSTD: '';
      FBias_AsDST: 0;
      FBias_AsSTD: 0),

    ( FStartsAt: 18598;
      FEndsAt: 18628.9999884259;
      FType: lttDaylight;
      FAbbrv_AsDST: 'GMT-02';
      FAbbrv_AsSTD: 'GMT-02';
      FName_AsDST: '-02';
      FName_AsSTD: '-02';
      FBias_AsDST: -7200;
      FBias_AsSTD: -7200)
  );

const
  CAfrica_Cairo_2009: array[0 .. 4] of TDecomposedPeriod = (
    //0
    ( FStartsAt: 39814;
      FEndsAt: 39926.9999884259;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT+02';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET';
      FName_AsSTD: 'EET';
      FBias_AsDST: 7200;
      FBias_AsSTD: 7200),
    //1
    ( FStartsAt: 39927;
      FEndsAt: 39927.0416550926;
      FType: lttInvalid;
      FAbbrv_AsDST: '';
      FAbbrv_AsSTD: '';
      FName_AsDST: '';
      FName_AsSTD: '';
      FBias_AsDST: 0;
      FBias_AsSTD: 0),
    //2
    ( FStartsAt: 39927.0416666667;
      FEndsAt: 40045.9999884259;//9583217593;
      FType: lttDaylight;
      FAbbrv_AsDST: 'GMT+03';
      FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST';
      FName_AsSTD: 'EEST';
      FBias_AsDST: 10800;
      FBias_AsSTD: 10800),
    //3
    ( FStartsAt: 40046;//40045.9583333333;
      FEndsAt: 40046.0416550926;//40045.9999884259;
      FType: lttAmbiguous;
      FAbbrv_AsDST: 'GMT+03';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST';
      FName_AsSTD: 'EET';
      FBias_AsDST: 10800;
      FBias_AsSTD: 7200),
    //4
    ( FStartsAt: 40046.0416666667;
      FEndsAt: 40178.9999884259;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT+02';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET';
      FName_AsSTD: 'EET';
      FBias_AsDST: 7200;
      FBias_AsSTD: 7200)
  );

const
  CAfrica_Cairo_2010: array[0 .. 8] of TDecomposedPeriod = (
    //0
    ( FStartsAt: 40179;
      FEndsAt: 40297.9999884259;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT+02';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET';
      FName_AsSTD: 'EET';
      FBias_AsDST: 7200;
      FBias_AsSTD: 7200),
    //1
    ( FStartsAt: 40298;
      FEndsAt: 40298.0416550926;
      FType: lttInvalid;
      FAbbrv_AsDST: '';
      FAbbrv_AsSTD: '';
      FName_AsDST: '';
      FName_AsSTD: '';
      FBias_AsDST: 0;
      FBias_AsSTD: 0),
    //2
    ( FStartsAt: 40298.0416666667;
      FEndsAt: 40400.9999884259;
      FType: lttDaylight;
      FAbbrv_AsDST: 'GMT+03';
      FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST';
      FName_AsSTD: 'EEST';
      FBias_AsDST: 10800;
      FBias_AsSTD: 10800),
    //3
    ( FStartsAt: 40401;
      FEndsAt: 40401.0416550926;
      FType: lttAmbiguous;
      FAbbrv_AsDST: 'GMT+03';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST';
      FName_AsSTD: 'EET';
      FBias_AsDST: 10800;
      FBias_AsSTD: 7200),
    //4
    ( FStartsAt: 40401.0416666667;
      FEndsAt: 40430.9583217593;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT+02';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET';
      FName_AsSTD: 'EET';
      FBias_AsDST: 7200;
      FBias_AsSTD: 7200),
    //5
    ( FStartsAt: 40430.9583333333;
      FEndsAt: 40430.9999884259;
      FType: lttInvalid;
      FAbbrv_AsDST: '';
      FAbbrv_AsSTD: '';
      FName_AsDST: '';
      FName_AsSTD: '';
      FBias_AsDST: 0;
      FBias_AsSTD: 0),
    //6
    ( FStartsAt: 40431;
      FEndsAt: 40451.9999884259;//40451.9583217593;
      FType: lttDaylight;
      FAbbrv_AsDST: 'GMT+03';
      FAbbrv_AsSTD: 'GMT+03';
      FName_AsDST: 'EEST';
      FName_AsSTD: 'EEST';
      FBias_AsDST: 10800;
      FBias_AsSTD: 10800),
    //7
    ( FStartsAt: 40452;
      FEndsAt: 40452.0416550926;//40451.9999884259;
      FType: lttAmbiguous;
      FAbbrv_AsDST: 'GMT+03';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EEST';
      FName_AsSTD: 'EET';
      FBias_AsDST: 10800;
      FBias_AsSTD: 7200),
    //8
    ( FStartsAt: 40452.0416666667;//40452;
      FEndsAt: 40543.9999884259;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT+02';
      FAbbrv_AsSTD: 'GMT+02';
      FName_AsDST: 'EET';
      FName_AsSTD: 'EET';
      FBias_AsDST: 7200;
      FBias_AsSTD: 7200)
  );
  const

  CEurope_London_2018: array[0 .. 4] of TDecomposedPeriod = (
    //0
    ( FStartsAt: 43101;
      FEndsAt: 43184 +((1/86400)*3599);
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT';
      FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'GMT';
      FName_AsSTD: 'GMT';
      FBias_AsDST: 0;
      FBias_AsSTD: 0),
    //1
    ( FStartsAt: 43184 +((1/86400)*3600);
      FEndsAt: 43184 +((1/86400)*7199);
      FType: lttInvalid;
      FAbbrv_AsDST: '';
      FAbbrv_AsSTD: '';
      FName_AsDST: '';
      FName_AsSTD: '';
      FBias_AsDST: 0;
      FBias_AsSTD: 0),
    //2
    ( FStartsAt: 43184 +((1/86400)*7200);
      FEndsAt: 43401 +((1/86400)*3599);
      FType: lttDaylight;
      FAbbrv_AsDST: 'GMT+01';
      FAbbrv_AsSTD: 'GMT+01';
      FName_AsDST: 'BST';
      FName_AsSTD: 'BST';
      FBias_AsDST: 3600;
      FBias_AsSTD: 3600),
    //3
    ( FStartsAt: 43401 +((1/86400)*3600);
      FEndsAt: 43401 +((1/86400)*7199);
      FType: lttAmbiguous;
      FAbbrv_AsDST: 'GMT+01';
      FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'BST';
      FName_AsSTD: 'GMT';
      FBias_AsDST: 3600;
      FBias_AsSTD: 0),
    //4
    ( FStartsAt: 43401 +((1/86400)*7200);
      FEndsAt: 43465.9999884259;
      FType: lttStandard;
      FAbbrv_AsDST: 'GMT';
      FAbbrv_AsSTD: 'GMT';
      FName_AsDST: 'GMT';
      FName_AsSTD: 'GMT';
      FBias_AsDST: 0;
      FBias_AsSTD: 0)
  );

implementation

end.
