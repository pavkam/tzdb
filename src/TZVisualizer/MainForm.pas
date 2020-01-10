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

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TZDB, ExtCtrls, Decomposer, DateUtils, Generics.Collections,
  ComCtrls;

type
  TfrmMain = class(TForm)
    gbSelectTZ: TGroupBox;
    rbLocal: TRadioButton;
    rbSelected: TRadioButton;
    cbbSelectTZ: TComboBox;
    pChart: TPanel;
    edtYear: TEdit;
    Label1: TLabel;
    btGo: TButton;
    reInfo: TRichEdit;
    procedure rbLocalClick(Sender: TObject);
    procedure rbSelectedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btGoClick(Sender: TObject);
    procedure edtYearChange(Sender: TObject);
    procedure cbbSelectTZChange(Sender: TObject);
  private
    { Private declarations }

    procedure AdjustUI;
    procedure PrintPretty(const AList: TList<TDecomposedPeriod>);
    procedure WritePretty(const AString: string; const AColor: TColor; const AStyle: TFontStyles);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.AdjustUI;
var
  LYear: Integer;
begin
  cbbSelectTZ.Enabled := rbSelected.Checked;
  btGo.Enabled := TryStrToInt(edtYear.Text, LYear) and (LYear > 1800) and (LYear < 2100);

  if rbSelected.Checked then
    btGo.Enabled := btGo.Enabled and (cbbSelectTZ.Text <> '');
end;

procedure TfrmMain.btGoClick(Sender: TObject);
var
  LYear: Integer;
  LLt: TList<TDecomposedPeriod>;
begin
  ReportMemoryLeaksOnShutdown := true;
  LYear := StrToInt(edtYear.Text);

  if rbLocal.Checked then
    LLt := Decompose(TTimeZone.Local, LYear)
  else
  begin
    LLt := nil;

    if cbbSelectTZ.Text <> '' then
      LLt := Decompose(TBundledTimeZone.GetTimeZone(cbbSelectTZ.Text), LYear);
  end;

  { LLt should be initialized here // or not }
  if Assigned(LLt) then
  begin
    PrintPretty(LLt);
    LLt.Free;
  end;
end;

procedure TfrmMain.cbbSelectTZChange(Sender: TObject);
begin
  { Call the meta-action-update }
  AdjustUI();
end;

procedure TfrmMain.edtYearChange(Sender: TObject);
begin
  { Call the meta-action-update }
  AdjustUI();
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := true;

  { Load up all known time zones into the combobox. }
  cbbSelectTZ.Items.AddStrings(TBundledTimeZone.KnownTimeZones());

  { Call the meta-action-update }
  rbLocalClick(rbLocal);
  edtYear.Text := IntToStr(YearOf(Now));
end;

procedure TfrmMain.PrintPretty(const AList: TList<TDecomposedPeriod>);
const
  CTimeType: array[TLocalTimeType] of string = ('Standard', 'Daylight', 'Ambiguous', 'Invalid');
var
  I: Integer;
  LDec: TDecomposedPeriod;

function Name(const AName: string): string;
begin
  if AName = '' then
    Result := '<none>'
  else
    Result := AName;
end;

begin
  reInfo.Clear;

  for I := 0 to AList.Count - 1 do
  begin
    LDec := AList[I];

    case LDec.LocalType of
      lttStandard:
      begin
        { Initial }
        WritePretty(CTimeType[LDec.LocalType], clGreen, [fsBold, fsUnderline]);
        WritePretty(' time from ', clGreen, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.StartsAt), clGreen, [fsBold]);
        WritePretty(' to ', clGreen, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.EndsAt), clGreen, [fsBold]);
        WritePretty(':' + sLineBreak, clGreen, []);
        WritePretty('In this period the local time (wall clock) is not adjusted and follows the standard rules.' + sLineBreak, clGreen, [fsItalic]);

        { More }
        WritePretty('Time zone abbreviation: ', clGreen, []);
        WritePretty(Name(LDec.Abbreviation) + sLineBreak, clGreen, [fsBold]);

        WritePretty('Time zone display name: ', clGreen, []);
        WritePretty(Name(LDec.DisplayName) + sLineBreak, clGreen, [fsBold]);

        WritePretty('UTC bias (from universal time): ', clGreen, []);
        WritePretty(string(LDec.Bias) + sLineBreak, clGreen, [fsBold]);
        WritePretty(sLineBreak, clGreen, []);
      end;

      lttDaylight:
      begin
        { Initial }
        WritePretty(CTimeType[LDec.LocalType], clBlue, [fsBold, fsUnderline]);
        WritePretty(' time from ', clBlue, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.StartsAt), clBlue, [fsBold]);
        WritePretty(' to ', clBlue, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.EndsAt), clBlue, [fsBold]);
        WritePretty(':' + sLineBreak, clBlue, []);
        WritePretty('In this period the local time (wall clock) is adjusted by a specified amout of time (usually an hour). ' +
                    'It is considered "summer" time.' + sLineBreak, clBlue, [fsItalic]);

        { More }
        WritePretty('Time zone abbreviation: ', clBlue, []);
        WritePretty(Name(LDec.Abbreviation) + sLineBreak, clBlue, [fsBold]);

        WritePretty('Time zone display name: ', clBlue, []);
        WritePretty(Name(LDec.DisplayName) + sLineBreak, clBlue, [fsBold]);

        WritePretty('UTC bias (from universal time): ', clBlue, []);
        WritePretty(string(LDec.Bias) + sLineBreak, clBlue, [fsBold]);
        WritePretty(sLineBreak, clBlue, []);
      end;

      lttAmbiguous:
      begin
        { Initial }
        WritePretty(CTimeType[LDec.LocalType], clGray, [fsBold, fsUnderline]);
        WritePretty(' time from ', clGray, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.StartsAt), clGray, [fsBold]);
        WritePretty(' to ', clGray, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.EndsAt), clGray, [fsBold]);
        WritePretty(':' + sLineBreak, clGray, []);
        WritePretty('In this period the local time (wall clock) can be treated either as being in DST or as begin in standard mode.' + sLineBreak, clGray, [fsItalic]);

        { More }
        WritePretty('Time zone abbreviation: ', clGray, []);
        WritePretty('<depends on settings>' + sLineBreak, clGray, [fsBold]);

        WritePretty('Time zone display name: ', clGray, []);
        WritePretty('<depends on settings>' + sLineBreak, clGray, [fsBold]);

        WritePretty('UTC bias (from universal time): ', clGray, []);
        WritePretty('<depends on settings>' + sLineBreak, clGray, [fsBold]);
        WritePretty(sLineBreak, clGray, []);
      end;

      lttInvalid:
      begin
        { Initial }
        WritePretty(CTimeType[LDec.LocalType], clRed, [fsBold, fsUnderline]);
        WritePretty(' time from ', clRed, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.StartsAt), clRed, [fsBold]);
        WritePretty(' to ', clRed, []);
        WritePretty(FormatDateTime('yyyy-MM-dd hh:mm:ss.zzz', LDec.EndsAt), clRed, [fsBold]);
        WritePretty(':' + sLineBreak, clRed, []);
        WritePretty('In this period the local time (wall clock) is invalid. This hour does not "exist" in this time zone. ' +
                    'The clock should have been adjusted accordinly.' + sLineBreak, clRed, [fsItalic]);

        { More }
        WritePretty('Time zone abbreviation: ', clRed, []);
        WritePretty('<none>' + sLineBreak, clRed, [fsBold]);

        WritePretty('Time zone display name: ', clRed, []);
        WritePretty('<none>' + sLineBreak, clRed, [fsBold]);

        WritePretty('UTC bias (from universal time): ', clRed, []);
        WritePretty('<none>' + sLineBreak, clRed, [fsBold]);
        WritePretty(sLineBreak, clRed, []);
      end;
    end;
  end;
end;

procedure TfrmMain.rbLocalClick(Sender: TObject);
begin
  { Call the meta-action-update }
  AdjustUI();
end;

procedure TfrmMain.rbSelectedClick(Sender: TObject);
begin
  { Call the meta-action-update }
  AdjustUI();
end;

procedure TfrmMain.WritePretty(const AString: string; const AColor: TColor;
  const AStyle: TFontStyles);
begin
  reInfo.SelAttributes.Style := AStyle;
  reInfo.SelAttributes.Color := AColor;
  reInfo.SelText := AString;
end;

end.
