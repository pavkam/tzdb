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

{$INCLUDE '../TZDBPK/Version.inc'}

unit TZStrs;
interface

procedure CLIError(const AMessage: string);
procedure CLIFatal(const AMessage: string);
procedure CLIMessage(const AMessage: string);

{ File format constants }

const
  CAbbDayNames: array[1..7] of string = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun');
  CAbbMonthNames: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

  CLastDoW = 'last';
  CNthDoW  = '>=';
  CPreDoW  = '<=';
  CMinDate = 'min';
  CMaxDate = 'max';
  COnlyDate = 'only';
  CCommentChar = '#';

  CLinkId = 'Link';
  CRuleId = 'Rule';
  CZoneId = 'Zone';

resourcestring
  CCLIHeader = 'TZ Compiler. (c) 2010-2019 Alexandru Ciobanu (alex+git@ciobanu.org). Part of TZDB project.';
  CCLIUsage  = 'USAGE: <TZCompile> input-dir output-file iana_db_version' + sLineBreak +
               '  input-dir      : The path to the directory containing tzinfo database files.' + sLineBreak +
               '  output-file    : The path to the INC file that will contain the converted data.' + sLineBreak +
               '  iana_db_version: The version of the IANA DB that is being compiled (e.g. 2019b).';

  CCLIError_Prefix    = 'ERROR:';
  CCLIFatal_Prefix    = 'FATAL:';
  CCLIMessage_Prefix  = 'NOTE:';

  { Main messages }
  CCLIBadInputDir     = 'Input directory "%s" does not exist or is inaccessible.';
  CCLIBadOutputDir    = 'Output file''s directory "%s" does not exist or is inaccessible.';
  CCLIBadVersion      = 'The supplied version string "%s" is invalid.';
  CCLIGlobalException = 'An unhandled exception was raised while processing files. Exception type is %s; and message is "%s".';

  { Processing messages }
  CPMBadMonthIdentifier = 'Found an invalid month identifier "%s".';
  CPMBadDoWIdentifier   = 'Found an invalid "day of week" identifier "%s".';
  CPMBadTimeOfDay       = 'Found an invalid "time of day" identifier "%s".';
  CPMBadHour            = 'Found an invalid hour value "%s" (in "time of day").';
  CPMBadMinute          = 'Found an invalid minute value "%s" (in "time of day").';
  CPMBadSecond          = 'Found an invalid second value "%s" (in "time of day").';
  CPMBadDoWInLast       = 'Found an invalid "day of week" "%s" (in "last occurence of" mode).';
  CPMBadDoWInNth        = 'Found an invalid "day of week" "%s" (in "Nth occurence of" mode).';
  CPMBadIndexInNth      = 'Found an invalid "after day index" "%s" (in "Nth occurence of" mode).';
  CPMBadIndexInFixed    = 'Found an invalid "day index" "%s" (in "fixed day" mode).';

  CPMBadRuleSplitCount  = 'Found a rule line with an invalid number of elements: %d.';
  CPMBadRuleFROMField   = 'Found an invalid FROM field (parse message was "%s") in rule [%s].';
  CPMBadRuleTOField     = 'Found an invalid TO field (parse message was "%s") in rule [%s].';
  CPMBadRuleINField     = 'Found an invalid IN field (parse message was "%s") in rule [%s].';
  CPMBadRuleONField     = 'Found an invalid ON field (parse message was "%s") in rule [%s].';
  CPMBadRuleATField     = 'Found an invalid AT field (parse message was "%s") in rule [%s].';
  CPMBadRuleSAVEField   = 'Found an invalid SAVE field (parse message was "%s") in rule [%s].';

  CPMBadZoneSplitCount  = 'Found a zone line with an invalid number of elements: %d.';
  CPMBadZoneGMTOFFField = 'Found an invalid GMTOFF field (parse message was "%s") in zone [%s].';
  CPMBadZoneUNTILFieldYear = 'Found an invalid UNTIL/year field (parse message was "%s") in zone [%s].';
  CPMBadZoneUNTILFieldMonth = 'Found an invalid UNTIL/year field (parse message was "%s") in zone [%s].';
  CPMBadZoneUNTILFieldDay = 'Found an invalid UNTIL/year field (parse message was "%s") in zone [%s].';
  CPMBadZoneUNTILFieldTime = 'Found an invalid UNTIL/year field (parse message was "%s") in zone [%s].';

  CPMBadLineSplitCount  = 'Found a link line with an invalid number of elements: %d.';
  CPMBadLineFROM = 'Found an invalid FROM field (empty or bad format).';
  CPMBadLineTO = 'Found an invalid TO field (empty or bad format).';

  CPMAliasExists = 'Alias "%s" already points to zone "%s". Cannot reassign to zone "%s".';
  CPMAddedAlias = 'Added new alias "%s" for zone "%s".';
  CPMAliasFailed = 'Failed to add alias "%s". The referenced time zone "%s" does not exist.';
  CPMAddedZone = 'Added new zone "%s".';
  CPMAddedRuleFamily = 'Added new rule family "%s".';
  CPMAddedRule = 'Added new rule for month %d, day [%d/%d/%d/%d], at %d, char "%s", offset %d and letters "%s".';
  CPMBadFile = 'Unable to parse "%s" line in file (%s). Skipping!';

  CPMStartedFile = 'Processing file "%s" ...';
  CPMStats = 'Processed %d rules; %d zones; %d day parts; %d unique rules; %d unique rule families; %d aliases.';
CPMStartDump = 'Dumping parsed contents to "%s" ...';

implementation

procedure CLIFatal(const AMessage: string);
begin
  Write(CCLIFatal_Prefix, ' ');
  WriteLn(AMessage);

  Halt(1);
end;

procedure CLIError(const AMessage: string);
begin
  Write(CCLIError_Prefix, ' ');
  WriteLn(AMessage);
end;

procedure CLIMessage(const AMessage: string);
begin
  Write(CCLIMessage_Prefix, ' ');
  WriteLn(AMessage);
end;

end.
