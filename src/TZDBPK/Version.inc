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
