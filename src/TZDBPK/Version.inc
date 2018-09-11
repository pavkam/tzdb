{$IFNDEF PASDOC}
{$DEFINE UNSUPPORTED_VERSION}
{$IFDEF FPC}
  {$IFNDEF VER1_0}
    {$MODE OBJFPC}
    {$DEFINE SUPPORTS_INLINE}
    {$UNDEF UNSUPPORTED_VERSION}
  {$ENDIF}
{$ELSE}
  {$IFDEF CONDITIONALEXPRESSIONS}

    {$IF DECLARED(CompilerVersion)}
      {$IF CompilerVersion >= 14} //Delphi 6
        {$UNDEF UNSUPPORTED_VERSION}
      {$IFEND}
      {$IF CompilerVersion >= 17} //Delphi 2005
        {$DEFINE SUPPORTS_INLINE}
      {$IFEND}
    {$IFEND}

    {$IF DECLARED(RTLVersion)}
      {$IF RTLVersion >= 20} //Delphi 2009
        {$DEFINE SUPPORTS_MONITOR}
        {$DEFINE SUPPORTS_TSTRINGS_OWNSOBJECTS}
        {$DEFINE SUPPORTS_GENERICS}
      {$IFEND}
      {$IF RTLVersion >= 21} //Delphi 2010
        {$DEFINE SUPPORTS_TARRAY}
        {$DEFINE SUPPORTS_TTIMESPAN}
      {$IFEND}
      {$IF RTLVersion >= 22} //Delphi XE
        {$DEFINE SUPPORTS_TTIMEZONE}
      {$IFEND}
    {$IFEND}

  {$ENDIF}
{$ENDIF}
{$IFDEF UNSUPPORTED_VERSION}
  {$MESSAGE ERROR 'TZDB requires at least Delphi 6 or FreePascal 2.0 to compile!'}
{$ENDIF}
{$ENDIF}
