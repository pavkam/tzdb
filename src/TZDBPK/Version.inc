
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
      {$IF CompilerVersion >= 14}
        {$UNDEF UNSUPPORTED_VERSION}
      {$IFEND}
      {$IF CompilerVersion >= 17}
        {$DEFINE SUPPORTS_INLINE}
      {$IFEND}
    {$IFEND}

    {$IF DECLARED(RTLVersion)}
      {$IF RTLVersion >= 20}
        {$DEFINE SUPPORTS_MONITOR}
        {$DEFINE SUPPORTS_TSTRINGS_OWNSOBJECTS}
      {$IFEND}
      {$IF RTLVersion >= 21}
        {$DEFINE SUPPORTS_TARRAY}
        {$DEFINE SUPPORTS_TTIMESPAN}
      {$IFEND}
      {$IF RTLVersion >= 22}
        {$DEFINE SUPPORTS_TTIMEZONE}
      {$IFEND}
    {$IFEND}

  {$ENDIF}
{$ENDIF}
{$IFDEF UNSUPPORTED_VERSION}
  {$MESSAGE ERROR 'TZDB requires at least Delphi 6 or FreePascal 2.0 to compile!'}
{$ENDIF}