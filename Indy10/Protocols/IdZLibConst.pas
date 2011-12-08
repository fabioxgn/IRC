
{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{  Copyright (c) 1997-1999 Borland Software Corporation }
{                                                       }
{*******************************************************}

unit IdZLibConst;

interface

{$i IdCompilerDefines.inc}

{$UNDEF STATICLOAD_ZLIB}
{$IFNDEF FPC}
  {$IFDEF WIN32_OR_WIN64_OR_WINCE}
    {$DEFINE STATICLOAD_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFNDEF STATICLOAD_ZLIB}
uses
  IdException;
{$ENDIF}

resourcestring
  sTargetBufferTooSmall = 'ZLib error: target buffer may be too small';
  sInvalidStreamOp = 'Invalid stream operation';

  sZLibError = 'ZLib Error (%d)';

  {$IFNDEF STATICLOAD_ZLIB}
  RSZLibCallError = 'Error on call to ZLib library function %s';
  {$ENDIF}

implementation

end.
 
