unit StringUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TStringUtils }

  TStringUtils = class
    class function GetWordAtCursor(const S: string; CursorPos: Integer): string;
  end;

implementation

{ TStringUtils }

class function TStringUtils.GetWordAtCursor(const S: string; CursorPos: Integer): string;
var
  StartPos, EndPos: Integer;
begin
  if (CursorPos = 0) or (CursorPos > Length(S)) then
    Exit('');

  for StartPos := CursorPos downto 1 do
    if S[StartPos-1]  = ' ' then
      break;

  for EndPos := CursorPos to Length(S) do
    if S[EndPos+1] = ' ' then
      break;

  Result := Copy(S, StartPos, EndPos - StartPos+1);
end;

end.

