unit IRCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIRCUtils }

  TIRCUtils = class
    class function RemoveOPVoicePrefix(const Username: string): string;
  end;

implementation

{ TIRCUtils }

class function TIRCUtils.RemoveOPVoicePrefix(const Username: string): string;
begin
  if Username[1] in ['@', '+'] then
    Result := Copy(Username, 2, MaxInt)
  else
    Result := Username;
end;

end.

