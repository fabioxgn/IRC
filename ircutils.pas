unit IRCUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIRCUtils }

  TIRCUtils = class
    class function RemoveOPVoicePrefix(const Username: string): string;
    class function IsOpVoice(const NickName: string): Boolean;
    class function IsOp(const NickName: string): Boolean;
    class function IsVoice(const NickName: string): Boolean;
  end;

implementation

{ TIRCUtils }

class function TIRCUtils.RemoveOPVoicePrefix(const Username: string): string;
begin
  if IsOpVoice(Username) then
    Result := Copy(Username, 2, MaxInt)
  else
    Result := Username;
end;

class function TIRCUtils.IsOpVoice(const NickName: string): Boolean;
begin
	Result := IsOp(NickName) or IsVoice(NickName);
end;

class function TIRCUtils.IsOp(const NickName: string): Boolean;
begin
	Result := NickName[1] = '@';
end;

class function TIRCUtils.IsVoice(const NickName: string): Boolean;
begin
	Result := NickName[1] = '+';
end;

end.

