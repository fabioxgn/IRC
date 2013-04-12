unit idircconfig;

{$mode objfpc}{$H+}

interface

uses
	IdIRC, Classes;

type

{ TIdIRCConfig }

 TIdIRCConfig = class
    class procedure Configure(IdIRC: TIdIRC; AutoJoinChannels: TStrings);
    class procedure ConfigureEncoding(IdIRC: TIdIRC);
  end;

implementation

uses config, IdGlobal;

{ TIdIRCConfig }

class procedure TIdIRCConfig.Configure(IdIRC: TIdIRC; AutoJoinChannels: TStrings);
var
  C: TIRCConfig;
begin
  C := TIRCConfig.Create;
  try
    C.Load;

    IdIRC.Host := C.Host;
    IdIRC.Port := C.Port;
    IdIRC.Username:= C.Username;
    IdIRC.Nickname:= C.Nickname;
    IdIRC.RealName:= C.RealName;
    IdIRC.AltNickname := C.AltNickname;

    AutoJoinChannels.Clear;
    AutoJoinChannels.AddStrings(C.Channels);
  finally
    C.Free;
  end;
end;

class procedure TIdIRCConfig.ConfigureEncoding(IdIRC: TIdIRC);
begin
  IdIRC.IOHandler.DefStringEncoding := TIdTextEncoding.UTF8;
  IdIRC.IOHandler.DefAnsiEncoding := TIdTextEncoding.UTF8;
end;

end.

