unit partedcommand;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, command;

type

  { TPartedCommand }

  TPartedCommand = class(TCommand)
   private
    FNickname: string;
    FHost: string;
    FChannel: string;
    FPartMessage: string;
    procedure Send;
   public
		procedure Execute(const ANickname, AHost, AChannel, APartMessage: String);
  end;



implementation

{ TPartedCommand }

procedure TPartedCommand.Send;
begin
	Channels.Parted(FNickname, FHost, FChannel, FPartMessage);
end;

procedure TPartedCommand.Execute(const ANickname, AHost, AChannel, APartMessage: String);
begin
	FNickname := ANickname;
  FHost := AHost;
  FChannel := AChannel;
  FPartMessage := APartMessage;
	Syncronize(@Send);
end;

end.

