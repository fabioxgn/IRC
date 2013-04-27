unit joinedcommand;

{$mode objfpc}{$H+}

interface

uses command;

type

	{ TJoinedCommand }

	TJoinedCommand = class(TCommand)
	private
		FNickName: string;
		FHost: string;
		FChannel: string;
		procedure Send;
	public
		 procedure Execute(const ANickname, AHost, AChannel: string);
	end;

implementation

{ TJoinedCommand }

procedure TJoinedCommand.Send;
begin
	Channels.Joined(FNickName, FHost, FChannel);
end;

procedure TJoinedCommand.Execute(const ANickname, AHost, AChannel: string);
begin
	FNickName := ANickname;
	FHost := AHost;
	FChannel := AChannel;
	Syncronize(@Send);
end;

end.