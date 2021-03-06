unit quitcommand;

{$mode objfpc}{$H+}

interface

uses
	command;

type

	{ TQuitCommand }

	TQuitCommand = class(TCommand)
	private
		 FNickName: string;
		 FReason: string;
			procedure Send;
	public
			procedure Execute(const ANickName, AReason: string);
	end;


implementation

{ TQuitCommand }

procedure TQuitCommand.Send;
begin
	Channels.Quit(FNickName, FReason);
end;

procedure TQuitCommand.Execute(const ANickName, AReason: string);
begin
	FNickName := ANickName;
	FReason := AReason;
	Syncronize(@Send);
end;

end.
