unit command;

{$mode objfpc}{$H+}

interface

uses
	ChannelList, IdSync;

type

	{ TCommand }

	TCommandProc = procedure of object;

	 TCommand = class
	private
			FChannels: TChannelList;
	protected
		procedure Syncronize(AMethod: TCommandProc);
		property Channels: TChannelList read FChannels;
	public
		 constructor Create(AServer: TChannelList);
	end;

implementation

{ TCommand }

procedure TCommand.Syncronize(AMethod: TCommandProc);
begin
	 TIdSync.SynchronizeMethod(AMethod);
end;

constructor TCommand.Create(AServer: TChannelList);
begin
	 FChannels := AServer;
end;

end.
