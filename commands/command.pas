unit command;

{$mode objfpc}{$H+}

interface

uses
  ChannelList;

type

{ TCommand }

	TCommand = class
  private
		FChannels: TChannelList;
   protected
     property Channels: TChannelList read FChannels;
  public
	  constructor Create(AServer: TChannelList);
  end;

implementation

{ TCommand }

constructor TCommand.Create(AServer: TChannelList);
begin
	FChannels := AServer;
end;

end.

