unit fakeview;

{$mode objfpc}{$H+}

interface

uses
	IRCViewIntf;

type

	{ TFakeView }

	TFakeView = class(TInterfacedObject, IIRCView)
		procedure ServerMessage(const AText: string);
	  procedure UpdateNodeText(Node: TObject; AText: string);
	  procedure UpdateTabCaption(Tab: TObject; ACaption: string);
	  procedure NotifyChanged;
	  function GetChannelNode(const AChannel: string): TObject;
	  function GetChannelTab(const AChannel: string): TObject;
	end;

implementation

{ TFakeView }

procedure TFakeView.ServerMessage(const AText: string);
begin

end;

procedure TFakeView.UpdateNodeText(Node: TObject; AText: string);
begin

end;

procedure TFakeView.UpdateTabCaption(Tab: TObject; ACaption: string);
begin

end;

procedure TFakeView.NotifyChanged;
begin

end;

function TFakeView.GetChannelNode(const AChannel: string): TObject;
begin

end;

function TFakeView.GetChannelTab(const AChannel: string): TObject;
begin

end;

end.

