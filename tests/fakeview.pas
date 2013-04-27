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
		function GetTab(const ACaption: string): TObject;
		function GetNode(const ACaption: string; ParentNode: TObject): TObject;
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

function TFakeView.GetTab(const ACaption: string): TObject;
begin

end;

function TFakeView.GetNode(const ACaption: string; ParentNode: TObject): TObject;
begin

end;

end.