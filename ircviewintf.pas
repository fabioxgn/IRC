unit IRCViewIntf;

{$mode objfpc}{$H+}

interface

type
	IIRCView = interface
		procedure UpdateTabCaption(Tab: TObject; ACaption: string);
		procedure UpdateNodeText(Node: TObject; AText: string);
		procedure ServerMessage(const AText: string);
		procedure NotifyChanged;
		function GetTab(const ACaption: string): TObject;
		function GetNode(const ACaption: string; ParentNode: TObject): TObject;
	end;

implementation

end.
