unit IRCViewIntf;

{$mode objfpc}{$H+}

interface

type
	IIRCView = interface
	  procedure UpdateTabCaption(Tab: TObject; ACaption: string);
	  procedure UpdateNodeText(Node: TObject; AText: string);
  end;

implementation

end.

