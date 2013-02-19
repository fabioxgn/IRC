unit TreeviewHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, strutils;

type

  { TTreeViewHelper }

  TTreeViewHelper = class helper for TTreeview
    procedure FilterNodes(const Text: string);
  end;

implementation

{ TTreeViewHelper }

procedure TTreeViewHelper.FilterNodes(const Text: string);
var
  Node: TTreeNode;
  I: Integer;
  S: string;
  FirstSelected: Boolean;
begin
  S := Trim(Text);
  FirstSelected := False;
  for Node in Items do
	 begin
    for I := 0 to Node.Count -1 do
    begin
      Node.Items[I].Visible := (S = '') or AnsiStartsText(S, Node.Items[I].Text);
      if not FirstSelected then
      begin
        Node.Items[I].Selected := True;
        FirstSelected := True;
      end;
    end;
  end;
end;

end.

