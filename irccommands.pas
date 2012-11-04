unit IRCCommands;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TIRCCommand }

  TAlias = array[0..1] of string;

  TIRCCommand = class
  private
    function FormatCommand(const Raw: string): string;
    function GetCommandFromInput(const RawInput: string): string;
    function RemoveSlash(const Value: string): string;
    function ReplaceAlias(const Command: string): string;
  public
    function GetRawCommand(const RawInput: string): string;
  end;

implementation

const
  CommandAlias: array[0..0] of TAlias = (
    ('J', 'JOIN')
  );

{ TIRCCommand }

function TIRCCommand.FormatCommand(const Raw: string): string;
var
  UserCommand, NewCommand: string;
begin
  UserCommand := GetCommandFromInput(Raw);
  NewCommand := ReplaceAlias(UpperCase(UserCommand));
  Result := StringReplace(Raw, UserCommand, NewCommand, []);
  Result := RemoveSlash(Result);
end;

function TIRCCommand.GetCommandFromInput(const RawInput: string): string;
var
  CommandEnd: Integer;
begin
  CommandEnd := Pos(' ', RawInput);
  if CommandEnd = 0 then
    CommandEnd := MaxInt;

  Result := Copy(RawInput, 1, CommandEnd -1)
end;

function TIRCCommand.RemoveSlash(const Value: string): string;
begin
  Result := StringReplace(Value, '/', '', []);
end;

function TIRCCommand.ReplaceAlias(const Command: string): string;
var
  I: Integer;

  function AddSlash(const Value: string): string;
  begin
    Result := '/' + Value;
  end;

  function Replace: string;
  begin
    Result := StringReplace(Command, AddSlash(CommandAlias[I][0]), AddSlash(CommandAlias[I][1]), []);
  end;

begin
  Result := Command;
  for I := Low(CommandAlias) to High(CommandAlias) do
  begin
    if Command <> AddSlash(CommandAlias[I][0]) then
      Continue;

    Exit(Replace);
  end;
end;

function TIRCCommand.GetRawCommand(const RawInput: string): string;
begin
  if Pos('/', RawInput) <> 1 then
    Exit('');

  Result := FormatCommand(RawInput);
end;

end.

