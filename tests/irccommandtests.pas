unit IRCCommandTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IRCCommands, fpcunit, testutils, testregistry;

type

  { TIRCCommandsTests }

  TIRCCommandsTests= class(TTestCase)
  private
    FSUT: TIRCCommand;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure ConvertToRaw;
    procedure JoinAliasJ;
  end;

implementation

procedure TIRCCommandsTests.ConvertToRaw;
begin
  CheckEquals('JOIN #ubuntu', FSUT.GetRawCommand('/join #ubuntu'));
  CheckEquals('MSG fsapo', FSUT.GetRawCommand('/msg fsapo'));
  CheckEquals('', FSUT.GetRawCommand('Not a command'));
end;

procedure TIRCCommandsTests.JoinAliasJ;
begin
  CheckEquals('JOIN #ubuntu', FSUT.GetRawCommand('/j #ubuntu'));
end;

procedure TIRCCommandsTests.SetUp;
begin
  FSUT.Create;
end;

procedure TIRCCommandsTests.TearDown;
begin
  FSUT.Free;
end;

initialization

  RegisterTest(TIRCCommandsTests);
end.

