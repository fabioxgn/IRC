unit UserTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ChannelList, fpcunit, testutils, testregistry;

type

  TUserTests= class(TTestCase)
  private
    FSUT: TUser;
  protected
  published
    procedure RemoveOPVoiceChars;
  end;

implementation

procedure TUserTests.RemoveOPVoiceChars;
begin
  FSUT := TUser.Create('@op');
  try
    CheckEquals('op', FSUT.DisplayNick, '@ char must be removed from OP nicks');
  finally
    FSUT.Free;
  end;

  FSUT := TUser.Create('+voice');
  try
    CheckEquals('voice', FSUT.DisplayNick, '+ char must be removed from voice nicks');
  finally
    FSUT.Free;
  end;
end;

initialization

  RegisterTest(TUserTests);
end.

