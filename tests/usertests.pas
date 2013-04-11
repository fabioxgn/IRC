unit UserTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ChannelList, fpcunit, testutils, testregistry;

type

  { TUserTests }

  TUserTests= class(TTestCase)
  private
    FSUT: TUser;
  published
    procedure RemoveOPVoiceChars;
    procedure MaintainOpVoicePrefixInChannel;
  end;

implementation

procedure TUserTests.RemoveOPVoiceChars;
begin
  FSUT := TUser.Create('@op');
  try
    CheckEquals('op', FSUT.NickName, '@ char must be removed from OP nicks');
  finally
    FSUT.Free;
  end;

  FSUT := TUser.Create('+voice');
  try
    CheckEquals('voice', FSUT.NickName, '+ char must be removed from voice nicks');
  finally
    FSUT.Free;
  end;
end;

procedure TUserTests.MaintainOpVoicePrefixInChannel;
begin
	FSUT := TUser.Create('@op');
  try
	  FSUT.NickName := 'nick';
	  CheckEquals('@nick', FSUT.NickNameInChannel);
  finally
    FSUT.Free;
  end;

  FSUT := TUser.Create('+voice');
  try
		FSUT.NickName := 'myvoice';
	  CheckEquals('+myvoice', FSUT.NickNameInChannel);
  finally
  	FSUT.Free;
  end;
end;

initialization

  RegisterTest(TUserTests);
end.

