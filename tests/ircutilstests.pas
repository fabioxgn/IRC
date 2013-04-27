unit IRCUtilsTests;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpcunit, testutils, testregistry;

type

	TIRCUtilsTests = class(TTestCase)
	published
		procedure TestRemoveOPVoiceChars;
	end;

implementation

uses
	IRCUtils;

procedure TIRCUtilsTests.TestRemoveOPVoiceChars;
begin
	CheckEquals('Op', TIRCUtils.RemoveOPVoicePrefix('@Op'));
	CheckEquals('Voice', TIRCUtils.RemoveOPVoicePrefix('+Voice'));
	CheckEquals('User+@', TIRCUtils.RemoveOPVoicePrefix('User+@'));
end;



initialization
	RegisterTest(TIRCUtilsTests);

end.
