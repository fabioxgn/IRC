unit StringUtilsTests;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, fpcunit, testutils, testregistry;

type

	TStringUtilsTests = class(TTestCase)
	published
		procedure TestGetWordAtPosition;
	end;

implementation

uses StringUtils;

procedure TStringUtilsTests.TestGetWordAtPosition;
const
	Phrase = 'Write your own test';
begin
	CheckEquals('', TStringUtils.GetWordAtCursor(Phrase, 0));
	CheckEquals('Write', TStringUtils.GetWordAtCursor(Phrase, 3));
	CheckEquals('Write', TStringUtils.GetWordAtCursor(Phrase, 5));
	CheckEquals('your', TStringUtils.GetWordAtCursor(Phrase, 10));
	CheckEquals('own', TStringUtils.GetWordAtCursor(Phrase, 13));
end;



initialization

	RegisterTest(TStringUtilsTests);
end.
