program LightIRCTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, IRCCommandTests, IRCCommands,
  ChannelListTests, ChannelList, StringUtils, StringUtilsTests, UserTests,
  ircutilstests, IrcUtils, fakeview;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

