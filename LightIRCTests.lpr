program LightIRCTests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, IRCCommandTests, IRCCommands,
  ChannelListTests, ChannelList, StringUtils, StringUtilsTests, UserTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

