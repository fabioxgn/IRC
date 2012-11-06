unit channellisttests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ChannelList;

type

  { TChannelListTests }

  TChannelListTests= class(TTestCase)
  private
    FSUT: TChannelList;
    procedure Add2ChannelsWith2UsersEach;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AutoCompleteNickNames;
    procedure ChannelByName;
    procedure UserByNick;
  end;

implementation

{ TChannelListTests }

const
  StrChannel1 = '#channel1';
  StrChannel2 = '#channel2';
  StrUser1Channel1 = '@User1C1';
  StrUser2Channel1 = '+User2C1';
  StrUser1Channel2 = '@User1C2';
  StrUser2Channel2 = '+User2C2';


procedure TChannelListTests.Add2ChannelsWith2UsersEach;
var
 Channel2: TChannel;
 Channel1: TChannel;
begin
 Channel1 := TChannel.Create(StrChannel1);
 FSUT.Add(Channel1);

 Channel2 := TChannel.Create(StrChannel2);
 FSUT.Add(Channel2);

 Channel1.Users.Add(TUser.Create(StrUser1Channel1));
 Channel1.Users.Add(TUser.Create(StrUser2Channel1));
 Channel2.Users.Add(TUser.Create(StrUser1Channel2));
 Channel2.Users.Add(TUser.Create(StrUser2Channel2));
end;

procedure TChannelListTests.SetUp;
begin
 inherited SetUp;
 FSUT := TChannelList.Create;
end;

procedure TChannelListTests.TearDown;
begin
 FreeAndNil(FSUT);
 inherited TearDown;
end;

procedure TChannelListTests.AutoCompleteNickNames;
begin
  Add2ChannelsWith2UsersEach;

  CheckEquals(StrUser1Channel1, FSUT.AutoComplete(StrChannel1, 'User'));
  CheckEquals('', FSUT.AutoComplete(StrChannel1, 'User1C2'));
  CheckEquals('', FSUT.AutoComplete(StrChannel1, 'User3'));
end;

procedure TChannelListTests.ChannelByName;
const
  StrChannel1CamelCase = '#Channel1';
var
  Channel: TChannel;
begin
  Channel := TChannel.Create(StrChannel1);
  FSUT.Add(Channel);

  CheckTrue(Channel = FSUT.ChannelByName(StrChannel1), 'Invalid channel');
  CheckTrue(Channel = FSUT.ChannelByName(StrChannel1CamelCase), 'Search should be case insensitive');
  CheckFalse(Assigned(FSUT.ChannelByName(StrChannel2)), 'Shouldnt be assigned');
end;

procedure TChannelListTests.UserByNick;
const
  StrUser1 = 'User1';
var
  UserList: TUserList;
  User1: TUser;
begin
  UserList := TUserList.Create;
  try
    User1 := TUser.Create(StrUser1);
    UserList.Add(User1);
    UserList.Add(TUser.Create('User2'));

    CheckTrue(User1 = UserList.UserByNick(StrUser1), 'User not found');
    CheckFalse(Assigned(UserList.UserByNick('NonUser')), 'User found');
  finally
    UserList.Free;
  end;
end;

initialization
  RegisterTest(TChannelListTests);

end.

