unit ChannelList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, IRCViewIntf;

type

  { TUser }

  TUser = class
  private
    FNickNameInChannel: string;
    FNickName: string;
    procedure SetNickNameInChannel(AValue: string);
  public
    Tab: TObject;
    Node: TObject;
    property NickNameInChannel: string read FNickNameInChannel write SetNickNameInChannel;
    property NickName: string read FNickName;
    constructor Create(const ANickName: string);
  end;

  { TUserList }

  TUserList = class(specialize TFPGObjectList<TUser>)
    function UserByNick(const NickName: string): TUser;
  end;

  { TChannel }

  TChannel = class
  public
    Name: string;
    Tab: TObject;
    Node: TObject;
    Users: TUserList;
    constructor Create(const ChannelName: string);
    destructor Destroy; override;
  end;

  { TChannelList }

  TChannelList = class(specialize TFPGObjectList<TChannel>)
  private
    FView: IIRCView;
    FNickName: string;
  public
    constructor Create(View: IIRCView);
    function AutoComplete(const ChannelName: string; const SearchString: string): string;
    function ChannelByName(const Name: string): TChannel;
    procedure RemoveUserFromAllChannels(const NickName: string);
    procedure RemoveUserFromChannel(const ChannelName, Nickname: string);
    procedure NickNameChanged(const OldNickName, NewNickName: string);
    procedure Quit(const ANickName, AReason: string);
    procedure Parted(const ANickname, AHost, AChannel, APartMessage: string);
    procedure Joined(const ANickname, AHost, AChannel: string);
    property View: IIRCView read FView;
    property NickName: string read FNickName write FNickName;
  end;


implementation

uses strutils;

{ TUserList }

resourcestring
	StrQuit = '* %s %s';
  StrParted = '* Parted: ';
  StrJoined = '* Joined: ';


function TUserList.UserByNick(const NickName: string): TUser;
begin
  for Result in Self do
    if Result.NickName = NickName then
      Exit;
  Result := nil;
end;

procedure TUser.SetNickNameInChannel(AValue: string);
begin
 if FNickNameInChannel = AValue then
   Exit;

 FNickNameInChannel := AValue;

 FNickName :=  StringReplace(AValue, '@', '', []);
 FNickName :=  StringReplace(FNickName, '+', '', []);
end;

constructor TUser.Create(const ANickName: string);
begin
  NickNameInChannel := ANickName;
end;

{ TUserList }

constructor TChannel.Create(const ChannelName: string);
begin
  Name := ChannelName;
  Users := TUserList.Create;
end;

destructor TChannel.Destroy;
begin
  Users.Free;
  inherited Destroy;
end;

{ TChannelList }

constructor TChannelList.Create(View: IIRCView);
begin
  inherited Create;
	FView := View;
end;

function TChannelList.AutoComplete(const ChannelName: string; const SearchString: string): string;
var
  User: TUser;
  Channel: TChannel;
begin
  Result := '';
  for Channel in Self do
    if Channel.Name = ChannelName then
      for User in Channel.Users do
          if AnsiStartsText(SearchString, User.NickName) then
             Exit(User.NickName)
end;

function TChannelList.ChannelByName(const Name: string): TChannel;
begin
  for Result in Self do
    if AnsiCompareText(Result.Name, Name) = 0 then
      Exit;
  Result := nil;
end;

procedure TChannelList.RemoveUserFromAllChannels(const NickName: string);
var
 Channel: TChannel;
begin
 for Channel in Self do
   RemoveUserFromChannel(Channel.Name, NickName);
end;

procedure TChannelList.RemoveUserFromChannel(const ChannelName, Nickname: string);
var
  User: TUser;
  Channel: TChannel;
  RemoveChannel: Boolean;
begin
  Channel := ChannelByName(ChannelName);
  if Channel = nil then
     Exit;

  User := Channel.Users.UserByNick(Nickname);
  if (User <> nil) then
  begin
     RemoveChannel := User.NickNameInChannel = FNickName;
     User.Node.Free;
     Channel.Users.Extract(User).Free;
     if RemoveChannel then
     begin
				Channel.Node.Free;
        Channel.Tab.Free;
				Extract(Channel).Free;
     end;
     FView.NotifyChanged;
  end;
end;

procedure TChannelList.NickNameChanged(const OldNickName, NewNickName: string);
var
 User: TUser;
 Channel: TChannel;
begin
	for Channel in Self do
		for User in Channel.Users do
			if User.NickNameInChannel = OldNickName then
			begin
				User.NickNameInChannel := NewNickName;

				if (User.Tab <> nil) then
					FView.UpdateTabCaption(User.Tab, NewNickName);

        if (User.Node <> nil) then
        begin
					FView.UpdateNodeText(User.Node, NewNickName);
          FView.NotifyChanged;
        end;
      end;
end;

procedure TChannelList.Quit(const ANickName, AReason: string);
begin
  RemoveUserFromAllChannels(ANickName);
  FView.ServerMessage(Format(StrQuit, [ANickname, AReason]));
end;

procedure TChannelList.Parted(const ANickname, AHost, AChannel, APartMessage: String);
begin
  RemoveUserFromChannel(AChannel, ANickname);
	FView.ServerMessage(StrParted + ANickname + ' - ' + ' -' + AHost + ': ' + APartMessage + ' - ' + AChannel);
end;

procedure TChannelList.Joined(const ANickname, AHost, AChannel: string);
var
 	Channel: TChannel;
  User: TUser;
begin
  Channel := ChannelByName(AChannel);
  if Channel = nil then
  begin
		Channel := TChannel.Create(AChannel);
    Add(Channel);
    Channel.Tab := FView.GetTab(AChannel);
    Channel.Node := FView.GetNode(AChannel, nil);
  end;

  if NickName <> ANickname then
  begin
	  User := TUser.Create(ANickname);
  	Channel.Users.Add(User);
	  User.Node := FView.GetNode(User.NickNameInChannel, Channel.Node);
  end;

 	FView.ServerMessage(StrJoined + ANickname + ' - ' + AHost + ' - ' + AChannel);
  FView.NotifyChanged
end;

end.

