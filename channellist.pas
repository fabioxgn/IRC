unit ChannelList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type

  { TUser }

  TUser = class
  private
    FNick: string;
    FDisplayNick: string;
    procedure SetNick(AValue: string);
  public
    Tab: TObject;
    Node: TObject;
    property Nick: string read FNick write SetNick;
    property DisplayNick: string read FDisplayNick;
    constructor Create(const NickName: string);
  end;

  { TUserList }

  TUserList = class(specialize TFPGObjectList<TUser>)
    function UserByNick(const NickName: string): TUser;
  end;

  { TChannel }

  TChannel = class
    Name: string;
    Tab: TObject;
    Node: TObject;
    Users: TUserList;
    constructor Create(const ChannelName: string);
    destructor Destroy; override;
  end;

  { TChannelList }

  TChannelList = class(specialize TFPGObjectList<TChannel>)
    function AutoComplete(const ChannelName: string; const SearchString: string): string;
    function ChannelByName(const Name: string): TChannel;
  end;



implementation

uses strutils;

{ TUserList }

function TUserList.UserByNick(const NickName: string): TUser;
begin
  for Result in Self do
    if Result.Nick = NickName then
      Exit;
  Result := nil;
end;

procedure TUser.SetNick(AValue: string);
begin
 if FNick = AValue then
   Exit;

 FNick := AValue;

 FDisplayNick :=  StringReplace(AValue, '@', '', []);
 FDisplayNick :=  StringReplace(FDisplayNick, '+', '', []);
end;

constructor TUser.Create(const NickName: string);
begin
  Nick := NickName;
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

function TChannelList.AutoComplete(const ChannelName: string; const SearchString: string): string;
var
  User: TUser;
  Channel: TChannel;
begin
  Result := '';
  for Channel in Self do
    if Channel.Name = ChannelName then
      for User in Channel.Users do
          if AnsiStartsText(SearchString, User.DisplayNick) then
             Exit(User.DisplayNick)
end;

function TChannelList.ChannelByName(const Name: string): TChannel;
begin
  for Result in Self do
    if AnsiCompareText(Result.Name, Name) = 0 then
      Exit;
  Result := nil;
end;

end.

