unit IRC;

{$mode objfpc}{$H+}

interface

uses
  Classes, IdIRC, IdComponent, IdContext;

type

    { TIRC }

    TOnChannelJoined = function(const Name: string): TStrings of object;
    TOnNickListReceived = procedure(const Channel: string; List: TStrings) of object;
    TOnUserEvent = procedure(const Channel, User: string) of object;

    TIRC = class
    private
      FLog: TStrings;
      FChannels: TStrings;
      FActiveChannel: string;
      FIdIRC: TIdIRC;
      FOnChannelJoined: TOnChannelJoined;
      FOnNickListReceived: TOnNickListReceived;
      FOnUserJoined: TOnUserEvent;
      FOnUserLeft: TOnUserEvent;
      FAutoJoinChannels: TStrings;
			FReady: Boolean;
      procedure AddChannelMessage(const Channel, Message: string);
      procedure ConfigureIdIRC;
      function GetUserName: string;
      function HighlightUserName(const AMessage: String): string;
      procedure MessageToChannel(const Message: string);
      procedure ReadConfig;
      procedure OnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
      procedure OnNotice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
      procedure OnMOTD(ASender: TIdContext; AMOTD: TStrings);
      procedure OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
      procedure OnPrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
      procedure OnNickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
      procedure OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
      procedure OnLeave(ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
      procedure OnWelcome(ASender: TIdContext; const AMsg: String);
    public
      property Log: TStrings read FLog write FLog;
      property ActiveChannel: string read FActiveChannel write FActiveChannel;
      property OnChannelJoined: TOnChannelJoined read FOnChannelJoined write FOnChannelJoined;
      property OnNickListReceived: TOnNickListReceived read FOnNickListReceived write FOnNickListReceived;
      property OnUserJoined: TOnUserEvent read FOnUserJoined write FOnUserJoined;
      property OnUserParted: TOnUserEvent read FOnUserLeft write FOnUserLeft;
      property Ready: Boolean read FReady;
      property UserName: string read GetUserName;
      procedure AutoJoinChannels;
      procedure Connect;
      procedure Disconnect;
      procedure JoinChannel(const Name: string);
      procedure LeaveChannel(const Name: string);
      procedure SendMessage(const Message: string);
      constructor Create;
      destructor Destroy; override;
    end;

implementation

uses config, sysutils;

resourcestring
  StrJoined = '* Joined: ';
  StrParted = '* Parted: ';

procedure TIRC.ReadConfig;
var
  C: TIRCConfig;
begin
  C := TIRCConfig.Create;
  try
    C.Load;

    FIdIRC.Host := C.Host;
    FIdIRC.Port := C.Port;
    FIdIRC.Username:= C.Username;
    FIdIRC.Nickname:= C.Nickname;
    FIdIRC.RealName:= C.RealName;
    FIdIRC.AltNickname := C.AltNickname;

    FAutoJoinChannels := TStringList.Create;
    FAutoJoinChannels.AddStrings(C.Channels);
  finally
    C.Free;
  end;
end;

procedure TIRC.ConfigureIdIRC;
begin
  FIdIRC := TIdIRC.Create(nil);
  FIdIRC.OnStatus:= @OnStatus;
  FIdIRC.OnNotice:= @OnNotice;
  FIdIRC.OnMOTD:= @OnMOTD;
  FIdIRC.OnPrivateMessage:= @OnPrivateMessage;
  FIdIRC.OnNicknamesListReceived:= @OnNickNameListReceive;
  FIdIRC.OnJoin := @OnJoin;
  FIdIRC.OnPart:= @OnLeave;
  FIdIRC.OnServerWelcome := @OnWelcome;
end;

function TIRC.GetUserName: string;
begin
  Result := FIdIRC.UsedNickname;
end;

function TIRC.HighlightUserName(const AMessage: String): string;
begin
  Result := StringReplace(AMessage, UserName, '<' + UserName + '>', [])
end;

procedure TIRC.AddChannelMessage(const Channel, Message: string);
begin
  (FChannels.Objects[FChannels.IndexOf(Channel)] as TStrings).Add(Message);
end;

procedure TIRC.AutoJoinChannels;
var
  I: Integer;
begin
  if FAutoJoinChannels = nil then
     Exit;

  for I := 0 to FAutoJoinChannels.Count -1 do
    JoinChannel(FAutoJoinChannels.ValueFromIndex[I]);
  FreeAndNil(FAutoJoinChannels);
end;

procedure TIRC.MessageToChannel(const Message: string);
begin
  FIdIRC.Say(FActiveChannel, Message);
  AddChannelMessage(FActiveChannel, FIdIRC.UsedNickname + ': ' + Message);
end;

procedure TIRC.OnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  FLog.Add(AStatusText);
  if AStatus = hsConnected then
    FReady := True;
end;

procedure TIRC.OnNotice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
begin
  //TODO: Mostrar de quem é e para quem é
  FLog.Add(ANotice);
end;

procedure TIRC.OnMOTD(ASender: TIdContext; AMOTD: TStrings);
begin
  FLog.AddStrings(AMOTD);
end;

procedure TIRC.OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
begin
  FLog.Add(AMessage);
end;

procedure TIRC.OnPrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
var
  Mensagem: string;
begin
  Mensagem := HighlightUserName(AMessage);
  AddChannelMessage(ATarget, '<' + ANickname +'>' + ': ' + AMessage);
end;

procedure TIRC.OnNickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
begin
  FOnNickListReceived(AChannel, ANicknameList);
end;

procedure TIRC.OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
begin
  if ANickname = FIdIRC.UsedNickname then
    Exit;

  FOnUserJoined(AChannel, ANickname);
  FLog.Add(StrJoined + ANickname + ' - ' + AHost + ' - ' + AChannel);
  AddChannelMessage(AChannel, StrJoined + ANickname);
end;

procedure TIRC.OnLeave(ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
begin
  FOnUserLeft(AChannel, ANickname);

  if ANickname = UserName then
     Exit;

  AddChannelMessage(AChannel, StrParted + ANickname + ' -' + APartMessage);
end;

procedure TIRC.OnWelcome(ASender: TIdContext; const AMsg: String);
begin
  FLog.Add(AMsg);
end;

procedure TIRC.Connect;
begin
  ReadConfig;
  FIdIRC.Connect;
end;

procedure TIRC.Disconnect;
begin
  FIdIRC.Disconnect;
end;

procedure TIRC.JoinChannel(const Name: string);
var
  SL: TStrings;
begin
  SL := FOnChannelJoined(Name);
  FChannels.AddObject(Name, SL);
  FIdIRC.Join(Name);
end;

procedure TIRC.LeaveChannel(const Name: string);
begin
  FIdIRC.Part(Name);
end;

procedure TIRC.SendMessage(const Message: string);
begin
  if FActiveChannel = '' then
     FIdIRC.Raw(Message)
  else
    MessageToChannel(Message);
end;

constructor TIRC.Create;
begin
  inherited;
  ConfigureIdIRC;
  FChannels := TStringList.Create;
end;

destructor TIRC.Destroy;
begin
  FIdIRC.Free;
  FChannels.Free;
  FAutoJoinChannels.Free;
  inherited;
end;

end.

