unit IRC;

{$mode objfpc}{$H+}

interface

uses
  Classes, IdIRC, IdComponent, IdContext, IRCCommands;

type

    { TIRC }

    TOnChannelJoined = procedure(const Channel: string) of object;
    TOnNickListReceived = procedure(const Channel: string; List: TStrings) of object;
    TOnUserEvent = procedure(const Channel, User: string) of object;
    TOnMessageReceived = procedure(const Channel, Message: string) of object;

    TIRC = class
    private
      FReady: Boolean;
      FChannel: string;
      FMessage: string;
      FNickName: string;
      FServerMessage: string;
      FServerMessages: TStrings;
      FLog: TStrings;
      FNickNameList: TStrings;
      FActiveChannel: string;
      FIdIRC: TIdIRC;
      FOnChannelJoined: TOnChannelJoined;
      FOnNickListReceived: TOnNickListReceived;
      FOnUserJoined: TOnUserEvent;
      FOnUserLeft: TOnUserEvent;
      FOnMessageReceived: TOnMessageReceived;
      FAutoJoinChannels: TStrings;
      FCommands: TIRCCommand;
      procedure ConfigureEncoding;
      procedure ConfigureIdIRC;
      function FormatarMensagem(const NickName, Message: string): string;
      function GetConnected: Boolean;
      function GetUserName: string;
      function HighlightUserName(const AMessage: String): string;
      function IsInputCommand(const Message: string): Boolean;
      procedure MessageToChannel(const Message: string);
      procedure MessageReceived(const Channel, Message: string);
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
      function RemoveOPVoicePrefix(const Channel: string): string;
      procedure SendMessage;
      procedure SendChannelJoined;
      procedure SendNickNameListReceived;
      procedure SendParted;
      procedure SendServerMessage;
      procedure SendUserJoined;
    public
      property Log: TStrings read FLog write FLog;
      property Ready: Boolean read FReady;
      property ActiveChannel: string read FActiveChannel write FActiveChannel;
      property OnChannelJoined: TOnChannelJoined read FOnChannelJoined write FOnChannelJoined;
      property OnNickListReceived: TOnNickListReceived read FOnNickListReceived write FOnNickListReceived;
      property OnUserJoined: TOnUserEvent read FOnUserJoined write FOnUserJoined;
      property OnUserParted: TOnUserEvent read FOnUserLeft write FOnUserLeft;
      property OnMessageReceived: TOnMessageReceived read FOnMessageReceived write FOnMessageReceived;
      property UserName: string read GetUserName;
      property Connected: Boolean read GetConnected;
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

uses config, IdSync, IdGlobal, sysutils;

resourcestring
  StrJoined = '* Joined: ';
  StrParted = '* Parted: ';

const
  NickNameFormat = '<%s>';
  MessageFormat = NickNameFormat + ': %s';

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
  FIdIRC.OnRaw := @OnRaw;
end;

procedure TIRC.ConfigureEncoding;
begin
 FIdIRC.IOHandler.DefStringEncoding := TIdTextEncoding.Default;
end;

function TIRC.FormatarMensagem(const NickName, Message: string): string;
begin
  Result := Format(MessageFormat, [NickName, Message]);
end;

function TIRC.GetConnected: Boolean;
begin
  Result := FIdIRC.Connected;
end;

function TIRC.GetUserName: string;
begin
  Result := FIdIRC.UsedNickname;
end;

function TIRC.HighlightUserName(const AMessage: String): string;
begin
  Result := StringReplace(AMessage, UserName, Format(NickNameFormat, [UserName]), [])
end;

function TIRC.IsInputCommand(const Message: string): Boolean;
begin
  Result := Pos('/', TrimLeft(Message)) = 1;
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
var
  Channel: string;
begin
  Channel := RemoveOPVoicePrefix(FActiveChannel);
  FIdIRC.Say(Channel, Message);
  MessageReceived(FActiveChannel, FormatarMensagem(UserName, Message))
end;

procedure TIRC.MessageReceived(const Channel, Message: string);
begin
  FChannel := Channel;
  FMessage := Message;
  TIdSync.SynchronizeMethod(@SendMessage);
end;

procedure TIRC.OnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  if AStatus = hsConnected then
    FReady := True;

  FServerMessage := AStatusText;
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.OnNotice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
begin
  FServerMessage :=  Format('* Notice from %s to %s: %s ', [ANicknameFrom, ANicknameTo, ANotice]);
  SendServerMessage;
end;

procedure TIRC.OnMOTD(ASender: TIdContext; AMOTD: TStrings);
begin
  FServerMessages := AMOTD;
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
begin
  FServerMessage := AMessage;
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.OnPrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
var
  Mensagem: string;
begin
  Mensagem := HighlightUserName(AMessage);
  Mensagem := FormatarMensagem(ANickname, Mensagem);

  if ATarget <> UserName then
    MessageReceived(ATarget, Mensagem)
  else
    MessageReceived(ANickname, Mensagem);
end;

procedure TIRC.OnNickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
begin
  FChannel := AChannel;
  FNickNameList := ANicknameList;
  TIdSync.SynchronizeMethod(@SendNickNameListReceived);
end;

procedure TIRC.OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
begin
  if ANickname = FIdIRC.UsedNickname then
  begin
    FChannel := AChannel;
    TIdSync.SynchronizeMethod(@SendChannelJoined);
    Exit;
  end;

  FChannel := AChannel;
  FNickName := ANickname;
  TIdSync.SynchronizeMethod(@SendUserJoined);

  FServerMessage := StrJoined + ANickname + ' - ' + AHost + ' - ' + AChannel;
  TIdSync.SynchronizeMethod(@SendServerMessage);

  MessageReceived(AChannel, StrJoined + ANickname);
end;

procedure TIRC.OnLeave(ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
begin
  FChannel := AChannel;
  FNickName := ANickname;
  TIdSync.SynchronizeMethod(@SendParted);
  SendParted;

  if ANickname = UserName then
     Exit;

  MessageReceived(AChannel, StrParted + ANickname + ' -' + APartMessage);
end;

procedure TIRC.OnWelcome(ASender: TIdContext; const AMsg: String);
begin
  FServerMessage := AMsg;
  SendServerMessage;
end;

function TIRC.RemoveOPVoicePrefix(const Channel: string): string;
begin
  Result := Channel;
  if FIdIRC.IsOp(Channel) or FIdIRC.IsVoice(Channel) then
    Result := Copy(Channel, 2, MaxInt);
end;

procedure TIRC.SendMessage;
begin
  FOnMessageReceived(FChannel, FMessage);
end;

procedure TIRC.SendChannelJoined;
begin
  FOnChannelJoined(FChannel);
end;

procedure TIRC.SendNickNameListReceived;
begin
  FOnNickListReceived(FChannel, FNicknameList)
end;

procedure TIRC.SendParted;
begin
  FOnUserLeft(FChannel, FNickname);
end;

procedure TIRC.SendServerMessage;
begin
  if FServerMessages <> nil then
    FLog.AddStrings(FServerMessages);

  if FServerMessage <> '' then
    FLog.Add(FServerMessage);

  FServerMessages := nil;
  FServerMessage := '';
end;

procedure TIRC.SendUserJoined;
begin
  FOnUserJoined(FChannel, FNickname);
end;

procedure TIRC.Connect;
begin
  ReadConfig;
  FIdIRC.Connect;
  ConfigureEncoding;
end;

procedure TIRC.Disconnect;
begin
  FIdIRC.Disconnect;
end;

procedure TIRC.JoinChannel(const Name: string);
begin
  FIdIRC.Join(Name);
end;

procedure TIRC.LeaveChannel(const Name: string);
begin
  FIdIRC.Part(Name);
end;

procedure TIRC.SendMessage(const Message: string);
var
  IsCommand: Boolean;
  RawString: string;
begin
  IsCommand := IsInputCommand(Message);
  if (FActiveChannel = '') or IsCommand then
  begin
    RawString := Message;
    if IsCommand then
       RawString := FCommands.GetRawCommand(RawString);

    FIdIRC.Raw(RawString)
  end
  else
    MessageToChannel(Message);
end;

constructor TIRC.Create;
begin
  inherited;
  ConfigureIdIRC;
  FCommands := TIRCCommand.Create;
end;

destructor TIRC.Destroy;
begin
  FIdIRC.Free;
  FAutoJoinChannels.Free;
  FCommands.Free;
  inherited;
end;

end.

