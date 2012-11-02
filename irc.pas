unit IRC;

{$mode objfpc}{$H+}

interface

uses
  Classes, IdIRC, IdComponent, IdContext;

type

    { TIRC }

    TOnChannelJoined = function(const Name: string): TStrings of object;
    TIRC = class
    private
      FLog: TStrings;
      FChannels: TStrings;
      FActiveChannel: string;
      FIdIRC: TIdIRC;
      FOnChannelJoined: TOnChannelJoined;
      FAutoJoinChannels: TStrings;
			FReady: Boolean;
      procedure AddChannelMessage(const Channel, Message: string);
      procedure ConfigureIdIRC;
      procedure MessageToChannel(const Message: string);
      procedure ReadConfig;
      procedure OnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
      procedure OnNotice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
      procedure OnMOTD(ASender: TIdContext; AMOTD: TStrings);
      procedure OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
      procedure OnPrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
      procedure OnNickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
      procedure OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
      procedure OnWelcome(ASender: TIdContext; const AMsg: String);
    public
      property Log: TStrings read FLog write FLog;
      property ActiveChannel: string read FActiveChannel write FActiveChannel;
      property OnChannelJoined: TOnChannelJoined read FOnChannelJoined write FOnChannelJoined;
      property Ready: Boolean read FReady;
      procedure AutoJoinChannels;
      procedure Connect;
      procedure Disconnect;
      procedure JoinChannel(const Name: string);
      procedure SendMessage(const Message: string);
      procedure LeaveCurrentChannel;
      constructor Create;
      destructor Destroy; override;
    end;

implementation

uses config, sysutils;

procedure TIRC.ReadConfig;
var
  C: TIRCConfig;
begin
  C := TIRCConfig.Create;
  try
    C.Load;

    FIdIRC.Host:= C.Host;
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
  FIdIRC.OnServerWelcome := @OnWelcome;
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
begin
  //Todo: Highligh caso citação
  AddChannelMessage(ATarget, ANickname + ': ' + AMessage);
end;

procedure TIRC.OnNickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
var
  S: string;
begin
  AddChannelMessage(AChannel, 'Nick list');
  for S in ANicknameList do;
    AddChannelMessage(AChannel, S);
end;

procedure TIRC.OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
begin
  FLog.Add('Joined: ' + ANickname + ' - ' + AHost + ' - ' + AChannel);
  AddChannelMessage(AChannel, 'Joined: ' + ANickname);
end;

procedure TIRC.OnWelcome(ASender: TIdContext; const AMsg: String);
begin
  FLog.Add(AMsg);
  FReady := True;
end;

procedure TIRC.Connect;
begin
  ReadConfig;
  FIdIRC.Connect;
end;

procedure TIRC.Disconnect;
begin
  //TODO: msg

  if FIdIRC.Connected then
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

procedure TIRC.SendMessage(const Message: string);
begin
  if FActiveChannel = '' then
     FIdIRC.Raw(Message)
  else
    MessageToChannel(Message);
end;

procedure TIRC.LeaveCurrentChannel;
begin
  FIdIRC.Part(FActiveChannel);
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

