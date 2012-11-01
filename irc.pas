unit IRC;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdIRC, IdComponent, IdContext ;

type

    { TIRC }

    TIRC = class
    private
      FLog: TStrings;
      FChannels: TStrings;
      FActiveChannel: string;
      FIdIRC: TIdIRC;
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
    public
      property Log: TStrings read FLog write FLog;
      property ActiveChannel: string read FActiveChannel write FActiveChannel;
      procedure Connect;
      procedure Disconnect;
      procedure JoinChannel(const Name: string; Memo: TStrings);
      procedure SendMessage(const Message: string);
      constructor Create;
      destructor Destroy; override;
    end;

implementation

procedure TIRC.ReadConfig;
begin
  //TODO: Mostrar e salvar configurações
  FIdIRC.Host:= 'irc.freenode.org';
  FIdIRC.Port := 6667;
  FIdIRC.Username:= 'SapoIndy';
  FIdIRC.Nickname:= 'SapoIndy';
  FIdIRC.RealName:= 'Fabio Gomes';
  FIdIRC.AltNickname := 'SapoIndy2';
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
end;

procedure TIRC.AddChannelMessage(const Channel, Message: string);
begin
  (FChannels.Objects[FChannels.IndexOf(Channel)] as TStrings).Add(Message);
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

procedure TIRC.JoinChannel(const Name: string; Memo: TStrings);
begin
  FChannels.AddObject(Name, Memo);
  FIdIRC.Join(Name);
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
  inherited;
end;

end.

