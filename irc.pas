unit IRC;

{$mode objfpc}{$H+}

interface

uses
  Classes, IdIRC, IdComponent, IdContext, IRCCommands, IdException;

type

    { TIRC }

    TOnChannelJoined = procedure(const Channel: string) of object;
    TOnNickListReceived = procedure(const Channel: string; List: TStrings) of object;
    TOnUserEvent = procedure(const Channel, User: string) of object;
    TOnUserQuit = procedure(const NickName: string) of object;
    TOnMessageReceived = procedure(const Channel, Message: string) of object;
    TOnShowPopup = procedure(const Msg: string) of object;

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
      FOnUserQuit: TOnUserQuit;
      FOnMessageReceived: TOnMessageReceived;
      FOnShowPopup: TOnShowPopup;
      FAutoJoinChannels: TStrings;
      FCommands: TIRCCommand;
      procedure ConfigureEncoding;
      procedure ConfigureEvents;
      procedure ConfigureIdIRC;
      function FormatarMensagem(const NickName, Message: string): string;
      function GetConnected: Boolean;
      function GetUserName: string;
      function HighlightUserName(const AMessage: String): string;
      function IsInputCommand(const Message: string): Boolean;
      procedure MessageToChannel(const Msg: string);
      procedure MessageReceived(const Channel, Message: string);
      procedure Raw(const RawString: string);
      procedure ReadConfig;
      procedure OnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
      procedure OnNotice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
      procedure OnMOTD(ASender: TIdContext; AMOTD: TStrings);
      procedure OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
      procedure OnPrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
      procedure OnNickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
      procedure OnJoin(ASender: TIdContext; const ANickname, AHost, AChannel: String);
      procedure OnPart(ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
      procedure OnQuit(ASender: TIdContext; const ANickname, AHost, AReason: String);
      procedure OnWelcome(ASender: TIdContext; const AMsg: String);
      procedure RemoveEvents;
      function RemoveOPVoicePrefix(const Channel: string): string;
      procedure Say(const Channel, Msg: string);
      procedure SendMessage;
      procedure SendChannelJoined;
      procedure SendNickNameListReceived;
      procedure SendParted;
      procedure SendQuit;
      procedure SendServerMessage; overload;
      procedure SendServerMessage(const Msg: string); overload;
      procedure SendUserJoined;
      procedure DoConnect;
      procedure MessageBox(const Msg: string);
      procedure HandleIdException(E: EIdException);
    public
      property Log: TStrings read FLog write FLog;
      property Ready: Boolean read FReady;
      property ActiveChannel: string read FActiveChannel write FActiveChannel;
      property OnChannelJoined: TOnChannelJoined read FOnChannelJoined write FOnChannelJoined;
      property OnNickListReceived: TOnNickListReceived read FOnNickListReceived write FOnNickListReceived;
      property OnUserJoined: TOnUserEvent read FOnUserJoined write FOnUserJoined;
      property OnUserParted: TOnUserEvent read FOnUserLeft write FOnUserLeft;
      property OnUserQuit: TOnUserQuit read FOnUserQuit write FOnUserQuit;
      property OnMessageReceived: TOnMessageReceived read FOnMessageReceived write FOnMessageReceived;
      property OnShowPopup: TOnShowPopup read FOnShowPopup write FOnShowPopup;
      property UserName: string read GetUserName;
      property Connected: Boolean read GetConnected;
      procedure AutoJoinChannels;
      procedure Connect;
      procedure Disconnect;
      procedure Join(const Name: string);
      procedure Part(const Name: string);
      procedure SendMessage(const Message: string);
      constructor Create;
      destructor Destroy; override;
    end;

implementation

uses config, IdSync, IdGlobal, sysutils;

resourcestring
  StrJoined = '* Joined: ';
  StrParted = '* Parted: ';
  StrQuit = '* %s %s';
  StrAlreadyConnected = 'Alread connected to %s. Disconnect first.';

const
  NickNameFormat = '<%s>';
  MessageFormat = '%s ' + NickNameFormat + ': %s';

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
end;

procedure TIRC.ConfigureEncoding;
begin
 FIdIRC.IOHandler.DefStringEncoding := TIdTextEncoding.ASCII;
end;

procedure TIRC.ConfigureEvents;
begin
  // Remember to remove the events in the RemoveEvents method
  // If you don't clen up the events the thread can try to notify
  // the UI and cause a deadlock, see Issue #18
  FIdIRC.OnStatus := @OnStatus;
  FIdIRC.OnNotice := @OnNotice;
  FIdIRC.OnMOTD := @OnMOTD;
  FIdIRC.OnPrivateMessage := @OnPrivateMessage;
  FIdIRC.OnNicknamesListReceived := @OnNickNameListReceive;
  FIdIRC.OnJoin := @OnJoin;
  FIdIRC.OnPart := @OnPart;
  FIdIRC.OnServerWelcome := @OnWelcome;
  FIdIRC.OnRaw := @OnRaw;
  FIdIRC.OnQuit := @OnQuit;
end;

function TIRC.FormatarMensagem(const NickName, Message: string): string;
begin
  Result := Format(MessageFormat, [FormatDateTime(ShortTimeFormat, Now), NickName, Message]);
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
    Join(FAutoJoinChannels.ValueFromIndex[I]);
  FreeAndNil(FAutoJoinChannels);
end;

procedure TIRC.MessageToChannel(const Msg: string);
var
  Channel: string;
begin
  Channel := RemoveOPVoicePrefix(FActiveChannel);
  Say(Channel, Msg);
  MessageReceived(FActiveChannel, FormatarMensagem(UserName, Msg))
end;

procedure TIRC.MessageReceived(const Channel, Message: string);
begin
  FChannel := Channel;
  FMessage := Message;
  TIdSync.SynchronizeMethod(@SendMessage);
end;

procedure TIRC.Raw(const RawString: string);
begin
  try
    FIdIRC.Raw(RawString)
  except
    on E: EIdException do
      HandleIdException(E);
  end;
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
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.OnMOTD(ASender: TIdContext; AMOTD: TStrings);
begin

  FServerMessages := AMOTD;
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.OnRaw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
begin
  {$IFDEF DEBUG}
  FServerMessage := AMessage;
  TIdSync.SynchronizeMethod(@SendServerMessage);
  {$ENDIF}
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

  SendServerMessage(StrJoined + ANickname + ' - ' + AHost + ' - ' + AChannel);
end;

procedure TIRC.OnPart(ASender: TIdContext; const ANickname, AHost, AChannel, APartMessage: String);
begin
  FChannel := AChannel;
  FNickName := ANickname;
  TIdSync.SynchronizeMethod(@SendParted);

  if ANickname = UserName then
     Exit;

  SendServerMessage(StrParted + ANickname + ' - ' + ' -' + AHost + ': ' + APartMessage + ' - ' + AChannel);
end;

procedure TIRC.OnQuit(ASender: TIdContext; const ANickname, AHost, AReason: String);
begin
  FNickName := ANickname;
  FMessage := Format(StrQuit, [ANickname, AReason]);
  TIdSync.SynchronizeMethod(@SendQuit);
end;

procedure TIRC.OnWelcome(ASender: TIdContext; const AMsg: String);
begin
  FServerMessage := AMsg;
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.RemoveEvents;
begin
  FIdIRC.OnStatus := nil;
  FIdIRC.OnNotice := nil;
  FIdIRC.OnMOTD := nil;
  FIdIRC.OnPrivateMessage := nil;
  FIdIRC.OnNicknamesListReceived := nil;
  FIdIRC.OnJoin := nil;
  FIdIRC.OnPart := nil;
  FIdIRC.OnServerWelcome := nil;
  FIdIRC.OnRaw := nil;
  FIdIRC.OnQuit := nil;
end;

function TIRC.RemoveOPVoicePrefix(const Channel: string): string;
begin
  Result := Channel;
  if FIdIRC.IsOp(Channel) or FIdIRC.IsVoice(Channel) then
    Result := Copy(Channel, 2, MaxInt);
end;

procedure TIRC.Say(const Channel, Msg: string);
begin
  try
    FIdIRC.Say(Channel, Msg);
  except
    on E: EIdException do
      HandleIdException(E);
  end;
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

procedure TIRC.SendQuit;
begin
  FLog.Add(FMessage);
  FOnUserQuit(FNickName);
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

procedure TIRC.SendServerMessage(const Msg: string);
begin
  FServerMessage := Msg;
  TIdSync.SynchronizeMethod(@SendServerMessage);
end;

procedure TIRC.SendUserJoined;
begin
  FOnUserJoined(FChannel, FNickname);
end;

procedure TIRC.DoConnect;
begin
  try
     FIdIRC.Connect;
  except
    on E: Exception do
    begin
      try
        FIdIRC.Disconnect(False);
      except
      end;

      if FIdIRC.IOHandler <> nil then
        FIdIRC.IOHandler.InputBuffer.Clear;

      MessageBox(Format('Cannot connect to server: %s', [E.Message]));
      Abort;
    end;
  end;
end;

procedure TIRC.MessageBox(const Msg: string);
begin
  SendServerMessage('Disconnected from server.');
  if Assigned(FOnShowPopup) then
     FOnShowPopup(Msg);
end;

procedure TIRC.HandleIdException(E: EIdException);
begin
  MessageBox(E.Message);
  if not FIdIRC.Connected then
    Connect;
end;

procedure TIRC.Connect;
begin
  if FIdIRC.Connected then
     Disconnect;

  ConfigureEvents;
  ReadConfig;
  DoConnect;
  ConfigureEncoding;
end;

procedure TIRC.Disconnect;
begin
  if not FIdIRC.Connected then
    Exit;

  // It's necessary to remove the event handlers or
  // the thread can try to notify the interface
  // and cause a deadlock here see Issue #18
  RemoveEvents;

  {$IFDEF UNIX}
  Raw('QUIT');
  sleep(500); //Issue #18 - The thread deadlocks if we don't wait >(
  {$ENDIF}

  try
    FIdIRC.Disconnect(False);
    FIdIRC.IOHandler.InputBuffer.Clear;
  except
    //We just ignore everything at this point and hope for the best
  end;
end;

procedure TIRC.Join(const Name: string);
begin
  try
    FIdIRC.Join(Name);
   except
     on E: EIdException do
       HandleIdException(E);
   end;
end;

procedure TIRC.Part(const Name: string);
begin
  try
    FIdIRC.Part(Name);
  except
    on E: EIdException do
      HandleIdException(E);
  end;
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

    Raw(RawString);
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
