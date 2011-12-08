unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, IdIRC, StrUtils, IdComponent, IdContext, Windows;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionConectar: TAction;
    Desconectar: TAction;
    ActionList: TActionList;
    EditMensagem: TEdit;
    FIRC: TIdIRC;
    MainMenu: TMainMenu;
    MemoCanal: TMemo;
    MemoUsuarios: TMemo;
    MemoServidor: TMemo;
    MenuConectar: TMenuItem;
    MenuDesconectar: TMenuItem;
    MenuServidor: TMenuItem;
    PageControl: TPageControl;
    TabSheetCanal: TTabSheet;
    TabSheetServidor: TTabSheet;
    procedure ActionConectarExecute(Sender: TObject);
    procedure DesconectarExecute(Sender: TObject);
    procedure EditMensagemKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    procedure Status(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure Notice(ASender: TIdContext; const ANicknameFrom, AHost, ANicknameTo, ANotice: String);
    procedure MOTD(ASender: TIdContext; AMOTD: TStrings);
    procedure Raw(ASender: TIdContext; AIn: Boolean; const AMessage: String);
    procedure PrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
    procedure NickNameListReceive(ASender: TIdContext; const AChannel: String; ANicknameList: TStrings);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.DesconectarExecute(Sender: TObject);
begin
  FIRC.Disconnect;
end;

procedure TMainForm.ActionConectarExecute(Sender: TObject);
begin
  FIRC.Host:= 'irc.freenode.org';
  FIRC.Port := 6667;
  FIRC.Username:= 'SapoIndy';
  FIRC.Nickname:= 'SapoIndy';
  FIRC.RealName:= 'Fabio Gomes';
  FIRC.Connect;

  FIRC.Join('#lightirc');
end;

procedure TMainForm.EditMensagemKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> VK_RETURN then
     Exit;

  FIRC.Say('#lightirc', EditMensagem.Text);
  EditMensagem.Clear;
end;

procedure TMainForm.Status(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  MemoServidor.Lines.Add(AStatusText);
end;

procedure TMainForm.Notice(ASender: TIdContext; const ANicknameFrom, AHost,
  ANicknameTo, ANotice: String);
begin
  MemoServidor.Lines.Add(ANotice);
end;

procedure TMainForm.MOTD(ASender: TIdContext; AMOTD: TStrings);
begin
  MemoServidor.Lines.Add(AMOTD.Text);
end;

procedure TMainForm.Raw(ASender: TIdContext; AIn: Boolean; const AMessage: String
  );
begin
  MemoServidor.Lines.Add(AMessage);
end;

procedure TMainForm.PrivateMessage(ASender: TIdContext; const ANickname, AHost, ATarget, AMessage: String);
begin
  MemoCanal.Lines.Add(ANickname + ':' + AMessage);
end;

procedure TMainForm.NickNameListReceive(ASender: TIdContext;
  const AChannel: String; ANicknameList: TStrings);
begin
  MemoUsuarios.Lines.Assign(ANicknameList);
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIRC := TIdIRC.Create(Self);
  FIRC.OnStatus:= @Status;
  FIRC.OnNotice:= @Notice;
  FIRC.OnMOTD:= @MOTD;
  FIRC.OnPrivateMessage:= @PrivateMessage;
  FIRC.OnNicknamesListReceived:= @NickNameListReceive;
end;

destructor TMainForm.Destroy;
begin
  if FIRC.Connected then
     FIRC.Disconnect;
  inherited Destroy;
end;

end.

