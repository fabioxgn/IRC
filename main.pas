unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Menus, ActnList, Windows, IRC;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionJoinChannel: TAction;
    ActionConfig: TAction;
    ActionCloseTab: TAction;
    ActionConectar: TAction;
    Button1: TButton;
    ActionDesconectar: TAction;
    ActionList: TActionList;
    EditMensagem: TEdit;
    MainMenu: TMainMenu;
    MemoServidor: TMemo;
    MenuConectar: TMenuItem;
    MenuDesconectar: TMenuItem;
    MenuItemChannel: TMenuItem;
    MenuItemConfig: TMenuItem;
    MenuServidor: TMenuItem;
    PageControl: TPageControl;
    TabSheetServidor: TTabSheet;
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionConectarExecute(Sender: TObject);
    procedure ActionConfigExecute(Sender: TObject);
    procedure ActionDesconectarExecute(Sender: TObject);
    procedure ActionJoinChannelExecute(Sender: TObject);
    procedure EditMensagemKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FIRC: TIRC;
    procedure MostrarConfig;
    function NewChannel(const Nome: string): TStrings;
    function NovoCanal(const Canal: string): TStrings;
    procedure FecharAba(const Tab: TTabSheet);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses FileUtil, ConfigForm, config;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ActionDesconectarExecute(Sender: TObject);
begin
  FIRC.Disconnect;
end;

procedure TMainForm.ActionJoinChannelExecute(Sender: TObject);
var
  Channel: string;
begin
  Channel := InputBox('Channel', 'Channel Name:', '');

  if Channel = '' then
	  Exit;

  FIRC.JoinChannel(Channel);
end;

procedure TMainForm.ActionConectarExecute(Sender: TObject);
begin
  FIRC.Connect;
end;

procedure TMainForm.ActionConfigExecute(Sender: TObject);
begin
  MostrarConfig;
end;

procedure TMainForm.ActionCloseTabExecute(Sender: TObject);
begin
  FecharAba(PageControl.ActivePage);
end;

procedure TMainForm.EditMensagemKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key <> VK_RETURN then
    Exit;

  FIRC.SendMessage(EditMensagem.Text);
  EditMensagem.Clear;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FIRC.Disconnect;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if not FileExistsUTF8(DefaultConfigFile) then
    MostrarConfig;

  FIRC.Log := MemoServidor.Lines;
  FIRC.OnChannelJoined := @NewChannel;
  FIRC.Connect;

  while not FIRC.Ready do
		Application.ProcessMessages;

  FIRC.AutoJoinChannels;
end;

function TMainForm.NovoCanal(const Canal: string): TStrings;
var
  Tab: TTabSheet;
  Memo: TMemo;
begin
  Tab := TTabSheet.Create(PageControl);
  Tab.PageControl := PageControl;
  Tab.Caption := Canal;

  Memo := TMemo.Create(Tab);
  Memo.Parent := Tab;
  Memo.Align := alClient;
  Memo.ScrollBars := ssVertical;

  PageControl.ActivePage := Tab;
  Result := Memo.Lines;
end;

procedure TMainForm.MostrarConfig;
var
  F: TFormConfig;
begin
  F := TFormConfig.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = TabSheetServidor then
    FIRC.ActiveChannel := ''
  else
    FIRC.ActiveChannel := PageControl.ActivePage.Caption;
end;

procedure TMainForm.FecharAba(const Tab: TTabSheet);
begin
  if Tab = TabSheetServidor then
  begin
    ShowMessage('Não é possível fechar esta aba.');
    Exit;
  end;

  FIRC.LeaveCurrentChannel;
  Tab.Free;
end;

function TMainForm.NewChannel(const Nome: string): TStrings;
begin
  Result := NovoCanal(Nome);
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIRC := TIRC.Create;
end;

destructor TMainForm.Destroy;
begin
  FIRC.Free;
  inherited Destroy;
end;

end.
