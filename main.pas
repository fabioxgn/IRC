unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Menus, ActnList,
  Windows, IRC, SynEdit;

const
     WM_AFTER_SHOW = WM_USER + 300;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionJoinChannel: TAction;
    ActionConfig: TAction;
    ActionLeaveChannel: TAction;
    ActionConectar: TAction;
    ActionDesconectar: TAction;
    ActionList: TActionList;
    EditMensagem: TEdit;
    MainMenu: TMainMenu;
    MemoServidor: TMemo;
    MenuConectar: TMenuItem;
    MenuDesconectar: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItemChannel: TMenuItem;
    MenuItemConfig: TMenuItem;
    MenuServidor: TMenuItem;
    PageControl: TPageControl;
    PopupMenuTreeView: TPopupMenu;
    TabServer: TTabSheet;
    TreeViewUsers: TTreeView;
    procedure ActionLeaveChannelExecute(Sender: TObject);
    procedure ActionConectarExecute(Sender: TObject);
    procedure ActionConfigExecute(Sender: TObject);
    procedure ActionDesconectarExecute(Sender: TObject);
    procedure ActionJoinChannelExecute(Sender: TObject);
    procedure ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure EditMensagemKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
  private
    FIRC: TIRC;
    procedure AddChannelToList(const Channel: string);
    procedure CloseChannel(const Channel: string);
    procedure CloseChannelTab(const Channel: string);
    procedure ConfigureMemo(var Memo: TMemo);
    procedure MostrarConfig;
    function OnChannelJoined(const Nome: string): TStrings;
    procedure OnNickListReceived(const Channel: string; List: TStrings);
    procedure OnUserJoined(const Channel, User: string);
    procedure OnUserLeft(const Channel, User: string);
    function NovoCanal(const Canal: string): TStrings;
    procedure RemoveChannelFromList(const Channel: string);
    procedure RemoveUserFromChannelList(const User: string; const Channel: string);
    procedure WmAfterShow(var Msg: TMessage); message WM_AFTER_SHOW;
    procedure AfterShow;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses FileUtil, ConfigForm, config, messages;

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

  //TODO: Verificar se o channel já não está ativo, se estiver selecionar a aba

  FIRC.JoinChannel(Channel);
end;

procedure TMainForm.ActionListUpdate(AAction: TBasicAction; var Handled: Boolean);
var
  SelectedNode: TTreeNode;
begin
  SelectedNode := TreeViewUsers.Selected;
  ActionLeaveChannel.Visible := (SelectedNode <> nil) and (SelectedNode.Parent = nil);
end;

procedure TMainForm.ActionConectarExecute(Sender: TObject);
begin
  FIRC.Connect;
end;

procedure TMainForm.ActionConfigExecute(Sender: TObject);
begin
  MostrarConfig;
end;

procedure TMainForm.ActionLeaveChannelExecute(Sender: TObject);
begin
     FIRC.LeaveChannel(TreeViewUsers.Selected.Text);
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
  PostMessage(Self.Handle, WM_AFTER_SHOW, 0, 0);
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
  //TODO: Configurar memo do servidor
  ConfigureMemo(Memo);

  AddChannelToList(Canal);

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

procedure TMainForm.CloseChannelTab(const Channel: string);
var
  I: Integer;
  Tab: TTabSheet;
begin
  for I := 0 to PageControl.PageCount -1 do
  begin
    Tab := PageControl.Pages[I];
    if Tab.Caption = Channel then
    begin
      Tab.Free;
      Exit;
    end;
  end;
end;

procedure TMainForm.ConfigureMemo(var Memo: TMemo);
begin
  Memo.Align := alClient;
  Memo.ScrollBars := ssVertical;
  Memo.ReadOnly := True;
  Memo.Cursor := crDefault;
end;

procedure TMainForm.AddChannelToList(const Channel: string);
begin
  TreeViewUsers.BeginUpdate;
  try
    TreeViewUsers.Items.Add(nil, Channel);
    TreeViewUsers.AlphaSort;
  finally
    TreeViewUsers.EndUpdate;
  end;
end;

procedure TMainForm.CloseChannel(const Channel: string);
begin
  CloseChannelTab(Channel);
  RemoveChannelFromList(Channel);
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = TabServer then
    FIRC.ActiveChannel := ''
  else
    FIRC.ActiveChannel := PageControl.ActivePage.Caption;
end;

procedure TMainForm.RemoveChannelFromList(const Channel: string);
begin
  TreeViewUsers.Items.FindTopLvlNode(Channel).Free;
end;

procedure TMainForm.RemoveUserFromChannelList(const User: string; const Channel: string);
var
  ChannelNode: TTreeNode;
begin
  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
  ChannelNode.FindNode(User).Free;
end;

procedure TMainForm.WmAfterShow(var Msg: TMessage);
begin
  AfterShow;
end;

procedure TMainForm.AfterShow;
begin
  if not FileExistsUTF8(DefaultConfigFile) then
    MostrarConfig;

  FIRC.Log := MemoServidor.Lines;
  FIRC.Connect;

  //TODO: Timeout
  while not FIRC.Ready do
		Application.ProcessMessages;

  FIRC.AutoJoinChannels;
  TreeViewUsers.AlphaSort;
end;

function TMainForm.OnChannelJoined(const Nome: string): TStrings;
begin
  Result := NovoCanal(Nome);
end;

procedure TMainForm.OnNickListReceived(const Channel: string; List: TStrings);
var
  ChannelNode: TTreeNode;
  User: string;
begin
  TreeViewUsers.BeginUpdate;
  try
	  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
	  for User in List do
	    TreeViewUsers.Items.AddChild(ChannelNode, User);
	  TreeViewUsers.AlphaSort;
  finally
    TreeViewUsers.EndUpdate;
  end;
end;

procedure TMainForm.OnUserJoined(const Channel, User: string);
var
  ChannelNode: TTreeNode;
begin
  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
  TreeViewUsers.Items.AddChild(ChannelNode, User);
  TreeViewUsers.AlphaSort;
end;

procedure TMainForm.OnUserLeft(const Channel, User: string);
begin
  if User = FIRC.UserName then
    CloseChannel(Channel)
  else
    RemoveUserFromChannelList(User, Channel);
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIRC := TIRC.Create;
  FIRC.OnChannelJoined := @OnChannelJoined;
  FIRC.OnNickListReceived := @OnNickListReceived;
  FIRC.OnUserJoined := @OnUserJoined;
  FIRC.OnUserLeft := @OnUserLeft;

  ConfigureMemo(MemoServidor);
end;

destructor TMainForm.Destroy;
begin
  FIRC.Free;
  inherited Destroy;
end;

end.
