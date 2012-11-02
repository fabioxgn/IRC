unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Menus, ActnList,
  Windows, IRC, SynMemo, SynEdit;

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
    TabSheetServidor: TTabSheet;
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
    procedure MostrarConfig;
    function OnChannelJoined(const Nome: string): TStrings;
    procedure OnNickListReceived(const Channel: string; List: TStrings);
    procedure OnUserJoined(const Channel, User: string);
    procedure OnUserLeft(const Channel, User: string);
    function NovoCanal(const Canal: string): TStrings;
    procedure FecharAba(const Tab: TTabSheet);
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
var
  Channel: string;
  Tab: TTabSheet;
  I: Integer;
begin
  Channel := TreeViewUsers.Selected.Text;

  for I := 0 to PageControl.PageCount -1 do
  begin
    Tab := PageControl.Pages[I];
    if Tab.Caption = Channel then;
    begin
      PageControl.ActivePage := Tab;
      FecharAba(Tab);
      Exit;
    end;
  end;
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

  TreeViewUsers.Items.Add(nil, Canal);

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
  FIRC.LeaveCurrentChannel;
  Tab.Free;
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
  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
  for User in List do
    TreeViewUsers.Items.AddChild(ChannelNode, User);
end;

procedure TMainForm.OnUserJoined(const Channel, User: string);
var
  ChannelNode: TTreeNode;
begin
  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
  TreeViewUsers.Items.AddChild(ChannelNode, User);
end;

procedure TMainForm.OnUserLeft(const Channel, User: string);
var
  ChannelNode: TTreeNode;
begin
  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
  ChannelNode.FindNode(User).Free;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIRC := TIRC.Create;
  FIRC.OnChannelJoined := @OnChannelJoined;
  FIRC.OnNickListReceived := @OnNickListReceived;
  FIRC.OnUserJoined := @OnUserJoined;
  FIRC.OnUserLeft := @OnUserLeft;
end;

destructor TMainForm.Destroy;
begin
  FIRC.Free;
  inherited Destroy;
end;

end.
