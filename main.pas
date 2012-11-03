unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Menus, ActnList,
  ExtCtrls, LCLIntf, LMessages, LCLType, IRC;

const
     LM_AFTER_SHOW = LM_USER + 300;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionChat: TAction;
    ActionCloseChat: TAction;
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
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemChannel: TMenuItem;
    MenuItemConfig: TMenuItem;
    MenuServidor: TMenuItem;
    PageControl: TPageControl;
    PanelRight: TPanel;
    PopupMenuTreeView: TPopupMenu;
    TabServer: TTabSheet;
    TreeViewUsers: TTreeView;
    procedure ActionChatExecute(Sender: TObject);
    procedure ActionCloseChatExecute(Sender: TObject);
    procedure ActionLeaveChannelExecute(Sender: TObject);
    procedure ActionConectarExecute(Sender: TObject);
    procedure ActionConfigExecute(Sender: TObject);
    procedure ActionDesconectarExecute(Sender: TObject);
    procedure ActionJoinChannelExecute(Sender: TObject);
    procedure EditMensagemKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PopupMenuTreeViewPopup(Sender: TObject);
    procedure TreeViewUsersDblClick(Sender: TObject);
    procedure TreeViewUsersSelectionChanged(Sender: TObject);
  private
    FIRC: TIRC;
    procedure AddChannelToTree(const Channel: string);
    procedure CloseChannel(const Channel: string);
    procedure CloseChannelTab(const Channel: string);
    procedure ConfigureMemo(var Memo: TMemo);
    function FindChannelNode(const Channel: string): TTreeNode;
    function GetChannelTab(const Channel: string): TTabSheet;
    procedure MostrarConfig;
    procedure OnNickListReceived(const Channel: string; List: TStrings);
    procedure OnUserJoined(const Channel, User: string);
    procedure OnUserParted(const Channel, User: string);
    procedure OnMessageReceived(const Channel, Message: string);
    procedure OnChannelJoined(const Channel: string);
    procedure RemoveChannelFromList(const Channel: string);
    function RemoveOPVoicePrefix(const Username: string): string;
    procedure RemoveUserFromChannelList(const User: string; const Channel: string);
    function GetTabByName(const Channel: string): TTabSheet;
    function NewChannelTab(const Channel: string): TTabSheet;
    procedure SelectChannelTab;
    procedure WmAfterShow(var Msg: TLMessage); message LM_AFTER_SHOW;
    procedure AfterShow;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses FileUtil, ConfigForm, config, sysutils;

{$R *.lfm}

{ TMainForm }

const
  DefaultFontSize = 11;

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

procedure TMainForm.ActionLeaveChannelExecute(Sender: TObject);
begin
  FIRC.LeaveChannel(TreeViewUsers.Selected.Text);
end;

procedure TMainForm.ActionCloseChatExecute(Sender: TObject);
begin
  GetTabByName(RemoveOPVoicePrefix(TreeViewUsers.Selected.Text)).Free;
end;

procedure TMainForm.ActionChatExecute(Sender: TObject);
begin
  SelectChannelTab;
end;

procedure TMainForm.EditMensagemKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
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
  PostMessage(Self.Handle, LM_AFTER_SHOW, 0, 0);
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  AutoScroll := WindowState <> wsMinimized;
end;

procedure TMainForm.OnMessageReceived(const Channel, Message: string);
var
  Tab: TTabSheet;
  Memo: TMemo;
begin
  Tab := GetChannelTab(Channel);

  if Message = '' then
    Exit;

  Memo := Tab.Components[0] as TMemo; //TODO: Melhorar essa gambi
  Memo.Lines.Add(Message);
end;

procedure TMainForm.OnChannelJoined(const Channel: string);
begin
  PageControl.ActivePage := GetChannelTab(Channel);
  AddChannelToTree(Channel);
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
begin
  GetTabByName(Channel).Free;
end;

procedure TMainForm.ConfigureMemo(var Memo: TMemo);
begin
  Memo.Align := alClient;
  Memo.ScrollBars := ssVertical;
  Memo.ReadOnly := True;
  Memo.Cursor := crArrow;
  Memo.Font.Size := DefaultFontSize;
  Memo.TabStop := False;
end;

function TMainForm.FindChannelNode(const Channel: string): TTreeNode;
begin
  Result := TreeViewUsers.Items.GetFirstNode;
  while Assigned(Result) and (UpperCase(Result.Text) <> UpperCase(Channel)) do
    Result := Result.GetNextSibling;
end;

function TMainForm.GetChannelTab(const Channel: string): TTabSheet;
var
  ChannelName: string;
begin
  ChannelName := RemoveOPVoicePrefix(Channel);
  Result := GetTabByName(ChannelName);
  if Result = nil then
    Result := NewChannelTab(ChannelName);
end;

procedure TMainForm.AddChannelToTree(const Channel: string);
begin
  if FindChannelNode(Channel) <> nil then
    Exit;

  TreeViewUsers.Items.Add(nil, Channel);
  TreeViewUsers.AlphaSort;
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

procedure TMainForm.PopupMenuTreeViewPopup(Sender: TObject);
var
  IsChannel: Boolean;
begin
  if TreeViewUsers.Selected = nil then
  begin
   ActionLeaveChannel.Visible := False;
   ActionCloseChat.Visible := False;
   ActionChat.Visible := False;
   Exit;
  end;

  IsChannel := TreeViewUsers.Selected.Parent = nil;
  ActionLeaveChannel.Visible :=  IsChannel;

  if IsChannel then
  begin
    ActionCloseChat.Visible := False;
    ActionChat.Visible := False;
  end
  else
  begin
    ActionCloseChat.Visible := GetTabByName(RemoveOPVoicePrefix(TreeViewUsers.Selected.Text)) <> nil;
    ActionChat.Visible := not ActionCloseChat.Visible;
  end;
end;

procedure TMainForm.TreeViewUsersDblClick(Sender: TObject);
begin
  SelectChannelTab;
end;

procedure TMainForm.TreeViewUsersSelectionChanged(Sender: TObject);
var
  Selected: TTreeNode;
  Tab: TTabSheet;
begin
  Selected := TreeViewUsers.Selected;
  if Selected = nil then
     Exit;

  Tab := GetTabByName(RemoveOPVoicePrefix(Selected.Text));
  if Tab = nil then
    Exit;

  PageControl.ActivePage := Tab;
end;

procedure TMainForm.RemoveChannelFromList(const Channel: string);
begin
  TreeViewUsers.Items.FindTopLvlNode(Channel).Free;
end;

function TMainForm.RemoveOPVoicePrefix(const Username: string): string;
begin
  if Username[1] in ['@', '+'] then
    Result := Copy(Username, 2, MaxInt)
  else
    Result := Username;
end;

procedure TMainForm.RemoveUserFromChannelList(const User: string; const Channel: string);
var
  ChannelNode: TTreeNode;
begin
  ChannelNode := TreeViewUsers.Items.FindTopLvlNode(Channel);
  ChannelNode.FindNode(User).Free;
end;

function TMainForm.GetTabByName(const Channel: string): TTabSheet;
var
  I: Integer;
begin
  for I := 0 to PageControl.PageCount -1 do
  begin
    Result := PageControl.Pages[I];
    if UpperCase(Result.Caption) = UpperCase(Channel) then
      Exit;
  end;
  Result := nil;
end;

function TMainForm.NewChannelTab(const Channel: string): TTabSheet;
var
  Memo: TMemo;
begin
  Result := TTabSheet.Create(PageControl);
  Result.PageControl := PageControl;
  Result.Caption := Channel;

  Memo := TMemo.Create(Result);
  Memo.Parent := Result;
  ConfigureMemo(Memo);
end;

procedure TMainForm.SelectChannelTab;
begin
  if (TreeViewUsers.Selected = nil) or (TreeViewUsers.Selected.Parent = nil) then
     Exit;

  PageControl.ActivePage := GetChannelTab(TreeViewUsers.Selected.Text);
end;

procedure TMainForm.WmAfterShow(var Msg: TLMessage);
begin
  AfterShow;
end;

procedure TMainForm.AfterShow;
begin
  if not FileExistsUTF8(DefaultConfigFile) then
    MostrarConfig;

  FIRC.Log := MemoServidor.Lines;
  FIRC.Connect;
  FIRC.AutoJoinChannels;
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

procedure TMainForm.OnUserParted(const Channel, User: string);
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
  FIRC.OnMessageReceived := @OnMessageReceived;
  FIRC.OnNickListReceived := @OnNickListReceived;
  FIRC.OnUserJoined := @OnUserJoined;
  FIRC.OnUserParted := @OnUserParted;
  FIRC.OnChannelJoined := @OnChannelJoined;

  ConfigureMemo(MemoServidor);
end;

destructor TMainForm.Destroy;
begin
  FIRC.Free;
  inherited Destroy;
end;

end.
