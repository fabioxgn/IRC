unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Menus, ActnList,
  ExtCtrls, LCLIntf, LCLType, IRC, ChannelList, sysutils, IRCViewIntf;

type

  { TMainForm }

  TMainForm = class(TForm, IIRCView)
    ActionExit: TAction;
    ActionCloseChannel: TAction;
    ActionCloseTab: TAction;
    ActionChat: TAction;
    ActionCloseChat: TAction;
    ActionJoinChannel: TAction;
    ActionConfig: TAction;
    ActionConnect: TAction;
    ActionDisconnect: TAction;
    ActionList: TActionList;
    EditFilter: TEdit;
    EditInput: TEdit;
    ImageListTrayIcons: TImageList;
    MainMenu: TMainMenu;
    MemoServidor: TMemo;
    MenuItemExit: TMenuItem;
    MenuItemConnect: TMenuItem;
    MenuItemDisconnect: TMenuItem;
    MenuItemCloseChannel: TMenuItem;
    MenuItemCloseChat: TMenuItem;
    MenuItemChat: TMenuItem;
    MenuItemCloseTab: TMenuItem;
    MenuItemCloseActiveTab: TMenuItem;
    MenuItemJoinChannel: TMenuItem;
    MenuItemConfig: TMenuItem;
    MenuItemServer: TMenuItem;
    PageControl: TPageControl;
    PanelRoot: TPanel;
    PanelLeft: TPanel;
    PanelRight: TPanel;
    PopupMenuTray: TPopupMenu;
    PopupMenuPageControl: TPopupMenu;
    PopupMenuTreeView: TPopupMenu;
    StatusBar: TStatusBar;
    TabServer: TTabSheet;
    TimerConnection: TTimer;
    TrayIcon: TTrayIcon;
    TreeViewUsers: TTreeView;
    procedure ActionChatExecute(Sender: TObject);
    procedure ActionCloseChannelExecute(Sender: TObject);
    procedure ActionCloseChatExecute(Sender: TObject);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionConnectExecute(Sender: TObject);
    procedure ActionConfigExecute(Sender: TObject);
    procedure ActionDisconnectExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionJoinChannelExecute(Sender: TObject);
    procedure EditFilterKeyDown(Sender: TObject; var Key: Word;
     Shift: TShiftState);
    procedure EditFilterKeyUp(Sender: TObject; var Key: Word;
     Shift: TShiftState);
    procedure EditInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure ApplicationActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure PageControlMouseDown(Sender: TObject; Button: TMouseButton;
     Shift: TShiftState; X, Y: Integer);
    procedure PopupMenuTreeViewPopup(Sender: TObject);
    procedure TimerConnectionTimer(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure TreeViewUsersDblClick(Sender: TObject);
    procedure TreeViewUsersSelectionChanged(Sender: TObject);
  private
    FIRC: TIRC;
    FChannelList: TChannelList;
    procedure AddChannelToTree(Channel: TChannel);
    procedure AddNicksToChannel(const Channel: TChannel; const List: TStrings);
    procedure AddNicksToTreeView(const Channel: TChannel);
    procedure AddUserToTreeView(const User: TUser; const Channel: TChannel);
    procedure ConfigureMemo(var Memo: TMemo);
    procedure SetColors;
    function FindChannelNode(const Channel: string): TTreeNode;
    function GetChannelTab(const Channel: string): TTabSheet;
    function IsActiveTabChannel: Boolean;
    function IsChatTabOpen(const Nome: string): Boolean;
    function IsSelectedNodeUser: Boolean;
    function IsSelectedNodeChannel: Boolean;
    procedure MostrarConfig;
    procedure OnNickListReceived(const ChannelName: string; List: TStrings);
    procedure OnUserJoined(const ChannelName, Nick: string);
    procedure OnMessageReceived(const Channel, Message: string; OwnMessage: Boolean);
    procedure OnChannelJoined(const ChannelName: string);
    procedure UpdateNodeText(Node: TObject; AText: string);
    procedure UpdateTabCaption(Tab: TObject; ACaption: string);
    procedure RemoveChannelFromList(const Channel: string);
    procedure RemoveNickFromChannelList(const Nick: string; const ChannelName: string);
    procedure OnShowPopup(const Msg: string);
    function GetTabByName(const Channel: string): TTabSheet;
    function NewChannelTab(const Channel: string): TTabSheet;
    procedure SelectChannelTab;
    procedure SetFocusEditInput;
    procedure StopTrayIconAnimation;
    procedure ServerMessage(const AText: string);
    procedure NotifyChanged;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

uses FileUtil, ConfigForm, config, StringUtils, IRCUtils, TreeviewHelper;

{$R *.lfm}

const
  DefaultFontSize = 11;
  BackGroundColor = $00222827;
  FontColor = $00D0D0D0;
  LightColor = $00D0D0D0;

procedure TMainForm.ActionDisconnectExecute(Sender: TObject);
begin
  FIRC.Disconnect;
end;

procedure TMainForm.ActionExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.ActionJoinChannelExecute(Sender: TObject);
var
  Channel: string;
begin
  Channel := InputBox('Channel', 'Channel Name:', '');

  if Channel = '' then
	  Exit;

  FIRC.Join(Channel);
end;

procedure TMainForm.EditFilterKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and IsSelectedNodeUser then
    ActionChat.Execute;
end;

procedure TMainForm.EditFilterKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 if Key = VK_RETURN then
   Exit;

 TreeViewUsers.BeginUpdate;
 try
   TreeViewUsers.FilterNodes(EditFilter.Text);
   TreeViewUsers.FullExpand;
 finally
   TreeViewUsers.EndUpdate;
 end;
end;

procedure TMainForm.EditInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Word, Nick: string;
  CarretPos: TPoint;
begin
  if (key <> VK_TAB) or (EditInput.Text = '') then
   Exit;

  Word := TStringUtils.GetWordAtCursor(EditInput.Text, EditInput.CaretPos.x);
  Nick := FChannelList.AutoComplete(FIRC.ActiveChannel, Word);
  if Nick <> '' then
  begin
    EditInput.Text := StringReplace(EditInput.Text, Word, Nick, []);
    CarretPos.x := Pos(Nick, EditInput.Text) + Length(Nick);
    CarretPos.y := EditInput.CaretPos.y;
    EditInput.CaretPos := CarretPos;
  end;

  Key := VK_UNDEFINED;
end;

procedure TMainForm.ActionConnectExecute(Sender: TObject);
begin
  FIRC.Connect;
end;

procedure TMainForm.ActionConfigExecute(Sender: TObject);
begin
  MostrarConfig;
end;

procedure TMainForm.ActionCloseChatExecute(Sender: TObject);
begin
  GetTabByName(TIRCUtils.RemoveOPVoicePrefix(TreeViewUsers.Selected.Text)).Free;
end;

procedure TMainForm.ActionCloseTabExecute(Sender: TObject);
var
   TabToClose: TTabSheet;
begin
  if PageControl.ActivePage = nil then
     Exit;

  if Sender is TTabSheet then
     TabToClose := TTabSheet(Sender)
  else
     TabToClose := PageControl.ActivePage;

  if TabToClose = TabServer then
     Exit;

  if IsActiveTabChannel then
    FIRC.Part(PageControl.ActivePage.Caption)
  else
    GetTabByName(PageControl.ActivePage.Caption).Free;
end;

procedure TMainForm.ActionChatExecute(Sender: TObject);
begin
  SelectChannelTab;
end;

procedure TMainForm.ActionCloseChannelExecute(Sender: TObject);
begin
  FIRC.Part(TreeViewUsers.Selected.Text);
end;

procedure TMainForm.EditInputKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key <> VK_RETURN then
    Exit;

  FIRC.SendMessage(EditInput.Text);
  EditInput.Clear;
end;

procedure TMainForm.ApplicationActivate(Sender: TObject);
begin
  StopTrayIconAnimation;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FIRC.Disconnect;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FIRC.IsConnected then
    Exit;

  if not FileExistsUTF8(DefaultConfigFile) then
    MostrarConfig;

  FIRC.Connect;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
  AutoScroll := WindowState <> wsMinimized;
end;

procedure TMainForm.OnMessageReceived(const Channel, Message: string; OwnMessage: Boolean);
var
  Tab: TTabSheet;
  Memo: TMemo;

 function AnimateTrayIcon: Boolean;
 begin
   Result := (not OwnMessage) and ((Channel[1] <> '#') or (Pos(FIRC.NickName, Message) > 0))
 end;

begin
  if Message = '' then
    Exit;

  if AnimateTrayIcon then
	  TrayIcon.Animate := True; //Should not toggle, more messages may have triggered this

  Tab := GetChannelTab(Channel);

  Memo := Tab.Components[0] as TMemo;
  Memo.Lines.Add(Message);
end;

procedure TMainForm.OnChannelJoined(const ChannelName: string);
var
  Channel: TChannel;
  Tab: TTabSheet;
begin
  Channel := TChannel.Create(ChannelName);
  FChannelList.Add(Channel);

  Tab := GetChannelTab(Channel.Name);
  Channel.Tab := Tab;
  PageControl.ActivePage := Tab;

  AddChannelToTree(Channel);
  SetFocusEditInput;
end;

procedure TMainForm.UpdateTabCaption(Tab: TObject; ACaption: string);
begin
  (Tab as TTabSheet).Caption := ACaption;
end;

procedure TMainForm.UpdateNodeText(Node: TObject; AText: string);
begin
	(Node as TTreeNode).Text := AText;
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

procedure TMainForm.ConfigureMemo(var Memo: TMemo);
begin
  Memo.Align := alClient;
  Memo.ScrollBars := ssVertical;
  Memo.ReadOnly := True;
  Memo.Cursor := crArrow;
  Memo.Font.Size := DefaultFontSize;
  Memo.TabStop := False;
  Memo.BorderStyle := bsSingle;
  Memo.Color := BackGroundColor;
  Memo.Font.Color := FontColor;
  Memo.Invalidate;
end;

procedure TMainForm.SetColors;
begin
  EditFilter.Color := LightColor;
  EditInput.Color := BackGroundColor;;
  EditInput.Font.Color := FontColor;
  TreeViewUsers.BackgroundColor := LightColor;
end;

function TMainForm.FindChannelNode(const Channel: string): TTreeNode;
begin
  Result := FChannelList.ChannelByName(Channel).Node as TTreeNode;
end;

function TMainForm.GetChannelTab(const Channel: string): TTabSheet;
var
  ChannelName: string;
begin
  ChannelName := TIRCUtils.RemoveOPVoicePrefix(Channel);
  Result := GetTabByName(ChannelName);
  if Result = nil then
    Result := NewChannelTab(ChannelName);
end;

function TMainForm.IsActiveTabChannel: Boolean;
begin
 Result := TreeViewUsers.Items.FindTopLvlNode(PageControl.ActivePage.Caption) <> nil;
end;

function TMainForm.IsChatTabOpen(const Nome: string): Boolean;
begin
  Result := GetTabByName(TIRCUtils.RemoveOPVoicePrefix(Nome)) <> nil;
end;

function TMainForm.IsSelectedNodeUser: Boolean;
begin
 Result := (TreeViewUsers.Selected <> nil) and (TreeViewUsers.Selected.Parent <> nil);
end;

function TMainForm.IsSelectedNodeChannel: Boolean;
begin
  Result := (TreeViewUsers.Selected <> nil) and (TreeViewUsers.Selected.Parent = nil);
end;

procedure TMainForm.AddChannelToTree(Channel: TChannel);
begin
  Channel.Node := TreeViewUsers.Items.Add(nil, Channel.Name);
  TreeViewUsers.AlphaSort;
end;

procedure TMainForm.AddNicksToChannel(const Channel: TChannel; const List: TStrings);
var
 Nick: string;
begin
  for Nick in List do
    Channel.Users.Add(TUser.Create(Nick))
end;

procedure TMainForm.AddNicksToTreeView(const Channel: TChannel);
var
  User: TUser;
begin
	 TreeViewUsers.BeginUpdate;
	 try
	   Channel.Node := TreeViewUsers.Items.FindTopLvlNode(Channel.Name);
	 for User in Channel.Users do
	   User.Node := TreeViewUsers.Items.AddChild(TTreeNode(Channel.Node), User.Nick);
	 TreeViewUsers.AlphaSort;
	 finally
	   TreeViewUsers.EndUpdate;
	 end;
end;

procedure TMainForm.AddUserToTreeView(const User: TUser; const Channel: TChannel);
begin
 User.Node := TreeViewUsers.Items.AddChild(TTreeNode(Channel.Node), User.DisplayNick);
 TreeViewUsers.AlphaSort;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = TabServer then
    FIRC.ActiveChannel := ''
  else
    FIRC.ActiveChannel := PageControl.ActivePage.Caption;

  SetFocusEditInput;
end;

procedure TMainForm.PageControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  if Button <> mbRight then
    Exit;

  TabIndex := PageControl.IndexOfTabAt(X, Y);
  if TabIndex >= 0 then
     PageControl.ActivePage := PageControl.Pages[TabIndex];
end;

procedure TMainForm.PopupMenuTreeViewPopup(Sender: TObject);
var
  IsChatOpen: Boolean;
begin
  IsChatOpen := IsChatTabOpen(TreeViewUsers.Selected.Text);

  ActionCloseChannel.Visible := IsSelectedNodeChannel;
  ActionChat.Visible := IsSelectedNodeUser and (not IsChatOpen);
  ActionCloseChat.Visible := IsSelectedNodeUser and IsChatOpen;
end;

procedure TMainForm.TimerConnectionTimer(Sender: TObject);
begin
  if FIRC.IsConnected then
		StatusBar.Panels[0].Text := Format('Connected. %s@%s', [FIRC.NickName, FIRC.HostName])
  else
	  StatusBar.Panels[0].Text := 'Disconnected :(';
end;

procedure TMainForm.TrayIconClick(Sender: TObject);
begin
  StopTrayIconAnimation;
  if WindowState = wsMinimized then
  	Application.Restore
   else
     Application.Minimize;
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

  Tab := GetTabByName(TIRCUtils.RemoveOPVoicePrefix(Selected.Text));
  if Tab = nil then
    Exit;

  PageControl.ActivePage := Tab;
end;

procedure TMainForm.RemoveChannelFromList(const Channel: string);
begin
  FChannelList.ChannelByName(Channel).Node.Free;
end;

procedure TMainForm.RemoveNickFromChannelList(const Nick: string; const ChannelName: string);
var
  User: TUser;
  Channel: TChannel;
begin
  Channel := FChannelList.ChannelByName(ChannelName);
  if Channel = nil then
     Exit;

  User := Channel.Users.UserByNick(Nick);
  if (User <> nil) then
     User.Node.Free;
end;

procedure TMainForm.OnShowPopup(const Msg: string);
begin
  ShowMessage(Msg);
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

procedure TMainForm.SetFocusEditInput;
begin
  if EditInput.CanFocus then
	  EditInput.SetFocus;
end;

procedure TMainForm.StopTrayIconAnimation;
begin
  TrayIcon.Animate := False;
  TrayIcon.Icon.Assign(Application.Icon);
end;

procedure TMainForm.ServerMessage(const AText: string);
begin
  MemoServidor.Append(AText);
end;

procedure TMainForm.NotifyChanged;
begin
	TreeViewUsers.Invalidate;
end;

procedure TMainForm.OnNickListReceived(const ChannelName: string; List: TStrings);
var
  Channel: TChannel;
begin
  Channel := FChannelList.ChannelByName(ChannelName);
  AddNicksToChannel(Channel, List);
  AddNicksToTreeView(Channel);
end;

procedure TMainForm.OnUserJoined(const ChannelName, Nick: string);
var
  Channel: TChannel;
  User: TUser;
begin
  Channel := FChannelList.ChannelByName(ChannelName);

  User := TUser.Create(Nick);
  Channel.Users.Add(User);
  AddUserToTreeView(User, Channel);
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Application.OnActivate := @ApplicationActivate;

  TrayIcon.Icons := ImageListTrayIcons;
  TrayIcon.AnimateInterval := 1250;

  FChannelList := TChannelList.Create(Self);

  FIRC := TIRC.Create(FChannelList);
  FIRC.OnMessageReceived := @OnMessageReceived;
  FIRC.OnNickListReceived := @OnNickListReceived;
  FIRC.OnUserJoined := @OnUserJoined;
  FIRC.OnChannelJoined := @OnChannelJoined;
  FIRC.OnShowPopup := @OnShowPopup;

  ConfigureMemo(MemoServidor);
  SetColors;
end;

destructor TMainForm.Destroy;
begin
  FIRC.Free;
  FChannelList.Free;
  inherited Destroy;
end;

end.
