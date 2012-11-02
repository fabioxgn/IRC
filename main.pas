unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Dialogs, StdCtrls, ComCtrls, Menus, ActnList, Windows, IRC;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionCanal: TAction;
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
    MenuCanal: TMenuItem;
    MenuServidor: TMenuItem;
    PageControl: TPageControl;
    TabSheetServidor: TTabSheet;
    procedure ActionCanalExecute(Sender: TObject);
    procedure ActionCloseTabExecute(Sender: TObject);
    procedure ActionConectarExecute(Sender: TObject);
    procedure ActionDesconectarExecute(Sender: TObject);
    procedure EditMensagemKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PageControlChange(Sender: TObject);
  private
    FIRC: TIRC;
    function NovoCanal(const Canal: string): TStrings;
    procedure FecharAba(const Tab: TTabSheet);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.ActionDesconectarExecute(Sender: TObject);
begin
  FIRC.Disconnect;
end;

procedure TMainForm.ActionConectarExecute(Sender: TObject);
begin
  FIRC.Connect;
end;

procedure TMainForm.ActionCanalExecute(Sender: TObject);
var
  Canal: string;
begin
  Canal := InputBox('Canal', 'Informe o Canal', '');

  FIRC.JoinChannel(Canal, NovoCanal(Canal));
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

constructor TMainForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FIRC := TIRC.Create;
end;

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FIRC.Log := MemoServidor.Lines;
end;

destructor TMainForm.Destroy;
begin
  FIRC.Free;
  inherited Destroy;
end;

end.
