unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Menus, ActnList, IdIRC, StrUtils, IdComponent, IdContext, Windows, IRC;

type

  { TMainForm }

  TMainForm = class(TForm)
    ActionCanal: TAction;
    ActionConectar: TAction;
    Desconectar: TAction;
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
    procedure ActionConectarExecute(Sender: TObject);
    procedure DesconectarExecute(Sender: TObject);
    procedure EditMensagemKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    function NovoCanal(const Canal: string): TStrings;
    procedure PageControlChange(Sender: TObject);
  private
    FIRC: TIRC;
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

procedure TMainForm.DesconectarExecute(Sender: TObject);
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

procedure TMainForm.EditMensagemKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
  FIRC.ActiveChannel := PageControl.ActivePage.Caption;
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

