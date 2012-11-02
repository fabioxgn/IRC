unit ConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ActnList, ValEdit, config;

type

  { TFormConfig }

  TFormConfig = class(TForm)
    ActionOK: TAction;
    ActionList: TActionList;
    ButtonOK: TButton;
    EditHost: TLabeledEdit;
    EditPort: TLabeledEdit;
    EditUserName: TLabeledEdit;
    EditNickName: TLabeledEdit;
    EditRealName: TLabeledEdit;
    EditAltNickName: TLabeledEdit;
    Label1: TLabel;
    MemoChannels: TMemo;
    procedure ActionOKExecute(Sender: TObject);
  private
    FConfig: TIRCConfig;
    procedure CarregarConfiguracao;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TFormConfig }

procedure TFormConfig.ActionOKExecute(Sender: TObject);
var
  Port: Integer;
begin
  if not TryStrToInt(EditPort.Text, Port) then
  	 raise Exception.Create('Port value is invalid.');

  FConfig.Host := EditHost.Text;
  FConfig.Port := Port;
  FConfig.Username := EditUserName.Text;
  FConfig.Nickname := EditNickName.Text;
  FConfig.AltNickname := EditAltNickName.Text;
  FConfig.RealName := EditRealName.Text;
  FConfig.Channels.AddStrings(MemoChannels.Lines);
  FConfig.Save;
end;

procedure TFormConfig.CarregarConfiguracao;
begin
  FConfig.Load;
  EditHost.Text := FConfig.Host;
  EditPort.Text := IntToStr(FConfig.Port);
  EditUserName.Text := FConfig.Username;
  EditNickName.Text := FConfig.Nickname;
  EditAltNickName.Text := FConfig.AltNickname;
  EditRealName.Text := FConfig.RealName;
  MemoChannels.Lines.AddStrings(FConfig.Channels);
end;

constructor TFormConfig.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfig := TIRCConfig.Create;
  CarregarConfiguracao;
end;

destructor TFormConfig.Destroy;
begin
  FConfig.Free;
  inherited Destroy;
end;

end.

