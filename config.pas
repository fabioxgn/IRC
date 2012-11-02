unit config;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IniFiles;

const
  DefaultConfigFile = 'config.ini';

type

  { TIRCConfig }

  TIRCConfig = class
  private
    FHost: string;
    FPort: Integer;
    FUsername: string;
    FNickname: string;
    FRealName: string;
    FAltNickname: string;
    FChannels: TStrings;
    FIni: TIniFile;
  public
    procedure Save;
    procedure Load;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Username: string read FUsername write FUsername;
    property Nickname: string read FNickname write FNickname;
    property RealName: string read FRealName write FRealName;
    property AltNickname: string read FAltNickname write FAltNickname;
    property Channels: TStrings read FChannels;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

const
  SectionServer = 'Server';
  SectionUser = 'User';
  SectionChannels = 'Channels';

{ TIRCConfig }

procedure TIRCConfig.Save;
begin
  FIni.WriteString(SectionServer, 'Host', FHost);
  FIni.WriteInteger(SectionServer, 'Port', FPort);

  FIni.WriteString(SectionUser, 'Username', FUsername);
  FIni.WriteString(SectionUser, 'Nickname', FNickname);
  FIni.WriteString(SectionUser, 'RealName', FRealName);
  FIni.WriteString(SectionUser, 'AltNickname', FAltNickname);

  FIni.WriteString(SectionChannels, 'AutoJoin', FChannels.CommaText);

  FIni.UpdateFile;
end;

procedure TIRCConfig.Load;
var
  Channel: string;
begin
  FHost := FIni.ReadString(SectionServer, 'Host', '');
  FPort := FIni.ReadInteger(SectionServer, 'Port', 6667);

  FUsername := FIni.ReadString(SectionUser, 'Username', '');
  FNickname := FIni.ReadString(SectionUser, 'Nickname', '');
  FRealName := FIni.ReadString(SectionUser, 'RealName', '');
  FAltNickname := FIni.ReadString(SectionUser, 'AltNickname', '');

  FChannels.CommaText := FIni.ReadString(SectionChannels, 'AutoJoin', '');
end;

constructor TIRCConfig.Create;
begin
  FChannels := TStringList.Create;
  Fini := TIniFile.Create('config.ini');
end;

destructor TIRCConfig.Destroy;
begin
  FChannels.Free;
  FIni.Free;
  inherited Destroy;
end;

end.

