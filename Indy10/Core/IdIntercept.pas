{
  $Project$
  $Workfile$
  $Revision$
  $DateUTC$
  $Id$

  This file is part of the Indy (Internet Direct) project, and is offered
  under the dual-licensing agreement described on the Indy website.
  (http://www.indyproject.org/)

  Copyright:
   (c) 1993-2005, Chad Z. Hower and the Indy Pit Crew. All rights reserved.
}
{
  $Log$
}
{
  Rev 1.10    3/10/2005 12:00:46 AM  JPMugaas
  Minor problem Craig Peterson had noted in an E-Mail to me.

  Rev 1.9    11/30/04 6:19:12 PM  RLebeau
  Promoted the TIdConnectionIntercept.Intercept property from protected to
  published

  Rev 1.8    2004.02.03 4:16:44 PM  czhower
  For unit name changes.

  Rev 1.7    2004.01.20 10:03:24 PM  czhower
  InitComponent

  Rev 1.6    5/12/2003 12:33:32 AM  GGrieve
  add Data from BlockCipher descendent

  Rev 1.5    2003.10.14 1:26:48 PM  czhower
  Uupdates + Intercept support

  Rev 1.4    2003.10.11 5:48:16 PM  czhower
  -VCL fixes for servers
  -Chain suport for servers (Super core)
  -Scheduler upgrades
  -Full yarn support

  Rev 1.3    10/5/2003 3:20:46 PM  BGooijen
  .net

  Rev 1.2    2003.10.01 1:12:34 AM  czhower
  .Net

  Rev 1.1    3/5/2003 10:59:48 PM  BGooijen
  Fixed (i know, the SendBuffer looks bad)

  Rev 1.0    11/13/2002 08:44:42 AM  JPMugaas

  2002-03-01 - Andrew P.Rybin
    - Nested Intercept support (ex: ->logging->compression->encryption)

  2002-04-09 - Chuck Smith
    - set ABuffer.Position := 0; in OnSend/OnReceive for Nested Stream send/receive
}

unit IdIntercept;

interface

{$I IdCompilerDefines.inc}
//here only to put FPC in Delphi mode

uses
  Classes,
  IdGlobal, IdBaseComponent, IdBuffer, IdException;

type
  EIdInterceptCircularLink = class(EIdException);
  TIdConnectionIntercept = class;
  TIdInterceptNotifyEvent = procedure(ASender: TIdConnectionIntercept) of object;
  TIdInterceptStreamEvent = procedure(ASender: TIdConnectionIntercept; var ABuffer: TIdBytes) of object;

  TIdConnectionIntercept = class(TIdBaseComponent)
  protected
    FConnection: TComponent;
    FIntercept: TIdConnectionIntercept;
    FIsClient: Boolean;
    FData: TObject;

    FOnConnect: TIdInterceptNotifyEvent;
    FOnDisconnect: TIdInterceptNotifyEvent;
    FOnReceive: TIdInterceptStreamEvent;
    FOnSend: TIdInterceptStreamEvent;
    //
    procedure InitComponent; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetIntercept(AValue: TIdConnectionIntercept);
    //
  public
    procedure Connect(AConnection: TComponent); virtual;
    procedure Disconnect; virtual;
    procedure Receive(var VBuffer: TIdBytes); virtual;
    procedure Send(var VBuffer: TIdBytes); virtual;
    //
    property Connection: TComponent read FConnection;
    property IsClient: Boolean read FIsClient;
    property Data: TObject read FData write FData; // user can use this to keep context
  published
    property Intercept: TIdConnectionIntercept read FIntercept write SetIntercept;
    property OnConnect: TIdInterceptNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TIdInterceptNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnReceive: TIdInterceptStreamEvent read FOnReceive write FOnReceive;
    property OnSend: TIdInterceptStreamEvent read FOnSend write FOnSend;
  end;

  TIdServerIntercept = class(TIdBaseComponent)
  public
    procedure Init; virtual; abstract;
    function Accept(AConnection: TComponent): TIdConnectionIntercept; virtual; abstract;
  end;

implementation
uses
  IdResourceStringsCore;

{ TIdIntercept }

procedure TIdConnectionIntercept.Disconnect;
begin
  if Intercept <> nil then begin
    Intercept.Disconnect;
  end;
  if Assigned(OnDisconnect) then begin
    OnDisconnect(Self);
  end;
  FConnection := nil;
end;

procedure TIdConnectionIntercept.Connect(AConnection: TComponent);
begin
  FConnection := AConnection;
  if Assigned(OnConnect) then begin
    OnConnect(Self);
  end;
  if Intercept <> nil then begin
    Intercept.Connect(AConnection);
  end;
end;

procedure TIdConnectionIntercept.Receive(var VBuffer: TIdBytes);
begin
  if Intercept <> nil then begin
    Intercept.Receive(VBuffer);
  end;
  if Assigned(OnReceive) then begin
    OnReceive(Self, VBuffer);
  end;
end;

procedure TIdConnectionIntercept.Send(var VBuffer: TIdBytes);
begin
  if Assigned(OnSend) then begin
    OnSend(Self, VBuffer);
  end;
  if Intercept <> nil then begin
    Intercept.Send(VBuffer);
  end;
end;

procedure TIdConnectionIntercept.SetIntercept(AValue: TIdConnectionIntercept);
var
  LIntercept: TIdConnectionIntercept;
Begin
  LIntercept := AValue;
  while Assigned(LIntercept) do begin
    if LIntercept = Self then begin //recursion
      raise EIdInterceptCircularLink.CreateFmt(RSInterceptCircularLink, [ClassName]); // TODO: Resource string and more english
    end;
    LIntercept := LIntercept.Intercept;
  end;

  // remove self from the Intercept's free notification list    {Do not Localize}
  if Assigned(FIntercept) then begin
    FIntercept.RemoveFreeNotification(Self);
  end;
  FIntercept := AValue;
  // add self to the Intercept's free notification list    {Do not Localize}
  if Assigned(FIntercept) then begin
    FIntercept.FreeNotification(Self);
  end;
end;

procedure TIdConnectionIntercept.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, OPeration);
  if (Operation = opRemove) and (AComponent = Intercept) then begin
    FIntercept := nil;
  end;
end;

procedure TIdConnectionIntercept.InitComponent;
begin
  inherited InitComponent;
  FIsClient := True;
end;

end.
