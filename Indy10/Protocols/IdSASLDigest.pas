unit IdSASLDigest;

interface
{$i IdCompilerDefines.inc}
uses
  Classes,
  SysUtils, //here to facilitate inline expansion
   IdSASL, IdSASLUserPass, IdUserPassProvider, IdException;

type
  TIdSASLDigest = class(TIdSASLUserPass)
  protected
    Fauthzid : String;
  public
    function StartAuthenticate(const AChallenge, AHost, AProtocolName:string) : String; override;
    function ContinueAuthenticate(const ALastResponse, AHost, AProtocolName : string): string; override;
    class function ServiceName: TIdSASLServiceName; override;
    function IsReadyToStart: Boolean; override;
  published
    property authzid : String read Fauthzid write Fauthzid;
  end;
  EIdSASLDigestException = class(EIdException);
  EIdSASLDigestChallException = class(EIdSASLDigestException);
  EIdSASLDigestChallNoAlgorithm = class(EIdSASLDigestChallException);
  EIdSASLDigestChallInvalidAlg  = class(EIdSASLDigestChallException);
  EIdSASLDigestAuthConfNotSupported = class(EIdSASLDigestException);

                                       

function CalcDigestResponse(const AUserName, APassword, ARealm, ANonce, ACNonce : String;
  const ANC : Integer;
  const  AQop, ADigestURI : String; const AAuthzid : String = '') : String;

implementation
uses IdFIPS, IdGlobal, IdGlobalProtocols, IdHash, IdHashMessageDigest, IdResourceStringsProtocols;

const
  SASL_DIGEST_METHOD = 'AUTHENTICATE:';  {do not localize}

function NCToStr(const AValue : Integer):String;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  Result := IntToHex(AValue,8);
end;

function RemoveQuote(const aStr:string):string;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  if (Length(aStr)>=2) and (aStr[1]='"') and (astr[Length(aStr)]='"') then begin
    Result := Copy(aStr, 2, Length(astr)-2)
  end else begin
    Result := aStr;
  end;
end;

//
function HashResult(const AStr : String): TIdBytes;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  with TIdHashMessageDigest5.Create do
  try
    Result := HashString(AStr);
  finally
    Free;
  end;
end;

function HashResultAsHex(const ABytes : TIdBytes) : String;  overload;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  with TIdHashMessageDigest5.Create do
  try
    Result := LowerCase(HashBytesAsHex(ABytes));
  finally
    Free;
  end;
end;

function HashResultAsHex(const AStr : String) : String; overload;
{$IFDEF USE_INLINE} inline; {$ENDIF}
begin
  with TIdHashMessageDigest5.Create do
  try
    Result := LowerCase(HashStringAsHex(AStr));
  finally
    Free;
  end;
end;

function CalcDigestResponse(const AUserName, APassword, ARealm, ANonce, ACNonce : String;
  const ANC : Integer; const  AQop, ADigestURI : String; const AAuthzid : String = '') : String;
var
  LA1 : TIdBytes;
   LA2: TIdBytes;
  LA1_P : TIdBytes;
begin
  LA1_P := IdGlobal.ToBytes(':' + ANonce + ':' + ACNonce);
  LA1 :=  HashResult(AUserName + ':' + ARealm + ':' +
    APassword);
  IdGlobal.AppendBytes(LA1,LA1_P);
  If AAuthzid <> '' then begin
    IdGlobal.AppendBytes(LA1,IdGlobal.ToBytes(AAuthzid));
  end;
  if AQop = 'auth' then begin
     LA2 := ToBytes('AUTHENTICATE:' + ADigestURI);
  end
  else if (AQop = 'auth-int') or (AQop = 'auth-conf') then begin
    LA2 := ToBytes('AUTHENTICATE:' + ADigestURI + ':00000000000000000000000000000000');
  end else begin
   SetLength(LA2,0);
  end;
  Result := HashResultAsHex(HashResultAsHex(LA1) + ':' + ANonce + ':' +
     NCToStr(ANC) + ':' + ACNonce + ':' + AQop +':' + HashResultAsHex(LA2));
end;
//

{ TIdSASLDigest }

function TIdSASLDigest.ContinueAuthenticate(const ALastResponse, AHost,
  AProtocolName: string): string;
begin
  Result := '';
end;

function TIdSASLDigest.IsReadyToStart: Boolean;
begin
  Result := not GetFIPSMode;
end;

class function TIdSASLDigest.ServiceName: TIdSASLServiceName;

begin
  Result := 'DIGEST-MD5';
end;

function TIdSASLDigest.StartAuthenticate(const AChallenge, AHost, AProtocolName: string): String;
var
  LBuf : String;
  LChallange: TStringList;
  LReply : TStringList;
  Lqop : String;
  LstrCNonce : String;
  i : Integer;
  LstrResponse : String;
  LURL : String;
  LCharset : String;
  LQopOptions: TStrings;
  LAlgorithm : String;
  LNonce : String;
  LRealm: String;

begin
  LURL := AProtocolName+'/'+AHost;
  LReply := TStringList.Create;
  LChallange := TStringList.Create;
  LQopOptions:= TStringList.Create;
  try
    LBuf := AChallenge;
    while Length(LBuf) > 0 do begin
      LChallange.Add(Fetch(LBuf,','));
    end;
    for i := LChallange.Count-1 downto 0 do
    begin
      {$IFDEF HAS_TStrings_ValueFromIndex}
      LChallange.ValueFromIndex[i] := RemoveQuote(LChallange.ValueFromIndex[i]);
      {$ELSE}
      LChallange.Values[LChallange.Names[i]] := RemoveQuote(LChallange.Values[LChallange.Names[i]]);
      {$ENDIF}
    end;
    LQopOptions.CommaText := LChallange.Values['qop'];
    Lqop := 'auth';
    if LQopOptions.IndexOf('auth-int') > -1 then
    begin
      Lqop := 'auth-int';
    end;
    if LQopOptions.IndexOf('auth-conf') > -1 then begin
      if LQopOptions.IndexOf('auth') = -1 then begin
        EIdSASLDigestAuthConfNotSupported.Toss(RSSASLDigestAuthConfNotSupported);
      end;
    end;
    LNonce := LChallange.Values['nonce'];
    LRealm :=  LChallange.Values['realm'];
    LAlgorithm :=  LChallange.Values['algorithm'];
    if LAlgorithm = '' then begin
      EIdSASLDigestChallNoAlgorithm.Toss(RSSASLDigestMissingAlgorithm);
    end;
    {
    if LAlgorithm <> 'md5-sess' then begin
      EIdSASLDigestChallInvalidAlg.Toss(RSSASLDigestInvalidAlgorithm);
    end;
    }

    //Commented out for case test mentioned in RFC 2831
    LstrCNonce := HashResultAsHex(DateTimeToStr(Now));

    LCharset :=  LChallange.Values['charset'];

    LstrResponse := CalcDigestResponse(GetUserName,Self.GetPassword,LRealm,LNonce,LstrCNonce,
     1, Lqop, LURL,Fauthzid);


//    if LQopOptions.IndexOf('auth-conf') > -1 then begin
//      if LQopOptions.IndexOf('auth') = -1 then begin
//        EIdSASLDigestAuthConfNotSupported.Toss(RSSASLDigestAuthConfNotSupported);
//      end;
//    end;

   if LCharset = '' then begin
     Result := '';
   end else begin
     Result := 'charset='+LCharset+',';
   end;
{
#( username | realm | nonce | cnonce |
    nonce-count | qop | digest-uri | response |
    maxbuf | charset | cipher | authzid |
    auth-param )
}
    Result := Result + 'username="'+GetUsername+'"'+
     ',realm="'+LRealm+'"'+ {Do not localize}
      ',nonce="'+ LNonce+'"'+
      ',nc='+NCToStr(1)+
      ',cnonce="'+LstrCNonce+'"'+
      ',digest-uri="'+LURL+'"'+
      ',response='+LstrResponse+
      ',qop='+Lqop;
  finally
    FreeAndNil(LQopOptions);
    FreeAndNil(LChallange);
    FreeAndNil(LReply);
  end;
end;

end.
