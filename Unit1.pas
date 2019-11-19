unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, clImap4FileHandler, clImap4Server, clPop3FileHandler,
  clPop3Server, clSmtpFileHandler, clTcpServer, clSmtpServer, clUtils, clUserMgr,
  clLogger, IniFiles, ExtCtrls, clTcpClient, clMC, clSmtp, clSmtpRelay,
  clTcpCommandServer, clMailUserMgr, clTcpClientTls, clTcpCommandClient,
  clTcpServerTls;

type
  TForm1 = class(TForm)
    clSmtpServer1: TclSmtpServer;
    clSmtpFileHandler1: TclSmtpFileHandler;
    clPop3Server1: TclPop3Server;
    clPop3FileHandler1: TclPop3FileHandler;
    clImap4Server1: TclImap4Server;
    clImap4FileHandler1: TclImap4FileHandler;
    clSmtpRelay1: TclSmtpRelay;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure clReceiveCommand(Sender: TObject;
      AConnection: TclCommandConnection; ACommandParams: TclTcpCommandParams);
    procedure clSendResponse(Sender: TObject;
      AConnection: TclCommandConnection; const ACommand, AText: String);
    procedure clAcceptConnection(Sender: TObject;
      AConnection: TclUserConnection; var Handled: Boolean);
    procedure clCloseConnection(Sender: TObject; AConnection: TclUserConnection);
    procedure clServerError(Sender: TObject; AConnection: TclUserConnection; E: Exception);
    procedure FormDestroy(Sender: TObject);
    procedure clServerStart(Sender: TObject);
    procedure clServerStop(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure clSmtpRelay1SendCommand(Sender: TObject;
      const AText: String);
    procedure clSmtpRelay1ReceiveResponse(Sender: TObject;
      AList: TStrings);
  private
    FBadPath: string;
    procedure LoadImap(Ini: TIniFile);
    procedure LoadPop3(Ini: TIniFile);
    procedure LoadSmtp(Ini: TIniFile);
    procedure LoadRelay(Ini: TIniFile);
    function GetSettingsFile: string;
    function GetSmtpSettingsFile(Ini: TIniFile): string;
    function GetMailBoxDir(Ini: TIniFile): string;
    procedure SaveSmtp(Ini: TIniFile);
    function GetLogFile: string;
    procedure ProcessBounces(const AMessageFile: string);
    procedure ExtractRelayTo(AEnvelope, AMailToList: TStrings);
    procedure CopyMessageFile(AStatus: TclSmtpRelayStatus; const ACopyFrom, ACopyTo: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.GetLogFile: string;
begin
  Result := ChangeFileExt(ParamStr(0), '.log');
end;

function TForm1.GetSettingsFile: string;
begin
  Result := AddTrailingBackSlash(ExtractFilePath(ParamStr(0))) + 'settings.ini';
end;

function TForm1.GetMailBoxDir(Ini: TIniFile): string;
begin
  Result := Ini.ReadString('MAILBOX', 'MailboxDir', 'C:\CleverMailBox');
end;

function TForm1.GetSmtpSettingsFile(Ini: TIniFile): string;
begin
  Result := AddTrailingBackSlash(GetMailBoxDir(Ini)) + 'smtp.dat';
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ini: TIniFile;
begin
  TclLogger.Instance().SetLogMessageFile(GetLogFile());

  if not FileExists(GetSettingsFile()) then
  begin
    ShowMessage(GetSettingsFile() + ' does not exist');
    Halt;
  end;

  ini := TIniFile.Create(GetSettingsFile());
  try
    LoadSmtp(ini);
    LoadPop3(ini);
    LoadImap(ini);
    LoadRelay(ini);
  finally
    ini.Free();
  end;

  ForceFileDirectories(AddTrailingBackSlash(clSmtpFileHandler1.MailBoxDir));
  ForceFileDirectories(AddTrailingBackSlash(clSmtpFileHandler1.RelayDir));
  ForceFileDirectories(AddTrailingBackSlash(FBadPath));

  clSmtpServer1.Start();
  clPop3Server1.Start();
  clImap4Server1.Start();
  Timer1.Enabled := True;
end;

procedure TForm1.LoadSmtp(Ini: TIniFile);
var
  i, cnt: Integer;
  counter: TIniFile;
  account: TclMailUserAccountItem;
begin
  clSmtpServer1.Port := Ini.ReadInteger('SMTP', 'Port', 25);

  clSmtpFileHandler1.MailBoxDir := GetMailBoxDir(Ini);
  clSmtpFileHandler1.RelayDir := Ini.ReadString('SMTP', 'RelayDir', 'C:\CleverMailBox\RelayQueue');

  counter := TIniFile.Create(GetSmtpSettingsFile(Ini));
  try
    clSmtpFileHandler1.Counter := counter.ReadInteger('SMTP', 'Counter', 1);
  finally
    counter.Free();
  end;

  clSmtpServer1.UserAccounts.Clear();
  cnt := Ini.ReadInteger('USERS', 'Count', 0);
  for i := 0 to cnt - 1 do
  begin
    account := clSmtpServer1.UserAccounts.Add();

    account.UserName := Ini.ReadString('USER' + IntToStr(i), 'UserName', '');
    account.Password := Ini.ReadString('USER' + IntToStr(i), 'Password', '');
    account.Email := Ini.ReadString('USER' + IntToStr(i), 'Email', '');
  end;
end;

procedure TForm1.SaveSmtp(Ini: TIniFile);
var
  counter: TIniFile;
begin
  counter := TIniFile.Create(GetSmtpSettingsFile(Ini));
  try
    counter.WriteInteger('SMTP', 'Counter', clSmtpFileHandler1.Counter);
  finally
    counter.Free();
  end;
end;

procedure TForm1.LoadPop3(Ini: TIniFile);
var
  i, cnt: Integer;
  account: TclUserAccountItem;
begin
  clPop3Server1.Port := Ini.ReadInteger('POP3', 'Port', 110);
  clPop3FileHandler1.MailBoxDir := GetMailBoxDir(Ini);

  clPop3Server1.UserAccounts.Clear();
  cnt := Ini.ReadInteger('USERS', 'Count', 0);
  for i := 0 to cnt - 1 do
  begin
    account := clPop3Server1.UserAccounts.Add();

    account.UserName := Ini.ReadString('USER' + IntToStr(i), 'UserName', '');
    account.Password := Ini.ReadString('USER' + IntToStr(i), 'Password', '');
  end;
end;

procedure TForm1.LoadImap(Ini: TIniFile);
var
  i, cnt: Integer;
  account: TclUserAccountItem;
begin
  clImap4Server1.Port := Ini.ReadInteger('IMAP', 'Port', 143);
  clImap4FileHandler1.MailBoxDir := GetMailBoxDir(Ini);

  clImap4Server1.UserAccounts.Clear();
  cnt := Ini.ReadInteger('USERS', 'Count', 0);
  for i := 0 to cnt - 1 do
  begin
    account := clImap4Server1.UserAccounts.Add();

    account.UserName := Ini.ReadString('USER' + IntToStr(i), 'UserName', '');
    account.Password := Ini.ReadString('USER' + IntToStr(i), 'Password', '');
  end;
end;

procedure TForm1.LoadRelay(Ini: TIniFile);
begin
  clSmtpRelay1.DnsServer := Ini.ReadString('SMTP', 'DnsServer', '');
  FBadPath := Ini.ReadString('SMTP', 'BadDir', 'C:\CleverMailBox\Bad');
  Timer1.Interval := Ini.ReadInteger('SMTP', 'RelayInterval', 5000);
end;

procedure TForm1.clReceiveCommand(Sender: TObject;
  AConnection: TclCommandConnection; ACommandParams: TclTcpCommandParams);
begin
  clPutLogMessage(Sender, edInside, 'Command: ' + ACommandParams.Command + ' ' + ACommandParams.Parameters);
end;

procedure TForm1.clSendResponse(Sender: TObject;
  AConnection: TclCommandConnection; const ACommand, AText: String);
begin
  clPutLogMessage(Sender, edInside, 'Reply: ' + ACommand);
end;

procedure TForm1.clAcceptConnection(Sender: TObject; AConnection: TclUserConnection; var Handled: Boolean);
begin
  clPutLogMessage(Sender, edInside, 'Accept Connection. Host: ' + AConnection.PeerIP);
end;

procedure TForm1.clCloseConnection(Sender: TObject; AConnection: TclUserConnection);
begin
  clPutLogMessage(Sender, edInside, 'Close Connection. Host: ' + AConnection.PeerIP);
end;

procedure TForm1.clServerError(Sender: TObject; AConnection: TclUserConnection; E: Exception);
begin
  clPutLogMessage(Sender, edInside, 'Exception !!!', E);
end;

procedure TForm1.clSmtpRelay1SendCommand(Sender: TObject; const AText: String);
begin
  clPutLogMessage(Sender, edInside, 'Send Relay Command: ' + Trim(AText));
end;

procedure TForm1.clSmtpRelay1ReceiveResponse(Sender: TObject; AList: TStrings);
begin
  clPutLogMessage(Sender, edInside, 'Receive Relay Response: ' + Trim(AList.Text));
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  ini: TIniFile;
begin
  Timer1.Enabled := False;

  if FileExists(GetSettingsFile()) then
  begin
    ini := TIniFile.Create(GetSettingsFile());
    try
      SaveSmtp(ini);
    finally
      ini.Free();
    end;
  end;

  clImap4Server1.Stop();
  clPop3Server1.Stop();
  clSmtpServer1.Stop();
end;

procedure TForm1.clServerStart(Sender: TObject);
begin
  clPutLogMessage(Sender, edInside, 'Start service');
end;

procedure TForm1.clServerStop(Sender: TObject);
begin
  clPutLogMessage(Sender, edInside, 'Stop service');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  path: string;
  searchRec: TSearchRec;
  envelope: TStrings;
begin
  envelope := TStringList.Create();
  try
    Timer1.Enabled := False;
    try
      path := AddTrailingBackSlash(clSmtpFileHandler1.RelayDir);

      if SysUtils.FindFirst(path + '*' + cEnvelopeFileExt, 0, searchRec) = 0 then
      begin
        clPutLogMessage(Sender, edEnter, 'Process Queue');
        try
          repeat
            envelope.LoadFromFile(path + searchRec.Name);
            if (envelope.Count < 2) then Continue;

            clSmtpRelay1.MailFrom := envelope[0];
            ExtractRelayTo(envelope, clSmtpRelay1.MailToList);

            clSmtpRelay1.MailData.LoadFromFile(path + ChangeFileExt(searchRec.Name, cMessageFileExt));

            clSmtpRelay1.Send();

            ProcessBounces(path + searchRec.Name);

            DeleteFile(path + searchRec.Name);
            DeleteFile(path + ChangeFileExt(searchRec.Name, cMessageFileExt));

          until (SysUtils.FindNext(searchRec) <> 0);
          SysUtils.FindClose(searchRec);
        finally
          clPutLogMessage(Sender, edLeave, 'Process Queue');
        end;
      end;
    except
      on E: Exception do
      begin
        clPutLogMessage(Sender, edInside, 'Process Queue', E);
      end;
    end;
    Timer1.Enabled := True;
  finally
    envelope.Free();
  end;
end;

procedure TForm1.ExtractRelayTo(AEnvelope, AMailToList: TStrings);
var
  i: Integer;
begin
  AMailToList.Clear();

  for i := 1 to AEnvelope.Count - 1 do
  begin
    AMailToList.Add(AEnvelope[i]);
  end;
end;

procedure TForm1.CopyMessageFile(AStatus: TclSmtpRelayStatus; const ACopyFrom, ACopyTo: string);
const
  copyStatus: array[Boolean] of string = ('failed to copy', 'ok');
var
  success: Boolean;
begin
  success := CopyFile(PChar(ACopyFrom), PChar(ACopyTo), True);
  clPutLogMessage(clSmtpRelay1, edInside, Format('Delivery error: code = %d, error = %d, info = %s, %s copied to Bad folder - %s',
    [AStatus.ResponseCode, AStatus.ErrorCode, AStatus.ErrorText, ACopyFrom, copyStatus[success]]));
end;

procedure TForm1.ProcessBounces(const AMessageFile: string);
var
  i: Integer;
  copyFrom, copyTo: string;
  status: TclSmtpRelayStatus;
begin
  for i := 0 to clSmtpRelay1.StatusList.Count - 1 do
  begin
    status := clSmtpRelay1.StatusList[i];
    if (status.ResponseCode <> 250) or (status.ErrorCode <> 0) then
    begin
      copyFrom := AMessageFile;
      copyTo := AddTrailingBackSlash(FBadPath) + ExtractFileName(copyFrom);

      CopyMessageFile(status, copyFrom, copyTo);

      copyFrom := ChangeFileExt(AMessageFile, cMessageFileExt);
      copyTo := AddTrailingBackSlash(FBadPath) + ExtractFileName(copyFrom);

      CopyMessageFile(status, copyFrom, copyTo);

      Break;
    end;
  end;
end;

end.
