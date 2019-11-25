unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, clImap4FileHandler, clImap4Server, clPop3FileHandler,
  clPop3Server, clSmtpFileHandler, clTcpServer, clSmtpServer, clUtils, clUserMgr,
  clLogger, IniFiles, ExtCtrls, clTcpClient, clMC, clSmtp, clSmtpRelay,
  clTcpCommandServer, clMailUserMgr, clTcpClientTls, clTcpCommandClient,
  clTcpServerTls, Vcl.StdCtrls, Vcl.ComCtrls;

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
    btnStart: TButton;
    btnStop: TButton;
    pcSettings: TPageControl;
    tsUserAccounts: TTabSheet;
    tsSmtp: TTabSheet;
    tsPop3: TTabSheet;
    tsImap: TTabSheet;
    pnlLogo: TPanel;
    imLogoLeft: TImage;
    imLogoMiggle: TImage;
    imLogoRight: TImage;
    lbUsers: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    edtUser: TEdit;
    edtPassword: TEdit;
    btnAddUser: TButton;
    btnDeleteUser: TButton;
    Label3: TLabel;
    edtSmtpPort: TEdit;
    Label4: TLabel;
    edtMailboxFolder: TEdit;
    Label5: TLabel;
    edtPop3Port: TEdit;
    Label6: TLabel;
    edtImapPort: TEdit;
    Label7: TLabel;
    edtRelayFolder: TEdit;
    Label8: TLabel;
    edtBadFolder: TEdit;
    Label9: TLabel;
    edtDnsServer: TEdit;
    Label10: TLabel;
    edtRelayInterval: TEdit;
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
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure lbUsersClick(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure btnDeleteUserClick(Sender: TObject);
    procedure edtUserChange(Sender: TObject);
  private
    FUserAccounts: TclMailUserAccountList;
    FUserAccountChanging: Boolean;

    function GetSettingsFile: string;
    function GetSmtpSettingsFile: string;
    function GetLogFile: string;

    procedure LoadSmtpCounter;
    procedure SaveSmtpCounter;

    procedure LoadSettings;
    procedure SaveSettings;
    procedure UpdateSettings;

    procedure UpdateControls(AStarted: Boolean);

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

function TForm1.GetSmtpSettingsFile: string;
begin
  Result := AddTrailingBackSlash(edtMailboxFolder.Text) + 'smtp.dat';
end;

procedure TForm1.lbUsersClick(Sender: TObject);
var
  account: TclMailUserAccountItem;
begin
  FUserAccountChanging := True;
  try
    if (lbUsers.ItemIndex > -1) then
    begin
      account := lbUsers.Items.Objects[lbUsers.ItemIndex] as TclMailUserAccountItem;
      edtUser.Text := account.UserName;
      edtPassword.Text := account.Password;
    end else
    begin
      edtUser.Text := '';
      edtPassword.Text := '';
    end;
  finally
    FUserAccountChanging := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FUserAccounts := TclMailUserAccountList.Create(Self, TclMailUserAccountItem);

  TclLogger.Instance().SetLogMessageFile(GetLogFile());
  UpdateControls(False);
  LoadSettings();
end;

procedure TForm1.LoadSettings;
var
  i, cnt: Integer;
  ini: TIniFile;
  account: TclMailUserAccountItem;
begin
  if FileExists(GetSettingsFile()) then
  begin
    ini := TIniFile.Create(GetSettingsFile());
    try
      edtRelayInterval.Text := IntToStr(Ini.ReadInteger('SMTP', 'RelayInterval', 5000));
      edtSmtpPort.Text := IntToStr(Ini.ReadInteger('SMTP', 'Port', 25));
      edtDnsServer.Text := Ini.ReadString('SMTP', 'DnsServer', '');
      edtRelayFolder.Text := Ini.ReadString('SMTP', 'RelayDir', 'C:\CleverMailBox\RelayQueue');
      edtBadFolder.Text := Ini.ReadString('SMTP', 'BadDir', 'C:\CleverMailBox\Bad');

      edtMailboxFolder.Text := Ini.ReadString('MAILBOX', 'MailboxDir', 'C:\CleverMailBox');

      edtPop3Port.Text := IntToStr(Ini.ReadInteger('POP3', 'Port', 110));

      edtImapPort.Text := IntToStr(Ini.ReadInteger('IMAP', 'Port', 143));

      lbUsers.Items.Clear();

      cnt := Ini.ReadInteger('USERS', 'Count', 0);
      for i := 0 to cnt - 1 do
      begin
        account := FUserAccounts.Add();

        account.UserName := Ini.ReadString('USER' + IntToStr(i), 'UserName', '');
        account.Password := Ini.ReadString('USER' + IntToStr(i), 'Password', '');
        account.Email := account.UserName;

        lbUsers.Items.AddObject(account.UserName, account);
      end;
    finally
      ini.Free();
    end;
  end;
end;

procedure TForm1.LoadSmtpCounter;
var
  counter: TIniFile;
begin
  counter := TIniFile.Create(GetSmtpSettingsFile());
  try
    clSmtpFileHandler1.Counter := counter.ReadInteger('SMTP', 'Counter', 1);
  finally
    counter.Free();
  end;
end;

procedure TForm1.SaveSettings;
var
  i: Integer;
  ini: TIniFile;
  account: TclMailUserAccountItem;
begin
  ini := TIniFile.Create(GetSettingsFile());
  try
    Ini.WriteInteger('SMTP', 'RelayInterval', StrToIntDef(edtRelayInterval.Text, 5000));
    Ini.WriteInteger('SMTP', 'Port', StrToIntDef(edtSmtpPort.Text, 25));
    Ini.WriteString('SMTP', 'DnsServer', edtDnsServer.Text);
    Ini.WriteString('SMTP', 'RelayDir', edtRelayFolder.Text);
    Ini.WriteString('SMTP', 'BadDir', edtBadFolder.Text);

    Ini.WriteString('MAILBOX', 'MailboxDir', edtMailboxFolder.Text);

    Ini.WriteInteger('POP3', 'Port', StrToIntDef(edtPop3Port.Text, 110));

    Ini.WriteInteger('IMAP', 'Port', StrToIntDef(edtImapPort.Text, 143));

    Ini.WriteInteger('USERS', 'Count', FUserAccounts.Count);
    for i := 0 to FUserAccounts.Count - 1 do
    begin
      account := FUserAccounts[i];

      Ini.WriteString('USER' + IntToStr(i), 'UserName', account.UserName);
      Ini.WriteString('USER' + IntToStr(i), 'Password', account.Password);
    end;
  finally
    ini.Free();
  end;
end;

procedure TForm1.SaveSmtpCounter;
var
  counter: TIniFile;
begin
  if not DirectoryExists(ExtractFilePath(GetSmtpSettingsFile())) then Exit;

  counter := TIniFile.Create(GetSmtpSettingsFile());
  try
    counter.WriteInteger('SMTP', 'Counter', clSmtpFileHandler1.Counter);
  finally
    counter.Free();
  end;
end;

procedure TForm1.btnAddUserClick(Sender: TObject);
var
  account: TclMailUserAccountItem;
begin
  FUserAccountChanging := True;
  try
    account := FUserAccounts.Add();
    account.UserName := 'New User';
    lbUsers.Items.AddObject(account.UserName, account);
    lbUsers.ItemIndex := lbUsers.Items.Count - 1;

    lbUsersClick(nil);
  finally
    FUserAccountChanging := False;
  end;
end;

procedure TForm1.btnDeleteUserClick(Sender: TObject);
begin
  FUserAccountChanging := True;
  try
    if (lbUsers.ItemIndex > -1) then
    begin
      FUserAccounts.Delete(lbUsers.ItemIndex);
      lbUsers.Items.Delete(lbUsers.ItemIndex);
      lbUsers.ItemIndex := -1;

      lbUsersClick(nil);
    end;
  finally
    FUserAccountChanging := False;
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  UpdateControls(True);
  UpdateSettings();
  LoadSmtpCounter();

  ForceFileDirectories(AddTrailingBackSlash(clSmtpFileHandler1.MailBoxDir));
  ForceFileDirectories(AddTrailingBackSlash(clSmtpFileHandler1.RelayDir));
  ForceFileDirectories(AddTrailingBackSlash(edtBadFolder.Text));

  clSmtpServer1.Start();
  clPop3Server1.Start();
  clImap4Server1.Start();

  Timer1.Enabled := True;
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

procedure TForm1.btnStopClick(Sender: TObject);
begin
  Timer1.Enabled := False;

  clImap4Server1.Stop();
  clPop3Server1.Stop();
  clSmtpServer1.Stop();

  SaveSmtpCounter();
  UpdateControls(False);
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
begin
  btnStopClick(nil);
  SaveSettings();
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

procedure TForm1.UpdateSettings;
begin
  Timer1.Interval := StrToInt(edtRelayInterval.Text);
  clSmtpRelay1.DnsServer := edtDnsServer.Text;

  clSmtpServer1.Port := StrToInt(edtSmtpPort.Text);
  clSmtpServer1.UserAccounts := FUserAccounts;

  clSmtpFileHandler1.MailBoxDir := edtMailboxFolder.Text;
  clSmtpFileHandler1.RelayDir := edtRelayFolder.Text;

  clPop3Server1.Port := StrToInt(edtPop3Port.Text);
  clPop3Server1.UserAccounts := FUserAccounts;

  clPop3FileHandler1.MailBoxDir := edtMailboxFolder.Text;

  clImap4Server1.Port := StrToInt(edtImapPort.Text);
  clImap4Server1.UserAccounts := FUserAccounts;

  clImap4FileHandler1.MailBoxDir := edtMailboxFolder.Text;
end;

procedure TForm1.UpdateControls(AStarted: Boolean);
const
  AppTitle: array[Boolean] of String = ('Clever Mail Server', 'Clever Mail Server - Started');
begin
  btnStart.Enabled := not AStarted;
  btnStop.Enabled := AStarted;

  Caption := AppTitle[AStarted];
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

procedure TForm1.edtUserChange(Sender: TObject);
var
  account: TclMailUserAccountItem;
begin
  if (lbUsers.ItemIndex > -1) and (not FUserAccountChanging) then
  begin
    account := lbUsers.Items.Objects[lbUsers.ItemIndex] as TclMailUserAccountItem;

    account.UserName := edtUser.Text;
    account.Password := edtPassword.Text;
    account.Email := account.UserName;

    lbUsers.Items[lbUsers.ItemIndex] := account.UserName;
  end;
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
      copyTo := AddTrailingBackSlash(edtBadFolder.Text) + ExtractFileName(copyFrom);

      CopyMessageFile(status, copyFrom, copyTo);

      copyFrom := ChangeFileExt(AMessageFile, cMessageFileExt);
      copyTo := AddTrailingBackSlash(edtBadFolder.Text) + ExtractFileName(copyFrom);

      CopyMessageFile(status, copyFrom, copyTo);

      Break;
    end;
  end;
end;

end.
