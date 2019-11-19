object Form1: TForm1
  Left = 247
  Top = 114
  BorderStyle = bsToolWindow
  Caption = 'Mail Server'
  ClientHeight = 131
  ClientWidth = 261
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object clSmtpServer1: TclSmtpServer
    ServerName = 'Clever Internet Suite SMTP service'
    TLSFlags = []
    OnStart = clServerStart
    OnStop = clServerStop
    OnServerError = clServerError
    OnAcceptConnection = clAcceptConnection
    OnCloseConnection = clCloseConnection
    MaxDataSize = -1
    OnReceiveCommand = clReceiveCommand
    OnSendResponse = clSendResponse
    UserAccounts = <>
    HelpText.Strings = (
      'Commands Supported:'
      'HELO EHLO AUTH HELP QUIT MAIL NOOP RSET RCPT DATA STARTTLS')
    Left = 48
    Top = 24
  end
  object clSmtpFileHandler1: TclSmtpFileHandler
    Server = clSmtpServer1
    Left = 48
    Top = 64
  end
  object clPop3Server1: TclPop3Server
    ServerName = 'Clever Internet Suite POP3 service'
    TLSFlags = []
    OnStart = clServerStart
    OnStop = clServerStop
    OnServerError = clServerError
    OnAcceptConnection = clAcceptConnection
    OnCloseConnection = clCloseConnection
    MaxDataSize = -1
    OnReceiveCommand = clReceiveCommand
    OnSendResponse = clSendResponse
    UserAccounts = <>
    HelpText.Strings = (
      'Valid commands:'
      'USER'
      'PASS'
      'APOP'
      'AUTH'
      'QUIT'
      'NOOP'
      'HELP'
      'STAT'
      'RETR'
      'TOP'
      'DELE'
      'RSET'
      'LIST'
      'UIDL'
      'STLS')
    Left = 88
    Top = 24
  end
  object clPop3FileHandler1: TclPop3FileHandler
    Server = clPop3Server1
    Left = 88
    Top = 64
  end
  object clImap4Server1: TclImap4Server
    ServerName = 'Clever Internet Suite IMAP4 service'
    TLSFlags = []
    OnStart = clServerStart
    OnStop = clServerStop
    OnServerError = clServerError
    OnAcceptConnection = clAcceptConnection
    OnCloseConnection = clCloseConnection
    MaxDataSize = -1
    OnReceiveCommand = clReceiveCommand
    OnSendResponse = clSendResponse
    UserAccounts = <>
    Capabilities.Strings = (
      'IMAP4rev1')
    Left = 128
    Top = 24
  end
  object clImap4FileHandler1: TclImap4FileHandler
    Server = clImap4Server1
    MailBoxInfoFile = 'imap.dat'
    MessagesInfoFile = 'messages.dat'
    Left = 128
    Top = 64
  end
  object clSmtpRelay1: TclSmtpRelay
    OnSendCommand = clSmtpRelay1SendCommand
    OnReceiveResponse = clSmtpRelay1ReceiveResponse
    MailAgent = 'Clever Internet Suite v 7.2'
    Left = 168
    Top = 24
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 208
    Top = 24
  end
  object clMailMessage1: TclMailMessage
    ToList = <>
    CCList = <>
    BCCList = <>
    Date = 38936.624744814800000000
    CharSet = 'iso-8859-1'
    ContentType = 'text/plain'
    MimeOLE = 'Produced By Clever Internet Suite MimeOLE v 6.2'
    Left = 168
    Top = 64
  end
end
