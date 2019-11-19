{
  Clever Internet Suite
  Copyright (C) 1999 - 2012 Clever Components
  All Rights Reserved
  www.CleverComponents.com
}

unit clLogger;

interface

uses
  Classes, Windows, SysUtils, SyncObjs, clWUtils;

type
  TclLogEntryDirection = (edEnter, edLeave, edInside);

  TclLogger = class
  private
    FAccessor: TCriticalSection;
    FLogFileName: string;
    constructor CreateInstance(ADummy: Integer = 0);
    class function AccessInstance(Request: Integer): TclLogger;
    procedure PutMessageToFile(const AFileName, AMessage: string);
    procedure PutDataToFile(const AFileName: string; AData: PclChar; ADataSize: Integer);
    class function AddTrailingBackSlash(const APath: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TclLogger;
    class procedure ReleaseInstance;

    procedure SetLogMessageFile(const AFileName: string);
    procedure InitLogMessage(const AFileName: string);
    procedure PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
      const AMessage: string; E: Exception; const Args: array of const); overload;
    procedure PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
      const AMessage: string; E: Exception); overload;
    procedure PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
      const AMessage: string); overload;
    procedure PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
      const AMessage: string; E: Exception; IsDataConnection: Boolean); overload;
    procedure PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
      const AMessage: string; AData: PclChar; ADataSize: Integer); overload;
    property LogFileName: string read FLogFileName;
  end;

//TEMPLATE
//  {IFDEF LOGGER}try clPutLogMessage(Self, edEnter, 'AssignError');{ENDIF}
//  {IFDEF LOGGER}clPutLogMessage(Self, edLeave, 'AssignError'); except on E: Exception do begin clPutLogMessage(Self, edLeave, 'AssignError', E); raise; end; end;{ENDIF}

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string); overload;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception); overload;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception; const Args: array of const); overload;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; AData: PclChar; ADataSize: Integer); overload;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; AData: TStream; APosition: Integer); overload;

implementation

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string);
begin
  TclLogger.Instance().PutLogMessage(AInstance, ADirection, AMessage);
end;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception);
begin
  TclLogger.Instance().PutLogMessage(AInstance, ADirection, AMessage, E);
end;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception; const Args: array of const);
begin
  TclLogger.Instance().PutLogMessage(AInstance, ADirection, AMessage, E, Args);
end;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; AData: PclChar; ADataSize: Integer);
begin
  TclLogger.Instance().PutLogMessage(AInstance, ADirection, AMessage, AData, ADataSize);
end;

procedure clPutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; AData: TStream; APosition: Integer); overload;
var
  oldPos, size: Integer;
  buf: PclChar;
begin
  try
    oldPos := AData.Position;
    GetMem(buf, AData.Size);
    try
      AData.Position := APosition;
      size := AData.Read(buf^, AData.Size);
      clPutLogMessage(AInstance, ADirection, AMessage, buf, size);
    finally
      FreeMem(buf);
      AData.Position := oldPos;
    end;
  except
  end;
end;

{ TclLogger }

var
  FInstance: TclLogger = nil;

class function TclLogger.AccessInstance(Request: Integer): TclLogger;
begin
  case Request of
    0 : ;
    1 : if not Assigned(FInstance) then FInstance := CreateInstance();
    2 : FInstance := nil;
  else raise Exception.CreateFmt('Illegal request %d in AccessInstance', [Request]);
  end;
  Result := FInstance;
end;

class function TclLogger.AddTrailingBackSlash(const APath: string): string;
begin
  Result := APath;
  if (Result <> '') and (Result[Length(Result)] <> '\') then
  begin
    Result := Result + '\';
  end;
end;

constructor TclLogger.Create;
begin
  inherited Create();
  raise Exception.CreateFmt('Access class %s through Instance only', [ClassName]);
end;

constructor TclLogger.CreateInstance(ADummy: Integer);
begin
  inherited Create();
  FAccessor := TCriticalSection.Create();
  FLogFileName := AddTrailingBackslash(ExtractFilePath(ParamStr(0))) + 'clevercomponents.log';
end;

destructor TclLogger.Destroy;
begin
  if AccessInstance(0) = Self then AccessInstance(2);
  FAccessor.Free();
  inherited Destroy();
end;

procedure TclLogger.InitLogMessage(const AFileName: string);
var
  hFile: THANDLE;
  CreationTime, LastAccessTime, LastWriteTime: TFileTime;
  sysTime: TSystemTime;
  sysDate: TDateTime;
begin
  FAccessor.Enter();
  try
    hFile := CreateFile(PChar(AFileName), 0, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      GetFileTime(hFile, @CreationTime, @LastAccessTime, @LastWriteTime);
      CloseHandle(hFile);
      FileTimeToLocalFileTime(LastWriteTime, LastWriteTime);
      FileTimeToSystemTime(LastWriteTime, sysTime);
      sysDate := SystemTimeToDateTime(sysTime);
      if (sysDate < Date()) then
      begin
        DeleteFile(AFileName);
      end;
    end;
  finally
    FAccessor.Leave();
  end;
  SetLogMessageFile(AFileName);
end;

class function TclLogger.Instance: TclLogger;
begin
  Result:=AccessInstance(1);
end;

procedure TclLogger.PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception);
const
  cDirection: array[TclLogEntryDirection] of string = ('>', '<', '=');
var
  prefix, postfix: string;
  code: DWORD;
begin
  code := GetLastError();
  try
    prefix := FormatDateTime('dd-mm-yyyy:hh-nn-ss-zzz', Now()) + ' ';
    if (AInstance = nil) then
    begin
      prefix := prefix + '0 ';
    end else
    begin
      prefix := prefix + AInstance.ClassName + ' ';
    end;

    prefix := prefix + IntToStr(GetCurrentThreadId()) + ':' + IntToStr(Integer(AInstance))
      + ' ' + cDirection[ADirection] + ' ';
    if (E <> nil) then
    begin
      postfix := ' ' + E.ClassName + ': ' + E.Message;
    end else
    begin
      postfix := ' ';
    end;

    PutMessageToFile(LogFileName, prefix + AMessage + postfix + #13#10);
  finally
    SetLastError(code);
  end;
end;

procedure TclLogger.PutDataToFile(const AFileName: string; AData: PclChar; ADataSize: Integer);
var
  hFile: THandle; 
  cnt: Cardinal;
begin
  FAccessor.Enter();
  try
    hFile := CreateFile(PChar(AFileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hFile = INVALID_HANDLE_VALUE) then
    begin
      hFile := CreateFile(PChar(AFileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
        CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    end;
    if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      SetFilePointer(hFile, 0, nil, FILE_END);
      WriteFile(hFile, AData^, ADataSize, cnt, nil);
      CloseHandle(hFile);
    end;
  finally
    FAccessor.Leave();
  end;
end;

procedure TclLogger.PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception; IsDataConnection: Boolean);
const
  data: array[Boolean] of string = ('', 'data: ');
var
  code: DWORD;
begin
  code := GetLastError();
  try
    PutLogMessage(AInstance, ADirection, data[IsDataConnection] + AMessage, E);
  finally
    SetLastError(code);
  end;
end;

procedure TclLogger.PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection;
  const AMessage: string; E: Exception; const Args: array of const);
var
  code: DWORD;
begin
  code := GetLastError();
  try
    PutLogMessage(AInstance, ADirection, Format(AMessage, Args), E);
  finally
    SetLastError(code);
  end;
end;

procedure TclLogger.PutLogMessage(AInstance: TObject; ADirection: TclLogEntryDirection; const AMessage: string);
var
  code: DWORD;
begin
  code := GetLastError();
  try
    PutLogMessage(AInstance, ADirection, AMessage, nil);
  finally
    SetLastError(code);
  end;
end;

procedure TclLogger.PutMessageToFile(const AFileName, AMessage: string);
var
  hFile: THandle; 
  len, cnt: Cardinal;
  buf: PclChar;
begin
  FAccessor.Enter();
  try
    hFile := CreateFile(PChar(AFileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if (hFile = INVALID_HANDLE_VALUE) then
    begin
      hFile := CreateFile(PChar(AFileName), GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil,
        CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    end;
    if (hFile <> INVALID_HANDLE_VALUE) then
    begin
      SetFilePointer(hFile, 0, nil, FILE_END);
      len := Length(AMessage) + 1;
      GetMem(buf, len);
      StrCopy(buf, PclChar(GetTclString(AMessage)));
      WriteFile(hFile, buf[0], len - 1, cnt, nil);
      FreeMem(buf);
      CloseHandle(hFile);
    end;
  finally
    FAccessor.Leave();
  end;
end;

class procedure TclLogger.ReleaseInstance;
begin
  AccessInstance(0).Free();
end;

procedure TclLogger.SetLogMessageFile(const AFileName: string);
begin
  FAccessor.Enter();
  try
    FLogFileName := AFileName;
  finally
    FAccessor.Leave();
  end;
end;

procedure TclLogger.PutLogMessage(AInstance: TObject;
  ADirection: TclLogEntryDirection; const AMessage: string; AData: PclChar;
  ADataSize: Integer);
const
  eofData = #13#10#13#10;
var
  code: DWORD;
begin
  code := GetLastError();
  try
    if (AData <> nil) and (ADataSize > 0) then
    begin
      PutLogMessage(AInstance, ADirection, AMessage + ' (' + IntToStr(ADataSize) + '): '#13#10);
      PutDataToFile(LogFileName, AData, ADataSize);
      PutDataToFile(LogFileName, eofData, Length(eofData));
    end else
    begin
      PutLogMessage(AInstance, ADirection, AMessage + ' (no data)');
    end;
  finally
    SetLastError(code);
  end;
end;

initialization

finalization
  TclLogger.ReleaseInstance();
  
end.
