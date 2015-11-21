unit mpClient;

interface

uses
  Windows, Classes, SysUtils,
  // indy components
  IdTCPClient, IdStack, IdGlobal;

type
  // CLIENT OBJECTS
  TmpMessageIDs = ( MSG_UNKNOWN, MSG_NOTIFY, MSG_REGISTER, MSG_AUTH_RESET,
    MSG_STATISTICS, MSG_STATUS, MSG_REQUEST, MSG_REPORT );
  TmpMessage = class(TObject)
  public
    id: integer;
    username: string;
    auth: string;
    data: string;
    constructor Create(id: integer; username, auth, data: string); Overload;
  end;
  TmpStatus = class(TObject)
  public
    programVersion: string;
    tes5Hash: string;
    tes4Hash: string;
    fnvHash: string;
    fo3Hash: string;
    constructor Create; Overload;
  end;

  { Client methods }
  procedure InitializeClient;
  procedure ConnectToServer;
  function ServerAvailable: boolean;
  procedure SendClientMessage(var msg: TmpMessage);
  function CheckAuthorization: boolean;
  procedure SendGameMode;
  procedure SendStatistics;
  procedure ResetAuth;
  function UsernameAvailable(username: string): boolean;
  function RegisterUser(username: string): boolean;
  function GetStatus: boolean;
  procedure CompareStatuses;
  function UpdateDictionary: boolean;
  function UpdateProgram: boolean;
  function DownloadProgram: boolean;
  function SendReports(var lst: TList): boolean;
  function SendPendingReports: boolean;
  function UpdateChangeLog: boolean;

const
  // DELAYS
  StatusDelay = 2.0 / (60.0 * 24.0); // 2 minutes
  MaxConnectionAttempts = 3;

var
  TCPClient: TidTCPClient;
  LocalStatus, RemoteStatus: TmpStatus;
  LastStatusTime: TDateTime;
  ConnectionAttempts: Integer;

implementation

uses
  UITypes, Dialogs,
  // abbrevia components
  AbZBrows, AbUnZper, AbArcTyp, AbMeter, AbBrowse, AbBase,
  // mte units
  CRC32, mteHelpers, mteLogger, mteTracker, RttiJson,
  // mp units
  mpConfiguration, mpCore,
  // xedit units
  wbInterface;

{ TmpMessage Constructor }
constructor TmpMessage.Create(id: integer; username, auth, data: string);
begin
  self.id := id;
  self.username := username;
  self.auth := auth;
  self.data := data;
end;

{ Constructor for TmpStatus }
constructor TmpStatus.Create;
begin
  ProgramVersion := GetVersionMem;
  if FileExists('TES5Dictionary.txt') then
    TES5Hash := GetCRC32('TES5Dictionary.txt');
  if FileExists('TES4Dictionary.txt') then
    TES4Hash := GetCRC32('TES4Dictionary.txt');
  if FileExists('FNVDictionary.txt') then
    FNVHash := GetCRC32('FNVDictionary.txt');
  if FileExists('FO3Dictionary.txt') then
    FO3Hash := GetCRC32('FO3Dictionary.txt');

  // log messages
  Logger.Write('GENERAL', 'Status', 'ProgramVersion: '+ProgramVersion);
  case CurrentProfile.gameMode of
    1: Logger.Write('GENERAL', 'Status', 'TES5 Dictionary Hash: '+TES5Hash);
    2: Logger.Write('GENERAL', 'Status', 'TES4 Dictionary Hash: '+TES4Hash);
    3: Logger.Write('GENERAL', 'Status', 'FO3 Dictionary Hash: '+FNVHash);
    4: Logger.Write('GENERAL', 'Status', 'FNV Dictionary Hash: '+FO3Hash);
  end;
end;


{******************************************************************************}
{ Client methods
  Set of methods for communicating with the Merge Plugins server.
  - InitializeClient
  - TCPClient.Connected
  - UsernameAvailable
  - RegisterUser
  - SaveReports
  - SendReports
}
{******************************************************************************}

procedure InitializeClient;
begin
  TCPClient := TidTCPClient.Create(nil);
  TCPClient.Host := settings.serverHost;
  TCPClient.Port := settings.serverPort;
  TCPClient.ReadTimeout := 4000;
  TCPClient.ConnectTimeout := 1000;
  ConnectionAttempts := 0;
end;

procedure ConnectToServer;
begin
  if (ProgramStatus.bConnecting or TCPClient.Connected)
  or (ConnectionAttempts >= MaxConnectionAttempts) then
    exit;

  ProgramStatus.bConnecting := true;
  try
    Logger.Write('CLIENT', 'Connect', 'Attempting to connect to '+TCPClient.Host+':'+IntToStr(TCPClient.Port));
    TCPClient.Connect;
    Logger.Write('CLIENT', 'Connect', 'Connection successful!');
    CheckAuthorization;
    SendGameMode;
    GetStatus;
    CompareStatuses;
    SendPendingReports;
  except
    on x: Exception do begin
      Logger.Write('ERROR', 'Connect', 'Connection failed.');
      Inc(ConnectionAttempts);
      if ConnectionAttempts = MaxConnectionAttempts then
        Logger.Write('CLIENT', 'Connect', 'Maximum connection attempts reached.  '+
          'Click the disconnected icon in the status bar to retry.');
    end;
  end;
  ProgramStatus.bConnecting := false;
end;

function ServerAvailable: boolean;
begin
  Result := false;

  try
    if TCPClient.Connected then begin
      TCPClient.IOHandler.WriteLn('', IndyTextEncoding_OSDefault());
      Result := true;
    end;
  except on Exception do
    // we're not connected
  end;
end;

procedure SendClientMessage(var msg: TmpMessage);
var
  msgJson: string;
begin
  msgJson := TRttiJson.ToJson(msg);
  TCPClient.IOHandler.WriteLn(msgJson, IndyTextEncoding_OSDefault());
end;

function CheckAuthorization: boolean;
var
  msg, response: TmpMessage;
  line: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  if settings.username = '' then
    exit;
  Logger.Write('CLIENT', 'Login', 'Checking if authenticated as "'+settings.username+'"');

  // attempt to check authorization
  // throws exception if server is unavailable
  try
    // send notify request to server
    msg := TmpMessage.Create(Ord(MSG_NOTIFY), settings.username, settings.key, 'Authorized?');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
    response := TmpMessage(TRttiJson.FromJson(line, TmpMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = 'Yes';
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception authorizing user '+x.Message);
    end;
  end;

  // set bAuthorized boolean
  ProgramStatus.bAuthorized := Result;
end;

procedure SendGameMode;
var
  msg: TmpMessage;
begin
  if not TCPClient.Connected then
    exit;

  // attempt to check authorization
  // throws exception if server is unavailable
  try
    // send notifification to server
    msg := TmpMessage.Create(Ord(MSG_NOTIFY), settings.username, settings.key, wbAppName);
    SendClientMessage(msg);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception sending game mode '+x.Message);
    end;
  end;
end;

procedure SendStatistics;
var
  msg, response: TmpMessage;
  LLine: string;
begin
  if not TCPClient.Connected then
    exit;

  // attempt to check authorization
  // throws exception if server is unavailable
  try
    // send statistics to server
    msg := TmpMessage.Create(Ord(MSG_STATISTICS), settings.username, settings.key, TRttiJson.ToJson(sessionStatistics));
    SendClientMessage(msg);

    // get response
    LLine := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
    response := TmpMessage(TRttiJson.FromJson(LLine, TmpMessage));
    Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception sending statistics '+x.Message);
    end;
  end;
end;

procedure ResetAuth;
var
  msg, response: TmpMessage;
  line: string;
begin
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Login', 'Resetting authentication as "'+settings.username+'"');

  // attempt to reset authorization
  // throws exception if server is unavailable
  try
    // send auth reset request to server
    msg := TmpMessage.Create(Ord(MSG_AUTH_RESET), settings.username, settings.key, '');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
    response := TmpMessage(TRttiJson.FromJson(line, TmpMessage));
    Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception resetting authentication '+x.Message);
    end;
  end;
end;

function UsernameAvailable(username: string): boolean;
var
  msg, response: TmpMessage;
  line: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Login', 'Checking username availability "'+username+'"');

  // attempt to register user
  // throws exception if server is unavailable
  try
    // send register request to server
    msg := TmpMessage.Create(Ord(MSG_REGISTER), username, settings.key, 'Check');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
    response := TmpMessage(TRttiJson.FromJson(line, TmpMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = 'Available';
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception checking username '+x.Message);
    end;
  end;
end;

function RegisterUser(username: string): boolean;
var
  msg, response: TmpMessage;
  line: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Login', 'Registering username "'+username+'"');

  // attempt to register user
  // throws exception if server is unavailable
  try
    // send register request to server
    msg := TmpMessage.Create(Ord(MSG_REGISTER), username, settings.key, 'Register');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
    response := TmpMessage(TRttiJson.FromJson(line, TmpMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = ('Registered ' + username);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception registering username '+x.Message);
    end;
  end;
end;

function GetStatus: boolean;
var
  msg, response: TmpMessage;
  LLine: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  if (Now - LastStatusTime) < StatusDelay then
    exit;
  LastStatusTime := Now;
  Logger.Write('CLIENT', 'Update', 'Getting update status');

  // attempt to get a status update
  // throws exception if server is unavailable
  try
    // send status request to server
    msg := TmpMessage.Create(Ord(MSG_STATUS), settings.username, settings.key, '');
    SendClientMessage(msg);

    // get response
    LLine := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
    response := TmpMessage(TRttiJson.FromJson(LLine, TmpMessage));
    RemoteStatus := TmpStatus(TRttiJson.FromJson(response.data, TmpStatus));
    //Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception getting status '+x.Message);
    end;
  end;
end;

procedure CompareStatuses;
begin
  if not Assigned(RemoteStatus) then
    exit;

  // handle program update
  ProgramStatus.bProgramUpdate := VersionCompare(LocalStatus.programVersion,
    RemoteStatus.programVersion);

  // handle dictionary update based on gamemode
  case wbGameMode of
    gmTES5: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.tes5Hash <> RemoteStatus.tes5Hash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.tes5Hash+' != '+RemoteStatus.tes5hash);
    end;
    gmTES4: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.tes4Hash <> RemoteStatus.tes4Hash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.tes4Hash+' != '+RemoteStatus.tes4hash);
    end;
    gmFNV: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.fnvHash <> RemoteStatus.fnvHash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.fnvHash+' != '+RemoteStatus.fnvhash);
    end;
    gmFO3: begin
      ProgramStatus.bDictionaryUpdate := LocalStatus.fo3Hash <> RemoteStatus.fo3Hash;
      if ProgramStatus.bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          LocalStatus.fo3Hash+' != '+RemoteStatus.fo3hash);
    end;
  end;
end;

function UpdateDictionary: boolean;
var
  msg: TmpMessage;
  filename: string;
  stream: TFileStream;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  filename := wbAppName+'Dictionary.txt';
  Logger.Write('CLIENT', 'Update',  filename);
  Tracker.Write('Updating '+filename);

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmpMessage.Create(Ord(MSG_REQUEST), settings.username, settings.key, filename);
    SendClientMessage(msg);

    // get response
    stream := TFileStream.Create(filename, fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load dictionary from response
    Logger.Write('CLIENT', 'Update', filename+' recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    LoadDictionary;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception updating dictionary '+x.Message);
    end;
  end;
end;

function UpdateProgram: boolean;
var
  archive: TAbUnZipper;
begin
  // check if zip for updating exists
  Result := false;
  if not FileExists('MergePlugins.zip') then
    exit;

  // rename program
  if FileExists('MergePlugins.exe.bak') then
    DeleteFile('MergePlugins.exe.bak');
  RenameFile('MergePlugins.exe', 'MergePlugins.exe.bak');

  // Create an instance of the TZipForge class
  archive := TAbUnZipper.Create(nil);
  try
    with archive do begin
      // The name of the ZIP file to unzip
      FileName := PathList.Values['ProgramPath'] + 'MergePlugins.zip';
      // Set base (default) directory for all archive operations
      BaseDirectory := PathList.Values['ProgramPath'];
      // Extract all files from the archive to current directory
      ExtractOptions := [eoCreateDirs, eoRestorePath];
      ExtractFiles('*.*');
      Result := true;
    end;
  except
    on x: Exception do begin
      Logger.Write('ERROR', 'Update', 'Exception ' + x.Message);
      exit;
    end;
  end;

  // clean up
  archive.Free;
  DeleteFile('MergePlugins.zip');
end;

function DownloadProgram: boolean;
var
  msg: TmpMessage;
  filename: string;
  stream: TFileStream;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  filename := 'MergePlugins.zip';
  if FileExists(filename) then begin
    MessageDlg(GetLanguageString('mpOpt_PendingUpdate'), mtInformation, [mbOk], 0);
    Result := true;
    exit;
  end;

  Logger.Write('CLIENT', 'Update', 'Merge Plugins v'+RemoteStatus.programVersion);
  Tracker.Write('Updating program to v'+RemoteStatus.programVersion);

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmpMessage.Create(Ord(MSG_REQUEST), settings.username, settings.key, 'Program');
    SendClientMessage(msg);

    // get response
    Logger.Write('CLIENT', 'Update', 'Downloading '+filename);
    stream := TFileStream.Create('MergePlugins.zip', fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load dictionary from response
    Logger.Write('CLIENT', 'Update', filename+' recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception updating program '+x.Message);
    end;
  end;
end;

function SendReports(var lst: TList): boolean;
var
  i: integer;
  report: TReport;
  msg, response: TmpMessage;
  reportJson, LLine: string;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;

  // attempt to send reports
  // throws exception if server is unavailable
  try
    // send all reports in @lst
    for i := 0 to Pred(lst.Count) do begin
      report := TReport(lst[i]);
      if Length(report.notes) > 255 then begin
        Logger.Write('CLIENT', 'Report', 'Skipping '+report.filename+', notes too long.');
        continue;
      end;
      reportJson := TRttiJson.ToJson(report);
      Logger.Write('CLIENT', 'Report', 'Sending '+report.filename);

      // send report to server
      msg := TmpMessage.Create(Ord(MSG_REPORT), settings.username, settings.key, reportJson);
      SendClientMessage(msg);

      // get response
      LLine := TCPClient.IOHandler.ReadLn(IndyTextEncoding_OSDefault());
      response := TmpMessage.Create;
      response := TmpMessage(TRttiJson.FromJson(LLine, response.ClassType));
      Logger.Write('CLIENT', 'Response', response.data);
      Inc(sessionStatistics.reportsSubmitted);
    end;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception sending reports '+x.Message);
    end;
  end;
end;

function SendPendingReports: boolean;
var
  lst: TList;
  info: TSearchRec;
  report: TReport;
  path: string;
  i: Integer;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;

  // exit if no reports to load
  path := PathList.Values['ProgramPath'] + 'reports\';
  if FindFirst(path + '*', faAnyFile, info) <> 0 then
    exit;
  lst := TList.Create;
  // load reports into list
  repeat
    if IsDotFile(info.Name) then
      continue;
    if not StrEndsWith(info.Name, '.txt') then
      continue;
    try
      report := TReport.Create;
      LoadReport(path + info.Name, report);
      DeleteFile(PChar(path + info.Name));
      report.Save(path + 'submitted\' + info.Name);
      lst.Add(report);
    except
      on x: Exception do
        Logger.Write('ERROR', 'General', Format('Unable to load report at %s%s: %s', [path, info.Name, x.Message]));
    end;
  until FindNext(info) <> 0;
  FindClose(info);

  // send reports if any were found
  if lst.Count > 0 then
    Result := SendReports(lst);

  // free reports in list, then list
  for i := Pred(lst.Count) downto 0 do begin
    report := TReport(lst[i]);
    report.Free;
  end;
  lst.Free;
end;

function UpdateChangeLog: boolean;
var
  msg: TmpMessage;
  stream: TFileStream;
begin
  Result := false;
  // don't update changelog if we've updated it within a day
  if FileExists('changelog.txt') then begin
    if Now - GetLastModified('changelog.txt') < 1.0 then
      exit;
  end;

  // exit if we aren't connected to the server
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Update', 'Getting changelog');
  Tracker.Write('Getting changelog');

  // attempt to request changelog
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmpMessage.Create(Ord(MSG_REQUEST), settings.username, settings.key, 'Changelog');
    SendClientMessage(msg);

    // get response
    stream := TFileStream.Create('changelog.txt', fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load changelog from response
    Logger.Write('CLIENT', 'Update', 'Changelog recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception getting changelog '+x.Message);
    end;
  end;
end;


end.
