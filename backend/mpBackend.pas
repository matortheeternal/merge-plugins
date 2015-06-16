unit mpBackend;

interface

uses
  Windows, SysUtils, ShlObj, ShellApi, Classes, IniFiles, Dialogs, Masks,
  Controls, Registry, DateUtils,
  // zeosdbo components
  ZConnection, ZDataset,
  ZDbcCache, ZAbstractRODataset, ZDbcMySQL, ZDbcPostgreSQL, DB, ZSqlUpdate,
  ComCtrls, ZDbcInterbase6, ZSqlMonitor, ZAbstractDataset, ZSequence,
  // superobject
  superobject,
  // mp components
  mpLogger, mpTracker;

type
  TUser = class(TObject)
  public
    username: string;
    ip: string;
    auth: string;
    constructor Create(username, ip, auth: string); Overload;
    function Dump: ISuperObject;
    procedure LoadDump(obj: ISuperObject);
  end;
  TmpMessage = class(TObject)
  public
    id: integer;
    username: string;
    auth: string;
    data: string;
    constructor Create(id: integer; username, auth, data: string); Overload;
    function ToJson: string;
    procedure FromJson(json: string);
  end;
  TmpStatus = class(TObjecT)
  public
    programVersion: string;
    tes5Hash: string;
    tes4Hash: string;
    fnvHash: string;
    fo3Hash: string;
    function ToJson: string;
    procedure FromJson(json: string);
    procedure Save(const filename: string);
    procedure Load(const filename: string);
  end;
  TReport = class(TObject)
  public
    game: string;
    username: string;
    filename: string;
    hash: string;
    recordCount: integer;
    rating: integer;
    mergeVersion: string;
    notes: TStringList;
    dateSubmitted: TDateTime;
    constructor Create; Overload;
    constructor Create(const fields: TFields); Overload;
    function ToJson: string;
    procedure FromJson(json: string);
  end;
  TEntry = class(TObject)
  public
    filename: string;
    hash: string;
    records: string;
    version: string;
    rating: string;
    reports: string;
    notes: string;
    constructor Create; Overload;
    constructor Create(const s: string); Overload;
  end;
  TSettings = class(TObject)
  public
    uniqueIPs: TStringList;
    blacklistedIPs: TStringList;
    constructor Create; Overload;
    procedure Save(const filename: string);
    procedure Load(const filename: string);
  end;
  TStatistics = class(TObject)
  public
    timesRun: integer;
    uniqueIPs: TStringList;
    dictionaryUpdates: integer;
    programUpdates: integer;
    reportsRecieved: integer;
    reportsApproved: integer;
    reportsDenied: integer;
    totalBandwidth: Int64;
    totalUptime: TDateTime;
    tes5ReportsRecieved: integer;
    tes4ReportsRecieved: integer;
    fnvReportsRecieved: integer;
    fo3ReportsRecieved: integer;
    tes5Logins: integer;
    tes4Logins: integer;
    fnvLogins: integer;
    fo3Logins: integer;
    constructor Create; virtual;
    procedure Save(const filename: string);
    procedure Load(const filename: string);
  end;

  { MySQL methods }
  procedure DoLogin(userID, password, database, host, port: string);
  procedure QueryReports;
  function ReportWhereClause(report: TReport): string;
  function ReportSetClause(report: TReport): string;
  procedure UpdateReport(report: TReport; SetClause, WhereClause: string);
  procedure AddReport(report: TReport; table: string);
  procedure RemoveReport(report: TReport; table: string);
  { General functions }
  function csvText(s: string): string;
  function FormatByteSize(const bytes: Int64): string;
  function DateBuiltString(date: TDateTime): string;
  function DateTimeToSQL(date: TDateTime): string;
  function TimeStr(date: TDateTime): string;
  function AppendIfMissing(str, substr: string): string;
  function StrEndsWith(s1, s2: string): boolean;
  function RemoveFromEnd(s1, s2: string): string;
  function IntegerListSum(list: TList; maxIndex: integer): integer;
  function Wordwrap(var s: string; charCount: integer): string;
  function ContainsMatch(var sl: TStringList; const s: string): boolean;
  function IsURL(s: string): boolean;
  function GetSessionUptime: TDateTime;
  function GetVersionMem: string;
  { Windows API functions }
  function GetCSIDLShellFolder(CSIDLFolder: integer): string;
  function GetFileSize(const aFilename: String): Int64;
  function GetLastModified(const aFileName: String): TDateTime;
  function RecursiveFileSearch(aPath, aFileName: string; ignore: TStringList; maxDepth: integer): string;
  procedure CopyDirectory(src, dst: string; fIgnore, dIgnore: TStringList);
  procedure GetFilesList(path: string; var fIgnore, dIgnore, list: TStringList);
  procedure CopyFiles(src, dst: string; var list: TStringList);
  { Data methods }
  procedure LoadSettings;
  procedure SaveSettings;
  procedure LoadStatus;
  procedure SaveStatus;
  procedure LoadStatistics;
  procedure SaveStatistics;
  procedure LoadUsers;
  procedure SaveUsers;
  function Authorized(username, auth: string): boolean;
  function ResetAuth(username, ip, auth: string): boolean;
  procedure LoadDictionary(var lst: TList; filename: string);
  procedure LoadBlacklist(var lst, dictionary: TList);
  function GetRatingColor(rating: real): integer;
  function GetEntry(var dictionary: TList; pluginName, numRecords, version: string): TEntry;

const
  ReportColumns = 'game,username,filename,hash,record_count,rating,merge_version,notes,date_submitted';

  // MSG IDs
  MSG_NOTIFY = 0;
  MSG_REGISTER = 1;
  MSG_AUTH_RESET = 2;
  MSG_STATISTICS = 3;
  MSG_STATUS = 4;
  MSG_REQUEST = 5;
  MSG_REPORT = 6;

var
  TES5Dictionary, TES4Dictionary, FO3Dictionary, FNVDictionary,
  ApprovedReports, UnapprovedReports, UsersList: TList;
  statistics: TStatistics;
  settings: TSettings;
  status: TmpStatus;
  TempPath, LogPath, ProgramPath: string;
  bLoginSuccess, bProgressCancel: boolean;
  wbStartTime: TDateTime;
  sessionBandwidth: Int64;
  Connection: TZConnection;

implementation

{******************************************************************************}
{ SQL Methods
  Methods for interacting with the SQL database.

  List of methods:
  - DoLogin
  - QueryReports
  - SQLQuery
  - ReportWhereClause
  - ReportSetClause
  - ReportValuesClause
  - UpdateReport
  - AddReport
  - RemoveReport
}
{******************************************************************************}

{ Attempt to login to MySQL database }
procedure DoLogin(userID, password, database, host, port: string);
begin
  // attempt to connect to mysql
  bLoginSuccess := false;
  try
    Connection := TZConnection.Create(nil);
    Connection.User := userID;
    Connection.Port := StrToInt(port);
    Connection.Database := database;
    Connection.Password := password;
    Connection.HostName := host;
    Connection.Protocol := 'mysql';
    Connection.Connect;
    bLoginSuccess := Connection.Connected;
    if not bLoginSuccess then
      ShowMessage('Failed to connect to database.');
  except
    on x : Exception do begin
      ShowMessage('Failed to connect: '#13#10+x.Message);
      Connection.Free;
    end;
  end;
end;

{ Query database for Approved and Unapproved reports }
procedure QueryReports;
var
  Dataset: TZQuery;
  report: TReport;
begin
  // initialize lists
  ApprovedReports := TList.Create;
  UnapprovedReports := TList.Create;

  // get approved_reports
  Dataset := TZQuery.Create(nil);
  Dataset.Connection := Connection;
  Dataset.Fields.Clear;
  Dataset.SQL.Add('SELECT '+ReportColumns+' FROM approved_reports');
  Dataset.ExecSQL;
  Dataset.Open;

  // load into ApprovedReports list
  Dataset.First;
  while not Dataset.EOF do begin
    report := TReport.Create(Dataset.Fields);
    ApprovedReports.Add(report);
    Dataset.Next;
  end;
  Dataset.Close;

  // get unapproved_reports
  Dataset := TZQuery.Create(nil);
  Dataset.Connection := Connection;
  Dataset.Fields.Clear;
  Dataset.SQL.Add('SELECT '+ReportColumns+' FROM unapproved_reports');
  Dataset.ExecSQL;
  Dataset.Open;

  // load into UnapprovedReports list
  Dataset.First;
  while not Dataset.EOF do begin
    report := TReport.Create(Dataset.Fields);
    UnapprovedReports.Add(report);
    Dataset.Next;
  end;
  Dataset.Close;
  Dataset.Free;
end;

procedure SQLQuery(query: string);
var
  SQLQuery: TZQuery;
begin
  Logger.Write('[SQL] '+query);
  SQLQuery := TZQuery.Create(nil);
  SQLQuery.Connection := Connection;
  SQLQuery.Fields.Clear;
  SQLQuery.SQL.Add(query);
  SQLQuery.ExecSQL;
  SQLQuery.Free;
end;

function ReportWhereClause(report: TReport): string;
begin
  Result := 'WHERE '+
    'game='''+report.game+''' AND '+
    'username='''+report.username+''' AND '+
    'filename='''+report.filename+''' AND '+
    'merge_version='''+report.mergeVersion+'''';
end;

function ReportSetClause(report: TReport): string;
begin
  Result := 'SET '+
    'game='''+report.game+''', '+
    'username='''+report.username+''', '+
    'filename='''+report.filename+''', '+
    'hash='''+report.hash+''', '+
    'record_count='+IntToStr(report.recordCount)+', '+
    'rating='+IntToStr(report.rating)+', '+
    'merge_version='''+report.mergeVersion+''', '+
    'notes='''+StringReplace(Trim(report.notes.Text), #13#10, '@13', [rfReplaceAll])+'''';
end;

function ReportValuesClause(report: TReport): string;
begin
  Result := '('+ReportColumns+') '+
    'VALUES ('''+
    report.game+''','''+
    report.username+''','''+
    report.filename+''','''+
    report.hash+''','+
    IntToStr(report.recordCount)+','+
    IntToStr(report.rating)+','''+
    report.mergeVersion+''','''+
    StringReplace(Trim(report.notes.Text), #13#10, '@13', [rfReplaceAll])+''','''+
    DateTimeToSQL(report.dateSubmitted)+''')';
end;

procedure UpdateReport(report: TReport; SetClause, WhereClause: string);
var
  query: string;
begin
  query := 'UPDATE approved_reports '+SetClause+' '+WhereClause+';';
  SQLQuery(query);
end;

procedure AddReport(report: TReport; table: string);
var
  query, ValuesClause: string;
begin
  // execute SQL
  ValuesClause := ReportValuesClause(report);
  query := 'INSERT INTO '+table+' '+ValuesClause+';';
  SQLQuery(query);
end;

procedure RemoveReport(report: TReport; table: string);
var
  query, WhereClause: string;
begin
  // execute SQL
  WhereClause := ReportWhereClause(report);
  query := 'DELETE FROM '+table+' '+WhereClause+';';
  SQLQuery(query);
end;

{******************************************************************************}
{ General functions
  Set of functions that help with converting data formats and handling strings.

  List of functions:
  - csvText
  - FormatByteSize
  - DateBuiltString
  - IntegerListSum
}
{*****************************************************************************}

{ Replaces newlines with a comma and space }
function csvText(s: string): string;
begin
  result := StringReplace(Trim(s), #13, ', ', [rfReplaceAll]);
end;

{ Format file byte size }
function FormatByteSize(const bytes: Int64): string;
const
 B = 1; //byte
 KB = 1024 * B; //kilobyte
 MB = 1024 * KB; //megabyte
 GB = 1024 * MB; //gigabyte
begin
  if bytes > GB then
    result := FormatFloat('#.## GB', bytes / GB)
  else
    if bytes > MB then
      result := FormatFloat('#.## MB', bytes / MB)
    else
      if bytes > KB then
        result := FormatFloat('#.## KB', bytes / KB)
      else
        if bytes > 0 then
          result := FormatFloat('#.## bytes', bytes)
        else
          result := '0 bytes';
end;

{ Converts a TDateTime to a string, with 0 being the string 'Never' }
function DateBuiltString(date: TDateTime): string;
begin
  if date = 0 then
    Result := 'Never'
  else begin
    Result := DateTimeToStr(date);
  end;
end;

{ Converts a TdateTime to an SQL-compatible string }
function DateTimeToSQL(date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:mm:ss', date);
end;

{ Converts an SQL-compatible date time string to a TDateTime }
function SQLToDateTime(date: string): TDateTime;
var
  fs: TFormatSettings;
begin
  GetLocaleFormatSettings(GetThreadLocale, fs);
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy-mm-dd';
  fs.TimeSeparator := ':';
  fs.LongTimeFormat := 'hh:nn:ss';
  Result := StrToDateTime(date, fs);
end;

{ Converts a TDateTime to a time string, e.g. 19d 20h 3m 30s }
function TimeStr(date: TDateTime): string;
begin
  Result := Format('%dd %dh %dm %ds', [Trunc(date), HourOf(date), MinuteOf(date), SecondOf(date)]);
end;

{
  AppendIfMissing:
  Appends substr to the end of str if it's not already there.

  Example usage:
  s := 'This is a sample string.';
  Logger.Write(AppendIfMissing(s, 'string.')); //'This is a sample string.'
  Logger.Write(AppendIfMissing(s, '  Hello.')); //'This is a sample string.  Hello.'
}
function AppendIfMissing(str, substr: string): string;
begin
  Result := str;
  if Length(str) > Length(substr) then
    if Copy(str, Length(str) - Length(substr), Length(substr)) = substr then
      exit;

  Result := str + substr;
end;

{
  StrEndsWith:
  Checks to see if a string ends with an entered substring.

  Example usage:
  s := 'This is a sample string.';
  if StrEndsWith(s, 'string.') then
    AddMessage('It works!');
}
function StrEndsWith(s1, s2: string): boolean;
var
  n1, n2: integer;
begin
  Result := false;

  n1 := Length(s1);
  n2 := Length(s2);
  if n1 < n2 then exit;

  Result := (Copy(s1, n1 - n2 + 1, n2) = s2);
end;

{
  RemoveFromEnd:
  Creates a new string with s1 removed from the end of s2, if found.

  Example usage:
  s := 'This is a sample string.';
  AddMessage(RemoveFromEnd(s, 'string.')); //'This is a sample '
}
function RemoveFromEnd(s1, s2: string): string;
begin
  Result := s1;
  if StrEndsWith(s1, s2) then
    Result := Copy(s1, 1, Length(s1) - Length(s2));
end;

{ Calculates the integer sum of all values in a TList to maxIndex }
function IntegerListSum(list: TList; maxIndex: integer): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to maxIndex do
    Inc(result, Integer(list[i]));
end;

{ Inserts line breaks in string @s before @charCount has been exceeded }
function Wordwrap(var s: string; charCount: integer): string;
var
  i, lastSpace, counter: Integer;
begin
  counter := 0;
  lastSpace := 0;
  for i := 1 to Length(s) do begin
    Inc(counter);
    if (s[i] = ' ') or (s[i] = ',') then
      lastSpace := i;
    if (s[i] = #13) or (s[i] = #10) then begin
      lastSpace := 0;
      counter := 0;
    end;
    if (counter = charCount) and (lastSpace > 0) then begin
      Insert(#13#10, s, lastSpace + 1);
      lastSpace := 0;
      counter := 0;
    end;
  end;
  Result := s;
end;

{ Checks to see if any mask in @sl matches the string @s }
function ContainsMatch(var sl: TStringList; const s: string): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Pred(sl.Count) do
    if MatchesMask(s, sl[i]) then begin
      Result := true;
      break;
    end;
end;

{ Returns true if the string is an http:// or https:// url }
function IsURL(s: string): boolean;
begin
  Result := (Pos('http://', s) = 1) or (Pos('https://', s) = 1);
end;

{ Returns the time the application has been running for }
function GetSessionUptime: TDateTime;
begin
  Result := Now - wbStartTime;
end;

{ Get program version from memory }
function GetVersionMem: string;
var
  verblock: PVSFIXEDFILEINFO;
  versionMS, versionLS, verlen: cardinal;
  rs: TResourceStream;
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    rs := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
    try
      m.CopyFrom(rs, rs.Size);
    finally
      rs.Free;
    end;
    m.Position := 0;
    if VerQueryValue(m.Memory, '\', Pointer(verblock), verlen) then begin
      VersionMS := verblock.dwFileVersionMS;
      VersionLS := verblock.dwFileVersionLS;
      Result := Format('%s.%s.%s.%s', [IntToStr(versionMS shr 16),
        IntToStr(versionMS and $FFFF), IntToStr(VersionLS shr 16),
        IntToStr(VersionLS and $FFFF)]);
    end;
  finally
    m.Free;
  end;
end;


{******************************************************************************}
{ Windows API functions
  Set of functions that help deal with the Windows File System.

  List of functions:
  - GetCSIDLShellFolder
  - GetFileSize
  - GetLastModified
  - RecursiveFileSearch
}
{******************************************************************************}

{ Gets a folder by its integer CSID. }
function GetCSIDLShellFolder(CSIDLFolder: integer): string;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(0, PChar(Result), CSIDLFolder, True);
  SetLength(Result, StrLen(PChar(Result)));
  if (Result <> '') then
    Result := IncludeTrailingBackslash(Result);
end;

{ Gets the size of a file at @aFilename through the windows API }
function GetFileSize(const aFilename: String): Int64;
var
  info: TWin32FileAttributeData;
begin
  result := -1;

  if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    EXIT;

  result := Int64(info.nFileSizeLow) or Int64(info.nFileSizeHigh shl 32);
end;

{ Gets the last time a file was modified }
function GetLastModified(const aFileName: String): TDateTime;
var
  info: TWin32FileAttributeData;
  FileTime: TFileTime;
  LocalTime, SystemTime: TSystemTime;
begin
  result := 0;
  // exit if can't get attributes
  if not GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    exit;

  // get last modified
  FileTime := info.ftLastWriteTime;

  // convert to system time
  if not FileTimeToSystemTime(FileTime, SystemTime) then
    RaiseLastOSError;
  if not SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime) then
    RaiseLastOSError;

  Result := SystemTimeToDateTime(LocalTime);
end;

{
  RecursiveFileSearch:
  Recursively searches a path for a file matching aFileName, ignoring
  directories in the ignore TStringList, and not traversing deeper than
  maxDepth.

  Example usage:
  ignore := TStringList.Create;
  ignore.Add('Data');
  p := RecursiveFileSearch('Skyrim.exe', GamePath, ignore, 1, false);
  AddMessage(p);
}
function RecursiveFileSearch(aPath, aFileName: string; ignore: TStringList; maxDepth: integer): string;
var
  skip: boolean;
  i: integer;
  rec: TSearchRec;
begin
  Result := '';
  aPath := AppendIfMissing(aPath, PathDelim);
  if Result <> '' then exit;
  // always ignore . and ..
  ignore.Add('.');
  ignore.Add('..');

  if FindFirst(aPath + '*', faAnyFile, rec) = 0 then begin
    repeat
      skip := false;
      for i := 0 to Pred(ignore.Count) do begin
        skip := Lowercase(rec.Name) = ignore[i];
        if skip then
          break;
      end;
      if not skip then begin
        if ((rec.attr and faDirectory) = faDirectory) and (maxDepth > 0) then begin
          Result := RecursiveFileSearch(aPath+rec.Name, aFileName, ignore, maxDepth - 1);
        end
        else if (rec.Name = aFileName) then
          Result := aPath + rec.Name;
      end;
      if (Result <> '') then break;
    until FindNext(rec) <> 0;

    FindClose(rec);
  end;
end;

{
  CopyDirectory:
  Recursively copies all of the contents of a directory.

  Example usage:
  slIgnore := TStringList.Create;
  slIgnore.Add('mteFunctions.pas');
  CopyDirectory(ScriptsPath, 'C:\ScriptsBackup', slIgnore);
}
procedure CopyDirectory(src, dst: string; fIgnore, dIgnore: TStringList);
var
  info: TSearchRec;
  isDirectory: boolean;
begin
  src := AppendIfMissing(src, PathDelim);
  dst := AppendIfMissing(dst, PathDelim);

  // if no files in source path, exit
  if (FindFirst(src + '*', faAnyFile, info) <> 0) then
    exit;
  repeat
    isDirectory := (info.Attr and faDirectory = faDirectory);
    // skip . and ..
    if (info.Name = '.') or (info.Name = '..') then
      continue;

    // skip if ignored
    if isDirectory and ContainsMatch(dIgnore, info.Name) then
      continue
    else if ContainsMatch(fIgnore, info.Name) then
      continue;

    // copy the file or recurse
    ForceDirectories(dst);
    if isDirectory then
      CopyDirectory(src+info.Name, dst+info.Name, fIgnore, dIgnore)
    else
      CopyFile(PChar(src+info.Name), PChar(dst+info.Name), false);
  until FindNext(info) <> 0;

  FindClose(info);
end;

{
  GetFilesList:
  Searches @path, recursively traversing subdirectories that don't match a mask
  in @dIgnore, adding files that don't match a mask in @fIgnore to @list.

  Example usage:
  FilesList := TStringList.Create;
  fileIgnore := TStringList.Create;
  fileIgnore.Add('*.esp');
  dirIgnore := TStringList.Create;
  dirIgnore.Add('translations');
  GetFilesList(wbDataPath, fileIgnore, dirIgnore, FilesList);
}
procedure GetFilesList(path: string; var fIgnore, dIgnore, list: TStringList);
var
  info: TSearchRec;
  isDirectory: boolean;
begin
  path := AppendIfMissing(path, PathDelim);

  // if no files in source path, exit
  if (FindFirst(path + '*', faAnyFile, info) <> 0) then
    exit;
  repeat
    isDirectory := (info.Attr and faDirectory = faDirectory);
    // skip . and ..
    if (info.Name = '.') or (info.Name = '..') then
      continue;

    // skip if ignored
    if isDirectory and ContainsMatch(dIgnore, info.Name) then
      continue
    else if ContainsMatch(fIgnore, info.Name) then
      continue;

    // copy the file or recurse
    if isDirectory then
      GetFilesList(path + info.Name, fIgnore, dIgnore, list)
    else
      list.Add(path + info.Name);
  until FindNext(info) <> 0;

  FindClose(info);
end;

{ Copies files in @list from @src to @dst }
procedure CopyFiles(src, dst: string; var list: TStringList);
var
  i: Integer;
  srcFile, dstFile: string;
begin
  src := AppendIfMissing(src, PathDelim);
  dst := AppendIfMissing(dst, PathDelim);
  for i := 0 to Pred(list.Count) do begin
    srcFile := list[i];
    dstFile := StringReplace(srcFile, src, dst, []);
    ForceDirectories(ExtractFilePath(dstFile));
    CopyFile(PChar(srcFile), PChar(dstFile), false);
  end;
end;


{******************************************************************************}
{ Data methods
  Set of methods for working with data.

  List of methods:
  - LoadSettings
  - SaveSettings
  - LoadStatistics
  - SaveStatistics
  - LoadDictionary
  - GetRatingColor
  - GetRating
  - IsBlackListed
  - GetEntry
}
{******************************************************************************}

procedure LoadSettings;
begin
  settings := TSettings.Create;
  settings.Load('settings.ini');
end;

procedure SaveSettings;
begin
  settings.Save('settings.ini');
end;

procedure LoadStatus;
begin
  status := TmpStatus.Create;
  status.Load('status.json');
end;

procedure SaveStatus;
begin
  status.Save('status.json');
end;

procedure LoadStatistics;
begin
  statistics := TStatistics.Create;
  statistics.Load('statistics.ini');
end;

procedure SaveStatistics;
begin
  statistics.Save('statistics.ini');
end;

procedure LoadUsers;
var
  user: TUser;
  obj, userItem: ISuperObject;
  sl: TStringList;
  filename: string;
begin
  UsersList := TList.Create;
  // don't load file if it doesn't exist
  filename := 'users.json';
  if not FileExists(filename) then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through merges
  for userItem in obj['users'] do begin
    user := TUser.Create;
    user.LoadDump(userItem);
    UsersList.Add(user);
  end;

  // finalize
  obj := nil;
  sl.Free;
end;

procedure SaveUsers;
var
  i: Integer;
  user: TUser;
  json: ISuperObject;
  filename: string;
begin
  // initialize json
  json := SO;
  json.O['users'] := SA([]);

  // loop through merges
  Tracker.Write('Dumping users to JSON');
  for i := 0 to Pred(UsersList.Count) do begin
    Tracker.Update(1);
    user := TUser(UsersList[i]);
    Tracker.Write('  Dumping '+user.username);
    json.A['users'].Add(user.Dump);
  end;

  // save and finalize
  filename := 'users.json';
  Tracker.Write(' ');
  Tracker.Write('Saving to ' + filename);
  Tracker.Update(1);
  json.SaveTo(filename);
  json := nil;
end;

function Authorized(username, auth: string): boolean;
var
  i: Integer;
  user: TUser;
begin
  Result := true;
  for i := 0 to Pred(UsersList.Count) do begin
    user := TUser(UsersList[i]);
    if SameText(user.username, username) then begin
      Result := SameText(user.auth, auth);
      exit;
    end;
  end;
end;

function ResetAuth(username, ip, auth: string): boolean;
var
  i: Integer;
  user: TUser;
begin
  Result := false;
  for i := 0 to Pred(UsersList.Count) do begin
    user := TUser(UsersList[i]);
    if SameText(user.username, username) then begin
      Result := SameText(user.ip, ip);
      if Result then
        user.auth := auth;
      exit;
    end;
  end;
end;

procedure LoadDictionary(var lst: TList; filename: string);
var
  i: Integer;
  entry: TEntry;
  sl: TStringList;
begin
  // don't attempt to load dictionary if it doesn't exist
  if not FileExists(filename) then begin
    Logger.Write('[INIT] No dictionary file '+filename);
    exit;
  end;

  // load dictionary file
  sl := TStringList.Create;
  sl.LoadFromFile(filename);

  // load dictionary file into entry object
  for i := 0 to Pred(sl.Count) do begin
    entry := TEntry.Create(sl[i]);
    lst.Add(entry);
  end;

  // free temporary stringlist
  sl.Free;
end;

procedure LoadBlacklist(var lst, dictionary: TList);
var
  i: Integer;
  entry: TEntry;
begin
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TEntry(dictionary[i]);
    if entry.rating = '-1' then
      lst.Add(entry);
  end;
end;

function GetRatingColor(rating: real): integer;
var
  k1, k2: real;
  r, g: byte;
begin
  if rating = -2.0 then begin
    Result := $707070;
    exit;
  end;

  if rating = -1.0 then begin
    Result := $000000;
    exit;
  end;

  if (rating > 2.0) then begin
    k2 := (rating - 2.0)/2.0;
    k1 := 1.0 - k2;
    r := Trunc($E5 * k1 + $00 * k2);
    g := Trunc($A8 * k1 + $90 * k2);
  end
  else begin
    k2 := (rating/2.0);
    k1 := 1.0 - k2;
    r := Trunc($FF * k1 + $E5 * k2);
    g := Trunc($00 * k1 + $A8 * k2);
  end;

  Result := g * 256 + r;
end;

function GetEntry(var dictionary: TList; pluginName, numRecords, version: string): TEntry;
var
  i: Integer;
  entry: TEntry;
begin
  Result := TEntry.Create;
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TEntry(dictionary[i]);
    if entry.filename = pluginName then begin
      Result := entry;
      exit;
    end;
  end;
end;


{******************************************************************************}
{ Object methods
  Set of methods for objects TMerge and TPlugin

  List of methods:
  - TUser.Create
  - TUser.Dump
  - TUser.LoadDump
  - TmpMessage.Create
  - TmpMessage.ToJson
  - TmpMessage.FromJson
  - TmpStatus.ToJson
  - TmpStatus.FromJson
  - TmpStatus.Save
  - TmpStatus.Load
  - TReport.Create
  - TReport.Create
  - TReport.ToJson
  - TReport.FromJson
  - TEntry.Create
  - TSettings.Create
  - TSettings.Save
  - TSettings.Load
  - TStatistics.Create
  - TStatistics.Save
  - TStatistics.Load
}
{******************************************************************************}


constructor TUser.Create(username, ip, auth: string);
begin
  self.username := username;
  self.ip := ip;
  self.auth := auth;
end;

function TUser.Dump: ISuperObject;
var
  obj: ISuperObject;
begin
  obj := SO;
  obj.S['username'] := username;
  obj.S['ip'] := ip;
  obj.S['auth'] := auth;

  Result := obj;
end;

procedure TUser.LoadDump(obj: ISuperObject);
begin
  username := obj.AsObject.S['username'];
  ip := obj.AsObject.S['ip'];
  auth := obj.AsObject.S['auth'];
end;

{ TmpMessage Constructor }
constructor TmpMessage.Create(id: integer; username, auth, data: string);
begin
  self.id := id;
  self.username := username;
  self.auth := auth;
  self.data := data;
end;

{ TmpMessage to json string }
function TmpMessage.ToJson: string;
var
  obj: ISuperObject;
begin
  obj := SO;

  // filename, hash, errors
  obj.I['id'] := id;
  obj.S['username'] := username;
  obj.S['auth'] := auth;
  obj.S['data'] := data;

  Result := obj.AsJSon;
end;

{ Json string to TmpMessage }
procedure TmpMessage.FromJson(json: string);
var
  obj: ISuperObject;
begin
  obj := SO(PChar(json));

  id := obj.I['id'];
  username := obj.S['username'];
  auth := obj.S['auth'];
  data := obj.S['data'];
end;

{ TmpStatus to json string }
function TmpStatus.ToJson: string;
var
  obj: ISuperObject;
begin
  obj := SO;

  // filename, hash, errors
  obj.S['programVersion'] := programVersion;
  obj.S['tes5Hash'] := tes5Hash;
  obj.S['tes4Hash'] := tes4Hash;
  obj.S['fnvHash'] := fnvHash;
  obj.S['fo3Hash'] := fo3Hash;

  Result := obj.AsJSon;
end;

procedure TmpStatus.Save(const filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := ToJson;
  sl.SaveToFile(filename);
  sl.Free;
end;

procedure TmpStatus.Load(const filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  FromJson(sl.Text);
  sl.Free;
end;

{ Json string to TmpStatus }
procedure TmpStatus.FromJson(json: string);
var
  obj: ISuperObject;
begin
  obj := SO(PChar(json));

  programVersion := obj.S['programVersion'];
  tes5Hash := obj.S['tes5Hash'];
  tes4Hash := obj.S['tes4Hash'];
  fnvHash := obj.S['fnvHash'];
  fo3Hash := obj.S['fo3Hash'];
end;

{ TReport Constructor }
constructor TReport.Create;
begin
  notes := TStringList.Create;
end;

constructor TReport.Create(const fields: TFields);
var
  s: string;
begin
  game := fields[0].AsString;
  username := fields[1].AsString;
  filename := fields[2].AsString;
  hash := fields[3].AsString;
  recordCount := fields[4].AsInteger;
  rating := fields[5].AsInteger;
  mergeVersion := fields[6].AsString;
  notes := TStringList.Create;
  s := StringReplace(fields[7].AsString, '@13', #13#10, [rfReplaceAll]);
  notes.Text := Wordwrap(s, 70);
  dateSubmitted := fields[8].AsDateTime;
end;

{ TReport to json string }
function TReport.ToJson: string;
var
  obj: ISuperObject;
begin
  obj := SO;

  obj.S['game'] := game;
  obj.S['username'] := username;
  obj.S['filename'] := filename;
  obj.S['hash'] := hash;
  obj.I['recordCount'] := recordCount;
  obj.I['rating'] := rating;
  obj.S['mergeVersion'] := mergeVersion;
  obj.S['notes'] := StringReplace(notes.Text, #13#10, '@13', [rfReplaceAll]);
  obj.S['dateSubmitted'] := DateTimeToSQL(dateSubmitted);

  Result := obj.AsJSon;
end;

{ Json string to TReport }
procedure TReport.FromJson(json: string);
var
  obj: ISuperObject;
begin
  obj := SO(PChar(json));

  game := obj.S['game'];
  username := obj.S['username'];
  filename := obj.S['filename'];
  hash := obj.S['hash'];
  recordCount := obj.I['recordCount'];
  rating := obj.I['rating'];
  mergeVersion := obj.S['mergeVersion'];
  notes.Text := obj.S['notes'];
  dateSubmitted := SQLToDateTime(obj.S['dateSubmitted']);
end;

{ TEntry Constructor }
constructor TEntry.Create;
begin
  reports := '0';
  rating := 'No rating';
end;

constructor TEntry.Create(const s: string);
var
  i, lastIndex, ct: Integer;
begin
  lastIndex := 1;
  ct := 0;
  for i := 1 to Length(s) do begin
    if s[i] = ';' then begin
      if ct = 0 then
        filename := Copy(s, lastIndex, i - lastIndex)
      else if ct = 1 then
        records := Copy(s, lastIndex, i - lastIndex)
      else if ct = 2 then
        version := Copy(s, lastIndex, i - lastIndex)
      else if ct = 3 then
        rating := Copy(s, lastIndex, i - lastIndex)
      else if ct = 4 then begin
        reports := Copy(s, lastIndex, i - lastIndex);
        notes := Copy(s, i + 1, Length(s));
      end;
      LastIndex := i + 1;
      Inc(ct);
    end;
  end;
end;

{ TSettings constructor }
constructor TSettings.Create;
begin
  uniqueIPs := TStringList.Create;
  blacklistedIPs := TStringList.Create;
end;

{ Load settings from settings.json }
procedure TSettings.Load(const filename: string);
var
  obj, item: ISuperObject;
  sl: TStringList;
begin
  // don't load file if it doesn't exist
  if not FileExists(filename) then
    exit;

  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through unique IPs
  for item in obj['uniqueIPs'] do
    uniqueIPs.Add(item.AsString);

  // loop through blacklisted IPs
  for item in obj['blacklistedIPs'] do
    blacklistedIPs.Add(item.AsString);

  // finalize
  obj := nil;
  sl.Free;
end;

{ Save settings to settings.json }
procedure TSettings.Save(const filename: string);
var
  i: Integer;
  json: ISuperObject;
begin
  // initialize json
  json := SO;
  json.O['uniqueIPs'] := SA([]);
  json.O['blacklistedIPs'] := SA([]);

  // dump unique IPs
  Tracker.Write('Dumping unique IPs to JSON');
  for i := 0 to Pred(uniqueIPs.Count) do
    json.A['uniqueIPs'].S[i] := uniqueIPs[i];

  // dump blacklisted IPs
  Tracker.Write('Dumping blacklisted IPs to JSON');
  for i := 0 to Pred(blacklistedIPs.Count) do
    json.A['blacklistedIPs'].S[i] := blacklistedIPs[i];

  // save and finalize
  Tracker.Write(' ');
  Tracker.Write('Saving to '+filename);
  json.SaveTo(filename);
  json := nil;
end;

{ TStatistics constructor }
constructor TStatistics.Create;
begin
  timesRun := 0;
  dictionaryUpdates := 0;
  programUpdates := 0;
  reportsRecieved := 0;
  reportsApproved := 0;
  reportsDenied := 0;
  totalBandwidth := 0;
  totalUptime := 0;
  tes5ReportsRecieved := 0;
  tes4ReportsRecieved := 0;
  fnvReportsRecieved := 0;
  fo3ReportsRecieved := 0;
  tes5Logins := 0;
  tes4Logins := 0;
  fnvLogins := 0;
  fo3Logins := 0;
end;

procedure TStatistics.Save(const filename: string);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  ini.WriteInteger('Statistics', 'timesRun', timesRun);
  ini.WriteInteger('Statistics', 'dictionaryUpdates', dictionaryUpdates);
  ini.WriteInteger('Statistics', 'programUpdates', programUpdates);
  ini.WriteInteger('Statistics', 'reportsRecieved', reportsRecieved);
  ini.WriteInteger('Statistics', 'reportsApproved', reportsApproved);
  ini.WriteInteger('Statistics', 'reportsDenied', reportsDenied);
  ini.WriteInteger('Statistics', 'totalBandwidth', totalBandwidth);
  ini.WriteFloat('Statistics', 'totalUptime', totalUptime);
  ini.WriteInteger('Statistics', 'tes5ReportsRecieved', tes5ReportsRecieved);
  ini.WriteInteger('Statistics', 'tes4ReportsRecieved', tes4ReportsRecieved);
  ini.WriteInteger('Statistics', 'fnvReportsRecieved', fnvReportsRecieved);
  ini.WriteInteger('Statistics', 'fo3ReportsRecieved', fo3ReportsRecieved);
  ini.WriteInteger('Statistics', 'tes5Logins', tes5Logins);
  ini.WriteInteger('Statistics', 'tes4Logins', tes4Logins);
  ini.WriteInteger('Statistics', 'fnvLogins', fnvLogins);
  ini.WriteInteger('Statistics', 'fo3Logins', fo3Logins);

  // save file
  ini.UpdateFile;
  ini.Free;
end;

procedure TStatistics.Load(const filename: string);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  timesRun := ini.ReadInteger('Statistics', 'timesRun', 0);
  dictionaryUpdates := ini.ReadInteger('Statistics', 'dictionaryUpdates', 0);
  programUpdates := ini.ReadInteger('Statistics', 'programUpdates', 0);
  reportsRecieved := ini.ReadInteger('Statistics', 'reportsRecieved', 0);
  reportsApproved := ini.ReadInteger('Statistics', 'reportsApproved', 0);
  reportsDenied := ini.ReadInteger('Statistics', 'reportsDenied', 0);
  totalBandwidth := ini.ReadInteger('Statistics', 'totalBandwidth', 0);
  totalUptime := ini.ReadFloat('Statistics', 'totalUptime', 0);
  tes5ReportsRecieved := ini.ReadInteger('Statistics', 'tes5ReportsRecieved', 0);
  tes4ReportsRecieved := ini.ReadInteger('Statistics', 'tes4ReportsRecieved', 0);
  fnvReportsRecieved := ini.ReadInteger('Statistics', 'fnvReportsRecieved', 0);
  fo3ReportsRecieved := ini.ReadInteger('Statistics', 'fo3ReportsRecieved', 0);
  tes5Logins := ini.ReadInteger('Statistics', 'tes5Logins', 0);
  tes4Logins := ini.ReadInteger('Statistics', 'tes4Logins', 0);
  fnvLogins := ini.ReadInteger('Statistics', 'fnvLogins', 0);
  fo3Logins := ini.ReadInteger('Statistics', 'fo3Logins', 0);

  // save file
  ini.UpdateFile;
  ini.Free;
end;



end.
