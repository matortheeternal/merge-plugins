unit mteHelpers;

interface

uses
  Windows, SysUtils, Masks, Dialogs, StdCtrls, FileCtrl, ShellApi, Classes,
  ComCtrls, CommCtrl, DateUtils, shlObj;

  { General functions }
  function csvText(s: string): string;
  function FormatByteSize(const bytes: Int64): string;
  function DateBuiltString(date: TDateTime): string;
  function DateTimeToSQL(date: TDateTime): string;
  function SQLToDateTime(date: string): TDateTime;
  function RateStr(date: TDateTime): string;
  function TimeStr(date: TDateTime): string;
  function AppendIfMissing(str, substr: string): string;
  function StrEndsWith(s1, s2: string): boolean;
  function RemoveFromEnd(s1, s2: string): string;
  function IntegerListSum(list: TList; maxIndex: integer): integer;
  function Wordwrap(var s: string; charCount: integer): string;
  function ContainsMatch(var sl: TStringList; const s: string): boolean;
  function IsURL(s: string): boolean;
  function IsDotFile(fn: string): boolean;
  procedure SaveStringToFile(s: string; fn: string);
  function ApplyTemplate(const template: string; var map: TStringList): string;
  { Windows API functions }
  procedure ExecNewProcess(ProgramName: string; synchronous: Boolean);
  procedure BrowseForFile(var ed: TEdit; filter, initDir: string);
  procedure BrowseForFolder(var ed: TEdit; initDir: string);
  function GetCSIDLShellFolder(CSIDLFolder: integer): string;
  function GetFileSize(const aFilename: String): Int64;
  function GetLastModified(const aFileName: String): TDateTime;
  function RecursiveFileSearch(aPath, aFileName: string; ignore: TStringList; maxDepth: integer): string;
  procedure CopyDirectory(src, dst: string; fIgnore, dIgnore: TStringList);
  procedure GetFilesList(path: string; var fIgnore, dIgnore, list: TStringList);
  procedure CopyFiles(src, dst: string; var list: TStringList);
  procedure CorrectListViewWidth(var lv: TListView);
  function GetVersionMem: string;
  function FileVersion(const FileName: string): String;

implementation

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

function DateTimeToSQL(date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:mm:ss', date);
end;

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

{ Converts a TDateTime to a rate string, e.g. Every 24.0 hours }
function RateStr(date: TDateTime): string;
begin
  if date > 1.0 then
    Result := Format('Every %0.2f days', [date])
  else if date * 24.0 > 1.0 then
    Result := Format('Every %0.1f hours', [date * 24.0])
  else if date * 24.0 * 60.0 > 1.0 then
    Result := Format('Every %0.1f minutes', [date * 24.0 * 60.0])
  else
    Result := Format('Every %0.1f seconds', [date * 24.0 * 60.0 * 60.0]);
end;

{ Converts a TDateTime to a time string, e.g. 19d 20h 3m 30s }
function TimeStr(date: TDateTime): string;
begin
  Result := Format('%dd %dh %dm', [Trunc(date), HourOf(date), MinuteOf(date)]);
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
var
  strEnd: string;
begin
  Result := str;
  if Length(str) > Length(substr) then begin
    strEnd :=  Copy(str, Length(str) - Length(substr) + 1, Length(substr));
    if not SameText(strEnd, substr) then
      Result := str + substr;
  end;
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

{ Returns true if @fn is . or .. }
function IsDotFile(fn: string): boolean;
begin
  Result := (fn = '.') or (fn = '..');
end;

{ Saves a string @s to a file at @fn }
procedure SaveStringToFile(s: string; fn: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := s;
  sl.SaveToFile(fn);
  sl.Free;
end;

function ApplyTemplate(const template: string; var map: TStringList): string;
const
  openTag = '{{';
  closeTag = '}}';
var
  i: Integer;
  name, value: string;
begin
  Result := template;
  for i := 0 to Pred(map.Count) do begin
    name := map.Names[i];
    value := map.ValueFromIndex[i];
    Result := StringReplace(Result, openTag + name + closeTag, value, [rfReplaceAll]);
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

{ Create a new synchronous or asynchronous process }
procedure ExecNewProcess(ProgramName: string; synchronous: Boolean);
var
  StartInfo : TStartupInfo;
  ProcInfo : TProcessInformation;
  CreateOK : Boolean;
begin
  { fill with known state }
  FillChar(StartInfo, SizeOf(TStartupInfo), #0);
  FillChar(ProcInfo, SizeOf(TProcessInformation), #0);
  StartInfo.cb := SizeOf(TStartupInfo);
  CreateOK := CreateProcess(PChar(ProgramName), nil, nil, nil,False,
              CREATE_NEW_PROCESS_GROUP+NORMAL_PRIORITY_CLASS,
              nil, nil, StartInfo, ProcInfo);

  // check if successful
  if CreateOK then begin
    if synchronous then
      WaitForSingleObject(ProcInfo.hProcess, INFINITE);
  end
  else
    ShowMessage('Unable to run '+ProgramName);

  // close handles
  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);
end;

{ Links a file selection through a TOpenDialog to the text stored in @ed,
  applying filter @filter }
procedure BrowseForFile(var ed: TEdit; filter, initDir: string);
var
  openDialog: TOpenDialog;
begin
  openDialog := TOpenDialog.Create(ed.Parent);

  if FileExists(ed.Text) then
    openDialog.InitialDir := ExtractFilePath(ed.Text)
  else if DirectoryExists(ed.Text) then
    openDialog.InitialDir := ed.Text
  else
    openDialog.InitialDir := initDir;

  openDialog.Filter := filter;
  if openDialog.Execute then
    ed.Text := openDialog.FileName;
end;

{ Links a file selection through a TOpenDialog to the text stored in @ed,
  applying filter @filter }
procedure BrowseForFolder(var ed: TEdit; initDir: string);
var
  s: string;
begin
  // start in current directory value if valid
  if DirectoryExists(ed.Text) then
    s := ed.Text
  else
    s := initDir;
  // prompt user to select a directory
  SelectDirectory('Select a directory', '', s, []);

  // save text to TEdit
  if s <> '' then
    ed.Text := AppendIfMissing(s, '\');
end;

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

{ Fixes @lv's width to fit client width if it has autosizable columns,
  which resolves an issue where autosize doesn't work on virtual vsReport
  TListViews when a scroll bar becomes visible. }
procedure CorrectListViewWidth(var lv: TListView);
var
  i, w: Integer;
  col: TListColumn;
  AutoSizedColumns: TList;
begin
  AutoSizedColumns := TList.Create;
  w := lv.ClientWidth;

  // loop through columns keeping track of remaining width
  for i := 0 to Pred(lv.Columns.Count) do begin
    col := lv.Columns[i];
    if col.AutoSize then
      AutoSizedColumns.Add(col)
    else
      Dec(w, ListView_GetColumnWidth(lv.Handle, i));
  end;

  // set auotsized columns to fit client width
  for i := 0 to Pred(AutoSizedColumns.Count) do begin
    col := TListColumn(AutoSizedColumns[i]);
    col.Width := w div AutoSizedColumns.Count;
  end;

  // clean up
  AutoSizedColumns.Free;
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

{ Get program version from disk }
function FileVersion(const FileName: string): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('%d.%d.%d.%d', [
            HiWord(dwFileVersionMS), //Major
            LoWord(dwFileVersionMS), //Minor
            HiWord(dwFileVersionLS), //Release
            LoWord(dwFileVersionLS)]); //Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

end.