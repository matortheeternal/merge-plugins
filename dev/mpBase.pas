unit mpBase;

interface

uses
  Windows, SysUtils, ShlObj, Classes, IniFiles, Dialogs,
  superobject,
  mpLogger, mpTracker,
  Registry,
  wbBSA,
  wbHelpers,
  wbInterface,
  wbImplementation,
  wbDefinitionsFNV, wbDefinitionsFO3, wbDefinitionsTES3, wbDefinitionsTES4,
  wbDefinitionsTES5;

type
  TEntry = class(TObject)
  public
    pluginName: string;
    records: string;
    version: string;
    rating: string;
    reports: string;
    notes: string;
    constructor Create; Overload;
    constructor Create(const s: string); Overload;
  end;
  TPluginFlag = (IS_BLACKLISTED, HAS_ERRORS, HAS_BSA, HAS_TRANSLATION, HAS_FACEDATA,
    HAS_VOICEDATA, HAS_FRAGMENTS);
  TPluginFlags = set of TPluginFlag;
  TPlugin = class(TObject)
    public
      pluginFile: IwbFile;
      hasData: boolean;
      fileSize: Int64;
      dateModified: string;
      flags: TPluginFlags;
      filename: string;
      merge: string;
      numRecords: string;
      numOverrides: string;
      author: string;
      entry: TEntry;
      description: TStringList;
      masters: TStringList;
      errors: TStringList;
      reports: TStringList;
      constructor Create; virtual;
      procedure GetData;
      procedure GetFlags;
      function GetFlagsString: string;
      procedure FindErrors;
  end;
  TMerge = class(TObject)
    public
      name: string;
      filename: string;
      dateBuilt: TDateTime;
      plugins: TStringList;
      pluginSizes: TList;
      pluginDates: TStringList;
      masters: TStringList;
      status: string;
      method: string;
      renumbering: string;
      mergeDataPath: string;
      mergePlugin: TPlugin;
      map: TStringList;
      files: TStringList;
      fails: TStringList;
      constructor Create; virtual;
      function Dump: ISuperObject;
      procedure LoadDump(obj: ISuperObject);
      function GetTimeCost(pluginObjects: TList): integer;
      procedure GetStatus;
      function GetStatusColor: integer;
  end;
  TSettings = class(TObject)
  public
    language: string;
    simpleDictionaryView: boolean;
    simplePluginsView: boolean;
    updateDictionary: boolean;
    updateProgram: boolean;
    usingMO: boolean;
    MODirectory: string;
    copyGeneralAssets: boolean;
    mergeDirectory: string;
    handleFaceGenData: boolean;
    handleVoiceAssets: boolean;
    handleMCMTranslations: boolean;
    handleScriptFragments: boolean;
    extractBSAs: boolean;
    buildMergedBSA: boolean;
    constructor Create; virtual;
    procedure Save(const filename: string);
    procedure Load(const filename: string);
  end;

  function LoadDataPath: boolean;
  procedure LoadDefinitions;
  function IsOverride(aRecord: IwbMainRecord): boolean;
  function LocalFormID(aRecord: IwbMainRecord): integer;
  function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
  function CountOverrides(aFile: IwbFile): integer;
  procedure GetMasters(aFile: IwbFile; var sl: TStringList);
  procedure AddMasters(aFile: IwbFile; var sl: TStringList);
  function BSAExists(filename: string): boolean;
  function TranslationExists(filename: string): boolean;
  function FaceDataExists(filename: string): boolean;
  function VoiceDataExists(filename: string): boolean;
  function FragmentsExist(filename: string): boolean;
  function CheckForErrorsLinear(const aElement: IwbElement; LastRecord: IwbMainRecord; var errors: TStringList): IwbMainRecord;
  function CheckForErrors(const aIndent: Integer; const aElement: IwbElement; var errors: TStringList): Boolean;
  function PluginsModified(plugins: TList; merge: TMerge): boolean;
  function csvText(s: string): string;
  function FormatByteSize(const bytes: Int64): string;
  function DateBuiltString(date: TDateTime): string;
  function IntegerListSum(list: TList; maxIndex: integer): integer;
  function Wordwrap(var s: string; charCount: integer): string;
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  function GetCSIDLShellFolder(CSIDLFolder: integer): string;
  function GetFileSize(const aFilename: String): Int64;
  function GetLastModified(const aFileName: String): TDateTime;
  function RecursiveFileSearch(aPath, aFileName: string; ignore: TStringList; maxDepth: integer): string;
  procedure LoadSettings;
  procedure LoadDictionary;
  function GetRatingColor(rating: real): integer;
  function GetEntry(pluginName, numRecords, version: string): TEntry;
  function IsBlacklisted(const filename: string): boolean;
  function AppendIfMissing(str, substr: string): string;
  function PluginByFilename(plugins: TList; filename: string): TPlugin;
  function MergeByName(merges: TList; name: string): TMerge;
  function MergeByFilename(merges: TList; filename: string): TMerge;
  function CreateNewMerge(merges: TList): TMerge;
  function CreateNewPlugin(plugins: TList; filename: string): TPlugin;

const
  ProgramVersion = '2.0';
  flagChar = 'XEATGVF';

var
  dictionary: TList;
  blacklist: TList;
  settings: TSettings;
  handler: IwbContainerHandler;
  PluginsList: TList;

implementation

{******************************************************************************}
{ Bethesda Plugin Functions
  Set of functions that read bethesda plugin files for various attributes.

  List of functions:
  - LoadDataPath
  - LoadDefinitions
  - IsOverride
  - LocalFormID
   -LoadOrderPrefix
  - CountOverrides
  - GetMasters
  - AddMasters
  - BSAExists
  - MCMExists
  - FaceDataExists
  - VoiceDataExists
  - FragmentsExist
  - CheckForErorrsLinear
  - CheckForErrors
}
{*****************************************************************************}

{ Loads Data Path from Registry key or app path }
function LoadDataPath: boolean;
const
  sBethRegKey             = '\SOFTWARE\Bethesda Softworks\';
  sBethRegKey64           = '\SOFTWARE\Wow6432Node\Bethesda Softworks\';
var
  s: string;
begin
  Result := false;
  if wbDataPath = '' then with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;

    if not OpenKeyReadOnly(sBethRegKey + wbGameName + '\') then
      if not OpenKeyReadOnly(sBethRegKey64 + wbGameName + '\') then begin
        s := 'Fatal: Could not open registry key: ' + sBethRegKey + wbGameName + '\';
        if wbGameMode = gmTES5 then
          ShowMessage(s+#13#10'This can happen after Steam updates, run game''s launcher to restore registry settings');
        Result := false;
        exit;
      end;

    wbDataPath := ReadString('Installed Path');

    if wbDataPath = '' then begin
      s := 'Fatal: Could not determine '+wbGameName+' installation path, no "Installed Path" registry key';
      if wbGameMode = gmTES5 then
        ShowMessage(s+#13#10'This can happen after Steam updates, run game''s launcher to restore registry settings');
      Result := false;
    end;
  finally
    Free;
  end;

  if wbDataPath <> '' then begin
    Result := true;
    wbDataPath := IncludeTrailingPathDelimiter(wbDataPath) + 'Data\';
  end;
end;

{ Loads definitions based on wbGameMode }
procedure LoadDefinitions;
begin
  if wbGameMode = gmTES5 then
    DefineTES5;
end;

{ Returns true if the input record is an override record }
function IsOverride(aRecord: IwbMainRecord): boolean;
begin
  Result := not aRecord.Equals(aRecord.MasterOrSelf);
end;

{ Gets the local formID of a record (so no load order prefix) }
function LocalFormID(aRecord: IwbMainRecord): integer;
begin
  Result := aRecord.LoadOrderFormID and $00FFFFFF;
end;

{ Gets the load order prefix from the FormID of a record }
function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
begin
  Result := aRecord.LoadOrderFormID and $FF000000;
end;

{ Returns the number of override records in a file }
function CountOverrides(aFile: IwbFile): integer;
var
  i: Integer;
  aRecord: IwbMainRecord;
begin
  Result := 0;
  for i := 0 to Pred(aFile.GetRecordCount) do begin
    aRecord := aFile.GetRecord(i);
    if IsOverride(aRecord) then
      Inc(Result);
  end;
end;

{ Gets the masters in an IwbFile and puts them into a stringlist }
procedure GetMasters(aFile: IwbFile; var sl: TStringList);
var
  Container, MasterFiles, MasterFile: IwbContainer;
  i: integer;
begin
  Container := aFile as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  if Container.ElementExists['Master Files'] then begin
    MasterFiles := Container.ElementByPath['Master Files'] as IwbContainer;
    for i := 0 to MasterFiles.ElementCount - 1 do begin
      MasterFile := MasterFiles.Elements[i] as IwbContainer;
      sl.Add(MasterFile.GetElementEditValue('MAST - Filename'));
    end;
  end;
end;

{ Gets the masters in an IwbFile and puts them into a stringlist }
procedure AddMasters(aFile: IwbFile; var sl: TStringList);
var
  i: integer;
begin
  for i := 0 to Pred(sl.Count) do begin
    if Lowercase(aFile.FileName) = Lowercase(sl[i]) then
      continue;
    aFile.AddMasterIfMissing(sl[i]);
  end;
end;

{ Checks if a BSA exists associated with the given filename }
function BSAExists(filename: string): boolean;
var
  bsaFilename, ContainerName: string;
begin
  Result := false;
  bsaFilename := ChangeFileExt(filename, '.bsa');
  if FileExists(wbDataPath + bsaFilename) then begin
    ContainerName := wbDataPath + bsaFilename;
    if not handler.ContainerExists(ContainerName) then
      handler.AddBSA(ContainerName);
    Result := true;
  end;
end;

{ Returns true if a file exists at @path matching @filename }
function MatchingFileExists(path: string; filename: string): boolean;
var
  rec: TSearchRec;
begin
  Result := false;
  filename := Lowercase(filename);
  if FindFirst(path, faAnyFile, rec) = 0 then begin
    repeat
      if Pos(filename, Lowercase(rec.Name)) > 0 then begin
        Result := true;
        exit;
      end;
    until FindNext(rec) <> 0;
  end;
end;

{ Return true if MCM translation files for @filename are found }
function TranslationExists(filename: string): boolean;
var
  searchPath, bsaFilename, ContainerName: string;
  ResourceList: TStringList;
begin
  searchPath := wbDataPath + 'Interface\translations\*';
  Result := MatchingFileExists(searchPath, ChangeFileExt(filename, ''));
  if Result then exit;

  // check in BSA
  if BSAExists(filename) then begin
    bsaFilename := ChangeFileExt(filename, '.bsa');
    ContainerName := wbDataPath + bsaFilename;
    ResourceList := TStringList.Create;
    handler.ContainerResourceList(ContainerName, ResourceList, 'Interface\translations');
    Result := ResourceList.Count > 0;
  end;
end;

{ Return true if file-specific FaceGenData files for @filename are found }
function FaceDataExists(filename: string): boolean;
var
  facetintDir, facegeomDir, bsaFilename, ContainerName: string;
  ResourceList: TStringList;
  facetint, facegeom: boolean;
begin
  facetintDir := 'textures\actors\character\facegendata\facetint\' + filename;
  facegeomDir := 'meshes\actors\character\facegendata\facegeom\' + filename;
  facetint := DirectoryExists(wbDataPath + facetintDir);
  facegeom := DirectoryExists(wbDataPath + facegeomDir);
  Result := facetint or facegeom;
  if Result then exit;

  // check in BSA
  if BSAExists(filename) then begin
    bsaFilename := ChangeFileExt(filename, '.bsa');
    ContainerName := wbDataPath + bsaFilename;
    ResourceList := TStringList.Create;
    handler.ContainerResourceList(ContainerName, ResourceList, facetintDir);
    handler.ContainerResourceList(ContainerName, ResourceList, facegeomDir);
    Result := ResourceList.Count > 0;
  end;
end;

{ Return true if file-specific Voice files for @filename are found }
function VoiceDataExists(filename: string): boolean;
var
  voiceDir, bsaFilename, ContainerName: string;
  ResourceList: TStringList;
begin
  voiceDir := 'sound\voice\' + filename;
  Result := DirectoryExists(wbDataPath + voiceDir);
  if Result then exit;

  // check in BSA
  if BSAExists(filename) then begin
    bsaFilename := ChangeFileExt(filename, '.bsa');
    ContainerName := wbDataPath + bsaFilename;
    ResourceList := TStringList.Create;
    handler.ContainerResourceList(ContainerName, ResourceList, voiceDir);
    Result := ResourceList.Count > 0;
  end;
end;

{ Return true if file-specific Script Fragments for @filename are found }
function FragmentsExist(filename: string): boolean;
begin
  Result := false;
end;

{ Recursively traverse a container looking for errors }
function CheckForErrorsLinear(const aElement: IwbElement;
  LastRecord: IwbMainRecord; var errors: TStringList): IwbMainRecord;
var
  Error: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  Error := aElement.Check;
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    // first error in this record - show record's name
    if Assigned(Result) and (Result <> LastRecord) then begin
      errors.Add(Result.Name);
    end;
    errors.Add('    ' + aElement.Path + ' -> ' + Error);
  end else
    // passing through last record with error
    Result := LastRecord;
  if Supports(aElement, IwbContainerElementRef, Container) then
    for i := 0 to Pred(Container.ElementCount) do
      Result := CheckForErrorsLinear(Container.Elements[i], Result, errors);
end;

function CheckForErrors(const aIndent: Integer; const aElement: IwbElement;
  var errors: TStringList): Boolean;
var
  Error, msg: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  if Supports(aElement, IwbMainRecord) then
    Tracker.Update(1);

  Error := aElement.Check;
  Result := Error <> '';
  if Result then begin
    Error := aElement.Check;
    msg := StringOfChar(' ', aIndent * 2) + aElement.Name + ' -> ' + Error;
    Tracker.Write(msg);
    errors.Add(msg);
  end;

  // recursion
  if Supports(aElement, IwbContainerElementRef, Container) then
    for i := Pred(Container.ElementCount) downto 0 do
      Result := CheckForErrors(aIndent + 1, Container.Elements[i], errors) or Result;

  if Result and (Error = '') then begin
    msg := StringOfChar(' ', aIndent * 2) + 'Above errors were found in :' + aElement.Name;
    Tracker.Write(msg);
    errors.Add(msg);
  end;
end;

{ Checks to see if the plugins in a merge have been modified since it was last
  merged. }
function PluginsModified(plugins: TList; merge: TMerge): boolean;
var
  plugin: TPlugin;
  i: integer;
begin
  Result := false;
  for i := 0 to Pred(merge.plugins.count) do begin
    plugin := PluginByFilename(plugins, merge.plugins[i]);
    if Assigned(plugin) then begin
      if plugin.dateModified <> merge.pluginDates[i] then
        Result := true;
    end;
  end;
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

{ Appends a string to the end of another string if not already present }
function AppendIfMissing(str, substr: string): string;
begin
  Result := str;
  if Length(str) > Length(substr) then
    if Copy(str, Length(str) - Length(substr), Length(substr)) = substr then
      exit;

  Result := str + substr;
end;

function IntegerListSum(list: TList; maxIndex: integer): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to maxIndex do
    Inc(result, Integer(list[i]));
end;

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


{******************************************************************************}
{ Load order functions
  Set of functions for building a working load order.

  List of functions:
  - RemoveCommentsAndEmpty
  - RemoveMissingFiles
  - AddMissingFiles
{******************************************************************************}

{ Remove comments and empty lines from a stringlist }
procedure RemoveCommentsAndEmpty(var sl: TStringList);
var
  i, j: integer;
  s: string;
begin
  for i := Pred(sl.Count) downto 0 do begin
    s := Trim(sl.Strings[i]);
    j := Pos('#', s);
    if j > 0 then
      System.Delete(s, j, High(Integer));
    if Trim(s) = '' then
      sl.Delete(i);
  end;
end;

{ Remove nonexistent files from stringlist }
procedure RemoveMissingFiles(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if not FileExists(wbDataPath + sl.Strings[i]) then
      sl.Delete(i);
end;

{ Add missing *.esp and *.esm files to list }
procedure AddMissingFiles(var sl: TStringList);
var
  F: TSearchRec;
  i: integer;
begin
  // find last master
  for i := Pred(sl.Count) downto 0 do
    if IsFileESM(sl[i]) then
      Break;

  // search for missing plugins, add to end
  if FindFirst(wbDataPath + '*.esp', faAnyFile, F) = 0 then try
    repeat
      if sl.IndexOf(F.Name) = -1 then
        sl.Add(F.Name);
    until FindNext(F) <> 0;
  finally
    FindClose(F);
  end;

  // search for missing masters, add after last master
  if FindFirst(wbDataPath + '*.esm', faAnyFile, F) = 0 then try
    repeat
      if sl.IndexOf(F.Name) = -1 then begin
        sl.Insert(i, F.Name);
        Inc(i);
      end;
    until FindNext(F) <> 0;
  finally
    FindClose(F);
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

  if NOT GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @info) then
    EXIT;

  FileTime := info.ftLastWriteTime;

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
  aPath := AppendIfMissing(aPath, '\');
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

{******************************************************************************}
{ Dictionary and Settings methods
  Set of method for managing the dictionary.

  List of methods:
  - LoadSettings
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

procedure LoadDictionary;
var
  i: Integer;
  entry: TEntry;
  sl: TStringList;
begin
  // initialize dictionary and blacklist
  dictionary := TList.Create;
  blacklist := TList.Create;

  // load dictionary file
  sl := TStringList.Create;
  sl.LoadFromFile('dictionary.txt');

  // load dictionary file into entry object
  for i := 0 to Pred(sl.Count) do begin
    entry := TEntry.Create(sl[i]);
    if entry.rating = '-1.0' then
      blacklist.Add(entry);
    dictionary.Add(entry);
  end;

  // free temporary stringlist
  sl.Free;
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

function GetEntry(pluginName, numRecords, version: string): TEntry;
var
  i: Integer;
  entry: TEntry;
begin
  Result := TEntry.Create;
  for i := 0 to Pred(dictionary.Count) do begin
    entry := TEntry(dictionary[i]);
    if entry.pluginName = pluginName then begin
      Result := entry;
      exit;
    end;
  end;
end;

function IsBlacklisted(const filename: string): boolean;
var
  i: Integer;
  entry: TEntry;
begin
  Result := false;
  for i := 0 to Pred(blacklist.Count) do begin
    entry := TEntry(blacklist[i]);
    if entry.pluginName = filename then begin
      Result := true;
      break;
    end;
  end;
end;

{******************************************************************************}
{ Plugin and Merge methods
  Methods for dealing with plugins and merges.

  List of methods:
  - PluginByName
  - MergeByName
  - MergeByFilename
  - CreateNewMerge
  - CreateNewPlugin
}
{******************************************************************************}

{ Gets a plugin matching the given name. }
function PluginByFilename(plugins: TList; filename: string): TPlugin;
var
  i: integer;
  plugin: TPlugin;
begin
  Result := nil;
  for i := 0 to Pred(plugins.count) do begin
    plugin := TPlugin(plugins[i]);
    if plugin.filename = filename then begin
      Result := plugin;
      exit;
    end;
  end;
end;

{ Gets a merge matching the given name. }
function MergeByName(merges: TList; name: string): TMerge;
var
  i: integer;
  merge: TMerge;
begin
  Result := nil;
  for i := 0 to Pred(merges.Count) do begin
    merge := TMerge(merges[i]);
    if merge.name = name then begin
      Result := merge;
      exit;
    end;
  end;
end;


{ Gets a merge matching the given name. }
function MergeByFilename(merges: TList; filename: string): TMerge;
var
  i: integer;
  merge: TMerge;
begin
  Result := nil;
  for i := 0 to Pred(merges.Count) do begin
    merge := TMerge(merges[i]);
    if merge.filename = filename then begin
      Result := merge;
      exit;
    end;
  end;
end;

{ Create a new merge with non-conflicting name and filename }
function CreateNewMerge(merges: TList): TMerge;
var
  i: Integer;
  merge: TMerge;
  name: string;
begin
  merge := TMerge.Create;

  // deal with conflicting merge names
  i := 0;
  name := merge.name;
  while Assigned(MergeByName(merges, name)) do begin
    Inc(i);
    name := 'NewMerge' + IntToStr(i);
  end;
  merge.name := name;

  // deal with conflicting merge filenames
  i := 0;
  name := merge.filename;
  while Assigned(MergeByFilename(merges, name)) do begin
    Inc(i);
    name := 'NewMerge' + IntToStr(i) + '.esp';
  end;
  merge.filename := name;

  Result := merge;
end;

{ Create a new plugin }
function CreateNewPlugin(plugins: TList; filename: string): TPlugin;
var
  aFile: IwbFile;
  LoadOrder: integer;
  plugin: TPlugin;
begin
  Result := nil;
  LoadOrder := plugins.Count + 1;
  // fail if maximum load order reached
  if LoadOrder > 254 then begin
    Tracker.Write('Maximum load order reached!  Can''t create file '+filename);
    exit;
  end;

  // create new plugin file
  aFile := wbNewFile(wbDataPath + filename, LoadOrder);
  aFile._AddRef;

  // create new plugin object
  plugin := TPlugin.Create;
  plugin.filename := filename;
  plugin.pluginFile := aFile;
  plugins.Add(plugin);

  Result := plugin;
end;

{******************************************************************************}
{ Object methods
  Set of methods for objects TMerge and TPlugin

  List of methods:
  - TPlugin.Create
  - TPlugin.GetFlags
  - TPlugin.GetFlagsString
  - TPlugin.GetData
  - TMerge.Create
  - TMerge.Dump
  - TMerge.LoadDump
  - TMerge.GetTimeCost
  - TEntry.Create
  - TSettings.Create
  - TSettings.Save
  - TSettings.Load
}
{******************************************************************************}


constructor TPlugin.Create;
begin
  hasData := false;
  merge := ' ';
  description := TStringList.Create;
  masters := TStringList.Create;
  errors := TStringList.Create;
  reports := TStringList.Create;
end;

{ Gets the flag values for a TPlugin }
procedure TPlugin.GetFlags;
begin
  if IsBlacklisted(filename) then begin
    flags := flags + [IS_BLACKLISTED];
    exit;
  end;
  if errors.Count > 0 then
    flags := flags + [HAS_ERRORS];
  if BSAExists(filename) then
    flags := flags + [HAS_BSA];
  if TranslationExists(filename) then
    flags := flags + [HAS_TRANSLATION];
  if FaceDataExists(filename) then
    flags := flags + [HAS_FACEDATA];
  if VoiceDataExists(filename) then
    flags := flags + [HAS_VOICEDATA];
  if FragmentsExist(filename) then
    flags := flags + [HAS_FRAGMENTS];
end;

{ Returns a string representing the flags in a plugin }
function TPlugin.GetFlagsString: string;
var
  flag: TPluginFlag;
begin
  Result := '';
  for flag in flags do
     Result := Result + flagChar[Ord(flag) + 1];

  {if IS_BLACKLISTED in flags then
    Result := Result + flagChar[IS_BLACKLISTED + 1];
  if HAS_ERRORS in flags then
    Result := Result + flagChar[HAS_ERRORS + 1];
  if HAS_BSA in flags then
    Result := Result + flagChar[HAS_BSA + 1];
  if HAS_TRANSLATION in flags then
    Result := Result + flagChar[HAS_TRANSLATION + 1];
  if HAS_FACEDATA in flags then
    Result := Result + flagChar[HAS_FACEDATA + 1];
  if HAS_VOICEDATA in flags then
    Result := Result + flagChar[HAS_VOICEDATA + 1];
  if HAS_FRAGMENTS in flags then
    Result := Result + flagChar[HAS_FRAGMENTS + 1];}
end;

{ Fetches data associated with a plugin. }
procedure TPlugin.GetData;
var
  Container: IwbContainer;
  s: string;
begin
  hasData := true;
  // get data
  filename := pluginFile.FileName;
  Container := pluginFile as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  author := Container.GetElementEditValue('CNAM - Author');
  numRecords := Container.GetElementEditValue('HEDR - Header\Number of Records');
  s := Container.GetElementEditValue('SNAM - Description');
  description.Text := Wordwrap(s, 70);
  GetMasters(pluginFile, masters);
  GetFlags;
  entry := GetEntry(filename, numRecords, ProgramVersion);
  s := Trim(StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]));
  reports.Text := Wordwrap(s, 70);

  // get file attributes
  fileSize := GetFileSize(wbDataPath + filename);
  dateModified := DateTimeToStr(GetLastModified(wbDataPath + filename));

  if not (IS_BLACKLISTED in flags) then
    numOverrides := IntToStr(CountOverrides(pluginFile));
end;

{ Checks for errors in a plugin }
procedure TPlugin.FindErrors;
begin
  errors.Clear;
  CheckForErrors(2, pluginFile as IwbElement, errors);
  if errors.Count = 0 then
    errors.Add('None.');
end;

{ TMerge Constructor }
constructor TMerge.Create;
begin
  name := 'NewMerge';
  filename := 'NewMerge.esp';
  dateBuilt := 0;
  plugins := TStringList.Create;
  pluginSizes := TList.Create;
  pluginDates := TStringList.Create;
  masters := TStringList.Create;
  map := TStringList.Create;
  files := TStringList.Create;
  method := 'Overrides';
  renumbering := 'Conflicting';
  fails := TStringList.Create;
end;

{ Produces a dump of the merge. }
function TMerge.Dump: ISuperObject;
var
  obj: ISuperObject;
  i: integer;
begin
  obj := SO;

  // name, filename, datebuilt
  obj.S['name'] := name;
  obj.S['filename'] := filename;
  obj.S['dateBuilt'] := DateTimeToStr(dateBuilt);

  // plugins, pluginSizes, pluginDates, masters
  obj.O['plugins'] := SA([]);
  for i := 0 to Pred(plugins.Count) do
    obj.A['plugins'].S[i] := plugins[i];
  obj.O['pluginSizes'] := SA([]);
  for i := 0 to Pred(pluginSizes.Count) do
    obj.A['pluginSizes'].I[i] := Int64(pluginSizes[i]);
  obj.O['pluginDates'] := SA([]);
  for i := 0 to Pred(pluginDates.Count) do
    obj.A['pluginDates'].S[i] := pluginDates[i];
  obj.O['masters'] := SA([]);
  for i := 0 to Pred(masters.Count) do
    obj.A['masters'].S[i] := masters[i];

  // method, renumbering
  obj.S['method'] := method;
  obj.S['renumbering'] := renumbering;

  // files, log
  obj.O['files'] := SA([]);
  for i := 0 to Pred(files.Count) do
    obj.A['files'].S[i] := files[i];
  obj.O['fails'] := SA([]);
  for i := 0 to Pred(fails.Count) do
    obj.A['fails'].S[i] := fails[i];

  Result := obj;
end;

{ Loads a dump of a merge. }
procedure TMerge.LoadDump(obj: ISuperObject);
var
  item: ISuperObject;
begin
  // load object attributes
  name := obj.AsObject.S['name'];
  filename := obj.AsObject.S['filename'];
  method := obj.AsObject.S['method'];
  renumbering := obj.AsObject.S['renumbering'];

  // try loading dateBuilt and parsing to DateTime
  try
    dateBuilt := StrToDateTime(obj.AsObject.S['dateBuilt']);
  except on Exception do
    dateBuilt := 0; // on exception set to never built
  end;

  // load array attributes
  for item in obj['plugins'] do
    plugins.Add(item.AsString);
  for item in obj['pluginSizes'] do
    pluginSizes.Add(Pointer(item.AsInteger));
  for item in obj['pluginDates'] do
    pluginDates.Add(item.AsString);
  for item in obj['masters'] do
    masters.Add(item.AsString);
  for item in obj['files'] do
    files.Add(item.AsString);
  for item in obj['fails'] do
    fails.Add(item.AsString);
end;

function TMerge.GetTimeCost: integer;
var
  i: Integer;
  plugin: TPlugin;
begin
  Result := 1;
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(PluginsList, plugins[i]);
    if Assigned(plugin) then begin
      if not plugin.HasData then
        plugin.GetData;
      Inc(Result, 2*StrToInt(plugin.numRecords));
      if not plugin.errors.Count > 0 then
        Inc(Result, StrToInt(plugin.numRecords));
    end;
  end;
end;

procedure TMerge.GetStatus;
var
  i: Integer;
  plugin: TPlugin;
begin
  Tracker.Write('Getting status for '+name);

  // don't merge if no plugins to merge
  if plugins.Count < 1 then begin
    Tracker.Write('  No plugins to merge');
    status := 'No plugins to merge';
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is blank
  if settings.usingMO and (settings.MODirectory = '') then begin
    Tracker.Write('  Mod Organizer Directory blank');
    status := 'Mod Organizer Directory blank';
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is invalid
  if settings.usingMO and not DirectoryExists(settings.MODirectory) then begin
     Tracker.Write('  Mod Organizer Directory invalid');
     status := 'Mod Organizer Directory invalid';
     exit;
  end;

  // loop through plugins
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(PluginsList, plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      Tracker.Write('  Plugin '+plugins[i]+' is missing');
      status := 'Plugin '+plugins[i]+' is not loaded';
      exit;
    end;

    // check for errors
    if plugin.errors.Count = 0 then begin
      Tracker.Write('  Checking for errors in '+plugin.filename);
      plugin.FindErrors;
    end;
    if plugin.errors[0] = 'None.' then begin
      Tracker.Write('  No errors in '+plugin.filename);
      continue
    end
    else begin
      Tracker.Write('  '+plugin.filename+' has errors');
      status := plugin.filename+' has errors';
      exit;
    end;
  end;

  // status green, ready to go
  Tracker.Write('  Ready to be merged.');
  Tracker.Write(' ');
  status := 'Ready to be merged.';
end;

function TMerge.GetStatusColor: integer;
begin
  Result := $0000FF;
  if status = '' then
    Result := $707070;
  if status = 'Ready to be merged.' then
    Result := $009000;
end;

{ TEntry Constructor }
constructor TEntry.Create;
begin
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
        pluginName := Copy(s, lastIndex, i - lastIndex)
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

{ TSetting.Create }
constructor TSettings.Create;
begin
  language := 'English';
  simpleDictionaryView := false;
  simplePluginsView := false;
  updateDictionary := false;
  updateProgram := false;
  usingMO := false;
  MODirectory := '';
  copyGeneralAssets := false;
  mergeDirectory := wbDataPath;
  handleFaceGenData := true;
  handleVoiceAssets := true;
  handleMCMTranslations := true;
  handleScriptFragments := false;
  extractBSAs := false;
  buildMergedBSA := false;
end;

procedure TSettings.Save(const filename: string);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  ini.WriteString('Settings', 'Language', language);
  ini.WriteBool('Settings', 'simpleDictionaryView', simpleDictionaryView);
  ini.WriteBool('Settings', 'simplePluginsView', simplePluginsView);
  ini.WriteBool('Settings', 'updateDictionary', updateDictionary);
  ini.WriteBool('Settings', 'updateProgram', updateProgram);
  ini.WriteBool('Settings', 'usingMO', usingMO);
  ini.WriteString('Settings', 'MODirectory', MODirectory);
  ini.WriteBool('Settings', 'copyGeneralAssets', copyGeneralAssets);
  ini.WriteString('Settings', 'mergeDirectory', mergeDirectory);
  ini.WriteBool('Settings', 'handleFaceGenData', handleFaceGenData);
  ini.WriteBool('Settings', 'handleVoiceAssets', handleVoiceAssets);
  ini.WriteBool('Settings', 'handleMCMTranslations', handleMCMTranslations);
  ini.WriteBool('Settings', 'handleScriptFragments', handleScriptFragments);
  ini.WriteBool('Settings', 'extractBSAs', extractBSAs);
  ini.WriteBool('Settings', 'buildMergedBSA', buildMergedBSA);
  ini.UpdateFile;
  ini.Free;
end;

procedure TSettings.Load(const filename: string);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  language := ini.ReadString('Settings', 'Language', 'English');
  simpleDictionaryView := ini.ReadBool('Settings', 'simpleDictionaryView', false);
  simplePluginsView := ini.ReadBool('Settings', 'simplePluginsView', false);
  updateDictionary := ini.ReadBool('Settings', 'updateDictionary', false);
  updateProgram := ini.ReadBool('Settings', 'updateProgram', false);
  usingMO := ini.ReadBool('Settings', 'usingMO', false);
  MODirectory := ini.ReadString('Settings', 'MODirectory', '');
  copyGeneralAssets := ini.ReadBool('Settings', 'copyGeneralAssets', false);
  mergeDirectory := ini.ReadString('Settings', 'mergeDirectory', wbDataPath);
  handleFaceGenData := ini.ReadBool('Settings', 'handleFaceGenData', false);
  handleVoiceAssets := ini.ReadBool('Settings', 'handleVoiceAssets', false);
  handleMCMTranslations := ini.ReadBool('Settings', 'handleMCMTranslations', false);
  handleScriptFragments := ini.ReadBool('Settings', 'handleScriptFragments', false);
  extractBSAs := ini.ReadBool('Settings', 'extractBSAs', false);
  buildMergedBSA := ini.ReadBool('Settings', 'buildMergedBSA', false);
  ini.UpdateFile;
  ini.Free;
end;

end.