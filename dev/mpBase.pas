unit mpBase;

interface

uses
  Windows, SysUtils, ShlObj, Classes, IniFiles, Dialogs,
  superobject,
  mpLogger, mpTracker, mpProgressForm,
  Registry,
  wbBSA,
  wbHelpers,
  wbInterface,
  wbImplementation,
  wbDefinitionsFNV, wbDefinitionsFO3, wbDefinitionsTES3, wbDefinitionsTES4,
  wbDefinitionsTES5;

type
  TGameMode = Record
    longName: string;
    gameName: string;
    gameMode: TwbGameMode;
    appName: string;
    exeName: string;
  end;
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
  TMergeStatus = Record
    id: integer;
    color: integer;
    caption: string[64];
  end;
  TPluginFlag = (IS_BLACKLISTED, HAS_ERRORS, HAS_BSA, HAS_TRANSLATION, HAS_FACEDATA,
    HAS_VOICEDATA, HAS_FRAGMENTS);
  TPluginFlags = set of TPluginFlag;
  TPlugin = class(TObject)
    public
      _File: IwbFile;
      hasData: boolean;
      hash: string;
      fileSize: Int64;
      dateModified: string;
      flags: TPluginFlags;
      filename: string;
      merge: string;
      numRecords: string;
      numOverrides: string;
      author: string;
      dataPath: string;
      entry: TEntry;
      description: TStringList;
      masters: TStringList;
      errors: TStringList;
      reports: TStringList;
      constructor Create; virtual;
      procedure GetData;
      procedure GetHash;
      procedure GetFlags;
      function GetFlagsString: string;
      procedure GetDataPath;
      procedure FindErrors;
  end;
  TMerge = class(TObject)
    public
      name: string;
      filename: string;
      dateBuilt: TDateTime;
      plugins: TStringList;
      hashes: TStringList;
      masters: TStringList;
      status: integer;
      method: string;
      renumbering: string;
      dataPath: string;
      plugin: TPlugin;
      map: TStringList;
      files: TStringList;
      fails: TStringList;
      constructor Create; virtual;
      function Dump: ISuperObject;
      procedure LoadDump(obj: ISuperObject);
      function GetTimeCost: integer;
      procedure GetStatus;
      function PluginsModified: boolean;
      function FilesExist: boolean;
      function GetStatusColor: integer;
  end;
  TSettings = class(TObject)
  public
    language: string;
    defaultGame: integer;
    selectedGame: integer;
    tes5path: string;
    fnvpath: string;
    tes4path: string;
    fo3path: string;
    username: string;
    saveReportsLocally: boolean;
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

  function GamePathValid(path: string; id: integer): boolean;
  procedure SetGame(id: integer);
  function GetGameID(name: string): integer;
  function GetGamePath(gameName: string): string;
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
  procedure ExtractBSA(ContainerName, folder, destination: string);
  function CheckForErrorsLinear(const aElement: IwbElement; LastRecord: IwbMainRecord; var errors: TStringList): IwbMainRecord;
  function CheckForErrors(const aIndent: Integer; const aElement: IwbElement; var errors: TStringList): Boolean;
  procedure CreateSEQFile(merge: TMerge);
  function csvText(s: string): string;
  function FormatByteSize(const bytes: Int64): string;
  function DateBuiltString(date: TDateTime): string;
  function IntegerListSum(list: TList; maxIndex: integer): integer;
  function Wordwrap(var s: string; charCount: integer): string;
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
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
  function PluginByFilename(filename: string): TPlugin;
  function MergeByName(merges: TList; name: string): TMerge;
  function MergeByFilename(merges: TList; filename: string): TMerge;
  function CreateNewMerge(merges: TList): TMerge;
  function CreateNewPlugin(filename: string): TPlugin;
  procedure SaveMerges;
  procedure LoadMerges;

const
  ProgramVersion = '2.0';
  flagChar = 'XEATGVF';
  StatusArray: array[0..9] of TMergeStatus = (
    ( id: 0; color: $808080; caption: 'Unknown'; ),
    ( id: 1; color: $0000FF; caption: 'No plugins to merge'; ),
    ( id: 2; color: $0000FF; caption: 'Directories invalid'; ),
    ( id: 3; color: $0000FF; caption: 'Plugins not loaded'; ),
    ( id: 4; color: $0000FF; caption: 'Errors in plugins'; ),
    ( id: 5; color: $900000; caption: 'Up to date'; ),
    ( id: 6; color: $900000; caption: 'Up to date [Forced]'; ),
    ( id: 7; color: $009000; caption: 'Ready to be built'; ),
    ( id: 8; color: $009000; caption: 'Ready to be rebuilt'; ),
    ( id: 9; color: $009000; caption: 'Ready to be rebuilt [Forced]'; )
  );
  GameArray: array[1..4] of TGameMode = (
    ( longName: 'Skyrim'; gameName: 'Skyrim'; gameMode: gmTES5;
      appName: 'TES5'; exeName: 'TESV.exe'; ),
    ( longName: 'Fallout New Vegas'; gameName: 'FalloutNV'; gameMode: gmFNV;
      appName: 'FNV'; exeName: 'FalloutNV.exe'; ),
    ( longName: 'Oblivion'; gameName: 'Oblivion'; gameMode: gmTES4;
      appName: 'TES4'; exeName: 'Oblivion.exe'; ),
    ( longName: 'Fallout 3'; gameName: 'Fallout3'; gameMode: gmFO3;
      appName: 'FO3'; exeName: 'Fallout3.exe'; )
  );
  UpToDateStatuses = [5, 6];
  BuildStatuses = [7, 8, 9];
  RebuildStatuses = [8, 9];
  ForcedStatuses = [6, 9];

var
  dictionary, blacklist, PluginsList, MergesList: TList;
  settings: TSettings;
  handler: IwbContainerHandler;
  bDontSave, bCaptureTracker, bChangeGameMode: boolean;
  tempPath, dictionaryFilename: string;

implementation

{******************************************************************************}
{ Bethesda Plugin Functions
  Set of functions that read bethesda plugin files for various attributes.

  List of functions:
  - GamePathValid
  - SetGame
  - GetGameID
  - GetGamePath
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
  - ExtractBSA
  - CheckForErorrsLinear
  - CheckForErrors
  - PluginsModified
  - CreatSEQFile
}
{*****************************************************************************}

{ Check if game paths are valid }
function GamePathValid(path: string; id: integer): boolean;
begin
  Result := FileExists(path + GameArray[id].exeName)
    and DirectoryExists(path + 'Data');
end;

{ Sets the game mode in the TES5Edit API }
procedure SetGame(id: integer);
begin
  wbGameName := GameArray[id].gameName;
  wbGameMode := GameArray[id].gameMode;
  wbAppName := GameArray[id].appName;
  case id of
    1: wbDataPath := settings.tes5path + 'Data\';
    2: wbDataPath := settings.fnvpath + 'Data\';
    3: wbDataPath := settings.tes4path + 'Data\';
    4: wbDataPath := settings.fo3path + 'Data\';
  end;
end;

{ Get the game ID associated with a game long name }
function GetGameID(name: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to 4 do
    if GameArray[i].longName = name then begin
      Result := i;
      exit;
    end;
end;

{ Gets the path of a game from registry key or app path }
function GetGamePath(gameName: string): string;
const
  sBethRegKey             = '\SOFTWARE\Bethesda Softworks\';
  sBethRegKey64           = '\SOFTWARE\Wow6432Node\Bethesda Softworks\';
begin
  Result := '';
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;

    if not OpenKeyReadOnly(sBethRegKey + gameName + '\') then
      if not OpenKeyReadOnly(sBethRegKey64 + gameName + '\') then
        exit;

    Result := ReadString('Installed Path');
  finally
    Free;
  end;

  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

{ Loads definitions based on wbGameMode }
procedure LoadDefinitions;
begin
  case wbGameMode of
    gmTES5: DefineTES5;
    gmFNV: DefineFNV;
    gmTES4: DefineTES4;
    gmFO3: DefineTES3;
  end;
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

{ Extracts assets from @folder in the BSA @filename to @destination }
procedure ExtractBSA(ContainerName, folder, destination: string);
var
  ResourceList: TStringList;
  i: Integer;
begin
  if not handler.ContainerExists(ContainerName) then begin
    Tracker.Write('    '+ContainerName+' not loaded.');
    exit;
  end;
  ResourceList := TStringList.Create;
  handler.ContainerResourceList(ContainerName, ResourceList, folder);
  for i := 0 to Pred(ResourceList.Count) do
    handler.ResourceCopy(ContainerName, ResourceList[i], destination);
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

{ Creates a SEQ (sequence) file for the input plugin.  Important for quests that
  are Start Game Enabled to execute properly. }
procedure CreateSEQFile(merge: TMerge);
var
  _File: IwbFile;
  Group: IwbGroupRecord;
  n: Integer;
  MainRecord: IwbMainRecord;
  QustFlags: IwbElement;
  FormIDs: array of Cardinal;
  FileStream: TFileStream;
  p, s: string;
begin
  _File := merge.plugin._File;
  Group := _File.GroupBySignature['QUST'];

  // don't create SEQ file if no QUST record group
  if not Assigned(Group) then 
    exit;
  
  // loop through child elements  
  for n := 0 to Pred(Group.ElementCount) do begin
    if not Supports(Group.Elements[n], IwbMainRecord, MainRecord) then 
      continue;
    
    // script quests that are not start game enabled
    QustFlags := MainRecord.ElementByPath['DNAM - General\Flags'];
    if not (Assigned(QustFlags) and (QustFlags.NativeValue and 1 > 0)) then 
      continue;
    
    // skip quests that aren't overrides or newly flagged as start game enabled    
    if not (IsOverride(MainRecord) or (MainRecord.Master.ElementNativeValues['DNAM\Flags'] and 1 = 0)) then
      continue;    
    
    // add quest formID to formIDs array
    SetLength(FormIDs, Succ(Length(FormIDs)));
    FormIDs[High(FormIDs)] := MainRecord.FixedFormID;
  end;
  
  // write formIDs to disk
  if Length(FormIDs) <> 0 then try
    p := merge.dataPath + 'seq\';
    if not ForceDirectories(p) then
      raise Exception.Create('  Unable to create SEQ directory for merge.');
    s := p + ChangeFileExt(_File.FileName, '.seq');
    FileStream := TFileStream.Create(s, fmCreate);
    FileStream.WriteBuffer(FormIDs[0], Length(FormIDs)*SizeOf(Cardinal));
    Tracker.Write('  Created SEQ file: ' + s);
    merge.files.Add(s);
  except
    on e: Exception do begin
      if Assigned(FileStream) then
        FreeAndNil(FileStream);
      Tracker.Write('  Error: Can''t create SEQ file: ' + s + ', ' + E.Message);
      Exit;
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
  - PluginListCompare
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

{ Get date modified for plugins in load order and store in stringlist objects }
procedure GetPluginDates(var sl: TStringList);
var
  i: Integer;
begin
  for i := 0 to Pred(sl.Count) do
    sl.Objects[i] := TObject(FileAge(wbDataPath + sl[i]));
end;

{ Compare function for sorting load order by date modified/esms }
function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  IsESM1, IsESM2: Boolean;
  FileAge1,FileAge2: Integer;
  FileDateTime1, FileDateTime2: TDateTime;
begin
  IsESM1 := IsFileESM(List[Index1]);
  IsESM2 := IsFileESM(List[Index2]);

  if IsESM1 = IsESM2 then begin
    FileAge1 := Integer(List.Objects[Index1]);
    FileAge2 := Integer(List.Objects[Index2]);

    if FileAge1 < FileAge2 then
      Result := -1
    else if FileAge1 > FileAge2 then
      Result := 1
    else begin
      if not SameText(List[Index1], List[Index1])
      and FileAge(List[Index1], FileDateTime1) and FileAge(List[Index2], FileDateTime2) then begin
        if FileDateTime1 < FileDateTime2 then
          Result := -1
        else if FileDateTime1 > FileDateTime2 then
          Result := 1
        else
          Result := 0;
      end else
        Result := 0;
    end;

  end else if IsESM1 then
    Result := -1
  else
    Result := 1;
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

  // don't attempt to load dictionary if it doesn't exist
  if not FileExists(dictionaryFilename) then begin
    Logger.Write('No dictionary file '+dictionaryFilename);
    exit;
  end;

  // load dictionary file
  sl := TStringList.Create;
  sl.LoadFromFile(dictionaryFilename);

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
  - SaveMerges
  - LoadMerges
}
{******************************************************************************}

{ Gets a plugin matching the given name. }
function PluginByFilename(filename: string): TPlugin;
var
  i: integer;
  plugin: TPlugin;
begin
  Result := nil;
  for i := 0 to Pred(PluginsList.count) do begin
    plugin := TPlugin(PluginsList[i]);
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
function CreateNewPlugin(filename: string): TPlugin;
var
  aFile: IwbFile;
  LoadOrder: integer;
  plugin: TPlugin;
begin
  Result := nil;
  LoadOrder := PluginsList.Count + 1;
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
  plugin._File := aFile;
  PluginsList.Add(plugin);

  Result := plugin;
end;

procedure SaveMerges;
var
  i: Integer;
  merge: TMerge;
  json: ISuperObject;
begin
  // initialize json
  json := SO;
  json.O['merges'] := SA([]);

  // loop through merges
  for i := 0 to Pred(MergesList.Count) do begin
    merge := MergesList[i];
    json.A['merges'].Add(merge.Dump);
  end;

  // save and finalize
  json.SaveTo(wbAppName + 'Merges.json');
  json := nil;
end;

procedure LoadMerges;
const
  debug = false;
var
  merge: TMerge;
  plugin: TPlugin;
  obj, mergeItem: ISuperObject;
  sl: TStringList;
  i: Integer;
begin
  // don't load file if it doesn't exist
  if not FileExists(wbAppName + 'Merges.json') then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(wbAppName + 'Merges.json');
  obj := SO(PChar(sl.Text));

  // loop through merges
  for mergeItem in obj['merges'] do begin
    merge := TMerge.Create;
    merge.LoadDump(mergeItem);
    MergesList.Add(merge);
    for i := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[i]);
      if Assigned(plugin) then
        plugin.merge := merge.name;
    end;
  end;

  // finalize
  obj := nil;
  sl.Free;
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
  - TMerge.PluginsModified
  - TMerge.FilesExist
  - TMerge.GetStatus
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
end;

{ Fetches data associated with a plugin. }
procedure TPlugin.GetData;
var
  Container: IwbContainer;
  s: string;
begin
  hasData := true;
  // get data
  filename := _File.FileName;
  Container := _File as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  author := Container.GetElementEditValue('CNAM - Author');
  numRecords := Container.GetElementEditValue('HEDR - Header\Number of Records');

  // get masters, flags
  GetMasters(_File, masters);
  GetFlags;

  // get hash, datapath
  GetHash;
  GetDataPath;

  // get description
  s := Container.GetElementEditValue('SNAM - Description');
  description.Text := Wordwrap(s, 70);

  // get reports
  entry := GetEntry(filename, numRecords, ProgramVersion);
  s := Trim(StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]));
  reports.Text := Wordwrap(s, 70);

  // get file attributes
  fileSize := GetFileSize(wbDataPath + filename);
  dateModified := DateTimeToStr(GetLastModified(wbDataPath + filename));

  // get numOverrides if not blacklisted
  if not (IS_BLACKLISTED in flags) then
    numOverrides := IntToStr(CountOverrides(_File));
end;

procedure TPlugin.GetHash;
begin
  hash := IntToHex(wbCRC32File(wbDataPath + filename), 8);
end;

procedure TPlugin.GetDataPath;
begin
  dataPath := wbDataPath;
end;

{ Checks for errors in a plugin }
procedure TPlugin.FindErrors;
begin
  errors.Clear;
  CheckForErrors(2, _File as IwbElement, errors);
  if errors.Count = 0 then
    errors.Add('None.');
end;

{ TMerge Constructor }
constructor TMerge.Create;
begin
  name := 'NewMerge';
  filename := 'NewMerge.esp';
  status := 0;
  dateBuilt := 0;
  plugins := TStringList.Create;
  hashes := TStringList.Create;
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
  obj.O['pluginHashes'] := SA([]);
  for i := 0 to Pred(hashes.Count) do
    obj.A['pluginHashes'].S[i] := hashes[i];
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
  for item in obj['pluginHashes'] do
    hashes.Add(item.AsString);
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
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if not plugin.HasData then
        plugin.GetData;
      Inc(Result, 2*StrToInt(plugin.numRecords));
      if not plugin.errors.Count > 0 then
        Inc(Result, StrToInt(plugin.numRecords));
    end;
  end;
end;

{ Checks to see if the plugins in a merge have been modified since it was last
  merged. }
function TMerge.PluginsModified: boolean;
var
  plugin: TPlugin;
  i: integer;
begin
  Result := false;
  for i := 0 to Pred(plugins.count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if plugin.hash <> hashes[i] then begin
        Logger.Write(plugin.filename + ' has hash ' + plugin.dateModified +
          ', '+ name + ' has '+hashes[i]);
        Result := true;
      end;
    end;
  end;
end;

{ Checks if the files associated with a merge exist }
function TMerge.FilesExist: boolean;
begin
  Result := FileExists(dataPath + filename);
end;

procedure TMerge.GetStatus;
var
  i: Integer;
  plugin: TPlugin;
begin
  Logger.Write('Getting status for '+name);
  status := 0;

  // don't merge if no plugins to merge
  if plugins.Count < 1 then begin
    Logger.Write('  No plugins to merge');
    status := 1;
    exit;
  end;

  // don't merge if mod destination directory is blank
  if settings.mergeDirectory = '' then begin
    Logger.Write('  Merge directory blank');
    status := 2;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is blank
  if settings.usingMO and (settings.MODirectory = '') then begin
    Logger.Write('  Mod Organizer Directory blank');
    status := 2;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is invalid
  if settings.usingMO and not DirectoryExists(settings.MODirectory) then begin
     Logger.Write('  Mod Organizer Directory invalid');
     status := 2;
     exit;
  end;

  // loop through plugins
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      Logger.Write('  Plugin '+plugins[i]+' is missing');
      status := 3;
      exit;
    end;

    if plugin.errors.Count = 0 then
      Logger.Write('  '+plugin.filename+' needs to be checked for errors.')
    else if plugin.errors[0] = 'None.' then
      Logger.Write('  No errors in '+plugin.filename)
    else begin
      Logger.Write('  '+plugin.filename+' has errors');
      status := 4;
      exit;
    end
  end;

  dataPath := settings.mergeDirectory + name + '\';
  if (not PluginsModified) and FilesExist then begin
    Logger.Write('  Up to date.');
    status := 5;
    exit;
  end;

  // status green, ready to go
  if status = 0 then begin
    Logger.Write('  Ready to be merged.');
    if dateBuilt = 0 then
      status := 7
    else
      status := 8;
  end;
  Logger.Write(' ');
end;

function TMerge.GetStatusColor: integer;
begin
  Result := StatusArray[status].color;
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
  appMerging: string;
begin
  ini := TMemIniFile.Create(filename);
  appMerging := wbAppName + 'Merging';

  // save general settings
  ini.WriteString('General', 'Language', language);
  ini.WriteInteger('General', 'defaultGame', defaultGame);
  ini.WriteString('General', 'tes5path', tes5path);
  ini.WriteString('General', 'fnvpath', fnvpath);
  ini.WriteString('General', 'tes4path', tes4path);
  ini.WriteString('General', 'fo3path', fo3path);
  ini.WriteString('General', 'username', username);
  ini.WriteBool('General', 'saveReportsLocally', saveReportsLocally);
  ini.WriteBool('General', 'simpleDictionaryView', simpleDictionaryView);
  ini.WriteBool('General', 'simplePluginsView', simplePluginsView);
  ini.WriteBool('General', 'updateDictionary', updateDictionary);
  ini.WriteBool('General', 'updateProgram', updateProgram);

  // save game specific settings
  if wbAppName <> '' then begin
    ini.WriteBool(appMerging, 'usingMO', usingMO);
    ini.WriteString(appMerging, 'MODirectory', MODirectory);
    ini.WriteBool(appMerging, 'copyGeneralAssets', copyGeneralAssets);
    ini.WriteString(appMerging, 'mergeDirectory', mergeDirectory);
    ini.WriteBool(appMerging, 'handleFaceGenData', handleFaceGenData);
    ini.WriteBool(appMerging, 'handleVoiceAssets', handleVoiceAssets);
    ini.WriteBool(appMerging, 'handleMCMTranslations', handleMCMTranslations);
    ini.WriteBool(appMerging, 'handleScriptFragments', handleScriptFragments);
    ini.WriteBool(appMerging, 'extractBSAs', extractBSAs);
    ini.WriteBool(appMerging, 'buildMergedBSA', buildMergedBSA);
  end;

  // save file
  ini.UpdateFile;
  ini.Free;
end;

procedure TSettings.Load(const filename: string);
var
  ini: TMemIniFile;
  appMerging, defaultMergeDirectory: string;
begin
  ini := TMemIniFile.Create(filename);
  appMerging := wbAppName + 'Merging';
  defaultMergeDirectory := ExtractFilePath(ParamStr(0)) + wbGameName + '\';

  // load general settings
  language := ini.ReadString('General', 'Language', 'English');
  defaultGame := ini.ReadInteger('General', 'defaultGame', 0);
  tes5path := ini.ReadString('General', 'tes5path', '');
  fnvpath := ini.ReadString('General', 'fnvpath', '');
  tes4path := ini.ReadString('General', 'tes4path', '');
  fo3path := ini.ReadString('General', 'fo3path', '');
  username := ini.ReadString('General', 'username', '');
  saveReportsLocally := ini.ReadBool('General', 'saveReportsLocally', false);
  simpleDictionaryView := ini.ReadBool('General', 'simpleDictionaryView', false);
  simplePluginsView := ini.ReadBool('General', 'simplePluginsView', false);
  updateDictionary := ini.ReadBool('General', 'updateDictionary', false);
  updateProgram := ini.ReadBool('General', 'updateProgram', false);

  // load game specific settings
  if wbAppName <> '' then begin
    usingMO := ini.ReadBool(appMerging, 'usingMO', false);
    MODirectory := ini.ReadString(appMerging, 'MODirectory', '');
    copyGeneralAssets := ini.ReadBool(appMerging, 'copyGeneralAssets', false);
    mergeDirectory := ini.ReadString(appMerging, 'mergeDirectory', defaultMergeDirectory);
    handleFaceGenData := ini.ReadBool(appMerging, 'handleFaceGenData', false);
    handleVoiceAssets := ini.ReadBool(appMerging, 'handleVoiceAssets', false);
    handleMCMTranslations := ini.ReadBool(appMerging, 'handleMCMTranslations', false);
    handleScriptFragments := ini.ReadBool(appMerging, 'handleScriptFragments', false);
    extractBSAs := ini.ReadBool(appMerging, 'extractBSAs', false);
    buildMergedBSA := ini.ReadBool(appMerging, 'buildMergedBSA', false);
  end;

  // save file
  ini.UpdateFile;
  ini.Free;
end;

end.
