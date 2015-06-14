unit mpBase;

interface

uses
  Windows, SysUtils, ShlObj, ShellApi, Classes, IniFiles, Dialogs, Masks,
  Controls, Registry,
  superobject,
  mpLogger, mpTracker,
  wbBSA,
  wbHelpers,
  wbInterface,
  wbImplementation,
  wbDefinitionsFNV, wbDefinitionsFO3, wbDefinitionsTES3, wbDefinitionsTES4,
  wbDefinitionsTES5;

type
  TCallback = procedure of object;
  TLoaderThread = class(TThread)
  protected
    procedure Execute; override;
  end;
  TGameMode = Record
    longName: string;
    gameName: string;
    gameMode: TwbGameMode;
    appName: string;
    exeName: string;
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
  TMergeStatus = Record
    id: integer;
    color: integer;
    desc: string[64];
  end;
  TPluginFlag = Record
    id: integer;
    char: char;
    desc: string[128];
  end;
  TPluginFlagID = (IS_BLACKLISTED, HAS_ERRORS, NO_ERRORS, HAS_BSA,
    HAS_FACEDATA, HAS_VOICEDATA, HAS_TRANSLATION, HAS_INI, HAS_FRAGMENTS );
  TPluginFlags = set of TPluginFlagID;
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
    function GetFlagsDescription: string;
    procedure GetDataPath;
    procedure FindErrors;
    procedure LoadErrorDump(obj: ISuperObject);
    function ErrorDump: ISuperObject;
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
    procedure UpdateHashes;
    procedure GetStatus;
    procedure GetLoadOrders;
    procedure SortPlugins;
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
    handleINIs: boolean;
    handleScriptFragments: boolean;
    extractBSAs: boolean;
    buildMergedBSA: boolean;
    batCopy: boolean;
    debugRenumbering: boolean;
    debugMergeStatus: boolean;
    debugAssetCopying: boolean;
    debugRecordCopying: boolean;
    debugMasters: boolean;
    debugBatchCopying: boolean;
    debugBSAs: boolean;
    debugTempPath: boolean;
    debugLoadOrder: boolean;
    constructor Create; virtual;
    procedure Save(const filename: string);
    procedure Load(const filename: string);
  end;
  TStatistics = class(TObject)
  public
    timesRun: integer;
    mergesBuilt: integer;
    pluginsChecked: integer;
    pluginsMerged: integer;
    reportsSubmitted: integer;
    constructor Create; virtual;
    procedure Save(const filename: string);
    procedure Load(const filename: string);
  end;

  { Initialization Methods }
  function GamePathValid(path: string; id: integer): boolean;
  procedure SetGame(id: integer);
  function GetGameID(name: string): integer;
  function GetGamePath(gameName: string): string;
  procedure LoadDefinitions;
  function GetVersionMem: string;
  { Bethesda Plugin Functions }
  function IsOverride(aRecord: IwbMainRecord): boolean;
  function LocalFormID(aRecord: IwbMainRecord): integer;
  function LoadOrderPrefix(aRecord: IwbMainRecord): integer;
  function CountOverrides(aFile: IwbFile): integer;
  procedure GetMasters(aFile: IwbFile; var sl: TStringList);
  procedure AddMasters(aFile: IwbFile; var sl: TStringList);
  function BSAExists(filename: string): boolean;
  function INIExists(filename: string): boolean;
  function TranslationExists(filename: string): boolean;
  function FaceDataExists(filename: string): boolean;
  function VoiceDataExists(filename: string): boolean;
  function FragmentsExist(f: IwbFile): boolean;
  procedure ExtractBSA(ContainerName, folder, destination: string);
  function CheckForErrorsLinear(const aElement: IwbElement; LastRecord: IwbMainRecord; var errors: TStringList): IwbMainRecord;
  function CheckForErrors(const aIndent: Integer; const aElement: IwbElement; var errors: TStringList): Boolean;
  procedure CreateSEQFile(merge: TMerge);
  { General functions }
  function csvText(s: string): string;
  function FormatByteSize(const bytes: Int64): string;
  function DateBuiltString(date: TDateTime): string;
  function AppendIfMissing(str, substr: string): string;
  function StrEndsWith(s1, s2: string): boolean;
  function RemoveFromEnd(s1, s2: string): string;
  function IntegerListSum(list: TList; maxIndex: integer): integer;
  function Wordwrap(var s: string; charCount: integer): string;
  function ContainsMatch(var sl: TStringList; const s: string): boolean;
  function IsURL(s: string): boolean;
  { Load order functions }
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
  { Windows API functions }
  function GetCSIDLShellFolder(CSIDLFolder: integer): string;
  function GetFileSize(const aFilename: String): Int64;
  function GetLastModified(const aFileName: String): TDateTime;
  function RecursiveFileSearch(aPath, aFileName: string; ignore: TStringList; maxDepth: integer): string;
  procedure CopyDirectory(src, dst: string; fIgnore, dIgnore: TStringList);
  procedure GetFilesList(path: string; var fIgnore, dIgnore, list: TStringList);
  procedure CopyFiles(src, dst: string; var list: TStringList);
  { Mod Organizer methods }
  procedure ModOrganizerInit;
  function GetActiveProfile: string;
  procedure GetActiveMods(var modlist: TStringList; profileName: string);
  function GetModContainingFile(var modlist: TStringList; filename: string): string;
  { Dictionary and Settings methods }
  procedure LoadSettings;
  procedure LoadStatistics;
  procedure LoadDictionary;
  function GetRatingColor(rating: real): integer;
  function GetEntry(pluginName, numRecords, version: string): TEntry;
  function IsBlacklisted(const filename: string): boolean;
  { Plugin and Merge methods }
  function PluginLoadOrder(filename: string): integer;
  function PluginByFilename(filename: string): TPlugin;
  function MergeByName(merges: TList; name: string): TMerge;
  function MergeByFilename(merges: TList; filename: string): TMerge;
  function CreateNewMerge(merges: TList): TMerge;
  function CreateNewPlugin(filename: string): TPlugin;
  procedure SaveMerges;
  procedure LoadMerges;
  procedure SavePluginErorrs;
  procedure LoadPluginErrors;
  function MergePluginsCompare(List: TStringList; Index1, Index2: Integer): Integer;

const
  // IMPORTANT CONSTANTS
  ProgramVersion = '2.0';
  ProgramTesters = 'bla08, hishy, keithinhanoi, mindw0rk2, steve25469, Teabag, '+
    'Thalioden, zilav';
  ProgramTranslators = 'dhxxqk2010, Oaristys, Ganda, Martinezer, EHPDJFrANKy';
  xEditVersion = '3.1.1';

  // PLUGIN FLAGS
  FlagsArray: array[0..8] of TPluginFlag = (
    ( id: 0; char: 'X'; desc: 'Is blacklisted'; ),
    ( id: 1; char: 'E'; desc: 'Has errors'; ),
    ( id: 2; char: 'N'; desc: 'Has no errors'; ),
    ( id: 3; char: 'A'; desc: 'Has a BSA file'; ),
    ( id: 4; char: 'G'; desc: 'Has FaceGenData'; ),
    ( id: 5; char: 'V'; desc: 'Has Voice Data'; ),
    ( id: 6; char: 'T'; desc: 'Has MCM Translations'; ),
    ( id: 7; char: 'I'; desc: 'Has an INI file'; ),
    ( id: 8; char: 'F'; desc: 'Has Script fragments'; )
  );

  // MERGE STATUSES
  StatusArray: array[0..11] of TMergeStatus = (
    ( id: 0; color: $808080; desc: 'Unknown'; ),
    ( id: 1; color: $0000FF; desc: 'No plugins to merge'; ),
    ( id: 2; color: $0000FF; desc: 'Directories invalid'; ),
    ( id: 3; color: $0000FF; desc: 'Plugins not loaded'; ),
    ( id: 4; color: $0000FF; desc: 'Errors in plugins'; ),
    ( id: 5; color: $900000; desc: 'Up to date'; ),
    ( id: 6; color: $900000; desc: 'Up to date [Forced]'; ),
    ( id: 7; color: $009000; desc: 'Ready to be built'; ),
    ( id: 8; color: $009000; desc: 'Ready to be rebuilt'; ),
    ( id: 9; color: $009000; desc: 'Ready to be rebuilt [Forced]'; ),
    ( id: 10; color: $0080ed; desc: 'Check for errors required'; ),
    ( id: 11; color: $0000FF; desc: 'Merge failed'; )
  );
  // STATUS TYPES
  UpToDateStatuses = [5, 6];
  BuildStatuses = [7, 8, 9];
  RebuildStatuses = [8, 9];
  ForcedStatuses = [6, 9];

  // GAME MODES
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

var
  dictionary, blacklist, PluginsList, MergesList: TList;
  settings: TSettings;
  statistics: TStatistics;
  handler: IwbContainerHandler;
  bDontSave, bChangeGameMode, bForceTerminate, bLoaderDone,
  bProgressCancel: boolean;
  tempPath, logPath, dictionaryFilename, ActiveProfile: string;
  batch, ActiveMods: TStringList;
  LoaderCallback: TCallback;

implementation

{******************************************************************************}
{ Initialization Methods
  Methods that are used for initialization.

  List of methods:
  - GamePathValid
  - SetGame
  - GetGameID
  - GetGamePath
  - LoadDataPath
  - LoadDefinitions
}
{******************************************************************************}

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
{ Bethesda Plugin Functions
  Set of functions that read bethesda plugin files for various attributes.

  List of functions:
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
  filename: string;
begin
  Container := aFile as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  if Container.ElementExists['Master Files'] then begin
    MasterFiles := Container.ElementByPath['Master Files'] as IwbContainer;
    for i := 0 to MasterFiles.ElementCount - 1 do begin
      MasterFile := MasterFiles.Elements[i] as IwbContainer;
      filename := MasterFile.GetElementEditValue('MAST - Filename');
      if sl.IndexOf(filename) = -1 then
        sl.AddObject(filename, TObject(PluginLoadOrder(filename)));
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

{ Check if an INI exists associated with the given filename }
function INIExists(filename: string): boolean;
var
  iniFilename: string;
begin
  iniFilename := ChangeFileExt(filename, '.ini');
  Result := FileExists(wbDataPath + iniFilename);
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

{ Returns true if Topic Info Fragments exist in @f }
function TopicInfoFragmentsExist(f: IwbFile): boolean;
const
  infoFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Info VMAD\Script Fragments Info';
var
  group: IwbGroupRecord;
  rec, subgroup, container: IwbContainer;
  element, fragments: IwbElement;
  i, j: Integer;
  fn: string;
begin
  Result := false;
  // exit if no DIAL records in file
  if not f.HasGroup('DIAL') then
    exit;

  // find all DIAL records
  group := f.GroupBySignature['DIAL'];
  for i := 0 to Pred(group.ElementCount) do begin
    element := group.Elements[i];
    // find all INFO records
    if not Supports(element, IwbContainer, subgroup) then
      continue;
    for j := 0 to Pred(subgroup.ElementCount) do begin
      rec := subgroup.Elements[j] as IwbContainer;
      fragments := rec.ElementByPath[infoFragmentsPath];
      if not Assigned(fragments) then
        continue;
      if not Supports(fragments, IwbContainer, container) then
        continue;
      fn := container.ElementValues['fileName'];
      Result := Result or (Pos('TIF_', fn) = 1);
    end;
  end;
end;

{ Returns true if Quest Fragments exist in @f }
function QuestFragmentsExist(f: IwbFile): boolean;
const
  questFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  group: IwbGroupRecord;
  rec, container: IwbContainer;
  fragments: IwbElement;
  i: Integer;
  fn: string;
begin
  Result := false;
  // exit if no QUST records in file
  if not f.HasGroup('QUST') then
    exit;

  // find all QUST records
  group := f.GroupBySignature['QUST'];
  for i := 0 to Pred(group.ElementCount) do begin
    rec := group.Elements[i] as IwbContainer;
    fragments := rec.ElementByPath[questFragmentsPath];
    if not Assigned(fragments) then
      continue;
    if not Supports(fragments, IwbContainer, container) then
      continue;
    fn := container.ElementValues['fileName'];
    Result := Result or (Pos('QF_', fn) = 1);
  end;
end;

{ Returns true if Quest Fragments exist in @f }
function SceneFragmentsExist(f: IwbFile): boolean;
const
  sceneFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  group: IwbGroupRecord;
  rec, container: IwbContainer;
  fragments: IwbElement;
  i: Integer;
  fn: string;
begin
  Result := false;
  // exit if no SCEN records in file
  if not f.HasGroup('SCEN') then
    exit;

  // find all SCEN records
  group := f.GroupBySignature['SCEN'];
  for i := 0 to Pred(group.ElementCount) do begin
    rec := group.Elements[i] as IwbContainer;
    fragments := rec.ElementByPath[sceneFragmentsPath];
    if not Assigned(fragments) then
      continue;
    if not Supports(fragments, IwbContainer, container) then
      continue;
    fn := container.ElementValues['fileName'];
    Result := Result or (Pos('SF_', fn) = 1);
  end;
end;

{ Returns true if file-specific Script Fragments for @f are found }
function FragmentsExist(f: IwbFile): boolean;
begin
  Result := TopicInfoFragmentsExist(f) or QuestFragmentsExist(f)
    or SceneFragmentsExist(f);
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
  if bProgressCancel then exit;
  if Supports(aElement, IwbMainRecord) then
    Tracker.Update(1);

  Error := aElement.Check;
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    // first error in this record - show record's name
    if Assigned(Result) and (Result <> LastRecord) then begin
      Tracker.Write(Result.Name);
      errors.Add(Result.Name);
    end;
    Tracker.Write('    ' + aElement.Path + ' -> ' + Error);
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
  if bProgressCancel then begin
    Result := false;
    exit;
  end;

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
    msg := StringOfChar(' ', aIndent * 2) + 'Above errors were found in: ' + aElement.Name;
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

{ TLoaderThread }
procedure LoaderProgress(const s: string);
begin
  if s <> '' then
    Logger.Write('[' + FormatDateTime('nn:ss', Now - wbStartTime) + '] Background Loader: ' + s);
  if bForceTerminate then
    Abort;
end;

procedure TLoaderThread.Execute;
var
  i: Integer;
  f: IwbFile;
  plugin: TPlugin;
begin
  wbStartTime := Now;
  try
    for i := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[i]);
      f := plugin._File;
      if SameText(plugin.filename, wbGameName + '.esm') then
        continue;
      LoaderProgress('[' + plugin.filename + '] Building reference info.');
      f.BuildRef;
      if bForceTerminate then begin
        LoaderProgress('Aborted.');
        exit;
      end;
    end;
  except
    on E: Exception do begin
      LoaderProgress('Fatal: <' + e.ClassName + ': ' + e.Message + '>');
      wbLoaderError := true;
      bDontSave := true;
    end;
  end;
  bLoaderDone := true;
  LoaderProgress('finished');
  if Assigned(LoaderCallback) then
    LoaderCallback;
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
{ Mod Organizer methods
  Set of methods that allow interaction Mod Organizer settings.

  List of methods:
  - ModOrganizerInit
  - GetActiveProfile
  - GetActiveMods
  - GetModContainingFile
}
{******************************************************************************}

procedure ModOrganizerInit;
begin
  ActiveMods := TStringList.Create;
  ActiveProfile := GetActiveProfile;
  GetActiveMods(ActiveMods, ActiveProfile);
  //Logger.Write('ActiveMods: '#13#10+ActiveMods.Text);
end;

function GetActiveProfile: string;
var
  ini : TMemIniFile;
  fname : string;
begin
  // exit if not using MO
  Result := '';
  if not settings.usingMO then
    exit;

  // load ini file
  fname := settings.MODirectory + 'ModOrganizer.ini';
  if(not FileExists(fname)) then begin
    Logger.Write('Mod Organizer ini file ' + fname + ' does not exist');
    exit;
  end;
  ini := TMemIniFile.Create(fname);

  // get selected_profile
  Result := ini.ReadString( 'General', 'selected_profile', '');
  ini.Free;
end;

procedure GetActiveMods(var modlist: TStringList; profileName: string);
var
  modlistFilePath: string;
  s: string;
  i: integer;
begin
  // exit if not using MO
  if not settings.usingMO then
    exit;

  // prepare to load modlist
  modlistFilePath := settings.MODirectory + 'profiles/' + profileName + '/modlist.txt';
  modlist.Clear;

  // exit if modlist file doesn't exist
  if not (FileExists(modlistFilePath)) then begin
    Tracker.Write('Cannot find modlist ' + modListFilePath);
    exit;
  end;

  // load modlist
  modlist.LoadFromFile(modlistFilePath);
  for i := Pred(modlist.Count) downto 0 do begin
    s := modList[i];
    // if line starts with '+', then it's an active mod
    if (Pos('+', s) = 1) then
      modlist[i] := Copy(s, 2, Length(s) - 1)
    // else it's a comment or inactive mod, so delete it
    else
      modlist.Delete(i);
  end;
end;

function GetModContainingFile(var modlist: TStringList; filename: string): string;
var
  i: integer;
  modName: string;
  filePath: string;
begin
  // exit if not using MO
  Result := '';
  if not settings.usingMO then
    exit;

  // check for file in each mod folder in modlist
  for i := 0 to Pred(modlist.Count) do begin
    modName := modlist[i];
    filePath := settings.MODirectory + 'mods\' + modName + '\' + filename;
    if (FileExists(filePath)) then begin
      Result := modName;
      exit;
    end;
  end;
end;


{******************************************************************************}
{ Dictionary and Settings methods
  Set of methods for managing the dictionary.

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

procedure LoadStatistics;
begin
  statistics := TStatistics.Create;
  statistics.Load('statistics.ini');
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
    if entry.filename = pluginName then begin
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
    if entry.filename = filename then begin
      Result := true;
      break;
    end;
  end;
end;

{******************************************************************************}
{ Plugin and Merge methods
  Methods for dealing with plugins and merges.

  List of methods:
  - PluginLoadOrder
  - PluginByFilename
  - MergeByName
  - MergeByFilename
  - CreateNewMerge
  - CreateNewPlugin
  - SaveMerges
  - LoadMerges
  - SavePluginErrors
  - LoadPluginErrors
  - MergePluginsCompare
}
{******************************************************************************}

{ Gets the load order of the plugin matching the given name }
function PluginLoadOrder(filename: string): integer;
var
  i: integer;
  plugin: TPlugin;
begin
  Result := -1;
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    if plugin.filename = filename then begin
      Result := i;
      exit;
    end;
  end;
end;

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
  Tracker.Write('Dumping merges to JSON');
  for i := 0 to Pred(MergesList.Count) do begin
    Tracker.Update(1);
    merge := TMerge(MergesList[i]);
    Tracker.Write('  Dumping '+merge.name);
    json.A['merges'].Add(merge.Dump);
  end;

  // save and finalize
  Tracker.Write(' ');
  Tracker.Write('Saving to '+wbAppName+'Merges.json');
  Tracker.Update(1);
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

procedure SavePluginErorrs;
var
  i: Integer;
  plugin: TPlugin;
  json: ISuperObject;
begin
  // initialize json
  json := SO;
  json.O['plugins'] := SA([]);

  // loop through plugins
  Tracker.Write('Dumping plugin errors to JSON');
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := PluginsList[i];
    Tracker.Update(1);
    if (plugin.errors.Count = 0) then
      continue;
    Tracker.Write('  Dumping '+plugin.filename);
    json.A['plugins'].Add(plugin.ErrorDump);
  end;

  // save and finalize
  Tracker.Write(' ');
  Tracker.Write('Saving to '+wbAppName+'Errors.json');
  Tracker.Update(1);
  json.SaveTo(wbAppName + 'Errors.json');
  json := nil;
end;

procedure LoadPluginErrors;
var
  plugin: TPlugin;
  obj, pluginItem: ISuperObject;
  sl: TStringList;
  fn, hash: string;
begin
  // don't load file if it doesn't exist
  if not FileExists(wbAppName + 'Errors.json') then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(wbAppName + 'Errors.json');
  obj := SO(PChar(sl.Text));

  // loop through merges
  for pluginItem in obj['plugins'] do begin
    fn := pluginItem.AsObject.S['filename'];
    hash := pluginItem.AsObject.S['hash'];
    plugin := PluginByFileName(fn);
    if not Assigned(plugin) then
      exit;
    if plugin.hash = hash then begin
      plugin.LoadErrorDump(pluginItem);
      plugin.GetFlags;
    end;
  end;

  // finalize
  obj := nil;
  sl.Free;
end;

{ Comparator for sorting plugins in merge }
function MergePluginsCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  LO1, LO2: Integer;
begin
  LO1 := Integer(List.Objects[Index1]);
  LO2 := Integer(List.Objects[Index2]);
  Result := LO1 - LO2;
end;


{******************************************************************************}
{ Object methods
  Set of methods for objects TMerge and TPlugin

  List of methods:
  - TPlugin.Create
  - TPlugin.GetFlags
  - TPlugin.GetFlagsString
  - TPlugin.GetData
  - TPlugin.GetHash
  - TPlugin.GetDataPath
  - TPlugin.FindErrors
  - TMerge.Create
  - TMerge.Dump
  - TMerge.LoadDump
  - TMerge.GetTimeCost
  - TMerge.PluginsModified
  - TMerge.FilesExist
  - TMerge.GetStatus
  - TMerge.GetHashes
  - TMerge.GetLoadOrders
  - TMerge.SortPlugins
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
  flags := [];
  if IsBlacklisted(filename) then begin
    flags := flags + [IS_BLACKLISTED];
    exit;
  end;
  if (errors.Count > 0) then begin
    if (errors[0] = 'None.') then
      flags := flags + [NO_ERRORS]
    else
      flags := flags + [HAS_ERRORS];
  end;
  if BSAExists(filename) then
    flags := flags + [HAS_BSA];
  if INIExists(filename) then
    flags := flags + [HAS_INI];
  if TranslationExists(filename) then
    flags := flags + [HAS_TRANSLATION];
  if FaceDataExists(filename) then
    flags := flags + [HAS_FACEDATA];
  if VoiceDataExists(filename) then
    flags := flags + [HAS_VOICEDATA];
  if FragmentsExist(_File) then
    flags := flags + [HAS_FRAGMENTS];
end;

{ Returns a string representing the flags in a plugin }
function TPlugin.GetFlagsString: string;
var
  id: TPluginFlagID;
begin
  Result := '';
  for id in flags do
     Result := Result + FlagsArray[Ord(id)].char;
end;

{ Get a detailed flags description from a flags string @flags }
function TPlugin.GetFlagsDescription: string;
var
  flag: TPluginFlag;
  id: TPluginFlagId;
begin
  Result := '';
  // result is empty string if flags empty
  if flags = [] then exit;

  // get flags
  for id in flags do begin
    flag := FlagsArray[Ord(id)];
    Result := Result+'['+flag.char+'] '+flag.desc+#13#10;
  end;
  Result := Trim(Result);
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
var
  modName: string;
begin
  dataPath := wbDataPath;
  if settings.usingMO then begin
    modName := GetModContainingFile(ActiveMods, filename);
    if modName <> '' then
      dataPath := settings.MODirectory + 'mods\' + modName + '\';
  end;
end;

{ Checks for errors in a plugin }
procedure TPlugin.FindErrors;
begin
  // clear errors, then check
  errors.Clear;
  CheckForErrors(2, _File as IwbElement, errors);
  //CheckForErrorsLinear(_File as IwbElement, _File.Records[_File.RecordCount - 1], errors);

  if bProgressCancel then
    exit;
  if (errors.Count = 0) then
    errors.Add('None.');

  // update flags, statistics
  GetFlags;
  Inc(statistics.pluginsChecked);
end;

function TPlugin.ErrorDump: ISuperObject;
var
  obj: ISuperObject;
  i: integer;
begin
  obj := SO;

  // filename, hash, errors
  obj.S['filename'] := filename;
  obj.S['hash'] := hash;
  obj.O['errors'] := SA([]);
  for i := 0 to Pred(errors.Count) do
    obj.A['errors'].S[i] := errors[i];

  Result := obj;
end;

procedure TPlugin.LoadErrorDump(obj: ISuperObject);
var
  item: ISuperObject;
begin
  errors.Clear;
  for item in obj['errors'] do
    errors.Add(item.AsString);
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
    if Assigned(plugin) then
      Inc(Result, plugin._File.RecordCount);
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
  // true if number of hashes not equal to number of plugins
  if plugins.Count <> hashes.Count then begin
    Result := true;
    exit;
  end;
  // true if any plugin hash doesn't match
  for i := 0 to Pred(plugins.count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if plugin.hash <> hashes[i] then begin
        if settings.debugMergeStatus then
          Logger.Write(plugin.filename + ' has hash ' + plugin.hash + ', '+
            name + ' has '+hashes[i]);
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
  if settings.debugMergeStatus then Logger.Write('Getting status for '+name);
  status := 0;

  // don't merge if no plugins to merge
  if plugins.Count < 1 then begin
    if settings.debugMergeStatus then Logger.Write('  No plugins to merge');
    status := 1;
    exit;
  end;

  // don't merge if mod destination directory is blank
  if settings.mergeDirectory = '' then begin
    if settings.debugMergeStatus then Logger.Write('  Merge directory blank');
    status := 2;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is blank
  if settings.usingMO and (settings.MODirectory = '') then begin
    if settings.debugMergeStatus then Logger.Write('  Mod Organizer Directory blank');
    status := 2;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is invalid
  if settings.usingMO and not DirectoryExists(settings.MODirectory) then begin
     if settings.debugMergeStatus then Logger.Write('  Mod Organizer Directory invalid');
     status := 2;
     exit;
  end;

  // loop through plugins
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      if settings.debugMergeStatus then Logger.Write('  Plugin '+plugins[i]+' is missing');
      status := 3;
      exit;
    end;

    if plugin.errors.Count = 0 then begin
      if settings.debugMergeStatus then Logger.Write('  '+plugin.filename+' needs to be checked for errors.');
      status := 10;
    end
    else if plugin.errors[0] = 'None.' then begin
      if settings.debugMergeStatus then Logger.Write('  No errors in '+plugin.filename);
    end
    else begin
      if settings.debugMergeStatus then Logger.Write('  '+plugin.filename+' has errors');
      status := 4;
      exit;
    end
  end;

  dataPath := settings.mergeDirectory + name + '\';
  if (not PluginsModified) and FilesExist then begin
    if settings.debugMergeStatus then Logger.Write('  Up to date.');
    status := 5;
    exit;
  end;

  // status green, ready to go
  if status = 0 then begin
    if settings.debugMergeStatus then Logger.Write('  Ready to be merged.');
    if dateBuilt = 0 then
      status := 7
    else
      status := 8;
  end;
  if settings.debugMergeStatus then Logger.Write(' ');
end;

function TMerge.GetStatusColor: integer;
begin
  Result := StatusArray[status].color;
end;

{ Update the hashes list for the plugins in the merge }
procedure TMerge.UpdateHashes;
var
  i: Integer;
  aPlugin: TPlugin;
begin
  hashes.Clear;
  for i := 0 to Pred(plugins.Count) do begin
    aPlugin := PluginByFilename(plugins[i]);
    if Assigned(aPlugin) then
      hashes.Add(aPlugin.hash);
  end;
end;

{ Get load order for plugins in merge that don't have it }
procedure TMerge.GetLoadOrders;
var
  i: integer;
begin
  for i := 0 to Pred(plugins.Count) do
    if not Assigned(plugins.Objects[i]) then
      plugins.Objects[i] := TObject(PluginLoadOrder(plugins[i]));
end;

{ Sort plugins by load order position }
procedure TMerge.SortPlugins;
begin
  GetLoadOrders;
  plugins.CustomSort(MergePluginsCompare);
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
    ini.WriteBool(appMerging, 'handleINIs', handleINIs);
    ini.WriteBool(appMerging, 'handleScriptFragments', handleScriptFragments);
    ini.WriteBool(appMerging, 'extractBSAs', extractBSAs);
    ini.WriteBool(appMerging, 'buildMergedBSA', buildMergedBSA);
    ini.WriteBool(appMerging, 'batCopy', batCopy);
  end;

  // save advanced settings
  ini.WriteBool('Advanced', 'debugRenumbering', debugRenumbering);
  ini.WriteBool('Advanced', 'debugMergeStatus', debugMergeStatus);
  ini.WriteBool('Advanced', 'debugAssetCopying', debugAssetCopying);
  ini.WriteBool('Advanced', 'debugRecordCopying', debugRecordCopying);
  ini.WriteBool('Advanced', 'debugMasters', debugMasters);
  ini.WriteBool('Advanced', 'debugBatchCopying', debugBatchCopying);
  ini.WriteBool('Advanced', 'debugBSAs', debugBSAs);
  ini.WriteBool('Advanced', 'debugTempPath', debugTempPath);
  ini.WriteBool('Advanced', 'debugLoadOrder', debugLoadOrder);

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
    handleINIs := ini.ReadBool(appMerging, 'handleINIs', false);
    handleScriptFragments := ini.ReadBool(appMerging, 'handleScriptFragments', false);
    extractBSAs := ini.ReadBool(appMerging, 'extractBSAs', false);
    buildMergedBSA := ini.ReadBool(appMerging, 'buildMergedBSA', false);
    batCopy := ini.ReadBool(appMerging, 'batCopy', true);
  end;

  // load advanced settings
  debugRenumbering := ini.ReadBool('Advanced', 'debugRenumbering', false);
  debugMergeStatus := ini.ReadBool('Advanced', 'debugMergeStatus', false);
  debugAssetCopying := ini.ReadBool('Advanced', 'debugAssetCopying', false);
  debugRecordCopying := ini.ReadBool('Advanced', 'debugRecordCopying', false);
  debugMasters := ini.ReadBool('Advanced', 'debugMasters', false);
  debugBatchCopying := ini.ReadBool('Advanced', 'debugBatchCopying', false);
  debugBSAs := ini.ReadBool('Advanced', 'debugBSAs', false);
  debugTempPath := ini.ReadBool('Advanced', 'debugTempPath', false);
  debugLoadOrder := ini.ReadBool('Advanced', 'debugLoadOrder', false);

  // save file
  ini.UpdateFile;
  ini.Free;
end;

{ TStatistics constructor }
constructor TStatistics.Create;
begin
  timesRun := 0;
  mergesBuilt := 0;
  pluginsChecked := 0;
  pluginsMerged := 0;
  reportsSubmitted := 0;
end;

procedure TStatistics.Save(const filename: string);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(filename);
  ini.WriteInteger('Statistics', 'timesRun', timesRun);
  ini.WriteInteger('Statistics', 'mergesBuilt', mergesBuilt);
  ini.WriteInteger('Statistics', 'pluginsChecked', pluginsChecked);
  ini.WriteInteger('Statistics', 'pluginsMerged', pluginsMerged);
  ini.WriteInteger('Statistics', 'reportsSubmitted', reportsSubmitted);

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
  mergesBuilt := ini.ReadInteger('Statistics', 'mergesBuilt', 0);
  pluginsChecked := ini.ReadInteger('Statistics', 'pluginsChecked', 0);
  pluginsMerged := ini.ReadInteger('Statistics', 'pluginsMerged', 0);
  reportsSubmitted := ini.ReadInteger('Statistics', 'reportsSubmitted', 0);

  // save file
  ini.UpdateFile;
  ini.Free;
end;



end.
