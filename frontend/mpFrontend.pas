unit mpFrontend;

interface

uses
  Windows, SysUtils, Classes, IniFiles, Dialogs, Registry, Graphics, ShlObj,
  ShellAPI,
  // indy components
  IdTCPClient, IdStack, IdGlobal,
  // superobject json library
  superobject,
  // abbrevia components
  AbZBrows, AbUnZper, AbArcTyp, AbMeter, AbBrowse, AbBase,
  // mte components
  CRC32, mteLogger, mteTracker, mteHelpers, RttiIni, RttiJson,
  RttiTranslation,
  // xedit components
  wbHelpers, wbInterface, wbImplementation,
  wbDefinitionsFNV, wbDefinitionsFO3, wbDefinitionsTES3, wbDefinitionsTES4,
  wbDefinitionsTES5;

type
  // LOGGING
  TFilter = class(TObject)
  public
    group: string;
    &label: string;
    enabled: boolean;
    constructor Create(group: string; enabled: boolean); Overload;
    constructor Create(group, &label: string; enabled: boolean); Overload;
  end;
  TLogMessage = class (TObject)
  public
    time: string;
    appTime: string;
    group: string;
    &label: string;
    text: string;
    constructor Create(time, appTime, group, &label, text: string); Overload;
  end;
  // SERVER/CLIENT
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
  // GENERAL
  TGameMode = Record
    longName: string;
    gameName: string;
    gameMode: TwbGameMode;
    appName: string;
    exeName: string;
    appIDs: string;
    bsaOptMode: string;
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
  TReport = class(TObject)
  public
    game: string;
    username: string;
    filename: string;
    hash: string;
    recordCount: integer;
    rating: integer;
    mergeVersion: string;
    notes: string;
    dateSubmitted: TDateTime;
    procedure SetNotes(notes: string);
    function GetNotes: string;
    procedure Save(const filename: string);
  end;
  // PLUGINS AND MERGES
  TMergeStatusID = ( msUnknown, msNoPlugins, msDirInvalid, msUnloaded,
    msErrors, msFailed, msCheckErrors, msUpToDate, msUpToDateForced,
    msBuildReady, msRebuildReady, msRebuildReadyForced );
  TMergeStatus = Record
    id: TMergeStatusID;
    color: integer;
    desc: string[64];
  end;
  TPluginFlagID = (IS_BLACKLISTED, HAS_ERRORS, ERRORS_IGNORED, NO_ERRORS,
    HAS_BSA, HAS_FACEDATA, HAS_VOICEDATA, HAS_TRANSLATION, HAS_INI,
    HAS_FRAGMENTS, DISALLOW_MERGING, REFERENCES_SELF );
  TPluginFlags = set of TPluginFlagID;
  TPluginFlag = Record
    id: TPluginFlagID;
    char: char;
    desc: string[128];
  end;
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
    bIgnoreErrors: boolean;
    bDisallowMerging: boolean;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure GetData;
    procedure UpdateData;
    procedure GetHash;
    procedure GetFlags;
    function GetFlagsString: string;
    function GetFlagsDescription: string;
    procedure GetDataPath;
    function GetFormIndex: Integer;
    procedure FindErrors;
    procedure ResolveErrors;
    function HasBeenCheckedForErrors: boolean;
    function HasErrors: boolean;
    function CanBeMerged: boolean;
    function IsInMerge: boolean;
    procedure LoadInfoDump(obj: ISuperObject);
    function InfoDump: ISuperObject;
  end;
  TMerge = class(TObject)
  public
    name: string;
    filename: string;
    dateBuilt: TDateTime;
    plugins: TStringList;
    hashes: TStringList;
    masters: TStringList;
    status: TMergeStatusID;
    method: string;
    renumbering: string;
    dataPath: string;
    plugin: TPlugin;
    map: TStringList;
    lmap: TStringList;
    files: TStringList;
    fails: TStringList;
    geckScripts: TStringList;
    navConflicts: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    function Dump: ISuperObject;
    procedure LoadDump(obj: ISuperObject);
    function GetTimeCost: integer;
    procedure UpdateHashes;
    procedure GetStatus;
    procedure GetLoadOrders;
    procedure SortPlugins;
    procedure Remove(plugin: TPlugin);
    function PluginsModified: boolean;
    function FilesExist: boolean;
    function GetStatusColor: integer;
  end;
  // SETTINGS AND STATISTICS
  TSettings = class(TObject)
  public
    [IniSection('General')]
    profile: string;
    gameMode: integer;
    gamePath: string;
    language: string;
    username: string;
    key: string;
    registered: boolean;
    simpleDictionaryView: boolean;
    simplePluginsView: boolean;
    updateDictionary: boolean;
    updateProgram: boolean;
    [IniSection('Advanced')]
    serverHost: string;
    serverPort: integer;
    dontSendStatistics: boolean;
    generalMessageColor: Int64;
    clientMessageColor: Int64;
    loadMessageColor: Int64;
    mergeMessageColor: Int64;
    pluginMessageColor: Int64;
    errorMessageColor: Int64;
    logMessageTemplate: string;
    preserveTempPath: boolean;
    [IniSection('Merging')]
    mergeDirectory: string;
    handleFaceGenData: boolean;
    handleVoiceAssets: boolean;
    handleMCMTranslations: boolean;
    handleINIs: boolean;
    handleSEQ: boolean;
    handleScriptFragments: boolean;
    handleSelfReference: boolean;
    extractBSAs: boolean;
    buildMergedBSA: boolean;
    batCopy: boolean;
    bForceOversizedBSA: boolean;
    bSkipOversizedBSA: boolean;
    debugRenumbering: boolean;
    debugMergeStatus: boolean;
    debugAssetCopying: boolean;
    debugRecordCopying: boolean;
    debugMasters: boolean;
    debugBatchCopying: boolean;
    debugBSAs: boolean;
    debugScriptFragments: boolean;
    [IniSection('Integrations')]
    usingMO: boolean;
    MOPath: string;
    MOModsPath: string;
    copyGeneralAssets: boolean;
    compilerPath: string;
    decompilerPath: string;
    flagsPath: string;
    bsaOptPath: string;
    bsaOptOptions: string;
    constructor Create; virtual;
    procedure GenerateKey;
  end;
  TStatistics = class(TObject)
  public
    [IniSection('Statistics')]
    timesRun: integer;
    mergesBuilt: integer;
    pluginsChecked: integer;
    pluginsFixed: integer;
    pluginsMerged: integer;
    reportsSubmitted: integer;
    constructor Create; virtual;
  end;
  TProfile = class(TObject)
  public
    name: string;
    gameMode: Integer;
    gamePath: string;
    constructor Create(name: string); virtual;
    procedure Delete;
    procedure Rename(name: string);
  end;

  { Initialization Methods }
  function GamePathValid(path: string; id: integer): boolean;
  procedure SetGame(id: integer);
  function GetGameID(name: string): integer;
  function GetGamePath(mode: TGameMode): string;
  procedure LoadDefinitions;
  { Bethesda Plugin Functions }
  function IsOverride(aRecord: IwbMainRecord): boolean;
  function ExtractFormID(filename: string): string;
  function RemoveFileIndex(formID: string): string;
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
  function ReferencesSelf(f: IwbFile): boolean;
  procedure ExtractBSA(ContainerName, folder, destination: string); overload;
  procedure ExtractBSA(ContainerName, destination: string; var ignore: TStringList); overload;
  function RemoveSelfOrContainer(const aElement: IwbElement): boolean;
  procedure UndeleteAndDisable(const aRecord: IwbMainRecord);
  function FixErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
    var errors: TStringList): IwbMainRecord;
  function CheckForErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
    var errors: TStringList): IwbMainRecord;
  procedure CreateSEQFile(merge: TMerge);
  { Load order functions }
  procedure RemoveCommentsAndEmpty(var sl: TStringList);
  procedure RemoveMissingFiles(var sl: TStringList);
  procedure RemoveMergedPlugins(var sl: TStringList);
  procedure AddMissingFiles(var sl: TStringList);
  procedure GetPluginDates(var sl: TStringList);
  function PluginListCompare(List: TStringList; Index1, Index2: Integer): Integer;
  { Mod Organizer methods }
  procedure ModOrganizerInit;
  function GetActiveProfile: string;
  procedure GetActiveMods(var modlist: TStringList; profileName: string);
  function GetModContainingFile(var modlist: TStringList; filename: string): string;
  { Log methods }
  procedure InitLog;
  procedure RebuildLog;
  procedure SaveLog(var Log: TList);
  function MessageEnabled(msg: TLogMessage): boolean;
  { Loading and saving methods }
  procedure LoadLanguage;
  function GetString(name: string): string;
  procedure SaveProfile(var p: TProfile);
  procedure LoadRegistrationData(var s: TSettings);
  procedure LoadSettings; overload;
  function LoadSettings(path: string): TSettings; overload;
  procedure SaveRegistrationData(var s: TSettings);
  procedure SaveSettings; overload;
  procedure SaveSettings(var s: TSettings; path: string); overload;
  procedure LoadStatistics;
  procedure SaveStatistics;
  procedure LoadChangelog;
  procedure LoadDictionary;
  procedure RenameSavedPlugins;
  procedure SaveMerges;
  procedure LoadMerges;
  procedure AssignMergesToPlugins;
  procedure SavePluginInfo;
  procedure LoadPluginInfo;
  procedure SaveReports(var lst: TList; path: string);
  procedure LoadReport(var report: TReport); overload
  procedure LoadReport(const filename: string; var report: TReport); overload;
  { Helper methods }
  procedure DeleteTempPath;
  function GetRatingColor(rating: real): integer;
  function GetEntry(pluginName, numRecords, version: string): TEntry;
  function IsBlacklisted(const filename: string): boolean;
  function PluginLoadOrder(filename: string): integer;
  function PluginByFilename(filename: string): TPlugin;
  function MergeByName(merges: TList; name: string): TMerge;
  function MergeByFilename(merges: TList; filename: string): TMerge;
  function CreateNewMerge(merges: TList): TMerge;
  function CreateNewPlugin(filename: string): TPlugin;
  procedure UpdatePluginData;
  function MergePluginsCompare(List: TStringList; Index1, Index2: Integer): Integer;
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
  function VersionCompare(v1, v2: string): boolean;
  procedure CompareStatuses;
  function UpdateChangeLog: boolean;
  function UpdateDictionary: boolean;
  function UpdateProgram: boolean;
  function DownloadProgram: boolean;
  function SendReports(var lst: TList): boolean;
  function SendPendingReports: boolean;


const
  // IMPORTANT CONSTANTS
  ProgramTesters = 'bla08, hishy, Kesta';
  ProgramTranslators = 'dhxxqk2010, Oaristys, Ganda, Martinezer, EHPDJFrANKy';
  xEditVersion = '3.1.1';
  bTranslationDump = false;

  // MSG IDs
  MSG_UNKNOWN = 0;
  MSG_NOTIFY = 1;
  MSG_REGISTER = 2;
  MSG_AUTH_RESET = 3;
  MSG_STATISTICS = 4;
  MSG_STATUS = 5;
  MSG_REQUEST = 6;
  MSG_REPORT = 7;

  // PLUGIN FLAGS
  FlagsArray: array[0..11] of TPluginFlag = (
    ( id: IS_BLACKLISTED; char: 'X'; desc: 'Is blacklisted'; ),
    ( id: HAS_ERRORS; char: 'E'; desc: 'Has errors'; ),
    ( id: ERRORS_IGNORED; char: 'R'; desc: 'Errors ignored'; ),
    ( id: NO_ERRORS; char: 'N'; desc: 'Has no errors'; ),
    ( id: HAS_BSA; char: 'A'; desc: 'Has a BSA file'; ),
    ( id: HAS_FACEDATA; char: 'G'; desc: 'Has FaceGenData'; ),
    ( id: HAS_VOICEDATA; char: 'V'; desc: 'Has Voice Data'; ),
    ( id: HAS_TRANSLATION; char: 'T'; desc: 'Has MCM Translations'; ),
    ( id: HAS_INI; char: 'I'; desc: 'Has an INI file'; ),
    ( id: HAS_FRAGMENTS; char: 'F'; desc: 'Has Script fragments'; ),
    ( id: DISALLOW_MERGING; char: 'D'; desc: 'Disallow merging'; ),
    ( id: REFERENCES_SELF; char: 'S'; desc: 'Scripts reference self'; )
  );

  // MERGE STATUSES
  StatusArray: array[0..11] of TMergeStatus = (
    ( id: msUnknown; color: $808080; desc: 'Unknown'; ),
    ( id: msNoPlugins; color: $0000FF; desc: 'No plugins to merge'; ),
    ( id: msDirInvalid; color: $0000FF; desc: 'Directories invalid'; ),
    ( id: msUnloaded; color: $0000FF; desc: 'Plugins not loaded'; ),
    ( id: msErrors; color: $0000FF; desc: 'Errors in plugins'; ),
    ( id: msFailed; color: $0000FF; desc: 'Merge failed'; ),
    ( id: msCheckErrors; color: $0080ed; desc: 'Check for errors required'; ),
    ( id: msUpToDate; color: $900000; desc: 'Up to date'; ),
    ( id: msUpToDateForced; color: $900000; desc: 'Up to date [Forced]'; ),
    ( id: msBuildReady; color: $009000; desc: 'Ready to be built'; ),
    ( id: msRebuildReady; color: $009000; desc: 'Ready to be rebuilt'; ),
    ( id: msRebuildReadyForced; color: $009000; desc: 'Ready to be rebuilt [Forced]'; )
  );
  // STATUS TYPES
  ErrorStatuses = [msUnknown, msNoPlugins, msDirInvalid, msUnloaded, msErrors];
  UpToDateStatuses = [msUpToDate, msUpToDateForced];
  BuildStatuses = [msBuildReady, msRebuildReady, msRebuildReadyForced];
  RebuildStatuses = [msRebuildReady, msRebuildReadyForced];
  ForcedStatuses = [msUpToDateForced, msRebuildReadyForced];

  // DELAYS
  StatusDelay = 2.0 / (60.0 * 24.0); // 2 minutes
  MaxConnectionAttempts = 3;

  // GAME MODES
  GameArray: array[1..4] of TGameMode = (
    ( longName: 'Skyrim'; gameName: 'Skyrim'; gameMode: gmTES5;
      appName: 'TES5'; exeName: 'TESV.exe'; appIDs: '72850';
      bsaOptMode: 'sk'; ),
    ( longName: 'Oblivion'; gameName: 'Oblivion'; gameMode: gmTES4;
      appName: 'TES4'; exeName: 'Oblivion.exe'; appIDs: '22330,900883';
      bsaOptMode: 'ob'; ),
    ( longName: 'Fallout New Vegas'; gameName: 'FalloutNV'; gameMode: gmFNV;
      appName: 'FNV'; exeName: 'FalloutNV.exe'; appIDs: '22380,2028016';
      bsaOptMode: 'fo'; ),
    ( longName: 'Fallout 3'; gameName: 'Fallout3'; gameMode: gmFO3;
      appName: 'FO3'; exeName: 'Fallout3.exe'; appIDs: '22300,22370';
      bsaOptMode: 'fo'; )
  );

var
  dictionary, blacklist, PluginsList, MergesList, BaseLog, Log,
  LabelFilters, GroupFilters, pluginsToHandle, mergesToBuild: TList;
  timeCosts, changelog, language: TStringList;
  settings: TSettings;
  CurrentProfile: TProfile;
  statistics, sessionStatistics: TStatistics;
  status, RemoteStatus: TmpStatus;
  handler: IwbContainerHandler;
  bDontSave, bChangeMergeProfile, bForceTerminate, bLoaderDone, bAuthorized,
  bProgramUpdate, bDictionaryUpdate, bInstallUpdate, bConnecting,
  bUpdateMergeStatus, bAllowClose: boolean;
  TempPath, LogPath, ProgramPath, dictionaryFilename, ActiveModProfile,
  ProgramVersion, xEditLogLabel, xEditLogGroup, DataPath, GamePath,
  ProfilePath: string;
  ConnectionAttempts: Integer;
  ActiveMods: TStringList;
  TCPClient: TidTCPClient;
  AppStartTime, LastStatusTime: TDateTime;
  GameMode: TGameMode;

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
  - InitPapyrus
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
  GameMode := GameArray[id];
  wbGameName := GameMode.gameName;
  wbGameMode := GameMode.gameMode;
  wbAppName := GameMode.appName;
  wbDataPath := CurrentProfile.gamePath + 'Data\';
  // set general paths
  DataPath := wbDataPath;
end;

{ Get the game ID associated with a game long name }
function GetGameID(name: string): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Low(GameArray) to High(GameArray) do
    if GameArray[i].longName = name then begin
      Result := i;
      exit;
    end;
end;

{ Tries to load various registry keys }
function TryRegistryKeys(var keys: TStringList): string;
var
  i: Integer;
  path, name: string;
begin
  with TRegistry.Create do try
    RootKey := HKEY_LOCAL_MACHINE;

    // try all keys
    for i := 0 to Pred(keys.Count) do begin
      path := ExtractFilePath(keys[i]);
      name := ExtractFileName(keys[i]);
      if OpenKeyReadOnly(path) then begin
        Result := ReadString(name);
        break;
      end;
    end;
  finally
    Free;
  end;
end;

{ Gets the path of a game from registry key or app path }
function GetGamePath(mode: TGameMode): string;
const
  sBethRegKey     = '\SOFTWARE\Bethesda Softworks\';
  sBethRegKey64   = '\SOFTWARE\Wow6432Node\Bethesda Softworks\';
  sSteamRegKey    = '\SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\'+
    'Steam App ';
  sSteamRegKey64  = '\SOFTWARE\Wow6432Node\Microsoft\Windows\CurrentVersion\'+
    'Uninstall\Steam App ';
var
  i: Integer;
  gameName: string;
  keys, appIDs: TStringList;
begin
  Result := '';

  // initialize variables
  gameName := mode.gameName;
  keys := TStringList.Create;
  appIDs := TStringList.Create;
  appIDs.CommaText := mode.appIDs;

  // add keys to check
  keys.Add(sBethRegKey + gameName + '\Installed Path');
  keys.Add(sBethRegKey64 + gameName + '\Installed Path');
  for i := 0 to Pred(appIDs.Count) do begin
    keys.Add(sSteamRegKey + appIDs[i] + '\InstallLocation');
    keys.Add(sSteamRegKey64 + appIDs[i] + '\InstallLocation');
  end;

  // try to find path from registry
  Result := TryRegistryKeys(keys);

  // free memory
  keys.Free;
  appIDs.Free;

  // set result
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
    gmFO3: DefineFO3;
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
  - TranslationExists
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
  Result := not aRecord.IsMaster;
end;

function ExtractFormID(filename: string): string;
const
  HexChars = ['0'..'9', 'A'..'F', 'a'..'f'];
var
  i, counter: Integer;
begin
  counter := 0;
  // we loop from the back because the formID is usually at the
  // end of the filename
  for i := Length(filename) downto 1 do begin
    if (filename[i] in HexChars) then
      Inc(counter)
    else
      counter := 0;
    // set result and exit if counter has reached 8
    if counter = 8 then begin
      Result := Copy(filename, i, 8);
      exit;
    end;
  end;
end;

function RemoveFileIndex(formID: string): string;
begin
  if Length(formID) <> 8 then
    raise Exception.Create('RemoveFileIndex: FormID must be 8 characters long');
  Result := '00' + Copy(formID, 3, 6);
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
  info: TSearchRec;
begin
  Result := false;
  filename := Lowercase(filename);
  if FindFirst(path, faAnyFile, info) = 0 then begin
    repeat
      if Pos(filename, Lowercase(info.Name)) > 0 then begin
        Result := true;
        exit;
      end;
    until FindNext(info) <> 0;
    FindClose(info);
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
  rec: IwbMainRecord;
  group: IwbGroupRecord;
  subgroup, container: IwbContainer;
  element, fragments: IwbElement;
  i, j: Integer;
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
      if not Supports(subgroup.Elements[j], IwbMainRecord, rec) then
        continue;
      if not rec.IsMaster then
        continue;
      if not Supports(rec, IwbContainer, container) then
        continue;
      fragments := container.ElementByPath[infoFragmentsPath];
      if not Assigned(fragments) then
        continue;
      Result := true;
    end;
  end;
end;

{ Returns true if Quest Fragments exist in @f }
function QuestFragmentsExist(f: IwbFile): boolean;
const
  questFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  rec: IwbMainRecord;
  group: IwbGroupRecord;
  container: IwbContainer;
  fragments: IwbElement;
  i: Integer;
begin
  Result := false;
  // exit if no QUST records in file
  if not f.HasGroup('QUST') then
    exit;

  // find all QUST records
  group := f.GroupBySignature['QUST'];
  for i := 0 to Pred(group.ElementCount) do begin
    if not Supports(group.Elements[i], IwbMainRecord, rec) then
      continue;
    if not rec.IsMaster then
      continue;
    if not Supports(rec, IwbContainer, container) then
      continue;
    fragments := container.ElementByPath[questFragmentsPath];
    if not Assigned(fragments) then
      continue;
    Result := true;
  end;
end;

{ Returns true if Quest Fragments exist in @f }
function SceneFragmentsExist(f: IwbFile): boolean;
const
  sceneFragmentsPath = 'VMAD - Virtual Machine Adapter\Data\Quest VMAD\Script Fragments Quest';
var
  rec: IwbMainRecord;
  group: IwbGroupRecord;
  container: IwbContainer;
  fragments: IwbElement;
  i: Integer;
begin
  Result := false;
  // exit if no SCEN records in file
  if not f.HasGroup('SCEN') then
    exit;

  // find all SCEN records
  group := f.GroupBySignature['SCEN'];
  for i := 0 to Pred(group.ElementCount) do begin
    if not Supports(group.Elements[i], IwbMainRecord, rec) then
      continue;
    if not rec.IsMaster then
      continue;
    if not Supports(rec, IwbContainer, container) then
      continue;
    fragments := container.ElementByPath[sceneFragmentsPath];
    if not Assigned(fragments) then
      continue;
    Result := true;
  end;
end;

{ Returns true if file-specific Script Fragments for @f are found }
function FragmentsExist(f: IwbFile): boolean;
begin
  Result := TopicInfoFragmentsExist(f) or QuestFragmentsExist(f)
    or SceneFragmentsExist(f);
end;

{ References self }
function ReferencesSelf(f: IwbFile): boolean;
var
  i: Integer;
  filename, source: string;
  scripts: IwbGroupRecord;
  container: IwbContainerElementRef;
  rec: IwbMainRecord;
begin
  // exit if has no script records in file
  Result := false;
  if not f.HasGroup('SCPT') then
    exit;

  // get scripts, and check them all for self-reference
  filename := f.FileName;
  scripts := f.GroupBySignature['SCPT'];
  if not Supports(scripts, IwbContainerElementRef, container) then
    exit;
  for i := 0 to Pred(container.ElementCount) do begin
    if not Supports(container.Elements[i], IwbMainRecord, rec) then
      continue;
    source := rec.ElementEditValues['SCTX - Script Source'];
    if Pos(filename, source) > 0 then begin
      Result := true;
      break;
    end;
  end;
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

{ Extracts assets from the BSA @filename to @destination, ignoring assets
  matching items in @ignore }
procedure ExtractBSA(ContainerName, destination: string; var ignore: TStringList);
var
  ResourceList: TStringList;
  i, j: Integer;
  skip: boolean;
begin
  if not handler.ContainerExists(ContainerName) then begin
    Tracker.Write('    '+ContainerName+' not loaded.');
    exit;
  end;
  ResourceList := TStringList.Create;
  handler.ContainerResourceList(ContainerName, ResourceList, '');
  for i := 0 to Pred(ResourceList.Count) do begin
    skip := false;
    for j := 0 to Pred(ignore.Count) do begin
      skip := Pos(ignore[j], ResourceList[i]) > 0;
      if skip then break;
    end;

    if skip then continue;
    handler.ResourceCopy(ContainerName, ResourceList[i], destination);
  end;
end;

function RemoveSelfOrContainer(const aElement: IwbElement): Boolean;
var
  cElement: IwbElement;
begin
  Result := false;
  if aElement.IsRemoveable then begin
    aElement.Remove;
    Result := true;
  end
  else begin
    if not Assigned(aElement.Container) then begin
      Tracker.Write('    Element has no container!');
      exit;
    end;
    // if element isn't removable, try removing its container
    if Supports(aElement.Container, IwbMainRecord) then begin
      Tracker.Write('    Reached main record, cannot remove element');
      exit;
    end;
    Tracker.Write('    Failed to remove '+aElement.Path+', removing container');
    if Supports(aElement.Container, IwbElement, cElement) then
      Result := RemoveSelfOrContainer(cElement);
  end;
end;

procedure UndeleteAndDisable(const aRecord: IwbMainRecord);
var
  xesp: IwbElement;
  sig: string;
  container: IwbContainerElementRef;
begin
  try
    sig := aRecord.Signature;

    // undelete
    aRecord.IsDeleted := true;
    aRecord.IsDeleted := false;

    // set persistence flag depending on game
    if (wbGameMode in [gmFO3,gmFNV,gmTES5])
    and ((sig = 'ACHR') or (sig = 'ACRE')) then
      aRecord.IsPersistent := true
    else if wbGameMode = gmTES4 then
      aRecord.IsPersistent := false;

      // place it below the ground
    if not aRecord.IsPersistent then
      aRecord.ElementNativeValues['DATA\Position\Z'] := -30000;

    // remove elements
    aRecord.RemoveElement('Enable Parent');
    aRecord.RemoveElement('XTEL');

    // add enabled opposite of player (true - silent)
    xesp := aRecord.Add('XESP', True);
    if Assigned(xesp) and Supports(xesp, IwbContainerElementRef, container) then begin
      container.ElementNativeValues['Reference'] := $14; // Player ref
      container.ElementNativeValues['Flags'] := 1;  // opposite of parent flag
    end;

    // set to disable
    aRecord.IsInitiallyDisabled := true;
  except
    on x: Exception do
      Tracker.Write('    Exception: '+x.Message);
  end;
end;


function FixErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
  var errors: TStringList): IwbMainRecord;
const
  cUDR = 'Record marked as deleted but contains:';
  cUnresolved = '< Error: Could not be resolved >';
  cNULL = 'Found a NULL reference, expected:';
var
  Error: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  if Tracker.Cancel then
    exit;

  // update progress based on number of main records processed
  if Supports(aElement, IwbMainRecord) then
    Tracker.UpdateProgress(1);

  Error := aElement.Check;
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    // fix record marked as deleted errors (UDRs)
    if Pos(cUDR, Error) = 1 then begin
      if Assigned(Result) then begin
        Tracker.Write('  Fixing UDR: '+Result.Name);
        UndeleteAndDisable(Result);
      end;
    end
    else begin
      // fix unresolved FormID errors by NULLing them out
      if Pos(cUnresolved, Error) > 0 then begin
        Tracker.Write('  Fixing Unresolved FormID: '+aElement.Path);
        aElement.NativeValue := 0;
        // we may end up with an invalid NULL reference, so we Check again
        Error := aElement.Check;
        if Error = '' then exit;
      end;

      // fix invalid NULL references by removal
      if Pos(cNULL, Error) = 1 then begin
        Tracker.Write('  Removing NULL reference: '+aElement.Path);
        if RemoveSelfOrContainer(aElement) then exit;
      end;

      // unhandled error
      Tracker.Write(Format('  Unhandled error: %s -> %s', [aElement.Path, error]));
      if Assigned(Result) and (lastRecord <> Result) then begin
        lastRecord := Result;
        errors.Add(Result.Name);
      end;
      errors.Add('  '+aElement.Path + ' -> ' + Error);
    end;
  end;

  // done if element doesn't have children
  if not Supports(aElement, IwbContainerElementRef, Container) then
    exit;

  // recurse through children elements
  for i := Pred(Container.ElementCount) downto 0 do begin
    Result := FixErrors(Container.Elements[i], Result, errors);
    // break if container got deleted
    if not Assigned(Container) then break;
  end;
end;

function CheckForErrors(const aElement: IwbElement; lastRecord: IwbMainRecord;
  var errors: TStringList): IwbMainRecord;
var
  Error, msg: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  if Tracker.Cancel then
    exit;

  // update progress based on number of main records processed
  if Supports(aElement, IwbMainRecord) then
    Tracker.UpdateProgress(1);

  Error := aElement.Check;
  // log errors
  if Error <> '' then begin
    Result := aElement.ContainingMainRecord;
    if Assigned(Result) and (Result <> LastRecord) then begin
      Tracker.Write('  '+Result.Name);
      errors.Add(Result.Name);
    end;
    msg := '  '+aElement.Path + ' -> ' + Error;
    Tracker.Write('  '+msg);
    errors.Add(msg);
  end;

  // recursion
  if Supports(aElement, IwbContainerElementRef, Container) then
    for i := Pred(Container.ElementCount) downto 0 do
      Result := CheckForErrors(Container.Elements[i], Result, errors);
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
      raise Exception.Create('Unable to create SEQ directory for merge.');
    s := p + ChangeFileExt(_File.FileName, '.seq');
    FileStream := TFileStream.Create(s, fmCreate);
    FileStream.WriteBuffer(FormIDs[0], Length(FormIDs)*SizeOf(Cardinal));
    Tracker.Write('Created SEQ file: ' + s);
    merge.files.Add(s);
  except
    on e: Exception do begin
      if Assigned(FileStream) then
        FreeAndNil(FileStream);
      Tracker.Write('Error: Can''t create SEQ file: ' + s + ', ' + E.Message);
      Exit;
    end;
  end;
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

{ Remove merged plugins from stringlist }
procedure RemoveMergedPlugins(var sl: TStringList);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if Assigned(MergeByFilename(MergesList, sl[i])) then
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
  ActiveModProfile := GetActiveProfile;
  GetActiveMods(ActiveMods, ActiveModProfile);
  //Logger.Write('GENERAL', 'ModOrganizer', 'ActiveMods: '#13#10+ActiveMods.Text);
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
  fname := settings.MOPath + 'ModOrganizer.ini';
  if(not FileExists(fname)) then begin
    Logger.Write('GENERAL', 'ModOrganizer', 'Mod Organizer ini file ' + fname + ' does not exist');
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
  modlistFilePath := settings.MOPath + 'profiles/' + profileName + '/modlist.txt';
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
    filePath := settings.MOModsPath + modName + '\' + filename;
    if (FileExists(filePath)) then begin
      Result := modName;
      exit;
    end;
  end;
end;


{******************************************************************************}
{ Log methods
  Set of methods for logging

  List of methods:
  - InitLog
  - RebuildLog
  - SaveLog
  - MessageGroupEnabled
}
{******************************************************************************}

procedure InitLog;
begin
  BaseLog := TList.Create;
  Log := TList.Create;
  LabelFilters := TList.Create;
  GroupFilters := TList.Create;
  // INITIALIZE GROUP FILTERS
  GroupFilters.Add(TFilter.Create('GENERAL', true));
  GroupFilters.Add(TFilter.Create('LOAD', true));
  GroupFilters.Add(TFilter.Create('CLIENT', true));
  GroupFilters.Add(TFilter.Create('MERGE', true));
  GroupFilters.Add(TFilter.Create('PLUGIN', true));
  GroupFilters.Add(TFilter.Create('ERROR', true));
  // INITIALIZE LABEL FILTERS
  LabelFilters.Add(TFilter.Create('GENERAL', 'Game', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Status', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Path', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Definitions', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Dictionary', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Load Order', true));
  LabelFilters.Add(TFilter.Create('GENERAL', 'Log', true));
  LabelFilters.Add(TFilter.Create('LOAD', 'Order', false));
  LabelFilters.Add(TFilter.Create('LOAD', 'Plugins', false));
  LabelFilters.Add(TFilter.Create('LOAD', 'Background', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Connect', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Login', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Response', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Update', true));
  LabelFilters.Add(TFilter.Create('CLIENT', 'Report', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Status', false));
  LabelFilters.Add(TFilter.Create('MERGE', 'Create', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Edit', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Check', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Clean', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Delete', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Build', true));
  LabelFilters.Add(TFilter.Create('MERGE', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Report', true));
  LabelFilters.Add(TFilter.Create('PLUGIN', 'Check', true));
end;

procedure RebuildLog;
var
  i: Integer;
  msg: TLogMessage;
begin
  Log.Clear;
  for i := 0 to Pred(BaseLog.Count) do begin
    msg := TLogMessage(BaseLog[i]);
    if MessageEnabled(msg) then
      Log.Add(msg);
  end;
end;

procedure SaveLog(var Log: TList);
var
  sl: TStringList;
  i: Integer;
  msg: TLogMessage;
  fdt: string;
begin
  sl := TStringList.Create;
  for i := 0 to Pred(Log.Count) do begin
    msg := TLogMessage(Log[i]);
    sl.Add(Format('[%s] (%s) %s: %s', [msg.time, msg.group, msg.&label, msg.text]));
  end;
  fdt := FormatDateTime('mmddyy_hhnnss', TDateTime(Now));
  ForceDirectories(LogPath);
  sl.SaveToFile(LogPath+'log_'+fdt+'.txt');
  sl.Free;
end;

function GetGroupFilter(msg: TLogMessage): TFilter;
var
  i: Integer;
  filter: TFilter;
begin
  Result := nil;
  for i := 0 to Pred(GroupFilters.Count) do begin
    filter := TFilter(GroupFilters[i]);
    if filter.group = msg.group then begin
      Result := filter;
      exit;
    end;
  end;
end;

function GetLabelFilter(msg: TLogMessage): TFilter;
var
  i: Integer;
  filter: TFilter;
begin
  Result := nil;
  for i := 0 to Pred(LabelFilters.Count) do begin
    filter := TFilter(LabelFilters[i]);
    if (filter.&label = msg.&label) and (filter.group = msg.group) then begin
      Result := filter;
      exit;
    end;
  end;
end;

function MessageEnabled(msg: TLogMessage): boolean;
var
  GroupFilter, LabelFilter: TFilter;
begin
  Result := true;
  GroupFilter := GetGroupFilter(msg);
  LabelFilter := GetLabelFilter(msg);
  if GroupFilter <> nil then
    Result := Result and GroupFilter.enabled;
  if LabelFilter <> nil then
    Result := Result and LabelFilter.enabled;
end;

{******************************************************************************}
{ Loading and saving methods
  Set of methods for loading and saving data.

  List of methods:
  - LoadLanguage
  - SaveProfile
  - SaveRegistrationData
  - LoadRegistrationData
  - SaveSettings
  - LoadSettings
  - SaveStatistics
  - LoadStatistics
  - LoadDictionary
  - SaveMerges
  - LoadMerges
  - IndexOfDump
  - SavePluginErrors
  - LoadPluginErrors
  - SaveReports
  - ReportExists
  - LoadReport
}
{******************************************************************************}

procedure LoadLanguage;
const
  langFile = 'http://raw.githubusercontent.com/matortheeternal/merge-plugins/master/frontend/lang/english.lang';
  directions = 'Your english.lang file is missing.  Please download it from GitHub.  ' +
    'After you click OK, a webpage with the file will be opened.  Right-click the ' +
    'page and choose "Save page as", then save it as english.lang in the "lang\" ' +
    'folder where you have MergePlugins.exe installed.';
var
  filename: string;
begin
  filename := Format('lang\%s.lang', [settings.language]);
  language := TStringList.Create;
  if (not FileExists(filename)) then begin
    if settings.language <> 'english' then begin
      settings.language := 'english';
      LoadLanguage;
    end
    else begin
      MessageDlg(directions, mtConfirmation, [mbOk], 0);
      ForceDirectories(ProgramPath + 'lang\');
      ShellExecute(0, 'open', PChar(langFile), '', '', SW_SHOWNORMAL);
    end;
  end
  else
    language.LoadFromFile(filename);
end;

function GetString(name: string): string;
begin
  if language.Values[name] <> '' then
    Result := StringReplace(language.Values[name], '#13#10', #13#10, [rfReplaceAll])
  else
    Result := name;
end;

procedure SaveProfile(var p: TProfile);
var
  path: string;
  pSettings: TSettings;
begin
  // get profile path
  path := ProgramPath + 'profiles\' + p.name + '\settings.ini';
  ForceDirectories(ExtractFilePath(path));

  // load settings if they exist, else create them
  if FileExists(path) then
    pSettings := LoadSettings(path)
  else
    pSettings := TSettings.Create;

  // save profile details to settings
  pSettings.profile := p.name;
  pSettings.gameMode := p.gameMode;
  pSettings.gamePath := p.gamePath;
  SaveSettings(pSettings, path);
end;

procedure SaveRegistrationData(var s: TSettings);
const
  sMergePluginsRegKey = 'Software\\Merge Plugins\\';
  sMergePluginsRegKey64 = 'Software\\Wow6432Node\\Merge Plugins\\';
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_READ);
  reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    reg.Access := KEY_WRITE;
    if not reg.OpenKey(sMergePluginsRegKey, true) then
      if not reg.OpenKey(sMergePluginsRegKey64, true) then
        exit;

    reg.WriteString('Username', s.username);
    reg.WriteString('Key', s.key);
    reg.WriteBool('Registered', s.registered);
  except on Exception do
    // nothing
  end;

  reg.CloseKey();
  reg.Free;
end;

procedure LoadRegistrationData(var s: TSettings);
const
  sMergePluginsRegKey = 'Software\\Merge Plugins\\';
  sMergePluginsRegKey64 = 'Software\\Wow6432Node\\Merge Plugins\\';
var
  reg: TRegistry;
begin
  reg := TRegistry.Create(KEY_READ);
  reg.RootKey := HKEY_LOCAL_MACHINE;

  try
    if (not reg.KeyExists(sMergePluginsRegKey))
      xor (not reg.KeyExists(sMergePluginsRegKey64)) then
        exit;

    if not reg.OpenKeyReadOnly(sMergePluginsRegKey) then
      if not reg.OpenKeyReadOnly(sMergePluginsRegKey64) then
        exit;

    if reg.ReadBool('Registered') then begin
      s.username := reg.ReadString('Username');
      s.key := reg.ReadString('Key');
      s.registered := true;
    end;
  except on Exception do
    // nothing
  end;

  reg.CloseKey();
  reg.Free;
end;

procedure SaveSettings;
begin
  TRttiIni.Save(ProfilePath + 'settings.ini', settings);
  if settings.registered then
    SaveRegistrationData(settings);
end;

procedure SaveSettings(var s: TSettings; path: string);
begin
  TRttiIni.Save(path, s);
  // save registration data to registry if registered
  if (s.registered) then
    SaveRegistrationData(s);
end;

procedure LoadSettings;
begin
  settings := TSettings.Create;
  TRttiIni.Load(ProfilePath + 'settings.ini', settings);
  LoadRegistrationData(settings);
end;

function LoadSettings(path: string): TSettings;
begin
  Result := TSettings.Create;
  TRttiIni.Load(path, Result);
  LoadRegistrationData(Result);
end;

procedure SaveStatistics;
begin
  // move session statistics to general statistics
  Inc(statistics.timesRun, sessionStatistics.timesRun);
  Inc(statistics.mergesBuilt, sessionStatistics.mergesBuilt);
  Inc(statistics.pluginsChecked, sessionStatistics.pluginsChecked);
  Inc(statistics.pluginsFixed, sessionStatistics.pluginsFixed);
  Inc(statistics.pluginsMerged, sessionStatistics.pluginsMerged);
  Inc(statistics.reportsSubmitted, sessionStatistics.reportsSubmitted);
  // zero out session statistics
  sessionStatistics.timesRun := 0;
  sessionStatistics.mergesBuilt := 0;
  sessionStatistics.pluginsChecked := 0;
  sessionStatistics.reportsSubmitted := 0;
  // save to file
  TRttiIni.Save('statistics.ini', statistics);
end;

procedure LoadStatistics;
begin
  statistics := TStatistics.Create;
  sessionStatistics := TStatistics.Create;
  TRttiIni.Load('statistics.ini', statistics);
end;

procedure LoadChangelog;
begin
  // load changelog
  if not Assigned(changelog) then
    changelog := TStringList.Create;

  // don't attempt to load changelog if it doesn't exist
  if not FileExists('changelog.txt') then begin
    Logger.Write('GENERAL', 'Changelog', 'No changelog found');
    exit;
  end;

  // load changelog
  changelog.LoadFromFile('changelog.txt');
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
    Logger.Write('GENERAL', 'Dictionary', 'No dictionary file '+dictionaryFilename);
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

procedure RenameSavedPlugins;
var
  i: Integer;
  plugin: TPlugin;
  oldFileName, newFileName, bakFileName: string;
begin
  wbFileForceClosed;
  for i := Pred(PluginsList.Count) downto 0 do begin
    plugin := TPlugin(PluginsList[i]);
    try
      plugin._File._Release;
      oldFileName := plugin.dataPath + plugin.filename;
      newFileName := oldFileName + '.save';
      if FileExists(newFileName) then begin
        bakFileName := oldFileName + '.bak';
        if FileExists(bakFileName) then
          DeleteFile(bakFileName);
        RenameFile(oldFileName, bakFileName);
        RenameFile(newFileName, oldFileName);
      end;
    except
      on x: Exception do
        Tracker.Write('Failed to rename ' + plugin.filename + '.save');
    end;
  end;
end;

procedure SaveMerges;
var
  i: Integer;
  merge: TMerge;
  json: ISuperObject;
  filename: string;
begin
  // initialize json
  json := SO;
  json.O['merges'] := SA([]);

  // loop through merges
  Tracker.Write('Dumping merges to JSON');
  for i := 0 to Pred(MergesList.Count) do begin
    Tracker.UpdateProgress(1);
    merge := TMerge(MergesList[i]);
    Tracker.Write('  Dumping '+merge.name);
    json.A['merges'].Add(merge.Dump);
  end;

  // save and finalize
  filename := ProfilePath + 'Merges.json';
  Tracker.Write(' ');
  Tracker.Write('Saving to ' + filename);
  Tracker.UpdateProgress(1);
  json.SaveTo(filename);
  json := nil;
end;

procedure LoadMerges;
const
  debug = false;
var
  merge: TMerge;
  obj, mergeItem: ISuperObject;
  sl: TStringList;
  filename: string;
begin
  // don't load file if it doesn't exist
  filename := ProfilePath + 'Merges.json';
  if not FileExists(filename) then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through merges
  for mergeItem in obj['merges'] do begin
    merge := TMerge.Create;
    merge.LoadDump(mergeItem);
    MergesList.Add(merge);
  end;

  // finalize
  obj := nil;
  sl.Free;
end;

procedure AssignMergesToPlugins;
var
  i, j: Integer;
  merge: TMerge;
  plugin: TPlugin;
begin
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      if Assigned(plugin) then
        plugin.merge := merge.name;
    end;
  end;
end;

function IndexOfDump(a: TSuperArray; plugin: TPlugin): Integer;
var
  i: Integer;
  obj: ISuperObject;
begin
  Result := -1;
  for i := 0 to Pred(a.Length) do begin
    obj := a.O[i];
    if (obj.S['filename'] = plugin.filename)
    and (obj.S['hash'] = plugin.hash) then begin
      Result := i;
      exit;
    end;
  end;
end;

procedure SavePluginInfo;
var
  i, index: Integer;
  plugin: TPlugin;
  obj: ISuperObject;
  filename: string;
  sl: TStringList;
begin
  // don't load file if it doesn't exist
  filename := 'user\' + wbAppName + 'PluginInfo.json';
  if FileExists(filename) then begin
    // load file text into SuperObject to parse it
    sl := TStringList.Create;
    sl.LoadFromFile(filename);
    obj := SO(PChar(sl.Text));
    sl.Free;
  end
  else begin
    // initialize new json object
    obj := SO;
    obj.O['plugins'] := SA([]);
  end;

  // loop through plugins
  Tracker.Write('Dumping plugin errors to JSON');
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := PluginsList[i];
    Tracker.UpdateProgress(1);
    if not (plugin.HasBeenCheckedForErrors or plugin.bDisallowMerging) then
      continue;
    Tracker.Write('  Dumping '+plugin.filename);
    index := IndexOfDump(obj.A['plugins'], plugin);
    if index = -1 then
      obj.A['plugins'].Add(plugin.InfoDump)
    else
      obj.A['plugins'].O[index] := plugin.InfoDump;
  end;

  // save and finalize
  Tracker.Write(' ');
  filename := ProfilePath + 'PluginInfo.json';
  Tracker.Write('Saving to '+filename);
  Tracker.UpdateProgress(1);
  obj.SaveTo(filename);
  obj := nil;
end;

procedure LoadPluginInfo;
var
  plugin: TPlugin;
  obj, pluginItem: ISuperObject;
  sl: TStringList;
  filename, hash: string;
begin
  // don't load file if it doesn't exist
  filename := ProfilePath + 'PluginInfo.json';
  if not FileExists(filename) then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through merges
  filename := '';
  for pluginItem in obj['plugins'] do begin
    filename := pluginItem.AsObject.S['filename'];
    hash := pluginItem.AsObject.S['hash'];
    plugin := PluginByFileName(filename);
    if not Assigned(plugin) then
      continue;
    if plugin.hash = hash then begin
      plugin.LoadInfoDump(pluginItem);
      plugin.GetFlags;
    end;
  end;

  // finalize
  obj := nil;
  sl.Free;
end;

procedure SaveReports(var lst: TList; path: string);
var
  i: Integer;
  report: TReport;
  fn: string;
begin
  //ForceDirectories(path);
  for i := 0 to Pred(lst.Count) do begin
    report := TReport(lst[i]);
    report.dateSubmitted := Now;
    fn := Format('%s-%s.txt', [report.filename, report.hash]);
    report.Save(path + fn);
  end;
end;

procedure LoadReport(var report: TReport);
var
  fn, unsubmittedPath, submittedPath: string;
begin
  fn := Format('%s-%s.txt', [report.filename, report.hash]);
  unsubmittedPath := 'reports\' + fn;
  submittedPath := 'reports\submitted\' + fn;
  if FileExists(unsubmittedPath) then
    LoadReport(unsubmittedPath, report)
  else if FileExists(submittedPath) then
    LoadReport(submittedPath, report);
end;

procedure LoadReport(const filename: string; var report: TReport);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  report := TReport(TRttiJson.FromJson(sl.Text, report.ClassType));
  sl.Free;
end;


{******************************************************************************}
{ Helper methods
  Set of methods to help with working with Merge Plugins types.

  List of methods:
  - DeleteTempPath
  - GetRatingColor
  - GetEntry
  - IsBlacklisted
  - PluginLoadOrder
  - PluginByFilename
  - MergeByName
  - MergeByFilename
  - CreateNewMerge
  - CreateNewPlugin
  - MergePluginsCompare
}
{******************************************************************************}

procedure DeleteTempPath;
begin
  DeleteDirectory(TempPath);
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

procedure UpdatePluginData;
var
  i: Integer;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    plugin.UpdateData;
  end;
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
  if (bConnecting or TCPClient.Connected)
  or (ConnectionAttempts >= MaxConnectionAttempts) then
    exit;

  bConnecting := true;
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
  bConnecting := false;
end;

function ServerAvailable: boolean;
begin
  Result := false;

  try
    if TCPClient.Connected then begin
      TCPClient.IOHandler.WriteLn('', TIdTextEncoding.Default);
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
  TCPClient.IOHandler.WriteLn(msgJson, TIdTextEncoding.Default);
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
    msg := TmpMessage.Create(MSG_NOTIFY, settings.username, settings.key, 'Authorized?');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmpMessage(TRttiJson.FromJson(line, TmpMessage));
    Logger.Write('CLIENT', 'Response', response.data);
    Result := response.data = 'Yes';
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception authorizing user '+x.Message);
    end;
  end;

  // set bAuthorized boolean
  bAuthorized := Result;
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
    msg := TmpMessage.Create(MSG_NOTIFY, settings.username, settings.key, wbAppName);
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
    msg := TmpMessage.Create(MSG_STATISTICS, settings.username, settings.key, TRttiJson.ToJson(sessionStatistics));
    SendClientMessage(msg);

    // get response
    LLine := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
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
    msg := TmpMessage.Create(MSG_AUTH_RESET, settings.username, settings.key, '');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
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
    msg := TmpMessage.Create(MSG_REGISTER, username, settings.key, 'Check');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
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
    msg := TmpMessage.Create(MSG_REGISTER, username, settings.key, 'Register');
    SendClientMessage(msg);

    // get response
    line := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
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
    msg := TmpMessage.Create(MSG_STATUS, settings.username, settings.key, '');
    SendClientMessage(msg);

    // get response
    LLine := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
    response := TmpMessage(TRttiJson.FromJson(LLine, TmpMessage));
    RemoteStatus := TmpStatus(TRttiJson.FromJson(response.data, TmpStatus));
    //Logger.Write('CLIENT', 'Response', response.data);
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception getting status '+x.Message);
    end;
  end;
end;

function VersionCompare(v1, v2: string): boolean;
var
  sl1, sl2: TStringList;
  i, c1, c2: integer;
begin
  Result := false;

  // parse versions with . as delimiter
  sl1 := TStringList.Create;
  sl1.LineBreak := '.';
  sl1.Text := v1;
  sl2 := TStringList.Create;
  sl2.LineBreak := '.';
  sl2.Text := v2;

  // look through each version clause and perform comparisons
  i := 0;
  while (i < sl1.Count) and (i < sl2.Count) do begin
    c1 := StrToInt(sl1[i]);
    c2 := StrToInt(sl2[i]);
    if (c1 < c2) then begin
      Result := true;
      break;
    end
    else if (c1 > c2) then begin
      Result := false;
      break;
    end;
    Inc(i);
  end;

  // free ram
  sl1.Free;
  sl2.Free;
end;

procedure CompareStatuses;
begin
  if not Assigned(RemoteStatus) then
    exit;

  // handle program update
  // TODO: split string on . and do a greater than comparison for each clause
  bProgramUpdate := VersionCompare(status.programVersion, RemoteStatus.programVersion);

  // handle dictionary update based on gamemode
  case wbGameMode of
    gmTES5: begin
      bDictionaryUpdate := status.tes5Hash <> RemoteStatus.tes5Hash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.tes5Hash+' != '+RemoteStatus.tes5hash);
    end;
    gmTES4: begin
      bDictionaryUpdate := status.tes4Hash <> RemoteStatus.tes4Hash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.tes4Hash+' != '+RemoteStatus.tes4hash);
    end;
    gmFNV: begin
      bDictionaryUpdate := status.fnvHash <> RemoteStatus.fnvHash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.fnvHash+' != '+RemoteStatus.fnvhash);
    end;
    gmFO3: begin
      bDictionaryUpdate := status.fo3Hash <> RemoteStatus.fo3Hash;
      if bDictionaryUpdate then
        Logger.Write('GENERAL', 'Status', 'Dictionary update available '+
          status.fo3Hash+' != '+RemoteStatus.fo3hash);
    end;
  end;
end;

function UpdateChangeLog: boolean;
var
  msg: TmpMessage;
  stream: TFileStream;
begin
  Result := false;
  if not TCPClient.Connected then
    exit;
  Logger.Write('CLIENT', 'Update', 'Getting changelog');
  Tracker.Write('Getting changelog');

  // attempt to request changelog
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmpMessage.Create(MSG_REQUEST, settings.username, settings.key, 'Changelog');
    SendClientMessage(msg);

    // get response
    stream := TFileStream.Create('changelog.txt', fmCreate + fmShareDenyNone);
    TCPClient.IOHandler.LargeStream := True;
    TCPClient.IOHandler.ReadStream(stream, -1, False);

    // load changelog from response
    Logger.Write('CLIENT', 'Update', 'Changelog recieved.  (Size: '+FormatByteSize(stream.Size)+')');
    stream.Free;
    LoadChangelog;
    Result := true;
  except
    on x : Exception do begin
      Logger.Write('ERROR', 'Client', 'Exception getting changelog '+x.Message);
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
    msg := TmpMessage.Create(MSG_REQUEST, settings.username, settings.key, filename);
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
      FileName := ProgramPath + 'MergePlugins.zip';
      // Set base (default) directory for all archive operations
      BaseDirectory := ProgramPath;
      // Extract all files from the archive to current directory
      ExtractFiles('*.exe');
      BaseDirectory := ProgramPath + 'lang\';
      ExtractFiles('lang\*.lang');
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
    MessageDlg(GetString('mpOpt_PendingUpdate'), mtInformation, [mbOk], 0);
    Result := true;
    exit;
  end;

  Logger.Write('CLIENT', 'Update', 'Merge Plugins v'+RemoteStatus.programVersion);
  Tracker.Write('Updating program to v'+RemoteStatus.programVersion);

  // attempt to request dictionary
  // throws exception if server is unavailable
  try
    // send request to server
    msg := TmpMessage.Create(MSG_REQUEST, settings.username, settings.key, 'Program');
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
      msg := TmpMessage.Create(MSG_REPORT, settings.username, settings.key, reportJson);
      SendClientMessage(msg);

      // get response
      LLine := TCPClient.IOHandler.ReadLn(TIdTextEncoding.Default);
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
  path := ProgramPath + 'reports\';
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
      DeleteFile(path + info.Name);
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


{******************************************************************************}
{ Object methods
  Set of methods for objects.

  List of methods:
  - TLogMessage.Create
  - TmpMessage.Create
  - TReport.Create
  - TReport.TRttiJson.ToJson
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

constructor TLogMessage.Create(time, appTime, group, &label, text: string);
begin
  self.time := time;
  self.appTime := appTime;
  self.group := group;
  self.&label := &label;
  self.text := text;
end;

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

{ TReport }
procedure TReport.SetNotes(notes: string);
begin
  self.notes := StringReplace(Trim(notes), #13#10, '@13', [rfReplaceAll]);
end;

function TReport.GetNotes: string;
begin
  Result := StringReplace(notes, '@13', #13#10, [rfReplaceAll]);
end;

procedure TReport.Save(const filename: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  sl.Text := TRttiJson.ToJson(self);
  try
    ForceDirectories(ExtractFilePath(filename));
    sl.SaveToFile(filename);
  except
    on x: Exception do
      ShowMessage(Format('Unabled to save report %s: %s', [filename, x.Message]));
  end;
  sl.Free;
end;

{ TPlugin Constructor }
constructor TPlugin.Create;
begin
  hasData := false;
  merge := ' ';
  description := TStringList.Create;
  masters := TStringList.Create;
  errors := TStringList.Create;
  reports := TStringList.Create;
end;

destructor TPlugin.Destroy;
begin
  description.Free;
  masters.Free;
  errors.Free;
  reports.Free;
  _File._Release;
  inherited;
end;

{ Gets the flag values for a TPlugin }
procedure TPlugin.GetFlags;
begin
  flags := [];
  if IsBlacklisted(filename) then begin
    flags := flags + [IS_BLACKLISTED];
    exit;
  end;
  if HasBeenCheckedForErrors then begin
    if bIgnoreErrors then
      flags := flags + [ERRORS_IGNORED]
    else if HasErrors then
      flags := flags + [HAS_ERRORS]
    else
      flags := flags + [NO_ERRORS];
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
  if bDisallowMerging then
    flags := flags + [DISALLOW_MERGING];
  if ReferencesSelf(_File) then
    flags := flags + [REFERENCES_SELF];
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
  description.Text := Wordwrap(s, 80);

  // get reports
  entry := GetEntry(filename, numRecords, ProgramVersion);
  s := Trim(StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]));
  reports.Text := Wordwrap(s, 80);

  // get file attributes
  fileSize := GetFileSize(wbDataPath + filename);
  dateModified := DateTimeToStr(GetLastModified(wbDataPath + filename));

  // get numOverrides if not blacklisted
  if not (IS_BLACKLISTED in flags) then
    numOverrides := IntToStr(CountOverrides(_File));
end;

procedure TPlugin.UpdateData;
var
  s: string;
begin
  // get reports
  entry := GetEntry(filename, numRecords, ProgramVersion);
  s := Trim(StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]));
  reports.Text := Wordwrap(s, 80);

  // update blacklisted flag if it was blacklisted
  if IsBlacklisted(filename) then
    flags := [IS_BLACKLISTED];
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
      dataPath := settings.MOModsPath + modName + '\';
  end;
end;

function TPlugin.GetFormIndex: Integer;
var
  Container, MasterFiles: IwbContainer;
begin
  Result := 0;
  Container := self._File as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  if Container.ElementExists['Master Files'] then begin
    MasterFiles := Container.ElementByPath['Master Files'] as IwbContainer;
    Result := MasterFiles.ElementCount;
  end;
end;

{ Checks for errors in a plugin }
procedure TPlugin.FindErrors;
var
  tempErrors: TStringList;
begin
  tempErrors := TStringList.Create;
  CheckForErrors(_File as IwbElement, nil, tempErrors);

  // exit if user cancelled
  if Tracker.Cancel then
    exit;

  // transfer errors
  errors.Text := tempErrors.Text;
  tempErrors.Free;
  if (errors.Count = 0) then
    errors.Add('None.');

  // update flags, statistics
  GetFlags;
  Inc(sessionStatistics.pluginsChecked);
end;

{ Fixes errors in a plugin }
procedure TPlugin.ResolveErrors;
var
  mr: Integer;
  FileStream: TFileStream;
  prompt, newFileName: string;
  tempErrors: TStringList;
begin
  tempErrors := TStringList.Create;
  FixErrors(_File as IwbElement, nil, tempErrors);

  // exit if user cancelled
  if Tracker.Cancel then
    exit;

  // transfer errors to plugin
  errors.Text := tempErrors.Text;
  tempErrors.Free;
  if (errors.Count = 0) then
    errors.Add('None.');

  // update hash, flags
  GetHash;
  GetFlags;
  Inc(sessionStatistics.pluginsFixed);

  // save plugin if user is OK with that
  prompt := Format(GetString('mpProg_SavePlugin'), [filename]);
  mr := MessageDlg(prompt, mtConfirmation, [mbYes,mbNo], 0, mbYes);
  if mr = 6 then begin
    Tracker.Write(' ');
    newFileName := DataPath + filename + '.save';
    FileStream := TFileStream.Create(newFileName, fmCreate);
    try
      Tracker.Write('Saving: ' + newFileName);
      _File.WriteToStream(FileStream, False);
      Tracker.Write(' ');
    finally
      FileStream.Free;
    end;
  end;
end;

function TPlugin.HasBeenCheckedForErrors: boolean;
begin
  Result := errors.Count > 0;
end;

function TPlugin.HasErrors: boolean;
begin
  Result := false;
  if HasBeenCheckedForErrors then
    Result := errors[0] <> 'None.';
end;

function TPlugin.CanBeMerged: boolean;
begin
  Result := HasBeenCheckedForErrors and (not bDisallowMerging) and
    (bIgnoreErrors or not HasErrors);
end;

function TPlugin.IsInMerge: boolean;
begin
  Result := merge <> ' ';
end;

function TPlugin.InfoDump: ISuperObject;
var
  obj: ISuperObject;
  i: integer;
begin
  obj := SO;

  // filename, hash, errors
  obj.S['filename'] := filename;
  obj.S['hash'] := hash;
  obj.B['bIgnoreErrors'] := bIgnoreErrors;
  obj.B['bDisallowMerging'] := bDisallowMerging;
  obj.O['errors'] := SA([]);
  for i := 0 to Pred(errors.Count) do
    obj.A['errors'].S[i] := errors[i];

  Result := obj;
end;

procedure TPlugin.LoadInfoDump(obj: ISuperObject);
var
  item: ISuperObject;
begin
  // load boolean
  bIgnoreErrors := obj.AsObject.B['bIgnoreErrors'];
  bDisallowMerging := obj.AsObject.B['bDisallowMerging'];

  // load errors
  errors.Clear;
  for item in obj['errors'] do
    errors.Add(item.AsString);
end;

{ TMerge Constructor }
constructor TMerge.Create;
begin
  name := 'NewMerge';
  filename := 'NewMerge.esp';
  status := msUnknown;
  dateBuilt := 0;
  plugins := TStringList.Create;
  hashes := TStringList.Create;
  masters := TStringList.Create;
  map := TStringList.Create;
  lmap := TStringList.Create;
  files := TStringList.Create;
  geckScripts := TStringList.Create;
  navConflicts := TStringList.Create;
  method := 'Overrides';
  renumbering := 'Conflicting';
  fails := TStringList.Create;
end;

destructor TMerge.Destroy;
begin
  plugins.Free;
  hashes.Free;
  masters.Free;
  map.Free;
  lmap.Free;
  files.Free;
  geckScripts.Free;
  navConflicts.Free;
  fails.Free;
  plugin.Free;
  inherited;
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

// Checks to see if the plugins in a merge have been modified since it was last
// merged.
function TMerge.PluginsModified: boolean;
var
  plugin: TPlugin;
  i: integer;
begin
  Result := false;
  // true if number of hashes not equal to number of plugins
  if plugins.Count <> hashes.Count then begin
    Logger.Write('MERGE', 'Status', name + ' -> Plugin count changed');
    Result := true;
    exit;
  end;
  // true if any plugin hash doesn't match
  for i := 0 to Pred(plugins.count) do begin
    plugin := PluginByFilename(plugins[i]);
    if Assigned(plugin) then begin
      if plugin.hash <> hashes[i] then begin
        Logger.Write('MERGE', 'Status', name + ' -> '+plugin.filename + ' hash changed.');
        Result := true;
      end;
    end;
  end;
end;

// Checks if the files associated with a merge exist
function TMerge.FilesExist: boolean;
begin
  Result := FileExists(dataPath + filename);
end;

procedure TMerge.GetStatus;
var
  i: Integer;
  plugin: TPlugin;
begin
  Logger.Write('MERGE', 'Status', name + ' -> Getting status');
  status := msUnknown;

  // don't merge if no plugins to merge
  if (plugins.Count < 1) then begin
    Logger.Write('MERGE', 'Status', name + ' -> No plugins to merge');
    status := msNoPlugins;
    exit;
  end;

  // don't merge if mod destination directory is blank
  if (settings.mergeDirectory = '') then begin
    Logger.Write('MERGE', 'Status', name + ' -> Merge directory blank');
    status := msDirInvalid;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is blank
  if settings.usingMO and (settings.MOPath = '') then begin
    Logger.Write('MERGE', 'Status', name + ' -> Mod Organizer Directory blank');
    status := msDirInvalid;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is invalid
  if settings.usingMO and not DirectoryExists(settings.MOPath) then begin
     Logger.Write('MERGE', 'Status', name + ' -> Mod Organizer Directory invalid');
     status := msDirInvalid;
     exit;
  end;

  // loop through plugins
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      Logger.Write('MERGE', 'Status', name + ' -> Plugin '+plugins[i]+' is missing');
      if status = msUnknown then status := msUnloaded;
      continue;
    end;

    if (not plugin.HasBeenCheckedForErrors) then begin
      Logger.Write('MERGE', 'Status', name + ' -> '+plugin.filename+' needs to be checked for errors.');
      if status = msUnknown then status := msCheckErrors;
    end
    else if plugin.HasErrors and not plugin.bIgnoreErrors then begin
      Logger.Write('MERGE', 'Status', name + ' -> '+plugin.filename+' has errors');
      if status = msUnknown then status := msErrors;
    end;
  end;

  dataPath := settings.mergeDirectory + name + '\';
  if (not PluginsModified) and FilesExist and (status = msUnknown) then begin
    Logger.Write('MERGE', 'Status', name + ' -> Up to date');
    status := msUpToDate;
  end;

  // status green, ready to go
  if status = msUnknown then begin
    Logger.Write('MERGE', 'Status', name + ' -> Ready to be merged');
    if dateBuilt = 0 then
      status := msBuildReady
    else
      status := msRebuildReady;
  end;
end;

function TMerge.GetStatusColor: integer;
begin
  Result := StatusArray[Ord(status)].color;
end;

// Update the hashes list for the plugins in the merge
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

// Get load order for plugins in merge that don't have it
procedure TMerge.GetLoadOrders;
var
  i: integer;
begin
  for i := 0 to Pred(plugins.Count) do
    if not Assigned(plugins.Objects[i]) then
      plugins.Objects[i] := TObject(PluginLoadOrder(plugins[i]));
end;

// Sort plugins by load order position
procedure TMerge.SortPlugins;
begin
  GetLoadOrders;
  plugins.CustomSort(MergePluginsCompare);
end;

procedure TMerge.Remove(plugin: TPlugin);
begin
  plugin.merge := ' ';
  plugins.Delete(plugins.IndexOf(plugin.filename));
end;

{ TFilter }
constructor TFilter.Create(group: string; enabled: boolean);
begin
  self.group := group;
  self.enabled := enabled;
end;

constructor TFilter.Create(group, &label: string; enabled: boolean);
begin
  self.group := group;
  self.&label := &label;
  self.enabled := enabled;
end;

{ TEntry }
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
  // default settings
  language := 'English';
  serverHost := 'mergeplugins.us.to';
  serverPort := 960;
  simpleDictionaryView := false;
  simplePluginsView := false;
  updateDictionary := false;
  updateProgram := false;
  usingMO := false;
  MOPath := '';
  copyGeneralAssets := false;
  mergeDirectory := wbDataPath;
  handleFaceGenData := true;
  handleVoiceAssets := true;
  handleMCMTranslations := true;
  handleINIs := true;
  handleSEQ := true;
  handleScriptFragments := false;
  handleSelfReference := false;
  extractBSAs := false;
  buildMergedBSA := false;
  batCopy := true;
  generalMessageColor := clGreen;
  loadMessageColor := clPurple;
  clientMessageColor := clBlue;
  mergeMessageColor := $000080FF;
  pluginMessageColor := $00484848;
  errorMessageColor := clRed;
  logMessageTemplate := '[{{AppTime}}] ({{Group}}) {{Label}}: {{Text}}';

  // generate a new secure key
  GenerateKey;
end;

procedure TSettings.GenerateKey;
const
  chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var
  i: Integer;
begin
  key := '';
  for i := 0 to 31 do
    key := key + chars[Random(64)];
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


{ TProfile }
constructor TProfile.Create(name: string);
begin
  self.name := name;
end;

procedure TProfile.Delete;
var
  path: string;
begin
  path := ProgramPath + 'profiles\' + name;
  if DirectoryExists(path) then
    DeleteDirectory(path);
end;

procedure TProfile.Rename(name: string);
var
  oldProfilePath, newProfilePath: string;
begin
  // rename old profile folder if necessary
  oldProfilePath := ProgramPath + 'profiles\' + self.name;
  newProfilePath := ProgramPath + 'profiles\' + name;
  if DirectoryExists(oldProfilePath) then
    RenameFile(oldProfilePath, newProfilePath);

  // then change name in the object
  self.name := name;
end;


end.
