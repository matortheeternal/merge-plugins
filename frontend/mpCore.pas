unit mpCore;

interface

uses
  Classes,
  // third party libraries
  superobject,
  // mte units
  mteBase;

type
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
  // PLUGINS AND MERGES
  TPluginFlagID = (IS_BLACKLISTED, HAS_ERRORS, ERRORS_IGNORED, NO_ERRORS,
    HAS_BSA, HAS_FACEDATA, HAS_VOICEDATA, HAS_TRANSLATION, HAS_INI,
    HAS_FRAGMENTS, DISALLOW_MERGING, REFERENCES_SELF );
  TPluginFlags = set of TPluginFlagID;
  TPluginFlag = Record
    id: TPluginFlagID;
    char: char;
    desc: string[128];
  end;
  TPlugin = class(TBasePlugin)
  public
    flags: TPluginFlags;
    merge: string;
    entry: TEntry;
    errors: TStringList;
    reports: TStringList;
    bIgnoreErrors: boolean;
    bDisallowMerging: boolean;
    constructor Create; override;
    destructor Destroy; override;
    procedure GetMpData; overload;
    procedure UpdateData; override;
    procedure GetDataPath;
    procedure GetFlags;
    function GetFlagsString: string;
    function GetFlagsDescription: string;
    procedure FindErrors;
    procedure ResolveErrors;
    function HasBeenCheckedForErrors: boolean;
    function HasErrors: boolean;
    function CanBeMerged: boolean;
    function IsInMerge: boolean;
    procedure LoadInfoDump(obj: ISuperObject);
    function InfoDump: ISuperObject;
  end;
  TMergeStatusID = ( msUnknown, msUnloaded, msNoPlugins, msDirInvalid,
    msErrors, msCheckErrors, msNotContiguous, msBreaksDependencies,
    msUpToDate, msUpToDateForced, msBuildReady, msRebuildReady,
    msRebuildReadyForced, msFailed, msBuilt, msCanceled );
  TMergeStatus = Record
    id: TMergeStatusID;
    color: integer;
    desc: string[64];
  end;
  TMerge = class(TObject)
  public
    name: string;
    filename: string;
    dateBuilt: TDateTime;
    bIgnoreNonContiguous: boolean;
    method: string;
    renumbering: string;
    dataPath: string;
    status: TMergeStatusID;
    plugin: TPlugin;
    plugins: TStringList;
    hashes: TStringList;
    masters: TStringList;
    map: TStringList;
    lmap: TStringList;
    files: TStringList;
    fails: TStringList;
    ignoredDependencies: TStringList;
    geckScripts: TStringList;
    navConflicts: TStringList;
    constructor Create; virtual;
    destructor Destroy; override;
    function Dump: ISuperObject;
    procedure LoadDump(obj: ISuperObject);
    function GetTimeCost: integer;
    procedure UpdateHashes;
    procedure UpdateDataPath;
    procedure GetStatus;
    procedure GetLoadOrders;
    procedure SortPlugins;
    procedure Remove(plugin: TPlugin); overload;
    procedure Remove(pluginFilename: string); overload;
    function PluginsModified: boolean;
    function FilesExist: boolean;
    function GetStatusColor: integer;
  end;
  TMergeHelpers = class
    class function CreateNewMerge(var merges: TList): TMerge;
    class function GetMergeForPlugin(filename: string): string;
    class procedure AssignMergesToPlugins;
    class function MergeByName(var merges: TList; name: string): TMerge;
    class function MergeByFilename(var merges: TList; filename: string): TMerge;
  end;

  // Loading/Saving functions
  procedure HandleCanceled(msg: string);
  procedure RenameSavedPlugins;
  procedure LoadDictionary;
  procedure SaveMerges;
  procedure LoadMerges;
  procedure SavePluginInfo;
  procedure LoadPluginInfo;
  procedure SaveReports(var lst: TList; path: string);
  function ReportExistsFor(var plugin: TPlugin): boolean;
  procedure LoadReport(var report: TReport); overload;
  procedure LoadReport(const filename: string; var report: TReport); overload;
  // Helper functions
  function GetModContainingFile(var modlist: TStringList; filename: string): string;
  function GetFolderContainingFile(basePath: string; filename: string): string;
  function GetRatingColor(rating: real): integer;
  function GetEntry(pluginName, numRecords, version: string): TEntry;
  function IsBlacklisted(const filename: string): boolean;
  procedure UpdatePluginData;
  function CreateNewPlugin(sFilename: string): TPlugin;
  function PluginLoadOrder(sFilename: string): Integer;
  function PluginByFilename(sFilename: string): TPlugin;

const
  // IMPORTANT CONSTANTS
  ProgramTesters = 'bla08, hishy, Kesta, EphemeralSagacity';
  ProgramTranslators = 'GabenOurSavior, fiama, dhxxqk2010, Oaristys';
  xEditVersion = '3.1.1';
  bTranslationDump = false;

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
  StatusArray: array[0..15] of TMergeStatus = (
    ( id: msUnknown; color: $808080; desc: 'Unknown'; ),
    ( id: msUnloaded; color: $808080; desc: 'Plugins not loaded'; ),
    ( id: msNoPlugins; color: $0000FF; desc: 'No plugins to merge'; ),
    ( id: msDirInvalid; color: $0000FF; desc: 'Directories invalid'; ),
    ( id: msErrors; color: $0000FF; desc: 'Errors in plugins'; ),
    ( id: msCheckErrors; color: $0080ed; desc: 'Check for errors required'; ),
    ( id: msNotContiguous; color: $0080ed; desc: 'Plugins not contiguous'; ),
    ( id: msBreaksDependencies; color: $0080ed; desc: 'Merge breaks dependencies'; ),
    ( id: msUpToDate; color: $900000; desc: 'Up to date'; ),
    ( id: msUpToDateForced; color: $900000; desc: 'Up to date [Forced]'; ),
    ( id: msBuildReady; color: $009000; desc: 'Ready to be built'; ),
    ( id: msRebuildReady; color: $009000; desc: 'Ready to be rebuilt'; ),
    ( id: msRebuildReadyForced; color: $009000; desc: 'Ready to be rebuilt [Forced]'; ),
    ( id: msFailed; color: $000000; desc: 'Failed'; ),
    ( id: msBuilt; color: $000000; desc: 'Built'; ),
    ( id: msCanceled; color: $000000; desc: 'Canceled'; )
  );
  // STATUS TYPES
  ErrorStatuses = [msUnknown, msNoPlugins, msDirInvalid, msUnloaded, msErrors];
  UpToDateStatuses = [msUpToDate, msUpToDateForced];
  BuildStatuses = [msBuildReady, msRebuildReady, msRebuildReadyForced];
  RebuildStatuses = [msRebuildReady, msRebuildReadyForced];
  ForcedStatuses = [msUpToDateForced, msRebuildReadyForced];
  ResolveStatuses = [msNoPlugins, msDirInvalid, msUnloaded, msErrors,
    msNotContiguous, msBreaksDependencies, msCheckErrors];
  ImmutableStauses = [msUpToDateForced, msRebuildReadyforced, msFailed,
    msCanceled, msBuilt];

var
  Dictionary, Blacklist, MergesList: TList;
  pluginsToHandle, mergesToBuild: TList;
  ActiveMods: TStringList;
  ActiveModProfile, xEditLogGroup, xEditLogLabel, DictionaryFilename: string;

implementation

uses
  Windows, SysUtils, Dialogs,
  // mte units
  mteLogger, mteTracker, mteHelpers, mteLogging, RttiJson,
  // mp units
  mpConfiguration, mpClient,
  // xEdit units
  wbInterface, wbImplementation;

{ TPlugin Constructor }
constructor TPlugin.Create;
begin
  merge := ' ';
  errors := TStringList.Create;
  reports := TStringList.Create;
  inherited;
end;

destructor TPlugin.Destroy;
begin
  errors.Free;
  reports.Free;
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
procedure TPlugin.GetMpData;
var
  s: string;
begin
  hasData := true;
  GetFlags;

  // get reports
  entry := GetEntry(filename, numRecords, LocalStatus.programVersion);
  s := Trim(StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]));
  reports.Text := Wordwrap(s, 80);

  // get numOverrides if not blacklisted
  if not (IS_BLACKLISTED in flags) then
    numOverrides := IntToStr(CountOverrides(_File));

  // get data path and hash
  GetDataPath;
  GetHash;

  // call get data method
  GetData(PluginsList);
end;

procedure TPlugin.UpdateData;
var
  s: string;
begin
  // get reports
  entry := GetEntry(filename, numRecords, LocalStatus.ProgramVersion);
  s := Trim(StringReplace(entry.notes, '@13', #13#10, [rfReplaceAll]));
  reports.Text := Wordwrap(s, 80);

  // update blacklisted flag if it was blacklisted
  if IsBlacklisted(filename) then
    flags := [IS_BLACKLISTED];
end;

procedure TPlugin.GetDataPath;
var
  modName: string;
begin
  dataPath := wbDataPath;
  if settings.usingMO then begin
    modName := GetModContainingFile(ActiveMods, filename);
    if modName <> '' then
      dataPath := settings.ModsPath + modName + '\';
  end
  else if settings.usingNMM then begin
    modName := GetFolderContainingFile(settings.ModsPath, filename);
    if modName <> '' then
      dataPath := settings.ModsPath + modName + '\';
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
  prompt := Format(GetLanguageString('mpProg_SavePlugin'), [filename]);
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
  Result := Assigned(errors) and (errors.Count > 0);
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
  ignoredDependencies := TStringList.Create;
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
  ignoredDependencies.Free;
  inherited;
end;

{ Produces a dump of the merge. }
function TMerge.Dump: ISuperObject;
var
  obj: ISuperObject;
  i: integer;
begin
  obj := SO;

  // normal attributes
  obj.S['name'] := name;
  obj.S['filename'] := filename;
  obj.S['dateBuilt'] := DateTimeToStr(dateBuilt);
  obj.S['method'] := method;
  obj.S['renumbering'] := renumbering;
  obj.B['bIgnoreNonContiguous'] := bIgnoreNonContiguous;

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

  // files, log, ignored dependencies
  obj.O['files'] := SA([]);
  for i := 0 to Pred(files.Count) do
    obj.A['files'].S[i] := files[i];
  obj.O['fails'] := SA([]);
  for i := 0 to Pred(fails.Count) do
    obj.A['fails'].S[i] := fails[i];
  obj.O['ignoredDependencies'] := SA([]);
  for i := 0 to Pred(ignoredDependencies.Count) do
    obj.A['ignoredDependencies'].S[i] := ignoredDependencies[i];

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
  try
    bIgnoreNonContiguous := obj.AsObject.B['bIgnoreNonContiguous'];
  except on Exception do
    // nothing
  end;

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
  try
    for item in obj['ignoredDependencies'] do
      ignoredDependencies.Add(item.AsString);
  except on Exception do
    // nothing
  end;
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
  i, j, lastLoadOrder, currentLoadOrder: Integer;
  plugin: TPlugin;
  bBrokeDependencies: boolean;
begin
  if (status in ImmutableStauses) then
    exit;
  Logger.Write('MERGE', 'Status', name + ' -> Getting status');
  status := msUnknown;

  // don't merge if no plugins to merge
  if (plugins.Count < 1) then begin
    Logger.Write('MERGE', 'Status', name + ' -> No plugins to merge');
    status := msNoPlugins;
    exit;
  end;

  // don't merge if merge destination directory is blank
  if (settings.mergeDirectory = '') then begin
    Logger.Write('MERGE', 'Status', name + ' -> Merge directory blank');
    status := msDirInvalid;
    exit;
  end;

  // update the merge's datapath
  UpdateDataPath;

  // don't merge if usingMO is true and MODirectory is blank
  if settings.usingMO and (settings.ManagerPath = '') then begin
    Logger.Write('MERGE', 'Status', name + ' -> Mod Organizer Directory blank');
    status := msDirInvalid;
    exit;
  end;

  // don't merge if usingMO is true and MODirectory is invalid
  if settings.usingMO and not DirectoryExists(settings.ManagerPath) then begin
     Logger.Write('MERGE', 'Status', name + ' -> Mod Organizer Directory invalid');
     status := msDirInvalid;
     exit;
  end;

  // loop through plugins
  lastLoadOrder := -1;
  for i := 0 to Pred(plugins.Count) do begin
    plugin := PluginByFilename(plugins[i]);

    // see if plugin is loaded
    if not Assigned(plugin) then begin
      Logger.Write('MERGE', 'Status', name + ' -> Plugin '+plugins[i]+' is missing');
      if status = msUnknown then status := msUnloaded;
      continue;
    end;

    // check if plugins are contiguous
    currentLoadOrder := plugin._File.LoadOrder;
    if (lastLoadOrder > -1) and (not bIgnoreNonContiguous)
    and (currentLoadOrder - lastLoadOrder <> 1) then begin
      Logger.Write('MERGE', 'Status', name + ' -> Plugin '+plugins[i]+' is not contiguous');
      if status = msUnknown then status := msNotContiguous;
    end;
    lastLoadOrder := currentLoadOrder;

    // check if plugins break dependencies
    bBrokeDependencies := false;
    for j := 0 to Pred(plugin.requiredBy.Count) do begin
      if (plugins.IndexOf(plugin.requiredBy[j]) = -1)
      and (ignoredDependencies.IndexOf(plugin.requiredBy[j]) = -1) then begin
        if not bBrokeDependencies then
          Logger.Write('MERGE', 'Status', name + ' -> Plugin '+plugins[i]+' breaks dependencies with');
        bBrokeDependencies := true;
        Logger.Write('MERGE', 'Status', '  ' + plugin.requiredBy[j]);
        if status = msUnknown then status := msBreaksDependencies;
      end;
    end;

    // check if plugin has been checked for errors, and is error-free
    if (not plugin.HasBeenCheckedForErrors) then begin
      Logger.Write('MERGE', 'Status', name + ' -> '+plugin.filename+' needs to be checked for errors.');
      if status = msUnknown then status := msCheckErrors;
    end
    else if plugin.HasErrors and not plugin.bIgnoreErrors then begin
      Logger.Write('MERGE', 'Status', name + ' -> '+plugin.filename+' has errors');
      if status = msUnknown then status := msErrors;
    end;
  end;

  // check plugins were modified or files were deleted before
  // giving merge the up to date status
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

// Updates the data path to be used by the merge
procedure TMerge.UpdateDataPath;
begin
  // use the game's data path instead of using a subfolder
  // if the merge directory is the game's data path,
  if (settings.mergeDirectory = wbDataPath) then
    dataPath := wbDataPath
  else
    dataPath := settings.mergeDirectory + name + '\';
end;

// Get load order for plugins in merge that don't have it
procedure TMerge.GetLoadOrders;
var
  i, iLoadOrder: integer;
begin
  for i := 0 to Pred(plugins.Count) do
    if not Assigned(plugins.Objects[i]) then begin
      iLoadOrder := PluginLoadOrder(plugins[i]);
      plugins.Objects[i] := TObject(iLoadOrder);
    end;
end;

// Sort plugins by load order position
procedure TMerge.SortPlugins;
begin
  GetLoadOrders;
  plugins.CustomSort(LoadOrderCompare);
end;

procedure TMerge.Remove(plugin: TPlugin);
var
  index: Integer;
begin
  // clear plugin's merge property, if it's the name of this merge
  if plugin.merge = name then
    plugin.merge := ' ';
  // remove plugin from merge, if present
  index := plugins.IndexOf(plugin.filename);
  if index > -1 then
    plugins.Delete(index);
end;

procedure TMerge.Remove(pluginFilename: string);
var
  index: Integer;
begin
  index := plugins.IndexOf(pluginFilename);
  // remove plugin from merge, if present
  if index > -1 then
    plugins.Delete(index);
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


{}

class function TMergeHelpers.GetMergeForPlugin(filename: string): string;
var
  i: Integer;
  merge: TMerge;
begin
  Result := ' ';
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    if merge.plugins.IndexOf(filename) > -1 then begin
      Result := merge.name;
      break;
    end;
  end;
end;

class procedure TMergeHelpers.AssignMergesToPlugins;
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

{ Gets a merge matching the given name. }
class function TMergeHelpers.MergeByName(var merges: TList; name: string): TMerge;
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
class function TMergeHelpers.MergeByFilename(var merges: TList; filename: string): TMerge;
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
class function TMergeHelpers.CreateNewMerge(var merges: TList): TMerge;
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
  DictionaryFilename := wbAppName + 'Dictionary.txt';
  if not FileExists(DictionaryFilename) then begin
    Logger.Write('GENERAL', 'Dictionary', 'No dictionary file '+DictionaryFilename);
    exit;
  end;

  // load dictionary file
  sl := TStringList.Create;
  sl.LoadFromFile(DictionaryFilename);

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
  filename := PathList.Values['ProfilePath'] + 'Merges.json';
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
  filename := PathList.Values['ProfilePath'] + 'Merges.json';
  if not FileExists(filename) then
    exit;
  // load file into SuperObject to parse it
  sl := TStringList.Create;
  sl.LoadFromFile(filename);
  obj := SO(PChar(sl.Text));

  // loop through merges
  for mergeItem in obj['merges'] do begin
    merge := TMerge.Create;
    try
      merge.LoadDump(mergeItem);
      MergesList.Add(merge);
    except
      on x: Exception do begin
        Logger.Write('LOAD', 'Merge', 'Failed to load merge '+merge.name);
        Logger.Write('LOAD', 'Merge', x.Message);
      end;
    end;
  end;

  // finalize
  obj := nil;
  sl.Free;
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
  filename := PathList.Values['ProfilePath'] + 'PluginInfo.json';
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
  filename := PathList.Values['ProfilePath'] + 'PluginInfo.json';
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
  filename := PathList.Values['ProfilePath'] + 'PluginInfo.json';
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

function ReportExistsFor(var plugin: TPlugin): boolean;
var
  fn, unsubmittedPath, submittedPath: string;
begin
  fn := Format('%s-%s.txt', [plugin.filename, plugin.hash]);
  unsubmittedPath := 'reports\' + fn;
  submittedPath := 'reports\submitted\' + fn;
  Result := FileExists(unsubmittedPath) or FileExists(submittedPath);
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

procedure HandleCanceled(msg: string);
begin
  if Tracker.Cancel then
    raise Exception.Create(msg);
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

{}


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
    filePath := settings.ModsPath + modName + '\' + filename;
    if (FileExists(filePath)) then begin
      Result := modName;
      exit;
    end;
  end;
end;

function GetFolderContainingFile(basePath: string; filename: string): string;
var
  filePath: string;
  rec: TSearchRec;
begin
  Result := '';

  if FindFirst(basePath + '*', faDirectory, rec) <> 0 then
    exit;
  repeat
    filePath := basePath + '\' + rec.Name + '\' + filename;
    if FileExists(filePath) then begin
      Result := rec.Name;
      break;
    end;
  until FindNext(rec) <> 0;
  FindClose(rec);
end;

{}

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

function CreateNewPlugin(sFilename: string): TPlugin;
begin
  Result := TPlugin(TPluginHelpers.CreateNewBasePlugin(PluginsList, sFilename));
end;

function PluginLoadOrder(sFilename: string): Integer;
begin
  Result := TPluginHelpers.BasePluginLoadOrder(PluginsList, sFilename);
end;

function PluginByFilename(sFilename: string): TPlugin;
begin
  Result := TPlugin(TPluginHelpers.BasePluginByFilename(PluginsList, sFilename));
end;

initialization
begin
  MergesList := TList.Create;
end;

finalization
begin
  FreeList(MergesList);
end;

end.
