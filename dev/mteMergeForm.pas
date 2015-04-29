unit mteMergeForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList,
  ShlObj, Menus, IdHashMessageDigest,
  wbBSA,
  wbHelpers,
  wbInterface,
  wbImplementation,
  wbDefinitionsFNV,
  wbDefinitionsFO3,
  wbDefinitionsTES3,
  wbDefinitionsTES4,
  wbDefinitionsTES5, Grids, ValEdit;

type
  TMergeForm = class(TForm)
    ButtonPanel: TPanel;
    NewButton: TSpeedButton;
    BuildButton: TSpeedButton;
    ReportButton: TSpeedButton;
    DictionaryButton: TSpeedButton;
    OptionsButton: TSpeedButton;
    UpdateButton: TSpeedButton;
    HelpButton: TSpeedButton;
    MainPanel: TPanel;
    Splitter: TSplitter;
    DetailsPanel: TPanel;
    PageControl: TPageControl;
    PluginsTabSheet: TTabSheet;
    MergesTabSheet: TTabSheet;
    LogTabSheet: TTabSheet;
    XPManifest: TXPManifest;
    Memo1: TMemo;
    IconList: TImageList;
    StatusBar: TStatusBar;
    PluginsListView: TListView;
    DetailsLabel: TLabel;
    PluginsPopupMenu: TPopupMenu;
    Addtomerge1: TMenuItem;
    Removefrommerge1: TMenuItem;
    Report1: TMenuItem;
    MergesPopupMenu: TPopupMenu;
    Createnewmerge1: TMenuItem;
    Deletemerge1: TMenuItem;
    Rebuildmerge1: TMenuItem;
    Reportonmerge1: TMenuItem;
    NewMerge1: TMenuItem;
    MergeListView: TListView;
    DetailsEditor: TValueListEditor;
    FlagList: TImageList;
    procedure LogMessage(s: string);
    procedure FormCreate(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure NewButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    function AddDetailsItem(name, value: string; editable: boolean = false):
      TItemProp;
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MergeListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure NewMerge1Click(Sender: TObject);
    procedure UpdateMerges;
    procedure AddToMergeClick(Sender: TObject);
    procedure DetailsEditorSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TMerge = class(TObject)
    public
      name: string;
      filename: string;
      dateBuilt: TDateTime;
      plugins: TStringList;
      hashes: TStringList;
      masters: TStringList;
      method: string;
      renumbering: string;
      log: string;
      constructor Create; virtual;
  end;
  TPluginFlag = (IS_BLACKLISTED, HAS_ERRORS, HAS_BSA, HAS_MCM, HAS_FACEDATA,
    HAS_VOICEDATA, HAS_FRAGMENTS);
  TPluginFlags = set of TPluginFlag;
  TPlugin = class(TObject)
    public
      pluginFile: IwbFile;
      hasData: boolean;
      flags: TPluginFlags;
      filename: string;
      numRecords: string;
      numOverrides: string;
      author: string;
      description: string;
      masters: TStringList;
      errors: TStringList;
      reports: TStringList;
      hash: string;
      constructor Create; virtual;
      procedure GetData(bCountOverrides: boolean);
      procedure GetFlags;
      function GetFlagsString: string;
  end;

var
  MergeForm: TMergeForm;
  pluginObjects: TList;
  merges: TList;
  currentMerge: TMerge;
  blacklist: TStringList;
  time: TDateTime;


implementation

{$R *.dfm}

constructor TMerge.Create;
begin
  name := 'NewMerge';
  filename := 'NewMerge.esp';
  dateBuilt := 0;
  plugins := TStringList.Create;
  masters := TStringList.Create;
  hashes := TStringList.Create;
  method := 'Overrides';
  renumbering := 'Conflicting';
  log := '';
end;

constructor TPlugin.Create;
begin
  hasData := false;
  masters := TStringList.Create;
  errors := TStringList.Create;
  reports := TStringList.Create;
end;

{
  Returns true if the input record is an override record
}
function IsOverride(aRecord: IwbMainRecord): boolean;
begin
  Result := not aRecord.Equals(aRecord.MasterOrSelf);
end;

{
  Returns the number of override records in a file
}
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

{
  Replaces newlines with a comma and space
}
function csvText(s: string): string;
begin
  result := StringReplace(Trim(s), #13, ', ', [rfReplaceAll]);
end;

{
  Recursively traverse a container looking for errors
}
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
  Error: string;
  Container: IwbContainerElementRef;
  i: Integer;
begin
  Error := aElement.Check;
  Result := Error <> '';
  if Result then begin
    Error := aElement.Check;
    errors.Add(StringOfChar(' ', aIndent * 2) + aElement.Name + ' -> ' + Error);
  end else
    errors.Add('');

  // recursion
  if Supports(aElement, IwbContainerElementRef, Container) then
    for i := Pred(Container.ElementCount) downto 0 do
      Result := CheckForErrors(aIndent + 1, Container.Elements[i], errors) or Result;

  if Result and (Error = '') then begin
    errors.Add(StringOfChar(' ', aIndent * 2) + 'Above errors were found in :' + aElement.Name);
  end;
end;

{
  Gets the MD5 hash for a file
}
function MD5(const fileName : string) : string;
var
  idmd5 : TIdHashMessageDigest5;
  sl: TStringList;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(fileName);
    Result := idmd5.HashStringAsHex(fileName, nil);
  finally
    sl.Free;
    idmd5.Free;
  end;
end;

{
  Gets the masters in an IwbFile and puts them into a stringlist
}
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

{
  Checks if a BSA exists associated with the given filename
}
function BSAExists(filename: string): boolean;
begin
  if (Pos('.esp', filename) > 0)
  or (Pos('.esm', filename) > 0) then
    filename := Copy(filename, 1, Length(filename) - 4) + '.bsa'
  else
    filename := filename + '.bsa';
  Result := FileExists(wbDataPath + filename);
end;

function MCMExists(filename: string): boolean;
begin
  Result := false;
end;

function FaceDataExists(filename: string): boolean;
var
  facetint, facegeom: boolean;
begin
  facetint := DirectoryExists(wbDataPath + 'textures\actors\character\facegendata\facetint\' + filename);
  facegeom := DirectoryExists(wbDataPath + 'meshes\actors\character\facegendata\facegeom\' + filename);
  Result := facetint or facegeom;
end;

function VoiceDataExists(filename: string): boolean;
begin
  Result := DirectoryExists(wbDataPath + 'sound\voice\' + filename);
end;

function FragmentsExist(filename: string): boolean;
begin
  Result := false;
end;

{
  Gets the flag values for a TPlugin
}
procedure TPlugin.GetFlags;
begin
  if blacklist.IndexOf(filename) > -1 then begin
    flags := flags + [IS_BLACKLISTED];
    exit;
  end;
  if errors.Count > 0 then
    flags := flags + [HAS_ERRORS];
  if BSAExists(filename) then
    flags := flags + [HAS_BSA];
  if MCMExists(filename) then
    flags := flags + [HAS_MCM];
  if FaceDataExists(filename) then
    flags := flags + [HAS_FACEDATA];
  if VoiceDataExists(filename) then
    flags := flags + [HAS_VOICEDATA];
  if FragmentsExist(filename) then
    flags := flags + [HAS_FRAGMENTS];
end;

{
  Returns a string representing the flags in a plugin
}
function TPlugin.GetFlagsString: string;
begin
  Result := '';
  if IS_BLACKLISTED in flags then
    Result := Result + 'X';
  if HAS_ERRORS in flags then
    Result := Result + 'E';
  if HAS_BSA in flags then
    Result := Result + 'A';
end;

{
  Fetches data associated with a plugin.
}
procedure TPlugin.GetData(bCountOverrides: boolean);
var
  Container: IwbContainer;
begin
  hasData := true;
  filename := pluginFile.FileName;
  Container := pluginFile as IwbContainer;
  Container := Container.Elements[0] as IwbContainer;
  author := Container.GetElementEditValue('CNAM - Author');
  numRecords := Container.GetElementEditValue('HEDR - Header\Number of Records');
  description := Container.GetElementEditValue('SNAM - Description');
  GetMasters(pluginFile, masters);
  //CheckForErrors(0, pluginFile as IwbElement, errors);
  GetFlags;
  hash := MD5(wbDataPath + filename);

  if bCountOverrides then
    numOverrides := IntToStr(CountOverrides(pluginFile));
end;

{
  Prints a message to the log memo
}
procedure TMergeForm.LogMessage(s: string);
begin
  Memo1.Lines.Add(s);
end;

{
  Gets a folder by its integer CSID.
}
function GetCSIDLShellFolder(CSIDLFolder: integer): string;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(0, PChar(Result), CSIDLFolder, True);
  SetLength(Result, StrLen(PChar(Result)));
  if (Result <> '') then
    Result := IncludeTrailingBackslash(Result);
end;

{
  Remove comments and empty lines from a stringlist
}
procedure RemoveCommentsAndEmpty(sl: TStrings);
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

{
  Remove missing files from stringlist
}
procedure RemoveMissingFiles(sl: TStrings);
var
  i: integer;
begin
  for i := Pred(sl.Count) downto 0 do
    if not FileExists(wbDataPath + sl.Strings[i]) then
      sl.Delete(i);
end;

{
  Add missing *.esp and *.esm files to list
}
procedure AddMissingToLoadList(sl: TStrings);
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

{
  Initialize form, initialize TES5Edit API, and load plugins
}
procedure TMergeForm.FormCreate(Sender: TObject);
var
  wbPluginsFileName: string;
  sl: TStringList;
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // INITIALIZE LISTS
  merges := TList.Create;
  pluginObjects := TList.Create;
  blacklist := TStringList.Create;
  blacklist.Add('Skyrim.esm');
  blacklist.Add('Update.esm');
  blacklist.Add('Dawnguard.esm');
  blacklist.Add('HearthFires.esm');
  blacklist.Add('Dragonborn.esm');

  // GUI ICONS
  IconList.GetBitmap(0, NewButton.Glyph);
  IconList.GetBitmap(1, BuildButton.Glyph);
  IconList.GetBitmap(2, ReportButton.Glyph);
  IconList.GetBitmap(3, DictionaryButton.Glyph);
  IconList.GetBitmap(4, OptionsButton.Glyph);
  IconList.GetBitmap(5, UpdateButton.Glyph);
  IconList.GetBitmap(6, HelpButton.Glyph);

  // INITIALIZE TES5EDIT API
  wbGameMode := gmTES5;
  wbAppName := 'TES5';
  wbGameName := 'Skyrim';
  wbDataPath := 'C:\Program Files (x86)\Steam\steamapps\common\Skyrim\Data\';
  wbPluginsFileName := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA);
  wbPluginsFileName := wbPluginsFileName + wbGameName + '\Plugins.txt';
  DefineTES5;

  // LOAD PLUGINS
  sl := TStringList.Create;
  sl.LoadFromFile(wbPluginsFileName);
  RemoveCommentsAndEmpty(sl);
  if wbGameMode = gmTES5 then begin
    if sl.IndexOf('Update.esm') = -1 then
      sl.Insert(0, 'Update.esm');
    if sl.IndexOf(wbGameName+'.esm') = -1 then
      sl.Insert(0, wbGameName+'.esm');
  end;
  RemoveMissingFiles(sl);
  AddMissingToLoadList(sl);
  for i := 0 to Pred(sl.Count) do begin
    plugin := TPlugin.Create;
    plugin.filename := sl[i];
    plugin.pluginFile := wbFile(wbDataPath + sl[i], i);
    plugin.GetFlags;
    pluginObjects.Add(Pointer(plugin));

    ListItem := PluginsListView.Items.Add;
    ListItem.Caption := IntToHex(i, 2);
    ListItem.SubItems.Add(plugin.filename);
    ListItem.SubItems.Add(plugin.GetFlagsString);
  end;


  // CLEAN UP -
  sl.Free;
end;

{
   Adds a ListItem to DetailsView with @name and @value
}
function TMergeForm.AddDetailsItem(name, value: string;
  editable: boolean = false): TItemProp;
var
  prop: TItemProp;
begin
  DetailsEditor.InsertRow(name, value, true);
  prop := DetailsEditor.ItemProps[DetailsEditor.RowCount - 1];
  prop.ReadOnly := not editable;
  Result := prop;
end;

{
  Loads plugin details
}
procedure TMergeForm.PluginsListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  plugin: TPlugin;
  index: integer;
begin
  // don't do anything if no item selected
  if not Assigned(PluginsListView.Selected) then
    exit;

  // prepare list view for plugin information
  DetailsEditor.OnSetEditText := nil;
  DetailsEditor.Strings.Clear;
  DetailsEditor.Options := DetailsEditor.Options - [goEditing];
  DetailsLabel.Caption := 'Plugin Details:';

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(pluginObjects[index]);
  if not plugin.hasData then plugin.GetData(index > 0);

  // add details items
  AddDetailsItem('Filename', plugin.filename);
  AddDetailsItem('Hash', plugin.hash);
  AddDetailsItem('Flags', plugin.GetFlagsString);
  AddDetailsItem('Number of records', plugin.numRecords);
  AddDetailsItem('Number of overrides', plugin.numOverrides);
  AddDetailsItem('Author', plugin.author);
  AddDetailsItem('Description', plugin.description);
  AddDetailsItem('Master files', csvText(plugin.masters.Text));
  AddDetailsItem('Errors', csvText(plugin.errors.Text));
  AddDetailsItem('Reports', csvText(plugin.reports.Text));
end;

{
  Converts a TDateTime to a string, with 0 being the string 'Never'
}
function DateBuiltString(date: TDateTime): string;
begin
  if date = 0 then
    Result := 'Never'
  else begin
    Result := DateTimeToStr(date);
  end;
end;

{
  Create new merge
}
procedure TMergeForm.NewButtonClick(Sender: TObject);
var
  merge: TMerge;
begin
  Memo1.Lines.Add('Created new merge!');

  merge := TMerge.Create;
  merges.Add(merge);
  UpdateMerges;
end;

{
  Update merges in GUI
}
procedure TMergeForm.UpdateMerges;
var
  i: Integer;
  merge: TMerge;
  AddToMerge, MenuItem: TMenuItem;
  ListItem: TListItem;
begin
  // clear popup menu and list view
  AddToMerge := PluginsPopupMenu.Items[0];
  MergeListView.Items.Clear;
  AddToMerge.Clear;

  // add <New Merge> option to Plugins popup menu
  MenuItem := TMenuItem.Create(AddToMerge);
  MenuItem.Caption := '<New Merge>';
  MenuItem.OnClick := NewMerge1Click;
  AddToMerge.Add(MenuItem);

  // add merges to list view and plugins popup menu
  for i := 0 to Pred(merges.Count) do begin
    merge := TMerge(merges[i]);
    MenuItem := TMenuItem.Create(AddToMerge);
    MenuItem.Caption := merge.name;
    MenuItem.OnClick := AddToMergeClick;
    AddToMerge.Add(MenuItem);

    ListItem := MergeListView.Items.Add;
    ListItem.Caption := IntToHex(MergeListView.Items.Count, 2);
    ListItem.SubItems.Add(merge.name);
    ListItem.SubItems.Add(merge.filename);
    ListItem.SubItems.Add(IntToStr(merge.plugins.Count));
    ListItem.SubItems.Add(DateBuiltString(merge.dateBuilt));
  end;
end;

{
  Add selected plugins to existing merge
}
procedure TMergeForm.AddToMergeClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  merge: TMerge;
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  MenuItem := TMenuItem(Sender);
  merge := TMerge(merges[MenuItem.MenuIndex - 1]);

  // loop through plugins list, adding selected plugins to merge
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(pluginObjects[i]);
    if not plugin.hasData then
      plugin.GetData(true);
    merge.plugins.Add(plugin.filename);
    merge.hashes.Add(plugin.hash);
  end;

  // update gui
  UpdateMerges;
end;

{
  Add selected plugins to a new merge
}
procedure TMergeForm.NewMerge1Click(Sender: TObject);
var
  merge: TMerge;
  plugin: TPlugin;
  i: Integer;
  ListItem: TListItem;
begin
  merge := TMerge.Create;
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(pluginObjects[i]);
    merge.plugins.Add(plugin.filename);
    merge.hashes.Add(plugin.hash);
  end;
  merges.Add(merge);
  UpdateMerges;
end;

{
  Rebuild merges
}
procedure TMergeForm.BuildButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(TButton(Sender).Hint+' clicked!');
end;

{
  Submit report
}
procedure TMergeForm.ReportButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(TButton(Sender).Hint+' clicked!');
end;

{
  Update merge
}
procedure TMergeForm.DetailsEditorSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  // update merge
  currentMerge.name := DetailsEditor.Values['Merge name'];
  currentMerge.filename := DetailsEditor.Values['Filename'];
  currentMerge.method := DetailsEditor.Values['Merge method'];
  currentMerge.renumbering := DetailsEditor.Values['Renumbering'];

  UpdateMerges;
end;

{
  View the dictionary file
}
procedure TMergeForm.DictionaryButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(TButton(Sender).Hint+' clicked!');
end;

{
  Options
}
procedure TMergeForm.OptionsButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(TButton(Sender).Hint+' clicked!');
end;

{
  Update
}
procedure TMergeForm.UpdateButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(TButton(Sender).Hint+' clicked!');
end;

{
  Help
}
procedure TMergeForm.HelpButtonClick(Sender: TObject);
begin
  Memo1.Lines.Add(TButton(Sender).Hint+' clicked!');
end;

{
  Merge selected
}
procedure TMergeForm.MergeListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
var
  mergeItem: TListItem;
  merge: TMerge;
  prop: TItemProp;
begin
  // don't do anything if no item selected
  mergeItem := MergeListView.Selected;
  if not Assigned(mergeItem) then
    exit;

  // prepare list view for merge information
  DetailsEditor.OnSetEditText := nil;
  DetailsEditor.Strings.Clear;
  DetailsEditor.Options := DetailsEditor.Options + [goEditing];
  DetailsLabel.Caption := 'Merge Details:';

  // get merge information
  merge := merges[MergeListView.ItemIndex];
  currentMerge := merge;
  AddDetailsItem('Merge name', merge.name, true);
  AddDetailsItem('Filename', merge.filename, true);
  AddDetailsItem('Plugin count', IntToStr(merge.plugins.Count));
  AddDetailsItem('Date built', DateBuiltString(merge.dateBuilt));
  AddDetailsItem('Plugins', csvText(merge.plugins.Text));
  AddDetailsItem('Masters', csvText(merge.masters.Text));
  AddDetailsItem(' ', ' ');
  prop := AddDetailsItem('Merge method', merge.method, false);
  prop.EditStyle := esPickList;
  prop.PickList.Add('Overrides');
  prop.PickList.Add('New records');
  prop := AddDetailsItem('Renumbering', merge.renumbering, false);
  prop.EditStyle := esPickList;
  prop.PickList.Add('Conflicting');
  prop.PickList.Add('All');
  AddDetailsItem('Log', merge.log);

  // return event
  DetailsEditor.OnSetEditText := DetailsEditorSetEditText;
end;

end.
