unit mpMergeForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList,
  Menus, Grids, ValEdit, ShlObj, ShellAPI,
  // third party libraries
  superobject,
  // mp units
  mpBase, mpMerge, mpLogger, mpDictionaryForm, mpOptionsForm, mpProgressForm,
  mpTracker, mpSplashForm, mpEditForm, mpGameForm,
  // tes5edit units
  wbBSA, wbHelpers, wbInterface, wbImplementation;

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
    AddToMerge: TMenuItem;
    RemoveFromMerge: TMenuItem;
    ReportOnPlugin: TMenuItem;
    MergesPopupMenu: TPopupMenu;
    CreateNewMergeItem: TMenuItem;
    DeleteMergeItem: TMenuItem;
    BuildMergeItem: TMenuItem;
    NewMerge: TMenuItem;
    MergeListView: TListView;
    DetailsEditor: TValueListEditor;
    FlagList: TImageList;
    CheckforErrorsItem: TMenuItem;
    ForceRebuildItem: TMenuItem;
    ReportOnMergeItem: TMenuItem;
    OpenInExplorerItem: TMenuItem;
    IgnoreRebuildItem: TMenuItem;
    EditMergeItem: TMenuItem;
    DoubleIconList: TImageList;
    CheckPluginsForErrorsItem: TMenuItem;
    procedure LogMessage(const s: string);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure RebuildButtonClick(Sender: TObject);
    procedure CreateMergeButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    function AddDetailsItem(name, value: string; editable: boolean = false):
      TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList; editable: boolean = false);
    procedure UpdatePluginDetails;
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    function FlagNotSafe(Rect: TRect; x: integer): boolean;
    procedure DrawFlag(canvas: TCanvas; flags: string; index, x, y: integer);
    procedure DrawPluginFlags(canvas: TCanvas; Rect: TRect; x, y: integer; flags: string);
    procedure PluginsListViewData(Sender: TObject; Item: TListItem);
    procedure PluginsListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure UpdateMergeDetails;
    procedure MergeListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure UpdateMerges;
    procedure AddToNewMergeClick(Sender: TObject);
    procedure UpdatePluginsPopupMenu;
    procedure AddToMergeClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure CheckForErrorsClick(Sender: TObject);
    procedure PluginsPopupMenuPopup(Sender: TObject);
    procedure RemoveFromMergeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MergeListViewData(Sender: TObject; Item: TListItem);
    procedure MergeListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure DeleteMergeItemClick(Sender: TObject);
    procedure MergesPopupMenuPopup(Sender: TObject);
    procedure OpenInExplorerItemClick(Sender: TObject);
    procedure ForceRebuildItemClick(Sender: TObject);
    procedure IgnoreRebuildItemClick(Sender: TObject);
    procedure ReportOnMergeItemClick(Sender: TObject);
    procedure BuildMergeItemClick(Sender: TObject);
    procedure EditMergeItemClick(Sender: TObject);
    procedure CheckPluginsForErrorsItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MergeForm: TMergeForm;
  currentMerge: TMerge;
  bFirstActivation: boolean;

implementation

{$R *.dfm}


{******************************************************************************}
{ Merge Form Events
  Events for the Merge Form.
  - LogMessage
  - ProgressMessage
  - FormCreate
  - FormClose
}
{******************************************************************************}

{ Prints a message to the log memo }
procedure TMergeForm.LogMessage(const s: string);
begin
  Memo1.Lines.Add(s);
  SendMessage(Memo1.Handle, EM_LINESCROLL, 0, Memo1.Lines.Count);
end;

procedure ProgressMessage(const s: string);
begin
  if s = '' then
    exit;
  MergeForm.LogMessage(s);
  if Assigned(tracker.OnLogEvent) and bCaptureTracker then
    Tracker.Write(s);
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TMergeForm.FormCreate(Sender: TObject);
var
  wbPluginsFileName: string;
  sl: TStringList;
  i: integer;
  plugin: TPlugin;
  time: TDateTime;
  aFile: IwbFile;
  splash: TSplashForm;
begin
  try
    // CREATE SPLASH
    splash := TSplashForm.Create(nil);
    splash.Show;

    // INITIALIZE VARIABLES
    time := now;
    tempPath := wbProgramPath + 'temp\';
    logPath := wbProgramPath + 'logs\';
    ForceDirectories(tempPath);
    MergesList := TList.Create;
    PluginsList := TList.Create;
    bFirstActivation := true;

    // GUI ICONS
    Tracker.Write('Loading Icons');
    IconList.GetBitmap(0, NewButton.Glyph);
    DoubleIconList.GetBitmap(0, BuildButton.Glyph);
    IconList.GetBitmap(2, ReportButton.Glyph);
    IconList.GetBitmap(3, DictionaryButton.Glyph);
    IconList.GetBitmap(4, OptionsButton.Glyph);
    IconList.GetBitmap(5, UpdateButton.Glyph);
    IconList.GetBitmap(6, HelpButton.Glyph);

    // INITIALIZE TES5EDIT API
    wbDisplayLoadOrderFormID := True;
    wbSortSubRecords := True;
    wbDisplayShorterNames := True;
    wbHideUnused := True;
    wbFlagsAsArray := True;
    wbRequireLoadOrder := True;
    wbLanguage := 'English';
    wbEditAllowed := True;
    wbProgressCallback := ProgressMessage;
    Logger.OnLogEvent := LogMessage;
    handler := wbCreateContainerHandler;
    handler._AddRef;

    // SET GAME VARS
    if settings.selectedGame = 0 then
      if settings.defaultGame <> 0 then
        settings.selectedGame := settings.defaultGame
      else
        raise Exception.Create('Invalid game selection!');
    SetGame(settings.selectedGame);
    Logger.Write('Selected game: '+wbGameName);
    Logger.Write('Using data path: '+wbDataPath);

    // LOAD SETTINGS FOR GAME
    LoadSettings;
    if settings.usingMO then
      ModOrganizerInit;

    // LOAD DICTIONARY
    dictionaryFilename := wbAppName+'Dictionary.txt';
    Logger.Write('Loading dictionary '+dictionaryFilename);
    LoadDictionary;

    // LOAD TES5EDIT DEFINITIONS
    Logger.Write('Loading '+wbAppName+'Edit Definitions');
    LoadDefinitions;

    // PREPARE TO LOAD PLUGINS
    if settings.usingMO then
      wbPluginsFileName := settings.MODirectory + 'profiles\'+ActiveProfile+'\plugins.txt'
    else
      wbPluginsFileName := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA) + wbGameName + '\Plugins.txt';
    Logger.Write('Using load order '+wbPluginsFileName);
    sl := TStringList.Create;
    sl.LoadFromFile(wbPluginsFileName);
    RemoveCommentsAndEmpty(sl);
    RemoveMissingFiles(sl);
    // if GameMode is not Skyrim sort by date modified
    // else add Update.esm and Skyrim.esm to load order
    if wbGameMode <> gmTES5 then begin
      GetPluginDates(sl);
      sl.CustomSort(PluginListCompare);
    end
    else begin
      if sl.IndexOf('Update.esm') = -1 then
        sl.Insert(0, 'Update.esm');
      if sl.IndexOf('Skyrim.esm') = -1 then
        sl.Insert(0, 'Skyrim.esm');
    end;
    // debug message
    if settings.debugLoadOrder then begin
      Logger.Write('Load order: ');
      for i := 0 to Pred(sl.Count) do
        Logger.Write('  ['+IntToHex(i, 2)+'] '+sl[i]);
    end;


    // LOAD PLUGINS
    for i := 0 to Pred(sl.Count) do begin
      Tracker.Write('Loading '+sl[i]);
      plugin := TPlugin.Create;
      plugin.filename := sl[i];
      plugin._File := wbFile(wbDataPath + sl[i], i);
      plugin._File._AddRef;
      plugin.GetData;
      PluginsList.Add(Pointer(plugin));

      // load hardcoded dat
      if i = 0 then begin
        aFile := wbFile(wbProgramPath + wbGameName + wbHardcodedDat, 0);
        aFile._AddRef;
      end;
    end;

    // BUILD REFERENCE TABLE
    for i := 1 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[i]);
      Tracker.Write('Building references for '+plugin.filename);
      Logger.Write('Building references for '+plugin.filename);
      plugin._File.BuildRef;
    end;

    // LOAD MERGES
    Tracker.Write('Loading Merges');
    LoadMerges;
    UpdateMerges;
    UpdatePluginsPopupMenu;

    // FINALIZE
    sl.Free;
    PluginsListView.OwnerDraw := not settings.simplePluginsView;
    PluginsListView.Items.Count := PluginsList.Count;
    PluginsListView.Columns[1].AutoSize := true;
    Sleep(250);
    splash.Free;

    // PRINT TIME
    time := (Now - time) * 86400;
    LogMessage(FormatFloat('0.###', time) + ' spent loading.');
  except on x: Exception do
    LogMessage(x.Message);
  end;
end;

// Force PluginsListView to autosize columns
procedure TMergeForm.FormActivate(Sender: TObject);
begin
  if bFirstActivation then begin
    PageControl.ActivePageIndex := 1;
    MergeListView.Width := MergeListView.Width + 1;
    PageControl.ActivePageIndex := 0;
    PluginsListView.Width := PluginsListView.Width + 1;
    bFirstActivation := false;
  end;
end;

procedure TMergeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not bDontSave then SaveMerges;
  DeleteDirectory(tempPath);
  Action := caFree;
end;

{******************************************************************************}
{ Details Editor Events
  Methods for helping with the DetailsEditor control.  Methods include:
  - AddDetailsItem
  - AddDetailsList
  - PageControlChange
}
{******************************************************************************}

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
  Add one or more ListItem to DetailsView with @name and the values
  in @sl
}
procedure TMergeForm.AddDetailsList(name: string; sl: TStringList;
  editable: boolean = false);
var
  i: integer;
begin
  if sl.Count > 0 then begin
    AddDetailsItem(name, sl[0], editable);
    for i := 1 to Pred(sl.Count) do
      AddDetailsItem(' ', sl[i], editable);
  end
  else
    AddDetailsItem(name, ' ', editable);
end;

{
  Switch details view when page control is changed
}
procedure TMergeForm.PageControlChange(Sender: TObject);
var
  ndx: integer;
begin
  ndx := TPageControl(Sender).ActivePageIndex;
  if ndx = 0 then
    PluginsListViewChange(nil, nil, TItemChange(nil))
  else if ndx = 1 then
    UpdateMergeDetails;
end;

{******************************************************************************}
{ Plugins List View Events
  Events involving the PluginsListView control.  Events include:
  - UpdatePluginDetails
  - PluginsListViewChange
  - AddToMerge
  - AddToNewMerge
  - CheckPluginForErrors
}
{******************************************************************************}

procedure TMergeForm.UpdatePluginDetails;
var
  plugin: TPlugin;
  index: integer;
begin
  // don't do anything if no item selected
  if not Assigned(PluginsListView.Selected) then
    exit;

  // prepare list view for plugin information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Plugin Details:';

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetData;

  // add details items
  AddDetailsItem('Filename', plugin.filename);
  AddDetailsItem('Hash', plugin.hash);
  AddDetailsItem('File size', FormatByteSize(plugin.fileSize));
  AddDetailsItem('Date modified', plugin.dateModified);
  AddDetailsItem('Merge rating', plugin.entry.rating);
  AddDetailsItem('Flags', plugin.GetFlagsString);
  AddDetailsItem('Number of records', plugin.numRecords);
  AddDetailsItem('Number of overrides', plugin.numOverrides);
  AddDetailsItem('Author', plugin.author);
  AddDetailsList('Description', plugin.description);
  AddDetailsList('Masters', plugin.masters);
  AddDetailsList('Errors', plugin.errors);
  AddDetailsList('Reports', plugin.reports);
end;

procedure TMergeForm.PluginsListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdatePluginDetails;
end;

procedure TMergeForm.PluginsListViewData(Sender: TObject; Item: TListItem);
var
  plugin: TPlugin;
begin
  plugin := TPlugin(PluginsList[Item.Index]);
  Item.Caption := IntToHex(Item.Index, 2);
  Item.SubItems.Add(plugin.filename);
  Item.SubItems.Add(plugin.GetFlagsString);
  Item.SubItems.Add(plugin.merge);
  PluginsListView.Canvas.Font.Color := GetRatingColor(StrToFloatDef(plugin.entry.rating, -2.0));
  PluginsListView.Canvas.Font.Style := PluginsListView.Canvas.Font.Style + [fsBold];
end;

{ True if the flag can't be drawn without colliding with next column }
function TMergeForm.FlagNotSafe(Rect: TRect; x: integer): boolean;
begin
  Result := Rect.Right < x + 20;
end;

{ Draws the flag icon at @index if it is in @flags on @canvas at @x and @y}
procedure TMergeForm.DrawFlag(canvas: TCanvas; flags: string; index, x, y: integer);
var
  icon: TIcon;
begin
  icon := TIcon.Create;
  FlagList.GetIcon(index, icon);
  canvas.Draw(x, y, icon);
  icon.Free;
end;

{ Draws the icons for @flags on @canvas in @Rect at @x and @y }
procedure TMergeForm.DrawPluginFlags(canvas: TCanvas; Rect: TRect; x, y: integer; flags: string);
var
  i: integer;
begin
  for i := 0 to 6 do begin
    if FlagNotSafe(Rect, x) then
      exit;
    if Pos(flagChar[i + 1], flags) > 0 then begin
      DrawFlag(canvas, flags, i, x, y);
      x := x + 17;
    end;
  end;
end;

procedure TMergeForm.PluginsListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  R := Rect;
  R.Right := R.Left + ListView.Columns[0].Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  ListView.Canvas.TextRect(R, x, y, Item.Caption);
  for i := 0 to Item.SubItems.Count - 1 do begin
    R.Left := R.Right + 3;
    R.Right := R.Left + ListView.Columns[i + 1].Width;
    x := R.Left;
    if ListView.Columns[i + 1].Caption = 'Flags' then
      DrawPluginFlags(ListView.Canvas, R, x, y, Item.SubItems[i])
    else
      ListView.Canvas.TextRect(R, x, y, Item.SubItems[i]);
  end;
end;


{******************************************************************************}
{ PluginsPopupMenu methods
  Methods for dealing with the popup menu for the PluginsListView.
  - PluginsPopupMenuPopup
  - UpdatePluginsPopupMenu
  - AddToMergeClick
  - AddToNewMergeClick
  - CheckForErrorsClick
  - RemoveFromMergeClick
}
{******************************************************************************}

procedure TMergeForm.PluginsPopupMenuPopup(Sender: TObject);
var
  i: integer;
  pluginInMerge, blacklisted: boolean;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  blacklisted := false;
  pluginInMerge := false;
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    plugin := PluginsList[i];
    if IS_BLACKLISTED in plugin.flags then
      blacklisted := true;
    if plugin.merge <> ' ' then
      pluginInMerge := true;
  end;

  PluginsPopupMenu.Items[0].Enabled := (not blacklisted) and (not pluginInMerge);
  PluginsPopupMenu.Items[1].Enabled := (not blacklisted) and pluginInMerge;
  PluginsPopupMenu.Items[2].Enabled := not blacklisted;
end;

procedure TMergeForm.UpdatePluginsPopupMenu;
var
  i: Integer;
  merge: TMerge;
  AddToMergeItem, MenuItem: TMenuItem;
begin
  // clear popup menu
  AddToMergeItem := PluginsPopupMenu.Items[0];
  AddToMergeItem.Clear;

  // add <New Merge> option to Plugins popup menu
  MenuItem := TMenuItem.Create(AddToMergeItem);
  MenuItem.Caption := '<New Merge>';
  MenuItem.OnClick := AddToNewMergeClick;
  AddToMergeItem.Add(MenuItem);

  // add merges to plugins popup menu
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    MenuItem := TMenuItem.Create(AddToMergeItem);
    MenuItem.Caption := merge.name;
    MenuItem.OnClick := AddToMergeClick;
    AddToMergeItem.Add(MenuItem);
  end;
end;

procedure TMergeForm.AddToMergeClick(Sender: TObject);
var
  MenuItem: TMenuItem;
  merge: TMerge;
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  MenuItem := TMenuItem(Sender);
  merge := TMerge(MergesList[MenuItem.MenuIndex - 1]);

  // loop through plugins list, adding selected plugins to merge
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    if not plugin.hasData then
      plugin.GetData;
    merge.plugins.AddObject(plugin.filename, TObject(i));
    plugin.merge := merge.name;
  end;

  // update
  PluginsListView.Repaint;
  UpdateMerges;
end;

procedure TMergeForm.AddToNewMergeClick(Sender: TObject);
var
  merge: TMerge;
  plugin: TPlugin;
  i: Integer;
  ListItem: TListItem;
begin
  merge := CreateNewMerge(MergesList);

  // add items selected in PluginListView to merge
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    ListItem.SubItems[2] := merge.name;
    plugin := TPlugin(PluginsList[i]);
    if not plugin.hasData then
      plugin.GetData;
    merge.plugins.AddObject(plugin.filename, TObject(i));
    plugin.merge := merge.name;
  end;

  // update
  MergesList.Add(merge);
  UpdateMerges;
  UpdatePluginsPopupMenu;
  PluginsListView.Repaint;
end;

procedure TMergeForm.CheckForErrorsClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
  ProgressForm: TProgressForm;
  pluginsToCheck: TList;
begin
  pluginsToCheck := TList.Create;
  ProgressForm := TProgressForm.Create(MergeForm);
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := 0;

  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    if IS_BLACKLISTED in plugin.flags then
      continue;
    ProgressForm.ProgressBar.Max := ProgressForm.ProgressBar.Max + StrToInt(plugin.numRecords);
    pluginsToCheck.Add(plugin);
  end;

  for i := 0 to Pred(pluginsToCheck.Count) do begin
    plugin := TPlugin(pluginsToCheck[i]);
    Tracker.Write('Checking for errors in '+plugin.filename);
    plugin.FindErrors;
  end;

  Tracker.Write('All done!');
  ProgressForm.ProgressBar.Position := ProgressForm.ProgressBar.Max;
  ProgressForm.Visible := false;
  ProgressForm.ShowModal;
  ProgressForm.Free;
  UpdateMerges;
  PluginsListView.Repaint;
end;

{ Remove from Merge }
procedure TMergeForm.RemoveFromMergeClick(Sender: TObject);
var
  i: integer;
  listItem: TListItem;
  pluginName, mergeName: string;
  merge: TMerge;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // get plugin associated with merge item and remove it from merge
    plugin := TPlugin(PluginsList[i]);
    pluginName := plugin.filename;
    mergeName := plugin.merge;
    if mergeName <> ' ' then begin
      merge := MergeByName(MergesList, mergeName);
      if Assigned(merge) then
        merge.plugins.Delete(merge.plugins.IndexOf(pluginName));
    end;
    plugin.merge := ' ';
  end;

  // update
  UpdateMerges;
  PluginsListView.Repaint;
end;

{******************************************************************************}
{ Merge List View Events
  Events involving the MergeListView control.  Events include:
  - UpdateMergeDetails
  - UpdateMerges
  - MergeListViewChange
  - SaveMergeEdit
}
{******************************************************************************}

procedure TMergeForm.UpdateMergeDetails;
var
  mergeItem: TListItem;
  merge: TMerge;
  prop: TItemProp;
  sl: TStringList;
begin
  // don't do anything if no item selected
  mergeItem := MergeListView.Selected;
  if not Assigned(mergeItem) then
    exit;

  // prepare list view for merge information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Merge Details:';

  // get merge information
  merge := MergesList[MergeListView.ItemIndex];
  currentMerge := merge;
  AddDetailsItem('Status', StatusArray[merge.status].caption, false);
  AddDetailsItem('Merge name', merge.name, true);
  AddDetailsItem('Filename', merge.filename, true);
  AddDetailsItem('Plugin count', IntToStr(merge.plugins.Count));
  AddDetailsItem('Date built', DateBuiltString(merge.dateBuilt));
  AddDetailsList('Plugins', merge.plugins);
  AddDetailsItem(' ', ' ');
  prop := AddDetailsItem('Merge method', merge.method, false);
  prop.EditStyle := esPickList;
  prop.PickList.Add('Overrides');
  prop.PickList.Add('New records');
  prop := AddDetailsItem('Renumbering', merge.renumbering, false);
  prop.EditStyle := esPickList;
  prop.PickList.Add('Conflicting');
  prop.PickList.Add('All');
  if merge.files.Count < 500 then begin
    sl := TStringList.Create;
    sl.Text := StringReplace(merge.files.Text, settings.mergeDirectory, '', [rfReplaceAll]);
    AddDetailsList('Files', sl);
    sl.Free;
  end
  else
    AddDetailsItem('Files', 'Too many files to display.');
  AddDetailsList('Fails', merge.fails);
end;

procedure TMergeForm.UpdateMerges;
var
  i: integer;
  merge: TMerge;
  bMergesToBuild, bMergesToCheck: boolean;
begin
  bMergesToBuild := false;
  bMergesToCheck := false;
  // update merge count
  MergeListView.Items.Count := MergesList.Count;

  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    // sort plugins in merge
    merge.SortPlugins;
    // get status of each merge
    if not (merge.status in ForcedStatuses) then
      merge.GetStatus;
    if (merge.status in BuildStatuses) then
      bMergesToBuild := true;
    if (merge.status = 10) then
      bMergesToCheck := true;
  end;

  // enable build button if there are merges to build
  BuildButton.Enabled := bMergesToBuild;
  if not bMergesToBuild then
    BuildButton.Hint := 'No merges to build!'
  else if bMergesToCheck then
    BuildButton.Hint := 'Check merges for errors'
  else
    BuildButton.Hint := 'Build all merges';
end;

procedure TMergeForm.MergeListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateMergeDetails;
end;

procedure TMergeForm.MergeListViewData(Sender: TObject; Item: TListItem);
var
  merge: TMerge;
begin
  merge := TMerge(MergesList[Item.Index]);
  Item.Caption := IntToHex(Item.Index, 2);
  Item.SubItems.Add(merge.name);
  Item.SubItems.Add(merge.filename);
  Item.SubItems.Add(IntToStr(merge.plugins.count));
  Item.SubItems.Add(DateBuiltString(merge.dateBuilt));
  MergeListView.Canvas.Font.Color := StatusArray[merge.status].color;
  MergeListView.Canvas.Font.Style := MergeListView.Canvas.Font.Style + [fsBold];
end;

procedure TMergeForm.MergeListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  R := Rect;
  R.Right := R.Left + ListView.Columns[0].Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;
  ListView.Canvas.TextRect(R, x, y, Item.Caption);
  for i := 0 to Item.SubItems.Count - 1 do begin
    R.Left := R.Right + 3;
    R.Right := R.Left + ListView.Columns[i + 1].Width;
    x := R.Left;
    ListView.Canvas.TextRect(R, x, y, Item.SubItems[i]);
  end;
end;


{******************************************************************************}
{ MergePopupMenu methods
  Methods for dealing with the popup menu for the MergesListView.
  - MergesPopupMenuPopup
  - EditMergeItemClick
  - CheckPluginsForErrorsItemClick
  - DeleteMergeItemClick
  - OpenInExplorerItemClick
  - ForceRebuildItemClick
  - IgnoreRebuildItemClick
  - ReportOnMergeItemClick
  - ReportOnMergesItemClick
  - RebuildMergeItemClick
}
{******************************************************************************}

procedure TMergeForm.MergesPopupMenuPopup(Sender: TObject);
var
  neverBuilt, hasBuildStatus, hasUpToDateStatus,
  hasCheckStatus: boolean;
  merge: TMerge;
  i, mergesSelected: Integer;
begin
  neverBuilt := false;
  hasBuildStatus := false;
  hasUpToDateStatus := false;
  hasCheckStatus := false;
  mergesSelected := 0;

  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    Inc(mergesSelected);
    neverBuilt := neverBuilt or (merge.dateBuilt = 0);
    hasBuildStatus := hasBuildStatus or (merge.status in BuildStatuses);
    hasUpToDateStatus := hasUpToDateStatus or (merge.status in UpToDateStatuses);
    hasCheckStatus := hasCheckStatus or (merge.status = 10);
  end;

  MergesPopupMenu.Items[1].Enabled := (mergesSelected > 0);
  MergesPopupMenu.Items[2].Enabled := (mergesSelected > 0) and hasCheckStatus;
  MergesPopupMenu.Items[3].Enabled := (mergesSelected > 0);
  MergesPopupMenu.Items[4].Enabled := (mergesSelected > 0) and not hasCheckStatus;
  MergesPopupMenu.Items[5].Enabled := (mergesSelected > 0);
  MergesPopupMenu.Items[6].Enabled := (mergesSelected > 0) and hasUpToDateStatus;
  MergesPopupMenu.Items[7].Enabled := (mergesSelected > 0) and hasUpToDateStatus;
  MergesPopupMenu.Items[8].Enabled := (mergesSelected > 0) and hasBuildStatus;

  // handle build/rebuild menu item
  if (mergesSelected = 1) then begin
    if neverBuilt then
      MergesPopupMenu.Items[4].Caption := 'Build merge'
    else if hasBuildStatus then
      MergesPopupMenu.Items[4].Caption := 'Rebuild merge'
    else begin
      MergesPopupMenu.Items[4].Enabled := false;
      MergesPopupMenu.Items[4].Caption := 'Rebuild merge';
    end;
  end
  else if (mergesSelected > 1) then begin
    if neverBuilt then
      MergesPopupMenu.Items[4].Caption := 'Build merges'
    else if hasBuildStatus then
      MergesPopupMenu.Items[4].Caption := 'Rebuild merges'
    else begin
      MergesPopupMenu.Items[4].Enabled := false;
      MergesPopupMenu.Items[4].Caption := 'Rebuild merges';
    end;
  end;
end;

procedure TMergeForm.EditMergeItemClick(Sender: TObject);
var
  EditMerge: TEditForm;
  i, j, mr: integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    // create EditForm
    EditMerge := TEditForm.Create(nil);
    EditMerge.merge := merge;
    mr := EditMerge.ShowModal;
    merge := EditMerge.merge;
    EditMerge.Free;

    // if modal result <> mrOk then continue
    if mr <> mrOk then
      continue;

    // update popup menus and stuff
    UpdateMergeDetails;
    UpdatePluginsPopupMenu;
    for j := 0 to Pred(currentMerge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      if Assigned(plugin) then
        plugin.merge := merge.name;
    end;
    MergeListView.Repaint;
  end;
end;

{ Check plugins in merge for errors }
procedure TMergeForm.CheckPluginsForErrorsItemClick(Sender: TObject);
var
  timeCost, i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
  timeCosts, merges: TList;
begin
  timeCosts := TList.Create;
  merges := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status = 10) then
      continue;

    // else calculate time cost and build merge
    timeCost := merge.GetTimeCost;
    timeCosts.Add(Pointer(timeCost));
    merges.Add(merge);
    Logger.Write('Time cost for '+merge.name+': '+IntToStr(timeCost));
  end;

  // free and exit if no merges to check for errors
  if merges.Count = 0 then begin
    timeCosts.Free;
    merges.Free;
    exit;
  end;

  // Show progress form
  ProgressForm := TProgressForm.Create(MergeForm);
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));
  bCaptureTracker := true;

  // check merges for errors
  for i := 0 to Pred(merges.Count) do begin
    merge := TMerge(merges[i]);
    // check plugins for errors
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      Tracker.Write('Checking for errors in '+plugin.filename);
      plugin.FindErrors;
    end;
    ProgressForm.ProgressBar.Position := IntegerListSum(timeCosts, i);
  end;

  // all done
  Tracker.Write('All done!');
  ProgressForm.ProgressBar.Position := ProgressForm.ProgressBar.Max;
  ProgressForm.Visible := false;
  ProgressForm.ShowModal;
  ProgressForm.Free;
  timeCosts.Free;
  merges.Free;
  UpdateMerges;
  PluginsListView.Repaint;
end;

procedure TMergeForm.DeleteMergeItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if MessageDlg('Are you sure you want to delete '+merge.name+'?',
      mtConfirmation, mbOKCancel, 0) = mrOK then begin

      // remove merge from plugin merge properties
      for j := 0 to Pred(PluginsList.Count) do begin
        plugin := TPlugin(PluginsList[j]);
        plugin.merge := ' ';
      end;

      // delete merge
      merge.Free;
      MergesList.Delete(MergeListView.ItemIndex);

      // clear details editor
      DetailsEditor.Strings.Clear;
    end;

    // update merges
    MergeListView.Items.Count := MergesList.Count;
    UpdatePluginsPopupMenu;
  end;
end;

procedure TMergeForm.BuildMergeItemClick(Sender: TObject);
var
  timeCost, i: Integer;
  merge: TMerge;
  bfn: string;
  timeCosts, merges: TList;
begin
  timeCosts := TList.Create;
  merges := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status in BuildStatuses) then
      continue;

    // else calculate time cost and build merge
    timeCost := merge.GetTimeCost * 2;
    timeCosts.Add(Pointer(timeCost));
    merges.Add(merge);
    Logger.Write('Time cost for '+merge.name+': '+IntToStr(timeCost));
  end;

  // free and exit if no merges to check for errors
  if merges.Count = 0 then begin
    timeCosts.Free;
    merges.Free;
    exit;
  end;

  // Show progress form
  ProgressForm := TProgressForm.Create(MergeForm);
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));
  Application.ProcessMessages;

  // build merges
  for i := 0 to Pred(merges.Count) do begin
    try
      merge := TMerge(merges[i]);
      if (merge.status in RebuildStatuses) then
        RebuildMerge(merge)
      else
        BuildMerge(merge);
    except on x : Exception do
      Tracker.Write('Exception: '+x.Message);
    end;
    Tracker.Write(' '#13#10);
    ProgressForm.ProgressBar.Position := IntegerListSum(timeCosts, i);
  end;

  // batch copy assets
  if settings.batCopy and (batch.Count > 0) then begin
    bfn := tempPath+FormatDateTime('mmddyy_hhnnss', Now)+'.bat';
    batch.Add('pause');
    batch.SaveToFile(bfn);
    batch.Clear;
    ShellExecute(Application.Handle, 'open', PChar(bfn), '', PChar(wbProgramPath), SW_SHOWNORMAL);
  end;

  // save log
  ProgressForm.LogMemo.Lines.SaveToFile(LogPath + 'merge_'+FormatDateTime('mmddyy_hhnnss', Now)+'.txt');

  // display progress form after merging
  ProgressForm.SaveLog;
  ProgressForm.DetailsButtonClick(nil);
  ProgressForm.Visible := false;
  ProgressForm.ShowModal;

  // free memory
  ProgressForm.Free;

  // update mpMergeForm
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
  DeleteDirectory(tempPath);
end;

procedure TMergeForm.OpenInExplorerItemClick(Sender: TObject);
var
  i: Integer;
  path: string;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);

    // open in explorer
    path := settings.mergeDirectory + merge.name;
    ForceDirectories(path);
    ShellExecute(TForm(MergeForm).Handle, 'open', PChar(path), '', '', SW_SHOWNORMAL);
  end;
end;

procedure TMergeForm.ForceRebuildItemClick(Sender: TObject);
var
  i: Integer;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status in UpToDateStatuses) then
      continue;
    merge.Status := 9;
  end;

  // update
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
end;

procedure TMergeForm.IgnoreRebuildItemClick(Sender: TObject);
var
  i: Integer;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status in RebuildStatuses) then
      continue;
    merge.Status := 6;
  end;

  // update
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
end;

procedure TMergeForm.ReportOnMergeItemClick(Sender: TObject);
var
  i: Integer;
  merge: TMerge;
  merges: TList;
begin
  merges := TList.Create;

  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    merges.Add(merge);
  end;

  // report on all merges
  //Report(merges);
  merges.Free;
end;

{******************************************************************************}
{ QuickBar Button Events
  Events involving buttons on the QuickBar.  Events include:
  - CreateMergeButtonClick
  - RebuildButtonClick
  - ReportButtonClick
  - OptionsButtonClick
  - DictionaryButtonClick
  - UpdateButtonClick
  - HelpButtonClick
}
{******************************************************************************}

procedure TMergeForm.CreateMergeButtonClick(Sender: TObject);
var
  merge: TMerge;
begin
  LogMessage('Created new merge!');
  merge := CreateNewMerge(MergesList);
  // add and update merge
  MergesList.Add(merge);
  UpdateMerges;
  MergeListView.Repaint;
  UpdatePluginsPopupMenu;
end;

procedure TMergeForm.RebuildButtonClick(Sender: TObject);
var
  i, timeCost: integer;
  merge: TMerge;
  ProgressForm: TProgressForm;
  timeCosts, merges: TList;
  bfn: string;
begin
  LogMessage('Rebuild merges!');
  if MergesList.Count = 0 then
    exit;

  // calculate time costs
  timeCosts := TList.Create;
  merges := TList.Create;
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    if not (merge.status in BuildStatuses) then
      continue;
    timeCost := merge.GetTimeCost * 2;
    Logger.Write('Time cost for '+merge.name+': '+IntToStr(timeCost));
    merges.Add(merge);
    timeCosts.Add(Pointer(timeCost));
  end;

  // exit if no merges to build
  if timeCosts.Count = 0 then begin
    Logger.Write('No merges to build!');
    timeCosts.Free;
    merges.Free;
    exit;
  end;

  // make and show progress form
  ProgressForm := TProgressForm.Create(MergeForm);
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));
  Application.ProcessMessages;

  // rebuild merges
  for i := 0 to Pred(merges.count) do begin
    merge := merges[i];
    if not (merge.status in BuildStatuses) then
      continue;
    try
      if merge.status = 7 then
        BuildMerge(merge)
      else
        RebuildMerge(merge);
    except on x : Exception do
      Tracker.Write('Exception: '+x.Message);
    end;
    Tracker.Write(' '#13#10);
    ProgressForm.ProgressBar.Position := IntegerListSum(timeCosts, i);
  end;

  // batch copy assets
  if settings.batCopy and (batch.Count > 0) then begin
    bfn := tempPath+FormatDateTime('mmddyy_hhnnss', Now)+'.bat';
    batch.Add('pause');
    batch.SaveToFile(bfn);
    batch.Clear;
    ShellExecute(Application.Handle, 'open', PChar(bfn), '', PChar(wbProgramPath), SW_SHOWNORMAL);
  end;

  // save log
  ProgressForm.LogMemo.Lines.SaveToFile(LogPath + 'merge_'+FormatDateTime('mmddyy_hhnnss', Now)+'.txt');

  // display progress form after merging
  ProgressForm.DetailsButtonClick(nil);
  ProgressForm.Visible := false;
  ProgressForm.ShowModal;

  // free memory
  ProgressForm.Free;
  timeCosts.Free;
  merges.Free;

  // update mpMergeForm
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
  //PluginsListView.Items.Count := PluginsList.Count;
end;

{ Submit report }
procedure TMergeForm.ReportButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

{ View the dictionary file }
procedure TMergeForm.DictionaryButtonClick(Sender: TObject);
var
  DictionaryForm: TDictionaryForm;
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
  DictionaryForm := TDictionaryForm.Create(nil);
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

{ Options }

procedure TMergeForm.OptionsButtonClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
begin
  // Create and show options form
  OptionsForm := TOptionsForm.Create(nil);
  OptionsForm.ShowModal;
  OptionsForm.Free;

  // initialize MO if usingMO changed
  if settings.usingMO then
    ModOrganizerInit;

  // update owner draw if changed
  PluginsListView.OwnerDraw := not settings.simplePluginsView;

  // if user selected to change game mode, restart application with -sg param
  if bChangeGameMode then begin
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '-sg', '', SW_SHOWNORMAL);
    Close;
  end;
end;

{ Update }
procedure TMergeForm.UpdateButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

{ Help }
procedure TMergeForm.HelpButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

end.
