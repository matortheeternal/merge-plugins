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
  mpTracker, mpSplash,
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
    Addtomerge1: TMenuItem;
    RemoveFromMerge: TMenuItem;
    ReportOnPlugin: TMenuItem;
    MergesPopupMenu: TPopupMenu;
    CreateNewMergeItem: TMenuItem;
    DeleteMergeItem: TMenuItem;
    RebuildMergeItem: TMenuItem;
    NewMerge1: TMenuItem;
    MergeListView: TListView;
    DetailsEditor: TValueListEditor;
    FlagList: TImageList;
    CheckforErrors1: TMenuItem;
    ForceRebuildItem: TMenuItem;
    BuildAllMergesItem: TMenuItem;
    N1: TMenuItem;
    ReportOnMergesItem: TMenuItem;
    ReportOnMergeItem: TMenuItem;
    OpenInExplorerItem: TMenuItem;
    IgnoreRebuildItem: TMenuItem;
    procedure LogMessage(const s: string);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure RebuildMergesClick(Sender: TObject);
    procedure CreateNewMergeClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    function AddDetailsItem(name, value: string; editable: boolean = false):
      TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList; editable: boolean = false);
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    function FlagNotSafe(Rect: TRect; x: integer): boolean;
    procedure DrawFlag(canvas: TCanvas; flags: string; index, x, y: integer);
    procedure DrawPluginFlags(canvas: TCanvas; Rect: TRect; x, y: integer; flags: string);
    procedure PluginsListViewData(Sender: TObject; Item: TListItem);
    procedure PluginsListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure MergeListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure UpdateMerges;
    procedure AddToNewMergeClick(Sender: TObject);
    procedure UpdatePluginsPopupMenu;
    procedure AddToMergeClick(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure CheckForErrorsClick(Sender: TObject);
    procedure DeleteMerge(Sender: TObject);
    procedure PluginsPopupMenuPopup(Sender: TObject);
    procedure MergesPopupMenuPopup(Sender: TObject);
    procedure SaveMergeEdit(Sender: TObject);
    procedure RemoveFromMergeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MergeListViewData(Sender: TObject; Item: TListItem);
    procedure MergeListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure OpenInExplorerClick(Sender: TObject);
    procedure ForceRebuildItemClick(Sender: TObject);
    procedure IgnoreRebuildItemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MergeForm: TMergeForm;
  currentMerge: TMerge;
  blacklist: TStringList;

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
    ForceDirectories(tempPath);
    MergesList := TList.Create;
    PluginsList := TList.Create;
    Tracker.Write('Loading Settings');
    LoadSettings;
    Tracker.Write('Loading Dictionary');
    LoadDictionary;

    // GUI ICONS
    Tracker.Write('Loading Icons');
    IconList.GetBitmap(0, NewButton.Glyph);
    IconList.GetBitmap(1, BuildButton.Glyph);
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
    wbGameMode := gmTES5;
    wbAppName := 'TES5';
    wbGameName := 'Skyrim';
    wbLanguage := 'English';
    wbEditAllowed := True;

    // LOAD TES5EDIT DEFINITIONS
    Tracker.Write('Loading TES5Edit Definitions');
    wbProgressCallback := ProgressMessage;
    Logger.OnLogEvent := LogMessage;

    if not LoadDataPath then begin
      splash.Free;
      bDontSave := true;
      MergeForm.Close;
      exit;
    end;
    handler := wbCreateContainerHandler;
    handler._AddRef;
    LoadDefinitions;

    // PREPARE TO LOAD PLUGINS
    wbPluginsFileName := GetCSIDLShellFolder(CSIDL_LOCAL_APPDATA);
    wbPluginsFileName := wbPluginsFileName + wbGameName + '\Plugins.txt';
    sl := TStringList.Create;
    sl.LoadFromFile(wbPluginsFileName);
    RemoveCommentsAndEmpty(sl);
    if wbGameMode = gmTES5 then begin
      if sl.IndexOf('Update.esm') = -1 then
        sl.Insert(0, 'Update.esm');
      if sl.IndexOf('Skyrim.esm') = -1 then
        sl.Insert(0, 'Skyrim.esm');
    end;
    RemoveMissingFiles(sl);
    AddMissingFiles(sl);

    // LOAD PLUGINS
    for i := 0 to Pred(sl.Count) do begin
      Tracker.Write('Loading '+sl[i]);
      plugin := TPlugin.Create;
      plugin.filename := sl[i];
      plugin.pluginFile := wbFile(wbDataPath + sl[i], i);
      plugin.pluginFile._AddRef;
      plugin.GetData;
      PluginsList.Add(Pointer(plugin));

      // load hardcoded dat
      if i = 0 then begin
        aFile := wbFile(wbProgramPath + wbGameName + wbHardcodedDat, 0);
        aFile._AddRef;
      end;
    end;

    // LOAD MERGES
    Tracker.Write('Loading Merges');
    LoadMerges;
    UpdateMerges;
    MergeListView.Items.Count := MergesList.Count;

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
  PluginsListView.Width := PluginsListView.Width + 1;
end;

procedure TMergeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not bDontSave then SaveMerges;
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
    MergeListViewChange(nil, nil, TItemChange(nil))
end;

{******************************************************************************}
{ Plugins List View Events
  Events involving the PluginsListView control.  Events include:
  - PluginsListViewChange
  - AddToMerge
  - AddToNewMerge
  - CheckPluginForErrors
}
{******************************************************************************}

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
  DetailsEditor.OnStringsChange := nil;
  DetailsEditor.Options := DetailsEditor.Options - [goEditing];
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Plugin Details:';

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetData;

  // add details items
  AddDetailsItem('Filename', plugin.filename);
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
  b: boolean;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  b := true;
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    b := false;
    plugin := PluginsList[i];
    if IS_BLACKLISTED in plugin.flags then begin
      b := true;
      break;
    end;
  end;

  PluginsPopupMenu.Items[0].Enabled := not b;
  PluginsPopupMenu.Items[1].Enabled := not b;
  PluginsPopupMenu.Items[2].Enabled := not b;
  PluginsPopupMenu.Items[3].Enabled := not b;
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
    merge.plugins.Add(plugin.filename);
    merge.pluginSizes.Add(Pointer(plugin.fileSize));
    merge.pluginDates.Add(plugin.dateModified);
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
    merge.plugins.Add(plugin.filename);
    merge.pluginSizes.Add(Pointer(plugin.fileSize));
    merge.pluginDates.Add(plugin.dateModified);
    plugin.merge := merge.name;
  end;

  // update merges
  MergesList.Add(merge);
  MergeListView.Items.Count := MergesList.Count;
  UpdateMerges;
  // update plugins
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
  ProgressForm := TProgressForm.Create(nil);
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := 0;

  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
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
      plugin.merge := ' ';
    end;
  end;

  // update
  UpdateMerges;
  PluginsListView.Repaint;
end;

{******************************************************************************}
{ Merge List View Events
  Events involving the MergeListView control.  Events include:
  - MergeListViewChange
  - UpdateMerges
  - SaveMergeEdit
}
{******************************************************************************}

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
  DetailsEditor.OnStringsChange := nil;
  DetailsEditor.Strings.Clear;
  DetailsEditor.Options := DetailsEditor.Options + [goEditing];
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
  AddDetailsList('Masters', merge.masters);
  AddDetailsItem(' ', ' ');
  prop := AddDetailsItem('Merge method', merge.method, false);
  prop.EditStyle := esPickList;
  prop.PickList.Add('Overrides');
  prop.PickList.Add('New records');
  prop := AddDetailsItem('Renumbering', merge.renumbering, false);
  prop.EditStyle := esPickList;
  prop.PickList.Add('Conflicting');
  prop.PickList.Add('All');
  AddDetailsList('Fails', merge.fails);

  // return event
  DetailsEditor.OnStringsChange := SaveMergeEdit;
end;

procedure TMergeForm.UpdateMerges;
var
  i: integer;
  merge: TMerge;
begin
  // get status of each merge
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    if not (merge.status in ForcedStatuses) then
      merge.GetStatus;
  end;
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

procedure TMergeForm.SaveMergeEdit(Sender: TObject);
var
  i: integer;
  plugin: TPlugin;
begin
  if not Assigned(currentMerge) then
    exit;

  // update merge
  currentMerge.name := DetailsEditor.Values['Merge name'];
  currentMerge.filename := DetailsEditor.Values['Filename'];
  currentMerge.method := DetailsEditor.Values['Merge method'];
  currentMerge.renumbering := DetailsEditor.Values['Renumbering'];
  UpdatePluginsPopupMenu;

  for i := 0 to Pred(currentMerge.plugins.Count) do begin
    plugin := PluginByFilename(currentMerge.plugins[i]);
    if Assigned(plugin) then
      plugin.merge := currentMerge.name;
  end;

  MergeListView.Repaint;
end;


{******************************************************************************}
{ MergePopupMenu methods
  Methods for dealing with the popup menu for the MergesListView.
  - MergesPopupMenuPopup
  - DeleteMerge
}
{******************************************************************************}

procedure TMergeForm.MergesPopupMenuPopup(Sender: TObject);
var
  mergeSelected, neverBuilt: boolean;
begin
  neverBuilt := false;
  mergeSelected := MergeListView.ItemIndex > -1;
  if mergeSelected then begin
    currentMerge := TMerge(MergesList[MergeListView.ItemIndex]);
    neverBuilt := currentMerge.dateBuilt = 0;
  end;

  MergesPopupMenu.Items[1].Enabled := mergeSelected;
  MergesPopupMenu.Items[2].Enabled := mergeSelected;
  MergesPopupMenu.Items[3].Enabled := mergeSelected;
  MergesPopupMenu.Items[4].Enabled := mergeSelected and (not neverBuilt);
  MergesPopupMenu.Items[5].Enabled := mergeSelected and (not neverBuilt);
  MergesPopupMenu.Items[6].Enabled := mergeSelected and (not neverBuilt);
end;

procedure TMergeForm.DeleteMerge(Sender: TObject);
var
  i: Integer;
  plugin: TPlugin;
begin
  // exit if no merge selected
  if MergeListView.ItemIndex = -1 then
    exit;

  // else prompt user, delete merge and update details editor
  currentMerge := TMerge(MergesList[MergeListView.ItemIndex]);
  if MessageDlg('Are you sure you want to delete '+currentMerge.name+'?',
    mtConfirmation, mbOKCancel, 0) = mrOK then begin

    // remove merge from plugin merge properties
    for i := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[i]);
      plugin.merge := ' ';
    end;

    // delete merge
    currentMerge := nil;
    MergesList.Delete(MergeListView.ItemIndex);

    // clear details editor
    DetailsEditor.OnStringsChange := nil;
    DetailsEditor.Options := DetailsEditor.Options - [goEditing];
    DetailsEditor.Strings.Clear;
  end;

  // update merges
  MergeListView.Items.Count := MergesList.Count;
  UpdatePluginsPopupMenu;
end;

procedure TMergeForm.OpenInExplorerClick(Sender: TObject);
var
  path: string;
begin
  currentMerge := TMerge(MergesList[MergeListView.ItemIndex]);
  path := settings.mergeDirectory + currentMerge.name;
  ForceDirectories(path);
  ShellExecute(TForm(MergeForm).Handle, 'open', PChar(path), '', '', SW_SHOWNORMAL);
end;

procedure TMergeForm.ForceRebuildItemClick(Sender: TObject);
begin
  // force rebuild
  currentMerge := TMerge(MergesList[MergeListView.ItemIndex]);
  currentMerge.Status := 9;
  MergeListView.Repaint;
  MergeListViewChange(nil, nil, TItemChange(nil));
end;

procedure TMergeForm.IgnoreRebuildItemClick(Sender: TObject);
begin
  // ignore rebuild
  currentMerge := TMerge(MergesList[MergeListView.ItemIndex]);
  currentMerge.Status := 6;
  MergeListView.Repaint;
  MergeListViewChange(nil, nil, TItemChange(nil));
end;


{******************************************************************************}
{ QuickBar Button Events
  Events involving buttons on the QuickBar.  Events include:
  - CreateNewMergeClick
  - RebuildMergesClick
  - ReportButtonClick
  - OptionsButtonClick
  - DictionaryButtonClick
  - UpdateButtonClick
  - HelpButtonClick
}
{******************************************************************************}

procedure TMergeForm.CreateNewMergeClick(Sender: TObject);
var
  merge: TMerge;
begin
  LogMessage('Created new merge!');
  merge := CreateNewMerge(MergesList);
  // add and update merge
  MergesList.Add(merge);
  MergeListView.Items.Count := MergesList.Count;
  UpdateMerges;
  MergeListView.Repaint;
  UpdatePluginsPopupMenu;
end;

procedure TMergeForm.RebuildMergesClick(Sender: TObject);
var
  i, timeCost: integer;
  merge: TMerge;
  ProgressForm: TProgressForm;
  timeCosts: TList;
begin
  LogMessage('Rebuild merges!');
  if MergesList.Count = 0 then
    exit;

  // calculate time costs
  timeCosts := TList.Create;
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    if not (merge.status in BuildStatuses) then
      continue;
    timeCost := merge.GetTimeCost;
    Logger.Write('Time cost for '+merge.name+': '+IntToStr(timeCost));
    timeCosts.Add(Pointer(timeCost));
  end;

  // exit if no merges to build
  if timeCosts.Count = 0 then begin
    Logger.Write('No merges to build!');
    exit;
  end;

  // make and show progress form
  ProgressForm := TProgressForm.Create(nil);
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));
  bCaptureTracker := true;

  // rebuild merges
  for i := 0 to Pred(MergesList.count) do begin
    merge := MergesList[i];
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

  // display progress form after merging
  ProgressForm.DetailsButtonClick(nil);
  ProgressForm.Visible := false;
  ProgressForm.ShowModal;

  // free memory
  ProgressForm.Free;
  timeCosts.Free;

  // update mpMergeForm
  UpdateMerges;
  MergeListViewChange(nil, nil, TItemChange(nil));
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
  //LogMessage(TButton(Sender).Hint+' clicked!');
  OptionsForm := TOptionsForm.Create(nil);
  OptionsForm.ShowModal;
  OptionsForm.Free;
  PluginsListView.OwnerDraw := not settings.simplePluginsView;
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
