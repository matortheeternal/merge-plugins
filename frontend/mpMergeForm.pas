unit mpMergeForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl,
  Menus, Grids, ValEdit, ShlObj, ShellAPI,
  // third party libraries
  superobject, W7Taskbar,
  // mp units
  mpBase, mpMerge, mpLogger, mpDictionaryForm, mpOptionsForm, mpProgressForm,
  mpTracker, mpSplashForm, mpEditForm, mpGameForm, mpReportForm,
  // tes5edit units
  wbBSA, wbHelpers, wbInterface, wbImplementation, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient;

type
  TMergeForm = class(TForm)
    // GENERIC
    XPManifest: TXPManifest;
    IconList: TImageList;
    FlagList: TImageList;
    DoubleIconList: TImageList;
    StatusBar: TStatusBar;
    // QUICKBAR
    QuickBar: TPanel;
    NewButton: TSpeedButton;
    BuildButton: TSpeedButton;
    ReportButton: TSpeedButton;
    DictionaryButton: TSpeedButton;
    OptionsButton: TSpeedButton;
    UpdateButton: TSpeedButton;
    HelpButton: TSpeedButton;
    // MAIN PANEL
    MainPanel: TPanel;
    Splitter: TSplitter;
    DetailsPanel: TPanel;
    // PAGE CONTROL - MERGES/PLUGINS/LOG
    PageControl: TPageControl;
    PluginsTabSheet: TTabSheet;
    MergesTabSheet: TTabSheet;
    LogTabSheet: TTabSheet;
    LogMemo: TMemo;
    PluginsListView: TListView;
    MergeListView: TListView;
    // PLUGINS POPUP MENU
    PluginsPopupMenu: TPopupMenu;
    AddToMerge: TMenuItem;
    RemoveFromMerge: TMenuItem;
    ReportOnPlugin: TMenuItem;
    CreateNewMergeItem: TMenuItem;
    DeleteMergeItem: TMenuItem;
    BuildMergeItem: TMenuItem;
    NewMerge: TMenuItem;
    // MERGES POPUP MENU
    MergesPopupMenu: TPopupMenu;
    CheckforErrorsItem: TMenuItem;
    ForceRebuildItem: TMenuItem;
    ReportOnMergeItem: TMenuItem;
    OpenInExplorerItem: TMenuItem;
    IgnoreRebuildItem: TMenuItem;
    EditMergeItem: TMenuItem;
    CheckPluginsForErrorsItem: TMenuItem;
    OpenPluginLocationItem: TMenuItem;
    // DETAIL EDITOR
    DetailsLabel: TLabel;
    DetailsEditor: TValueListEditor;
    RemoveBadPluginsItem: TMenuItem;
    Timer: TTimer;
    StatusIcons: TImageList;
    Heartbeat: TTimer;

    // MERGE FORM EVENTS
    procedure LogMessage(const s: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LoaderDone;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnTimer(Sender: TObject);
    procedure ClientStatusChanged(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure ShowAuthorizationMessage;
    // DETAILS EDITOR EVENTS
    function AddDetailsItem(name, value: string; editable: boolean = false):
      TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList; editable: boolean = false);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateApplicationDetails;
    procedure DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    {procedure DetailsEditorSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);}
    // PLUGINS LIST VIEW EVENTS
    procedure UpdatePluginDetails;
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PluginsListViewData(Sender: TObject; Item: TListItem);
    function FlagNotSafe(Rect: TRect; x: integer): boolean;
    procedure DrawFlag(canvas: TCanvas; flag: TPluginFlag; x, y: integer);
    procedure DrawPluginFlags(canvas: TCanvas; Rect: TRect; x, y: integer; flags: string);
    procedure PluginsListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure PluginsListViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // MERGE LIST VIEW EVENTS
    procedure UpdateMergeDetails;
    procedure UpdateMerges;
    procedure MergeListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MergeListViewData(Sender: TObject; Item: TListItem);
    procedure MergeListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    // PLUGINS POPUP MENU EVENTS
    procedure PluginsPopupMenuPopup(Sender: TObject);
    procedure UpdatePluginsPopupMenu;
    procedure AddToNewMergeClick(Sender: TObject);
    procedure AddToMergeClick(Sender: TObject);
    procedure CheckForErrorsClick(Sender: TObject);
    procedure RemoveFromMergeClick(Sender: TObject);
    procedure OpenPluginLocationItemClick(Sender: TObject);
    // MERGES POPUP MENU EVENTS
    procedure MergesPopupMenuPopup(Sender: TObject);
    procedure EditMergeItemClick(Sender: TObject);
    procedure BuildMergeItemClick(Sender: TObject);
    procedure CheckPluginsForErrorsItemClick(Sender: TObject);
    procedure RemoveBadPluginsItemClick(Sender: TObject);
    procedure DeleteMergeItemClick(Sender: TObject);
    procedure ReportOnMergeItemClick(Sender: TObject);
    procedure OpenInExplorerItemClick(Sender: TObject);
    procedure ForceRebuildItemClick(Sender: TObject);
    procedure IgnoreRebuildItemClick(Sender: TObject);
    procedure MergeListViewDblClick(Sender: TObject);
    procedure MergeListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    // QUICKBAR BUTTON EVENTS
    procedure CreateMergeButtonClick(Sender: TObject);
    procedure RebuildButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure StatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure HeartbeatTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MergeForm: TMergeForm;
  LastHint: string;
  LastURLTime: double;
  bMergesToBuild, bMergesToCheck: boolean;

implementation

{$R *.dfm}


{******************************************************************************}
{ Merge Form Events
  Events for the Merge Form.
  - LogMessage
  - ProgressMessage
  - FormCreate
  - FormShow
  - LoaderDone
  - FormClose
}
{******************************************************************************}

{ Prints a message to the log memo }
procedure TMergeForm.LogMessage(const s: string);
begin
  LogMemo.Lines.Add(s);
  SendMessage(LogMemo.Handle, EM_LINESCROLL, 0, LogMemo.Lines.Count);
end;

procedure ProgressMessage(const s: string);
begin
  if s = '' then
    exit;
  MergeForm.LogMessage(s);
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TMergeForm.FormCreate(Sender: TObject);
var
  wbPluginsFileName: string;
  sl: TStringList;
  i: integer;
  plugin: TPlugin;
  aFile: IwbFile;
  splash: TSplashForm;
begin
  InitializeTaskbarAPI;
  try
    // CREATE SPLASH
    splash := TSplashForm.Create(nil);
    splash.Show;
    SetTaskbarProgressState(tbpsIndeterminate);

    // INITIALIZE VARIABLES
    ProgramVersion := GetVersionMem;
    TempPath := wbProgramPath + 'temp\';
    LogPath := wbProgramPath + 'logs\';
    ForceDirectories(TempPath);
    ForceDirectories(LogPath);
    MergesList := TList.Create;
    PluginsList := TList.Create;
    bLoaderDone := false;
    Status := TmpStatus.Create;

    // INITIALIZE CLIENT
    InitializeClient;
    ConnectToServer;
    TCPClient.OnStatus := ClientStatusChanged;

    // GUI ICONS
    Tracker.Write('Loading Icons');
    IconList.GetBitmap(0, NewButton.Glyph);
    DoubleIconList.GetBitmap(0, BuildButton.Glyph);
    IconList.GetBitmap(2, ReportButton.Glyph);
    IconList.GetBitmap(3, DictionaryButton.Glyph);
    IconList.GetBitmap(4, OptionsButton.Glyph);
    IconList.GetBitmap(5, UpdateButton.Glyph);
    IconList.GetBitmap(6, HelpButton.Glyph);

    // STATUSBAR VALUES
    StatusBar.Panels[7].Text := settings.language;
    StatusBar.Panels[8].Text := 'v'+ProgramVersion;

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

    // LOAD MERGES, PLUGIN ERRORS
    Tracker.Write('Loading Merges, Plugin Errors');
    LoadMerges;
    LoadPluginErrors;
    UpdateMerges;
    UpdatePluginsPopupMenu;

    // FINALIZE
    sl.Free;
    PluginsListView.OwnerDraw := not settings.simplePluginsView;
    PluginsListView.Items.Count := PluginsList.Count;
    PluginsListView.Columns[1].AutoSize := true;
    Sleep(250);
    splash.Free;
  except
    on x: Exception do begin
      bDontSave := true;
      LogMessage('Exception: '+x.Message);
    end;
  end;
end;

// Force PluginsListView to autosize columns
procedure TMergeForm.FormShow(Sender: TObject);
begin
  // START BACKGROUND LOADER
  LoaderCallback := LoaderDone;
  SetTaskbarProgressState(tbpsIndeterminate);
  TLoaderThread.Create;
  // SHOW LOADER HINT
  StatusBar.SimpleText := 'Background loader in progress.';

  // SET UP PAGE CONTROLS
  PageControl.ActivePageIndex := 1;
  MergeListView.Width := MergeListView.Width + 1;
  PageControl.ActivePageIndex := 0;
  PluginsListView.Width := PluginsListView.Width + 1;
end;

procedure TMergeForm.LoaderDone;
begin
  SetTaskbarProgressState(tbpsNone);
  StatusBar.SimpleText := 'Background loader finished.';
  FlashWindow(Application.Handle, True);
  UpdateMerges;
end;

procedure TMergeForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ProgressForm: TProgressForm;
begin
  // disconnect from server
  TCPClient.Disconnect;

  // save if bDontSave is false
  if not bDontSave then begin
    // show progress form
    ProgressForm := TProgressForm.Create(Self);
    ProgressForm.PopupParent := Self;
    ProgressForm.SetTitle('Saving');
    ProgressForm.Show;
    Enabled := false;
    ProgressForm.ProgressBar.Max := PluginsList.Count + MergesList.Count + 2;
    ProgressForm.DetailsButtonClick(nil);
    // Save plugin errors
    SavePluginErorrs;
    ProgressForm.SetProgress(PluginsList.Count + 1);
    Tracker.Write(' ');
    // save merges
    SaveMerges;
    // finish up
    ProgressForm.SetProgress(ProgressForm.ProgressBar.Max);
    // ProgressForm.SaveLog;
    Application.ProcessMessages;
    ProgressForm.Free;
    Enabled := true;
  end;

  // save statistics and settings
  SaveStatistics;
  SaveSettings;

  // delete temppath
  DeleteDirectory(TempPath);
  Action := caFree;

  // restart program if update applied
  if bInstallUpdate then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
  // restart program if game mode changed
  if bChangeGameMode then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '-sg', '', SW_SHOWNORMAL);
end;

procedure TMergeForm.OnTimer(Sender: TObject);
begin
  if not TCPClient.Connected then begin
    ConnectToServer;
    if TCPClient.Connected then begin
      CheckAuthorization;
      SendGameMode;
      GetStatus;
      CompareStatuses;
      ShowAuthorizationMessage;
      StatusBar.Repaint;
    end;
  end;
end;

procedure TMergeForm.HeartbeatTimer(Sender: TObject);
begin
  TCPClient.CheckForGracefulDisconnect(false);
  TCPClient.Connected;
end;

procedure TMergeForm.ClientStatusChanged(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
  StatusBar.Repaint;
end;

procedure TMergeForm.ShowAuthorizationMessage;
begin
  if bAuthorized then begin
    Logger.Write('Authorized');
  end
  else begin
    Logger.Write('Not authorized');
  end;
end;

procedure TMergeForm.StatusBarDrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  icon: TIcon;
begin
  icon := TIcon.Create;
  case Panel.Index of
    2: begin
      if not bLoaderDone then begin
        StatusIcons.GetIcon(0, icon);
        StatusBar.Canvas.Draw(Rect.Left + 2, Rect.Top + 2, icon);
      end;
    end;
    3: begin
      if TCPClient.Connected then
        StatusIcons.GetIcon(1, icon)
      else
        StatusIcons.GetIcon(2, icon);
      StatusBar.Canvas.Draw(Rect.Left + 2, Rect.Top + 2, icon);
    end;
    4: begin
      if bLoaderDone and bMergesToBuild then begin
        StatusIcons.GetIcon(3, icon);
        StatusBar.Canvas.Draw(Rect.Left + 2, Rect.Top + 2, icon);
      end;
    end;
    5: begin
      if bDictionaryUpdate then begin
        StatusIcons.GetIcon(4, icon);
        StatusBar.Canvas.Draw(Rect.Left + 2, Rect.Top + 2, icon);
      end;
    end;
    6: begin
      if bProgramUpdate then begin
        StatusIcons.GetIcon(5, icon);
        StatusBar.Canvas.Draw(Rect.Left + 2, Rect.Top + 2, icon);
      end;
    end;
  end;
end;

{******************************************************************************}
{ Details Editor Events
  Methods for helping with the DetailsEditor control.  Methods include:
  - AddDetailsItem
  - AddDetailsList
  - PageControlChange
  - UpdateApplicationDetails
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
  case ndx of
    0: UpdatePluginDetails;
    1: UpdateMergeDetails;
    2: UpdateApplicationDetails;
  end;
end;

procedure TMergeForm.UpdateApplicationDetails;
var
  s: string;
begin
  // prepare list view for application information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Application Details';

  // add details items
  AddDetailsItem('Application', 'Merge Plugins');
  AddDetailsItem('Author', 'matortheeternal');
  AddDetailsItem('Version', ProgramVersion);
  AddDetailsItem('Date built', DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Game mode', wbGameName);
  AddDetailsItem('Language', settings.language);
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Times run', IntToStr(statistics.timesRun));
  AddDetailsItem('Merges built', IntToStr(statistics.mergesBuilt));
  AddDetailsItem('Plugins checked for errors', IntToStr(statistics.pluginsChecked));
  AddDetailsItem('Plugins merged', IntToStr(statistics.pluginsMerged));
  AddDetailsItem('Reports submitted', IntToStr(statistics.reportsSubmitted));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Website', 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem('API Credits', 'superobject, TurboPower Abbrevia, xEdit');
  AddDetailsItem('xEdit Version', xEditVersion);
  AddDetailsItem('xEdit Credits', 'zilav, hlp, Sharlikran, ElminsterAU');
  s := ProgramTesters;
  AddDetailsItem('Testers', Wordwrap(s, 70));
  s := ProgramTranslators;
  AddDetailsItem('Translators', Wordwrap(s, 70));
end;

{ Handle user clicking URL }
procedure TMergeForm.DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: integer;
  value: string;
begin
  // only process left clicks
  if Button <> mbLeft then
    exit;
  DetailsEditor.MouseToCell(X, Y, ACol, ARow);
  try
    value := DetailsEditor.Cells[ACol, ARow];
    if Pos(' ', value) > 0 then
      value := Copy(value, 1, Pos(' ', value));
    if IsURL(value) and ((Now - LastURLTime) * 86400 > 1.0) then begin
      ShellExecute(0, 'open', PChar(value), '', '', SW_SHOWNORMAL);
      LastURLTime := Now;
    end;
  except
    // invalid cell
  end;
end;

{******************************************************************************}
{ PluginsListView Events
  Events involving the PluginsListView control.  Events include:
  - UpdatePluginDetails
  - PluginsListViewChange
  - PluginsListViewData
  - FlagNotSafe
  - DrawFlag
  - DrawPluginFlags
  - PluginsListViewDrawItem
  - PluginsListViewMouseMove
}
{******************************************************************************}

procedure TMergeForm.UpdatePluginDetails;
var
  plugin: TPlugin;
  index: integer;
  sl: TStringList;
begin
  // don't do anything if no item selected
  if not Assigned(PluginsListView.Selected) then
    exit;

  // prepare list view for plugin information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Plugin Details';

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetData;

  // add details items
  sl := TStringList.Create;
  sl.Text := plugin.GetFlagsDescription;
  AddDetailsItem('Filename', plugin.filename);
  AddDetailsItem('Hash', plugin.hash);
  AddDetailsItem('File size', FormatByteSize(plugin.fileSize));
  AddDetailsItem('Date modified', plugin.dateModified);
  AddDetailsItem('Merge rating', plugin.entry.rating);
  AddDetailsList('Flags', sl);
  AddDetailsItem('Number of records', plugin.numRecords);
  AddDetailsItem('Number of overrides', plugin.numOverrides);
  AddDetailsItem('Author', plugin.author);
  AddDetailsList('Description', plugin.description);
  AddDetailsList('Masters', plugin.masters);
  AddDetailsList('Errors', plugin.errors);
  AddDetailsList('Reports', plugin.reports);

  // free
  sl.Free;
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
  if Item.Index > Pred(PluginsList.Count) then
    exit;
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
procedure TMergeForm.DrawFlag(canvas: TCanvas; flag: TPluginFlag; x, y: integer);
var
  icon: TIcon;
begin
  icon := TIcon.Create;
  FlagList.GetIcon(flag.id, icon);
  canvas.Draw(x, y, icon);
  icon.Free;
end;

{ Draws the icons for @flags on @canvas in @Rect at @x and @y }
procedure TMergeForm.DrawPluginFlags(canvas: TCanvas; Rect: TRect; x, y: integer; flags: string);
var
  flag: TPluginFlag;
begin
  for flag in FlagsArray do begin
    if FlagNotSafe(Rect, x) then
      exit;
    if Pos(flag.char, flags) > 0 then begin
      DrawFlag(canvas, flag, x, y);
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
    // fixes drawing bug
    R.Right := R.Left + ListView_GetColumnWidth(ListView.Handle, ListView.Columns[i + 1].Index);
    x := R.Left;
    if ListView.Columns[i + 1].Caption = 'Flags' then
      DrawPluginFlags(ListView.Canvas, R, x, y, Item.SubItems[i])
    else
      ListView.Canvas.TextRect(R, x, y, Item.SubItems[i]);
  end;
end;

{ Show custom hint for plugin flags }
procedure TMergeForm.PluginsListViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pt: TPoint;
  li : TLIstItem;
  lvHitInfo: TLVHitTestInfo;
  hint : string;
  plugin: TPlugin;
begin
  pt := PluginsListView.ScreenToClient(Mouse.CursorPos);
  li := PluginsListView.GetItemAt(pt.x, pt.y) ;
  // if not over an item, exit
  if not Assigned(li) then
    exit;
  // prepare to get hit info for subitem
  FillChar(lvHitInfo, SizeOf(lvHitInfo), 0);
  lvHitInfo.pt := pt;
  // if not over subitem, exit
  if PluginsListView.Perform(LVM_SUBITEMHITTEST, 0, LParam(@lvHitInfo)) = -1 then
    exit;
  // if over flags subitem, display hint
  plugin := TPlugin(PluginsList[li.Index]);
  if lvHitInfo.iSubItem = 2 then begin
    hint := plugin.GetFlagsDescription;
    if hint <> '' then
      hint := plugin.filename + ' Flags'#13#10 + hint;
  end;
  if (hint <> LastHint) then begin
    LastHint := hint;
    PluginsListView.Hint := hint;
    Application.ActivateHint(Mouse.CursorPos) ;
  end;
end;

{******************************************************************************}
{ Plugins Popup Menu methods
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
  bPluginInMerge, bBlacklisted, bNeedsErrorCheck: boolean;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  bBlacklisted := false;
  bPluginInMerge := false;
  bNeedsErrorCheck := false;
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    plugin := PluginsList[i];
    if IS_BLACKLISTED in plugin.flags then
      bBlacklisted := true;
    if plugin.merge <> ' ' then
      bPluginInMerge := true;
    if plugin.errors.Count = 0 then
      bNeedsErrorCheck := true;
  end;

  PluginsPopupMenu.Items[0].Enabled := (not bBlacklisted) and (not bPluginInMerge);
  PluginsPopupMenu.Items[1].Enabled := (not bBlacklisted) and bPluginInMerge;
  PluginsPopupMenu.Items[2].Enabled := (not bBlacklisted);
  PluginsPopupMenu.Items[3].Enabled := (not bBlacklisted) and bLoaderDone
    and bNeedsErrorCheck;
  PluginsPopupMenu.Items[4].Enabled := (not bBlacklisted);
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
  ProgressForm := TProgressForm.Create(Self);
  ProgressForm.PopupParent := Self;
  Enabled := false;
  ProgressForm.SetTitle('Checking for errors');
  ProgressForm.Show;
  ProgressForm.ProgressBar.Max := 0;

  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    // skip blacklisted plugins and plugins that have already been checked
    if (IS_BLACKLISTED in plugin.flags) or (plugin.errors.Count > 0) then
      continue;
    ProgressForm.ProgressBar.Max := ProgressForm.ProgressBar.Max + StrToInt(plugin.numRecords);
    pluginsToCheck.Add(plugin);
  end;

  for i := 0 to Pred(pluginsToCheck.Count) do begin
    if bProgressCancel then continue;
    plugin := TPlugin(pluginsToCheck[i]);
    Tracker.Write('Checking for errors in '+plugin.filename);
    plugin.FindErrors;
    if bProgressCancel then Tracker.Write('Check for errors canceled.');
  end;

  // all done!
  if not bProgressCancel then Tracker.Write('All done!');
  ProgressForm.SaveLog;
  ProgressForm.SetProgress(ProgressForm.ProgressBar.Max);
  ProgressForm.Visible := false;
  FlashWindow(Application.Handle, True);
  ProgressForm.ShowModal;

  // clean up
  Enabled := true;
  pluginsToCheck.Free;
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

procedure TMergeForm.OpenPluginLocationItemClick(Sender: TObject);
var
  i: integer;
  listItem: TListItem;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // get plugin associated with merge item and remove it from merge
    plugin := TPlugin(PluginsList[i]);
    if DirectoryExists(plugin.dataPath) then
      ShellExecute(0, 'open', PChar(plugin.dataPath), '', '', SW_SHOWNORMAL);
  end;
end;

{******************************************************************************}
{ Merge List View Events
  Events involving the MergeListView control.  Events include:
  - UpdateMergeDetails
  - UpdateMerges
  - MergeListViewChange
  - MergeListViewData
  - MergeListViewDrawItem
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
  DetailsLabel.Caption := 'Merge Details';

  // get merge information
  merge := MergesList[MergeListView.ItemIndex];
  AddDetailsItem('Status', StatusArray[merge.status].desc, false);
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
  if merge.files.Count < 250 then begin
    sl := TStringList.Create;
    sl.Text := StringReplace(merge.files.Text, settings.mergeDirectory, '', [rfReplaceAll]);
    AddDetailsList('Files', sl);
    sl.Free;
  end
  else
    AddDetailsItem('Files', 'Too many files to display.');
  if merge.fails.Count < 250 then
    AddDetailsList('Fails', merge.fails)
  else
    AddDetailsItem('Fails', 'Too many fails to display.');
end;

procedure TMergeForm.UpdateMerges;
var
  i: integer;
  merge: TMerge;
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
  BuildButton.Enabled := bMergesToBuild and bLoaderDone;
  if not bLoaderDone then
    BuildButton.Hint := 'Background Loader not done!'
  else if not bMergesToBuild then
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
  if Item.Index > Pred(MergesList.Count) then
    exit;
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
    // fixes drawing error
    R.Right := R.Left + ListView_GetColumnWidth(ListView.Handle, ListView.Columns[i + 1].Index);
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
  - RebuildMergeItemClick
  - ReportOnMergeItemClick
  - OpenInExplorerItemClick
  - ForceRebuildItemClick
  - IgnoreRebuildItemClick
  - MergeListViewDblClick
  - MergeListViewKeyDown
}
{******************************************************************************}

procedure TMergeForm.MergesPopupMenuPopup(Sender: TObject);
var
  bNeverBuilt, bHasBuildStatus, bHasUpToDateStatus,
  bHasCheckStatus, bHasErrorStatus, bHasSelection: boolean;
  merge: TMerge;
  i, mergesSelected: Integer;
begin
  bNeverBuilt := false;
  bHasBuildStatus := false;
  bHasUpToDateStatus := false;
  bHasCheckStatus := false;
  bHasErrorStatus := false;
  mergesSelected := 0;

  // loop through list view to find selection
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    Inc(mergesSelected);
    // update booleans
    bNeverBuilt := bNeverBuilt or (merge.dateBuilt = 0);
    bHasBuildStatus := bHasBuildStatus or (merge.status in BuildStatuses);
    bHasUpToDateStatus := bHasUpToDateStatus or (merge.status in UpToDateStatuses);
    bHasCheckStatus := bHasCheckStatus or (merge.status = 10);
    bHasErrorStatus := bHasErrorStatus or (merge.status = 3) or (merge.status = 4);
  end;

  bHasSelection := (mergesSelected > 0);
  // change enabled state of MergesPopupMenu items based on booleans
  MergesPopupMenu.Items[1].Enabled := bHasSelection;
  MergesPopupMenu.Items[2].Enabled := bHasSelection and bHasCheckStatus and bLoaderDone;
  MergesPopupMenu.Items[3].Enabled := bHasSelection and bHasErrorStatus;
  MergesPopupMenu.Items[4].Enabled := bHasSelection;
  MergesPopupMenu.Items[5].Enabled := bHasSelection and (not bHasCheckStatus) and bLoaderDone;
  MergesPopupMenu.Items[6].Enabled := bHasSelection;
  MergesPopupMenu.Items[7].Enabled := bHasSelection and bHasUpToDateStatus;
  MergesPopupMenu.Items[8].Enabled := bHasSelection and bHasUpToDateStatus;
  MergesPopupMenu.Items[9].Enabled := bHasSelection and bHasBuildStatus;

  // handle build/rebuild menu item
  if (mergesSelected = 1) then begin
    if bNeverBuilt then
      MergesPopupMenu.Items[5].Caption := 'Build merge'
    else if bHasBuildStatus then
      MergesPopupMenu.Items[5].Caption := 'Rebuild merge'
    else begin
      MergesPopupMenu.Items[5].Enabled := false;
      MergesPopupMenu.Items[5].Caption := 'Rebuild merge';
    end;
  end
  else if (mergesSelected > 1) then begin
    if bNeverBuilt then
      MergesPopupMenu.Items[5].Caption := 'Build merges'
    else if bHasBuildStatus then
      MergesPopupMenu.Items[5].Caption := 'Rebuild merges'
    else begin
      MergesPopupMenu.Items[5].Enabled := false;
      MergesPopupMenu.Items[5].Caption := 'Rebuild merges';
    end;
  end;
end;

procedure TMergeForm.EditMergeItemClick(Sender: TObject);
var
  EditMerge: TEditForm;
  i, j: integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    // create EditForm
    EditMerge := TEditForm.Create(Self);
    EditMerge.merge := merge;
    if EditMerge.ShowModal = mrOk then begin
      merge := EditMerge.merge;
      // update plugin.merge properties
      for j := 0 to Pred(merge.plugins.Count) do begin
        plugin := PluginByFilename(merge.plugins[j]);
        if Assigned(plugin) then
          plugin.merge := merge.name;
      end;
    end;

    // free and repaint
    EditMerge.Free;
    MergeListView.Repaint;
  end;

  // update merge details and popup menu
  UpdateMergeDetails;
  UpdatePluginsPopupMenu;
end;

{ Check plugins in merge for errors }
procedure TMergeForm.CheckPluginsForErrorsItemClick(Sender: TObject);
var
  timeCost, i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
  timeCosts, pluginsToCheck: TList;
begin
  timeCosts := TList.Create;
  pluginsToCheck := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status = 10) then
      continue;

    // else loop through plugins
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      // skip plugins that have already been checked for errors
      if (plugin.errors.Count > 0) then continue;
      pluginsToCheck.Add(plugin);
      timeCost := StrToInt(plugin.numRecords);
      timeCosts.Add(Pointer(timeCost));
    end;
  end;

  // free and exit if no merges to check for errors
  if pluginsToCheck.Count = 0 then begin
    timeCosts.Free;
    pluginsToCheck.Free;
    exit;
  end;

  // Show progress form
  ProgressForm := TProgressForm.Create(Self);
  ProgressForm.PopupParent := Self;
  ProgressForm.SetTitle('Checking for errors');
  ProgressForm.Show;
  Enabled := false;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));

  // check merges for errors
  for i := 0 to Pred(pluginsToCheck.Count) do begin
    if bProgressCancel then continue;
    plugin := TPlugin(pluginsToCheck[i]);
    // check plugins for errors
    Tracker.Write('Checking for errors in '+plugin.filename);
    plugin.FindErrors;
    ProgressForm.SetProgress(IntegerListSum(timeCosts, i));
  end;

  // all done
  if not bProgressCancel then Tracker.Write('All done!');
  ProgressForm.SaveLog;
  ProgressForm.SetProgress(ProgressForm.ProgressBar.Max);
  ProgressForm.Visible := false;
  FlashWindow(Application.Handle, True);
  ProgressForm.ShowModal;

  // clean up
  Enabled := true;
  ProgressForm.Free;
  timeCosts.Free;
  pluginsToCheck.Free;
  UpdateMerges;
  PluginsListView.Repaint;
end;

{ Remove unloaded plugins and plugins with errors }
procedure TMergeForm.RemoveBadPluginsItemClick(Sender: TObject);
var
  i, j: integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    for j := Pred(merge.plugins.Count) downto 0 do begin
      plugin := PluginByFilename(merge.plugins[j]);
      if not Assigned(plugin) then begin
        merge.plugins.Delete(j);
        continue;
      end;
      if (plugin.errors.Count > 0) then
        if (plugin.errors[0] <> 'None.') then begin
          merge.plugins.Delete(j);
          plugin.merge := ' ';
        end;
    end;
  end;

  // update merge statuses and merge details
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
end;

procedure TMergeForm.DeleteMergeItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
  mergeNames: string;
  bApproved: boolean;
  mergesToDelete: TList;
begin
  // see how many merges the user selected
  bApproved := false;
  mergesToDelete := TList.Create;
  mergeNames := '';
  for i := 0 to Pred(MergeListView.Items.Count) do
    if MergeListView.Items[i].Selected then begin
      merge := TMerge(MergesList[i]);
      mergesToDelete.Add(merge);
      MergeListView.Items[i].Selected := false;
      mergeNames := mergeNames + #13#10'    - ' + merge.name;
    end;

  // show multi-merge prompt if multiple merges selected
  if mergesToDelete.Count > 0 then
    bApproved := MessageDlg('Are you sure you want to delete these merges?'+
      mergeNames, mtConfirmation, mbOKCancel, 0) = mrOk;

  // exit if user didn't approve deletion
  if not bApproved then
    exit;

  // clear details editor
  DetailsEditor.Strings.Clear;

  // loop through merges
  for i := Pred(mergesToDelete.Count) downto 0 do begin
    merge := TMerge(mergesToDelete[i]);
    MergeListView.Items.Count := MergeListView.Items.Count - 1;

    // remove merge from plugin merge properties
    for j := 0 to Pred(PluginsList.Count) do begin
      plugin := TPlugin(PluginsList[j]);
      if plugin.merge = merge.name then
        plugin.merge := ' ';
    end;

    // delete merge
    mergesToDelete.Delete(i);
    MergesList.Delete(MergesList.IndexOf(merge));
    merge.Free;
  end;

  // update merges
  UpdatePluginsPopupMenu;
  MergeListView.Repaint;
end;

procedure TMergeForm.BuildMergeItemClick(Sender: TObject);
var
  timeCost, i: Integer;
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
    if not (merge.status in BuildStatuses) then
      continue;

    // else calculate time cost and build merge
    timeCost := merge.GetTimeCost * 2;
    timeCosts.Add(Pointer(timeCost));
    merges.Add(merge);
  end;

  // free and exit if no merges to check for errors
  if merges.Count = 0 then begin
    timeCosts.Free;
    merges.Free;
    exit;
  end;

  // Show progress form
  ProgressForm := TProgressForm.Create(Self);
  ProgressForm.PopupParent := Self;
  ProgressForm.SetTitle('Merging');
  ProgressForm.Show;
  Enabled := false;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));
  Application.ProcessMessages;

  // build merges
  for i := 0 to Pred(merges.Count) do begin
    if bProgressCancel then continue;
    merge := TMerge(merges[i]);
    try
      if (merge.status in RebuildStatuses) then
        RebuildMerge(merge)
      else
        BuildMerge(merge);
    except
      on x : Exception do begin
        merge.status := 11;
        Tracker.Write('Exception: '+x.Message);
      end;
    end;
    Tracker.Write(' '#13#10);
    ProgressForm.SetProgress(IntegerListSum(timeCosts, i));
    if bProgressCancel then Tracker.Write('Merging canceled.');
  end;

  // display progress form after merging
  ProgressForm.SaveLog;
  ProgressForm.SetProgress(ProgressForm.ProgressBar.Max);
  ProgressForm.DetailsButtonClick(nil);
  ProgressForm.Visible := false;
  FlashWindow(Application.Handle, True);
  ProgressForm.ShowModal;

  // free memory
  Enabled := true;
  ProgressForm.Free;

  // update mpMergeForm
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
  DeleteDirectory(TempPath);
end;

procedure TMergeForm.ReportOnMergeItemClick(Sender: TObject);
var
  i, j: Integer;
  merge: TMerge;
  pluginsList: TList;
  plugin: TPlugin;
  ReportForm: TReportForm;
  bReportsSent: boolean;
begin
  pluginsList := TList.Create;

  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      pluginsList.Add(plugin);
    end;
  end;

  // report on all merges
  ReportForm := TReportForm.Create(Self);
  if pluginsList.Count > 0 then begin
    ReportForm.pluginsList := pluginsList;
    ReportForm.AppName := wbAppName;
    ReportForm.ShowModal;
  end;

  // Send reports to backend
  bReportsSent := SendReports(ReportForm.reportsList);
  if not bReportsSent then begin
    Logger.Write('Saving reports locally');
    SaveReports(ReportForm.reportsList, 'user\reports\');
  end
  else if settings.saveReportsLocally then begin
    Logger.Write('Saving reports locally');
    SaveReports(ReportForm.reportsList, 'user\reports\submitted\Submitted-');
  end;

  // clean up
  ReportForm.Free;
  pluginsList.Free;
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
    ShellExecute(0, 'open', PChar(path), '', '', SW_SHOWNORMAL);
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
    // if forced up to date, set to Ready to be rebuilt
    if merge.status = 6 then
      merge.status := 8
    // if normal up to date, set to Ready to rebuilt [forced]
    else if merge.status = 5 then
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
    // if force rebuild, set to Up to date
    if merge.status = 9 then
      merge.status := 5
    // if normal rebuild, set to Up to date [Forced]
    else if merge.status = 8 then
      merge.Status := 6;
  end;

  // update
  UpdateMerges;
  UpdateMergeDetails;
  MergeListView.Repaint;
end;

{ Double click to edit merge }
procedure TMergeForm.MergeListViewDblClick(Sender: TObject);
begin
  EditMergeItemClick(nil);
end;

{ Shortcut to delete merges using the delete key }
procedure TMergeForm.MergeListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HiWord(GetKeyState(vk_Delete)) <> 0 then
    DeleteMergeItemClick(nil);
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
begin
  if not bLoaderDone then begin
    Logger.Write('Loader not done!  Can''t merge yet!');
    exit;
  end;
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
  ProgressForm := TProgressForm.Create(Self);
  ProgressForm.PopupParent := Self;
  ProgressForm.SetTitle('Merging');
  ProgressForm.Show;
  Enabled := false;
  ProgressForm.ProgressBar.Max := IntegerListSum(timeCosts, Pred(timeCosts.Count));
  Application.ProcessMessages;

  // rebuild merges
  for i := 0 to Pred(merges.count) do begin
    if bProgressCancel then continue;
    merge := merges[i];
    if not (merge.status in BuildStatuses) then
      continue;
    try
      if merge.status = 7 then
        BuildMerge(merge)
      else
        RebuildMerge(merge);
    except
      on x : Exception do begin
        merge.status := 11;
        Tracker.Write('Exception: '+x.Message);
      end;
    end;
    Tracker.Write(' '#13#10);
    ProgressForm.SetProgress(IntegerListSum(timeCosts, i));
    if bProgressCancel then Tracker.Write('Merging canceled.');
  end;

  // display progress form after merging
  ProgressForm.SaveLog;
  ProgressForm.DetailsButtonClick(nil);
  ProgressForm.SetProgress(ProgressForm.ProgressBar.Max);
  ProgressForm.Visible := false;
  FlashWindow(Application.Handle, True);
  ProgressForm.ShowModal;

  // free memory
  Enabled := true;
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
  DictionaryForm := TDictionaryForm.Create(Self);
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

{ Options }

procedure TMergeForm.OptionsButtonClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
begin
  // Create and show options form
  OptionsForm := TOptionsForm.Create(Self);
  OptionsForm.ShowModal;
  OptionsForm.Free;

  // initialize MO if usingMO changed
  if settings.usingMO then
    ModOrganizerInit;

  // update owner draw if changed
  PluginsListView.OwnerDraw := not settings.simplePluginsView;

  // if user selected to change game mode, close application
  if bChangeGameMode then
    Close;

  // if user selected to update program, close application
  if bInstallUpdate then begin
    bInstallUpdate := UpdateProgram;
    if bInstallUpdate then
      Close;
  end;
end;

{ Update }
procedure TMergeForm.UpdateButtonClick(Sender: TObject);
begin
  if bProgramUpdate and TCPClient.Connected then begin
    if not DownloadProgram then
      exit;
    bInstallUpdate := UpdateProgram;
    if bInstallUpdate then
      Close;
  end;
end;

{ Help }
procedure TMergeForm.HelpButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

end.
