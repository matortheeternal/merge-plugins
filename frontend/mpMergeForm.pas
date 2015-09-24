unit mpMergeForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl, Menus, Grids,
  ValEdit, ShellAPI, StrUtils, Clipbrd,
  // indy units
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  // third party libraries
  superobject, W7Taskbar,
  // mte components
  mteHelpers, mteTracker, mteLogger, mteProgressForm,
  // mp units
  mpFrontend, mpThreads, mpMerge, mpDictionaryForm, mpOptionsForm,
  mpSplashForm, mpEditForm, mpReportForm,
  // tes5edit units
  wbBSA, wbHelpers, wbInterface, wbImplementation;

type
  TMergeForm = class(TForm)
    // GENERIC
    XPManifest: TXPManifest;
    IconList: TImageList;
    FlagList: TImageList;
    DoubleIconList: TImageList;
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
    NewMergeItem: TMenuItem;
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
    CleanMergeItem: TMenuItem;
    ReconnectTimer: TTimer;
    Heartbeat: TTimer;
    RefreshTimer: TTimer;
    LogListView: TListView;
    LogPopupMenu: TPopupMenu;
    FilterGroupItem: TMenuItem;
    CopyToClipboardItem: TMenuItem;
    SaveAndClearItem: TMenuItem;
    ToggleAutoScrollItem: TMenuItem;
    StatusPanel: TPanel;
    StatusPanelMessage: TPanel;
    StatusPanelBlocking: TPanel;
    StatusPanelProgram: TPanel;
    StatusPanelDictionary: TPanel;
    StatusPanelConnection: TPanel;
    StatusPanelMerges: TPanel;
    StatusPanelLanguage: TPanel;
    StatusPanelVersion: TPanel;
    ImageBlocked: TImage;
    ImageDisconnected: TImage;
    ImageBuild: TImage;
    ImageDictionaryUpdate: TImage;
    ImageProgramUpdate: TImage;
    ImageConnected: TImage;
    FilterLabelItem: TMenuItem;
    DetailsPopupMenu: TPopupMenu;
    DetailsCopyToClipboardItem: TMenuItem;
    IgnoreErrorsItem: TMenuItem;
    DoNotMergeItem: TMenuItem;

    // MERGE FORM EVENTS
    procedure UpdateLog;
    procedure LogMessage(const group, &label, text: string);
    procedure FormCreate(Sender: TObject);
    procedure InitDone;
    procedure FormShow(Sender: TObject);
    procedure LoaderDone;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SaveDone;
    procedure ProgressDone;
    procedure AutoUpdate;
    procedure OnTimer(Sender: TObject);
    procedure OnHeartbeatTimer(Sender: TObject);
    procedure OnRepaintTimer(Sender: TObject);
    procedure ShowAuthorizationMessage;
    procedure UpdateStatusPanel;
    procedure UpdateListViews;
    // DETAILS EDITOR EVENTS
    function AddDetailsItem(name, value: string; editable: boolean = false):
      TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList; editable: boolean = false);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateApplicationDetails;
    procedure DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // PLUGINS LIST VIEW EVENTS
    procedure UpdatePluginDetails;
    procedure AddPluginsToMerge(var merge: TMerge);
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
    // PLUGINS POPUP MENU EVENTS
    procedure PluginsPopupMenuPopup(Sender: TObject);
    procedure UpdatePluginsPopupMenu;
    procedure AddToNewMergeClick(Sender: TObject);
    procedure AddToMergeClick(Sender: TObject);
    procedure CheckForErrorsClick(Sender: TObject);
    procedure RemoveFromMergeClick(Sender: TObject);
    procedure OpenPluginLocationItemClick(Sender: TObject);
    procedure ReportOnPluginClick(Sender: TObject);
    // MERGE LIST VIEW EVENTS
    procedure UpdateMergeDetails;
    procedure UpdateMerges;
    function NewMerge: TMerge;
    procedure MergeListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MergeListViewData(Sender: TObject; Item: TListItem);
    procedure MergeListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    // MERGES POPUP MENU EVENTS
    procedure MergesPopupMenuPopup(Sender: TObject);
    procedure EditMergeItemClick(Sender: TObject);
    procedure BuildMergeItemClick(Sender: TObject);
    procedure CheckPluginsForErrorsItemClick(Sender: TObject);
    procedure CleanMergeItemClick(Sender: TObject);
    procedure DeleteMergeItemClick(Sender: TObject);
    procedure ReportOnMergeItemClick(Sender: TObject);
    procedure OpenInExplorerItemClick(Sender: TObject);
    procedure ForceRebuildItemClick(Sender: TObject);
    procedure IgnoreRebuildItemClick(Sender: TObject);
    procedure MergeListViewDblClick(Sender: TObject);
    procedure MergeListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    // LOG LIST VIEW EVENTS
    procedure LogListViewData(Sender: TObject; Item: TListItem);
    procedure LogListViewDrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
    // LOG POPUP MENU EVENTS
    procedure LogPopupMenuPopup(Sender: TObject);
    procedure ToggleGroupFilter(Sender: TObject);
    procedure ToggleLabelFilter(Sender: TObject);
    procedure CopyToClipboardItemClick(Sender: TObject);
    procedure SaveAndClearItemClick(Sender: TObject);
    // QUICKBAR BUTTON EVENTS
    procedure CreateMergeButtonClick(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ToggleAutoScrollItemClick(Sender: TObject);
    procedure DetailsCopyToClipboardItemClick(Sender: TObject);
    procedure IgnoreErrorsItemClick(Sender: TObject);
    procedure DoNotMergeItemClick(Sender: TObject);
    procedure ImageDisconnectedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  splash: TSplashForm;
  MergeForm: TMergeForm;
  LastHint: string;
  LastURLTime: double;
  bMergesToBuild, bMergesToCheck, bAutoScroll, bCreated, bClosing: boolean;
  pForm: TProgressForm;

implementation

{$R *.dfm}


{******************************************************************************}
{ Merge Form Events
  Events for the Merge Form.
  - UpdateLog
  - LogMessage
  - ProgressMessage
  - FormCreate
  - FormShow
  - LoaderDone
  - FormClose
}
{******************************************************************************}

procedure TMergeForm.UpdateLog;
var
  bLogActive: boolean;
begin
  LogListView.Items.Count := Log.Count;
  bLogActive := PageControl.ActivePage = LogTabSheet;
  // autoscroll if active
  if bAutoScroll and bLogActive then begin
    //LogListView.ClearSelection;
    //LogListView.Items[Pred(LogListView.Items.Count)].MakeVisible(false);
    SendMessage(LogListView.Handle, WM_VSCROLL, SB_LINEDOWN, 0);
  end;
  // correct width if active
  if bLogActive then
    CorrectListViewWidth(LogListView);
end;

{ Prints a message to the log }
procedure TMergeForm.LogMessage(const group, &label, text: string);
var
  msg: TLogMessage;
begin
  msg := TLogMessage.Create(
    FormatDateTime('hh:nn:ss', Now),
    FormatDateTime('hh:nn:ss', Now - AppStartTime),
    group, &label, text);
  BaseLog.Add(msg);

  // if message is enabled, add to log
  if MessageEnabled(msg) then begin
    Log.Add(msg);
    // if merge form is created, update log list view
    if bCreated then
      UpdateLog;
  end;
end;

procedure ProgressMessage(const s: string);
begin
  if s = '' then
    exit;
  Logger.Write(xEditLogGroup, xEditLogLabel, s);
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TMergeForm.FormCreate(Sender: TObject);
begin
  // INITIALIAZE BASE
  bCreated := false;
  AppStartTime := Now;
  InitLog;
  Logger.OnLogEvent := LogMessage;
  //bAutoScroll := true;
  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);
  xEditLogGroup := 'LOAD';
  xEditLogLabel := 'Plugins';
  wbProgressCallback := ProgressMessage;

  // CREATE SPLASH
  splash := TSplashForm.Create(nil);
  try
    InitCallback := InitDone;
    UpdateCallback := AutoUpdate;
    TInitThread.Create;
    splash.ShowModal;
  finally
    splash.Free;
  end;

  // finalize
  bCreated := true;
end;

procedure TMergeForm.InitDone;
begin
  splash.ModalResult := mrOk;
end;

// Force PluginsListView to autosize columns
procedure TMergeForm.FormShow(Sender: TObject);
begin
  // HANDLE AUTO-UPDATE
  if bInstallUpdate then begin
    Logger.Write('CLIENT', 'Disconnect', 'Disconnecting...');
    TCPClient.Disconnect;
    bAllowClose := true;
    bClosing := true;
    Logger.Write('GENERAL', 'Update', 'Restarting.');
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
    Close;
  end;

  // GUI ICONS
  //Tracker.Write('Loading Icons');
  NewButton.Flat := true;
  BuildButton.Flat := true;
  ReportButton.Flat := true;
  DictionaryButton.Flat := true;
  OptionsButton.Flat := true;
  UpdateButton.Flat := true;
  HelpButton.Flat := true;
  IconList.GetBitmap(0, NewButton.Glyph);
  DoubleIconList.GetBitmap(0, BuildButton.Glyph);
  IconList.GetBitmap(2, ReportButton.Glyph);
  IconList.GetBitmap(3, DictionaryButton.Glyph);
  IconList.GetBitmap(4, OptionsButton.Glyph);
  IconList.GetBitmap(5, UpdateButton.Glyph);
  IconList.GetBitmap(6, HelpButton.Glyph);

  // STATUSBAR VALUES
  StatusPanelLanguage.Caption := settings.language;
  StatusPanelVersion.Caption := 'v'+ProgramVersion;

  // UPDATE GUI
  PluginsListView.OwnerDraw := not settings.simplePluginsView;
  PluginsListView.Items.Count := PluginsList.Count;
  UpdateLog;
  UpdateMerges;
  UpdatePluginsPopupMenu;

  // ATTEMPT TO CONNECT TO SERVER
  if (not bConnecting) and (not TCPClient.Connected) then
    ConnectToServer;

  // START BACKGROUND LOADER
  LoaderCallback := LoaderDone;
  SetTaskbarProgressState(tbpsIndeterminate);
  TLoaderThread.Create;

  // CORRECT LIST VIEW WIDTHS
  CorrectListViewWidth(MergeListView);
  CorrectListViewWidth(PluginsListView);

  // SHOW LOADER HINT
  StatusPanelMessage.Caption := 'Background loader in progress.';
end;

procedure TMergeForm.LoaderDone;
begin
  SetTaskbarProgressState(tbpsNone);
  StatusPanelMessage.Caption := 'Background loader finished.';
  xEditLogGroup := 'GENERAL';
  xEditLogLabel := 'xEdit';
  UpdateMerges;
  FlashWindow(Application.Handle, True);
end;

procedure TMergeForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := bAllowClose;
  if not bClosing then begin
    bClosing := true;
    Enabled := false;

    // show progress form
    pForm := TProgressForm.Create(Self);
    pForm.LogPath := LogPath;
    pForm.PopupParent := Self;
    pForm.Caption := 'Closing';
    pForm.MaxProgress(PluginsList.Count + MergesList.Count + 2);
    pForm.Show;

    // start save thread
    SaveCallback := SaveDone;
    TSaveThread.Create;
  end;
end;

procedure TMergeForm.SaveDone;
begin
  // clean up pForm, close form
  pForm.SetProgress(pForm.ProgressBar.Max);
  pForm.SaveLog;
  pForm.Free;

  // restart program if update applied
  if bInstallUpdate then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
  // restart program if user wants merge profile change
  if bChangeMergeProfile then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);

  // allow close and close
  bAllowClose := true;
  Close;
end;

procedure TMergeForm.ProgressDone;
begin
  xEditLogGroup := 'GENERAL';
  pForm.SaveLog;
  pForm.Visible := false;
  FlashWindow(Application.Handle, True);
  pForm.ShowModal;
  pForm.Free;
  Enabled := true;
  ShowWindow(Application.Handle, SW_RESTORE);
  SetForegroundWindow(Application.Handle);

  // free lists
  if Assigned(timeCosts) then timeCosts.Free;
  if Assigned(pluginsToCheck) then pluginsToCheck.Free;
  if Assigned(mergesToBuild) then mergesToBuild.Free;

  // update
  UpdateListViews;
  UpdateMerges;
end;

procedure TMergeForm.AutoUpdate;
begin
  if settings.updateDictionary then begin
    // update dictionary
    if bDictionaryUpdate and UpdateDictionary then begin
      status := TmpStatus.Create;
      CompareStatuses;
    end;
  end;
  if settings.updateProgram then begin
    // update program
    if bProgramUpdate and DownloadProgram then
      bInstallUpdate := UpdateProgram;
  end;
end;

procedure TMergeForm.OnTimer(Sender: TObject);
begin
  if not (TCPClient.Connected or bConnecting or bClosing) then
    ConnectToServer;
end;

procedure TMergeForm.OnRepaintTimer(Sender: TObject);
begin
  if not bClosing then UpdateStatusPanel;
end;

procedure TMergeForm.OnHeartbeatTimer(Sender: TObject);
begin
  try
    if TCPClient.IOHandler.Opened and
    not (bConnecting or bClosing or ServerAvailable) then
      raise Exception.Create('Connection unavailable');
  except
    on x : Exception do begin
      if Assigned(TCPClient) and Assigned(TCPClient.IOHandler) then begin
        Logger.Write('CLIENT', 'Connection', 'Connection to server lost.');
        TCPClient.IOHandler.CloseGracefully;
      end;
    end;
  end;
end;

procedure TMergeForm.ShowAuthorizationMessage;
begin
  if bAuthorized then begin
    Logger.Write('CLIENT', 'Login', 'Authorized');
  end
  else begin
    Logger.Write('CLIENT', 'Login', 'Not authorized');
  end;
end;

procedure TMergeForm.UpdateStatusPanel;
begin
  ImageBlocked.Visible := not bLoaderDone;
  ImageConnected.Visible := TCPClient.Connected;
  ImageDisconnected.Visible := not TCPClient.Connected;
  ImageBuild.Visible := bLoaderDone and bMergesToBuild;
  ImageDictionaryUpdate.Visible := bDictionaryUpdate;
  ImageProgramUpdate.Visible := bProgramUpdate;
end;

procedure TMergeForm.UpdateListViews;
begin
  if PageControl.ActivePageIndex = 0 then begin
    UpdatePluginDetails;
    PluginsListView.Repaint;
  end;
  if PageControl.ActivePageIndex = 1 then begin
    UpdateMergeDetails;
    MergeListView.Repaint;
  end;
  if PageControl.ActivePageIndex = 2 then
    LogListView.Repaint;
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
    0: begin
      UpdatePluginDetails;
      CorrectListViewWidth(PluginsListView);
    end;
    1: begin
      UpdateMergeDetails;
      CorrectListViewWidth(MergeListView);
    end;
    2: begin
      UpdateApplicationDetails;
      CorrectListViewWidth(LogListView);
    end;
  end;
end;

procedure TMergeForm.UpdateApplicationDetails;
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
  AddDetailsItem('Times run', IntToStr(statistics.timesRun + sessionStatistics.timesRun));
  AddDetailsItem('Merges built', IntToStr(statistics.mergesBuilt + sessionStatistics.mergesBuilt));
  AddDetailsItem('Plugins checked for errors', IntToStr(statistics.pluginsChecked + sessionStatistics.pluginsChecked));
  AddDetailsItem('Plugins merged', IntToStr(statistics.pluginsMerged + sessionStatistics.pluginsMerged));
  AddDetailsItem('Reports submitted', IntToStr(statistics.reportsSubmitted + sessionStatistics.reportsSubmitted));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Website', 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem('API Credits', 'superobject, TurboPower Abbrevia, xEdit');
  AddDetailsItem('xEdit Version', xEditVersion);
  AddDetailsItem('xEdit Credits', 'zilav, hlp, Sharlikran, ElminsterAU');
  AddDetailsItem('Testers', Wordwrap(ProgramTesters, 70));
  AddDetailsItem('Translators', Wordwrap(ProgramTranslators, 70));
end;

procedure TMergeForm.DetailsCopyToClipboardItemClick(Sender: TObject);
var
  i: Integer;
  name, value, previousName, previousValue: string;
  sl: TStringList;
begin
  sl := TStringList.Create;

  // build stringlist of formatted name value pairs with special formatting for
  // empty names and empty values
  name := ' ';
  value := ' ';
  for i := 0 to Pred(DetailsEditor.Strings.Count) do begin
    previousName := name;
    name := DetailsEditor.Strings.Names[i];
    previousValue := value;
    value := DetailsEditor.Strings.ValueFromIndex[i];
    if (name <> ' ') then
      sl.Add(Format('%s: %s', [name, value]))
    else if (value <> ' ') then begin
      if (previousName <> ' ') then begin
        sl[sl.Count - 1] := previousName + ':';
        sl.Add('- '+previousValue);
      end;
      sl.Add('- '+value);
    end
    else
      sl.Add(' ');
  end;

  // copy to clipboard
  Clipboard.AsText := sl.Text;
  sl.Free;
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

procedure TMergeForm.AddPluginsToMerge(var merge: TMerge);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // loop through plugins list, adding selected plugins to merge
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    // skip plugins that have been been disallowed by the user
    if (plugin.bDisallowMerging) then
      continue;
    // add plugin to merge
    Logger.Write('PLUGIN', 'Merge', 'Added '+plugin.filename+' to merge '+merge.name);
    if not plugin.hasData then
      plugin.GetData;
    merge.plugins.AddObject(plugin.filename, TObject(i));
    plugin.merge := merge.name;
  end;

  // update and repaint
  UpdateMerges;
  UpdateListViews;
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
  FlagList.GetIcon(Ord(flag.id), icon);
  canvas.Draw(x, y - 1, icon);
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
  li : TListItem;
  lvHitInfo: TLVHitTestInfo;
  hint : string;
  plugin: TPlugin;
begin
  pt := PluginsListView.ScreenToClient(Mouse.CursorPos);
  li := PluginsListView.GetItemAt(pt.x, pt.y);
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
  bPluginInMerge, bBlacklisted, bAllNeedErrorCheck, bHasErrors, bAllIgnoreErrors,
  bAllDoNotMerge, bAllPluginsInMerge, bAllPluginsHaveErrors: boolean;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // initialize selection booleans
  bBlacklisted := false;
  bPluginInMerge := false;
  bHasErrors := false;
  bAllNeedErrorCheck := true;
  bAllDoNotMerge := true;
  bAllIgnoreErrors := true;
  bAllPluginsInMerge := true;
  bAllPluginsHaveErrors := true;

  // loop through selection
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    plugin := PluginsList[i];
    bBlacklisted := bBlacklisted or (IS_BLACKLISTED in plugin.flags);
    bPluginInMerge := bPluginInMerge or plugin.IsInMerge;
    bHasErrors := bHasErrors or plugin.HasErrors;
    // ALL
    bAllNeedErrorCheck := bAllNeedErrorCheck and not plugin.HasBeenCheckedForErrors;
    bAllIgnoreErrors := bAllIgnoreErrors and plugin.bIgnoreErrors;
    bAllDoNotMerge := bAllDoNotMerge and plugin.bDisallowMerging;
    bAllPluginsInMerge := bAllPluginsInMerge and plugin.IsInMerge;
    bAllPluginsHaveErrors := bAllPluginsHaveErrors and plugin.HasErrors;
  end;

  // toggle menu item captions
  if bAllDoNotMerge then
    PluginsPopupMenu.Items[5].Caption := 'Allow merging'
  else
    PluginsPopupMenu.Items[5].Caption := 'Disallow merging';
  if bAllIgnoreErrors then
    PluginsPopupMenu.Items[4].Caption := 'Unignore errors'
  else
    PluginsPopupMenu.Items[4].Caption := 'Ignore errors';


  // disable/enable menu items
  PluginsPopupMenu.Items[0].Enabled := not (bBlacklisted or bPluginInMerge or bAllDoNotMerge);
  PluginsPopupMenu.Items[1].Enabled := bAllPluginsInMerge;
  PluginsPopupMenu.Items[2].Enabled := not bBlacklisted;
  PluginsPopupMenu.Items[3].Enabled := bLoaderDone and bAllNeedErrorCheck and not bBlacklisted;
  PluginsPopupMenu.Items[4].Enabled := bHasErrors and not bBlacklisted;
  PluginsPopupMenu.Items[5].Enabled := not (bBlacklisted or bPluginInMerge);
  PluginsPopupMenu.Items[6].Enabled := not bBlacklisted;
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
begin
  MenuItem := TMenuItem(Sender);
  merge := TMerge(MergesList[MenuItem.MenuIndex - 1]);
  AddPluginsToMerge(merge);
end;

procedure TMergeForm.AddToNewMergeClick(Sender: TObject);
var
  merge: TMerge;
begin
  merge := NewMerge;
  if Assigned(merge) then
    AddPluginsToMerge(merge);
end;

procedure TMergeForm.CheckForErrorsClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // create lists
  pluginsToCheck := TList.Create;
  timeCosts := TStringList.Create;

  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    // skip blacklisted plugins and plugins that have already been checked
    if (IS_BLACKLISTED in plugin.flags) or (plugin.errors.Count > 0) then
      continue;
    timeCosts.Add(plugin.numRecords);
    pluginsToCheck.Add(plugin);
  end;

  // prepare progress form
  self.Enabled := false;
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := 'Checking for errors';
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start error check thread
  ErrorCheckCallback := ProgressDone;
  TErrorCheckThread.Create;
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
  UpdateListViews
end;

procedure TMergeForm.ReportOnPluginClick(Sender: TObject);
var
  i: Integer;
  PluginsToReport: TList;
  plugin: TPlugin;
  ReportForm: TReportForm;
  bReportsSent, bModalOK: boolean;
begin
  PluginsToReport := TList.Create;
  bModalOK := false;

  // loop through plugins
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    if not PluginsListView.Items[i].Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    PluginsToReport.Add(plugin);
  end;

  // report on all merges
  ReportForm := TReportForm.Create(Self);
  if PluginsToReport.Count > 0 then begin
    ReportForm.pluginsToReport := PluginsToReport;
    ReportForm.AppName := wbAppName;
    bModalOK := ReportForm.ShowModal = mrOk;
  end;

  // Send reports to backend
  if bModalOK then begin
    bReportsSent := SendReports(ReportForm.reportsList);
    if not bReportsSent then begin
      Logger.Write('CLIENT', 'Reports', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\');
    end
    else begin
      Logger.Write('CLIENT', 'Reports', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\submitted\');
    end;
  end;

  // clean up
  ReportForm.Free;
  PluginsToReport.Free;
end;

procedure TMergeForm.IgnoreErrorsItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // ignore errors in plugin if it has been checked for errors
    plugin := TPlugin(PluginsList[i]);
    if not plugin.HasBeenCheckedForErrors then
      continue;
    plugin.bIgnoreErrors := not plugin.bIgnoreErrors;
    plugin.GetFlags;

    // log message
    if plugin.bIgnoreErrors then
      Logger.Write('PLUGIN', 'Errors', 'Ignoring errors in '+plugin.filename)
    else
      Logger.Write('PLUGIN', 'Errors', 'No longer ignoring errors in '+plugin.filename);
  end;

  // update plugins list view
  UpdateMerges;
  UpdateListViews;
end;

procedure TMergeForm.DoNotMergeItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    // only process selected list items
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;

    // ignore errors in plugin
    plugin := TPlugin(PluginsList[i]);
    plugin.bDisallowMerging := not plugin.bDisallowMerging;
    plugin.GetFlags;

    // log message
    if plugin.bDisallowMerging then
      Logger.Write('PLUGIN', 'Merge', 'Disallowed merging of '+plugin.filename)
    else
      Logger.Write('PLUGIN', 'Merge', 'Allowed merging of '+plugin.filename);
  end;

  // update plugins list view
  UpdateListViews;
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

    // open plugin location in explorer if it exists
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
  AddDetailsItem('Status', StatusArray[Ord(merge.status)].desc, false);
  AddDetailsItem('Merge name', merge.name, true);
  AddDetailsItem('Filename', merge.filename, true);
  AddDetailsItem('Plugin count', IntToStr(merge.plugins.Count));
  AddDetailsItem('Date built', DateBuiltString(merge.dateBuilt));
  AddDetailsList('Plugins', merge.plugins);
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Merge method', merge.method, false);
  AddDetailsItem('Renumbering', merge.renumbering, false);
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
    if (merge.status = msCheckErrors) then
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

function TMergeForm.NewMerge: TMerge;
var
  merge: TMerge;
  EditMerge: TEditForm;
begin
  Result := nil;
  merge := CreateNewMerge(MergesList);

  // edit merge immediately after its creation
  EditMerge := TEditForm.Create(Self);
  EditMerge.merge := merge;
  if EditMerge.ShowModal = mrOk then begin
    merge := EditMerge.merge;
    LogMessage('MERGE', 'New', 'Created new merge '+merge.name);
    // add merge to list and update views
    MergesList.Add(merge);
    UpdateMerges;
    MergeListView.Repaint;
    UpdatePluginsPopupMenu;
    // set result
    Result := merge;
  end;

  // add and update merge
  EditMerge.Free;
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
  MergeListView.Canvas.Font.Color := StatusArray[Ord(merge.status)].color;
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
{ LogListView methods
}
{******************************************************************************}

procedure TMergeForm.LogListViewData(Sender: TObject; Item: TListItem);
var
  msg: TLogMessage;
begin
  if (Item.Index > Pred(Log.Count)) then
    exit;
  msg := TLogMessage(Log[Item.Index]);
  Item.Caption := msg.time;
  Item.SubItems.Add(msg.appTime);
  Item.SubItems.Add(msg.group);
  Item.SubItems.Add(msg.&label);
  Item.SubItems.Add(msg.text);

  // handle coloring
  if (msg.group = 'GENERAL') then
    LogListView.Canvas.Font.Color := settings.generalMessageColor
  else if (msg.group = 'LOAD') then
    LogListView.Canvas.Font.Color := settings.loadMessageColor
  else if (msg.group = 'CLIENT') then
    LogListView.Canvas.Font.Color := settings.clientMessageColor
  else if (msg.group = 'MERGE') then
    LogListView.Canvas.Font.Color := settings.mergeMessageColor
  else if (msg.group = 'PLUGIN') then
    LogListView.Canvas.Font.Color := settings.pluginMessageColor
  else if (msg.group = 'ERROR') then
    LogListView.Canvas.Font.Color := settings.errorMessageColor;
end;

procedure TMergeForm.LogListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
  msg: string;
  map: TStringList;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  // prepare map
  map := TStringList.Create;
  map.Values[ListView.Columns[0].Caption] := Item.Caption;
  for i := 0 to Pred(Item.SubItems.Count) do
    map.Values[ListView.Columns[i + 1].Caption] := Item.SubItems[i];

  // prepare text rect
  R := Rect;
  R.Right := R.Left + ListView.Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;

  // draw message
  msg := ApplyTemplate(settings.logMessageTemplate, map);
  ListView.Canvas.TextRect(R, x, y, msg);

  // clean up
  map.Free;
end;


{******************************************************************************}
{ Log Popup Menu events
  - LogPopupMenuPopup
  - FilterInitItemClick
  - FilterSqlItemClick
  - FilterServerItemClick
  - FilterDataItemClick
  - FilterErrorItemClick
  - CopyToClipboardItemClick
  - SaveAndClearItemClick
}
{******************************************************************************}

function EnableStr(var b: boolean): string;
begin
  Result := IfThen(not b, 'Enable', 'Disable');
end;

procedure TMergeForm.LogPopupMenuPopup(Sender: TObject);
var
  i: Integer;
  item: TMenuItem;
  filter: TFilter;
begin
  // rebuild group filter items
  FilterGroupItem.Clear;
  for i := 0 to Pred(GroupFilters.Count) do begin
    filter := TFilter(GroupFilters[i]);
    item := TMenuItem.Create(FilterGroupItem);
    item.Caption := EnableStr(filter.enabled) + ' ' + filter.group;
    item.OnClick := ToggleGroupFilter;
    FilterGroupItem.Add(item);
  end;

  // rebuild label filter items
  FilterLabelItem.Clear;
  for i := 0 to Pred(LabelFilters.Count) do begin
    filter := TFilter(LabelFilters[i]);
    item := TMenuItem.Create(FilterLabelItem);
    item.Caption := EnableStr(filter.enabled) + ' ' + filter.group + ', ' + filter.&label;
    item.OnClick := ToggleLabelFilter;
    FilterLabelItem.Add(item);
  end;

  // toggle copy to clipboard item based on whether or not log items are selected
  CopyToClipboardItem.Enabled := Assigned(LogListView.Selected);

  // rename toggle auto scroll item based on whether or not auto scroll is enabled
  LogPopupMenu.Items[3].Caption := EnableStr(bAutoScroll) + ' auto scroll';
end;

// toggles a group filter for the LogListView
procedure TMergeForm.ToggleGroupFilter(Sender: TObject);
var
  index: integer;
  filter: TFilter;
begin
  index := FilterGroupItem.IndexOf(TMenuItem(Sender));
  filter := GroupFilters[index];
  filter.enabled := not filter.enabled;
  LogListView.Items.Count := 0;
  RebuildLog;
  LogListView.Items.Count := Log.Count;
end;

// toggles a label filter for the LogListView
procedure TMergeForm.ToggleLabelFilter(Sender: TObject);
var
  index: integer;
  filter: TFilter;
begin
  index := FilterLabelItem.IndexOf(TMenuItem(Sender));
  filter := LabelFilters[index];
  filter.enabled := not filter.enabled;
  LogListView.Items.Count := 0;
  RebuildLog;
  LogListView.Items.Count := Log.Count;
end;

// toggles auto scroll for the LogListView
procedure TMergeForm.ToggleAutoScrollItemClick(Sender: TObject);
begin
  bAutoScroll := not bAutoScroll;
end;

procedure TMergeForm.CopyToClipboardItemClick(Sender: TObject);
var
  i: Integer;
  sl: TStringList;
  msg: TLogMessage;
begin
  sl := TStringList.Create;

  // put selected messages in stringlist
  for i := 0 to Pred(Log.Count) do begin
    if not LogListView.Items[i].Selected then
      continue;

    msg := TLogMessage(Log[i]);
    sl.Add(Format('[%s] (%s) %s: %s', [msg.time, msg.group, msg.&label, msg.text]));
  end;

  // put stringlist in clipboard, then free
  Clipboard.AsText := sl.Text;
  sl.Free;
end;

procedure TMergeForm.SaveAndClearItemClick(Sender: TObject);
begin
  SaveLog(BaseLog);
  LogListView.Items.Count := 0;
  BaseLog.Clear;
  Log.Clear;
  LogMessage('GENERAL', 'Log', 'Saved and cleared log.');
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
    bHasCheckStatus := bHasCheckStatus or (merge.status = msCheckErrors);
    bHasErrorStatus := bHasErrorStatus or (merge.status in ErrorStatuses);
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
    Logger.Write('MERGE', 'Edit', 'Editing '+merge.name);
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
  i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  timeCosts := TStringList.Create;
  pluginsToCheck := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status = msCheckErrors) then
      continue;

    // else loop through plugins
    Logger.Write('MERGE', 'Check', 'Checking '+merge.name+' for errors');
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      // skip plugins that have already been checked for errors
      if (plugin.errors.Count > 0) then continue;
      pluginsToCheck.Add(plugin);
      timeCosts.Add(plugin.numRecords);
    end;
  end;

  // free and exit if no merges to check for errors
  if pluginsToCheck.Count = 0 then begin
    timeCosts.Free;
    pluginsToCheck.Free;
    exit;
  end;

  // Show progress form
  self.Enabled := false;
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := 'Checking for errors';
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start error checking thread
  ErrorCheckCallback := ProgressDone;
  TErrorCheckThread.Create;
end;

{ Remove unloaded plugins and plugins with errors }
procedure TMergeForm.CleanMergeItemClick(Sender: TObject);
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
    Logger.Write('MERGE', 'Clean', 'Cleaning '+merge.name);
    // remove plugins that aren't loaded or have errors
    for j := Pred(merge.plugins.Count) downto 0 do begin
      plugin := PluginByFilename(merge.plugins[j]);
      if not Assigned(plugin) then begin
        Logger.Write('MERGE', 'Clean', 'Removing '+merge.plugins[j]+', plugin not loaded');
        merge.plugins.Delete(j);
        continue;
      end;
      if plugin.HasErrors and (not plugin.bIgnoreErrors) then begin
        Logger.Write('MERGE', 'Clean', 'Removing '+merge.plugins[j]+', plugin has errors');
        merge.Remove(plugin);
      end;
      if plugin.bDisallowMerging then begin
        Logger.Write('MERGE', 'Clean', 'Removing '+merge.plugins[j]+', plugin marked as do not merge');
        merge.Remove(plugin);
      end;
      if IS_BLACKLISTED in plugin.flags then begin
        Logger.Write('MERGE', 'Clean', 'Removing '+merge.plugins[j]+', plugin marked as blacklisted');
        merge.Remove(plugin);
      end;
    end;
  end;

  // update
  UpdateMerges;
  UpdateListViews;
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
    Logger.Write('MERGE', 'Delete', 'Deleting merge '+merge.name);
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
begin
  timeCosts := TStringList.Create;
  mergesToBuild := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status in BuildStatuses) then
      continue;

    // else calculate time cost and build merge
    Logger.Write('MERGE', 'Build', 'Building '+merge.name);
    timeCost := merge.GetTimeCost * 2;
    timeCosts.Add(IntToStr(timeCost));
    mergesToBuild.Add(merge);
  end;

  // free and exit if no merges to check for errors
  if mergesToBuild.Count = 0 then begin
    timeCosts.Free;
    mergesToBuild.Free;
    exit;
  end;

  // Show progress form
  self.Enabled := false;
  xEditLogGroup := 'MERGE';
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := 'Merging';
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start merge thread
  MergeCallback := ProgressDone;
  TMergeThread.Create;
end;

procedure TMergeForm.ReportOnMergeItemClick(Sender: TObject);
var
  i, j: Integer;
  merge: TMerge;
  pluginsList: TList;
  plugin: TPlugin;
  ReportForm: TReportForm;
  bReportsSent, bModalOK: boolean;
begin
  pluginsList := TList.Create;
  bModalOK := false;

  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    Logger.Write('MERGE', 'Report', 'Reporting on '+merge.name);
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      pluginsList.Add(plugin);
    end;
  end;

  // report on all merges
  ReportForm := TReportForm.Create(Self);
  if pluginsList.Count > 0 then begin
    ReportForm.pluginsToReport := pluginsList;
    ReportForm.AppName := wbAppName;
    bModalOK := ReportForm.ShowModal = mrOk;
  end;

  // Send reports to backend
  if bModalOK then begin
    bReportsSent := SendReports(ReportForm.reportsList);
    if not bReportsSent then begin
      Logger.Write('MERGE', 'Report', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\');
    end
    else begin
      Logger.Write('MERGE', 'Report', 'Saving reports locally');
      SaveReports(ReportForm.reportsList, ProgramPath + 'reports\submitted\');
    end;
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
    Logger.Write('MERGE', 'Status', 'Forced rebuild status on '+merge.name);
    // if forced up to date, set to Ready to be rebuilt
    if merge.status = msUpToDateForced then
      merge.status := msRebuildReady
    // if normal up to date, set to Ready to rebuilt [forced]
    else if merge.status = msUpToDate then
      merge.Status := msRebuildReadyForced;
  end;

  // update
  UpdateMerges;
  UpdateListViews;
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
    Logger.Write('MERGE', 'Status', 'Ignored rebuild status on '+merge.name);
    // if force rebuild, set to Up to date
    if merge.status = msRebuildReadyForced then
      merge.status := msUpToDate
    // if normal rebuild, set to Up to date [Forced]
    else if merge.status = msRebuildReady then
      merge.Status := msUpToDateForced;
  end;

  // update
  UpdateMerges;
  UpdateListViews;
end;

procedure TMergeForm.ImageDisconnectedClick(Sender: TObject);
begin
  if (not TCPClient.Connected)
  and (ConnectionAttempts = MaxConnectionAttempts) then begin
    Logger.Write('CLIENT', 'Connect', 'Retrying connecting to the server.');
    ConnectionAttempts := 0;
  end;
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
begin
  NewMerge;
end;

procedure TMergeForm.BuildButtonClick(Sender: TObject);
var
  i, timeCost: integer;
  merge: TMerge;
begin
  // exit if the loader isn't done
  if not bLoaderDone then begin
    Logger.Write('ERROR', 'Merge', 'Loader not done, can''t merge yet!');
    exit;
  end;

  // exit if there are no merges
  if MergesList.Count = 0 then begin
    Logger.Write('ERROR', 'Merge', 'There are no merges!');
    exit;
  end;

  // calculate time costs, prepare merges
  timeCosts := TStringList.Create;
  mergesToBuild := TList.Create;
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    Logger.Write('MERGE', 'Build', 'Building '+merge.name);
    if not (merge.status in BuildStatuses) then
      continue;
    timeCost := merge.GetTimeCost * 2;
    mergesToBuild.Add(merge);
    timeCosts.Add(IntToStr(timeCost));
  end;

  // exit if no merges to build
  if timeCosts.Count = 0 then begin
    Logger.Write('ERROR', 'Merge', 'No merges to build!');
    timeCosts.Free;
    mergesToBuild.Free;
    exit;
  end;

  // make and show progress form
  self.Enabled := false;
  xEditLogGroup := 'MERGE';
  pForm := TProgressForm.Create(Self);
  pForm.LogPath := LogPath;
  pForm.PopupParent := Self;
  pForm.Caption := 'Merging';
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start merge thread
  MergeCallback := ProgressDone;
  TMergeThread.Create;
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

  // update owner draw if changed
  PluginsListView.OwnerDraw := not settings.simplePluginsView;

  // rebuild log because some messages may have been enabled/disabled
  RebuildLog;

  // initialize MO if usingMO changed
  if settings.usingMO then
    ModOrganizerInit;

  // update
  UpdateMerges;
  UpdateListViews;

  // if user selected to change game mode, close application
  if bChangeMergeProfile then
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
  // if not connected to server, don't try to update anything
  if not TCPClient.Connected then
    exit;

  // update program
  if bProgramUpdate and DownloadProgram then begin
    bInstallUpdate := UpdateProgram;
    if bInstallUpdate then
      Close;
  end;

  // update dictionary
  if bDictionaryUpdate and UpdateDictionary then begin
    status := TmpStatus.Create;
    CompareStatuses;
    UpdatePluginData;
    UpdateListViews;
  end;
end;

{ Help }
procedure TMergeForm.HelpButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

end.
