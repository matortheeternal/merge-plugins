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
  mteHelpers, mteTracker, mteLogger, mteProgressForm, mteTaskHandler,
  RttiTranslation,
  // mp units
  mpFrontend, mpThreads, mpMerge, mpDictionaryForm, mpOptionsForm,
  mpSplashForm, mpEditForm, mpReportForm, mpChangeLogForm, mpResolveForm,
  // tes5edit units
  wbBSA, wbHelpers, wbInterface, wbImplementation;

type
  TMergeForm = class(TForm)
    [FormPrefix('mpMain')]
      XPManifest: TXPManifest;
      FlagList: TImageList;
    IconList: TImageList;
      TaskTimer: TTimer;
      [FormSection('QuickBar')]
        QuickBar: TPanel;
        NewButton: TSpeedButton;
        FindErrorsButton: TSpeedButton;
        BuildButton: TSpeedButton;
        ReportButton: TSpeedButton;
        DictionaryButton: TSpeedButton;
        OptionsButton: TSpeedButton;
        UpdateButton: TSpeedButton;
        HelpButton: TSpeedButton;
        bhBuild: TBalloonHint;
        bhFind: TBalloonHint;
        bhNew: TBalloonHint;
        bhReport: TBalloonHint;
        bhDict: TBalloonHint;
        bhOptions: TBalloonHint;
        bhUpdate: TBalloonHint;
        bhHelp: TBalloonHint;
      [FormSection('Main Panel')]
        MainPanel: TPanel;
        Splitter: TSplitter;
        PageControl: TPageControl;
        DetailsPanel: TPanel;
        [FormSection('Details Panel')]
          DetailsLabel: TLabel;
          DetailsEditor: TValueListEditor;
          DetailsPopupMenu: TPopupMenu;
          DetailsCopyToClipboardItem: TMenuItem;
        [FormSection('Plugins Tab')]
          PluginsTabSheet: TTabSheet;
          PluginsListView: TListView;
          [FormSection('Plugins Popup Menu')]
            PluginsPopupMenu: TPopupMenu;
            AddToMergeItem: TMenuItem;
            NewMergeItem: TMenuItem;
            RemoveFromMergeItem: TMenuItem;
            ReportOnPluginItem: TMenuItem;
            DoNotMergeItem: TMenuItem;
            OpenPluginLocationItem: TMenuItem;
            [FormSection('Errors Submenu')]
              ErrorsItem: TMenuItem;
              CheckForErrorsItem: TMenuItem;
              FixErrorsItem: TMenuItem;
              IgnoreErrorsItem: TMenuItem;
              ResetErrorsItem: TMenuItem;
        [FormSection('Merges Tab')]
          MergesTabSheet: TTabSheet;
          MergeListView: TListView;
          [FormSection('Merges Popup Menu')]
            MergesPopupMenu: TPopupMenu;
            CreateNewMergeItem: TMenuItem;
            EditMergeItem: TMenuItem;
            DeleteMergeItem: TMenuItem;
            BuildMergeItem: TMenuItem;
            ToggleRebuildItem: TMenuItem;
            OpenInExplorerItem: TMenuItem;
            [FormSection('Plugins Submenu')]
              PluginsItem: TMenuItem;
              ResolveIssuesItem: TMenuItem;
              CheckPluginsItem: TMenuItem;
              FixPluginsItem: TMenuItem;
              ReportOnPluginsItem: TMenuItem;
        [FormSection('Log Tab')]
          LogTabSheet: TTabSheet;
          LogListView: TListView;
          [FormSection('Log Popup Menu')]
            LogPopupMenu: TPopupMenu;
            FilterGroupItem: TMenuItem;
            FilterLabelItem: TMenuItem;
            CopyToClipboardItem: TMenuItem;
            SaveAndClearItem: TMenuItem;
            ToggleAutoScrollItem: TMenuItem;
      [FormSection('Status Bar')]
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
        bhLoader: TBalloonHint;
        bhLoadException: TBalloonHint;

    // MERGE FORM EVENTS
    procedure UpdateLog;
    procedure LogMessage(const group, &label, text: string);
    procedure FormCreate(Sender: TObject);
    procedure InitDone;
    procedure FormShow(Sender: TObject);
    procedure LoaderStatus(s: string);
    procedure LoaderDone;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SaveDone;
    procedure ConnectDone;
    procedure ProgressDone;
    procedure AutoUpdate;
    function ShouldDisplay(bh: TBalloonHint): boolean;
    procedure DisableHints;
    procedure HideHints;
    procedure DisplayHints;
    procedure Reconnect;
    procedure Heartbeat;
    procedure RefreshGUI;
    procedure OnTaskTimer(Sender: TObject);
    procedure ShowAuthorizationMessage;
    procedure UpdateStatusBar;
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
    procedure IgnoreErrorsItemClick(Sender: TObject);
    procedure DoNotMergeItemClick(Sender: TObject);
    procedure RemoveFromMergeItemClick(Sender: TObject);
    procedure OpenPluginLocationItemClick(Sender: TObject);
    procedure ReportOnPluginItemClick(Sender: TObject);
    procedure FixErrorsItemClick(Sender: TObject);
    procedure ResetErrorsItemClick(Sender: TObject);
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
    procedure CheckPluginsItemClick(Sender: TObject);
    procedure RemovePluginsItemClick(Sender: TObject);
    procedure ResolveIssuesItemClick(Sender: TObject);
    procedure DeleteMergeItemClick(Sender: TObject);
    procedure ReportOnPluginsItemClick(Sender: TObject);
    procedure OpenInExplorerItemClick(Sender: TObject);
    procedure ToggleRebuildItemClick(Sender: TObject);
    procedure FixPluginsItemClick(Sender: TObject);
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
    // QUICKBAR EVENTS
    procedure UpdateQuickbar;
    procedure CreateMergeButtonClick(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure FindErrorsButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ToggleAutoScrollItemClick(Sender: TObject);
    procedure DetailsCopyToClipboardItemClick(Sender: TObject);
    procedure ImageDisconnectedClick(Sender: TObject);
  protected
    procedure WMSize(var AMessage: TMessage); message WM_SIZE;
    procedure WMMove(var AMessage: TMessage); message WM_MOVE;
    procedure WMActivateApp(var AMessage: TMessage); message WM_ACTIVATEAPP;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  MessageDelay = (0.1 / 86400.0);

var
  splash: TSplashForm;
  MergeForm: TMergeForm;
  LastHint: string;
  LastURLTime, LastMessageTime, FormDisplayTime: double;
  bMergesToBuild, bMergesToCheck, bAutoScroll, bCreated, bClosing: boolean;
  pForm: TProgressForm;
  rForm: TResolveForm;
  TaskHandler: TTaskHandler;

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
  StatusCallback := LoaderStatus;

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

  // do translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load language
  TRttiTranslation.Load(language, self);

  // finalize
  bCreated := true;
end;

procedure TMergeForm.WMSize(var AMessage: TMessage);
begin
  if not bCreated then
    exit;
  if Now - LastMessageTime < MessageDelay then
    exit;
  LastMessageTime := Now;
  if (AMessage.WParam <> SIZE_MINIMIZED) then
    DisplayHints;
  inherited;
end;

procedure TMergeForm.WMMove(var AMessage: TMessage);
begin
  if not bCreated then
    exit;
  if Now - LastMessageTime < MessageDelay then
    exit;
  LastMessageTime := Now;
  DisplayHints;
  inherited;
end;

procedure TMergeForm.WMActivateApp(var AMessage: TMessage);
begin
  if not bCreated then
    exit;
  if Now - LastMessageTime < MessageDelay then
    exit;
  LastMessageTime := Now;
  if AMessage.WParam = 1 then
    DisplayHints
  else
    HideHints;
  inherited;
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

  // DISABLE GUI IF INITIALIZATION EXCEPTION
  if bInitException then begin
    StatusPanelMessage.Caption := 'The application failed to initialize';
    Logger.Write('ERROR', 'Load', 'There was an exception initializing the application');
    Logger.Write('ERROR', 'Load', 'Review your log messages to resolve the issue');
    Logger.Write('ERROR', 'Load', 'You can also change the program''s settings, if necessary');
    PluginsTabSheet.Enabled := false;
    PluginsTabSheet.TabVisible := false;
    MergesTabSheet.Enabled := false;
    MergesTabSheet.TabVisible := false;
    PageControl.ActivePage := LogTabSheet;
    PageControlChange(PageControl);
  end;

  // QUICKBAR
  NewButton.Flat := true;
  FindErrorsButton.Flat := true;
  BuildButton.Flat := true;
  ReportButton.Flat := true;
  DictionaryButton.Flat := true;
  OptionsButton.Flat := true;
  UpdateButton.Flat := true;
  HelpButton.Flat := true;
  IconList.GetBitmap(0, NewButton.Glyph);
  IconList.GetBitmap(1, FindErrorsButton.Glyph);
  IconList.GetBitmap(2, BuildButton.Glyph);
  IconList.GetBitmap(3, ReportButton.Glyph);
  IconList.GetBitmap(4, DictionaryButton.Glyph);
  IconList.GetBitmap(5, OptionsButton.Glyph);
  IconList.GetBitmap(6, UpdateButton.Glyph);
  IconList.GetBitmap(7, HelpButton.Glyph);

  // STATUSBAR VALUES
  StatusPanelLanguage.Caption := settings.language;
  StatusPanelVersion.Caption := 'v'+ProgramVersion;

  // UPDATE GUI
  PluginsListView.OwnerDraw := not settings.simplePluginsView;
  PluginsListView.Items.Count := PluginsList.Count;
  UpdateLog;
  UpdateMerges;
  UpdatePluginsPopupMenu;
  UpdateStatusBar;
  UpdateQuickBar;

  if not bInitException then begin
    // ATTEMPT TO CONNECT TO SERVER
    ConnectCallback := ConnectDone;
    if (not bConnecting) and (not TCPClient.Connected) then
      TConnectThread.Create;

    // START BACKGROUND LOADER
    LoaderCallback := LoaderDone;
    SetTaskbarProgressState(tbpsIndeterminate);
    TLoaderThread.Create;

    // CORRECT LIST VIEW WIDTHS
    CorrectListViewWidth(MergeListView);
    CorrectListViewWidth(PluginsListView);

    // LOAD AND DISPLAY HINTS
    StatusPanelMessage.Caption := GetString('mpMain_LoaderInProgress');
    bhLoader.Title := GetString('mpMain_LoaderInProgress');
    bhLoader.Description := GetString('mpMain_LoaderLimitations');
    bhLoadException.Title := GetString('mpMain_LoadException');
    bhLoadException.Description := GetString('mpMain_PluginsNotLoaded');
    DisplayHints;

    // initialize task handler
    TaskHandler := TTaskHandler.Create;
    TaskHandler.AddTask(TTask.Create('Disable Hints', 12.0 * seconds, DisableHints));
    TaskHandler.AddTask(TTask.Create('Reconnect', 15.0 * seconds, Reconnect));
    TaskHandler.AddTask(TTask.Create('Heartbeat', 0.9 * seconds, Heartbeat));
    TaskHandler.AddTask(TTask.Create('Refresh GUI', 3.0 * seconds, RefreshGUI));
    TaskTimer.Enabled := true;
  end;

  // ACTIVATE WINDOW
  FormDisplayTime := Now;
  Application.Restore;
end;

procedure TMergeform.LoaderStatus(s: string);
begin
  StatusPanelMessage.Caption := s;
end;

procedure TMergeForm.LoaderDone;
begin
  SetTaskbarProgressState(tbpsNone);
  xEditLogGroup := 'GENERAL';
  xEditLogLabel := 'xEdit';
  FlashWindow(Application.Handle, True);
  UpdateQuickbar;
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
    pForm.Caption := GetString('mpProg_Closing');
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

procedure TMergeForm.ConnectDone;
begin
  // UPDATE QUICKBAR
  UpdateQuickbar;
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
  if Assigned(pluginsToHandle) then pluginsToHandle.Free;
  if Assigned(mergesToBuild) then mergesToBuild.Free;

  // update merges and gui
  UpdateListViews;
  UpdateMerges;
  UpdateQuickbar;
  UpdatePluginsPopupMenu;
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

function TMergeForm.ShouldDisplay(bh: TBalloonHint): boolean;
begin
  Result := (Now - FormDisplayTime) * 86400 < (bh.HideAfter / 1000);
end;

procedure TMergeForm.DisableHints;
begin
  HideHints;
  TaskHandler.RemoveTask('Disable Hints');
end;

procedure TMergeForm.HideHints;
begin
  bhLoader.HideHint;
  bhLoadException.HideHint;
end;

procedure TMergeForm.DisplayHints;
var
  pt: TPoint;
begin
  if bLoadException and ShouldDisplay(bhLoadException) then begin
    pt.X := 126;
    pt.Y := 16;
    pt := MainPanel.ClientToScreen(pt);
    bhLoadException.ShowHint(pt);
  end;

  if ShouldDisplay(bhLoader) then begin
    pt.X := 8;
    pt.Y := 4;
    pt := ImageBlocked.ClientToScreen(pt);
    bhLoader.ShowHint(pt);
  end;
end;

procedure TMergeForm.Reconnect;
begin
  if not (TCPClient.Connected or bConnecting or bClosing) then
    TConnectThread.Create;
end;

procedure TMergeForm.RefreshGUI;
begin
  if not bClosing then UpdateStatusBar;
end;

procedure TMergeForm.Heartbeat;
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

procedure TMergeForm.OnTaskTimer(Sender: TObject);
begin
  TaskHandler.ExecTasks;
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

procedure TMergeForm.UpdateStatusBar;
begin
  ImageBlocked.Visible := not (bLoaderDone or bInitException);
  ImageConnected.Visible := TCPClient.Connected;
  ImageDisconnected.Visible := not TCPClient.Connected;
  ImageBuild.Visible := bLoaderDone and bMergesToBuild;
  ImageDictionaryUpdate.Visible := bDictionaryUpdate;
  ImageProgramUpdate.Visible := bProgramUpdate;
end;

procedure TMergeForm.UpdateListViews;
begin
  if PageControl.ActivePage = PluginsTabSheet then begin
    UpdatePluginDetails;
    PluginsListView.Repaint;
  end;
  if PageControl.ActivePage = MergesTabSheet then begin
    UpdateMergeDetails;
    MergeListView.Repaint;
  end;
  if PageControl.ActivePage = LogTabSheet then
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
  sl.Text := Wordwrap(sl.Text, 80);
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
  DetailsLabel.Caption := GetString('mpMain_AppDetails');

  // add details items
  AddDetailsItem(GetString('mpMain_Application'), 'Merge Plugins');
  AddDetailsItem(GetString('mpMain_Author'), 'matortheeternal');
  AddDetailsItem(GetString('mpMain_Version'), ProgramVersion);
  AddDetailsItem(GetString('mpMain_DateBuilt'), DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_GameMode'), wbGameName);
  AddDetailsItem(GetString('mpMain_Language'), settings.language);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_TimesRun'), IntToStr(statistics.timesRun + sessionStatistics.timesRun));
  AddDetailsItem(GetString('mpMain_MergesBuilt'), IntToStr(statistics.mergesBuilt + sessionStatistics.mergesBuilt));
  AddDetailsItem(GetString('mpMain_PluginsChecked'), IntToStr(statistics.pluginsChecked + sessionStatistics.pluginsChecked));
  AddDetailsItem(GetString('mpMain_PluginsFixed'), IntToStr(statistics.pluginsFixed + sessionStatistics.pluginsFixed));
  AddDetailsItem(GetString('mpMain_PluginsMerged'), IntToStr(statistics.pluginsMerged + sessionStatistics.pluginsMerged));
  AddDetailsItem(GetString('mpMain_ReportsSubmitted'), IntToStr(statistics.reportsSubmitted + sessionStatistics.reportsSubmitted));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_Website'), 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem(GetString('mpMain_ApiCredits'), 'superobject, TurboPower Abbrevia, xEdit');
  AddDetailsItem(GetString('mpMain_xEditVersion'), xEditVersion);
  AddDetailsItem(GetString('mpMain_xEditCredits'), 'zilav, hlp, Sharlikran, ElminsterAU');
  AddDetailsItem(GetString('mpMain_Testers'), ProgramTesters);
  AddDetailsItem(GetString('mpMain_Translators'), ProgramTranslators);
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
  DetailsLabel.Caption := GetString('mpMain_PluginDetails');

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetData;

  // add details items
  sl := TStringList.Create;
  sl.Text := plugin.GetFlagsDescription;
  AddDetailsItem(GetString('mpMain_Filename'), plugin.filename);
  AddDetailsItem(GetString('mpMain_Hash'), plugin.hash);
  AddDetailsItem(GetString('mpMain_FileSize'), FormatByteSize(plugin.fileSize));
  AddDetailsItem(GetString('mpMain_DateModified'), plugin.dateModified);
  AddDetailsItem(GetString('mpMain_MergeRating'), plugin.entry.rating);
  AddDetailsList(GetString('mpMain_Flags'), sl);
  AddDetailsItem(GetString('mpMain_NumRecords'), plugin.numRecords);
  AddDetailsItem(GetString('mpMain_NumOverrides'), plugin.numOverrides);
  AddDetailsItem(GetString('mpMain_Author'), plugin.author);
  AddDetailsList(GetString('mpMain_Description'), plugin.description);
  AddDetailsList(GetString('mpMain_Masters'), plugin.masters);
  AddDetailsList(GetString('mpMain_Errors'), plugin.errors);
  AddDetailsList(GetString('mpMain_Reports'), plugin.reports);

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
  UpdateQuickbar;
  UpdateStatusBar;
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
    if ListView.Columns[i + 1].Tag = 300 then
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
      hint := Format(GetString('mpMain_PluginFlags'), [plugin.filename, hint]);
  end;
  if (hint <> LastHint) then begin
    LastHint := hint;
    PluginsListView.Hint := hint;
    Application.ActivateHint(Mouse.CursorPos);
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
    DoNotMergeItem.Caption := GetString('mpMain_AllowMerging')
  else
    DoNotMergeItem.Caption := GetString('mpMain_DoNotMergeItem_Caption');
  if bAllIgnoreErrors then
    IgnoreErrorsItem.Caption := GetString('mpMain_UnignoreErrors')
  else
    IgnoreErrorsItem.Caption := GetString('mpMain_IgnoreErrorsItem_Caption');


  // disable/enable menu items
  AddToMergeItem.Enabled := not (bBlacklisted or bPluginInMerge or bAllDoNotMerge);
  RemoveFromMergeItem.Enabled := bAllPluginsInMerge;
  DoNotMergeItem.Enabled := not (bBlacklisted or bPluginInMerge);
  CheckForErrorsItem.Enabled := bLoaderDone and bAllNeedErrorCheck and not bBlacklisted;
  FixErrorsItem.Enabled := bLoaderDone and bHasErrors and not bBlacklisted;
  IgnoreErrorsItem.Enabled := bHasErrors and not bBlacklisted;
  ResetErrorsItem.Enabled := bHasErrors and not bBlacklisted;
  ReportOnPluginItem.Enabled := not bBlacklisted;
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
  MenuItem.Caption := GetString('mpMain_NewMergeItem_Caption');
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
  merge := MergesList[AddToMergeItem.IndexOf(MenuItem) - 1];
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
  pluginsToHandle := TList.Create;
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
    pluginsToHandle.Add(plugin);
  end;

  // show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Checking'));

  // start error check thread
  ErrorCheckCallback := ProgressDone;
  TErrorCheckThread.Create;
end;

{ True if the flag can't be drawn without colliding with next column }

procedure TMergeForm.FixErrorsItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  plugin: TPlugin;
begin
  // create lists
  pluginsToHandle := TList.Create;
  timeCosts := TStringList.Create;

  for i := 0 to Pred(PluginsListView.Items.Count) do begin
    ListItem := PluginsListView.Items[i];
    if not ListItem.Selected then
      continue;
    plugin := TPlugin(PluginsList[i]);
    // skip blacklisted plugins and plugins that have already been checked
    if (IS_BLACKLISTED in plugin.flags) or not (HAS_ERRORS in plugin.flags) then
      continue;
    timeCosts.Add(plugin.numRecords);
    pluginsToHandle.Add(plugin);
  end;

  // show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Fixing'));

  // start error check thread
  ErrorFixCallback := ProgressDone;
  TErrorFixThread.Create;
end;

{ Remove from Merge }
procedure TMergeForm.RemoveFromMergeItemClick(Sender: TObject);
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
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
end;

procedure TMergeForm.ReportOnPluginItemClick(Sender: TObject);
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

procedure TMergeForm.ResetErrorsItemClick(Sender: TObject);
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
    plugin.errors.Clear;
    plugin.GetFlags;

    // log message
    Logger.Write('PLUGIN', 'Errors', 'Reset errors on '+plugin.filename);
  end;

  // update plugins list view
  UpdateMerges;
  UpdateListViews;
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
  DetailsLabel.Caption := GetString('mpMain_MergeDetails');

  // get merge information
  merge := MergesList[MergeListView.ItemIndex];
  AddDetailsItem(GetString('mpMain_Status'), StatusArray[Ord(merge.status)].desc, false);
  AddDetailsItem(GetString('mpMain_MergeName'), merge.name, true);
  AddDetailsItem(GetString('mpMain_Filename'), merge.filename, true);
  AddDetailsItem(GetString('mpMain_PluginCount'), IntToStr(merge.plugins.Count));
  AddDetailsItem(GetString('mpMain_DateBuilt'), DateBuiltString(merge.dateBuilt));
  AddDetailsList(GetString('mpMain_Plugins'), merge.plugins);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetString('mpMain_MergeMethod'), merge.method, false);
  AddDetailsItem(GetString('mpMain_Renumbering'), merge.renumbering, false);
  if merge.files.Count < 250 then begin
    sl := TStringList.Create;
    sl.Text := StringReplace(merge.files.Text, settings.mergeDirectory, '', [rfReplaceAll]);
    AddDetailsList(GetString('mpMain_Files'), sl);
    sl.Free;
  end
  else
    AddDetailsItem(GetString('mpMain_Files'), GetString('mpMain_TooManyFiles'));
  if merge.fails.Count < 250 then
    AddDetailsList(GetString('mpMain_Fails'), merge.fails)
  else
    AddDetailsItem(GetString('mpMain_Fails'), GetString('mpMain_TooManyFails'));
end;

procedure TMergeForm.UpdateMerges;
var
  i: integer;
  merge: TMerge;
begin
  // update merge count
  MergeListView.Items.Count := MergesList.Count;

  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    // sort plugins in merge
    merge.SortPlugins;
    // get status of each merge
    if not (merge.status in ForcedStatuses) then
      merge.GetStatus;
  end;
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
  Result := IfThen(not b, GetString('mpMain_Enable'), GetString('mpMain_Disable'));
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
    item.Caption := Format('%s %s, %s', [EnableStr(filter.enabled), filter.group, filter.&label]);
    item.OnClick := ToggleLabelFilter;
    FilterLabelItem.Add(item);
  end;

  // toggle copy to clipboard item based on whether or not log items are selected
  CopyToClipboardItem.Enabled := Assigned(LogListView.Selected);

  // rename toggle auto scroll item based on whether or not auto scroll is enabled
  ToggleAutoScrollItem.Caption := Format('%s %s', [EnableStr(bAutoScroll), GetString('mpMain_AutoScroll')]);
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
  CorrectListViewWidth(LogListView);
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
  CorrectListViewWidth(LogListView);
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
  bNeverBuilt, bHasBuildStatus, bHasUpToDateStatus, bHasResolveStatus,
  bHasCheckStatus, bHasErrorStatus, bHasSelection, bHasPluginErrors: boolean;
  merge: TMerge;
  i, mergesSelected: Integer;
  sBuild, sRebuild: string;
begin
  bNeverBuilt := false;
  bHasBuildStatus := false;
  bHasUpToDateStatus := false;
  bHasCheckStatus := false;
  bHasErrorStatus := false;
  bHasPluginErrors := false;
  bHasResolveStatus := false;
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
    bHasPluginErrors := bHasPluginErrors or (merge.status = msErrors);
    bHasErrorStatus := bHasErrorStatus or (merge.status in ErrorStatuses);
    bHasResolveStatus := bHasResolveStatus or (merge.status in ResolveStatuses);
  end;

  bHasSelection := (mergesSelected > 0);
  // change enabled state of MergesPopupMenu items based on booleans
  EditMergeItem.Enabled := bHasSelection;
  DeleteMergeItem.Enabled := bHasSelection;
  BuildMergeItem.Enabled := bHasSelection and bHasBuildStatus and bLoaderDone;
  ToggleRebuildItem.Enabled := bHasSelection and not bNeverBuilt and
    (bHasUpToDateStatus or bHasBuildStatus);
  OpenInExplorerItem.Enabled := bHasSelection;
  // plugins submenu
  ResolveIssuesItem.Enabled := bHasSelection and bHasResolveStatus;
  CheckPluginsItem.Enabled := bHasSelection and bHasCheckStatus and bLoaderDone;
  FixPluginsItem.Enabled := bHasSelection and bHasPluginErrors and bLoaderDone;
  ReportOnPluginsItem.Enabled := bHasSelection and bHasUpToDateStatus;

  // one or multiple merges?
  if (mergesSelected = 1) then begin
    sBuild := 'mpMain_BuildMerge';
    sRebuild := 'mpMain_RebuildMerge';
  end
  else begin
    sBuild := 'mpMain_BuildMerges';
    sRebuild := 'mpMain_RebuildMerges';
  end;
  // handle build merges menu item
  if bNeverBuilt and not bHasErrorStatus then
    BuildMergeItem.Caption := GetString(sBuild)
  else if bHasBuildStatus then
    BuildMergeItem.Caption := GetString(sRebuild)
  else begin
    BuildMergeItem.Enabled := false;
    BuildMergeItem.Caption := GetString(sRebuild);
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

{ Fix erorrs in plugins in merge }
procedure TMergeForm.FixPluginsItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  timeCosts := TStringList.Create;
  pluginsToHandle := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status = msErrors) then
      continue;

    // else loop through plugins
    Logger.Write('MERGE', 'Plugins', 'Fixing erorrs in plugins in '+merge.name);
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      // skip plugins that don't have errors
      if not plugin.HasErrors then continue;
      pluginsToHandle.Add(plugin);
      timeCosts.Add(plugin.numRecords);
    end;
  end;

  // free and exit if no plugins to fix errors in
  if pluginsToHandle.Count = 0 then begin
    timeCosts.Free;
    pluginsToHandle.Free;
    exit;
  end;

  // show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Fixing'));

  // start error checking thread
  ErrorFixCallback := ProgressDone;
  TErrorFixThread.Create;
end;

{ Check plugins in merge for errors }
procedure TMergeForm.CheckPluginsItemClick(Sender: TObject);
var
  i, j: Integer;
  plugin: TPlugin;
  merge: TMerge;
begin
  timeCosts := TStringList.Create;
  pluginsToHandle := TList.Create;

  // get timecosts
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status = msCheckErrors) then
      continue;

    // else loop through plugins
    Logger.Write('MERGE', 'Plugins', 'Checking plugins in '+merge.name+' for errors');
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      // skip plugins that have already been checked for errors
      if plugin.HasBeenCheckedForErrors then continue;
      pluginsToHandle.Add(plugin);
      timeCosts.Add(plugin.numRecords);
    end;
  end;

  // free and exit if no merges to check for errors
  if pluginsToHandle.Count = 0 then begin
    timeCosts.Free;
    pluginsToHandle.Free;
    exit;
  end;

  // Show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Checking'));

  // start error checking thread
  ErrorCheckCallback := ProgressDone;
  TErrorCheckThread.Create;
end;

procedure TMergeForm.ResolveIssuesItemClick(Sender: TObject);
var
  i: Integer;
  merge: TMerge;
begin
  // create resolve form
  self.Enabled := false;
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    if not (merge.status in ResolveStatuses) then
      continue;

    // create resolve form
    rForm := TResolveForm.Create(self);
    rForm.merge := merge;
    rForm.ShowModal;
    rForm.Free;
  end;

  // update merges and gui
  self.Enabled := true;
  UpdateMerges;
  UpdateQuickbar;
  UpdateStatusBar;
  UpdatePluginsPopupMenu;
  UpdateListViews;
end;

{ Remove unloaded plugins and plugins with errors }
procedure TMergeForm.RemovePluginsItemClick(Sender: TObject);
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
    Logger.Write('MERGE', 'Plugins', 'Removing plugins from '+merge.name);
    // remove plugins that aren't loaded or have errors
    for j := Pred(merge.plugins.Count) downto 0 do begin
      plugin := PluginByFilename(merge.plugins[j]);
      if not Assigned(plugin) then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+merge.plugins[j]+', plugin not loaded');
        merge.plugins.Delete(j);
        continue;
      end;
      if plugin.HasErrors and (not plugin.bIgnoreErrors) then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+merge.plugins[j]+', plugin has errors');
        merge.Remove(plugin);
      end;
      if plugin.bDisallowMerging then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+merge.plugins[j]+', plugin marked as do not merge');
        merge.Remove(plugin);
      end;
      if IS_BLACKLISTED in plugin.flags then begin
        Logger.Write('MERGE', 'Plugins', 'Removing '+merge.plugins[j]+', plugin marked as blacklisted');
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
    bApproved := MessageDlg(GetString('mpMain_DeleteMerges') + mergeNames, mtConfirmation,
      mbOKCancel, 0) = mrOk;

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
  UpdateListViews;
  UpdateQuickbar;
  UpdateStatusBar;
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
  pForm.Caption := GetString('mpProg_Merging');
  pForm.MaxProgress(IntegerListSum(timeCosts, Pred(timeCosts.Count)));
  pForm.Show;

  // start merge thread
  MergeCallback := ProgressDone;
  TMergeThread.Create;
end;

procedure TMergeForm.ReportOnPluginsItemClick(Sender: TObject);
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

procedure TMergeForm.ToggleRebuildItemClick(Sender: TObject);
var
  i: Integer;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergeListView.Items.Count) do begin
    if not MergeListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    Logger.Write('MERGE', 'Status', 'Toggled rebuild status on '+merge.name);
    // if forced up to date, set to Ready to be rebuilt
    if merge.status = msUpToDateForced then
      merge.status := msRebuildReady
    // if normal up to date, set to Ready to rebuilt [forced]
    else if merge.status = msUpToDate then
      merge.Status := msRebuildReadyForced
    // if force rebuild, set to Up to date
    else if merge.status = msRebuildReadyForced then
      merge.status := msUpToDate
    // if normal rebuild, set to Up to date [Forced]
    else if merge.status = msRebuildReady then
      merge.Status := msUpToDateForced;
  end;

  // update
  UpdateMerges;
  UpdateListViews;
  UpdateQuickBar;
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

procedure TMergeForm.UpdateQuickbar;
var
  i: Integer;
  bUncheckedPlugins: boolean;
  merge: TMerge;
  plugin: TPlugin;
  sTitle: string;
begin
  // DISABLE ALL BUTTONS IF INITIALIZATION EXCEPTION
  if bInitException then begin
    NewButton.Enabled := false;
    FindErrorsButton.Enabled := false;
    BuildButton.Enabled := false;
    ReportButton.Enabled := false;
    DictionaryButton.Enabled := false;
    OptionsButton.Enabled := true;
    UpdateButton.Enabled := false;
    HelpButton.Enabled := false;
    exit;
  end;

  // FIND ERRORS BUTTON
  bUncheckedPlugins := false;
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    if (IS_BLACKLISTED in plugin.flags) then
      continue;
    if not plugin.HasBeenCheckedForErrors then
      bUncheckedPlugins := true;
  end;

  // enable find errors button if there are unchecked plugins
  FindErrorsButton.Enabled := bLoaderDone and bUncheckedPlugins;
  // swap hints
  sTitle := GetString('mpMain_FindErrorsButton_Hint');
  if not bLoaderDone then
    FindErrorsButton.Hint := sTitle + GetString('mpMain_FindErrors_Loader')
  else if not bUncheckedPlugins then
    FindErrorsButton.Hint := sTitle + GetString('mpMain_NoPluginsToCheck')
  else
    FindErrorsButton.Hint := sTitle + GetString('mpMain_CheckAllPlugins');

  // BUILD BUTTON
  bMergesToBuild := false;
  bMergesToCheck := false;
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    if (merge.status in BuildStatuses) then
      bMergesToBuild := true;
    if (merge.status = msCheckErrors) then
      bMergesToCheck := true;
  end;

  // enable build button if there are merges to build
  BuildButton.Enabled := bMergesToBuild and bLoaderDone;
  // swap hints
  sTitle := GetString('mpMain_BuildButton_Hint');
  if not bLoaderDone then
    BuildButton.Hint := sTitle + GetString('mpMain_BuildMerges_Loader')
  else if not bMergesToBuild then
    BuildButton.Hint := sTitle + GetString('mpMain_NoMerges')
  else if bMergesToCheck then
    BuildButton.Hint := sTitle + GetString('mpMain_CheckMerges')
  else
    BuildButton.Hint := sTitle + GetString('mpMain_BuildAllMerges');

  // REPORT BUTTON
  ReportButton.Enabled := TCPClient.Connected; // TODO

  // UPDATE BUTTON
  UpdateButton.Enabled := bProgramUpdate or bDictionaryUpdate;
  sTitle := GetString('mpMain_UpdateButton_Hint');
  if bProgramUpdate and bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetString('mpMain_UpdateBoth')
  else if bProgramUpdate then
    UpdateButton.Hint := sTitle + GetString('mpMain_UpdateProgram')
  else if bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetString('mpMain_UpdateDictionary')
  else
    UpdateButton.Hint := sTitle + GetString('mpMain_NoUpdates');

  // HELP BUTTON
  HelpButton.Enabled := false; // TODO: help file integration
end;

procedure TMergeForm.CreateMergeButtonClick(Sender: TObject);
begin
  NewMerge;
end;

procedure TMergeForm.FindErrorsButtonClick(Sender: TObject);
var
  i: Integer;
  plugin: TPlugin;
begin
  // exit if the loader isn't done
  if not bLoaderDone then begin
    Logger.Write('ERROR', 'Check', 'Loader not done, can''t check for errors yet!');
    exit;
  end;

   // calculate time costs, prepare plugins
  timeCosts := TStringList.Create;
  pluginsToHandle := TList.Create;
  for i := 0 to Pred(PluginsList.Count) do begin
    plugin := TPlugin(PluginsList[i]);
    Logger.Write('PLUGIN', 'Check', 'Checking for errors in '+plugin.filename);
    if (IS_BLACKLISTED in plugin.flags) or (HAS_ERRORS in plugin.flags) then
      continue;
    pluginsToHandle.Add(plugin);
    timeCosts.Add(IntToStr(plugin._File.RecordCount));
  end;

  // exit if no plugins to check
  if timeCosts.Count = 0 then begin
    Logger.Write('ERROR', 'Check', 'No plugins to check for errors!');
    timeCosts.Free;
    pluginsToHandle.Free;
    exit;
  end;

  // make and show progress form
  ShowProgressForm(self, pForm, GetString('mpProg_Checking'));

  // start error check thread
  ErrorCheckCallback := ProgressDone;
  TErrorCheckThread.Create;
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
  pForm.Caption := GetString('mpProg_Merging');
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
  prevLanguage: string;
begin
  prevLanguage := settings.language;
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

  // if user changed language, update language displayed
  if settings.language <> prevLanguage then begin
    LoadLanguage;
    TRttiTranslation.Load(language, self);
  end;

  // update gui
  UpdateMerges;
  UpdateListViews;
  UpdateQuickBar;
  UpdateStatusBar;

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
  if bProgramUpdate and ChangeLogPrompt(self) and DownloadProgram then begin
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
