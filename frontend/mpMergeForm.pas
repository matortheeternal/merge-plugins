unit mpMergeForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl, Menus, Grids,
  ValEdit, ShellAPI, StrUtils, ShlObj, Clipbrd,
  // third party libraries
  superobject, W7Taskbar,
  // mte components
  mteBase, mteHelpers, mteTracker, mteLogger, mteLogging, mteProgressForm,
  mteTaskHandler, mteChangeLogForm, RttiTranslation,
  // mp units
  mpCore, mpClient, mpThreads, mpConfiguration, mpMerge, mpLoader,
  // mp forms
  mpDictionaryForm, mpOptionsForm, mpSplashForm, mpEditForm, mpReportForm,
  mpResolveForm,
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
          DetailsGrid: TStringGrid;
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
          MergesListView: TListView;
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
            [FormSection('Move Submenu')]
              MoveItem: TMenuItem;
              UpItem: TMenuItem;
              DownItem: TMenuItem;
              ToTopItem: TMenuItem;
              ToBottomItem: TMenuItem;
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
    procedure ToggleFormState(bEnabled: boolean);
    procedure InitDone;
    procedure FormShow(Sender: TObject);
    procedure LoaderStatus(s: string);
    procedure LoaderDone;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SaveDone;
    procedure ConnectDone;
    procedure ProgressDone;
    procedure AutoUpdate;
    function ShouldDisplay(bh: TBalloonHint): boolean;
    procedure ConfigureSettings;
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
    // DETAILS GRID EVENTS
    procedure AddDetailsItem(name, value: string);
    procedure AddDetailsList(name: string; sl: TStringList);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateApplicationDetails;
    procedure DetailsGridMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DetailsGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    // PLUGINS LIST VIEW EVENTS
    procedure UpdatePluginDetails;
    procedure AddPluginsToMerge(var merge: TMerge);
    procedure PluginsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure PluginsListViewData(Sender: TObject; Item: TListItem);
    function FlagNotSafe(Rect: TRect; x: integer): boolean;
    procedure DrawFlag(canvas: TCanvas; flag: TPluginFlag; x, y: integer);
    procedure DrawPluginFlags(canvas: TCanvas; Rect: TRect; x, y: integer;
      flags: string);
    procedure PluginsListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure PluginsListViewKeyPress(Sender: TObject; var Key: Char);
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
    procedure MergesListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MergesListViewData(Sender: TObject; Item: TListItem);
    procedure MergesListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure MergesListViewDblClick(Sender: TObject);
    procedure MergesListViewKeyPress(Sender: TObject; var Key: Char);
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
    procedure UpItemClick(Sender: TObject);
    procedure DownItemClick(Sender: TObject);
    procedure ToTopItemClick(Sender: TObject);
    procedure ToBottomItemClick(Sender: TObject);
    // LOG LIST VIEW EVENTS
    procedure LogListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure LogListViewData(Sender: TObject; Item: TListItem);
    procedure LogListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
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
    fLastBufferTime: TDateTime;
    sBuffer: string;
    slDetails: TStringList;
  public
    { Public declarations }
  end;

const
  // delay for clearing keystroke buffer when
  // performing a text search on a list view
  fBufferDelay = 1.1 * seconds;
  // delay for detecting WMMessages and
  // re-displaying hints
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
    ListView_CorrectWidth(LogListView);
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

procedure InitLog;
begin
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
  LabelFilters.Add(TFilter.Create('CLIENT', 'Status', true));
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
  UpdateCallback := AutoUpdate;

  if not InitBase then begin
    ProgramStatus.bClose := true;
    exit;
  end;

  // CREATE SPLASH
  splash := TSplashForm.Create(nil);
  try
    InitCallback := InitDone;
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

procedure TMergeForm.ToggleFormState(bEnabled: boolean);
begin
  // show/hide hints
  if bEnabled then
    DisplayHints
  else
    HideHints;

  // disable/enable form
  Enabled := bEnabled;
end;

procedure TMergeForm.WMSize(var AMessage: TMessage);
begin
  if bCreated and (Now - LastMessageTime > MessageDelay) then begin
    LastMessageTime := Now;
    if (AMessage.WParam <> SIZE_MINIMIZED) then
      DisplayHints;
  end;
  inherited;
end;

procedure TMergeForm.WMMove(var AMessage: TMessage);
begin
  if bCreated and (Now - LastMessageTime > MessageDelay) then begin
    LastMessageTime := Now;
    if (AMessage.WParam <> SIZE_MINIMIZED) then
      DisplayHints;
  end;
  inherited;
end;

procedure TMergeForm.WMActivateApp(var AMessage: TMessage);
begin
  if bCreated and (Now - LastMessageTime > MessageDelay) then begin
    LastMessageTime := Now;
    if AMessage.WParam = 1 then
      DisplayHints
    else
      HideHints;
  end;
  inherited;
end;

procedure TMergeForm.InitDone;
begin
  splash.ModalResult := mrOk;
end;

// Force PluginsListView to autosize columns
procedure TMergeForm.FormShow(Sender: TObject);
begin
  // HANDLE OFFLINE MODE
  if ProgramStatus.bOfflineMode then
    Logger.Write('CLIENT', 'Status', 'Running in offline mode.');

  // HANDLE AUTO-UPDATE
  if ProgramStatus.bInstallUpdate then begin
    Logger.Write('CLIENT', 'Status', 'Disconnecting...');
    TCPClient.Disconnect;
    ProgramStatus.bClose := true;
    bClosing := true;
    Logger.Write('GENERAL', 'Update', 'Restarting.');
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
    Close;
  end;

  // DISABLE GUI IF INITIALIZATION EXCEPTION
  if ProgramStatus.bInitException then begin
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
  StatusPanelVersion.Caption := 'v'+LocalStatus.programVersion;

  // UPDATE GUI
  slDetails := TStringList.Create;
  DetailsGrid.ColWidths[1] := DetailsGrid.ClientWidth - DetailsGrid.ColWidths[0];
  PluginsListView.OwnerDraw := not settings.simplePluginsView;
  PluginsListView.Items.Count := PluginsList.Count;
  UpdateLog;
  UpdateMerges;
  UpdatePluginsPopupMenu;
  UpdateStatusBar;
  UpdateQuickBar;

  if not ProgramStatus.bInitException then begin
    // ATTEMPT TO CONNECT TO SERVER
    ConnectCallback := ConnectDone;
    if (not ProgramStatus.bConnecting) and (not TCPClient.Connected) then
      TConnectThread.Create;

    // START BACKGROUND LOADER
    LoaderCallback := LoaderDone;
    SetTaskbarProgressState(tbpsIndeterminate);
    TLoaderThread.Create;

    // CORRECT LIST VIEW WIDTHS
    ListView_CorrectWidth(MergesListView);
    ListView_CorrectWidth(PluginsListView);

    // LOAD AND DISPLAY HINTS
    StatusPanelMessage.Caption := GetLanguageString('mpMain_LoaderInProgress');
    bhLoader.Title := GetLanguageString('mpMain_LoaderInProgress');
    bhLoader.Description := GetLanguageString('mpMain_LoaderLimitations');
    bhLoadException.Title := GetLanguageString('mpMain_LoadException');
    bhLoadException.Description := GetLanguageString('mpMain_PluginsNotLoaded');
    DisplayHints;

    // initialize task handler
    TaskHandler := TTaskHandler.Create;
    bLogTasks := false;
    TaskHandler.AddTask(TTask.Create('Disable Hints', 12.0 * seconds, DisableHints));
    TaskHandler.AddTask(TTask.Create('Reconnect', 15.0 * seconds, Reconnect));
    TaskHandler.AddTask(TTask.Create('Heartbeat', 0.9 * seconds, Heartbeat));
    TaskHandler.AddTask(TTask.Create('Refresh GUI', 3.0 * seconds, RefreshGUI));
    TaskHandler.AddTask(TTask.Create('Configure Settings', 0.8 * seconds, ConfigureSettings));
    TaskTimer.Enabled := true;
  end;

  // ACTIVATE WINDOW
  FormDisplayTime := Now;
  ForceForeground(Handle);
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

procedure TMergeForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  slDetails.Free;
end;

procedure TMergeForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := ProgramStatus.bClose;
  if not bClosing then begin
    bClosing := true;
    ToggleFormState(false);

    // show progress form
    pForm := TProgressForm.Create(Self);
    pForm.LogPath := PathList.Values['LogPath'];
    pForm.PopupParent := Self;
    pForm.Caption := GetLanguageString('mpProg_Closing');
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
  if ProgramStatus.bInstallUpdate then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);
  // restart program if user wants merge profile change
  if ProgramStatus.bChangeMergeProfile then
    ShellExecute(Application.Handle, 'runas', PChar(ParamStr(0)), '', '', SW_SHOWNORMAL);

  // allow close and close
  ProgramStatus.bClose := true;
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
  ToggleFormState(true);
  ShowWindow(Application.Handle, SW_RESTORE);
  SetForegroundWindow(Application.Handle);

  // free lists
  TryToFree(timeCosts);
  TryToFree(pluginsToHandle);
  TryToFree(mergesToBuild);

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
    if ProgramStatus.bDictionaryUpdate and UpdateDictionary then begin
      LocalStatus := TmpStatus.Create;
      CompareStatuses;
    end;
  end;
  if settings.updateProgram then begin
    // update program
    if ProgramStatus.bProgramUpdate and DownloadProgram then
      ProgramStatus.bInstallUpdate := UpdateProgram;
  end;
end;

function TMergeForm.ShouldDisplay(bh: TBalloonHint): boolean;
begin
  Result := (Now - FormDisplayTime) * 86400 < (bh.HideAfter / 1000);
end;

procedure TMergeForm.ConfigureSettings;
begin
  TaskHandler.RemoveTask('Configure Settings');

  // BRING UP OPTIONS FORM IF WE'RE ON A NEW PROFILE
  if settings.newProfile then
    OptionsButtonClick(nil);
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
  if ProgramStatus.bLoadException and ShouldDisplay(bhLoadException) then begin
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
  if not (TCPClient.Connected or ProgramStatus.bConnecting or bClosing) then
    TConnectThread.Create;
end;

procedure TMergeForm.RefreshGUI;
begin
  if not bClosing then UpdateStatusBar;
end;

procedure TMergeForm.Heartbeat;
begin
  try
    if not Assigned(TCPClient.IOHandler) then
      raise Exception.Create('IOHandler not open');
    if TCPClient.IOHandler.Opened and
    not (ProgramStatus.bConnecting or bClosing or ServerAvailable) then
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
  if ProgramStatus.bAuthorized then begin
    Logger.Write('CLIENT', 'Login', 'Authorized');
  end
  else begin
    Logger.Write('CLIENT', 'Login', 'Not authorized');
  end;
end;

procedure TMergeForm.UpdateStatusBar;
begin
  ImageBlocked.Visible := not (ProgramStatus.bLoaderDone or ProgramStatus.bInitException);
  ImageConnected.Visible := TCPClient.Connected;
  ImageDisconnected.Visible := not TCPClient.Connected;
  ImageBuild.Visible := ProgramStatus.bLoaderDone and bMergesToBuild;
  ImageDictionaryUpdate.Visible := ProgramStatus.bDictionaryUpdate;
  ImageProgramUpdate.Visible := ProgramStatus.bProgramUpdate;
  StatusPanelLanguage.Caption := settings.language;
end;

procedure TMergeForm.UpdateListViews;
begin
  if PageControl.ActivePage = PluginsTabSheet then begin
    UpdatePluginDetails;
    PluginsListView.Repaint;
  end;
  if PageControl.ActivePage = MergesTabSheet then begin
    UpdateMergeDetails;
    MergesListView.Repaint;
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
procedure TMergeForm.AddDetailsItem(name, value: string);
begin
  slDetails.Add(name + '=' + value);
end;

{
  Add one or more ListItem to DetailsView with @name and the values
  in @sl
}
procedure TMergeForm.AddDetailsList(name: string; sl: TStringList);
var
  i: integer;
  slTemp: TStringList;
begin
  slTemp := TStringList.Create;
  try
    slTemp.Text := Wordwrap(sl.Text, 80);
    if slTemp.Count > 0 then begin
      for i := 0 to Pred(slTemp.Count) do
        slDetails.Add(Format('%s[%d]=%s', [name, i, slTemp[i]]));
    end
    else
      slDetails.Add(name + '= ');
  finally
    slTemp.Free;
  end;
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
      ListView_CorrectWidth(PluginsListView);
    end;
    1: begin
      UpdateMergeDetails;
      ListView_CorrectWidth(MergesListView);
    end;
    2: begin
      UpdateApplicationDetails;
      ListView_CorrectWidth(LogListView);
    end;
  end;

  // force repaint
  PageControl.Repaint;
end;

procedure TMergeForm.UpdateApplicationDetails;
begin
  // prepare list view for application information
  slDetails.Clear;
  DetailsLabel.Caption := GetLanguageString('mpMain_AppDetails');

  // add details items
  AddDetailsItem(GetLanguageString('mpMain_Application'), 'Merge Plugins');
  AddDetailsItem(GetLanguageString('mpMain_Author'), 'matortheeternal');
  AddDetailsItem(GetLanguageString('mpMain_Version'), LocalStatus.programVersion);
  AddDetailsItem(GetLanguageString('mpMain_DateBuilt'), DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('mpMain_GameMode'), wbGameName);
  AddDetailsItem(GetLanguageString('mpMain_Language'), settings.language);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('mpMain_TimesRun'), IntToStr(statistics.timesRun + sessionStatistics.timesRun));
  AddDetailsItem(GetLanguageString('mpMain_MergesBuilt'), IntToStr(statistics.mergesBuilt + sessionStatistics.mergesBuilt));
  AddDetailsItem(GetLanguageString('mpMain_PluginsChecked'), IntToStr(statistics.pluginsChecked + sessionStatistics.pluginsChecked));
  AddDetailsItem(GetLanguageString('mpMain_PluginsFixed'), IntToStr(statistics.pluginsFixed + sessionStatistics.pluginsFixed));
  AddDetailsItem(GetLanguageString('mpMain_PluginsMerged'), IntToStr(statistics.pluginsMerged + sessionStatistics.pluginsMerged));
  AddDetailsItem(GetLanguageString('mpMain_ReportsSubmitted'), IntToStr(statistics.reportsSubmitted + sessionStatistics.reportsSubmitted));
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('mpMain_Website'), 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem(GetLanguageString('mpMain_ApiCredits'), 'superobject, TurboPower Abbrevia, xEdit');
  AddDetailsItem(GetLanguageString('mpMain_xEditVersion'), xEditVersion);
  AddDetailsItem(GetLanguageString('mpMain_xEditCredits'), 'zilav, hlp, Sharlikran, ElminsterAU');
  AddDetailsItem(GetLanguageString('mpMain_Testers'), ProgramTesters);
  AddDetailsItem(GetLanguageString('mpMain_Translators'), ProgramTranslators);

  // update details item count
  DetailsGrid.RowCount := slDetails.Count;
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
  for i := 0 to Pred(slDetails.Count) do begin
    previousName := name;
    name := slDetails.Names[i];
    previousValue := value;
    value := slDetails.ValueFromIndex[i];
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
procedure TMergeForm.DetailsGridMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: integer;
  value: string;
begin
  // only process left clicks
  if Button <> mbLeft then
    exit;

  DetailsGrid.MouseToCell(X, Y, ACol, ARow);
  // skip clicks on cells in column 0
  if ACol = 0 then
    exit;

  try
    value := slDetails.ValueFromIndex[ARow];
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

{ Handle drawing of a cell }
procedure TMergeForm.DetailsGridDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
  sText, sNextVal, sNextName: string;
  iHalfBottom, iPadding: Integer;
begin
  // initialize stuff
  sText := ' ';
  iPadding := (Rect.Bottom - Rect.Top) - DetailsGrid.Canvas.TextHeight('Hg');
  iHalfBottom := Rect.Top + (Rect.Bottom - Rect.Top) div 2;
  DetailsGrid.Font.Style := [];
  DetailsGrid.Font.Color := clBlack;

  // draw name
  if ACol = 0 then begin
    DetailsGrid.Canvas.Brush.Color := clMenu;
    DetailsGrid.Canvas.Rectangle(Rect);
    DetailsGrid.Canvas.Brush.Color := clWindow;
    DetailsGrid.Canvas.Rectangle(Rect.Left, Rect.Top, Rect.Right, iHalfBottom);

    if Assigned(slDetails) and (slDetails.Count > ARow) then
      sText := slDetails.Names[ARow];
    DetailsGrid.Canvas.Brush.Style := bsClear;
    DetailsGrid.Canvas.TextOut(Rect.Left + 4, Rect.Top + (iPadding div 2), sText);
  end
  // draw value
  else if ACol = 1 then begin
    DetailsGrid.Canvas.Brush.Color := clWindow;
    DetailsGrid.Canvas.Rectangle(Rect);

    if Assigned(slDetails) and (slDetails.Count > ARow) then
      sText := slDetails.ValueFromIndex[ARow];
    // handle special drawing of urls and master files
    if (Pred(slDetails.Count) > ARow) then begin
      sNextVal := slDetails.ValueFromIndex[ARow + 1];
      sNextName := slDetails.Names[ARow + 1];
      // urls blue and underlined
      if IsURL(sNextVal) then begin
        DetailsGrid.Font.Style := [fsUnderline];
        DetailsGrid.Font.Color := clBlue;
      end
      // esps and esms red if not loaded
      else if Pos('Plugins[', sNextName) = 1 then begin
        if not Assigned(PluginByFileName(sNextVal)) then
          DetailsGrid.Font.Color := clRed;
      end;
    end;

    // draw text
    DetailsGrid.Canvas.Brush.Style := bsClear;
    DetailsGrid.Canvas.TextOut(Rect.Left + 4, Rect.Top + (iPadding div 2), sText);
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
  slDetails.Clear;
  DetailsLabel.Caption := GetLanguageString('mpMain_PluginDetails');

  // get plugin information
  index := PluginsListView.ItemIndex;
  plugin := TPlugin(PluginsList[index]);
  if not plugin.hasData then plugin.GetMpData;

  // get flags description
  sl := TStringList.Create;
  sl.Text := plugin.GetFlagsDescription;

  // add details items
  AddDetailsItem(GetLanguageString('mpMain_Filename'), plugin.filename);
  AddDetailsItem(GetLanguageString('mpMain_Hash'), plugin.hash);
  AddDetailsItem(GetLanguageString('mpMain_FileSize'), FormatByteSize(plugin.fileSize));
  AddDetailsItem(GetLanguageString('mpMain_DateModified'), plugin.dateModified);
  AddDetailsItem(GetLanguageString('mpMain_MergeRating'), plugin.entry.rating);
  AddDetailsList(GetLanguageString('mpMain_Flags'), sl);
  AddDetailsItem(GetLanguageString('mpMain_NumRecords'), plugin.numRecords);
  AddDetailsItem(GetLanguageString('mpMain_NumOverrides'), plugin.numOverrides);
  AddDetailsItem(GetLanguageString('mpMain_Author'), plugin.author);
  AddDetailsList(GetLanguageString('mpMain_Description'), plugin.description);
  AddDetailsList(GetLanguageString('mpMain_Masters'), plugin.masters);
  AddDetailsList(GetLanguageString('mpMain_Errors'), plugin.errors);
  AddDetailsList(GetLanguageString('mpMain_Reports'), plugin.reports);

  // update gui
  DetailsGrid.RowCount := slDetails.Count;

  // free memory
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
      plugin.GetMpData;
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
  PluginsListView.Repaint;
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

{ Type to search for plugins. }
procedure TMergeForm.PluginsListViewKeyPress(Sender: TObject; var Key: Char);
var
  iFoundIndex: Integer;
  fBufferDiff: Real;
  sTempBuffer: string;
begin
  // Calculate time between current keystroke and last
  // keystroke we buffered
  fBufferDiff := Now - fLastBufferTime;

  // If we are within the buffer delay append the key to a
  // temporary buffer and search for next item matching the
  // buffer in the list view items.
  if fBufferDiff < fBufferDelay then begin
    fLastBufferTime := Now;
    sTempBuffer := sBuffer + Key;
    iFoundIndex := ListView_NextMatch(PluginsListView, sTempBuffer, 1);
    // If we found a match, handle it
    if iFoundIndex > -1 then begin
      ListView_HandleMatch(PluginsListView, iFoundIndex, sBuffer, sTempBuffer);
      Key := #0;
    end;
  end

  // Restart buffering if we didn't have an active buffer
  else begin
    fLastBufferTime := Now;
    sTempBuffer := Key;
    PluginsListView.ClearSelection;
    iFoundIndex := ListView_NextMatch(PluginsListView, sTempBuffer, 1);
    // If we found a match, handle it
    if iFoundIndex > -1 then begin
      ListView_HandleMatch(PluginsListView, iFoundIndex, sBuffer, sTempBuffer);
      Key := #0;
    end;
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
      hint := Format(GetLanguageString('mpMain_PluginFlags'), [plugin.filename, hint]);
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
    DoNotMergeItem.Caption := GetLanguageString('mpMain_AllowMerging')
  else
    DoNotMergeItem.Caption := GetLanguageString('mpMain_DoNotMergeItem_Caption');
  if bAllIgnoreErrors then
    IgnoreErrorsItem.Caption := GetLanguageString('mpMain_UnignoreErrors')
  else
    IgnoreErrorsItem.Caption := GetLanguageString('mpMain_IgnoreErrorsItem_Caption');


  // disable/enable menu items
  AddToMergeItem.Enabled := not (bBlacklisted or bPluginInMerge or bAllDoNotMerge);
  RemoveFromMergeItem.Enabled := bAllPluginsInMerge;
  DoNotMergeItem.Enabled := not (bBlacklisted or bPluginInMerge);
  CheckForErrorsItem.Enabled := ProgramStatus.bLoaderDone and bAllNeedErrorCheck and not bBlacklisted;
  FixErrorsItem.Enabled := ProgramStatus.bLoaderDone and bHasErrors and not bBlacklisted;
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
  MenuItem.Caption := GetLanguageString('mpMain_NewMergeItem_Caption');
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
  ToggleFormState(false);
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Checking'));

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
  ToggleFormState(false);
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Fixing'));

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
      merge := TMergeHelpers.MergeByName(MergesList, mergeName);
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
  ToggleFormState(false);
  ReportForm := TReportForm.Create(Self);
  try
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
        SaveReports(ReportForm.reportsList, PathList.Values['ProgramPath'] + 'reports\');
      end
      else begin
        Logger.Write('CLIENT', 'Reports', 'Saving reports locally');
        SaveReports(ReportForm.reportsList, PathList.Values['ProgramPath'] + 'reports\submitted\');
      end;
    end;
  finally
    ToggleFormState(true);
    // clean up
    ReportForm.Free;
    PluginsToReport.Free;
  end;
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
  Events involving the MergesListView control.  Events include:
  - UpdateMergeDetails
  - UpdateMerges
  - MergesListViewChange
  - MergesListViewData
  - MergesListViewDrawItem
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
  mergeItem := MergesListView.Selected;
  if not Assigned(mergeItem) then
    exit;

  // prepare list view for merge information
  slDetails.Clear;
  DetailsLabel.Caption := GetLanguageString('mpMain_MergeDetails');

  // get merge information
  merge := MergesList[MergesListView.ItemIndex];
  AddDetailsItem(GetLanguageString('mpMain_Status'), StatusArray[Ord(merge.status)].desc);
  AddDetailsItem(GetLanguageString('mpMain_MergeName'), merge.name);
  AddDetailsItem(GetLanguageString('mpMain_Filename'), merge.filename);
  AddDetailsItem(GetLanguageString('mpMain_PluginCount'), IntToStr(merge.plugins.Count));
  AddDetailsItem(GetLanguageString('mpMain_DateBuilt'), DateBuiltString(merge.dateBuilt));
  AddDetailsList(GetLanguageString('mpMain_Plugins'), merge.plugins);
  AddDetailsItem(' ', ' ');
  AddDetailsItem(GetLanguageString('mpMain_MergeMethod'), merge.method);
  AddDetailsItem(GetLanguageString('mpMain_Renumbering'), merge.renumbering);

  // files list
  sl := TStringList.Create;
  sl.Text := StringReplace(merge.files.Text, settings.mergeDirectory, '', [rfReplaceAll]);
  AddDetailsList(GetLanguageString('mpMain_Files'), sl);
  sl.Free;
  // fails list
  AddDetailsList(GetLanguageString('mpMain_Fails'), merge.fails);

  // update gui
  DetailsGrid.RowCount := slDetails.Count;
end;

procedure TMergeForm.UpdateMerges;
var
  i: integer;
  merge: TMerge;
begin
  // update merge count
  MergesListView.Items.Count := MergesList.Count;

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
  merge := TMergeHelpers.CreateNewMerge(MergesList);

  // edit merge immediately after its creation
  EditMerge := TEditForm.Create(Self);
  EditMerge.merge := merge;
  if EditMerge.ShowModal = mrOk then begin
    merge := EditMerge.merge;
    LogMessage('MERGE', 'New', 'Created new merge '+merge.name);
    // add merge to list and update views
    MergesList.Add(merge);
    UpdateMerges;
    MergesListView.Repaint;
    UpdatePluginsPopupMenu;
    // set result
    Result := merge;
  end;

  // add and update merge
  EditMerge.Free;
end;

procedure TMergeForm.MergesListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateMergeDetails;
  MergesListView.Repaint;
end;

procedure TMergeForm.MergesListViewData(Sender: TObject; Item: TListItem);
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
  MergesListView.Canvas.Font.Color := StatusArray[Ord(merge.status)].color;
  MergesListView.Canvas.Font.Style := MergesListView.Canvas.Font.Style + [fsBold];
end;

procedure TMergeForm.MergesListViewDrawItem(Sender: TCustomListView;
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

procedure TMergeForm.LogListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  // repaint whenever user changes selection
  LogListView.Repaint;
end;

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
  Result := IfThen(not b, GetLanguageString('mpMain_Enable'), GetLanguageString('mpMain_Disable'));
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
  ToggleAutoScrollItem.Caption := Format('%s %s', [EnableStr(bAutoScroll), GetLanguageString('mpMain_AutoScroll')]);
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
  ListView_CorrectWidth(LogListView);
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
  ListView_CorrectWidth(LogListView);
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
  - MergesListViewDblClick
  - MergesListViewKeyDown
}
{******************************************************************************}

procedure TMergeForm.MergesPopupMenuPopup(Sender: TObject);
var
  bNeverBuilt, bHasBuildStatus, bHasUpToDateStatus, bHasResolveStatus,
  bHasCheckStatus, bHasErrorStatus, bHasSelection, bHasPluginErrors,
  bIsNotTop, bIsNotBottom: boolean;
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
  bIsNotTop := true;
  bIsNotBottom := true;
  mergesSelected := 0;

  // loop through list view to find selection
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    Inc(mergesSelected);
    // update booleans
    if i = 0 then bIsNotTop := false;
    if i = Pred(MergesList.Count) then bIsNotBottom := false;
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
  BuildMergeItem.Enabled := bHasSelection and bHasBuildStatus and ProgramStatus.bLoaderDone;
  ToggleRebuildItem.Enabled := bHasSelection and not bNeverBuilt and
    (bHasUpToDateStatus or bHasBuildStatus);
  OpenInExplorerItem.Enabled := bHasSelection;
  // plugins submenu
  PluginsItem.Enabled := bHasSelection;
  ResolveIssuesItem.Enabled := bHasSelection and bHasResolveStatus;
  CheckPluginsItem.Enabled := bHasSelection and bHasCheckStatus and ProgramStatus.bLoaderDone;
  FixPluginsItem.Enabled := bHasSelection and bHasPluginErrors and ProgramStatus.bLoaderDone;
  ReportOnPluginsItem.Enabled := bHasSelection and bHasUpToDateStatus;
  // move submenu
  MoveItem.Enabled := bHasSelection;
  UpItem.Enabled := bHasSelection and bIsNotTop;
  DownItem.Enabled := bHasSelection and bIsNotBottom;
  ToTopItem.Enabled := bHasSelection and bIsNotTop;
  ToBottomItem.Enabled := bHasSelection and bIsNotBottom;

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
    BuildMergeItem.Caption := GetLanguageString(sBuild)
  else if bHasBuildStatus then
    BuildMergeItem.Caption := GetLanguageString(sRebuild)
  else begin
    BuildMergeItem.Enabled := false;
    BuildMergeItem.Caption := GetLanguageString(sRebuild);
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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
    MergesListView.Repaint;
  end;

  // update merge details and popup menu
  UpdateMergeDetails;
  UpdatePluginsPopupMenu;
end;

procedure TMergeForm.UpItemClick(Sender: TObject);
var
  i, max: Integer;
begin
  max := Pred(MergesListView.Items.Count);
  // if merge at index 0 is selected, exit
  // we can't move it up!
  if MergesListView.Items[0].Selected then
    exit;

  // loop through merges
  for i := 0 to max do begin
    if not MergesListView.Items[i].Selected then
      continue;
    MergesList.Move(i, i - 1);
    MergesListView.Items[i].Selected := false;
    MergesListView.Items[i - 1].Selected := true;
  end;

  // update gui
  UpdateListViews;
end;

procedure TMergeForm.DownItemClick(Sender: TObject);
var
  i, max: Integer;
begin
  max := Pred(MergesListView.Items.Count);
  // if merge at max index is selected, exit
  // we can't move it down!
  if MergesListView.Items[max].Selected then
    exit;

  // loop through merges in reverse so we don't move the same merge
  // multiple times
  for i := max downto 0 do begin
    if not MergesListView.Items[i].Selected then
      continue;
    MergesList.Move(i, i + 1);
    MergesListView.Items[i].Selected := false;
    MergesListView.Items[i + 1].Selected := true;
  end;

  // update gui
  UpdateListViews;
end;

procedure TMergeForm.ToTopItemClick(Sender: TObject);
var
  i, max, iIndex: Integer;
  tempList: TList;
begin
  max := Pred(MergesListView.Items.Count);
  // if merge at index 0 is selected, exit
  // we can't move it up!
  if MergesListView.Items[0].Selected then
    exit;

  // create tempList
  tempList := TList.Create;

  // loop through merges to build new list
  iIndex := 0;
  for i := 0 to max do begin
    if not MergesListView.Items[i].Selected then begin
      tempList.Add(MergesList[i]);
    end
    else begin
      tempList.Insert(iIndex, MergesList[i]);
      Inc(iIndex);
    end;
  end;

  // set MergesList to tempList
  MergesList.Clear;
  for i := 0 to max do MergesList.Add(tempList[i]);
  tempList.Free;

  // update selection
  for i := 0 to max do
    MergesListView.Items[i].Selected := i < iIndex;

  // update gui
  UpdateListViews;
end;

procedure TMergeForm.ToBottomItemClick(Sender: TObject);
var
  i, max, iIndex: Integer;
  tempList: TList;
begin
  max := Pred(MergesListView.Items.Count);
  // if merge at max index is selected, exit
  // we can't move it down!
  if MergesListView.Items[max].Selected then
    exit;

  // create tempList
  tempList := TList.Create;

  // loop through merges to build new list
  iIndex := 0;
  for i := 0 to max do begin
    if not MergesListView.Items[i].Selected then begin
      tempList.Insert(iIndex, MergesList[i]);
      Inc(iIndex);
    end
    else begin
      tempList.Add(MergesList[i]);
    end;
  end;

  // set MergesList to tempList
  MergesList.Clear;
  for i := 0 to max do MergesList.Add(tempList[i]);
  tempList.Free;

  // update selection
  for i := 0 to max do
    MergesListView.Items[i].Selected := i >= iIndex;

  // update gui
  UpdateListViews;
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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
  ToggleFormState(false);
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Fixing'));

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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
  ToggleFormState(false);
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Checking'));

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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
  for i := 0 to Pred(MergesListView.Items.Count) do
    if MergesListView.Items[i].Selected then begin
      merge := TMerge(MergesList[i]);
      mergesToDelete.Add(merge);
      MergesListView.Items[i].Selected := false;
      mergeNames := mergeNames + #13#10'    - ' + merge.name;
    end;

  // show multi-merge prompt if multiple merges selected
  if mergesToDelete.Count > 0 then
    bApproved := MessageDlg(GetLanguageString('mpMain_DeleteMerges') + mergeNames, mtConfirmation,
      mbOKCancel, 0) = mrOk;

  // exit if user didn't approve deletion
  if not bApproved then
    exit;

  // clear details editor
  slDetails.Clear;

  // loop through merges
  for i := Pred(mergesToDelete.Count) downto 0 do begin
    merge := TMerge(mergesToDelete[i]);
    Logger.Write('MERGE', 'Delete', 'Deleting merge '+merge.name);
    MergesListView.Items.Count := MergesListView.Items.Count - 1;

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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
  ToggleFormState(false);
  xEditLogGroup := 'MERGE';
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Merging'));

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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
      continue;
    merge := TMerge(MergesList[i]);
    Logger.Write('MERGE', 'Report', 'Reporting on '+merge.name);
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      pluginsList.Add(plugin);
    end;
  end;

  // report on all merges
  ToggleFormState(false);
  ReportForm := TReportForm.Create(Self);
  try
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
        SaveReports(ReportForm.reportsList, PathList.Values['ProgramPath'] + 'reports\');
      end
      else begin
        Logger.Write('MERGE', 'Report', 'Saving reports locally');
        SaveReports(ReportForm.reportsList, PathList.Values['ProgramPath'] + 'reports\submitted\');
      end;
    end;
  finally
    ToggleFormState(true);
    // clean up
    ReportForm.Free;
    pluginsList.Free;
  end;
end;

procedure TMergeForm.OpenInExplorerItemClick(Sender: TObject);
var
  i: Integer;
  path: string;
  merge: TMerge;
begin
  // loop through merges
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
  for i := 0 to Pred(MergesListView.Items.Count) do begin
    if not MergesListView.Items[i].Selected then
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
    Logger.Write('CLIENT', 'Status', 'Retrying connecting to the server.');
    ConnectionAttempts := 0;
  end;
end;

{ Double click to edit merge }
procedure TMergeForm.MergesListViewDblClick(Sender: TObject);
begin
  EditMergeItemClick(nil);
end;

{ Type to search for merges.  Also handles deleting merges with
  the delete key. }
procedure TMergeForm.MergesListViewKeyPress(Sender: TObject; var Key: Char);
var
  iFoundIndex: Integer;
  fBufferDiff: Real;
  sTempBuffer: string;
begin
  // Calculate time between current keystroke and last
  // keystroke we buffered
  fBufferDiff := Now - fLastBufferTime;

  // If we are within the buffer delay append the key to a
  // temporary buffer and search for next item matching the
  // buffer in the list view items.
  if fBufferDiff < fBufferDelay then begin
    fLastBufferTime := Now;
    sTempBuffer := sBuffer + Key;
    iFoundIndex := ListView_NextMatch(MergesListView, sTempBuffer, 1);
    // If we found a match, handle it
    if iFoundIndex > -1 then begin
      ListView_HandleMatch(MergesListView, iFoundIndex, sBuffer, sTempBuffer);
      Key := #0;
    end;
  end
  else begin
    // Shortcut to delete merges using the delete key
    if HiWord(GetKeyState(vk_Delete)) <> 0 then begin
      DeleteMergeItemClick(nil);
      Key := #0;
      exit;
    end;

    // Restart buffering if we didn't have an active buffer
    fLastBufferTime := Now;
    sTempBuffer := Key;
    MergesListView.ClearSelection;
    iFoundIndex := ListView_NextMatch(MergesListView, sTempBuffer, 1);
    // If we found a match, handle it
    if iFoundIndex > -1 then begin
      ListView_HandleMatch(MergesListView, iFoundIndex, sBuffer, sTempBuffer);
      Key := #0;
    end;
  end;
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
  i, j: Integer;
  bUncheckedPlugins, bMergesToReportOn: boolean;
  merge: TMerge;
  plugin: TPlugin;
  sTitle: string;
begin
  // DISABLE ALL BUTTONS IF INITIALIZATION EXCEPTION
  if ProgramStatus.bInitException then begin
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
    if not plugin.HasBeenCheckedForErrors then begin
      bUncheckedPlugins := true;
      break;
    end;
  end;

  // enable find errors button if there are unchecked plugins
  FindErrorsButton.Enabled := ProgramStatus.bLoaderDone and bUncheckedPlugins;
  // swap hints
  sTitle := GetLanguageString('mpMain_FindErrorsButton_Hint');
  if not ProgramStatus.bLoaderDone then
    FindErrorsButton.Hint := sTitle + GetLanguageString('mpMain_FindErrors_Loader')
  else if not bUncheckedPlugins then
    FindErrorsButton.Hint := sTitle + GetLanguageString('mpMain_NoPluginsToCheck')
  else
    FindErrorsButton.Hint := sTitle + GetLanguageString('mpMain_CheckAllPlugins');

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
  BuildButton.Enabled := bMergesToBuild and ProgramStatus.bLoaderDone;
  // swap hints
  sTitle := GetLanguageString('mpMain_BuildButton_Hint');
  if not ProgramStatus.bLoaderDone then
    BuildButton.Hint := sTitle + GetLanguageString('mpMain_BuildMerges_Loader')
  else if not bMergesToBuild then
    BuildButton.Hint := sTitle + GetLanguageString('mpMain_NoMerges')
  else if bMergesToCheck then
    BuildButton.Hint := sTitle + GetLanguageString('mpMain_CheckMerges')
  else
    BuildButton.Hint := sTitle + GetLanguageString('mpMain_BuildAllMerges');

  // REPORT BUTTON
  bMergesToReportOn := false;
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    if merge.status <> msUpToDate then continue;
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      if not Assigned(plugin) then
        continue;
      if not ReportExistsFor(plugin) then
        bMergesToReportOn := true;
    end;
  end;
  ReportButton.Enabled := bMergesToReportOn;
  if not bMergesToReportOn then
    ReportButton.Hint := GetLanguageString('mpMain_NoMergesToReportOn')
  else
    ReportButton.Hint := GetLanguageString('mpMain_ReportButton_Hint');

  // DICTIONARY BUTTON
  DictionaryButton.Enabled := dictionary.Count > 0;
  if not DictionaryButton.Enabled then
    DictionaryButton.Hint := GetLanguageString('mpMain_NoDictionary')
  else
    DictionaryButton.Hint := GetLanguageString('mpMain_DictionaryButton_Hint');

  // UPDATE BUTTON
  UpdateButton.Enabled := ProgramStatus.bProgramUpdate or ProgramStatus.bDictionaryUpdate;
  sTitle := GetLanguageString('mpMain_UpdateButton_Hint');
  if ProgramStatus.bProgramUpdate and ProgramStatus.bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetLanguageString('mpMain_UpdateBoth')
  else if ProgramStatus.bProgramUpdate then
    UpdateButton.Hint := sTitle + GetLanguageString('mpMain_UpdateProgram')
  else if ProgramStatus.bDictionaryUpdate then
    UpdateButton.Hint := sTitle + GetLanguageString('mpMain_UpdateDictionary')
  else
    UpdateButton.Hint := sTitle + GetLanguageString('mpMain_NoUpdates');

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
  if not ProgramStatus.bLoaderDone then begin
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
  ToggleFormState(false);
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Checking'));

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
  if not ProgramStatus.bLoaderDone then begin
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
  ToggleFormState(false);
  xEditLogGroup := 'MERGE';
  ShowProgressForm(self, pForm, GetLanguageString('mpProg_Merging'));

  // start merge thread
  MergeCallback := ProgressDone;
  TMergeThread.Create;
end;

{ Submit report }
procedure TMergeForm.ReportButtonClick(Sender: TObject);
var
  i, j: Integer;
  merge: TMerge;
  pluginsList: TList;
  plugin: TPlugin;
  bModalOK, bReportsSent: boolean;
begin
  // initialize variables
  pluginsList := TList.Create;

  // loop through plugins in merges
  for i := 0 to Pred(MergesList.Count) do begin
    merge := TMerge(MergesList[i]);
    // skip merges that aren't up to date - only want user to submit
    // reports on plugins in merges they've built
    if merge.status <> msUpToDate then continue;
    // loop through plugins in merge
    for j := 0 to Pred(merge.plugins.Count) do begin
      plugin := PluginByFilename(merge.plugins[j]);
      // if we can't find the plugin, continue
      if not Assigned(plugin) then continue;
      // if the user doesn't have a local report for the plugin
      // we will add it to the list of plugins for them to report on
      if not ReportExistsFor(plugin) then
        pluginsList.Add(plugin);
    end;
  end;

  // create report form
  bModalOK := false;
  ToggleFormState(false);
  ReportForm := TReportForm.Create(Self);
  try
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
        SaveReports(ReportForm.reportsList, PathList.Values['ProgramPath'] + 'reports\');
      end
      else begin
        Logger.Write('MERGE', 'Report', 'Saving reports locally');
        SaveReports(ReportForm.reportsList, PathList.Values['ProgramPath'] + 'reports\submitted\');
      end;
    end;
  finally
    ToggleFormState(true);
    // clean up
    ReportForm.Free;
    pluginsList.Free;
  end;
end;

{ View the dictionary file }
procedure TMergeForm.DictionaryButtonClick(Sender: TObject);
var
  DictionaryForm: TDictionaryForm;
begin
  ToggleFormState(false);
  DictionaryForm := TDictionaryForm.Create(Self);
  try
    DictionaryForm.ShowModal;
  finally
    DictionaryForm.Free;
    ToggleFormState(true);
  end;
end;

{ Options }
procedure TMergeForm.OptionsButtonClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
  prevLanguage: string;
begin
  prevLanguage := settings.language;
  ToggleFormState(false);
  // Create and show options form
  OptionsForm := TOptionsForm.Create(Self);
  OptionsForm.ShowModal;
  OptionsForm.Free;

  // update owner draw if changed
  PluginsListView.OwnerDraw := not settings.simplePluginsView;

  // initialize MO if usingMO changed
  if settings.usingMO then
    ModOrganizerInit;

  // if user changed language, update language displayed
  if settings.language <> prevLanguage then begin
    LoadLanguage;
    TRttiTranslation.Load(language, self);
  end;

  // update gui
  ToggleFormState(true);
  UpdateMerges;
  UpdateListViews;
  UpdateQuickBar;
  UpdateStatusBar;

  // if user selected to change game mode, close application
  if ProgramStatus.bChangeMergeProfile then
    Close;

  // if user selected to update program, close application
  if ProgramStatus.bInstallUpdate then begin
    ProgramStatus.bInstallUpdate := UpdateProgram;
    if ProgramStatus.bInstallUpdate then
      Close;
  end;
end;

{ Update }
procedure TMergeForm.UpdateButtonClick(Sender: TObject);
begin
  // if not connected to server, don't try to update anything
  if not TCPClient.Connected then
    exit;                                               

  // disable form
  ToggleFormState(false);

  // set up changelog variables
  clProgramVersion := LocalStatus.ProgramVersion;
  // update program
  if ProgramStatus.bProgramUpdate and ChangeLogPrompt(self) and DownloadProgram then begin
    ProgramStatus.bInstallUpdate := UpdateProgram;
    if ProgramStatus.bInstallUpdate then
      Close;
  end;

  // update dictionary
  if ProgramStatus.bDictionaryUpdate and UpdateDictionary then begin
    LocalStatus := TmpStatus.Create;
    CompareStatuses;
    UpdatePluginData;
    UpdateListViews;
  end;

  // re-enable form
  ToggleFormState(true);
end;

{ Help }
procedure TMergeForm.HelpButtonClick(Sender: TObject);
begin
  //LogMessage(TButton(Sender).Hint+' clicked!');
end;

end.
