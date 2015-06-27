unit mpBackendForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl,
  Menus, Grids, ValEdit, ShlObj, ShellAPI,
  // indy components
  IdContext, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer,
  IdGlobal, IdSync,
  // third party libraries
  superobject, W7Taskbar,
  // mp units
  mpBackend, mpLogger, mpDictionaryForm, mpOptionsForm, mpProgressForm,
  mpTracker, mpEditForm;

type
  TBackendForm = class(TForm)
    // GENERIC
    XPManifest: TXPManifest;
    IconList: TImageList;
    StatusBar: TStatusBar;
    // QUICKBAR
    QuickBar: TPanel;
    ApproveButton: TSpeedButton;
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
    // PAGE CONTROL - UNAPPROVED/APPROVED/LOG
    PageControl: TPageControl;
    UnapprovedTabSheet: TTabSheet;
    ApprovedTabSheet: TTabSheet;
    LogTabSheet: TTabSheet;
    UnapprovedListView: TListView;
    ApprovedListView: TListView;
    LogListView: TListView;
    // DETAILS EDITOR
    DetailsLabel: TLabel;
    DetailsEditor: TValueListEditor;
    // UNAPPROVED POPUP MENU
    UnapprovedPopupMenu: TPopupMenu;
    ApproveReportItem: TMenuItem;
    DeleteReportItem: TMenuItem;
    // APPROVED POPUP MENU
    ApprovedPopupMenu: TPopupMenu;
    UnapproveReportItem: TMenuItem;
    EditReportItem: TMenuItem;
    ViewDictionaryEntryItem: TMenuItem;
    TCPServer: TIdTCPServer;
    Timer: TTimer;

    // MERGE FORM EVENTS
    procedure LogMessage(const group, &label, text: string);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    // DETAILS EDITOR EVENTS
    function AddDetailsItem(name, value: string): TItemProp;
    procedure AddDetailsList(name: string; sl: TStringList);
    procedure PageControlChange(Sender: TObject);
    procedure UpdateUnapprovedDetails;
    procedure UpdateApprovedDetails;
    procedure UpdateApplicationDetails;
    procedure DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    // LIST VIEW EVENTS
    procedure UnapprovedListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure UnapprovedListViewData(Sender: TObject; Item: TListItem);
    procedure ApprovedListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ApprovedListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure ApprovedListViewData(Sender: TObject; Item: TListItem);
    // APPROVED POPUP MENU EVENTS
    procedure ApprovedPopupMenuPopup(Sender: TObject);
    procedure EditReportItemClick(Sender: TObject);
    procedure UnapproveReportItemClick(Sender: TObject);
    procedure ViewDictionaryEntryItemClick(Sender: TObject);
    // UNAPPROVED POPUP MENU EVENTS
    procedure UnapprovedPopupMenuPopup(Sender: TObject);
    procedure DeleteReportItemClick(Sender: TObject);
    procedure ApproveReportItemClick(Sender: TObject);
    // QUICKBAR BUTTON EVENTS
    procedure ApproveButtonClick(Sender: TObject);
    procedure RebuildButtonClick(Sender: TObject);
    procedure ReportButtonClick(Sender: TObject);
    procedure DictionaryButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    // SERVER EVENTS
    procedure TCPServerConnect(AContext: TIdContext);
    procedure TCPServerDisconnect(AContext: TIdContext);
    procedure TCPServerException(AContext: TIdContext; AException: Exception);
    procedure SendResponse(var user: TUser; var AContext: TIdContext; id: integer; data: string; bLog: boolean = true);
    procedure HandleMessage(msg: TmpMessage; size: integer; AContext: TIdContext);
    procedure WriteMessage(msg: TmpMessage; ip: string);
    procedure TCPServerExecute(AContext: TIdContext);
    procedure LogListViewData(Sender: TObject; Item: TListItem);
    procedure LogListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure ApprovedListViewDrawItem(Sender: TCustomListView; Item: TListItem;
      Rect: TRect; State: TOwnerDrawState);
    procedure OnTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BackendForm: TBackendForm;
  LastHint: string;
  LastURLTime: double;
  aAscending: boolean;
  aColumnToSort: integer;

implementation

{$R *.dfm}


{******************************************************************************}
{ Backend Form Events
  Events for the Backend Form.
  - LogMessage
  - ProgressMessage
  - FormCreate
  - FormShow
  - LoaderDone
  - FormClose
}
{******************************************************************************}

{ Prints a message to the log memo }
procedure TBackendForm.LogMessage(const group, &label, text: string);
begin
  Log.Add(TLogMessage.Create(FormatDateTime('nn:ss', Now - wbStartTime), group, &label, text));
  LogListView.Items.Count := Log.Count;
  LogListView.Items[Pred(Log.Count)].MakeVisible(false);
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TBackendForm.FormCreate(Sender: TObject);
begin
  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);
  try
    // INITIALIZE VARIABLES
    Log := TList.Create;
    wbStartTime := Now;
    Logger.OnLogEvent := LogMessage;
    ProgramPath := ExtractFilePath(ParamStr(0));
    tempPath := ProgramPath + 'temp\';
    logPath := ProgramPath + 'logs\';
    ForceDirectories(tempPath);
    status := TmpStatus.Create;

    // GUI ICONS
    Tracker.Write('Loading Icons');
    IconList.GetBitmap(0, ApproveButton.Glyph);
    IconList.GetBitmap(1, BuildButton.Glyph);
    IconList.GetBitmap(2, ReportButton.Glyph);
    IconList.GetBitmap(3, DictionaryButton.Glyph);
    IconList.GetBitmap(4, OptionsButton.Glyph);
    IconList.GetBitmap(5, UpdateButton.Glyph);
    IconList.GetBitmap(6, HelpButton.Glyph);

    // CREATE AND LOAD DICTIONARIES
    TES5Dictionary := TList.Create;
    TES4Dictionary := TList.Create;
    FO3Dictionary := TList.Create;
    FNVDictionary := TList.Create;
    LoadDictionary(TES5Dictionary, slTES5Dictionary, 'TES5Dictionary.txt');
    LoadDictionary(TES4Dictionary, slTES4Dictionary, 'TES4Dictionary.txt');
    LoadDictionary(FO3Dictionary, slFO3Dictionary, 'FO3Dictionary.txt');
    LoadDictionary(FNVDictionary, slFNVDictionary, 'FNVDictionary.txt');

    // QUERY DATA FROM MYSQL
    QueryReports;
    QueryUsers;
    QueryBlacklist;

    // PREPARE LIST VIEWS
    LogListView.OwnerDraw := not settings.simpleLogView;
    LogListView.ShowColumnHeaders := settings.simpleLogView;
    ShowScrollBar(LogListView.Handle, SB_BOTH, true);
    UnapprovedListView.OwnerDraw := not settings.simpleReportsView;
    ApprovedListView.OwnerDraw := not settings.simpleReportsView;
    UnapprovedListView.Items.Count := UnapprovedReports.Count;
    ApprovedListView.Items.Count := ApprovedReports.Count;
  except
    on x: Exception do
      LogMessage('INIT', 'Exception', x.Message);
  end;
end;

procedure TBackendForm.FormShow(Sender: TObject);
begin
  // Force ListViews to autosize columns
  PageControl.ActivePageIndex := 1;
  UnapprovedListView.Width := UnapprovedListView.Width + 1;
  PageControl.ActivePageIndex := 0;
  ApprovedListView.Width := ApprovedListView.Width + 1;
  PageControl.ActivePageIndex := 2;
end;

procedure TBackendForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // update statistics
  statistics.totalUptime := statistics.totalUptime + GetSessionUptime;
  statistics.totalBandwidth := statistics.totalBandwidth + sessionBandwidth;

  // save statistics, settings
  SaveStatistics;
  SaveSettings;
end;


procedure TBackendForm.OnTimer(Sender: TObject);
begin
  if PageControl.ActivePageIndex = 2 then
    UpdateApplicationDetails;
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
function TBackendForm.AddDetailsItem(name, value: string): TItemProp;
var
  prop: TItemProp;
begin
  DetailsEditor.InsertRow(name, value, true);
  prop := DetailsEditor.ItemProps[DetailsEditor.RowCount - 1];
  prop.ReadOnly := true;
  Result := prop;
end;

{
  Add one or more ListItem to DetailsView with @name and the values
  in @sl
}
procedure TBackendForm.AddDetailsList(name: string; sl: TStringList);
var
  i: integer;
begin
  if sl.Count > 0 then begin
    AddDetailsItem(name, sl[0]);
    for i := 1 to Pred(sl.Count) do
      AddDetailsItem(' ', sl[i]);
  end
  else
    AddDetailsItem(name, ' ');
end;

{
  Switch details view when page control is changed
}
procedure TBackendForm.PageControlChange(Sender: TObject);
var
  ndx: integer;
begin
  ndx := TPageControl(Sender).ActivePageIndex;
  case ndx of
    0: UpdateUnapprovedDetails;
    1: UpdateApprovedDetails;
    2: UpdateApplicationDetails;
  end;
end;

procedure TBackendForm.UpdateUnapprovedDetails;
var
  index: integer;
  report: TReport;
begin
  // don't do anything if no item selected
  if not Assigned(UnapprovedListView.Selected) then
    exit;

  // prepare list view for report information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Report Details';

  // get report information
  index := UnapprovedListView.ItemIndex;
  report := TReport(UnapprovedReports[index]);

  // add details items
  AddDetailsItem('Game', report.game);
  AddDetailsItem('Username', report.username);
  AddDetailsItem('Filename', report.filename);
  AddDetailsItem('Hash', report.hash);
  AddDetailsItem('Record count', IntToStr(report.recordCount));
  AddDetailsItem('Rating', IntToStr(report.rating));
  AddDetailsItem('Merge version', report.mergeVersion);
  AddDetailsList('Notes', report.notes);
  AddDetailsItem('Date submitted', DateToStr(report.dateSubmitted));
end;

procedure TBackendForm.UpdateApprovedDetails;
var
  index: integer;
  report: TReport;
begin
  // don't do anything if no item selected
  if not Assigned(ApprovedListView.Selected) then
    exit;

  // prepare list view for report information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Report Details';

  // get report information
  index := ApprovedListView.ItemIndex;
  report := TReport(ApprovedReports[index]);

  // add details items
  AddDetailsItem('Game', report.game);
  AddDetailsItem('Username', report.username);
  AddDetailsItem('Filename', report.filename);
  AddDetailsItem('Hash', report.hash);
  AddDetailsItem('Record count', IntToStr(report.recordCount));
  AddDetailsItem('Rating', IntToStr(report.rating));
  AddDetailsItem('Merge version', report.mergeVersion);
  AddDetailsList('Notes', report.notes);
  AddDetailsItem('Date submitted', DateToStr(report.dateSubmitted));
end;

procedure TBackendForm.UpdateApplicationDetails;
var
  sessionUptime: TDateTime;
begin
  // prepare list view for application information
  DetailsEditor.Strings.Clear;
  DetailsLabel.Caption := 'Application Details';

  // get current uptime
  sessionUptime := GetSessionUptime;

  // add details items
  AddDetailsItem('Application', 'Merge Plugins Backend');
  AddDetailsItem('Author', 'matortheeternal');
  AddDetailsItem('Version', GetVersionMem);
  AddDetailsItem('Date built', DateTimeToStr(GetLastModified(ParamStr(0))));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Times run', IntToStr(statistics.timesRun));
  AddDetailsItem('Unique users', IntToStr(Users.Count));
  AddDetailsItem('Blacklist size', IntToStr(Blacklist.Count));
  AddDetailsItem('Dictionary updates', IntToStr(statistics.dictionaryUpdates));
  AddDetailsItem('Program updates', IntToStr(statistics.programUpdates));
  AddDetailsItem('Reports recieved', IntToStr(statistics.reportsRecieved));
  AddDetailsItem('Reports approved', IntToStr(statistics.reportsApproved));
  AddDetailsItem('Reports denied', IntToStr(statistics.reportsDenied));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('TES5 logins', IntToStr(statistics.tes5Logins));
  AddDetailsItem('TES4 logins', IntToStr(statistics.tes4Logins));
  AddDetailsItem('FNV logins', IntToStr(statistics.fnvLogins));
  AddDetailsItem('FO3 logins', IntToStr(statistics.fo3Logins));
  AddDetailsItem('TES5 reports recieved', IntToStr(statistics.tes5Reports));
  AddDetailsItem('TES4 reports recieved', IntToStr(statistics.tes4Reports));
  AddDetailsItem('FNV reports recieved', IntToStr(statistics.fnvReports));
  AddDetailsItem('FO3 reports recieved', IntToStr(statistics.fo3Reports));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Session bandwidth', FormatByteSize(sessionBandwidth));
  AddDetailsItem('Session uptime', TimeStr(sessionUptime));
  AddDetailsItem('Total bandwidth', FormatByteSize(statistics.totalBandwidth + sessionBandwidth));
  AddDetailsItem('Total uptime', TimeStr(statistics.totalUptime + sessionUptime));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Website', 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem('API Credits', 'superobject, ZeosDBO, xEdit');
end;

{ Handle user clicking URL }
procedure TBackendForm.DetailsEditorMouseUp(Sender: TObject; Button: TMouseButton;
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
{ List View events
  - NotesListViewData
  - CompareReports
  - UnapprovedListViewChange
  - UnapprovedListViewData
  - ApprovedListViewChange
  - ApprovedListViewColumnClick
  - ApprovedListViewData
}
{******************************************************************************}

procedure TBackendForm.LogListViewData(Sender: TObject; Item: TListItem);
var
  msg: TLogMessage;
begin
  if (Item.Index > Pred(Log.Count)) then
    exit;
  msg := TLogMessage(Log[Item.Index]);
  Item.Caption := msg.time;
  Item.SubItems.Add(msg.group);
  Item.SubItems.Add(msg.&label);
  Item.SubItems.Add(msg.text);

  // handle coloring
  if (msg.group = 'SERVER') then
    LogListView.Canvas.Font.Color := settings.serverMessageColor
  else if (msg.group = 'INIT') then
    LogListView.Canvas.Font.Color := settings.initMessageColor
  else if (msg.group = 'SQL') then
    LogListView.Canvas.Font.Color := settings.SQLMessageColor
  else if (msg.group = 'DICTIONARY') then
    LogListView.Canvas.Font.Color := settings.dictionaryMessageColor
  else if (msg.group = 'JAVA') then
    LogListView.Canvas.Font.Color := settings.javaMessageColor
  else if (msg.group = 'ERROR') then
    LogListView.Canvas.Font.Color := settings.errorMessageColor;
end;

procedure TBackendForm.LogListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i, x, y: integer;
  ListView: TListView;
  R: TRect;
  msg, FormatString: string;
  ItemArray: array of TVarRec;
  Column: TListColumn;
begin
  ListView := TListView(Sender);
  if Item.Selected then begin
    ListView.Canvas.Brush.Color := $FFEEDD;
    ListView.Canvas.FillRect(Rect);
  end;

  // prepare format string
  FormatString := '';
  for i := 0 to Pred(ListView.Columns.Count) do begin
    Column := ListView.Columns[i];
    if Column.Caption = 'Time' then
      FormatString := FormatString + '[%s] ';
    if Column.Caption = 'Group' then
      FormatString := FormatString + '(%s) ';
    if Column.Caption = 'Label' then
      FormatString := FormatString + '%s: ';
    if Column.Caption = 'Text' then
      FormatString := FormatString + '%s';
  end;

  // prepare item array
  SetLength(ItemArray, 1 + Item.SubItems.Count);
  ItemArray[0].VType := vtUnicodeString;
  ItemArray[0].VUnicodeString := Pointer(Item.Caption);
  for i := 0 to Pred(Item.SubItems.Count) do begin
    ItemArray[i + 1].VType := vtUnicodeString;
    ItemArray[i + 1].VUnicodeString := Pointer(Item.SubItems[i]);
  end;

  // prepare text rect
  R := Rect;
  R.Right := R.Left + ListView.Width - 3;
  x := Rect.Left + 3;
  y := (Rect.Bottom - Rect.Top - ListView.Canvas.TextHeight('Hg')) div 2 + Rect.Top;

  // draw message
  msg := Format(FormatString, ItemArray);
  ListView.Canvas.TextRect(R, x, y, msg);
end;

function CompareReports(P1, P2: Pointer): Integer;
var
  report1, report2: TReport;
begin
  Result := 0;
  report1 := TReport(P1);
  report2 := TReport(P2);

  if aColumnToSort = 0 then
    Result := AnsiCompareText(report1.game, report2.game)
  else if aColumnToSort = 1 then
    Result := AnsiCompareText(report1.filename, report2.filename)
  else if aColumnToSort = 2 then
    Result := Trunc(report1.dateSubmitted) - Trunc(report2.dateSubmitted)
  else if aColumnToSort = 3 then
    Result := AnsiCompareText(report1.username, report2.username)
  else if aColumnToSort = 4 then
    Result := report1.rating - report2.rating;

  if aAscending then
    Result := -Result;
end;

procedure TBackendForm.UnapprovedListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  UpdateUnapprovedDetails;
end;

procedure TBackendForm.UnapprovedListViewData(Sender: TObject; Item: TListItem);
var
  report: TReport;
begin
  report := TReport(UnapprovedReports[Item.Index]);
  Item.Caption := report.game;
  Item.SubItems.Add(report.filename);
  Item.SubItems.Add(DateToStr(report.dateSubmitted));
  Item.SubItems.Add(report.username);
  Item.SubItems.Add(IntToStr(report.rating));
end;

procedure TBackendForm.ApprovedListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateApprovedDetails;
end;

procedure TBackendForm.ApprovedListViewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  aAscending := (aColumnToSort = Column.Index) and (not aAscending);
  aColumnToSort := Column.Index;
  ApprovedReports.Sort(CompareReports);
  ApprovedListView.Repaint;
  UpdateApprovedDetails;
end;

procedure TBackendForm.ApprovedListViewData(Sender: TObject; Item: TListItem);
var
  report: TReport;
begin
  report := TReport(ApprovedReports[Item.Index]);
  Item.Caption := report.game;
  Item.SubItems.Add(report.filename);
  Item.SubItems.Add(DateToStr(report.dateSubmitted));
  Item.SubItems.Add(report.username);
  Item.SubItems.Add(IntToStr(report.rating));
  ApprovedListView.Canvas.Font.Color := GetRatingColor(report.rating);
  ApprovedListView.Canvas.Font.Style := [fsBold];
end;

procedure TBackendForm.ApprovedListViewDrawItem(Sender: TCustomListView;
  Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  ListView: TListView;
  R: TRect;
  x, y, i: integer;
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
{ Approved Popup Menu events
  - ApprovedPopupMenuPopup
  - EditReportItemClick
  - UnapproveReportItemClick
  - ViewDictionaryEntryItemClick
}
{******************************************************************************}

procedure TBackendForm.ApprovedPopupMenuPopup(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  bItemSelected: boolean;
begin
  bItemSelected := false;
  // process list items
  for i := 0 to Pred(ApprovedListView.Items.Count) do begin
    ListItem := ApprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    bItemSelected := true;
  end;

  EditReportItem.Enabled := bItemSelected;
  UnapproveReportItem.Enabled := bItemSelected;
  ViewDictionaryEntryItem.Enabled := bItemSelected;
end;

procedure TBackendForm.EditReportItemClick(Sender: TObject);
var
  EditForm: TEditForm;
  i: integer;
  ListItem: TListItem;
  report: TReport;
  WhereClause, SetClause: string;
begin
  // process list items
  for i := 0 to Pred(ApprovedListView.Items.Count) do begin
    ListItem := ApprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    // get report data
    report := TReport(ApprovedReports[i]);
    WhereClause := ReportWhereClause(report);

    // show edit form for report
    EditForm := TEditForm.Create(Self);
    EditForm.report := report;
    EditForm.ShowModal;

    // get edited values
    report := EditForm.report;
    SetClause := ReportSetClause(report);
    ApprovedReports[i] := report;

    // update report in database
    UpdateReport(report, SetClause, WhereClause);

    // clean up
    UpdateApprovedDetails;
    EditForm.Free;
  end;
end;

procedure TBackendForm.UnapproveReportItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  report: TReport;
begin
  // process list items
  for i := Pred(ApprovedListView.Items.Count) downto 0 do begin
    ListItem := ApprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    report := TReport(ApprovedReports[i]);
    // update internal lists
    UnapprovedReports.Add(report);
    UnapprovedListView.Items.Count := UnapprovedReports.Count;
    ApprovedListView.Items.Count := ApprovedReports.Count - 1;
    ApprovedReports.Remove(report);
    // update sql
    AddReport(report, 'unapproved_reports');
    RemoveReport(report, 'approved_reports');
  end;

  // update gui
  ApprovedListView.Repaint;
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.ViewDictionaryEntryItemClick(Sender: TObject);
var
  report: TReport;
  DictionaryForm: TDictionaryForm;
begin
  report := TReport(ApprovedReports[ApprovedListView.ItemIndex]);
  DictionaryForm := TDictionaryForm.Create(Self);
  DictionaryForm.Game := report.game;
  DictionaryForm.FilterFilename := report.filename;
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

{******************************************************************************}
{ Umapproved Popup Menu events
  - UnapprovedPopupMenuPopup
  - ApproveReportItemClick
  - DeleteReportItemClick
}
{******************************************************************************}

procedure TBackendForm.UnapprovedPopupMenuPopup(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  bItemSelected: boolean;
begin
  bItemSelected := false;
   // process list items
  for i := 0 to Pred(UnapprovedListView.Items.Count) do begin
    ListItem := UnapprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    bItemSelected := true;
  end;

  DeleteReportItem.Enabled := bItemSelected;
  ApproveReportItem.Enabled := bItemSelected;
end;

procedure TBackendForm.DeleteReportItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  report: TReport;
begin
  // process list items
  for i := Pred(UnapprovedListView.Items.Count) downto 0 do begin
    ListItem := UnapprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    report := TReport(UnapprovedReports[i]);
    // update internal lists
    UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
    UnapprovedReports.Remove(report);
    // update sql
    RemoveReport(report, 'unapproved_reports');
  end;

  // update gui
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.ApproveReportItemClick(Sender: TObject);
var
  i: integer;
  ListItem: TListItem;
  report: TReport;
begin
  // process list items
  for i := Pred(UnapprovedListView.Items.Count) downto 0 do begin
    ListItem := UnapprovedListView.Items[i];
    // skip items that aren't selected
    if not ListItem.Selected then
      continue;

    report := TReport(UnapprovedReports[i]);
    // update internal lists
    ApprovedReports.Add(report);
    ApprovedListView.Items.Count := ApprovedReports.Count;
    UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
    UnapprovedReports.Remove(report);
    // update sql
    AddReport(report, 'approved_reports');
    RemoveReport(report, 'unapproved_reports');
  end;

  // update gui
  ApprovedListView.Repaint;
  UnapprovedListView.Repaint;
end;


{******************************************************************************}
{ TCP Server Events
  Events associated with the TCP server.
  - TCPServerConnect
  - TCPServerDisconnect
  - TCPServerException
  - TCPServerExecute
}
{******************************************************************************}

procedure TBackendForm.TCPServerConnect(AContext: TIdContext);
var
  ip: string;
  user: TUser;
begin
  ip := AContext.Connection.Socket.Binding.PeerIP;
  // handle blacklisted ip
  if IsBlacklisted(ip) then begin
    LogMessage('SERVER', 'Terminated', ip);
    AContext.Connection.Disconnect;
    exit;
  end;

  // handle connection
  LogMessage('SERVER', 'Connected', ip);
  user := GetUser(ip);
  if not Assigned(user) then
    user := AddUser(ip);
  Inc(user.timesSeen);
end;

procedure TBackendForm.TCPServerDisconnect(AContext: TIdContext);
var
  ip: string;
begin
  ip := AContext.Connection.Socket.Binding.PeerIP;
  LogMessage('SERVER', 'Disconnected', ip);
end;

procedure TBackendForm.TCPServerException(AContext: TIdContext;
  AException: Exception);
begin
  if AException.Message = 'Connection Closed Gracefully.' then
    exit;

  LogMessage('SERVER', 'Exception', AContext.Connection.Socket.Binding.PeerIP+
    ' '+AException.Message);
end;

procedure TBackendForm.SendResponse(var user: TUser; var AContext: TIdContext;
  id: integer; data: string; bLog: boolean = true);
var
  json: string;
  response: TmpMessage;
begin
  response := TmpMessage.Create(id, '', '', data);
  json := response.ToJson;
  Inc(user.download, Length(json));
  Inc(sessionBandwidth, Length(json));
  AContext.Connection.IOHandler.WriteLn(json);
  if bLog then LogMessage('SERVER', 'Response', response.data);
  response.Free;
end;

procedure TBackendForm.HandleMessage(msg: TmpMessage; size: integer;
  AContext: TIdContext);
var
  report: TReport;
  note, ip, WhereClause, SetClause: string;
  user: TUser;
  stream: TMemoryStream;
  bAuthorized: boolean;
begin
  // get ip, authorization
  ip := AContext.Connection.Socket.Binding.PeerIP;
  bAuthorized := Authorized(ip, msg.username, msg.auth);

  // get user and update values
  if bAuthorized then
    user := GetUser(ip, msg.username, msg.auth)
  else
    user := GetUser(ip);
  if not Assigned(user) then
    user := AddUser(ip);

  WhereClause := UserWhereClause(user);
  user.lastSeen := Now;
  Inc(user.upload, size);
  Inc(sessionBandwidth, size);

  // write message to log
  WriteMessage(msg, ip);

  // handle message by id
  case msg.id of
    MSG_NOTIFY: begin
      if msg.data = 'Authorized?' then begin
        note := 'No';
        if bAuthorized then
          note := 'Yes';
        // respond to user
        SendResponse(user, AContext, MSG_NOTIFY, note);
      end
      // game based logins
      else if msg.data = 'TES5' then
        Inc(statistics.tes5Logins)
      else if msg.data = 'TES4' then
        Inc(statistics.tes4Logins)
      else if msg.data = 'FNV' then
        Inc(statistics.fnvLogins)
      else if msg.data = 'FO3' then
        Inc(statistics.fo3Logins);
    end;

    MSG_REGISTER: begin
      note := 'Username unavailable';
      if (GetUser(ip, msg.username, msg.auth) = nil) then begin
        if msg.data = 'Check' then
          note := 'Available'
        else begin
          UpdateUser(ip, msg.username, msg.auth);
          note := 'Registered '+msg.username;
        end;
      end;
      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;

    MSG_AUTH_RESET:  begin
      note := 'Failed';
      if ResetAuth(ip, msg.username, msg.auth) then
        note := 'Success';
      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;

    MSG_STATISTICS:  begin
       // TODO: Handle user statistics
    end;

    MSG_STATUS: begin
      SendResponse(user, AContext, MSG_STATUS, status.ToJson, false);
      LogMessage('SERVER', 'Response', 'Current status');
    end;

    MSG_REQUEST:  begin
      if (Pos('Dictionary', msg.data) > 0) then begin
        if FileExists(msg.data) then begin
          stream := TMemoryStream.Create;
          stream.LoadFromFile(msg.data);
          AContext.Connection.IOHandler.LargeStream := True;
          AContext.Connection.IOHandler.Write(stream, 0, true);
          Inc(sessionBandwidth, stream.Size);
          Inc(user.download, stream.Size);
          Inc(statistics.dictionaryUpdates);
          LogMessage('SERVER', 'Response', 'Sent '+msg.data+' '+ GetDictionaryHash(msg.data));
          stream.Free;
        end;
      end
      else if msg.data = 'Program' then begin
        if FileExists('MergePlugins.zip') then begin
          stream := TMemoryStream.Create;
          stream.LoadFromFile('MergePlugins.zip');
          AContext.Connection.IOHandler.LargeStream := True;
          AContext.Connection.IOHandler.Write(stream, 0, true);
          Inc(sessionBandwidth, stream.Size);
          Inc(user.download, stream.Size);
          Inc(statistics.programUpdates);
          LogMessage('SERVER', 'Response', 'Sent '+msg.data+' '+ status.programVersion);
          stream.Free;
        end
        else
          LogMessage('SERVER', 'Error', 'MergePlugins.zip doesn''t exist!');
      end;
    end;

    MSG_REPORT: begin
      note := 'Not authorized';
      if bAuthorized then try
        report := TReport.Create;
        report.FromJson(msg.data);
        report.username := msg.username;
        report.dateSubmitted := Now;
        report.notes.Text := StringReplace(report.notes.Text, '@13', #13#10, [rfReplaceAll]);
        // add report to gui
        UnapprovedReports.Add(report);
        UnapprovedListView.Items.Count := UnapprovedReports.Count;
        // add report to sql
        AddReport(report, 'unapproved_reports');
        note := 'Report accepted.';
        Inc(statistics.reportsRecieved);
        // update statistics based on game
        if report.game = 'TES5' then
          Inc(statistics.tes5Reports)
        else if report.game = 'TES4' then
          Inc(statistics.tes4Reports)
        else if report.game = 'FNV' then
          Inc(statistics.fnvLogins)
        else if report.game = 'FO3' then
          Inc(statistics.fo3Reports);
      except
        on x : Exception do
          note := 'Failed to load report.';
      end;

      // respond to user
      SendResponse(user, AContext, MSG_NOTIFY, note);
    end;
  end;

  // update user
  SetClause := UserSetClause(user);
  UpdateUser(SetClause, WhereClause);
end;

procedure TBackendForm.WriteMessage(msg: TmpMessage; ip: string);
begin
  LogMessage('SERVER', 'Message', ip+' ('+msg.username+')  '+MSG_STRINGS[msg.id]);
  if (msg.data <> '') then
    LogMessage('SERVER', 'Message', msg.data);
end;

procedure TBackendForm.TCPServerExecute(AContext: TIdContext);
var
  msg: TmpMessage;
  LLine: string;
  size: integer;
begin
  LLine := AContext.Connection.IOHandler.ReadLn(TIdTextEncoding.Default);
  msg := TmpMessage.Create;
  msg.FromJson(LLine);
  size := Length(LLine);
  Inc(sessionBandwidth, size);
  HandleMessage(msg, size, AContext);

  // free message
  msg.Free;
end;

{******************************************************************************}
{ QuickBar Button Events
  Events involving buttons on the QuickBar.  Events include:
  - ApproveButtonClick
  - RebuildButtonClick
  - ReportButtonClick
  - OptionsButtonClick
  - DictionaryButtonClick
  - UpdateButtonClick
  - HelpButtonClick
}
{******************************************************************************}

procedure TBackendForm.ApproveButtonClick(Sender: TObject);
var
  i: integer;
  report: TReport;
begin
  // process list items
  for i := Pred(UnapprovedListView.Items.Count) downto 0 do begin
    report := TReport(UnapprovedReports[i]);
    // update internal lists
    ApprovedReports.Add(report);
    ApprovedListView.Items.Count := ApprovedReports.Count;
    UnapprovedListView.Items.Count := UnapprovedReports.Count - 1;
    UnapprovedReports.Remove(report);
    // update sql
    AddReport(report, 'approved_reports');
    RemoveReport(report, 'unapproved_reports');
  end;

  // update gui
  ApprovedListView.Repaint;
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.RebuildButtonClick(Sender: TObject);
begin
  // comment
end;

procedure TBackendForm.ReportButtonClick(Sender: TObject);
begin
  // comment
end;

procedure TBackendForm.DictionaryButtonClick(Sender: TObject);
var
  DictionaryForm: TDictionaryForm;
begin
  DictionaryForm := TDictionaryForm.Create(Self);
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

procedure TBackendForm.OptionsButtonClick(Sender: TObject);
var
  OptionsForm: TOptionsForm;
begin
  OptionsForm := TOptionsForm.Create(Self);
  OptionsForm.ShowModal;
  OptionsForm.Free;
  // update listviews
  LogListView.OwnerDraw := not settings.simpleLogView;
  LogListView.Repaint;
  ApprovedListView.OwnerDraw := not settings.simpleReportsView;
  ApprovedListView.Repaint;
  UnapprovedListView.OwnerDraw := not settings.simpleReportsView;
  UnapprovedListView.Repaint;
end;

procedure TBackendForm.UpdateButtonClick(Sender: TObject);
begin
  // comment
end;

procedure TBackendForm.HelpButtonClick(Sender: TObject);
begin
  // comment
end;

end.
