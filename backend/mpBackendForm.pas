unit mpBackendForm;

interface

uses
  // delphi units
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, ComCtrls, XPMan, StdCtrls, ImgList, CommCtrl,
  Menus, Grids, ValEdit, ShlObj, ShellAPI,
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
    LogMemo: TMemo;
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

    // MERGE FORM EVENTS
    procedure LogMessage(const s: string);
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
procedure TBackendForm.LogMessage(const s: string);
begin
  LogMemo.Lines.Add(s);
  SendMessage(LogMemo.Handle, EM_LINESCROLL, 0, LogMemo.Lines.Count);
end;

procedure ProgressMessage(const s: string);
begin
  if s = '' then
    exit;
  BackendForm.LogMessage(s);
end;

{ Initialize form, initialize TES5Edit API, and load plugins }
procedure TBackendForm.FormCreate(Sender: TObject);
begin
  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);
  try
    // INITIALIZE VARIABLES
    wbStartTime := Now;
    ProgramPath := ExtractFilePath(ParamStr(0));
    tempPath := ProgramPath + 'temp\';
    logPath := ProgramPath + 'logs\';
    ForceDirectories(tempPath);
    Logger.OnLogEvent := LogMessage;

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
    LoadDictionary(TES5Dictionary, 'TES5Dictionary.txt');
    LoadDictionary(TES4Dictionary, 'TES4Dictionary.txt');
    LoadDictionary(FO3Dictionary, 'FO3Dictionary.txt');
    LoadDictionary(FNVDictionary, 'FNVDictionary.txt');

    // LOAD REPORTS
    QueryReports;
    UnapprovedListView.Items.Count := UnapprovedReports.Count;
    ApprovedListView.Items.Count := ApprovedReports.Count;
  except
    on x: Exception do
      LogMessage('Exception: '+x.Message);
  end;
end;

procedure TBackendForm.FormShow(Sender: TObject);
begin
  // Force ListViews to autosize columns
  PageControl.ActivePageIndex := 1;
  UnapprovedListView.Width := UnapprovedListView.Width + 1;
  PageControl.ActivePageIndex := 0;
  ApprovedListView.Width := ApprovedListView.Width + 1;
end;

procedure TBackendForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // update statistics
  statistics.totalUptime := statistics.totalUptime + GetSessionUptime;
  statistics.totalBandwidth := statistics.totalBandwidth + sessionBandwidth;
  // save statistics
  statistics.Save('statistics.ini');
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
  AddDetailsItem('Unique IPs', IntToStr(settings.uniqueIPs.Count));
  AddDetailsItem('Blacklisted IPs', IntToStr(settings.blacklistedIPs.Count));
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
  AddDetailsItem('TES5 reports recieved', IntToStr(statistics.tes5ReportsRecieved));
  AddDetailsItem('TES4 reports recieved', IntToStr(statistics.tes4ReportsRecieved));
  AddDetailsItem('FNV reports recieved', IntToStr(statistics.fnvReportsRecieved));
  AddDetailsItem('FO3 reports recieved', IntToStr(statistics.fo3ReportsRecieved));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Session bandwidth', FormatByteSize(sessionBandwidth));
  AddDetailsItem('Session uptime', TimeStr(sessionUptime));
  AddDetailsItem('Total bandwidth', FormatByteSize(statistics.totalBandwidth + sessionBandwidth));
  AddDetailsItem('Total uptime', TimeStr(statistics.totalUptime + sessionUptime));
  AddDetailsItem(' ', ' ');
  AddDetailsItem('Website', 'http://www.nexusmods.com/skyrim/mods/37981');
  AddDetailsItem('API Credits', 'superobject, xEdit');
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
  - CompareReports
  - UnapprovedListViewChange
  - UnapprovedListViewData
  - ApprovedListViewChange
  - ApprovedListViewColumnClick
  - ApprovedListViewData
}
{******************************************************************************}

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
begin
  // comment
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
