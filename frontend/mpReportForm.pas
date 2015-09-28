unit mpReportForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  // mte units
  RttiTranslation,
  // mp units
  mpFrontend, mpDictionaryForm;

type
  TReportForm = class(TForm)
    [FormPrefix('mpRep')]
      btnNext: TButton;
      btnPrev: TButton;
      [FormSection('Info panel')]
        pnlInfo: TPanel;
        lblFilename: TLabel;
        lblFlags: TLabel;
        lblHash: TLabel;
        lblRecords: TLabel;
      [FormSection('User reports')]
        gbUserReports: TGroupBox;
        lblExRating: TLabel;
        lblExReports: TLabel;
        lblViewDetails: TLabel;
        lblExRatingValue: TLabel;
        lblExReportsvalue: TLabel;
      [FormSection('Your report')]
        gbYourReport: TGroupBox;
        lblRating: TLabel;
        cbRating: TComboBox;
        lblNotes: TLabel;
        lblCharacters: TLabel;
        meNotes: TMemo;

    procedure DisplayCurrentReport;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure meNotesChange(Sender: TObject);
    procedure lblViewDetailsClick(Sender: TObject);
    procedure lblViewDetailsMouseEnter(Sender: TObject);
    procedure lblViewDetailsMouseLeave(Sender: TObject);
    procedure cbRatingChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pluginsToReport: TList;
    reportsList: TList;
    AppName: string;
  end;

const
  RatingHints: array[0..5] of string = (
    'Only recommend a plugin be blacklisted if you''re certain there'#13+
    'are no circumstances in which it will ever be mergeable.  This is'#13+
    'for plugins that absolutely cannot be merged.  Please explain why'#13+
    'you feel the plugin should be blacklisted in the notes section.',
    'Failure to merge means merging the plugin fails completely.  E.g.'#13+
    'the merged plugin causes CTDs, major stability problems, or merging'#13+
    'fails to create a plugin at all.',
    'Dysfunctional means the merge doesn''t work ingame, with major aspects'#13+
    'of the plugins that were merged not working or being entirely missing.',
    'Partially functional means the majority of the plugin content works'#13+
    'but some of it doesn''t.  This would be if the plugin achieves its'#13+
    'goals but has bugs that were created in merging.',
    'Tweaking required means you got the plugin to work perfectly in a'#13+
    'merge, but had to tweak some things to get it to work.  E.g. errors'#13+
    'fixed or modification of asset files.',
    'Perfect means all aspects of the plugin work perfectly right away'#13+
    'and you didn''t have to do any tweaking.  Nothing missing, nothing'#13+
    'broken.'
  );

var
  ReportForm: TReportForm;
  currentPlugin: integer;
  PreviousNotes: string;

implementation

{$R *.dfm}

procedure TReportForm.DisplayCurrentReport;
var
  report: TReport;
  existingEntry: TEntry;
  plugin: TPlugin;
begin
  plugin := TPlugin(pluginsToReport[currentPlugin]);

  // create next entry
  if currentPlugin = reportsList.Count then begin
    report := TReport.Create;
    report.game := AppName;
    report.filename := plugin.filename;
    report.hash := plugin.hash;
    report.recordCount := StrToInt(plugin.numRecords);
    report.rating := 4;
    report.mergeVersion := ProgramVersion;
    report.SetNotes(PreviousNotes);

    // if user already made a report for this plugin, load it
    LoadReport(report);

    // add report to list
    reportsList.Add(report);
  end;

  // set pnlTitle labels to entry details
  report := reportsList[currentPlugin];
  lblFilename.Caption := StringReplace(report.filename, '&', '&&', [rfReplaceAll]);
  lblHash.Caption := Format(language.Values['mpRep_Hash'], [report.hash]);
  lblRecords.Caption := Format(language.Values['mpRep_Records'], [IntToStr(report.recordCount)]);
  lblFlags.Caption := Format(language.Values['mpRep_Flags'], [plugin.GetFlagsString]);
  lblFlags.Hint := plugin.GetFlagsDescription;

  // load existing entry details
  existingEntry := GetEntry(report.filename, '', '');
  lblExRatingValue.Caption := existingEntry.rating;
  lblExReportsValue.Caption := existingEntry.reports;
  // view details label control
  if existingEntry.reports = '0' then begin
    lblViewDetails.Enabled := false;
    lblViewDetails.Font.Color := $222222;
  end
  else begin
    lblViewDetails.Enabled := true;
    lblViewDetails.Font.Color := clHotLight;
  end;

  // load user's report details
  cbRating.ItemIndex := report.rating + 1;
  meNotes.Lines.Text := report.GetNotes;
  // activate rating hint
  cbRatingChange(nil);

  // update character count and next button state
  meNotesChange(meNotes);
end;

procedure TReportForm.btnNextClick(Sender: TObject);
var
  report: TReport;
begin
  // save settings in current entry
  report := TReport(reportsList[currentPlugin]);
  report.rating := cbRating.ItemIndex - 1;
  report.SetNotes(meNotes.Lines.Text);
  PreviousNotes := meNotes.Lines.Text;

  // if at last plugin, close form with modal result
  if currentPlugin = Pred(pluginsToReport.Count) then begin
    ModalResult := mrOk;
    exit;
  end;

  // go to next plugin
  Inc(currentPlugin);
  btnPrev.Enabled := true;
  if currentPlugin = Pred(pluginsToReport.Count) then
    btnNext.Caption := language.Values['mpRep_Done'];

  // display entry
  DisplayCurrentReport;
end;

procedure TReportForm.btnPrevClick(Sender: TObject);
var
  report: TReport;
begin
  // save settings in current entry
  report := TReport(reportsList[currentPlugin]);
  report.rating := cbRating.ItemIndex - 1;
  report.SetNotes(meNotes.Lines.Text);

  // go to previous plugin
  Dec(currentPlugin);
  btnPrev.Enabled := currentPlugin > 0;
  btnNext.Caption := language.Values['mpRep_Next'];

  // display entry
  DisplayCurrentReport;
end;

procedure TReportForm.cbRatingChange(Sender: TObject);
begin
  // set hint to current rating hint
  cbRating.Hint := RatingHints[cbRating.ItemIndex];
end;

procedure TReportForm.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);
end;

procedure TReportForm.FormShow(Sender: TObject);
begin
  // initialize vars
  currentPlugin := 0;
  reportsList := TList.Create;

  // deal with special cases
  if not Assigned(pluginsToReport) then
    Close;
  case pluginsToReport.Count of
    0: Close;
    1: btnNext.Caption := language.Values['mpRep_Done'];
  end;

  // display entry
  DisplayCurrentReport;
end;

procedure TReportForm.lblViewDetailsClick(Sender: TObject);
var
  DictionaryForm: TDictionaryForm;
  report: TReport;
begin
  // don't display if number of reports is 0
  if lblExReportsValue.Caption = '0' then
    exit;

  // create dictionary form filtered by plugin filename
  DictionaryForm := TDictionaryForm.Create(Self.Parent);
  report := TReport(reportsList[currentPlugin]);
  DictionaryForm.FilterFilename := report.filename;
  DictionaryForm.ShowModal;
  DictionaryForm.Free;
end;

// change link color on mouse enter
procedure TReportForm.lblViewDetailsMouseEnter(Sender: TObject);
begin
  if lblViewDetails.Enabled then
    lblViewDetails.Font.Color := clHighlight;
end;

// restore link color on mouse leave
procedure TReportForm.lblViewDetailsMouseLeave(Sender: TObject);
begin
  if lblViewDetails.Enabled then
    lblViewDetails.Font.Color := clHotlight;
end;

procedure TReportForm.meNotesChange(Sender: TObject);
var
  len: Integer;
  bTooShort, bTooLong: boolean;
begin
  len := Length(Trim(meNotes.Lines.Text));
  bTooShort := (len < 16);
  bTooLong := (len > 255);

  // disable next button if too long or too short
  btnNext.Enabled :=  not (bTooShort or bTooLong);

  // change characters label
  lblCharacters.Caption := IntToStr(len);

  // handle memo hint and label coloring
  if bTooShort then begin
    meNotes.Hint := language.Values['mpRep_NotesTooShort'];
    lblCharacters.Font.Color := clRed;
  end
  else if bTooLong then begin
    meNotes.Hint := language.Values['mpRep_NotesTooLong'];
    lblCharacters.Font.Color := clRed;
  end
  else begin
    meNotes.Hint := '';
    lblCharacters.Font.Color := clGreen;
  end;
end;

end.
