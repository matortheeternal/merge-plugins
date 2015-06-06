unit mpReportForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  mpBase;

type
  TReportForm = class(TForm)
    lblHash: TLabel;
    lblRecords: TLabel;
    lblFilename: TLabel;
    gbDetails: TGroupBox;
    pnlTitle: TPanel;
    lblRating: TLabel;
    cbRating: TComboBox;
    lblNotes: TLabel;
    meNotes: TMemo;
    btnNext: TButton;
    btnPrev: TButton;
    lblFlags: TLabel;
    procedure DisplayCurrentEntry;
    procedure btnNextClick(Sender: TObject);
    procedure btnPrevClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure meNotesChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pluginsList: TList;
  end;

var
  ReportForm: TReportForm;
  entryList: TList;
  currentPlugin: integer;

implementation

{$R *.dfm}

procedure TReportForm.DisplayCurrentEntry;
var
  entry: TEntry;
  plugin: TPlugin;
begin
  plugin := TPlugin(pluginsList[currentPlugin]);

  // create next entry
  if currentPlugin = entryList.Count then begin
    entry := TEntry.Create;
    entry.pluginName := plugin.filename;
    entry.hash := plugin.hash;
    entry.records := plugin.numRecords;
    entry.rating := '4';
    entry.notes := ' ';
    entryList.Add(entry);
  end;

  // set controls to entry details
  entry := entryList[currentPlugin];
  lblFilename.Caption := entry.pluginName;
  lblHash.Caption := 'HASH: '+entry.hash;
  lblRecords.Caption := 'RECORDS: '+entry.records;
  lblFlags.Caption := 'FLAGS: '+plugin.GetFlagsString;
  lblFlags.Hint := plugin.GetFlagsDescription;
  cbRating.ItemIndex := StrToInt(entry.rating) + 1;
  meNotes.Lines.Text := entry.notes;

  // update whether or not the next button is enabled
  meNotesChange(meNotes);
end;

procedure TReportForm.btnNextClick(Sender: TObject);
var
  entry: TEntry;
begin
  // if at last plugin, close form with modal result
  if currentPlugin = Pred(pluginsList.Count) then begin
    ModalResult := mrOk;
    exit;
  end;

  // save settings in current entry
  entry := entryList[currentPlugin];
  entry.rating := IntToStr(cbRating.ItemIndex - 1);
  entry.notes := meNotes.Lines.Text;

  // go to next plugin
  Inc(currentPlugin);
  btnPrev.Enabled := true;
  if currentPlugin = Pred(pluginsList.Count) then
    btnNext.Caption := 'Done';

  // display entry
  DisplayCurrentEntry;
end;

procedure TReportForm.btnPrevClick(Sender: TObject);
var
  entry: TEntry;
begin
  // save settings in current entry
  entry := entryList[currentPlugin];
  entry.rating := IntToStr(cbRating.ItemIndex - 1);
  entry.notes := meNotes.Lines.Text;

  // go to previous plugin
  Dec(currentPlugin);
  btnPrev.Enabled := currentPlugin > 0;
  btnNext.Caption := 'Next';

  // display entry
  DisplayCurrentEntry;
end;

procedure TReportForm.FormShow(Sender: TObject);
begin
  // initialize vars
  currentPlugin := 0;
  entryList := TList.Create;

  // deal with special cases
  if not Assigned(pluginsList) then
    Close;
  case pluginsList.Count of
    0: Close;
    1: btnNext.Caption := 'Done';
  end;

  // display entry
  DisplayCurrentEntry;
end;

procedure TReportForm.meNotesChange(Sender: TObject);
begin
  btnNext.Enabled := Length(Trim(meNotes.Lines.Text)) > 15;
end;

end.
