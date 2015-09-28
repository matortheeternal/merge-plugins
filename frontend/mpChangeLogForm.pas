unit mpChangeLogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  // mte units
  mteHelpers, RttiTranslation,
  // mp units
  mpFrontend;

type
  TChangeLogForm = class(TForm)
    [FormPrefix('mpCha')]
      ScrollBox: TScrollBox;
      LabelPrompt: TLabel;
      ButtonInstall: TButton;
      ButtonSkip: TButton;

    procedure FormCreate(Sender: TObject);
    procedure CreateVersionLabel(line: string; var top: Integer);
    procedure CreateLabel(line: string; var top: Integer);
    procedure DisplayChangelog;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  // public entry point
  function ChangeLogPrompt(AOwner: TComponent): boolean;

const
  spacing = 5;

var
  ChangeLogForm: TChangeLogForm;

implementation

{$R *.dfm}

procedure TChangeLogForm.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);

  // display changelog
  DisplayChangelog;
end;

function IsVersionLine(line: string): boolean;
begin
  Result := Pos('Version ', line) = 1;
end;

procedure TChangeLogForm.CreateVersionLabel(line: string; var top: Integer);
var
  lbl: TLabel;
begin
  // make version label
  lbl := TLabel.Create(ScrollBox);
  lbl.Parent := ScrollBox;
  lbl.Autosize := true;
  lbl.Top := top;
  lbl.Left := 8;
  lbl.Caption := line;
  lbl.Font.Style := [fsBold];

  // increment top for next label
  Inc(top, lbl.Height + spacing);
end;

procedure TChangeLogForm.CreateLabel(line: string; var top: Integer);
var
  lbl: TLabel;
begin
  // make label
  lbl := TLabel.Create(ScrollBox);
  lbl.Parent := ScrollBox;
  lbl.AutoSize := true;
  lbl.WordWrap := true;
  lbl.Top := top;
  lbl.Left := 20;
  lbl.Width := ScrollBox.ClientWidth - 36;
  lbl.Caption := Trim(line);

  // increment top for next label
  Inc(top, lbl.Height + spacing);
end;

procedure TChangeLogForm.DisplayChangelog;
var
  i, top, start: Integer;
  line, lineVersion: string;
begin
  // find start line
  start := 0;
  if not Assigned(changelog) then
    exit;
  for i := 0 to Pred(changelog.Count) do begin
    line := changelog[i];
    if not IsVersionLine(line) then
      continue;

    // identify start of changelog as first version newer than current version
    lineVersion := Copy(line, 9, Length(line));
    if VersionCompare(status.programVersion, lineVersion) then begin
      start := i;
      break;
    end;
  end;

  // loop through the changelog, creating labels in scrollbox
  // as necessary to render text
  top := 8;
  for i := start to Pred(changelog.Count) do begin
    line := changelog[i];
    if IsVersionLine(line) then
      CreateVersionLabel(line, top)
    else
      CreateLabel(line, top);
  end;
end;

function ChangeLogPrompt(AOwner: TComponent): boolean;
var
  clForm: TChangeLogForm;
begin
  Result := false;
  if FileExists('changelog.txt') then begin
    if Now - GetLastModified('changelog.txt') > 1.0 then
      UpdateChangeLog;
  end
  else
     UpdateChangeLog;

  // if we don't have a changelog, exit returning false
  if not FileExists('changelog.txt') then
    exit;

  // create change log form
  clForm := TChangeLogForm.Create(AOwner);
  Result := clForm.ShowModal = mrOK;
  clForm.Free;
end;

end.
