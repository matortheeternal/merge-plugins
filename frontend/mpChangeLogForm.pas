unit mpChangeLogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  // mp units
  mpFrontend;

type
  TChangeLogForm = class(TForm)
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
  spacing = 6;

var
  ChangeLogForm: TChangeLogForm;

implementation

{$R *.dfm}

procedure TChangeLogForm.FormCreate(Sender: TObject);
begin
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
  lbl.Caption := Trim(line);
  lbl.Font.Style := [];
  lbl.AutoSize := false;
  lbl.Width := ScrollBox.ClientWidth - 16;

  // increment top for next label
  Inc(top, lbl.Height + spacing);
end;

procedure TChangeLogForm.DisplayChangelog;
var
  i, top, start: Integer;
  bFoundCurrentVersion: boolean;
  line, CurrentVersion: string;
begin
  // find start line
  start := 0;
  bFoundCurrentVersion := false;
  CurrentVersion := 'Version ' + status.programVersion;
  for i := 0 to Pred(changelog.Count) do begin
    line := changelog[i];
    // identify start of changelog for version after current version
    if bFoundCurrentVersion and IsVersionLine(line) then begin
      start := i;
      break;
    end;
    // look for current version
    if Pos(CurrentVersion, line) > 0 then
      bFoundCurrentVersion := true;
  end;

  // loop through the changelog, creating labels in scrollbox
  // as necessary to render text
  top := 8;
  for i := start to Pred(changelog.Count) do begin
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
  if UpdateChangeLog then begin
    clForm := TChangeLogForm.Create(AOwner);
    Result := clForm.ShowModal = mrOK;
    clForm.Free;
  end;
end;

end.
