unit mpProgressForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  W7Taskbar,
  mpBase, mpTracker;

type
  TProgressForm = class(TForm)
    ProgressBar: TProgressBar;
    ProgressLabel: TLabel;
    DetailsButton: TButton;
    LogMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure DetailsButtonClick(Sender: TObject);
    procedure ProgressMessage(const s: string);
    procedure UpdateProgress(const i: integer);
    procedure SetProgress(const i: integer);
    procedure ProcessMessages;
    procedure SaveLog;
    procedure SetTitle(title: string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
  RepaintDelay = 50;

var
  ProgressForm: TProgressForm;
  startTime: integer;

implementation

{$R *.dfm}

procedure TProgressForm.ProgressMessage(const s: string);
begin
  if Pos(' ', s) <> 1 then
    ProgressLabel.Caption := s;
  LogMemo.Lines.Add(s);
  ProcessMessages;
end;

procedure TProgressForm.UpdateProgress(const i: integer);
begin
  ProgressBar.StepBy(i);
  SetTaskbarProgressValue(ProgressBar.Position, ProgressBar.Max);
  ProcessMessages;
end;

procedure TProgressForm.SetProgress(const i: integer);
begin
  ProgressBar.Position := i;
  SetTaskbarProgressValue(ProgressBar.Position, ProgressBar.Max);
  ProcessMessages;
end;

procedure TProgressForm.ProcessMessages;
begin
  if GetTickCount - startTime > RepaintDelay then begin
    Application.ProcessMessages;
    startTime := GetTickCount;
  end;
end;

procedure TProgressForm.DetailsButtonClick(Sender: TObject);
begin
  if LogMemo.Visible then
    exit;
  DetailsButton.Visible := false;
  LogMemo.Visible := true;
  Height := 435;
  Top := Top - 153;
end;

procedure TProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SetTaskbarProgressState(tbpsNone);
end;

procedure TProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (fsModal in FormState);
  if not bProgressCancel then begin
    Tracker.Write('Cancelling...');
    bProgressCancel := true;
    SetTaskbarProgressState(tbpsError);
  end;
end;

procedure TProgressForm.SaveLog;
var
  fdt: string;
begin
  ForceDirectories(LogPath);
  fdt := FormatDateTime('mmddyy_hhnnss', TDateTime(Now));
  LogMemo.Lines.SaveToFile(LogPath + 'log_'+fdt+'.txt');
end;

procedure TProgressForm.SetTitle(title: string);
begin
  Caption := title;
end;

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  SetTaskbarProgressState(tbpsNormal);
  bProgressCancel := false;
  Height := 129;
  Tracker.OnLogEvent := ProgressMessage;
  Tracker.OnProgressEvent := UpdateProgress;
  startTime := 0;
end;

end.
