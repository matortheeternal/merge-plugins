{*******************************************************************************

     The contents of this file are subject to the Mozilla Public License
     Version 1.1 (the "License"); you may not use this file except in
     compliance with the License. You may obtain a copy of the License at
     http://www.mozilla.org/MPL/

     Software distributed under the License is distributed on an "AS IS"
     basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
     License for the specific language governing rights and limitations
     under the License.

*******************************************************************************}

program MergePlugins;

uses
  Forms,
  Dialogs,
  Controls,
  SysUtils,
  mpProfileForm in 'mpProfileForm.pas' {ProfileForm},
  mpProfilePanel in 'mpProfilePanel.pas',
  mpMergeForm in 'mpMergeForm.pas' {MergeForm},
  mpFrontend in 'mpFrontend.pas',
  mpMerge in 'mpMerge.pas',
  mpThreads in 'mpThreads.pas',
  mpDictionaryForm in 'mpDictionaryForm.pas' {DictionaryForm},
  mpOptionsForm in 'mpOptionsForm.pas' {OptionsForm},
  mpSplashForm in 'mpSplashForm.pas' {SplashForm},
  mpEditForm in 'mpEditForm.pas' {EditForm},
  mpReportForm in 'mpReportForm.pas' {Form1},
  mpChangeLogForm in 'mpChangeLogForm.pas' {ChangeLogForm};

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;


{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  bProfileProvided: boolean;
  ProgramPath: string;
begin
  // set important vars
  SysUtils.FormatSettings.DecimalSeparator := '.';
  Application.HintHidePause := 8000;
  ProgramPath := ExtractFilePath(ParamStr(0));

  // get command line arguments
  bProfileProvided := FindCmdLineSwitch('profile');

  // initialize application, load settings
  Application.Initialize;
  ForceDirectories(ProgramPath + 'profiles');
  LoadSettings;
  LoadStatistics;

  // have user select game mode
  if not bProfileProvided then begin
    ProfileForm := TProfileForm.Create(nil);
    if not (ProfileForm.ShowModal = mrOk) then
      exit;
    ProfileForm.Free;
  end;

  // run main application
  Inc(sessionStatistics.timesRun);
  Application.Title := 'Merge Plugins';
  Application.CreateForm(TMergeForm, MergeForm);
  Application.CreateForm(TDictionaryForm, DictionaryForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TProfileForm, ProfileForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TSplashForm, SplashForm);
  Application.CreateForm(TReportForm, ReportForm);
  Application.CreateForm(TChangeLogForm, ChangeLogForm);
  Application.Run;
end.
