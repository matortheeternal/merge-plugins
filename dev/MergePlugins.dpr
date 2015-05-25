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
  mpMergeForm in 'mpMergeForm.pas' {MergeForm},
  mpBase in 'mpBase.pas',
  mpMerge in 'mpMerge.pas',
  mpDictionaryForm in 'mpDictionaryForm.pas' {DictionaryForm},
  mpOptionsForm in 'mpOptionsForm.pas' {OptionsForm},
  mpLogger in 'mpLogger.pas',
  mpTracker in 'mpTracker.pas',
  mpProgressForm in 'mpProgressForm.pas' {ProgressForm},
  mpSplashForm in 'mpSplashForm.pas' {SplashForm},
  mpEditForm in 'mpEditForm.pas' {EditForm},
  mpGameForm in 'mpGameForm.pas' {GameForm};

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;


{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  bSelectGame: boolean;
begin
  SysUtils.FormatSettings.DecimalSeparator := '.';

  // get command line arguments
  bSelectGame := FindCmdLineSwitch('sg');

  // initialize application, load settings
  Application.Initialize;
  LoadSettings;

  // have user select game mode
  if (bSelectGame) or (settings.defaultGame = 0) then begin
    GameForm := TGameForm.Create(nil);
    if not (GameForm.ShowModal = mrOk) then
      exit;
    GameForm.Free;
  end;

  // run main application
  Application.Title := 'Merge Plugins';
  Application.CreateForm(TMergeForm, MergeForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TDictionaryForm, DictionaryForm);
  Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TGameForm, GameForm);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(TSplashForm, SplashForm);
  Application.Run;
end.
