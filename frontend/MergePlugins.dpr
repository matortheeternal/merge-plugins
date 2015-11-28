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
  Classes,
  RttiIni,
  mpProfileForm in 'mpProfileForm.pas' {ProfileForm},
  mpProfilePanel in 'mpProfilePanel.pas',
  mpMergeForm in 'mpMergeForm.pas' {MergeForm},
  mpMerge in 'mpMerge.pas',
  mpThreads in 'mpThreads.pas',
  mpDictionaryForm in 'mpDictionaryForm.pas' {DictionaryForm},
  mpOptionsForm in 'mpOptionsForm.pas' {OptionsForm},
  mpSplashForm in 'mpSplashForm.pas' {SplashForm},
  mpEditForm in 'mpEditForm.pas' {EditForm},
  mpReportForm in 'mpReportForm.pas' {ReportForm},
  mpResolveForm in 'mpResolveForm.pas' {ResolveForm},
  mpCore in 'mpCore.pas',
  mpClient in 'mpClient.pas',
  mpConfiguration in 'mpConfiguration.pas',
  mpLoader in 'mpLoader.pas';

{$R *.res}
{$MAXSTACKSIZE 2097152}

const
  IMAGE_FILE_LARGE_ADDRESS_AWARE = $0020;


{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

var
  bProfileProvided: boolean;
  sParam, sProfile, sPath: string;
  aSettings: TSettings;
  i: Integer;
begin
  // set important vars
  SysUtils.FormatSettings.DecimalSeparator := '.';
  Application.HintHidePause := 8000;
  PathList.Values['ProgramPath'] := ExtractFilePath(ParamStr(0));

  // get current profile if profile switch provided
  for i := 1 to ParamCount do begin
    sParam := ParamStr(i);
    if sParam = '-profile' then
      sProfile := ParamStr(i + 1);
    if sParam = '-offline' then
      ProgramStatus.bOfflineMode := true;
  end;
  bProfileProvided := sProfile <> '';
  sPath := Format('%sprofiles\%s\settings.ini',
    [PathList.Values['ProgramPath'], sProfile]);
  if bProfileProvided and FileExists(sPath) then begin
    aSettings := TSettings.Create;
    TRttiIni.Load(sPath, aSettings);
    CurrentProfile := TProfile.Create(aSettings.profile);
    CurrentProfile.gameMode := aSettings.gameMode;
    CurrentProfile.gamePath := aSettings.gamePath;
    aSettings.Free;
  end;

  // initialize application, load settings
  Application.Initialize;
  ForceDirectories(PathList.Values['ProgramPath'] + 'profiles');
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
  // handle auto-close
  if not ProgramStatus.bClose then
    Application.Run;
end.
