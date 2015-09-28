unit mpOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ImgList, FileCtrl, ExtCtrls,
  // mte units
  mteHelpers, RttiTranslation,
  // mp units
  mpChangeLogForm, mpFrontend;

type
  TOptionsForm = class(TForm)
    [FormPrefix('mpOpt')]
      SettingsPageControl: TPageControl;
      btnCancel: TButton;
      btnOK: TButton;
      IconList: TImageList;
      [FormSection('General Tab')]
        GeneralTabSheet: TTabSheet;
        gbLanguage: TGroupBox;
        lblLanguage: TLabel;
        gbStyle: TGroupBox;
        kbSimpleDictionary: TCheckBox;
        kbSimplePlugins: TCheckBox;
        gbReports: TGroupBox;
        lblUsername: TLabel;
        edUsername: TEdit;
        btnRegister: TButton;
        lblStatus: TLabel;
        lblStatusValue: TLabel;
        btnReset: TButton;
        gbUpdating: TGroupBox;
        kbUpdateDictionary: TCheckBox;
        lblDictionaryStatus: TLabel;
        btnUpdateDictionary: TButton;
        kbUpdateProgram: TCheckBox;
        lblProgramStatus: TLabel;
        btnUpdateProgram: TButton;
        [FormSection('DontTranslate')]
          cbLanguage: TComboBox;
      [FormSection('Merging Tab')]
        MergingTabSheet: TTabSheet;
        gbAssetHandling: TGroupBox;
        lblMergeDestination: TLabel;
        edMergeDirectory: TEdit;
        btnBrowseAssetDirectory: TSpeedButton;
        kbFaceGen: TCheckBox;
        kbVoiceAssets: TCheckBox;
        kbTranslations: TCheckBox;
        kbINIs: TCheckBox;
        kbFragments: TCheckBox;
        kbExtractBSAs: TCheckBox;
        kbBuildBSA: TCheckBox;
        kbBatCopy: TCheckBox;
        gbDebug: TGroupBox;
        kbDebugRenumbering: TCheckBox;
        kbDebugMergeStatus: TCheckBox;
        kbDebugAssetCopying: TCheckBox;
        kbDebugRecordCopying: TCheckBox;
        kbDebugMasters: TCheckBox;
        kbDebugBatchCopying: TCheckBox;
        kbDebugBSAs: TCheckBox;
        kbDebugScriptFragments: TCheckBox;
      [FormSection('Advanced Tab')]
        AdvancedTabSheet: TTabSheet;
        gbMergeProfile: TGroupBox;
        lblCurrentProfile: TLabel;
        btnChangeMergeProfile: TButton;
        gbPrivacy: TGroupBox;
        kbNoStatistics: TCheckBox;
        kbNoPersistentConnection: TCheckBox;
        gbLogging: TGroupBox;
        lblClientColor: TLabel;
        cbClientColor: TColorBox;
        lblGeneralColor: TLabel;
        cbGeneralColor: TColorBox;
        lblLoadColor: TLabel;
        cbLoadColor: TColorBox;
        lblMergeColor: TLabel;
        cbMergeColor: TColorBox;
        lblPluginColor: TLabel;
        cbPluginColor: TColorBox;
        lblErrorColor: TLabel;
        cbErrorColor: TColorBox;
        lblTemplate: TLabel;
        meTemplate: TMemo;
        lblSample: TLabel;
        [FormSection('DontTranslate')]
          lblCurrentProfileName: TLabel;
          lblSampleValue: TLabel;
      [FormSection('Integrations Tab')]
        IntegrationsTabSheet: TTabSheet;
        btnDetect: TButton;
        gbModOrganizer: TGroupBox;
        kbUsingMO: TCheckBox;
        kbCopyGeneralAssets: TCheckBox;
        lblModOrganizerPath: TLabel;
        edModOrganizerPath: TEdit;
        lblModOrganizerModsPath: TLabel;
        edModOrganizerModsPath: TEdit;
        gbPapyrus: TGroupBox;
        lblDecompilerPath: TLabel;
        edDecompilerPath: TEdit;
        lblCompilerPath: TLabel;
        edCompilerPath: TEdit;
        lblFlagsPath: TLabel;
        edFlagsPath: TEdit;
        gbBSAs: TGroupBox;
        lblBSAOptPath: TLabel;
        edBsaOptPath: TEdit;
        lblBSAOptOptions: TLabel;
        edBsaOptOptions: TEdit;
        [FormSection('DontTranslate')]
          btnBrowseMO: TSpeedButton;
          btnBrowseMOMods: TSpeedButton;
          btnBrowseDecompiler: TSpeedButton;
          btnBrowseCompiler: TSpeedButton;
          btnBrowseFlags: TSpeedButton;
          btnBrowseBSAOpt: TSpeedButton;

    procedure FormCreate(Sender: TObject);
    procedure LoadLanguageOptions;
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowseAssetDirectoryClick(Sender: TObject);
    procedure btnBrowseMOClick(Sender: TObject);
    procedure kbUsingMOClick(Sender: TObject);
    procedure btnChangeMergeProfileClick(Sender: TObject);
    procedure edUsernameChange(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUpdateDictionaryClick(Sender: TObject);
    procedure btnUpdateProgramClick(Sender: TObject);
    procedure btnBrowseDecompilerClick(Sender: TObject);
    procedure btnBrowseCompilerClick(Sender: TObject);
    procedure btnBrowseFlagsClick(Sender: TObject);
    procedure btnBrowseBSAOptClick(Sender: TObject);
    procedure searchForModOrganizer;
    procedure btnDetectClick(Sender: TObject);
    procedure edBsaOptPathExit(Sender: TObject);
    procedure kbExtractBSAsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure kbBuildBSAMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure meTemplateChange(Sender: TObject);
    procedure btnBrowseMOModsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;
  slSampleLogMessage: TStringList;

implementation

{$R *.dfm}

procedure TOptionsForm.btnBrowseAssetDirectoryClick(Sender: TObject);
begin
  BrowseForFolder(edMergeDirectory, ProgramPath);
end;

procedure TOptionsForm.btnBrowseBSAOptClick(Sender: TObject);
begin
  BrowseForFile(edBsaOptPath, language.Values['mpOpt_ExeFilter'], ProgramPath);
  edBsaOptPathExit(nil);
end;

procedure TOptionsForm.btnBrowseCompilerClick(Sender: TObject);
begin
  BrowseForFile(edCompilerPath, language.Values['mpOpt_ExeFilter'], DataPath);
end;

procedure TOptionsForm.btnBrowseDecompilerClick(Sender: TObject);
begin
  BrowseForFile(edDecompilerPath, language.Values['mpOpt_ExeFilter'], ProgramPath);
end;

procedure TOptionsForm.btnBrowseFlagsClick(Sender: TObject);
begin
  BrowseForFile(edFlagsPath, language.Values['mpOpt_FlagsFilter'], DataPath + 'scripts\source\');
end;

procedure TOptionsForm.btnBrowseMOClick(Sender: TObject);
begin
  BrowseForFolder(edModOrganizerPath, ProgramPath);
  if DirectoryExists(edModOrganizerPath.Text + 'mods\') then begin
    edModOrganizerModsPath.Text := edModOrganizerPath.Text + 'mods\';
    edMergeDirectory.Text := edModOrganizerPath.Text + 'mods\';
  end;
end;

procedure TOptionsForm.btnBrowseMOModsClick(Sender: TObject);
begin
  BrowseForFolder(edModOrganizerModsPath, ProgramPath);
end;

procedure TOptionsForm.btnDetectClick(Sender: TObject);
const
  validDecompilerFilenames: array[1..1] of string = ('Champollion.exe');
  validCompilerFilenames: array[1..1] of string = ('PapyrusCompiler.exe');
  validFlagFilenames: array[1..1] of string = ('TESV_Papyrus_Flags.flg');
  validBsaOptFilenames: array[1..5] of string =
    ('BSAopt x32.exe',
    'BSAopt x64.exe',
    'BSAopt x32 (XP).exe',
    'BSAopt x64 (XP).exe',
    'BSAopt.exe');
  ignore: array[1..4] of string = ('merge', 'logs', 'temp', 'data');
var
  path: string;
  paths: array[1..2] of string;
begin
  // paths array
  paths[1] := ProgramPath;
  paths[2] := GamePath;

  // search for mod organizer
  if kbUsingMo.Checked then
    searchForModOrganizer;

  // search for champollion decompiler
  path := MultFileSearch(paths, validDecompilerFilenames, ignore, 2);
  if (path <> '') then
    edDecompilerPath.Text := path;

  // search for papyrus compiler
  path := MultFileSearch(paths, validCompilerFilenames, ignore, 2);
  if (path <> '') then
    edCompilerPath.Text := path;

  // search for papyrus flags
  path := DataPath + 'scripts\source\TESV_Papyrus_Flags.flg';
  if FileExists(path) then
    edFlagsPath.Text := path;

  // search for bsaopt
  path := MultFileSearch(paths, validBsaOptFilenames, ignore, 2);
  if (path <> '') then begin
    edBsaOptPath.Text := path;
    edBsaOptPathExit(nil);
  end;
end;

procedure TOptionsForm.searchForModOrganizer;
const
  validModOrganizerFilenames: array[1..1] of string = ('ModOrganizer.exe');
  ignore: array[1..1] of string = ('data');
var
  i: integer;
  modOrganizerPath, paths: string;
  pathList: TStringList;
  rec: TSearchRec;
begin
  // search for installations in GamePath
  if (modOrganizerPath = '') then
    modOrganizerPath := RecursiveFileSearch(GamePath, validModOrganizerFilenames, ignore, 2);

  // search for installations in ?:\Program Files and ?:\Program Files (x86)
  for i := 65 to 90 do begin
    if DirectoryExists(chr(i) + ':\Program Files') then
      paths := paths + chr(i) + ':\Program Files;';
    if DirectoryExists(chr(i) + ':\Program Files (x86)') then
      paths := paths + chr(i) + ':\Program Files (x86);';
  end;

  modOrganizerPath := FileSearch('Mod Organizer\ModOrganizer.exe', paths);

  // search each folder in each valid Program Files directory for ModOrganizer.exe
  if (modOrganizerPath = '') then begin
    pathList := TStringList.Create;
    while (Pos(';', paths) > 0) do begin
      pathList.Add(Copy(paths, 1, Pos(';', paths) - 1));
      paths := Copy(paths, Pos(';', paths) + 1, Length(paths));
    end;
    for i := 0 to pathList.Count - 1 do begin
      if FindFirst(pathList[i] + '\*', faDirectory, rec) = 0 then begin
        repeat
          modOrganizerPath := FileSearch('ModOrganizer.exe', pathList[i] + '\' + rec.Name);
          if (modOrganizerPath <> '') then
            break;
        until FindNext(rec) <> 0;

        FindClose(rec);
        if (modOrganizerPath <> '') then break;
      end;
    end;
  end;

  // if found, set TEdit captions, else alert user
  if (modOrganizerPath <> '') then begin
    edModOrganizerPath.Text := Copy(modOrganizerPath, 1, length(modOrganizerPath) - 16);
    if DirectoryExists(edModOrganizerPath.Text + 'mods\') then begin
      edModOrganizerModsPath.Text := edModOrganizerPath.Text + 'mods\';
      edMergeDirectory.Text := edModOrganizerPath.Text + 'mods\';
    end;
  end
  else begin
    MessageDlg(language.Values['mpOpt_ModOrganizerNotFound'], mtConfirmation, [mbOk], 0);
    edModOrganizerPath.Text := '';
  end;
end;

procedure TOptionsForm.btnOKClick(Sender: TObject);
begin
  // check if we need to update merge status afterwards
  bUpdateMergeStatus := (settings.usingMO <> kbUsingMO.Checked)
    or (settings.MOPath <> edModOrganizerPath.Text)
    or (settings.mergeDirectory <> edMergeDirectory.Text);

  // General > Language
  settings.language := cbLanguage.Text;
  // General > Reports
  settings.username := edUsername.Text;
  // General > Style
  settings.simpleDictionaryView := kbSimpleDictionary.Checked;
  settings.simplePluginsView := kbSimplePlugins.Checked;
  // General > Updating
  settings.updateDictionary := kbUpdateDictionary.Checked;
  settings.updateProgram := kbUpdateProgram.Checked;

  // Merging > Asset Handling
  settings.mergeDirectory := edMergeDirectory.Text;
  settings.handleFaceGenData := kbFaceGen.Checked;
  settings.handleVoiceAssets := kbVoiceAssets.Checked;
  settings.handleMCMTranslations := kbTranslations.Checked;
  settings.handleINIs := kbINIs.Checked;
  settings.handleScriptFragments := kbFragments.Checked;
  settings.extractBSAs := kbExtractBSAs.Checked;
  settings.buildMergedBSA := kbBuildBSA.Checked;
  settings.batCopy := kbBatCopy.Checked;
  // Merging > Debug
  settings.debugRenumbering := kbDebugRenumbering.Checked;
  settings.debugMergeStatus := kbDebugMergeStatus.Checked;
  settings.debugAssetCopying := kbDebugAssetCopying.Checked;
  settings.debugRecordCopying := kbDebugRecordCopying.Checked;
  settings.debugMasters := kbDebugMasters.Checked;
  settings.debugBatchCopying := kbDebugBatchCopying.Checked;
  settings.debugBSAs := kbDebugBSAs.Checked;
  settings.debugScriptFragments := kbDebugScriptFragments.Checked;

  // Advanced > Logging
  settings.clientMessageColor := cbClientColor.Selected ;
  settings.generalMessageColor := cbGeneralColor.Selected;
  settings.loadMessageColor := cbLoadColor.Selected;
  settings.mergeMessageColor := cbMergeColor.Selected;
  settings.pluginMessageColor := cbPluginColor.Selected;
  settings.errorMessageColor := cbErrorColor.Selected;
  settings.logMessageTemplate := meTemplate.Lines.Text;

  // Integrations > Mod Organizer
  settings.usingMO := kbUsingMO.Checked;
  settings.MOPath := edModOrganizerPath.Text;
  settings.MOModsPath := edModOrganizerModsPath.Text;
  settings.copyGeneralAssets := kbCopyGeneralAssets.Checked;
  // Integrations > Papyrus
  settings.decompilerPath := edDecompilerPath.Text;
  settings.compilerPath := edCompilerPath.Text;
  settings.flagsPath := edFlagsPath.Text;
  // Integrations > BSAs
  settings.bsaOptPath := edBsaOptPath.Text;
  settings.bsaOptOptions := edBsaOptOptions.Text;

  SaveSettings;
end;

procedure TOptionsForm.btnRegisterClick(Sender: TObject);
begin
  if not TCPClient.Connected then begin
    lblStatusValue.Caption := language.Values['mpOpt_ServerUnavailable'];
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := language.Values['mpOpt_ServerUnavailable'];
    btnRegister.Enabled := false;
    exit;
  end;

  if (btnRegister.Caption = language.Values['mpOpt_Register']) then begin
    if RegisterUser(edUsername.Text) then begin
      settings.registered := true;
      settings.username := edUsername.Text;
      SaveSettings;
      lblStatusValue.Caption := language.Values['mpOpt_Registered'];
      lblStatusValue.Font.Color := clGreen;
      lblStatusValue.Hint := '';
      edUsername.Enabled := false;
      btnRegister.Enabled := false;
    end
    else begin
      lblStatusValue.Caption := language.Values['mpOpt_FailedToRegister'];
      lblStatusValue.Font.Color := clRed;
      lblStatusValue.Hint := language.Values['mpOpt_FailedToRegister'];
    end;
  end
  else begin
    if UsernameAvailable(edUsername.Text) then begin
      lblStatusValue.Caption := language.Values['mpOpt_UsernameAvailable'];
      lblStatusValue.Font.Color := clBlue;
      lblStatusValue.Hint := language.Values['mpOpt_UsernameAvailable'];
      btnRegister.Caption := language.Values['mpOpt_Register'];
    end
    else begin
      lblStatusValue.Caption := language.Values['mpOpt_UsernameUnavailable'];
      lblStatusValue.Font.Color := clRed;
      lblStatusValue.Hint := language.Values['mpOpt_UsernameUnavailable'];
    end;
  end;
end;

procedure TOptionsForm.btnResetClick(Sender: TObject);
begin
  if settings.registered and not bAuthorized then begin
    ResetAuth;
    CheckAuthorization;
    if bAuthorized then begin
      btnReset.Enabled := false;
      lblStatusValue.Caption := language.Values['mpOpt_Registered'];
      lblStatusValue.Font.Color := clGreen;
      lblStatusValue.Hint := '';
    end;
  end;
end;

procedure TOptionsForm.btnUpdateDictionaryClick(Sender: TObject);
begin
  if TCPClient.Connected then begin
    if UpdateDictionary then begin
      status := TmpStatus.Create;
      CompareStatuses;
      UpdatePluginData;
      btnUpdateDictionary.Enabled := false;
      lblDictionaryStatus.Caption := language.Values['mpOpt_UpToDate'];
      lblDictionaryStatus.Font.Color := clGreen;
    end;
  end;
end;

procedure TOptionsForm.btnUpdateProgramClick(Sender: TObject);
begin
  if TCPClient.Connected then begin
    if ChangeLogPrompt(self) and DownloadProgram then begin
      bInstallUpdate := true;
      btnOKClick(nil);
      Close;
    end;
  end;
end;

procedure TOptionsForm.btnChangeMergeProfileClick(Sender: TObject);
begin
  bChangeMergeProfile := true;
  btnOKClick(nil);
  Close;
end;

procedure TOptionsForm.edBsaOptPathExit(Sender: TObject);
begin
  if FileExists(edBsaOptPath.Text) and (edBsaOptOptions.Text = '') then
    edBsaOptOptions.Text := Format('-game %s -passthrough -compress 9', [GameMode.bsaOptMode]);
end;

procedure TOptionsForm.edUsernameChange(Sender: TObject);
begin
  if not TCPClient.Connected then begin
    lblStatusValue.Caption := language.Values['mpOpt_ServerUnavailable'];
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := language.Values['mpOpt_ServerUnavailable'];
    btnRegister.Enabled := false;
    exit;
  end;

  btnRegister.Caption := language.Values['mpOpt_Check'];
  if Length(edUsername.Text) < 4 then begin
    lblStatusValue.Caption := language.Values['mpOpt_InvalidUsername'];
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := language.Values['mpOpt_UsernameTooShort'];
    btnRegister.Enabled := false;
  end
  else if Length(edUsername.Text) > 24 then begin
    lblStatusValue.Caption := language.Values['mpOpt_InvalidUsername'];
    lblStatusValue.Font.Color := clRed;
    lblStatusValue.Hint := language.Values['mpOpt_UsernameTooLong'];
    btnRegister.Enabled := false;
  end
  else begin
    lblStatusValue.Caption := language.Values['mpOpt_ValidUsername'];
    lblStatusValue.Font.Color := clBlack;
    lblStatusValue.Hint := language.Values['mpOpt_ValidUsername'];
    btnRegister.Enabled := true;
  end;
end;

procedure TOptionsForm.LoadLanguageOptions;
var
  info: TSearchRec;
  sLang: string;
begin
  cbLanguage.Items.Add('English');
  cbLanguage.ItemIndex := 0;
  if not DirectoryExists('lang') then
    exit;
  if FindFirst('lang\*.lang', faAnyFile, info) <> 0 then
    exit;
  repeat
    sLang := TitleCase(ChangeFileExt(info.Name, ''));
    if sLang <> 'English' then
      cbLanguage.Items.Add(sLang);
  until FindNext(info) <> 0;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var index: Integer;
begin
  // do translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);

  // get status update if we can
  if GetStatus then
    CompareStatuses;

  // prepare sample log message
  slSampleLogMessage := TStringList.Create;
  slSampleLogMessage.Values['Time'] := '12:34:56';
  slSampleLogMessage.Values['AppTime'] := '00:01:52';
  slSampleLogMessage.Values['Group'] := 'GENERAL';
  slSampleLogMessage.Values['Label'] := 'Test';
  slSampleLogMessage.Values['Text'] := 'This is a test message.';

  // General > Language
  LoadLanguageOptions;
  index := cbLanguage.Items.IndexOf(settings.language);
  if index > -1 then
    cbLanguage.ItemIndex := index;
  // General > reports
  edUsername.Text := settings.username;
  // General > Style
  kbSimpleDictionary.Checked := settings.simpleDictionaryView;
  kbSimplePlugins.Checked := settings.simplePluginsView;
  // General > Updating
  kbUpdateDictionary.Checked := settings.updateDictionary;
  kbUpdateProgram.Checked := settings.updateProgram;

  // Merging > Asset handling
  edMergeDirectory.Text := settings.mergeDirectory;
  kbFaceGen.Checked := settings.handleFaceGenData;
  kbVoiceAssets.Checked := settings.handleVoiceAssets;
  kbTranslations.Checked := settings.handleMCMTranslations;
  kbINIs.Checked := settings.handleINIs;
  kbFragments.Checked := settings.handleScriptFragments;
  kbExtractBSAs.Checked := settings.extractBSAs;
  kbBuildBSA.Checked := settings.buildMergedBSA;
  kbBatCopy.Checked := settings.batCopy;
  // Merging > Debug
  kbDebugRenumbering.Checked := settings.debugRenumbering;
  kbDebugMergeStatus.Checked := settings.debugMergeStatus;
  kbDebugAssetCopying.Checked := settings.debugAssetCopying;
  kbDebugRecordCopying.Checked := settings.debugRecordCopying;
  kbDebugMasters.Checked := settings.debugMasters;
  kbDebugBatchCopying.Checked := settings.debugBatchCopying;
  kbDebugBSAs.Checked := settings.debugBSAs;
  kbDebugScriptFragments.Checked := settings.debugScriptFragments;

  // Advanced > Profile
  lblCurrentProfileName.Caption := settings.profile;
  // Advanced > Logging
  cbClientColor.Selected := TColor(settings.clientMessageColor);
  cbGeneralColor.Selected := TColor(settings.generalMessageColor);
  cbLoadColor.Selected := TColor(settings.loadMessageColor);
  cbMergeColor.Selected := TColor(settings.mergeMessageColor);
  cbPluginColor.Selected := TColor(settings.pluginMessageColor);
  cbErrorColor.Selected := TColor(settings.errorMessageColor);
  meTemplate.Lines.Text := settings.logMessageTemplate;

  // Integrations > Mod Organizer
  kbUsingMO.Checked := settings.usingMO;
  edModOrganizerPath.Text := settings.MOPath;
  edModOrganizerModsPath.Text := settings.MOModsPath;
  kbCopyGeneralAssets.Checked := settings.copyGeneralAssets;
  // Integrations > Papyrus
  edDecompilerPath.Text := settings.decompilerPath;
  edCompilerPath.Text := settings.compilerPath;
  edFlagsPath.Text := settings.flagsPath;
  // Integrations > BSAs
  edBsaOptPath.Text := settings.bsaOptPath;
  edBsaOptOptions.Text := settings.bsaOptOptions;

  // disable controls if not using mod organizer
  kbUsingMOClick(nil);

  // if already registered, lock registering controls
  edUsernameChange(nil);
  if settings.registered then begin
    edUsername.Enabled := false;
    btnRegister.Enabled := false;
    // if not authorized then enable reset button
    if TCPClient.Connected then begin
      if not bAuthorized then begin
        btnReset.Enabled := true;
        lblStatusValue.Caption := language.Values['mpOpt_AuthFailed'];
        lblStatusValue.Font.Color := clRed;
        lblStatusValue.Hint := language.Values['mpOpt_AuthFailed'];
      end
      else begin
        lblStatusValue.Caption := language.Values['mpOpt_Registered'];
        lblStatusValue.Font.Color := clGreen;
        lblStatusValue.Hint := '';
        bAuthorized := true;
      end;
    end;
  end;

  // dictionary update
  if bDictionaryUpdate then begin
    btnUpdateDictionary.Enabled := bDictionaryUpdate;
    lblDictionaryStatus.Caption := language.Values['mpOpt_UpdateAvailable'];
    lblDictionaryStatus.Font.Color := $000080FF;
  end;

  // program update
  if bProgramUpdate then begin
    btnUpdateProgram.Enabled := bProgramUpdate;
    lblProgramStatus.Caption := language.Values['mpOpt_UpdateAvailable'];
    lblProgramStatus.Hint := Format(language.Values['mpOpt_VersionCompare'],
      [status.programVersion, RemoteStatus.programVersion]);
    lblProgramStatus.Font.Color := $000080FF;
  end;

  // set up browse buttons
  btnBrowseMO.Flat := true;
  btnBrowseMOMods.Flat := true;
  btnBrowseAssetDirectory.Flat := true;
  btnBrowseDecompiler.Flat := true;
  btnBrowseCompiler.Flat := true;
  btnBrowseFlags.Flat := true;
  btnBrowseBsaOpt.Flat := true;
  IconList.GetBitmap(0, btnBrowseMO.Glyph);
  IconList.GetBitmap(0, btnBrowseMOMods.Glyph);
  IconList.GetBitmap(0, btnBrowseAssetDirectory.Glyph);
  IconList.GetBitmap(0, btnBrowseDecompiler.Glyph);
  IconList.GetBitmap(0, btnBrowseCompiler.Glyph);
  IconList.GetBitmap(0, btnBrowseFlags.Glyph);
  IconList.GetBitmap(0, btnBrowseBsaOpt.Glyph);
end;

procedure TOptionsForm.kbBuildBSAMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  kbExtractBSAs.Checked := false;
end;

procedure TOptionsForm.kbExtractBSAsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  kbBuildBSA.Checked := false;
end;

procedure TOptionsForm.kbUsingMOClick(Sender: TObject);
var
  b: boolean;
begin
  b := kbUsingMO.Checked;
  edModOrganizerPath.Enabled := b;
  edModOrganizerModsPath.Enabled := b;
  btnBrowseMO.Enabled := b;
  btnBrowseMOMods.Enabled := b;
  kbCopyGeneralAssets.Enabled := b;
end;

procedure TOptionsForm.meTemplateChange(Sender: TObject);
var
  template: string;
begin
  template := meTemplate.Lines.Text;
  lblSampleValue.Caption := ApplyTemplate(template, slSampleLogMessage);
end;

end.
