unit mpOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ImgList, FileCtrl, ExtCtrls,
  // mte units
  mteHelpers,
  // mp units
  mpFrontend, mpGameForm;

type
  TOptionsForm = class(TForm)
    SettingsPageControl: TPageControl;
    GeneralTabSheet: TTabSheet;
    MergingTabSheet: TTabSheet;
    lblLanguage: TLabel;
    cbLanguage: TComboBox;
    gbStyle: TGroupBox;
    kbSimpleDictionary: TCheckBox;
    kbSimplePlugins: TCheckBox;
    btnCancel: TButton;
    btnOK: TButton;
    IconList: TImageList;
    gbAssetCopying: TGroupBox;
    lblMergeDestination: TLabel;
    edMergeDirectory: TEdit;
    btnBrowseAssetDirectory: TSpeedButton;
    kbFaceGen: TCheckBox;
    kbVoiceAssets: TCheckBox;
    kbTranslations: TCheckBox;
    kbFragments: TCheckBox;
    kbExtractBSAs: TCheckBox;
    kbBuildBSA: TCheckBox;
    gbUpdating: TGroupBox;
    kbUpdateDictionary: TCheckBox;
    kbUpdateProgram: TCheckBox;
    gbReports: TGroupBox;
    lblUsername: TLabel;
    edUsername: TEdit;
    kbSaveReports: TCheckBox;
    kbBatCopy: TCheckBox;
    AdvancedTabSheet: TTabSheet;
    kbINIs: TCheckBox;
    btnRegister: TButton;
    lblStatus: TLabel;
    btnReset: TButton;
    btnUpdateDictionary: TButton;
    btnUpdateProgram: TButton;
    lblDictionaryStatus: TLabel;
    lblProgramStatus: TLabel;
    gbPrivacy: TGroupBox;
    kbNoStatistics: TCheckBox;
    kbNoPersistentConnection: TCheckBox;
    gbLogging: TGroupBox;
    lblClientColor: TLabel;
    lblGeneralColor: TLabel;
    lblLoadColor: TLabel;
    lblMergeColor: TLabel;
    lblPluginColor: TLabel;
    lblErrorColor: TLabel;
    cbClientColor: TColorBox;
    cbGeneralColor: TColorBox;
    cbLoadColor: TColorBox;
    cbMergeColor: TColorBox;
    cbPluginColor: TColorBox;
    cbErrorColor: TColorBox;
    IntegrationsTabSheet: TTabSheet;
    gbModOrganizer: TGroupBox;
    lblModOrganizerPath: TLabel;
    btnBrowseMO: TSpeedButton;
    kbUsingMO: TCheckBox;
    edModOrganizerPath: TEdit;
    gbDebug: TGroupBox;
    kbDebugRenumbering: TCheckBox;
    kbDebugMergeStatus: TCheckBox;
    kbDebugAssetCopying: TCheckBox;
    kbDebugRecordCopying: TCheckBox;
    kbDebugMasters: TCheckBox;
    kbDebugBatchCopying: TCheckBox;
    kbDebugBSAs: TCheckBox;
    kbDebugScriptFragments: TCheckBox;
    kbCopyGeneralAssets: TCheckBox;
    gbPapyrus: TGroupBox;
    lblDecompilerPath: TLabel;
    btnBrowseDecompiler: TSpeedButton;
    edDecompilerPath: TEdit;
    edCompilerPath: TEdit;
    lblCompilerPath: TLabel;
    btnBrowseCompiler: TSpeedButton;
    edFlagsPath: TEdit;
    lblFlagsPath: TLabel;
    btnBrowseFlags: TSpeedButton;
    gbMergeProfile: TGroupBox;
    lblCurrentProfile: TLabel;
    cbCurrentProfile: TComboBox;
    btnChangeMergeProfile: TButton;
    kbLinkToMO: TCheckBox;
    cbModOrganizerProfile: TComboBox;
    gbBSAs: TGroupBox;
    btnBrowseBSAOpt: TSpeedButton;
    edBsaOptPath: TEdit;
    lblBSAOptPath: TLabel;
    lblBSAOptOptions: TLabel;
    edBsaOptOptions: TEdit;
    gbGameMode: TGroupBox;
    lblGameMode: TLabel;
    cbGameMode: TComboBox;
    btnUpdateGameMode: TButton;
    GroupBox1: TGroupBox;
    btnDetect: TButton;
    meTemplate: TMemo;
    lblSample: TLabel;
    lblSampleValue: TLabel;
    lblTemplate: TLabel;
    lblModOrganizerModsPath: TLabel;
    edModOrganizerModsPath: TEdit;
    btnBrowseMOMods: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnBrowseAssetDirectoryClick(Sender: TObject);
    procedure btnBrowseMOClick(Sender: TObject);
    procedure kbUsingMOClick(Sender: TObject);
    procedure btnUpdateGameModeClick(Sender: TObject);
    procedure edUsernameChange(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnUpdateDictionaryClick(Sender: TObject);
    procedure btnUpdateProgramClick(Sender: TObject);
    procedure btnChangeMergeProfileClick(Sender: TObject);
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
  BrowseForFile(edBsaOptPath, 'Executables|*.exe', ProgramPath);
  edBsaOptPathExit(nil);
end;

procedure TOptionsForm.btnBrowseCompilerClick(Sender: TObject);
begin
  BrowseForFile(edCompilerPath, 'Executables|*.exe', DataPath);
end;

procedure TOptionsForm.btnBrowseDecompilerClick(Sender: TObject);
begin
  BrowseForFile(edDecompilerPath, 'Executables|*.exe', ProgramPath);
end;

procedure TOptionsForm.btnBrowseFlagsClick(Sender: TObject);
begin
  BrowseForFile(edFlagsPath, 'Flags files|*.flg', DataPath + 'scripts\source\');
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
  // search for installations in ?:\Program Files and ?:\Program Files (x86)
  for i := 65 to 90 do begin
    if DirectoryExists(chr(i) + ':\Program Files') then
      paths := paths + chr(i) + ':\Program Files;';
    if DirectoryExists(chr(i) + ':\Program Files (x86)') then
      paths := paths + chr(i) + ':\Program Files (x86);';
  end;

  modOrganizerPath := FileSearch('Mod Organizer\ModOrganizer.exe', paths);

  // search for installations in GamePath
  if (modOrganizerPath = '') then
    modOrganizerPath := RecursiveFileSearch(GamePath, validModOrganizerFilenames, ignore, 2);

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
    MessageDlg('Couldn''t automatically detect Mod Organizer''s file path.  '+
      'Please enter it manually.', mtConfirmation, [mbOk], 0);
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
  settings.saveReportsLocally := kbSaveReports.Checked;
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

  // Advanced > Game Mode
  settings.defaultGame := GetGameID(cbGameMode.Text);
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
    lblStatus.Caption := 'Server unavailable';
    lblStatus.Font.Color := clRed;
    lblStatus.Hint := 'Sorry, the server is unavailable, try again later.';
    btnRegister.Enabled := false;
    exit;
  end;

  if (btnRegister.Caption = 'Register') then begin
    if RegisterUser(edUsername.Text) then begin
      settings.registered := true;
      settings.username := edUsername.Text;
      SaveSettings;
      lblStatus.Caption := 'Registered';
      lblStatus.Font.Color := clGreen;
      lblStatus.Hint := '';
      edUsername.Enabled := false;
      btnRegister.Enabled := false;
    end
    else begin
      lblStatus.Caption := 'Failed to register';
      lblStatus.Font.Color := clRed;
      lblStatus.Hint := 'Oops.  Something went wrong.  o_o';
    end;
  end
  else begin
    if UsernameAvailable(edUsername.Text) then begin
      lblStatus.Caption := 'Username available!';
      lblStatus.Font.Color := clBlue;
      lblStatus.Hint := 'Click register to claim it.';
      btnRegister.Caption := 'Register';
    end
    else begin
      lblStatus.Caption := 'Username unavailable, sorry';
      lblStatus.Font.Color := clRed;
      lblStatus.Hint := 'Someone beat ya to it, try another.';
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
      lblStatus.Caption := 'Registered';
      lblStatus.Font.Color := clGreen;
      lblStatus.Hint := '';
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
      lblDictionaryStatus.Caption := 'Up to date';
      lblDictionaryStatus.Font.Color := clGreen;
    end;
  end;
end;

procedure TOptionsForm.btnUpdateProgramClick(Sender: TObject);
begin
  if TCPClient.Connected then begin
    if DownloadProgram then begin
      bInstallUpdate := true;
      btnOKClick(nil);
      Close;
    end;
  end;
end;

procedure TOptionsForm.btnUpdateGameModeClick(Sender: TObject);
begin
  bChangeGameMode := true;
  btnOKClick(nil);
  Close;
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
    lblStatus.Caption := 'Server unavailable';
    lblStatus.Font.Color := clRed;
    lblStatus.Hint := 'Sorry, the server is unavailable, try again later.';
    btnRegister.Enabled := false;
    exit;
  end;

  btnRegister.Caption := 'Check';
  if Length(edUsername.Text) < 4 then begin
    lblStatus.Caption := 'Invalid username';
    lblStatus.Font.Color := clRed;
    lblStatus.Hint := 'Username length must be at least 4 characters.';
    btnRegister.Enabled := false;
  end
  else if Length(edUsername.Text) > 24 then begin
    lblStatus.Caption := 'Invalid username';
    lblStatus.Font.Color := clRed;
    lblStatus.Hint := 'Username length cannot exceed 24 characters.';
    btnRegister.Enabled := false;
  end
  else begin
    lblStatus.Caption := 'Valid, is it available?';
    lblStatus.Font.Color := clBlack;
    lblStatus.Hint := 'Click the check button to check if '#13+
      'the username is available';
    btnRegister.Enabled := true;
  end;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
var
  index: integer;
  defaultGame: TGameMode;
begin
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
  cbLanguage.Text := settings.language;
  // General > reports
  edUsername.Text := settings.username;
  kbSaveReports.Checked := settings.saveReportsLocally;
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

  // load valid game paths
  if GamePathValid(settings.tes5path, 1) then
    cbGameMode.Items.Add(GameArray[1].longName);
  if GamePathValid(settings.fnvpath, 2) then
    cbGameMode.Items.Add(GameArray[2].longName);
  if GamePathValid(settings.tes4path, 3) then
    cbGameMode.Items.Add(GameArray[3].longName);
  if GamePathValid(settings.fo3path, 4) then
    cbGameMode.Items.Add(GameArray[4].longName);
  defaultGame := GameArray[settings.defaultGame];
  index := cbGameMode.Items.IndexOf(defaultGame.longName);
  if index > -1 then
    cbGameMode.ItemIndex := index;

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
        lblStatus.Caption := 'Authorization failed!';
        lblStatus.Font.Color := clRed;
        lblStatus.Hint := 'Click reset to reset your authentication key.'#13+
          'NOTE: This will fail if your IP has changed since you last logged in.'#13+
          'If you can''t recover your username you can either make a new one or '#13+
          'contact support.';
      end
      else begin
        lblStatus.Caption := 'Registered.';
        lblStatus.Font.Color := clGreen;
        lblStatus.Hint := '';
        bAuthorized := true;
      end;
    end;
  end;

  // dictionary update
  if bDictionaryUpdate then begin
    btnUpdateDictionary.Enabled := bDictionaryUpdate;
    lblDictionaryStatus.Caption := 'Update available!';
    lblDictionaryStatus.Font.Color := $000080FF;
  end;

  // program update
  if bProgramUpdate then begin
    btnUpdateProgram.Enabled := bProgramUpdate;
    lblProgramStatus.Caption := 'Update available!';
    lblProgramStatus.Hint := 'Current Version: '+status.programVersion+
      #13#10'New Version: '+RemoteStatus.programVersion;
    lblProgramStatus.Font.Color := $000080FF;
  end;

  // set up browse buttons
  btnBrowseMO.Flat := true;
  btnBrowseAssetDirectory.Flat := true;
  btnBrowseDecompiler.Flat := true;
  btnBrowseCompiler.Flat := true;
  btnBrowseFlags.Flat := true;
  btnBrowseBsaOpt.Flat := true;
  IconList.GetBitmap(0, btnBrowseMO.Glyph);
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
