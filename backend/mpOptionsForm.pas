unit mpOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, Buttons, ImgList, FileCtrl,
  mpBackend;

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
    gbModOrganizer: TGroupBox;
    kbUsingMO: TCheckBox;
    lblModOrganizer: TLabel;
    edMODirectory: TEdit;
    btnDetect: TButton;
    kbCopyGeneral: TCheckBox;
    btnBrowseMO: TSpeedButton;
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
    gbGameMode: TGroupBox;
    lblGameMode: TLabel;
    cbGameMode: TComboBox;
    btnUpdateGameMode: TButton;
    gbReports: TGroupBox;
    lblUsername: TLabel;
    edUsername: TEdit;
    kbSaveReports: TCheckBox;
    kbBatCopy: TCheckBox;
    AdvancedTabSheet: TTabSheet;
    gbDebug: TGroupBox;
    kbDebugRenumbering: TCheckBox;
    kbDebugMergeStatus: TCheckBox;
    kbDebugAssetCopying: TCheckBox;
    kbDebugRecordCopying: TCheckBox;
    kbDebugMasters: TCheckBox;
    kbDebugBatchCopying: TCheckBox;
    kbDebugBSAs: TCheckBox;
    kbDebugTempPath: TCheckBox;
    kbDebugLoadOrder: TCheckBox;
    kbINIs: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure kbUsingMOClick(Sender: TObject);
    procedure btnUpdateGameModeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.dfm}

procedure TOptionsForm.btnOKClick(Sender: TObject);
begin
  // save changes to settings
  settings.Save('settings.ini');
end;

procedure TOptionsForm.btnUpdateGameModeClick(Sender: TObject);
begin
  btnOKClick(nil);
  Close;
end;

procedure TOptionsForm.FormCreate(Sender: TObject);
begin
  // set up buttons
  btnBrowseMO.Flat := true;
  btnBrowseAssetDirectory.Flat := true;
  IconList.GetBitmap(0, btnBrowseMO.Glyph);
  IconList.GetBitmap(0, btnBrowseAssetDirectory.Glyph);
end;

procedure TOptionsForm.kbUsingMOClick(Sender: TObject);
var
  b: boolean;
begin
  b := kbUsingMO.Checked;
  edMODirectory.Enabled := b;
  btnDetect.Enabled := b;
  btnBrowseMO.Enabled := b;
  kbCopyGeneral.Enabled := b;
end;

end.
