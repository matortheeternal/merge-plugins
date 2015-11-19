unit mpEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // mte units
  mteBase, mteLogger, RttiTranslation,
  // mp units
  mpConfiguration, mpCore;

type
  TEditForm = class(TForm)
    [FormPrefix('mpEdit')]
      lblName: TLabel;
      edName: TEdit;
      lblFilename: TLabel;
      edFilename: TEdit;
      lblMethod: TLabel;
      cbMethod: TComboBox;
      lblRenumbering: TLabel;
      cbRenumbering: TComboBox;
      btnOk: TButton;
      btnCancel: TButton;
      PageControl: TPageControl;
      TabSheet: TTabSheet;

    function NameValid: boolean;
    function FilenameValid: boolean;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure edFilenameChange(Sender: TObject);
    procedure edNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edFilenameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    merge: TMerge;
  end;

var
  EditForm: TEditForm;

implementation

{$R *.dfm}

procedure TEditForm.btnOkClick(Sender: TObject);
begin
  if Assigned(merge) then begin
    merge.name := Trim(edName.Text);
    merge.filename := Trim(edFilename.Text);
    merge.method := cbMethod.Text;
    merge.renumbering := cbRenumbering.Text;
  end;
end;

function TEditForm.NameValid: boolean;
var
  mergeExists: boolean;
  i: integer;
begin
  // check if merge exists
  mergeExists := false;
  for i := 0 to Pred(MergesList.Count) do
    if (TMerge(MergesList[i]).name = edName.Text)
    and (TMerge(MergesList[i]) <> merge) then begin
      mergeExists := true;
      break;
    end;

  // invalid if filename is blank or mergeExists
  Result := not ((edName.Text = '') or mergeExists);
end;

function TEditForm.FilenameValid: boolean;
var
  loadOrderError, mergeExists: boolean;
  plugin: TPlugin;
  loadOrder, highLoadOrder, i: integer;
  sFilename: string;
begin
  // check if there's a load order error merging into the specified file
  plugin := PluginByFilename(edFilename.Text);
  loadOrder := PluginLoadOrder(edFilename.Text);
  highLoadOrder := MaxInt;
  if merge.plugins.Count > 0 then begin
    sFilename := merge.plugins[merge.plugins.Count -1];
    highLoadOrder := PluginLoadOrder(sFilename);
  end;
  loadOrderError := Assigned(plugin) and (loadorder > -1) and (loadOrder < highLoadOrder);

  // check if merge exists
  mergeExists := false;
  for i := 0 to Pred(MergesList.Count) do
    if (TMerge(MergesList[i]).filename = edFileName.Text)
    and (TMerge(MergesList[i]) <> merge) then begin
      mergeExists := true;
      break;
    end;

  // invalid if load order error or filename is blank or mergeExists
  Result := not (loadOrderError or (edFilename.Text = '') or mergeExists);
end;

procedure TEditForm.edFilenameChange(Sender: TObject);
var
  valid: boolean;
begin
  // if invalid disable btnOk, show hint, and make font color red
  valid := FilenameValid;
  btnOk.Enabled := valid and NameValid;
  edFilename.ShowHint := valid;
  if valid then
    edFilename.Font.Color := clWindowText
  else
    edFilename.Font.Color := $0000ff;
end;

procedure TEditForm.edNameChange(Sender: TObject);
var
  valid, exists: boolean;
begin
  valid := NameValid;
  exists := DirectoryExists(settings.mergeDirectory + edName.Text)
    and (edName.Text <> merge.name);

  // if invalid show hint and make font color red
  btnOk.Enabled := valid and FilenameValid;
  edName.ShowHint := (not valid) or exists;
  if (not valid) or exists then
    edName.Font.Color := $0000ff
  else
    edName.Font.Color := clWindowText;
end;

{ Save merge by pressing enter in edName }
procedure TEditForm.edNameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (HiWord(GetKeyState(vk_Return)) <> 0) and btnOk.Enabled then begin
    btnOkClick(nil);
    ModalResult := mrOk;
  end;
end;

{ Save merge by pressing enter in edFilename }
procedure TEditForm.edFilenameKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (HiWord(GetKeyState(vk_Return)) <> 0) and btnOk.Enabled then begin
    btnOkClick(nil);
    ModalResult := mrOk;
  end;
end;

procedure TEditForm.FormCreate(Sender: TObject);
begin
  // do a translation dump?
  if bTranslationDump then
    TRttiTranslation.Save('lang\english.lang', self);

  // load translation
  TRttiTranslation.Load(language, self);
end;

procedure TEditForm.FormShow(Sender: TObject);
begin
  if Assigned(merge) then begin
    edName.Text := merge.name;
    edFilename.Text := merge.filename;
    cbMethod.ItemIndex := cbMethod.Items.IndexOf(merge.method);
    cbRenumbering.ItemIndex := cbRenumbering.Items.IndexOf(merge.renumbering);
  end;
end;

end.
