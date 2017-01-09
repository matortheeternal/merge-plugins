unit mpEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // mte units
  mteHelpers, mteBase, mteLogger, RttiTranslation,
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
    procedure edKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure edFilenameEnter(Sender: TObject);
    procedure cbMethodChange(Sender: TObject);
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
    merge.dataPath := settings.mergeDirectory + merge.name + '\';
    merge.filename := Trim(edFilename.Text);
    merge.method := cbMethod.ItemIndex;
    merge.renumbering := cbRenumbering.ItemIndex;
  end;
end;

function TEditForm.NameValid: boolean;
var
  aMerge: TMerge;
  i: integer;
begin
  Result := false;

  // return false if edName is blank
  if Trim(edName.Text) = '' then
    exit;

  // return false if merge with specified name already exists
  for i := 0 to Pred(MergesList.Count) do begin
    aMerge := TMerge(MergesList[i]);
    if (aMerge.name = edName.Text) and (aMerge <> merge) then
      exit;
  end;

  // all tests passed, return true
  Result := true;
end;

function TEditForm.FilenameValid: boolean;
var
  aMerge: TMerge;
  plugin: TPlugin;
  loadOrder, highLoadOrder, i: integer;
  sFilename: string;
begin
  Result := false;

  // return false if filename doesn't end in .esp
  if not StrEndsWith(edFilename.Text, '.esp') then
    exit;

  // return false if specified filename corresponds to a
  // plugin that is in merge
  if merge.plugins.IndexOf(edFilename.Text) > -1 then
    exit;

  // check if there's a load order error merging into the specified file
  plugin := PluginByFilename(edFilename.Text);
  loadOrder := PluginLoadOrder(edFilename.Text);
  highLoadOrder := MaxInt;
  if merge.plugins.Count > 0 then begin
    sFilename := merge.plugins[merge.plugins.Count -1];
    highLoadOrder := PluginLoadOrder(sFilename);
  end;

  // return false if there's a load order error
  if Assigned(plugin) and (loadOrder > highLoadOrder) then
    exit;

  // return false if merge exists
  for i := 0 to Pred(MergesList.Count) do begin
    aMerge := TMerge(MergesList[i]);
    if (aMerge.filename = edFileName.Text) and (aMerge <> merge) then
      exit;
  end;

  // all tests passed, return true
  Result := true;
end;

procedure TEditForm.cbMethodChange(Sender: TObject);
var
  bNewRecords: boolean;
begin
  bNewRecords := cbMethod.ItemIndex = 1;
  cbRenumbering.ItemIndex := IfThenInt(bNewRecords, 1, 0);
  cbRenumbering.Enabled := not bNewRecords;
  Repaint;
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

procedure TEditForm.edFilenameEnter(Sender: TObject);
begin
  // change selection to not include the .esp
  if (edFilename.SelLength = Length(edFilename.Text))
  and StrEndsWith(edFilename.Text, '.esp') then
    edFilename.SelLength := edFilename.SelLength - 4;
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

{ Save merge by pressing enter in edName or edFilename }
procedure TEditForm.edKeyDown(Sender: TObject; var Key: Word;
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
