unit mpGameForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ImgList, pngimage, StdCtrls, Buttons, FileCtrl,
  mpBase;

type
  TGameForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    LabeledEdit1: TLabeledEdit;
    LabeledEdit2: TLabeledEdit;
    LabeledEdit3: TLabeledEdit;
    LabeledEdit4: TLabeledEdit;
    IconList: TImageList;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    btnOk: TButton;
    btnCancel: TButton;
    procedure DeselectPanels(var panel: TPanel);
    procedure TogglePanel(var panel: TPanel);
    procedure SelectGame2(Sender: TObject);
    procedure SelectGame1(Sender: TObject);
    procedure SelectGame3(Sender: TObject);
    procedure SelectGame4(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure LabeledEdit2Change(Sender: TObject);
    procedure LabeledEdit3Change(Sender: TObject);
    procedure LabeledEdit4Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GameForm: TGameForm;

implementation

{$R *.dfm}

const
  SelectedColor = $f0e3d8;
  DefaultColor = clBtnFace;

procedure TGameForm.btnOkClick(Sender: TObject);
begin
  // SAVE SETTINGS
  settings.defaultGame := 0;
  settings.selectedGame := 0;
  if CheckBox1.Checked then settings.defaultGame := 1;
  if CheckBox2.Checked then settings.defaultGame := 2;
  if CheckBox3.Checked then settings.defaultGame := 3;
  if CheckBox4.Checked then settings.defaultGame := 4;
  if Panel1.Color = SelectedColor then settings.selectedGame := 1;
  if Panel2.Color = SelectedColor then settings.selectedGame := 2;
  if Panel3.Color = SelectedColor then settings.selectedGame := 3;
  if Panel4.Color = SelectedColor then settings.selectedGame := 4;
  settings.tes5path := LabeledEdit1.Text;
  settings.fnvpath := LabeledEdit2.Text;
  settings.tes4path := LabeledEdit3.Text;
  settings.fo3path := LabeledEdit4.Text;
  SaveSettings;
end;

procedure TGameForm.FormActivate(Sender: TObject);
begin
  if btnOk.Enabled then
    GameForm.FocusControl(btnOk)
  else
    GameForm.FocusControl(btnCancel);
end;

procedure TGameForm.FormCreate(Sender: TObject);
begin
  // GUI INITIALIZATION
  IconList.GetBitmap(0, SpeedButton1.Glyph);
  IconList.GetBitmap(0, SpeedButton2.Glyph);
  IconList.GetBitmap(0, SpeedButton3.Glyph);
  IconList.GetBitmap(0, SpeedButton4.Glyph);
  SpeedButton1.Flat := true;
  SpeedButton2.Flat := true;
  SpeedButton3.Flat := true;
  SpeedButton4.Flat := true;

  // DETECT PATHS
  if settings.tes5path = '' then
    settings.tes5path := GetGamePath('Skyrim');
  if settings.fnvpath = '' then
    settings.fnvpath := GetGamePath('FalloutNV');
  if settings.tes4path = '' then
    settings.tes4path := GetGamePath('Oblivion');
  if settings.fo3path = '' then
    settings.fo3path := GetGamePath('Fallout3');

  // LOAD VALUES
  CheckBox1.Checked := settings.defaultGame = 1;
  CheckBox2.Checked := settings.defaultGame = 2;
  CheckBox3.Checked := settings.defaultGame = 3;
  CheckBox4.Checked := settings.defaultGame = 4;
  LabeledEdit1.Text := settings.tes5path;
  LabeledEdit2.Text := settings.fnvpath;
  LabeledEdit3.Text := settings.tes4path;
  LabeledEdit4.Text := settings.fo3path;

  // TEST PATHS
  TogglePanel(Panel1);
  TogglePanel(Panel2);
  TogglePanel(Panel3);
  TogglePanel(Panel4);
  SelectGame4(nil);
  SelectGame3(nil);
  SelectGame2(nil);
  SelectGame1(nil);
end;

procedure TGameForm.DeselectPanels(var panel: TPanel);
begin
  if panel <> Panel1 then begin
    Panel1.Color := DefaultColor;
    Panel1.Repaint;
  end;
  if panel <> Panel2 then begin
    Panel2.Color := DefaultColor;
    Panel2.Repaint;
  end;
  if panel <> Panel3 then begin
    Panel3.Color := DefaultColor;
    Panel3.Repaint;
  end;
  if panel <> Panel4 then begin
    Panel4.Color := DefaultColor;
    Panel4.Repaint;
  end;
end;

procedure TGameForm.TogglePanel(var panel: TPanel);
begin
  if panel.Color = SelectedColor then begin
    btnOk.Enabled := false;
    panel.Color := DefaultColor
  end
  else begin
    btnOk.Enabled := true;
    panel.Color := SelectedColor;
  end;
  panel.Repaint;
end;

procedure TGameForm.LabeledEdit1Change(Sender: TObject);
begin
  CheckBox1.Enabled := GamePathValid(LabeledEdit1.Text, 1);
  SelectGame1(nil);
  if CheckBox1.Enabled then begin
    Panel1.Cursor := crHandPoint;
    Image1.Cursor := crHandPoint;
  end
  else begin
    Panel1.Cursor := crDefault;
    Image1.Cursor := crDefault;
  end;
end;

procedure TGameForm.LabeledEdit2Change(Sender: TObject);
begin
  CheckBox2.Enabled := GamePathValid(LabeledEdit2.Text, 2);
  SelectGame2(nil);
  if CheckBox2.Enabled then begin
    Panel2.Cursor := crHandPoint;
    Image2.Cursor := crHandPoint;
  end
  else begin
    Panel2.Cursor := crDefault;
    Image2.Cursor := crDefault;
  end;
end;

procedure TGameForm.LabeledEdit3Change(Sender: TObject);
begin
  CheckBox3.Enabled := GamePathValid(LabeledEdit3.Text, 3);
  SelectGame3(nil);
  if CheckBox3.Enabled then begin
    Panel3.Cursor := crHandPoint;
    Image3.Cursor := crHandPoint;
  end
  else begin
    Panel3.Cursor := crDefault;
    Image3.Cursor := crDefault;
  end;
end;

procedure TGameForm.LabeledEdit4Change(Sender: TObject);
begin
  CheckBox4.Enabled := GamePathValid(LabeledEdit4.Text, 4);
  SelectGame4(nil);
  if CheckBox4.Enabled then begin
    Panel4.Cursor := crHandPoint;
    Image4.Cursor := crHandPoint;
  end
  else begin
    Panel4.Cursor := crDefault;
    Image4.Cursor := crDefault;
  end;
end;

procedure TGameForm.CheckBox1Click(Sender: TObject);
begin
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
end;

procedure TGameForm.CheckBox2Click(Sender: TObject);
begin
  CheckBox1.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
end;

procedure TGameForm.CheckBox3Click(Sender: TObject);
begin
  CheckBox2.Checked := false;
  CheckBox1.Checked := false;
  CheckBox4.Checked := false;
end;

procedure TGameForm.CheckBox4Click(Sender: TObject);
begin
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox1.Checked := false;
end;

procedure TGameForm.SelectGame1(Sender: TObject);
begin
  if GamePathValid(LabeledEdit1.Text, 1) then begin
    DeselectPanels(Panel1);
    TogglePanel(Panel1);
  end;
end;

procedure TGameForm.SelectGame2(Sender: TObject);
begin
  if GamePathValid(LabeledEdit2.Text, 2) then begin
    DeselectPanels(Panel2);
    TogglePanel(Panel2);
  end;
end;

procedure TGameForm.SelectGame3(Sender: TObject);
begin
  if GamePathValid(LabeledEdit3.Text, 3) then begin
    DeselectPanels(Panel3);
    TogglePanel(Panel3);
  end;
end;

procedure TGameForm.SelectGame4(Sender: TObject);
begin
  if GamePathValid(LabeledEdit4.Text, 4) then begin
    DeselectPanels(Panel4);
    TogglePanel(Panel4);
  end;
end;

procedure BrowseForDirectory(ed: TLabeledEdit);
var
  s: string;
begin
  // start in current directory value if valid
  if DirectoryExists(ed.Text) then
    s := ed.Text;
  // prompt user to select a directory
  SelectDirectory('Select a directory', '', s, []);

  // save text to TEdit
  if s <> '' then
    ed.Text := AppendIfMissing(s, '\');
end;

procedure TGameForm.SpeedButton1Click(Sender: TObject);
begin
  BrowseForDirectory(LabeledEdit1);
end;

procedure TGameForm.SpeedButton2Click(Sender: TObject);
begin
  BrowseForDirectory(LabeledEdit2);
end;

procedure TGameForm.SpeedButton3Click(Sender: TObject);
begin
  BrowseForDirectory(LabeledEdit3);
end;

procedure TGameForm.SpeedButton4Click(Sender: TObject);
begin
  BrowseForDirectory(LabeledEdit4);
end;

end.
