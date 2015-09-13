unit mpProfileForm;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, StdCtrls, Graphics, ImgList,
  Menus, Dialogs, ExtCtrls,
  // mte components
  RttiIni,
  // mp components
  mpFrontend, mpProfilePanel;

type
  TProfileForm = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    GameIcons: TImageList;
    ProfilePopupMenu: TPopupMenu;
    NewProfileItem: TMenuItem;
    DeleteProfileItem: TMenuItem;
    GeneralIcons: TImageList;
    ScrollBox: TScrollBox;
    procedure RealignPanels;
    function CreateNewProfile(name: string): TProfilePanel;
    procedure NewProfileItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadProfiles;
    procedure CreateDefaultProfiles;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ProfilePopupMenuPopup(Sender: TObject);
    procedure SelectionChanged(Sender: TObject);
    procedure DeleteProfileItemClick(Sender: TObject);
    function ProfileNameTaken(name: string): boolean;
  public
    ProfilePanels: TList;
  end;

var
  ProfileForm: TProfileForm;
  SelectCallback: TNotifyEvent;
  MouseOverProfile: TProfilePanel;

implementation

{$R *.dfm}

procedure TProfileForm.DeleteProfileItemClick(Sender: TObject);
begin
  if not Assigned(MouseOverProfile) then exit;
  ProfilePanels.Delete(ProfilePanels.IndexOf(MouseOverProfile));
  MouseOverProfile.Free;
  RealignPanels;
end;

procedure TProfileForm.RealignPanels;
var
  i: Integer;
begin
  for i := 0 to Pred(ProfilePanels.Count) do
    TProfilePanel(ProfilePanels[i]).SetTop(100 * i);
end;

procedure TProfileForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  p: TProfilePanel;
begin
  // set profile if user clicked OK
  if ModalResult = mrOK then begin
    for i := 0 to Pred(ProfilePanels.Count) do begin
      p := TProfilePanel(ProfilePanels[i]);
      if p.Selected then begin
        profile := p.GetProfile;
        break;
      end;
    end;
  end;

  // free memory
  ProfilePanels.Free;
end;

procedure TProfileForm.FormCreate(Sender: TObject);
begin
  ProgramPath := ExtractFilePath(ParamStr(0));
  ProfilePanels := TList.Create;
  SelectCallback := SelectionChanged;
  LoadProfiles;
end;

procedure TProfileForm.LoadProfiles;
var
  path: string;
  info: TSearchRec;
  p: TProfilePanel;
begin
  path := ProgramPath + 'profiles\';
  if not DirectoryExists(path) then begin
    CreateDefaultProfiles;
    exit;
  end;

  // if no profiles found, create default profiles and exit
  if FindFirst(path + '*\settings.ini', faAnyFile, info) <> 0 then begin
    CreateDefaultProfiles;
    exit;
  end;
  // add found profiles
  repeat
    settings := TSettings.Create;
    TRttiIni.Load(info.Name, settings);
    p := CreateNewProfile(settings.profile);
    p.SetGame(settings.gameMode);
    p.SetPath(settings.gamePath);
  until FindNext(info) <> 0;
end;

procedure TProfileForm.CreateDefaultProfiles;
var
  i: Integer;
  path, name: string;
  p: TProfilePanel;
begin
  for i := Low(GameArray) to High(GameArray) do begin
    path := GetGamePath(GameArray[i].gameName);
    name := GameArray[i].gameName + 'Profile';
    if path <> '' then begin
      p := CreateNewProfile(name);
      p.SetGame(i);
      p.SetPath(path);
    end;
  end;
end;

function TProfileForm.CreateNewProfile(name: string): TProfilePanel;
begin
  Result := TProfilePanel.ICreate(ScrollBox, GameIcons, GeneralIcons, name);
  Result.SetTop(100 * ProfilePanels.Count);
  Result.SetCallback(SelectCallback);
  ProfilePanels.Add(Result);
end;

procedure TProfileForm.NewProfileItemClick(Sender: TObject);
var
  name: string;
  i: Integer;
begin
  // find profile name
  name := 'NewProfile';
  i := 1;
  while ProfileNameTaken(name + IntToStr(i)) do
    Inc(i);

  // create a new profile
  name := name + IntToStr(i);
  CreateNewProfile(name);
end;

function TProfileForm.ProfileNameTaken(name: string): boolean;
var
  i: Integer;
  pName: string;
begin
  Result := false;
  for i := 0 to Pred(ProfilePanels.Count) do begin
    pName := TProfilePanel(ProfilePanels[i]).GetProfile.name;
    if SameText(pName, name) then begin
      Result := true;
      break;
    end;
  end;
end;

procedure TProfileForm.ProfilePopupMenuPopup(Sender: TObject);
var
  pt: TPoint;
  index: Integer;
begin
  // get profile user is moused over
  pt := ScrollBox.ScreenToClient(Mouse.CursorPos);
  Index := pt.Y div 100;
  if Index < ProfilePanels.Count then
    MouseOverProfile := TProfilePanel(ProfilePanels[index])
  else
    MouseOverProfile := nil;

  // can only delete profile if mouse over a profile
  ProfilePopupMenu.Items[1].Enabled := Assigned(MouseOverProfile);
end;

procedure TProfileForm.SelectionChanged(Sender: TObject);
var
  i: Integer;
  p: TProfilePanel;
begin
  // deselect all panels except the sender
  for i := 0 to Pred(ProfilePanels.Count) do begin
    p := TProfilePanel(ProfilePanels[i]);
    if p <> TProfilePanel(Sender) then
      p.Deselect
  end;

  // enable ok button if profile panel is selected
  btnOk.Enabled := TProfilePanel(Sender).Selected;
end;

end.
