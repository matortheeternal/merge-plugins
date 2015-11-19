unit mtePluginSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CommCtrl, Menus, ComCtrls, ImgList;

type
  TStringFunction = function(s: string): string of object;
  TPluginSelectionForm = class(TForm)
    lvPlugins: TListView;
    btnCancel: TButton;
    btnOK: TButton;
    PluginsPopupMenu: TPopupMenu;
    SelectAllItem: TMenuItem;
    SelectNoneItem: TMenuItem;
    InvertSelectionItem: TMenuItem;
    StateImages: TImageList;
    procedure LoadSubItems(aListItem: TListItem; sPlugin: string);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure SelectAllItemClick(Sender: TObject);
    procedure SelectNoneItemClick(Sender: TObject);
    procedure InvertSelectionItemClick(Sender: TObject);
    procedure lvPluginsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lvPluginsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
    PluginInfoGetter: TStringFunction;
    sColumns: string;
    slAllPlugins, slCheckedPlugins: TStringList;
  end;

var
  PluginSelectionForm: TPluginSelectionForm;

implementation

const
  cChecked = 1;
  cUnChecked = 2;

{$R *.dfm}

procedure TPluginSelectionForm.btnOKClick(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
begin
  // clear checked plugins list
  slCheckedPlugins.Clear;
  // add checked plugins to slCheckedPlugins
  for i := 0 to Pred(lvPlugins.Items.Count) do begin
    ListItem := lvPlugins.Items[i];
    if ListItem.StateIndex = cChecked then
      slCheckedPlugins.Add(ListItem.Caption);
  end;
end;

procedure TPluginSelectionForm.LoadSubItems(aListItem: TListItem;
  sPlugin: string);
var
  sl: TStringList;
  i: Integer;
begin
  // get comma separated plugin info in a TStringList
  sl := TStringList.Create;
  try
    sl.CommaText := PluginInfoGetter(sPlugin);
    for i := 0 to Pred(sl.Count) do
      aListItem.SubItems.Add(sl[i]);
  finally
    sl.Free;
  end;
end;

procedure ToggleState(ListItem: TListItem);
begin
  case ListItem.StateIndex of
    cChecked: ListItem.StateIndex := cUnChecked;
    cUnChecked: ListItem.StateIndex := cChecked;
  end;
end;

procedure TPluginSelectionForm.lvPluginsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  i: Integer;
  ListItem: TListItem;
begin
  if Key = VK_SPACE then begin
    for i := 0 to Pred(lvPlugins.Items.Count) do begin
      ListItem := lvPlugins.Items[i];
      if ListItem.Selected then
        ToggleState(ListItem);
    end;
  end;
end;

procedure TPluginSelectionForm.lvPluginsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HT: THitTests;
  ListItem: TListItem;
begin
  // toggle
  ListItem := lvPlugins.GetItemAt(X, Y);
  HT := lvPlugins.GetHitTestInfoAt(X, Y);
  if (HT - [htOnStateIcon] <> HT) then
    ToggleState(ListItem);
end;

procedure TPluginSelectionForm.FormShow(Sender: TObject);
var
  i, iColumnSize: Integer;
  aListItem: TListItem;
  sPlugin: string;
  sl: TStringList;
  aColumn: TListColumn;
begin
  // create columns
  sl := TStringList.Create;
  try
    sl.CommaText := sColumns;
    iColumnSize := lvPlugins.ClientWidth div sl.Count;
    for i := 0 to Pred(sl.Count) do begin
      aColumn := lvPlugins.Columns.Add;
      aColumn.Caption := sl[i];
      aColumn.Width := iColumnSize;
      if i = 0 then
        aColumn.Tag := 300;
    end;
  finally
    sl.Free;
  end;

  // add plugin items to list
  for i := 0 to Pred(slAllPlugins.Count) do begin
    sPlugin := slAllPlugins[i];
    aListItem := lvPlugins.Items.Add;
    aListItem.StateIndex := cUnChecked;
    aListItem.Caption := sPlugin;
    // check ListItem if it's in the CheckedPlugins list
    if slCheckedPlugins.IndexOf(sPlugin) > -1 then
      aListItem.Checked := true;
    // add merge subitems
    LoadSubItems(aListItem, sPlugin);
  end;
end;

procedure TPluginSelectionForm.SelectAllItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(lvPlugins.Items.Count) do
    lvPlugins.Items[i].StateIndex := cChecked;
end;

procedure TPluginSelectionForm.SelectNoneItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(lvPlugins.Items.Count) do
    lvPlugins.Items[i].StateIndex := cUnChecked;
end;

procedure TPluginSelectionForm.InvertSelectionItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Pred(lvPlugins.Items.Count) do
    ToggleState(lvPlugins.Items[i]);
end;

end.
