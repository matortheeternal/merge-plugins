unit mtePluginSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TStringFunction = function(s: string): string of object;
  TPluginSelectionForm = class(TForm)
    lvPlugins: TListView;
    btnCancel: TButton;
    btnOK: TButton;
    procedure LoadSubItems(aListItem: TListItem; sPlugin: string);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    oPluginInfoGetter: TStringFunction;
    sColumns: string;
    slAllPlugins, slCheckedPlugins: TStringList;
  end;

var
  PluginSelectionForm: TPluginSelectionForm;

implementation

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
    if ListItem.Checked then
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
    sl.CommaText := oPluginInfoGetter(sPlugin);
    for i := 0 to Pred(sl.Count) do
      aListItem.SubItems.Add(sl[i]);
  finally
    sl.Free;
  end;
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
    end;
  finally
    sl.Free;
  end;

  // add plugin items to list
  for i := 0 to Pred(slAllPlugins.Count) do begin
    sPlugin := slAllPlugins[i];
    aListItem := lvPlugins.Items.Add;
    aListItem.Caption := sPlugin;
    // check ListItem if it's in the CheckedPlugins list
    if slCheckedPlugins.IndexOf(sPlugin) > -1 then
      aListItem.Checked := true;
    // add merge subitems
    LoadSubItems(aListItem, sPlugin);
  end;
end;

end.
