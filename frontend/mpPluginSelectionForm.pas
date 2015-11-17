unit mpPluginSelectionForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  // mp units
  mpFrontend;

type
  TPluginSelectionForm = class(TForm)
    lvPlugins: TListView;
    btnCancel: TButton;
    btnOK: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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

procedure TPluginSelectionForm.FormShow(Sender: TObject);
var
  i: Integer;
  ListItem: TListItem;
  sPlugin, sMerge: string;
begin
  // add plugin items to list
  for i := 0 to Pred(slAllPlugins.Count) do begin
    sPlugin := slAllPlugins[i];
    ListItem := lvPlugins.Items.Add;
    ListItem.Caption := sPlugin;
    // add merge subitem
    sMerge := GetMergeForPlugin(sPlugin);
    ListItem.SubItems.Add(sMerge);
    if slCheckedPlugins.IndexOf(sPlugin) > -1 then
      ListItem.Checked := true;
  end;
end;

end.
