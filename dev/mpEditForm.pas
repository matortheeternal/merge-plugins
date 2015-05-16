unit mpEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,
  mpBase, mpLogger;

type
  TEditForm = class(TForm)
    lblName: TLabel;
    lblFilename: TLabel;
    edName: TEdit;
    edFilename: TEdit;
    lblMethod: TLabel;
    lblRenumbering: TLabel;
    cbMethod: TComboBox;
    cbRenumbering: TComboBox;
    btnOk: TButton;
    btnCancel: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
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
    merge.name := edName.Text;
    merge.filename := edFilename.Text;
    merge.method := cbMethod.Text;
    merge.renumbering := cbRenumbering.Text;
  end;
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
