unit mpLoginForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ImgList, pngimage, StdCtrls, Buttons, FileCtrl,
  mpBackend;

type
  TLoginForm = class(TForm)
    btnLogin: TButton;
    btnCancel: TButton;
    lblUserID: TLabel;
    gbLogin: TGroupBox;
    edUserID: TEdit;
    lblPassword: TLabel;
    edPassword: TEdit;
    lblDatabase: TLabel;
    edDatabase: TEdit;
    lblHost: TLabel;
    edHost: TEdit;
    lblPort: TLabel;
    edPort: TEdit;
    procedure btnLoginClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.dfm}

const
  SelectedColor = $f0e3d8;
  DefaultColor = clBtnFace;

procedure TLoginForm.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TLoginForm.btnLoginClick(Sender: TObject);
begin
  // attempt to login to the mysql database
  DoLogin(edUserID.Text, edPassword.text, edDatabase.Text, edHost.Text,
    edPort.Text);

  // if login successful, close form with ModalResult = mrOk
  if bLoginSuccess then
    ModalResult := mrOk;
end;

end.
