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
    btnCancel: TButton;
    btnOK: TButton;
    IconList: TImageList;
    procedure btnOKClick(Sender: TObject);
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

end.
