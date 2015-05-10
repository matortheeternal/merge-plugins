unit mpSplash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, pngimage,
  mpTracker;

type
  TSplashForm = class(TForm)
    TitleLabel: TLabel;
    SplashImage: TImage;
    ByLabel: TLabel;
    ProgressLabel: TLabel;
    procedure ProgressMessage(const s: string);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.dfm}

procedure TSplashForm.ProgressMessage(const s: string);
begin
  ProgressLabel.Caption := s;
  Application.ProcessMessages;
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  Tracker.OnLogEvent := ProgressMessage;
end;

end.
