unit mpSplashForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, pngimage,
  mteTracker, mteHelpers;

type
  TSplashForm = class(TForm)
    lblTitle: TLabel;
    imgSplash: TImage;
    lblAuthor: TLabel;
    lblProgress: TLabel;
    lblVersion: TLabel;
    procedure ProgressMessage(const s: string);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  lblProgress.Caption := s;
  Application.ProcessMessages;
end;

procedure TSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Tracker.OnLogEvent := nil;
end;

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  Tracker.OnLogEvent := ProgressMessage;
  lblVersion.Caption := 'v'+GetVersionMem;
end;

end.
