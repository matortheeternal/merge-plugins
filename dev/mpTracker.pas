unit mpTracker;

interface

uses Classes, SysUtils;

type
  TProgressEvent = procedure(const i: integer) of object;
  TLogEvent = procedure(const s: string) of object;

  TProgressTracker = class
  private
    FProgressEvent : TProgressEvent;
    FLogEvent : TLogEvent;
  public
    procedure Update(const i: integer);
    property OnProgressEvent: TProgressEvent read FProgressEvent write FProgressEvent;
    procedure Write(const s: string);
    property OnLogEvent: TLogEvent read FLogEvent write FLogEvent;
  end;

var
  Tracker : TProgressTracker;

implementation

procedure TProgressTracker.Update(const i: integer);
begin
  if Assigned(FProgressEvent) then
    FProgressEvent(i);
end;

procedure TProgressTracker.Write(const s: string);
begin
  if Assigned(FLogEvent) then
    FLogEvent(s);
end;

initialization
  Tracker := TProgressTracker.Create;

finalization
  FreeAndNil(Tracker);

end.
