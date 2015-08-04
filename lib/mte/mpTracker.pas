unit mpTracker;

interface

uses Classes, SysUtils;

type
  TUpdateEvent = procedure(const i: integer) of object;
  TSetEvent = procedure(const i: integer) of object;
  TLogEvent = procedure(const s: string) of object;

  TProgressTracker = class
  private
    FUpdateEvent : TUpdateEvent;
    FSetEvent : TSetEvent;
    FLogEvent : TLogEvent;
  public
    procedure SetProgress(const i: integer);
    property OnSetEvent: TSetEvent read FSetEvent write FSetEvent;
    procedure UpdateProgress(const i: integer);
    property OnProgressEvent: TUpdateEvent read FUpdateEvent write FUpdateEvent;
    procedure Write(const s: string);
    property OnLogEvent: TLogEvent read FLogEvent write FLogEvent;
  end;

var
  Tracker : TProgressTracker;

implementation


procedure TProgressTracker.SetProgress(const i: integer);
begin
  if Assigned(FSetEvent) then
    FSetEvent(i);
end;

procedure TProgressTracker.UpdateProgress(const i: integer);
begin
  if Assigned(FUpdateEvent) then
    FUpdateEvent(i);
end;

procedure TProgressTracker.Write(const s: string);
begin
  if s = '' then
    exit;
  if Assigned(FLogEvent) then
    FLogEvent(s);
end;

initialization
  Tracker := TProgressTracker.Create;

finalization
  FreeAndNil(Tracker);

end.
